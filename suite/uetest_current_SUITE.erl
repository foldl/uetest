-module(uetest_current_SUITE).

-include("../include/uetest.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, 
        end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([usb_disconnect/1, ue_register/1, calibration/1, wait_ue_idle/1, 
        meas_idle_current/1, establish/1, meas_tx_current/1, meas_current/1, end_call/1, power_off/1,
         set_mode_ltg/1, set_mode_lwg/1]).

-import(uetest_utils, [delay/1]).

all() -> [{group, '10.current'}].

groups() ->
    [{'5 Mode', [sequence], [{group, set_lwg}, {group, '10.current'}, {group, set_ltg}, {group, '10.current'}]},
     {'set_lwg', [sequence], [set_mode_lwg]},
     {'set_ltg', [sequence], [set_mode_ltg]},
     {'10.current', [sequence, {repeat, uetest_common_cases:get_group_repeat('10.current')}], [ue_register, calibration, usb_disconnect, run, end_call]}
    ].

suite() ->
     [{require, tester_port},
     {require, ue_port},
     {require, power_monitor},
     {require, tester},
     {require, plmn},
     {require, auth},
     {require, v_power_on},
     {require, v_run},
     {require, log_to}].

% config:
% {ue_port, integer()}
% {tester_port, [any()]}
init_per_suite(InitConfigData0) ->
    InitConfigData = uetest_common_cases:update_basic_config(InitConfigData0),
    Mod = ?value(tester_mod, InitConfigData),
    {ok, Tester} = apply(tester_control, start, ct:get_config(tester_port)),
    Mod:clear_error(Tester),
    unlink(Tester),
    UePort = case ?value(ue_port, InitConfigData) of
        Port when is_integer(Port) ->
            [{ue_port, Port}];
        [Port1, Port2] ->
            [{ue_port, Port1}, {ue2_port, Port2}]
    end,
    TServer = spawn(fun () -> uetest_common_cases:group_config_loop({empty, []}) end),
    ConfigData = UePort ++ [{tester, Tester}, {idle_current, ct:get_config(idle_current, 10)},
        {log_to, ct:get_config(log_to)}, {power_monitor, "\"" ++ ct:get_config(power_monitor) ++ "\""}, 
        ?import_cfg(gsm_cell), ?import_cfg(gsm_tester_port), ?import_cfg(gsm_pathloss), ?import_cfg(gsm_plmn),
        ?import_cfg(gsm_auth), ?import_cfg(gsm_init),
        {v_power_on, ct:get_config(v_power_on)}, {v_run, ct:get_config(v_run)}, {conf_storage, TServer} | InitConfigData],
    ConfigData.

end_per_suite(ConfigData) ->
    ?value(conf_storage, ConfigData) ! stop,
    try_enable_sim2(ConfigData, 0),
    power_on_usb(ConfigData), % for other cases
    link(?value(tester, ConfigData)),
    tester_control:stop(?value(tester, ConfigData)).

try_enable_sim2(Ue, ConfigData, CFUNValue) when is_pid(Ue) ->
    case ?value(ue2_port, ConfigData) of
        undefined -> ok;
        shared ->
            ue_control:exec(Ue, "AT*CHDSTST=1"),
            ok = ue_control:set_cfun(Ue, CFUNValue),
            ue_control:exec(Ue, "AT*CHDSTST=0");
        Port when is_integer(Port) ->
            ok = ue_control:set_cfun(Ue, CFUNValue)
    end.

try_enable_sim2(ConfigData, CFUNValue) ->
    case ?value(ue2_port, ConfigData) of
        undefined -> ok;
        Port ->
            ConfigData10 = if 
                is_integer(Port) -> [{ue_port, Port}, {restart, reboot} | ConfigData];
                true -> [{restart, reboot} | ConfigData]
            end,
            power_on_usb(ConfigData),
            {ok, Ue} = uetest_common_cases:wait_ue_restarted(ConfigData10),
            try_enable_sim2(Ue, ConfigData, CFUNValue),
            ue_control:stop(Ue)
    end.

init_per_group('5 Mode', ConfigData) -> ConfigData;
init_per_group(set_lwg, ConfigData) -> 
    power_on_usb(ConfigData),
    ConfigData;
init_per_group(set_ltg, ConfigData) -> 
    power_on_usb(ConfigData),
    ConfigData;
init_per_group('10.current', ConfigData) -> 
    Mod = ?value(tester_mod, ConfigData),
    Tester = ?value(tester, ConfigData),
    link(Tester),
    power_off(ConfigData),
    Mod:abort(Tester),   % previous measurement might still running 

    ConfigData10 = uetest_common_cases:update_basic_config(ConfigData),
    power_on_usb(ConfigData10),
    
    GT = case uetest_common_cases:try_prepare_gsm_cell(ConfigData10) of
        {ok, T} -> 
            {GRX, GTX} = ?value(gsm_pathloss, ConfigData),
            Mod:set_pathloss(T, GRX, GTX),
            case ?value(gsm_init, ConfigData) of
                L when is_list(L) ->
                    lists:foreach(fun ({F, Args}) ->
                                        apply(Mod, F, [T | Args]) end,
                                L);
                _ -> ok
            end,
            Mod:cell_activate(T, true),
            unlink(T),
            T;
        _ -> undefined
    end,
    {Tag, CaseConfig} = uetest_common_cases:get_group_config('10.current', ?value(conf_storage, ConfigData)),
    uetest_common_cases:prepare_sim_cell([{cell, ?value(cell, CaseConfig)} | ConfigData10]),
    case ?value(pathloss, ConfigData) of
        {PLRX, PLTX} -> Mod:set_pathloss(?value(tester, ConfigData), PLRX, PLTX);
        _ -> ok        
    end,

    cmd_parser:set_cell_identifier(?value(cell, CaseConfig)),
    ct:comment("CASE: " ++ Tag),
    case uetest_common_cases:config_tester(Tester, ?value(init, CaseConfig), []) of
        {ok, Commands} -> 
            CaseConfig10 = proplists:delete(init, CaseConfig),
            Mod:cell_activate(Tester, true),
            unlink(Tester), 
            try_enable_sim2(ConfigData10, 0),
            ConfigData10 ++ [{gsm_tester, GT}, {tester_commands, Commands}, {case_tag, Tag} | CaseConfig10];
        _ ->
            unlink(Tester),
            {skip, invalid_tester_config}
    end.

end_per_group(_Group, ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    link(Tester),
    Mod:abort(Tester),
    unlink(Tester),
    case ?value(gsm_tester, ConfigData) of
        GT when is_pid(GT) -> tester_control:stop(GT);
        _ -> ok
    end,        
    ok.

init_per_testcase(_, ConfigData) -> 
    link(?value(tester, ConfigData)),
    ConfigData.

end_per_testcase(_, ConfigData) ->  
    unlink(?value(tester, ConfigData)), 
    ok.

ue_register(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    ok = Mod:wait_reg(Tester, 60 * 4),
    case ?value(gsm_tester, ConfigData) of
        undefined -> ok;
        GSM ->
            try_enable_sim2(ConfigData, 1), 
            Mod = ?value(tester_mod, ConfigData),
            ok = Mod:wait_reg(GSM, 60 * 4)
    end.

?def_common_case(end_call).

set_mode_ltg(ConfigData) ->
    {ok, Ue} = uetest_common_cases:wait_ue_restarted(ConfigData),
    case ue_control:exec(Ue, "AT*SWITCHMODEM=3") of
        {"OK", _X} -> ok
    end,
    ok = ue_control:set_band(Ue, triple),
    ue_control:stop(Ue).

set_mode_lwg(ConfigData) ->
    {ok, Ue} = uetest_common_cases:wait_ue_restarted(ConfigData),
    case ue_control:exec(Ue, "AT*SWITCHMODEM=4") of
        {"OK", _X} -> ok
    end,
    ok = ue_control:set_band(Ue, triple),
    ue_control:stop(Ue).

calibration(ConfigData) ->
    case ?value(pathloss, ConfigData) of
        {_PLRX, _PLTX} -> ok;
        _ -> calibration0(ConfigData)
    end.

calibration0(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    {ok, Ue} = uetest_common_cases:wait_ue_restarted(ConfigData),
    delay(10),
    ok = uetest_utils:wait_until(fun () -> 
                (cali(Ue, Tester, Mod, Mod:get_rsepre(Tester)) < 2) andalso
                (cali(Ue, Tester, Mod, Mod:get_rsepre(Tester)) < 2) end, 60),
    PL = Mod:get_pathloss(Tester),
    ct:comment({pathloss, PL}),
    ue_control:stop(Ue).

cali(Ue, Tester, Mod, RSE) ->
    delay(2),
    case ue_control:vzw_rsrp(Ue) of
       {ok, [{_, Rsrp} | _T]} -> 
            ct:pal("~p", [{Rsrp, RSE}]),
            PLAdj = RSE - Rsrp,
            case abs(PLAdj) > 1 of
                true ->
                    {V, _V2} = Mod:get_pathloss(Tester),
                    Mod:set_pathloss(Tester, V + PLAdj, V + PLAdj);
                _ -> ok
            end,
            ct:pal("~p", [{Rsrp, RSE, PLAdj}]),
            PLAdj;
        error -> 100
    end. 

establish(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    uetest_common_cases:establish(ConfigData),
    case ?value(tester_name, ConfigData) of
        '3110' ->
            CellId = tester_control:get_id(Tester),
            tester_control:send_command(Tester, "CONF:" ++ CellId ++ " FTP"),
            case uetest_utils:band_type(?value('band', ConfigData)) of 
               tdd -> tester_control:send_command(Tester, "SENS:" ++ CellId ++ ":FTP:MCS:SWIT:DL 22");
               _ -> tester_control:send_command(Tester, "SENS:" ++ CellId ++ ":FTP:MCS:SWIT:DL 12")
            end, 
            tester_control:send_command(Tester, "SENS:" ++ CellId ++ ":FTP:BO DLB"),
            tester_control:send_command(Tester, "SENS:" ++ CellId ++ ":ALL:CONT ON"),
            tester_control:send_command(Tester, "INIT");
        '8820' ->
            ok
    end.

wait_ue_idle(ConfigData) ->
    delay(10),

    Meas = fun () ->
            {ok, X} = powermonitor_meas(?value(power_monitor, ConfigData), 
                                       ?value(log_to, ConfigData), 
                                       "temp", 60, 
                                       ?value(v_run, ConfigData)),
            X
    end,

    MeasFun = fun () -> 
            X = Meas(),
            ct:pal("idle current = ~p", [X]),
            X < ?value(idle_current, ConfigData) 
    end,

    Convergence = fun (LastCurrent) ->
            X = Meas(),
            ct:pal("idle current = ~p", [X]),
            case abs(X - LastCurrent) < 1.1 of
                true -> {true, X};
                _ -> X
            end
    end,
    
    {true, Idle} = uetest_utils:nest_until(Convergence, Meas(), 8 * 60),
    
    case Idle < ?value(idle_current, ConfigData) of
        true -> 
            ct:pal("final idle current = ~p", [Idle]),
            true;
        false -> 
            uetest_common_cases:establish(ConfigData),
            delay(3),
            uetest_common_cases:end_call(ConfigData),
            case uetest_utils:wait_until(MeasFun, 2 * 60) of
                ok -> ok;
                _ -> 
                    uetest_common_cases:establish(ConfigData),
                    delay(3),
                    uetest_common_cases:end_call(ConfigData),
                    ok = uetest_utils:wait_until(MeasFun, 8 * 60)
            end
    end.

meas_idle_current(ConfigData) ->
    Fn = lists:flatten(io_lib:format("~p_~p_idle", [?value(group_name, ConfigData), ?value('band', ConfigData)])),
    {ok, Idle} = powermonitor_meas(?value(power_monitor, ConfigData), 
                                       ?value(log_to, ConfigData), 
                                       Fn, 120, 
                                       ?value(v_run, ConfigData)),
    ct:comment(Idle).

meas_current(ConfigData) ->
    Fn = lists:flatten(io_lib:format("drx_~p", [?value(group_name, ConfigData)])),
    {ok, C} = powermonitor_meas(?value(power_monitor, ConfigData), 
                                       ?value(log_to, ConfigData), 
                                       Fn, 30, 
                                       ?value(v_run, ConfigData)),
    ct:comment(C).

meas_tx_current(ConfigData) ->
    ct:comment([meas_power(ConfigData, Power) || Power <- ?value(tx_powers, ConfigData)]).

meas_power(ConfigData, Power) ->
    Tester = ?value(tester, ConfigData),
    Fn = lists:flatten(io_lib:format("~p_~p_~p", [?value(group_name, ConfigData), ?value('band', ConfigData), Power])),
    case ?value(tester_name, ConfigData) of
        '3110' ->
            tester_control:send_command(Tester, io_lib:format("SENS:~s:FTP:UEP ~pDBM", [tester_control:get_id(Tester), Power]));
        '8820' ->
            tester_control:send_command(Tester, io_lib:format("ILVL ~p", [Power]))
    end,
    delay(3),
    {ok, Current} = powermonitor_meas(?value(power_monitor, ConfigData), 
                                       ?value(log_to, ConfigData), 
                                       Fn, 20, 
                                       ?value(v_run, ConfigData)),
    {Power, Current}.

power_on_usb(ConfigData) ->
    FullFn = filename:join([?value(log_to, ConfigData), "power_on_usb.pt4"]),
    R = os:cmd(lists:flatten(io_lib:format("\"~s\" \"/SAVEFILE=~s\" /TRIGGER=DTD~pA /VOUT=~p /NOEXITWAIT /USB=ON /KEEPPOWER", 
                [?value(power_monitor, ConfigData), FullFn, 1, ?value(v_run, ConfigData)]))),
    case re:run(R, "Exit Code \\[([0-9]+)\\]", [{capture, all_but_first, list}]) of
        {match, ["0"]} -> ok
    end.

power_off(ConfigData) ->
    FullFn = filename:join([?value(log_to, ConfigData), "power_off.pt4"]),
    R = os:cmd(lists:flatten(io_lib:format("\"~s\" \"/SAVEFILE=~s\" /TRIGGER=DTD~pA /VOUT=~p /NOEXITWAIT /USB=OFF", 
                [?value(power_monitor, ConfigData), FullFn, 1, ?value(v_run, ConfigData)]))),
    case re:run(R, "Exit Code \\[([0-9]+)\\]", [{capture, all_but_first, list}]) of
        {match, ["0"]} -> ok
    end.

usb_disconnect(ConfigData) ->
    {ok, _Current} = powermonitor_meas(?value(power_monitor, ConfigData), 
                                       ?value(log_to, ConfigData), 
                                       "usb_disconnect", 1, 
                                       ?value(v_power_on, ConfigData)).

powermonitor_meas(PMExe, Path, Fn, Secs, Vout) ->
    FullFn = filename:join([Path, Fn ++ ".pt4"]),
    R = os:cmd(lists:flatten(io_lib:format("\"~s\" \"/SAVEFILE=~s\" /TRIGGER=DTD~pA /VOUT=~p /NOEXITWAIT /USB=OFF /KEEPPOWER", 
                [PMExe, FullFn, Secs, Vout]))),
    case re:run(R, "Exit Code \\[([0-9]+)\\]", [{capture, all_but_first, list}]) of
        {match, ["0"]} -> {ok, read_current(FullFn)};
        _ -> error
    end.

read_ulong(Fn, Pos) ->
    case file:pread(Fn, {bof, Pos}, 8) of
        {ok, <<V:64/integer-little>>} ->
            V;
        X -> throw(X)
    end.

read_float(Fn, Pos) ->
    case file:pread(Fn, {bof, Pos}, 4) of
        {ok, <<V:32/float-little>>} ->
            V;
        X -> throw(X)
    end.

read_current(Fn) ->
    {ok, Fid} = file:open(Fn, [binary]),
    Count   = read_ulong(Fid, 160),
    Missing = read_ulong(Fid, 168),
    MainCurrent = read_float(Fid, 180),
    file:close(Fid),
    case Count - Missing of
        X when X > 0 -> 
            MainCurrent / X;
        _ ->
            -1.0
    end.
