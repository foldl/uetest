-module(uetest_batch_SUITE).

-include("../include/uetest.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, 
        end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([ftp_meas/1, ue_register/1, establish/1, end_call/1, ul_throughput/1, dl_throughput/1, 
        random_access/1, ftp_vs_power_dl/1, ftp_vs_power_ul/1, ftp_vs_power_dl/0, blind_handover/1, end_irat_call/1]).

-import(uetest_utils, [delay/1]).

-define(def_group(Name, Steps), {Name, [sequence, {repeat, uetest_common_cases:get_group_repeat(Name)}], Steps}).

all() -> [{group, '1.ics'}, {group, '2.ra'}, {group, '7.ul_tpt'}, {group, '11.one_dl_tpt'}].

groups() ->
    [?def_group('1.ics', [ue_register]),
     ?def_group('2.ra', [ue_register, random_access]),
     ?def_group('7.ul_tpt', [ue_register, establish, ftp_vs_power_ul, end_call]),
     ?def_group('8.dl_tpt', [ue_register, establish, ftp_vs_power, end_call]),
     ?def_group('9.blind_ho', [ue_register, establish, blind_handover, end_irat_call]),
     ?def_group('11.one_dl_tpt', [ue_register, establish, ftp_vs_power_dl, end_call])
    ].

suite() ->
    [{require, ue_port},
     {require, tester_port},
     {require, tester},
     {require, plmn},
     {require, auth},
     {require, pathloss},
     {require, restart}].

% config:
% {ue_port, integer()}
% {tester_port, [any()]}
init_per_suite(InitConfigData0) ->
    InitConfigData = uetest_common_cases:update_basic_config(InitConfigData0),
    {ok, Tester} = apply(tester_control, start, ct:get_config(tester_port)),
    Mod = ?value(tester_mod, InitConfigData),
    Mod:clear_error(Tester),
    unlink(Tester),
    TServer = spawn(fun () -> uetest_common_cases:group_config_loop({empty, []}) end),
    [{tester, Tester}, {conf_storage, TServer} | InitConfigData].

end_per_suite(ConfigData) ->
    ?value(conf_storage, ConfigData) ! stop,
    link(?value(tester, ConfigData)),
    tester_control:stop(?value(tester, ConfigData)).

init_per_group('9.blind_ho', ConfigData) ->
    ConfigData10 = gen_init_per_group('9.blind_ho', ConfigData),
    Mod = ?value(tester_mod, ConfigData10),
    Tester = ?value(tester, ConfigData10),
    {PLMN, Auth} = ?value(plmn_auth, ConfigData10),
    Mod:prepare_blind_ho(Tester, PLMN, Auth),
    ConfigData10;
init_per_group(Group, ConfigData) ->
    gen_init_per_group(Group, ConfigData).

gen_init_per_group(Group, ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    Tester = ?value(tester, ConfigData),
    link(Tester),
    Mod:abort(Tester),   % previous measurement might still running 
    random:seed(now()),    
    {Tag, CaseConfig} = uetest_common_cases:get_group_config(Group, ?value(conf_storage, ConfigData)),
    uetest_common_cases:prepare_sim_cell([{cell, ?value(cell, CaseConfig)} | ConfigData]),
    uetest_common_cases:restart_ue(ConfigData),
    {PLRX, PLTX} = ?value(pathloss, ConfigData),
    Mod:set_pathloss(?value(tester, ConfigData), PLRX, PLTX),
    cmd_parser:set_cell_identifier(?value(cell, CaseConfig)),
    ct:comment("CASE: " ++ Tag),
    case uetest_common_cases:config_tester(Tester, ?value(cmds, CaseConfig), []) of
        {ok, Commands} -> 
            CaseConfig10 = proplists:delete(cmds, CaseConfig),
            Mod:cell_activate(Tester, true),
            unlink(Tester), 
            {ok, Ue} = uetest_common_cases:wait_ue_restarted(ConfigData),
            unlink(Ue),
            CaseConfig10 ++ [{tester_commands, Commands}, {ue, Ue}, {case_tag, Tag}
                | proplists:delete(ue, ConfigData)];
        _ ->
            unlink(Tester),
            {skip, invalid_tester_config}
    end.

end_per_group(_Group, ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    Tester = ?value(tester, ConfigData),
    link(Tester),
    Mod:abort(Tester),
    ue_control:stop(?value(ue, ConfigData)),
    unlink(Tester),
    ok.

init_per_testcase(_, ConfigData) -> 
    link(?value(tester, ConfigData)), 
    link(?value(ue, ConfigData)),
    ConfigData.

end_per_testcase(_, ConfigData) ->  
    unlink(?value(tester, ConfigData)), 
    unlink(?value(ue, ConfigData)),
    ok.

?def_common_case(ue_register).
?def_common_case(establish).
?def_common_case(end_call).
?def_common_case(ftp_meas).
?def_common_case(random_access).

blind_handover(ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    Tester = ?value(tester, ConfigData),
    {Rat, Id} = ?value(to, ConfigData),
    ToId = Mod:fmt_cell_id({Rat, Id}),
    Mod:blind_handover(Tester, ?value(cell_id, ConfigData), Rat),
    delay(10),
    ok = Mod:wait_conn(Tester, ToId, 20).

end_irat_call(ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    Tester = ?value(tester, ConfigData),
    Mod:end_call(Tester, Mod:fmt_cell_id(?value(to, ConfigData))),
    Mod:cell_activate(Tester, Mod:fmt_cell_id(?value(to, ConfigData)), false).

ul_throughput(ConfigData) ->
    uetest_common_cases:throughput_test(ConfigData, ul).

dl_throughput(ConfigData) ->
    uetest_common_cases:throughput_test(ConfigData, dl).

ftp_vs_power_dl() ->
    [{timetrap, {hours, 1}}].

ftp_vs_power_dl(ConfigData) ->
    ftp_vs_power(ConfigData, dl).

ftp_vs_power_ul(ConfigData) ->
    ftp_vs_power(ConfigData, ul).

ftp_vs_power(ConfigData, Dir) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    Mod:start_ftp(Tester, Dir),
    cmd_parser:set_cell_identifier(?value(cell, ConfigData)),
    {ok, _Commands} = uetest_common_cases:config_tester(Tester, ?value(conn_cmds, ConfigData), []),
    Mod:initiate(Tester),
    delay(1),

    lists:foreach(fun (A) -> vs_arg(Tester, set_rsepre, Mod, Dir, A) end, 
        proplists:get_all_values(vs_power, ConfigData)),
    lists:foreach(fun (A) -> vs_arg(Tester, set_awgn, Mod, Dir, A) end, 
        proplists:get_all_values(vs_awgn, ConfigData)).

vs_arg(Tester, ModFun, Mod, Dir, ConfigData) ->
    Step0 = case ?value(step, ConfigData) of
        X when is_number(X) -> abs(X);
        _ -> 1
    end,
    {From, To, Step} = case ?value(range, ConfigData) of
        {From0, To0} when From0 >= To0 -> {From0, To0, -Step0};
        {From1, To1} when From1 < To1 -> {From1, To1, Step0};
        undefined -> {-48, -90, -Step0}
    end,
    PowerLst = lists:seq(From, To, Step),
    ArgFun = fun (Arg) -> Mod:ModFun(Tester, Arg) end,
    ArgFun(hd(PowerLst)),
    delay(2),
    MCSCmd = case ?value(mcs, ConfigData) of
        L10 when is_list(L10) -> 
            case cmd_parser:parse_simple(L10) of
                {ok, Cmd} -> Cmd;
                _ -> ""
            end;
        _ -> ""
    end,
    put(scan_acc, []),
    case length(PowerLst) of
        L when L > 0 ->
            R = [begin
                    ArgFun(Pow),
                    delay(2),
                    case MCSCmd of
                        [] -> {Pow, Mod:meas_ftp(Tester, Dir), Mod:meas_ftp_bler(Tester, Dir) / 100};
                        _ -> 
                            AllMCS = mcs_adjust(Tester, Dir, MCSCmd, Mod, [1, 28]),
                            put(scan_acc, [{uetest_utils:num_to_str(Pow), AllMCS} | get(scan_acc)]),
                            {AMCS, ASpeed} = hd(lists:sort(fun ({_, X}, {_, Y}) -> X >= Y end, AllMCS)),
                            {Pow, ASpeed, Mod:meas_ftp_bler(Tester, Dir) / 100, AMCS}
                    end
                 end || Pow <- PowerLst],
             case get(scan_acc) of
                 [] -> ok;
                 ScanAcc -> 
                     uetest_common_cases:emit_chart(atom_to_list(ModFun) ++ " MCS Scan", "MCS", "Throughput (Mb/s)", ScanAcc),
                     uetest_common_cases:emit_chart("Best MCS", atom_to_list(ModFun), "MCS", [{element(1, X), element(4, X)} || X <- R])
             end,
             uetest_common_cases:emit_chart("Maximum Throughput", atom_to_list(ModFun), "Throughput (Mb/s)", [{element(1, X), element(2, X)} || X <- R]),
             uetest_common_cases:emit_chart("BLER under Maximum Throughput", atom_to_list(ModFun), "BLER", [{element(1, X), element(3, X)} || X <- R]),
             %ct:log("~p thoughput, {~p, Throughput, BLER [, MCS]}:~n~p", [Dir, ModFun, R]),
             SpeedList = [element(2, X) || X <- R],
             Max = lists:max(SpeedList),
             Min = lists:min(SpeedList),
             case ?value(min_ratio, ConfigData) of
                 Ratio when is_number(Ratio) -> 
                     true = Min > Ratio * Max;
                 _ -> ok
             end,
             case ?value(prev_ratio, ConfigData) of
                 Ratio10 when is_number(Ratio10) -> 
                     true = is_number(lists:foldl(fun 
                               (_X, false) -> false;
                               (X, LastX) ->
                                 if X > Ratio10 * LastX -> X;
                                    true -> false
                                end end, hd(SpeedList), tl(SpeedList)));
                 _ -> ok
             end,
             case ?value(min_speed, ConfigData) of
                 Speed when is_number(Speed) -> 
                     true = Min > Speed;
                 _ -> ok
             end,
             case ?value(bler, ConfigData) of
                 BLER when is_number(BLER) -> 
                     true = lists:max([element(3, X) || X <- R]) < BLER;
                 _ -> ok
             end;
         _ -> ok
     end.

mcs_adjust(Tester, Dir, Cmd, Mod, [From, To]) ->
     F = fun (MCS) ->
             tester_control:send_command(Tester, Cmd ++ " " ++ integer_to_list(MCS)),             
             Mod:initiate(Tester),
             delay(2),
             Mod:meas_ftp(Tester, Dir)
     end,
     [{MCS, F(MCS)} || MCS <- lists:seq(From, To)].
