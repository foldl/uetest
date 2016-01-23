-module(uetest_twocells_SUITE).

-include("../include/uetest.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, 
        end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([ue_register1/1, ue_register2/1, establish1/1, establish2/1, end_call1/1, end_call2/1, 
         run/1, reselection/1, gap/1]).

-import(uetest_utils, [delay/1]).

-define(MEAS_ERROR, 5).
-define(def_group(Name, Steps), {Name, [sequence, {repeat, uetest_common_cases:get_group_repeat(Name)}], Steps}).

all() -> [{group, '3.idle_meas'}, {group, '4.resel'}, {group, '5.conn_meas'}, {group, '6.handover'},
          {group, '13.plmn'}].

groups() ->
    [?def_group('3.idle_meas', [ue_register1, run]),
     ?def_group('4.resel', [ue_register1, run, reselection]),
     ?def_group('5.conn_meas', [ue_register1, establish1, run, end_call1]),
     ?def_group('6.handover', [ue_register1, establish1, run, ue_register2, end_call2]),
     ?def_group('12.ca_siso', [ue_register1, establish1, ftp_vs_power1, end_call1]),
     ?def_group('13.plmn', [ue_register1, gap])].

suite() ->
    [{require, ue_port},
     {require, tester_port},
     {require, tester},
     {require, plmn},
     {require, auth},
     {require, pathloss},
     {require, restart}].

init_per_suite(InitConfigData0) ->
    InitConfigData = uetest_common_cases:update_basic_config(InitConfigData0),
    [TP1, TP2] = ct:get_config(tester_port),
    {ok, Tester1} = apply(tester_control, start, TP1),
    {ok, Tester2} = apply(tester_control, start, TP2),
    Mod = ?value(tester_mod, InitConfigData),
    Mod:clear_error(Tester1), Mod:clear_error(Tester2),
    
    unlink(Tester1), unlink(Tester2),
    TServer = spawn(fun () -> uetest_common_cases:group_config_loop({empty, []}) end),
    [{tester, [Tester1, Tester2]}, {conf_storage, TServer} | InitConfigData].

end_per_suite(ConfigData) ->
    ?value(conf_storage, ConfigData) ! stop,
    [Tester1, Tester2] = ?value(tester, ConfigData),
    link(Tester1),
    link(Tester2),
    tester_control:stop(Tester1),
    tester_control:stop(Tester2).

group_type('12.ca_siso') -> ca;
group_type('13.plmn') -> plmn;
group_type(_) -> other.

init_per_group(Group, ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    [Tester1, Tester2] = ?value(tester, ConfigData),
    link(Tester1), link(Tester2),
    Mod:abort(Tester1), Mod:abort(Tester2),

    {Tag, CaseConfig} = uetest_common_cases:get_group_config(Group, ?value(conf_storage, ConfigData)),
    uetest_common_cases:prepare_sim_cell([{cell, ?value(cell, CaseConfig)}, 
                                          {nc_plmn, ?value(nc_plmn, CaseConfig)} | ConfigData]),
    
    uetest_common_cases:restart_ue(ConfigData),
    uetest_common_cases:set_pathloss(ConfigData),

    case group_type(Group) of
        ca -> Mod:prepare_ca([Tester1, Tester2], siso);
        _ -> ok
    end,

    ct:comment("CASE: " ++ Tag),
    random:seed(now()),
    case testers_command([Tester1, Tester2], ?value(cell, CaseConfig), ?value(init, CaseConfig))  of
        {ok, Commands} ->             
            case group_type(Group) of
                ca -> Mod:config_ca([Tester1, Tester2], siso);
                plmn -> ok;
                _Other -> 
                    case {Mod:rat_tech(Tester1), Mod:rat_tech(Tester2)} of
                        {lte, lte} ->
                            Mod:set_ta_code(Tester1, 1),
                            Mod:set_ta_code(Tester2, case ?value(tau, CaseConfig) of on -> 2; _ -> 1 end);
                        _ -> ok
                    end,
                    Mod:add_ncell(Tester1, Tester2),
                    Mod:enable_reselection(Tester1, true)
            end,

            CaseConfig10 = proplists:delete(init, CaseConfig),
            Mod:cell_activate(Tester1, true), 
            unlink(Tester1), unlink(Tester2), 
            {ok, Ue} = uetest_common_cases:wait_ue_restarted(ConfigData),
            ue_control:engineering_mode(Ue, 1),
            unlink(Ue),
            CaseConfig10 ++ [{tester_commands, Commands}, {ue, Ue}, {case_tag, Tag}
                | proplists:delete(ue, ConfigData)];
        _ ->
            unlink(Tester1), unlink(Tester2),
            {skip, invalid_tester_config}
    end.

end_per_group(_Group, ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    [Tester1, Tester2] = ?value(tester, ConfigData),
    link(Tester1),
    link(Tester2),
    Mod:abort(Tester1), Mod:abort(Tester2),
    Mod:cell_activate(Tester2, false), % bring cells down, since next case might not use two cells.
    Mod:cell_activate(Tester1, false),
    ue_control:stop(?value(ue, ConfigData)),
    unlink(Tester1),
    unlink(Tester2),
    ok.

init_per_testcase(_, ConfigData) -> 
    [Tester1, Tester2] = ?value(tester, ConfigData),
    link(Tester1),
    link(Tester2),
    link(?value(ue, ConfigData)),
    ConfigData.

end_per_testcase(_, ConfigData) ->  
    [Tester1, Tester2] = ?value(tester, ConfigData),
    unlink(Tester1),
    unlink(Tester2),
    unlink(?value(ue, ConfigData)),
    ok.

testers_command(Testers, Cells, CmdsList) ->
    R = lists:zipwith3(fun (T, C, Cmds) ->
                    cmd_parser:set_cell_identifier(C),
                    case uetest_common_cases:config_tester(T, Cmds, []) of
                        {ok, CR} -> {ok, CR};
                        _ -> error
                    end
            end, Testers, Cells, CmdsList),
    case lists:all(fun (X) -> element(1, X) == ok end, R) of
        true ->
            {ok, [element(2, X) || X <- R]};
        _ -> error
    end.

-define(def_common_case1(Name, RawName),
    Name(ConfigData) ->
        [Tester1, _Tester2] = ?value(tester, ConfigData),
        uetest_common_cases:RawName([{tester, Tester1} | ConfigData])).

-define(def_common_case2(Name, RawName),
    Name(ConfigData) ->
        [_Tester1, Tester2] = ?value(tester, ConfigData),
        uetest_common_cases:RawName([{tester, Tester2} | ConfigData])).

ue_register1(ConfigData) ->
        Mod = ?value(tester_mod, ConfigData),
        [Tester1, Tester2] = ?value(tester, ConfigData),
        uetest_common_cases:ue_register([{tester, Tester1} | ConfigData]),
        Mod:cell_activate(Tester2, true),
        delay(1).

?def_common_case2(ue_register2, ue_register).

?def_common_case1(establish1, establish).
?def_common_case2(establish2, establish).

?def_common_case1(end_call1, end_call).
?def_common_case2(end_call2, end_call).

% <AcT>:
% 0: GSM
% 1: GSM Compact
% 2: UTRAN
% 3: GSM w/EGPRS
% 4: UTRAN w/HSDPA
% 5: UTRAN w/HSUPA
% 6: UTRAN w/HSDPA and HSUPA
% 7: E-UTRAN
% 8: UTRAN w/HSPA+
parse_act(0) -> gsm;
parse_act(1) -> gsm;
parse_act(3) -> gsm;
parse_act(7) -> lte;
parse_act(_) -> umts.

get_cell_info(Mod, Tester) ->
    Rat = case Mod:rat_tech(Tester) of
        tds -> umts;
        wcdma -> umts;
        X -> X
    end,
    {Mod:get_plmn(Tester), Rat}.

gap(ConfigData) ->
    Ue = ?value(ue, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    {ok, PLMNs} = ue_control:plmn(Ue),
    ct:log("AT+COPS: ~p", [PLMNs]),
    Compacted = sets:from_list([{PLMN, parse_act(RAT)} || {_Stat, _NameL, _NameS, PLMN, RAT} <- PLMNs]),
    lists:foreach(fun (Tester) -> 
                true = sets:is_element(get_cell_info(Mod, Tester), Compacted)
        end, ?value(tester, ConfigData)).

reselection(ConfigData) ->
    case ?value(tau, ConfigData) of
        on ->
            ue_register2(ConfigData);
        _ ->
            Mod = ?value(tester_mod, ConfigData),
            Ue = ?value(ue, ConfigData),
            [_Tester1, Tester2] = ?value(tester, ConfigData),
            {Freq, Id} = Mod:get_freq_id(Tester2),
            uetest_utils:wait_until(fun() ->
                    case ue_control:get_scell(Ue) of
                        {ok, {Freq, Id}} -> true;
                        Other -> ct:print("~p", [Other])
                    end
                end, 2 * 60)
    end.

run(ConfigData) ->
    lists:foldl(fun 
            ({ExeTime, Cmd}, {ok, Time}) when ExeTime > 0 ->
                timer:sleep(ExeTime * 1000),
                {exec(ConfigData, Cmd), Time + ExeTime};
            ({_ExeTime, Cmd}, {ok, Time}) ->
                {exec(ConfigData, Cmd), Time}
        end, {ok, 0}, ?value(timeline, ConfigData)).

exec(ConfigData, {check_signal, Crit}) ->
    {ok, L} = ue_control:all_cells_quality(?value(ue, ConfigData)),
    ct:pal("~p", [{all_cells_quality, L}]),
    check_power(L, Crit);
exec(ConfigData, {check_rsrp, Crit}) ->
    {ok, L} = ue_control:vzw_rsrp(?value(ue, ConfigData)),
    ct:pal("~p", [{rsrp, L}]),
    check_power(L, Crit);
exec(ConfigData, L) when is_list(L) ->
    {ok, _Cmds} = testers_command(?value(tester, ConfigData), ?value(cell, ConfigData), L),
    ok.

check_power(Result, Crit) ->
    lists:foreach(fun 
            ({Id, [Value1, Value2]}) ->
                    case proplists:get_value(Id, Result) of
                        undefined -> ct:fail({no_measurement, Id});
                        V when Value1 =< V, V =< Value2 ->
                            ok;
                        V -> ct:fail({range, Id, [Value1, Value2], V})
                    end;
            ({Id, Value}) ->
                    case proplists:get_value(Id, Result) of
                        undefined -> ct:fail({no_measurement, Id});
                        V when abs(V - Value) < ?MEAS_ERROR ->
                            ok;
                        V -> ct:fail({range, Id, Value, V})
                    end
            end, Crit),
    ok.
