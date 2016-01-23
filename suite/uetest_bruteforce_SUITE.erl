-module(uetest_bruteforce_SUITE).

-include_lib("uetest/include/uetest.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, 
        end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([ue_register/1, random_access/1,establish/1, end_call/1]).

-import(uetest_utils, [delay/1]).

all() -> [{group, paging}, {group, drx}, {group, prach}].

groups() ->
    [{paging, [sequence, {repeat, 50}], [ue_register, random_access]},
     {drx, [sequence, {repeat, 30}], [ue_register, random_access]},
     {prach, [sequence, {repeat, 30}], [ue_register, random_access, establish, end_call]}].

suite() ->
    [{require, ue_port},
     {require, tester_port},
     {require, tester},
     {require, plmn},
     {require, auth},
     {require, pathloss},
     {require, restart},
     {require, cell}].

% config:
% {ue_port, integer()}
% {tester_port, [any()]}
% {cmd_file, FileName::string()}
% {sim, public/tester}
init_per_suite(InitConfigData0) ->
    InitConfigData = uetest_common_cases:update_basic_config(InitConfigData0),
    {ok, Tester} = apply(tester_control, start, ct:get_config(tester_port)),
    Mod = ?value(tester_mod, InitConfigData),
    Mod:clear_error(Tester),
    unlink(Tester),
    Counter = spawn(fun () -> counter_loop({none, 1}) end),
    [{tester, Tester}, {counter, Counter}, {cell, ct:get_config(cell)} | InitConfigData].

end_per_suite(ConfigData) ->
    tester_control:stop(?value(tester, ConfigData)).

init_per_group(Group, ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    Tester = ?value(tester, ConfigData),
    Tag = lists:concat([atom_to_list(Group), ".", integer_to_list(get_repeat_no(Group, ?value(counter, ConfigData)))]),
    ct:comment("CASE: " ++ Tag),

    Mod:abort(Tester),   % previous measurement might still running 
    uetest_common_cases:prepare_sim_cell(ConfigData),
    uetest_common_cases:restart_ue(ConfigData),
    {PLRX, PLTX} = ?value(pathloss, ConfigData),    
    Mod:set_pathloss(?value(tester, ConfigData), PLRX, PLTX),
    random:seed(now()),
    cmd_parser:set_cell_identifier(?value(cell, ConfigData)),
    Templates = uetest_utils:load_cmd_template(uetest_common_cases:tv_dir(), atom_to_list(Group) ++ ".txt"),
    case config_tester(Tester, Templates, []) of
        {ok, Commands} -> 
            unlink(Tester), 
            {ok, Ue} = uetest_common_cases:wait_ue_restarted(ConfigData),
            unlink(Ue),
            Mod:cell_activate(Tester, true),
            [{tester_commands, Commands}, {cell_id}, {ue, Ue}, {case_tag, Tag} | proplists:delete(ue, ConfigData)];
        _ ->
            unlink(Tester), 
            {skip, invalid_tester_config}
    end.

end_per_group(_Group, ConfigData) ->
    Mod = ?value(tester_mod, ConfigData),
    Tester = ?value(tester, ConfigData),
    Mod:abort(Tester),
    ue_control:stop(?value(ue, ConfigData)),
    ok.

init_per_testcase(_, ConfigData) -> 
    link(?value(ue, ConfigData)),
    ConfigData.

end_per_testcase(_, ConfigData) ->  
    unlink(?value(ue, ConfigData)),
    ok.

?def_common_case(ue_register).
?def_common_case(random_access).
?def_common_case(establish).
?def_common_case(end_call).

config_tester(_Tester, [], Acc) -> {ok, lists:reverse(Acc)};
config_tester(Tester, [Template | T], Acc) ->
    %ct:pal("template: ~p~n", [Template]),
    case config_tester0(Tester, Template, 10) of
        {ok, Cmd} -> 
            config_tester(Tester, T, [Cmd | Acc]);
        true -> 
            config_tester(Tester, T, Acc);
        _ ->
            false
    end.

config_tester0(_Tester, _Template, 0) -> false;
config_tester0(Tester, Template, N) ->
    case N of
        3 -> cmd_parser:set_random_flag(mid);
        2 -> cmd_parser:set_random_flag(max);
        1 -> cmd_parser:set_random_flag(min);
        _ -> cmd_parser:set_random_flag(any)
    end,
    case cmd_parser:compose(Template) of
        {ok, Cmd} ->
            % ct:pal("~p~n", [Cmd]),
            case  tester_control:exec_confirm(Tester, Cmd) of
                true -> {ok, Cmd};
                unknown -> true;
                _ -> 
                    % delay(2),
                    config_tester0(Tester, Template, N - 1)
            end;
        _ -> true
    end.

get_repeat_no(Group, Counter) -> 
    Counter ! {get, self(), Group},
    receive
        {Counter, N} -> N
    end.

counter_loop({Group, N}) ->
    receive
        {get, Pid, Group} ->
            Pid ! {self(), N},
            counter_loop({Group, N + 1});
        {get, Pid, Group2} ->
            Pid ! {self(), 1},
            counter_loop({Group2, 2});
        stop ->
            ok
    end.
