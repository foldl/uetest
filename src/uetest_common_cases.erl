%@doc Common cases.
-module(uetest_common_cases).

-include("uetest.hrl").
-include_lib("common_test/include/ct.hrl").

-export([ue_register/1, establish/1, end_call/1, random_access/1]).
-export([throughput_test/2, ftp_meas/1]).
-export([prepare_sim_cell/1, try_prepare_gsm_cell/1, set_pathloss/1]).

-export([tv_dir/0, update_basic_config/1, get_group_config/2, group_config_loop/1, get_group_repeat/1, config_tester/3, get_group_files/1]).

-export([restart_ue/1, wait_ue_restarted/1]).

-export([emit_chart/4]).

-import(uetest_utils, [delay/1]).

%@doc Update tester related config
update_basic_config(ConfigData) ->
    Tester = ct:get_config(tester),
    [{tester_mod, map_to_mod(Tester)}, {tester_name, Tester},
     {restart, ct:get_config(restart)}, {plmn_auth, {ct:get_config(plmn), ct:get_config(auth)}},
     {ue_port, ct:get_config(ue_port)}, {pathloss, ct:get_config(pathloss)} | ConfigData].

map_to_mod('3110') -> dt_3110_helper;
map_to_mod('8820') -> an_8820_helper.

%@doc Get test vector storage dir
tv_dir() ->    
    Dir0 = case init:get_argument(spec) of
        {ok, [[V | _T]]} -> V;
        _ -> 
            {ok, [[V2 | _T]]} = init:get_argument(ct_config),
            V2
    end,
    Dir = case Dir0 of
        [$., $., $/ | X] -> X
    end,   
    filename:join(code:root_dir(), filename:dirname(Dir)).

%@doc UE register. 2min timeout.
ue_register(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    ok = Mod:wait_reg(Tester, 60 * 2).

%@doc Establish call from idle state.
establish(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),

    ok = Mod:wait_idle(Tester, 20),

    Mod:establish(Tester),
    ok = Mod:wait_conn(Tester, 10).

%@doc End call from connected state.
end_call(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),

    ok = Mod:wait_conn(Tester, 1),

    Mod:end_call(Tester),
    ok = Mod:wait_idle(Tester, 10).

%@doc Establish and then end call.
random_access(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),

    ok = Mod:wait_idle(Tester, 20),

    Mod:establish(Tester),
    ok = Mod:wait_conn(Tester, 10),
    delay(2),
    Mod:end_call(Tester).

%@doc Perform standard thoughput test provided by tester.
throughput_test(ConfigData, Dir) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    true = Mod:run_throughput(Tester, Dir).

%@doc Measure IP package speed. 
% Use throughput in ConfigData to specify the required DL/UL speed.
ftp_meas(ConfigData) ->
    Tester = ?value(tester, ConfigData),
    Mod = ?value(tester_mod, ConfigData),
    {DlLimit, UlLimit} = ?value(throughput, ConfigData),

    case is_integer(DlLimit) of
        true ->
            Mod:start_ftp(Tester, dl),
            delay(2),
            F0 = fun () -> Mod:meas_ftp(Tester, dl) end,
            speed_check(uetest_utils:multi_run(F0, 5, 2), DlLimit);
        _ -> ok
    end,
    case is_integer(UlLimit) of
        true ->
            Mod:start_ftp(Tester, ul),
            delay(2),
            F1 = fun () -> Mod:meas_ftp(Tester, ul) end,
            speed_check(uetest_utils:multi_run(F1, 5, 2), UlLimit);
        _ -> ok
    end.

%@doc Prepare GSM tester according to Config, cell Id used by tester API is returned.
%     If failed, {error, Reason} is returned.
try_prepare_gsm_cell(Config) ->
    case ?value(gsm_tester_port, Config) of
        undefined -> {error, not_specified};
        Param ->
            Mod = ?value(tester_mod, Config),
            {ok, Tester} = apply(tester_control, start, Param),
            Mod:prepare_cell(Tester, ?value(gsm_cell, Config), ?value(gsm_plmn, Config), ?value(gsm_auth, Config)),
            {ok, Tester}
    end.

%@doc Prepare tester according to Config, cell Id used by tester API is returned.
prepare_sim_cell(Config) ->
    case ?value(tester, Config) of
        [T1, T2] ->
            [C1, C2] = ?value(cell, Config),
            PLMN = case ?value(nc_plmn, Config) of
                X when is_list(X) -> X;
                _ -> default 
            end,
            [prepare_sim_cell(Config, T1, C1, default), prepare_sim_cell(Config, T2, C2, PLMN)];
        T when is_pid(T) ->
            C = ?value(cell, Config),
            prepare_sim_cell(Config, T, C)
    end.

%@private
prepare_sim_cell(Config, Tester, Cell, default) ->
    Mod = ?value(tester_mod, Config),
    {PLMN, Auth} = ?value(plmn_auth, Config),
    Mod:prepare_cell(Tester, Cell, PLMN, Auth);
prepare_sim_cell(Config, Tester, Cell, PLMN) ->
    Mod = ?value(tester_mod, Config),
    {_PLMN0, Auth} = ?value(plmn_auth, Config),
    Mod:prepare_cell(Tester, Cell, PLMN, Auth).

%@private
prepare_sim_cell(Config, Tester, Cell) -> prepare_sim_cell(Config, Tester, Cell, default).

%@doc Check speed against Limit
speed_check(Speeds, Limit) ->
    true = uetest_utils:mean(Speeds) >= Limit,
    true = lists:min(Speeds) >= Limit * 0.2.

%@doc Set pathloss
set_pathloss(Config) ->
    Mod = ?value(tester_mod, Config),
    case ?value(tester, Config) of
        [T1, T2] ->
            [{PLRX1, PLTX1},{PLRX2, PLTX2}] = ?value(pathloss, Config),
            Mod:set_pathloss(T1, PLRX1, PLTX1),
            Mod:set_pathloss(T2, PLRX2, PLTX2);
        T ->
            {PLRX, PLTX} = ?value(pathloss, Config),
            Mod:set_pathloss(T, PLRX, PLTX)
    end.

%@doc Pop a configuration from server
% A list of Config :: Term() is assoicated with a group,
% each element is consumed by one run of the group.
pop_config(Pid) ->
    Pid ! {self(), pop},
    receive
        {pop, V} -> V
    end.

%@doc Get current group name
cur_group(Pid) ->
    Pid ! {self(), cur_group},
    receive
        {cur_group, Group} -> Group
    end.

%@doc Store Configs for a new Group
store_group_config(Pid, {Group, Configs}) ->
    Pid ! {store, {Group, Configs}}.

str_to_list(S) ->
    L = string:tokens(S, ";'\",._-/\\`~!@#$%^&*()[]{}"),
    [case string:to_integer(X) of
            {V, []} -> V;
            _ -> X
        end || X <- L].

%@doc smart sort of string list
smart_sort_strs(L) ->
    VL = [{X, str_to_list(X)} || X <- L],
    VLS = lists:sort(fun (A, B) -> element(2, A) =< element(2, B) end, VL),
    [element(1, X) || X <- VLS].

%@doc Get config files under cases/Group
get_group_files(Group) ->
    filelib:fold_files(
            filename:join([tv_dir(), atom_to_list(Group)]), 
            ".*\.erl$", false, fun (Fn, Acc) -> [Fn | Acc] end, []).

%@doc Get number of config files under cases/Group
get_group_repeat(Group) ->
    length(get_group_files(Group)).

%@doc Get a config for a new run of group.
%  If server is not serving Group, configure is initialized.
get_group_config(Group, ConfServer) ->
    CfgFn = case cur_group(ConfServer) of
        Group ->
            pop_config(ConfServer);
        _ ->
            Terms = smart_sort_strs(get_group_files(Group)),
            store_group_config(ConfServer, {Group, tl(Terms)}),
            hd(Terms)
    end,
    Tag = atom_to_list(Group) ++ [$. | filename:basename(CfgFn, filename:extension(CfgFn))],
    {ok, [R]} = file:consult(CfgFn),
    {Tag, R}.

%@doc Group configure storage loop
group_config_loop({Group, Configs} = State) ->
    receive
        {store, {NewGroup, NewConfigs}} ->
            group_config_loop({NewGroup, NewConfigs});
        {Pid, pop} ->
            Pid ! {pop, hd(Configs)},
            group_config_loop({Group, tl(Configs)});
        {Pid, cur_group} ->
            Pid ! {cur_group, Group},
            group_config_loop(State);
        _ ->
            ok
    end.

%@doc Config tester according to a list of command templates
-spec config_tester(Tester :: pid(), CommandTemplates :: [string()], Acc :: list()) -> {ok, list()} | false.
config_tester(_Tester, [], Acc) -> {ok, lists:reverse(Acc)};
config_tester(Tester, [Command | T], Acc) ->
    case cmd_parser:parse(Command) of
        [Template] ->
            case config_tester0(Tester, Template) of
                {ok, Cmd} -> 
                    config_tester(Tester, T, [Cmd | Acc]);
                true -> 
                    config_tester(Tester, T, Acc);
                _ ->
                    false
            end;
        _ ->
            config_tester(Tester, T, Acc)
    end.

config_tester0(Tester, Template) ->
    case cmd_parser:compose(Template) of
        {ok, Cmd} ->
            % ct:pal("~p~n", [Cmd]),
            case string:str(Cmd, ":ALL") > 1 of
                true ->
                    tester_control:send_command(Tester, Cmd),
                    {ok, Cmd};
                _ ->
                    case  tester_control:exec_confirm(Tester, Cmd) of
                        true -> {ok, Cmd};
                        unknown -> {ok, Cmd};
                        _ -> false
                    end
            end;
        _ -> true
    end.

%@doc Restart UE for a new case
restart_ue(ConfigData) ->
    case ?value(restart, ConfigData) of
        cfun ->
            {ok, Ue10} = ue_control:start(?value(ue_port, ConfigData)),
            ok = ue_control:set_cfun(Ue10, 0),
            ue_control:stop(Ue10);
        modem_reset ->
            {ok, Ue20} = ue_control:start(?value(ue_port, ConfigData)),
            ok = ue_control:set_cfun(Ue20, 0),
            delay(1),
            {"OK", _Response} = ue_control:exec(Ue20, "AT*MODEMRESET"),
            ue_control:stop(Ue20); 
        reboot ->
            ue_control:reboot()
    end.
        
%@doc Restart UE for a new case
wait_ue_restarted(ConfigData) ->
    case ?value(restart, ConfigData) of
        cfun ->
            {ok, Ue10} = ue_control:start(?value(ue_port, ConfigData)),
            ok = ue_control:set_cfun(Ue10, 1),
            {ok, Ue10};
        modem_reset ->
            {ok, Ue20} = ue_control:start(?value(ue_port, ConfigData)),
            delay(1),
            ok = ue_control:set_cfun(Ue20, 1),
            {ok, Ue20}; 
        reboot ->
            ue_control:wait_then_start(?value(ue_port, ConfigData), 120 * 1000);
        _ ->
            ue_control:start(?value(ue_port, ConfigData))
    end.

%@doc Emit a chart
emit_chart(Title, XLable, YLable, Series) ->
    S = "{\"title\": \"" ++ Title  ++ "\",\n"
      "\"xaxis\": \"" ++ XLable ++ "\",\n"
      "\"yaxis\": \"" ++ YLable ++ "\",\n"
      "\"series\": [\n" ++ series_to_str(Series) ++ "\n]}",
    %io:format("~s", [S]),
    ct:log(?chart_class, ?STD_IMPORTANCE, "~s", [S]).

series_to_str([{X, _Y} | _] = S) when is_number(X) ->
    "{"
    "\"data\": [" ++ data_to_str(S) ++ "]}";
series_to_str(Series) ->
    lists:flatten(string:join([serie_to_str(X) || X <- Series], ",\n")).

serie_to_str({Lable, Data}) ->
    "{\"label\": \"" ++ Lable ++ "\",\n"
    "\"data\": [" ++ data_to_str(Data) ++ "]}".

data_to_str(Data) -> 
    lists:flatten(string:join([data_ele_to_str(X) || X <- Data], ",")).

data_ele_to_str(T) when is_tuple(T) ->
    data_ele_to_str(tuple_to_list(T));
data_ele_to_str(L) when is_list(L) ->
    io_lib:format("~p", [L]).
