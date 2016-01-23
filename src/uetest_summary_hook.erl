%%% @doc Common Test Example Common Test Hook module.
-module(uetest_summary_hook).

-include("../include/uetest.hrl").
-include_lib("common_test/include/ct.hrl").

%% Callbacks
-export([init/2]).

-export([post_init_per_group/4]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).

-export([relative_path/2]).

-record(state, {db, tag, verdict, log_fn, time, logdir, reason, phylog_dir, icl_log, phylog_log, keep_phylog, graph_code,
                abort_on_fail, icl_log_dir}).

-define(max_allowed_error, 5).

-define(dbg_on, true).

%% @doc Always called before any other callback function. Use this to initiate
%% any common state. 
init(_Id, Opts) ->
    KeepPhy = ({ok,[[]]} == init:get_argument(keep_phylog)),
    Dir = case init:get_argument(logdir) of
        {ok, [[V]]} -> V;
        _ -> proplists:get_value(dir, Opts, "")
    end,
    AbortOnFail = init:get_argument(abort_on_fail) =:= {ok, [[]]},

    lists:foreach(fun (F) -> file:copy(filename:join([code:priv_dir(uetest), F]), filename:join([Dir, F])) end, 
        ["jquery.flot.axislabels.js", "jquery.flot.min.js"]),
    uelog_control:start([]),
    Fn = filename:join(Dir, proplists:get_value(fn, Opts, ?summary_db_name)),
    PhyDir = case proplists:get_value(phylog, Opts) of
        undefined ->
            D = filename:join(Dir, "phylog"),
            ok = filelib:ensure_dir(D),
            file:make_dir(D),
            D;
        X -> X
    end,
    ok = start_cnt_server(),
    {ok, Bin} = file:read_file(filename:join(code:priv_dir(uetest), "case_graph.html")),
    {ok, Name} = dets:open_file(make_ref(), [{type, bag}, {file, Fn}]),
    {ok, #state{db = Name, logdir = Dir, phylog_dir = PhyDir, keep_phylog = KeepPhy, graph_code = Bin,
            abort_on_fail = AbortOnFail}}.

%this is a very naive and simple solution.
relative_path(Base, Full) ->
    case lists:prefix(string:to_lower(Base), string:to_lower(Full)) of
        true ->
            case string:right(Full, length(Full) - length(Base)) of
                [$/ | T] -> T;
                [$\\ | T] -> T;
                _ -> Full
            end;
        _ -> Full
    end.

%% @doc Called after each init_per_group.
post_init_per_group(_Group, _Config,Return,State) when is_list(Return) ->
    Dir = filename:dirname(proplists:get_value(tc_logfile, Return)),
    SuiteLog = filename:join(Dir, "suite.log.html"),
    Tag = proplists:get_value(case_tag, Return),
    {ICL, IML} = start_log(State, Tag),
    {Return, State#state{tag = Tag, verdict = pass, reason = undefined,
                         log_fn = relative_path(State#state.logdir, SuiteLog), 
                         time = now(),
                         icl_log = ICL,
                         phylog_log = IML}};
post_init_per_group(_Group,_Config,Return, State) ->
    {Return, State#state{tag = undefined}}.

%% @doc Called after each end_per_group. 
post_end_per_group(_Group,_Config,Return,#state{tag = undefined} = State) ->
    {Return, State};
post_end_per_group(_Group,_Config,Return,#state{db = TbName, tag = Tag, log_fn = LogFn, time = Time, 
        verdict = Verdict, reason = Reason} = State) ->
    State10 = stop_log(State),
    dets:insert(TbName, {Tag, [{verdict, Verdict}, {log_fn, LogFn}, {time, Time}, 
                {time, timer:now_diff(now(), Time)/1000000}, {reason, Reason}]}),
    {Return, State10}.

inject_html(Html) ->
    Msg = {log, sync, self(), group_leader(), default, ?STD_IMPORTANCE,
	  [{Html,[]}]},
    case whereis(ct_logs) of
	undefined ->
	    {error,does_not_exist};
	_Pid ->
	    ct_logs ! Msg
    end.

%% @doc Called before init_per_testcase
pre_init_per_testcase(_TestcaseName, InitData, #state{graph_code = Code} = State) ->
    inject_html(binary_to_list(Code)),
    {InitData, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_TC, Reason, #state{abort_on_fail = AbortOnFail} = State)->
    %ct:print("fail: ~p~n", [AbortOnFail]),
    if AbortOnFail -> 
            uelog_control:stop(),
            halt(); 
        true -> ok 
    end,
    State#state{verdict = fail, reason = Reason}.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.  
on_tc_skip(_TC, Reason, #state{verdict = pass} = State) ->
    Reason10 = case Reason of
        {fail, {RunTimeError, StackTrace}} when is_list(StackTrace) -> RunTimeError;
        _ -> Reason
    end,
    State#state{verdict = skip, reason = Reason10};
on_tc_skip(_TC, _Reason, State) ->
    State.

%% @doc Called when the scope of the CTH is done
terminate(State) ->
    dets:close(State#state.db),
    uelog_control:stop(),
    ok.

%@doc Start new ICL/IML log
start_log(#state{phylog_dir = PhyDir} = _State, Tag) ->
    Fn = uetest_utils:make_file_name(Tag),
    uelog_control:new_log(Fn),
    IML = case {PhyDir, get_cnt(Tag)}  of
        {L, N} when is_list(L), N < ?max_allowed_error ->
            IML0 = filename:join([L, Fn ++ ".bin"]),
            uelog_control:new_phylog(fn_to_win(IML0)),
            IML0;
        _ -> none
    end,
    {Fn, IML}.

fn_to_win(Fn) ->
    [case C of $/ -> $\\; _ -> C end || C <- Fn].

delayed_apply(Mod, Fun, Args, Result) ->
    catch lists:foreach(fun (D) ->
                case apply(Mod, Fun, Args) of
                   Result -> throw(ok);
                   _ -> timer:sleep(D)
               end end, [500, 1000, 2000, 2000]).

move_log(LogFn, _StoreDir, {pass, false}) when is_list(LogFn) ->
    delayed_apply(file, delete, [LogFn], ok);
move_log(LogFn, StoreDir, {Verdict, _Keep}) when is_list(LogFn) ->
    Base = filename:basename(LogFn),
    Fn2 = filename:join(StoreDir, filename:rootname(Base) ++ "_" ++ atom_to_list(Verdict) ++ filename:extension(Base)),
    delayed_apply(file, rename, [LogFn, Fn2], ok);
move_log(_, _, _) -> ok.

%@doc Stop ICL/IML log
stop_log(#state{verdict = Verdict, phylog_log = IML1, icl_log = ICL, phylog_dir = PhyPath, icl_log_dir = Path, 
        tag = Tag, keep_phylog = Keep} = State) ->
    uelog_control:close_log(),
    move_log(filename:join(Path, ICL ++ ".icl"), PhyPath, {Verdict, true}),
    case is_list(IML1) of
        true ->
            uelog_control:close_phylog(),
            move_log(IML1, PhyPath, {Verdict, Keep}),
            if Verdict =/= pass -> inc_cnt(Tag); true -> ok end;
        _ -> ok
    end,
    State.

start_cnt_server() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) -> ok;
        _ ->
            spawn(fun () -> register(?MODULE, self()), loop(maps:new()) end),
            ok
    end.

get_cnt(CaseTag) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, get_cnt, CaseTag},
    receive
        {Ref, Cnt} -> Cnt
    end.

inc_cnt(CaseTag) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, inc_cnt, CaseTag}.

loop(Dict) ->
    receive
        {Pid, Ref, get_cnt, CaseTag} ->
            Pid ! {Ref, maps:get(CaseTag, Dict, 0)},
            loop(Dict);
        {_Pid, _Ref, inc_cnt, CaseTag} ->
            loop(maps:put(CaseTag, maps:get(CaseTag, Dict, 0) + 1, Dict));
        _ -> ok
    end.

dbg_print(Term) ->
    case ?dbg_on of
        true -> ct:print("~p", [Term]);
        _ -> ok
    end.
