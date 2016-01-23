%@doc A collection of utilities.

-module(uetest_utils).

-include("../include/uetest.hrl").

-export([delay/1, str_to_num/1, num_to_str/1,
        wait_until/2, nest_until/3,
        load_cmd_template/2, load_cmd_set_file/2, 
        make_file_name/1, cases_dir/0, unconsult/2,
        multi_run/3, band_type/1, move_icl/2]).

-export([start/0, build_doc/0, summary/1]).

-export([mean/1, variance/1, std_var/1]).

-export([proplists_set/3]).

-define(PASS_VERDICT_RATIO, 0.9).

%@doc Get LTE band type (FDD or TDD)
band_type(Band) when Band > 32 -> tdd;
band_type(_Band) -> fdd.

%@doc Delay for x seconds.
delay(Sec) ->
    timer:sleep(Sec * 1000).

%@doc Convert a string into a number which might be an integer or a float.
-spec str_to_num(string()) -> integer() | float() | error.
str_to_num(Str) ->
    case string:to_float(Str) of
        {error, no_float} -> 
            case string:to_integer(Str) of
                {Int, _Rest} when is_integer(Int) ->
                    Int;
                _ -> error
            end;
        {F, _Rest} -> F
    end.

%@doc Convert a string into a number which might be an integer or a float.
-spec num_to_str(Number :: number()) -> string().
num_to_str(Number) ->
    lists:flatten(io_lib:format("~p", [Number])).

%@doc Wait until the result of F() is true, or timed out.
-spec wait_until(fun(), TimeoutSec :: integer()) -> timeout | ok.
wait_until(_F, TimeoutSec) when TimeoutSec =< 0 -> timeout;
wait_until(F, TimeoutSec) ->
    case timer:tc(F) of
        {_Time, true} -> ok;
        {Time, _} ->
            delay(1),
            wait_until(F, TimeoutSec - 1 - Time div 1000000)
    end.

%@doc Wait until the result of F(Acc) returns {true, Result}, or timed out.
-spec nest_until(F :: fun(), Acc :: term(), TimeoutSec :: integer()) -> timeout | {true, Result :: term()}.
nest_until(_F, _Acc, TimeoutSec) when TimeoutSec =< 0 -> timeout;
nest_until(F, Acc, TimeoutSec) ->
    case timer:tc(fun () -> F(Acc) end) of
        {_Time, {true, Result}} -> {true, Result};
        {Time, Acc10} ->
            delay(1),
            nest_until(F, Acc10, TimeoutSec - 1 - Time div 1000000)
    end.

%@doc Load command file in priv directory and parse them into templates.
%    Refer to {@link load_cmd_file/1}.
load_cmd_template(Dir, FileName) ->
    Cmds = load_cmd_file(Dir, FileName),
    lists:concat([ begin case cmd_parser:parse(ACmd) of false -> ""; AStr -> AStr end end || ACmd <- Cmds]).

%@doc Load command file in priv directory.
% Each line start with `"!"' is treated as a command, otherwise as comment.
-spec load_cmd_file(Dir :: string(), FileName :: string()) -> [string()].
load_cmd_file(Dir, FileName) ->
    {ok, Fid} = file:open(filename:join(Dir, FileName), [read]),
    [R] = lists:reverse(load_cmd_file(Fid, [], [])),
    file:close(Fid),
    R.

%@doc Load command set file in priv directory.
% Each line start with `"!"' is treated as a command, otherwise as comment.
% Several groups of commands can be defined, separating by "===============" (a line starging with `"="').
-spec load_cmd_set_file(Dir :: string(), FileName :: string()) -> [[string()]].
load_cmd_set_file(Dir, FileName) ->
    {ok, Fid} = file:open(filename:join(Dir, FileName), [read]),
    R = lists:reverse(load_cmd_file(Fid, [], [])),
    file:close(Fid),
    R.

load_cmd_file(Fid, Acc, SetAcc) ->
    case io:get_line(Fid, "") of
        [$! | Line] ->
            load_cmd_file(Fid, [string:strip(string:strip(Line), right, $\n) | Acc], SetAcc);
        [$= | _Line] ->
            case length(Acc) > 0 of
                true -> load_cmd_file(Fid, [], [lists:reverse(Acc) | SetAcc]);
                _    -> load_cmd_file(Fid, [], SetAcc)
            end;
        eof -> 
            lists:reverse(case length(Acc) > 0 of
                true -> [lists:reverse(Acc) | SetAcc];
                _    -> SetAcc
            end);
        {error, Error} -> {error, Error};
        _ -> load_cmd_file(Fid, Acc, SetAcc)
    end.

%@doc Make a file name by adding a time prefix.
make_file_name(Group) when is_atom(Group) ->
    {{_Y,M,D},{H,N,S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~2..0B-~2..0B_~2..0B.~2..0B.~2..0B_~s", [M,D,H,N,S,atom_to_list(Group)]));
make_file_name(Group) when is_list(Group) ->
    {{_Y,M,D},{H,N,S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~2..0B-~2..0B_~2..0B.~2..0B.~2..0B_~s", [M,D,H,N,S,Group])).

%@doc Call F() for N times, 1 time per Interval second.
multi_run(F, N, Interval) when N > 1 -> multi_run(F, N, [], Interval).

multi_run(F, 1, Acc, _Interval) -> lists:reverse([F() | Acc]);
multi_run(F, N, Acc, Interval) ->
    Acc10 = [F() | Acc],
    delay(Interval),
    multi_run(F, N - 1, Acc10, Interval).

%@doc Calculate mean of a list.
mean(L) ->
    lists:sum(L) / length(L).

%@doc Variance of a list
variance(L) ->
    Mean = mean(L),
    lists:sum([square(X - Mean) || X <- L]).

%@doc Standard variance of a list
std_var(L) ->
    math:sqrt(variance(L)).

square(X) -> X * X.

%@doc Get cases' full directory.
cases_dir() ->
    filename:join([code:lib_dir(uetest), "cases"]).

%@doc Change the value of a key in proplists
proplists_set(Key, Value, List) ->
    [{Key, Value} | proplists:delete(Key, List)].

%@doc Start UETEST in webtool
start() ->
    vts:start(), 
    {vts, L} = vts:config_data(),
    LogDir = lists:foldl(fun 
            ({alias,{"/log_dir",LogDir}}, _Acc) -> LogDir;
            (_, Acc) -> Acc
        end, "", L),
    vts:init_data([], [], LogDir, [], [{filename:join(code:lib_dir(uetest),"suite"),all,all}]),
    os:cmd("start http://localhost:8888").

%@doc Build edoc for uetest.
build_doc() ->
    {ok, Path} = file:get_cwd(),
    file:set_cwd(code:lib_dir(uetest)),
    edoc:application(uetest, [{preprocess, true}, {include, "./include"}, {source_path, ["./suite"]}]),
    file:set_cwd(Path).

%@doc Opposite of consult
unconsult(Fn, L) ->
    {ok, Fid} = file:open(Fn, [write]),
    lists:foreach(fun (X) -> io:format(Fid, "~p.~n", [X]) end, L),
    file:close(Fid).

remove_suffix(Suffix, L) ->
    case lists:suffix(Suffix, L) of
        true -> {true, lists:sublist(L, 1, length(L) - length(Suffix))};
        false -> L
    end.

remove_suffix(L) ->
    case lists:foldl(fun (_Suffix, {true, _L10} = Acc) -> Acc;
                    (Suffix, Acc) -> remove_suffix(Suffix, Acc)
                end, L, ["_fail", "_pass", "_skip"]) of
        {true, X} -> X;
        X -> X
    end.

%@doc move IML logs' corresponding ICL logs to the same dir of IML logs
move_icl(IMLDir, ICLDir) ->
    filelib:fold_files(IMLDir, ".*\.bin$", false, 
        fun (Fn, Acc) -> 
                X = remove_suffix(filename:basename(Fn, ".bin")) ++ ".icl", %
                [file:rename(filename:join(ICLDir, X),filename:join(IMLDir, X)) | Acc]
        end, []).

%@doc Make a summary of all runs of each case.
summary(DbFn) ->
    {Fn, Dir} = case filelib:is_dir(DbFn) of
        true -> {filename:join(DbFn, ?summary_db_name), DbFn};
        _ -> {DbFn, filename:dirname(DbFn)}
    end,
    HtmlFn = filename:join(Dir, "uetest_summary.html"),
    {ok, TbName} = dets:open_file(?summary_db_name, [{file, Fn}, {type, bag}]),
    {ok, Fid} = file:open(HtmlFn, [write]),
    write_header(Fid),
    Keys = lists:sort(fun case_order/2, get_keys(TbName, dets:first(TbName), [])),

    write_summary_table(Fid, lists:map(fun (Key) -> case_verdict(Key, TbName) end, Keys)),

    write_case_table_header(Fid),
    lists:foldl(fun (Key, CntAcc) ->
                process_key(HtmlFn, Fid, Key, TbName, CntAcc) end, 1, Keys),
    write_footer(Fid),
    file:close(Fid),
    dets:close(TbName),
    os:cmd("start " ++ HtmlFn).

get_case_number(Case) ->
    case re:run(Case, "[0-9]+", [global, {capture, all, list}]) of
        {match, L} -> [list_to_integer(X) || [X] <- L];
        _ -> {0}
    end.
        
case_order(Case1, Case2) ->
    get_case_number(Case1) =< get_case_number(Case2).

get_keys(_Tid, '$end_of_table', Acc) -> Acc;
get_keys(Tid, Key, Acc) ->
    get_keys(Tid, dets:next(Tid, Key), [Key | Acc]).

case_class(Case) ->
    "group_" ++ string:join(string:tokens(Case, "."), "_").

case_verdict(Case, Tid) ->
    AllRuns = lists:sort(fun (A, B) -> proplists:get_value(time, A) >= proplists:get_value(time, B) end, 
        [X || [X] <- dets:match(Tid, {Case, '$1'})]),
    AllVerdict = [proplists:get_value(verdict, X) || X <- AllRuns],
    Pass = count(AllVerdict, pass, 0),
    Fail = count(AllVerdict, fail, 0),
    Total = length(AllVerdict),
    Skip = Total - Pass - Fail,
    Rate = case Pass + Fail > 0 of
        true -> Pass / (Pass + Fail);
        _ -> 0.0
    end,
    if 
        Rate > ?PASS_VERDICT_RATIO -> pass;
        Rate > 0 -> unstable;
        Skip == Total -> nt;
        true -> fail
    end.

process_key(_Fn, Fid, Case, Tid, Cnt) ->
    AllRuns = lists:sort(fun (A, B) -> proplists:get_value(time, A) >= proplists:get_value(time, B) end, 
        [X || [X] <- dets:match(Tid, {Case, '$1'})]),
    AllVerdict = [proplists:get_value(verdict, X) || X <- AllRuns],
    Pass = count(AllVerdict, pass, 0),
    Fail = count(AllVerdict, fail, 0),
    Total = length(AllVerdict),
    Skip = Total - Pass - Fail,
    Rate = case Pass + Fail > 0 of
        true -> Pass / (Pass + Fail);
        _ -> 0.0
    end,
    case is_odd(Cnt) of
        true -> file:write(Fid, "<tr class=\"odd\">");
        _ -> file:write(Fid, "<tr class=\"even\">")
    end,
    CaseClass = case_class(Case),

    file:write(Fid, io_lib:format("<td align=center>~p</td>", [Cnt])),

    file:write(Fid, io_lib:format("<td align=center>"
            "<a href=\"javascript:show_none('~s');\">None</a> "
            "<a href=\"javascript:show_all('~s');\">All</a> "
            "<a href=\"javascript:show_pass('~s');\">Pass</a> "
            "<a href=\"javascript:show_fail('~s');\">Fail</a> "
            "<a href=\"javascript:show_skip('~s');\">Skip</a></td><td align=left><b>~s</b></td>\n",
            [CaseClass, CaseClass, CaseClass, CaseClass, CaseClass, Case])),
    file:write(Fid, "<td align=right>" ++ integer_to_list(Total) ++ "</td>\n"),
    file:write(Fid, "<td align=right>" ++ integer_to_list(Pass) ++ "</td>\n"),
    case Fail > 0 of
        true -> file:write(Fid, "<td align=right><font color=\"red\">" ++ integer_to_list(Fail) ++ "</font></td>\n");
        _ -> file:write(Fid, "<td align=right>0</td>\n")
    end,
    case Skip > 0 of
        true -> file:write(Fid, "<td align=right><font color=\"brown\">" ++ integer_to_list(Skip) ++ "</font></td>\n");
        _ -> file:write(Fid, "<td align=right>0</td>\n")
    end,
    file:write(Fid, ["<td align=right>", io_lib:format("~.2f%", [100 * Rate]),"</td></tr>\n"]),
    lists:foldl(fun (Run, CntAcc) -> write_runs(CaseClass, Fid, Run, CntAcc), CntAcc + 1 end,
                Cnt + 1, AllRuns),
    Cnt + 1.

write_runs(CaseClass, Fid, Run, Cnt) ->
    Verdict = proplists:get_value(verdict, Run), 
    Class = case is_odd(Cnt) of true -> "odd"; _ -> "even" end,
    file:write(Fid, ["<tr class=\"", Class, " tr-hidden ", CaseClass, "_", atom_to_list(Verdict), "\">"]),
    file:write(Fid, ["<td></td><td></td>\n<td align=center><a href=\"", proplists:get_value(log_fn, Run), 
            "\">", format_now(proplists:get_value(time, Run)), "</a></td>\n"]),
    case Verdict of
        pass -> file:write(Fid, "<td align=center><font color=\"green\">PASS</font></td>\n");
        fail -> file:write(Fid, "<td align=center><font color=\"red\">FAIL</font></td>\n");
        _    -> file:write(Fid, "<td align=center><font color=\"brown\">SKIP</font></td>\n")
    end,
    file:write(Fid, "<td></td><td></td><td align=right>"),
    case proplists:get_value(time, Run) of
        T when is_float(T) ->
            io:format(Fid, "~.1fs", [T]);
        _ -> ok
    end,
    file:write(Fid, "</td><td align=left>\n"),
    case proplists:get_value(reason, Run) of
        undefined -> ok;
        {failed, {RunTimeError, [{M, F, _A, Props} | _]}} -> 
            io:format(Fid, "<font color=\"red\">~p</font>"
                           "<br/><font color=\"brown\">~p</font>", 
                           [{M, F, proplists:get_value(line, Props)}, RunTimeError]);
        {failed, {test_case_failed, CaseReason}} ->
            io:format(Fid, "<font color=\"red\">test_case_failed</font>"
                           "<br/><font color=\"brown\">~p</font>", 
                           [CaseReason]);
        {failed, {Error, _More}} -> 
            io:format(Fid, "<font color=\"brown\">~p</font>", [Error]);
        Reason -> 
            io:format(Fid, "<font color=\"brown\">~p</font>", [Reason])
    end,
    file:write(Fid, "</td></tr>\n").

format_now(Now) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time(Now),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
			    "Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~s ~2w ~4w ~2w:~2..0w:~2..0w",
		  [Mstr,Day,Year,Hour,Minute,Second]).

is_odd(N) when N rem 2 == 1 -> true;
is_odd(_) -> false.

count(L, Pattern) -> count(L, Pattern, 0).

count([], _Pattern, Acc) -> Acc;
count([Pattern | T], Pattern, Acc) ->
    count(T, Pattern, Acc + 1);
count([_X | T], Pattern, Acc) ->
    count(T, Pattern, Acc).

write_summary_table(Fid, L) ->
    Content = io_lib:format("
    <table>
        <thead>
        <tr>
        <th style=\"background:#50ff50;text-decoration:none;color:black\"><b>PASS</b><br />(> ~p%)</th>
        <th style=\"background:yellow;text-decoration:none;color:black\"><b>UNSTABLE</b><br />(&le; ~p%)</th>
        <th style=\"background:red;text-decoration:none;color:black\"><b>FAIL</b></th>
        <th style=\"background:transparent;text-decoration:none;color:black\"><b>NT</b></th>
        <th style=\"background:transparent;text-decoration:none;color:black\"><b>TOTAL</b></th>
        </tr>
        </thead>
        <tbody>", [100 * ?PASS_VERDICT_RATIO, 100 * ?PASS_VERDICT_RATIO]),
    file:write(Fid, Content),
    Line = io_lib:format("<tr>
        <td bgcolor=#50ff50>~p</td>
        <td bgcolor=\"yellow\">~p</td>
        <td bgcolor=\"red\">~p</td>
        <td>~p</td>
        <td>~p</td>
        </tr></tbody></table>", 
        [count(L, pass), count(L, unstable), count(L, fail), count(L, nt), length(L)]),
    file:write(Fid, [Line, "<br /><br />"]).

write_case_table_header(Fid) ->
    Content = "
    <table id=\"SortableTable\">
        <thead>
        <tr>
        <th><b>Index</b></th>
        <th><b>Show</b></th>
        <th><b>Case</b><br/>(History)</th>
        <th><b>Total</b><br/>(Verdict)</th>
        <th><b>Pass</b></th>
        <th><b>Failed</b></th>
        <th><b>Skipped</b><br/>(Time<sub>(s)</sub>)</th>
        <th><b>Pass Rate</b><br/>(Reason)</th>
        </tr>
        </thead>
        <tbody>",
    file:write(Fid, Content).

write_header(Fid) ->
    Content =       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
        <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
        <!-- autogenerated by 'uetest' -->
        <head>
        <title>Summary of all runs of each case</title>
        <meta http-equiv=\"cache-control\" content=\"no-cache\">
        <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">
        <link rel=\"stylesheet\" href=\"ct_default.css\" type=\"text/css\">
        <script type=\"text/javascript\" src=\"jquery-latest.js\"></script>
        <script type=\"text/javascript\" src=\"jquery.tablesorter.min.js\"></script>
        <style>
            .even {}
            .tr-hidden {
                display: none;
            }
        </style>        
        <script type=\"text/javascript\">
function show_all(group)
{
    show_type(group, 'pass', true);
    show_type(group, 'fail', true);
    show_type(group, 'skip', true);
}

function show_none(group)
{
    show_type(group, 'pass', false);
    show_type(group, 'fail', false);
    show_type(group, 'skip', false);
}
function show_pass(group)
{
    show_type(group, 'pass', true);
}

function show_fail(group)
{
    show_type(group, 'fail', true);
}

function show_skip(group)
{
    show_type(group, 'skip', true);
}

function show_type(group, type, show)
{
    var name = '.' + group + '_' + type;
    if (show)
        $(name).css('display', 'table-row');
    else
        $(name).css('display', 'none');
}
        </script>
        </head> <body> <center> <h1>Summary of all runs of each case</h1> 
        <div id=\"button_holder\" class=\"btn\">
        </div><br /><br />",
    file:write(Fid, Content).

write_footer(Fid) ->
    file:write(Fid, " </tbody> </table> </center> <br /><br /> <center> <div class=\"copyright\">Updated: "),
    file:write(Fid, format_now(now())),
    file:write(Fid, "</center></body></html>").


