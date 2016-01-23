%@private
-module(tester_gpib).

-export([open/5, close/1, send/2, read/1]).

-define(command_write_to_gpib ,     0).
-define(command_read_from_gpib,     1).
-define(command_dbg_msg       ,     2).
-define(command_shutdown      ,     3).

open(gpib, Handle, {PAD, SAD}, ServerPid, Opts) when is_integer(PAD) and is_pid(ServerPid) -> 
    init(gpib, Handle, {PAD, SAD}, ServerPid, Opts);
open(visa, Handle, {PAD, SAD}, ServerPid, Opts) when is_integer(PAD) and is_pid(ServerPid) -> 
    init(visa, Handle, {PAD, SAD}, ServerPid, Opts);
open(visa, Board, {IP, Name}, ServerPid, Opts) when is_integer(Board) and is_list(IP) and is_list(Name) -> 
    init(visa, Board, {IP, Name}, ServerPid, Opts).

close(GPIB) -> GPIB ! stop.

send(GPIB, L) ->
    GPIB ! {send, L}.

read(GPIB) ->
    GPIB ! read.

init(gpib, Handle, {PAD, SAD}, ServerPid, Opts) ->
    ExtPrg = case os:type() of
        {win32, _} ->
            [$", filename:join(code:priv_dir(uetest), "gpib.exe\" ")];
        OSType ->
            throw(OSType)
    end,
    Args = build_args([{pad, PAD}, {sad, SAD} | Opts], [" -shutup -port -handle " ++ integer_to_list(Handle)]),
    init0(ServerPid, ExtPrg, Args);
init(visa, Handle, {PAD, SAD}, ServerPid, Opts) when is_integer(PAD) ->
    ExtPrg = case os:type() of
        {win32, _} ->
            [$", filename:join(code:priv_dir(uetest), "gpib_visa.exe\" ")];
        OSType ->
            throw(OSType)
    end,
    Args = build_args([{pad, PAD}, {sad, SAD} | Opts], [" -shutup -port -handle " ++ integer_to_list(Handle)]),
    init0(ServerPid, ExtPrg, Args);
init(visa, Board, {IP, Name}, ServerPid, Opts) ->
    ExtPrg = case os:type() of
        {win32, _} ->
            [$", filename:join(code:priv_dir(uetest), "gpib_visa.exe\" ")];
        OSType ->
            throw(OSType)
    end,
    Args = build_args([{ip, IP}, {name, Name}, {board, Board} | Opts], [" -shutup -port"]),
    init0(ServerPid, ExtPrg, Args).

init0(ServerPid, ExtPrg, Args) ->
    spawn_link(fun () ->
            process_flag(trap_exit, true),
            Port = open_port({spawn, ExtPrg ++ Args}, [{packet, 2}]),
            loop(Port, {ServerPid})
        end).

loop(Port, {ServerPid} = State) ->
    receive
        {Port, {data, [?command_read_from_gpib | Str]}} ->
            gen_server:cast(ServerPid, {gpib_read, self(), Str}),
            loop(Port, {ServerPid});
        {Port, {data, [?command_dbg_msg | Str]}} ->
            io:format("dgb: ~p~n", [Str]),
            loop(Port, State);
        read ->
            Port ! {self(), {command, [?command_read_from_gpib]}},  
            loop(Port, State);
        {send, Msg} ->
            Port ! {self(), {command, [?command_write_to_gpib | Msg]}},  
            loop(Port, State);
        stop ->
            Port ! {self(), {command, [?command_shutdown]}},
            Port ! {self(), close};
        {Port, closed} ->
            exit(normal);
        {'EXIT', Port, _Reason} ->
            exit(port_terminated);
        {'EXIT', _Pid, _Reason} ->
            Port ! {self(), {command, [?command_shutdown]}},
            Port ! {self(), close},
            exit(port_terminated);
        X ->
            io:format("unknown message: ~p~n", [X]),
            loop(Port, State)
    end.

build_args([{pad, V} | Opts], Acc) when is_integer(V) ->
    build_args(Opts, [" -pad ", integer_to_list(V) | Acc]);
build_args([{sad, V} | Opts], Acc) when is_integer(V) ->
    build_args(Opts, [" -sad ", integer_to_list(V) | Acc]);
build_args([{sad, none} | Opts], Acc) ->
    build_args(Opts, Acc);
build_args([{Param, Val} | Opts], Acc) when is_list(Val) and is_atom(Param) ->
    build_args(Opts, [" -", atom_to_list(Param), " ", Val | Acc]);
build_args([{Param, Val} | Opts], Acc) when is_integer(Val) and is_atom(Param) ->
    build_args(Opts, [" -", atom_to_list(Param), " ", integer_to_list(Val) | Acc]);
build_args([], Acc) ->
    lists:concat(Acc).


