%@private

-module(at_uart).

-export([open/1, open/2, open/3, close/1, send/2, set_crlf/2, get_read_buf/1]).

-define(command_write_to_uart ,     0).
-define(command_read_from_uart,     1).
-define(command_dbg_msg       ,     2).
-define(command_shutdown      ,     3).

open(DeviceNo) when is_integer(DeviceNo) -> 
    init(DeviceNo, self(), []).

open(DeviceNo, Opts) when is_integer(DeviceNo) -> 
    init(DeviceNo, self(), Opts).

open(DeviceNo, ServerPid, Opts) when is_integer(DeviceNo) -> 
    init(DeviceNo, ServerPid, Opts).

close(Uart) -> Uart ! stop.

send(Uart, L) ->
    Uart ! {send, L}.

set_crlf(Uart, {CR, LF}) ->
    Uart ! {set_crfl, {CR, LF}}.

get_read_buf(Uart) ->
    Uart ! {get_read_buf, self()},
    receive
        {Uart, Buf} -> Buf
    end.

init(DeviceNo, ServerPid, Opts) ->
    CRLF = proplists:get_value(crlf, Opts, {13, 10}),
    ExtPrg = case os:type() of
        {win32, _} ->
            [$" | filename:join(code:priv_dir(uetest), "uart_port.exe\" ")];
        OSType ->
            throw(OSType)
    end,
    Args = build_args(Opts, ["-port " ++ integer_to_list(DeviceNo)]),
    spawn_link(fun () ->
            process_flag(trap_exit, true),
            Port = open_port({spawn, ExtPrg ++ Args}, [{packet, 2}]),
            loop(Port, {ServerPid, CRLF, []})
        end).

loop(Port, {ServerPid, CRLF, Acc} = State) ->
    receive
        {Port, {data, [?command_read_from_uart | Str]}} ->
            {L, Acc1} = decode(CRLF, Acc ++ Str, [], []),
            gen_server:cast(ServerPid, {uart_lines, self(), L}),
            loop(Port, {ServerPid, CRLF, Acc1});
        {Port, {data, [?command_dbg_msg | Str]}} ->
            io:format("COM dbg: ~s~n", [Str]),
            loop(Port, {ServerPid, CRLF, Acc});
        {send, Msg} ->
            Port ! {self(), {command, [?command_write_to_uart, Msg, element(1, CRLF)]}}, % 
            loop(Port, State);
        {get_read_buf, Pid} ->
            Pid ! {self(), Acc},
            loop(Port, {ServerPid, CRLF, []});
        stop ->
            Port ! {self(), {command, [?command_shutdown]}},
            Port ! {self(), close};
        {Port, closed} ->
            exit(normal);
        {'EXIT', _Pid, _Reason} ->
            exit(port_terminated);
        X ->
            io:format("unknow message: ~p~n", [X]),
            loop(Port, State)
    end.

% TODO: performance
decode({CR, LF}, [C1, C2 | T], Ls, Acc) when (C1 == CR) and (C2 == LF) ->
    decode({CR, LF}, T, [lists:reverse(Acc) | Ls], []);
decode(CRLF, [C | T], Ls, Acc) ->
    decode(CRLF, T, Ls, [C | Acc]);
decode(_CRLF, [], Ls, Acc) ->
    {lists:reverse(Ls), lists:reverse(Acc)}.

build_args([{baud, V} | Opts], Acc) ->
    build_args(Opts, [" -baud ", integer_to_list(V) | Acc]);
build_args([{databits, V} | Opts], Acc) ->
    build_args(Opts, [" -databits ", integer_to_list(V) | Acc]);
build_args([{stopbits, V} | Opts], Acc) ->
    build_args(Opts, [" -stopbits ", integer_to_list(V) | Acc]);
build_args([{parity, V} | Opts], Acc) ->
    build_args(Opts, [" -parity ", atom_to_list(V) | Acc]);
build_args([_X | Opts], Acc) ->
    build_args(Opts, Acc);
build_args([], Acc) ->
    lists:concat(Acc).

