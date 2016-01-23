%@private

-module(at_adb).

-export([open/1, open/0, open/3, close/1, send/2, set_crlf/2, get_read_buf/1]).

open() -> 
    init([], self(), "-1").

open(DeviceNo) -> 
    init(DeviceNo, self(), "-1").

open(DeviceNo, ServerPid, Arg) -> 
    init(DeviceNo, ServerPid, Arg).

close(Uart) -> Uart ! stop.

send(Uart, L) ->
    Uart ! {send, L}.

set_crlf(_Uart, {_CR, _LF}) ->
    ok.

get_read_buf(_Uart) -> [].

% Open Default device:
%    for sim1:  -1,  --sim1
%    sim1 default device is /tmp/atcmd
%    for sim2:  -2,  --sim2
%    sim2 default device is /tmp/atcmd1
init(DeviceNo, ServerPid, Arg) ->
    Args = case DeviceNo of
        [] -> "";
        _ -> "-d " ++ DeviceNo
    end, 
    spawn_link(fun () ->
            process_flag(trap_exit, true),
            Port = open_port({spawn, "adb shell" ++ Args}, [{line, 65535}]),
            timer:sleep(100),
            Port ! {self(), {command, "serial_client " ++ Arg ++ "\n"}},
            loop(Port, {ServerPid, []})
        end).

loop(Port, {ServerPid, Acc} = State) ->
    receive 
        {Port, {data, {eol, Line}}} ->
            Striped = string:strip(Acc ++ Line, right, $\r),
            %io:format("data: ~p~n", [Striped]),
            gen_server:cast(ServerPid, {uart_lines, self(), [Striped]}),
            loop(Port, {ServerPid, []});
        {Port, {data, {noeol, Line}}} ->
            loop(Port, {ServerPid, Acc ++ Line});
        {send, Line} ->
            %io:format("send: ~p~n", [Line]),
            Port ! {self(), {command, Line ++ "\n"}},
            loop(Port, State);
        stop ->
            Port ! {self(), {command, [3, $\n]}},
            Port ! {self(), close};
        {Port, closed} ->
            exit(normal);
        {'EXIT', _Port, normal} ->
            normal;
        {'EXIT', _Port, _Reason} ->
            exit(port_terminated);
        X ->
            io:format("at_adb: unknown message: ~p~n", [X]),
            loop(Port, State)
    end.

