%@private
-module(raw_gpib).

-export([open/3, close/1, send/2, read/1]).

-record(raw_state, {
                        server,
                        socket,
                        recv
                   }).

%@doc Open raw tcp connection on Host
%     ServerPid is used for receiving strings from Host, if set as 'undefined', then io:format is used.
-spec open(IP :: string(), Port :: integer(), ServerPid :: pid() | undefined) -> {ok, pid()} | {error, Reason :: any()}.
open(IP, Port, ServerPid) ->
    Ref = make_ref(),
    Self = self(),
    Pid = spawn_link(fun () ->
        process_flag(trap_exit, true),
        {ok, S} = gen_tcp:connect(IP, Port, [list]),
        Self ! {ok, Ref},
        loop(#raw_state{server = ServerPid, socket = S, recv = []})
    end),
    receive
        {ok, Ref} ->
            {ok, Pid}
    after
        5000 -> {error, econn}
    end.

close(GPIB) -> GPIB ! stop.

send(GPIB, L) ->
    GPIB ! {send, L}.

read(_GPIB) ->
    ok.

loop(#raw_state{socket = Socket, server = ServerPid} = State) ->
    receive
        {send, L} ->
            ok = gen_tcp:send(Socket, [L, $\n]),
            loop(State);
        stop ->
            ok = gen_tcp:close(Socket);
        {tcp, Socket, Data} ->
            case is_pid(ServerPid) of
               true -> gen_server:cast(ServerPid, {gpib_read, self(), Data});
               _ -> io:format("~p~n", [Data])
            end,
            loop(State);
        {tcp_closed, _Socket} ->
            exit(tcp_closed);
        {tcp_error, Socket, _Reason} ->
            gen_tcp:close(Socket),
            exit(tcp_error)
    end.

