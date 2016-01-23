%@doc A minimun implementation of HiSLIP 1.0.
-module(hislip_gpib).

-export([open/3, close/1, send/2, read/1]).

-record(hislip_state, {
                        server,
                        sync,
                        async,
                        sync_recv = <<>>,
                        async_recv = <<>>,
                        session,
                        msg_cnt = 16#ffffff00,
                        msg_acc = <<>>,
                        data,
                        overlap
                   }).

-define(HISLIP_VER, 16#0100).
-define(HISLIP_PORT, 4880).

%@doc Open raw tcp connection on Host
%     ServerPid is used for receiving strings from Host, if set as 'undefined', then io:format is used.
-spec open(IP :: string(), Name :: string(), ServerPid :: pid() | undefined) -> {ok, pid()} | {error, Reason :: any()}.
open(IP, Name, ServerPid) -> open(IP, ?HISLIP_PORT, Name, ServerPid).

-define('Initialize', 0).
-define('InitializeResponse', 1).
-define('FatalError', 2).
-define('Error', 3).
-define('AsyncLock', 4).
-define('AsyncLockResponse', 5).
-define('Data', 6).
-define('DataEnd', 7).
-define('DeviceClearComplete', 8).
-define('DeviceClearAcknowledge', 9).
-define('AsyncRemoteLocalControl', 10).
-define('AsyncRemoteLocalResponse', 11).
-define('Trigger', 12).
-define('Interrupted', 13).
-define('AsyncInterrupted', 14).
-define('AsyncMaximumMessageSize', 15).
-define('AsyncMaximumMessageSizeResponse', 16).
-define('AsyncInitialize', 17).
-define('AsyncInitializeResponse', 18).
-define('AsyncDeviceClear', 19).
-define('AsyncServiceRequest', 20).
-define('AsyncStatusQuery', 21).
-define('AsyncStatusResponse', 22).
-define('AsyncDeviceClearAcknowledge', 	23).
-define('AsyncLockInfo', 24).
-define('AsyncLockInfoResponse', 25).

open(IP, Port, Name, ServerPid) ->
    Ref = make_ref(),
    Self = self(),
    Pid = spawn_link(fun () ->
        process_flag(trap_exit, true),
        {ok, SSocket} = gen_tcp:connect(IP, Port, [binary]),
        S = length(Name),
        B = list_to_binary(Name),
        Msg = <<$H, $S, ?'Initialize', 0, ?HISLIP_VER:16, 0:16, S:64, B/binary>>,
        ok = gen_tcp:send(SSocket, Msg),
        initialize(#hislip_state{server = ServerPid, sync = SSocket}, Self, Ref)
    end),
    receive
        {ok, Ref} ->
            {ok, Pid}
    after
        5000 -> {error, econn}
    end.

close(Pid) -> Pid ! close.

send(Pid, Msg) when is_list(Msg) ->
    send(Pid, list_to_binary(Msg ++ "\n"));
send(Pid, Msg) when is_binary(Msg) ->
    Pid ! {send, Msg}.

read(_Pid) ->
    ok.

initialize(#hislip_state{sync = SSocket, async = ASocket, sync_recv = SRecv, async_recv = ARecv} = State, Pid, Ref) ->
    receive
        {tcp, SSocket, Data} ->
            %dbg(Data),
            {ok, SRecv10} = handle_message(<<SRecv/binary, Data/binary>>, self(), SSocket),
            initialize(State#hislip_state{sync_recv = SRecv10}, Pid, Ref);
        {tcp, ASocket, Data} ->
            {ok, ARecv10} = handle_message(<<ARecv/binary, Data/binary>>, self(), ASocket),
            initialize(State#hislip_state{async_recv = ARecv10}, Pid, Ref);
        {msg, SSocket, <<?'InitializeResponse', Overlap, _ServerVer:16, SessionID:16>>, <<>>} ->
            Msg2 = <<$H, $S, ?'AsyncInitialize', 0, SessionID:32, 0:64>>,
            {ok, {Address, Port}} = inet:peername(SSocket),
            {ok, ASocket10} = gen_tcp:connect(Address, Port, [binary]),
            ok = gen_tcp:send(ASocket10, Msg2),
            initialize(State#hislip_state{async = ASocket10, session = SessionID, overlap = Overlap}, Pid, Ref);
        {msg, ASocket, <<?'AsyncInitializeResponse', 0, _VendorID:32>>, <<>>} ->
            %dbg("connected"),
            Pid ! {ok, Ref},
            loop(State);
        {msg, _Socket, <<?'FatalError', Code, 0:32>>, ErrMsg} ->
            exit({hislip_fatal, {Code, ErrMsg}})
    end.

loop(#hislip_state{server = ServerPid, sync = SSocket, async = ASocket, sync_recv = SRecv, async_recv = ARecv, msg_cnt = Counter} = State) ->
    receive
        {send, L} ->
            S = size(L),
            B = <<$H, $S, ?'DataEnd', 1, Counter:32, S:64, L/binary>>,
            ok = gen_tcp:send(SSocket, B),
            loop(State#hislip_state{msg_cnt = (Counter + 2) band 16#FFFFFFFF});
        stop ->
            ok = gen_tcp:close(ASocket),
            ok = gen_tcp:close(SSocket);
        {tcp, SSocket, Data} ->
            {ok, SRecv10} = handle_message(<<SRecv/binary, Data/binary>>, self(), SSocket),
            loop(State#hislip_state{sync_recv = SRecv10});
        {tcp, ASocket, Data} ->
            {ok, ARecv10} = handle_message(<<ARecv/binary, Data/binary>>, self(), ASocket),
            loop(State#hislip_state{async_recv = ARecv10});
        {msg, SSocket, <<?'Data', 0, _MsgCnt:32>>, Msg} ->
            Old = State#hislip_state.msg_acc,
            MsgAcc = <<Old/binary, Msg/binary>>,
            loop(State#hislip_state{msg_acc = MsgAcc});
        {msg, SSocket, <<?'DataEnd', 0, _MsgCnt:32>>, Msg} ->
            Old = State#hislip_state.msg_acc,
            MsgAcc = binary_to_list(<<Old/binary, Msg/binary>>),
            case is_pid(ServerPid) of
                true -> gen_server:cast(ServerPid, {gpib_read, self(), MsgAcc});
               _ -> io:format("~p~n", [MsgAcc])
            end,
            loop(State#hislip_state{msg_acc = <<>>});
        {msg, _Socket, <<?'FatalError', Code, 0:32>>, ErrMsg} ->
            throw({fatal, {Code, ErrMsg}});
        {msg, _Socket, MsgHeader, Msg} ->
            throw({unknown, MsgHeader, Msg});
        {tcp_closed, SSocket} ->
            gen_tcp:close(ASocket),
            exit(tcp_closed);
        {tcp_closed, ASocket} ->
            gen_tcp:close(SSocket),
            exit(tcp_closed);
        {tcp_error, _Socket, _Reason} ->
            gen_tcp:close(SSocket),
            gen_tcp:close(ASocket),
            exit(tcp_error)
    end.

handle_message(<<$H, $S, MessageId, Ctrl, Param:32, Len:64, Bin/binary>> = Data, Pid, Socket) ->
    case Bin of
        <<X:Len/binary, B/binary>> ->
            Pid ! {msg, Socket, <<MessageId, Ctrl, Param:32>>, X},
            handle_message(B, Pid, Socket);
        _ -> {ok, Data}
    end;
handle_message(<<>>, _Pid, _Socket) ->
    {ok, <<>>};
handle_message(<<X, _Bin/binary>> = _Data, _Pid, _Socket) when X /= $H ->
    error.

dbg(Term) -> io:format("~p~n", [Term]).
