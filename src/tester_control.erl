%@doc Tester control.
-module(tester_control).

-export([start/1, start/2, start/4, stop/1]).

-export([dev_name/1, cmd/2, batch/2, send_command/2, send_query/2, send_query/3, send_query/4, exec_confirm/2]).
-export([completed/1, wait_op/2]).
-export([set_id/2, get_id/1]).

-record(state,
    {
        channel,
        data = dict:new(),
        driver = tester_gpib,
        caller,
        id
    }).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%@doc Start gpib connection
start(gpib) -> 
    start(gpib, 1);

%@doc Start gpib connection var visa driver
start(visa) -> 
    start(visa, 1).

%@doc Start gpib connection on a parimary address (PAD)
start(gpib, PAD) when is_integer(PAD) -> 
    start(gpib, 0, {PAD, none}, []);

%@doc Start gpib connection on a parimary address (PAD) var visa driver
start(visa, PAD) when is_integer(PAD) -> 
    start(visa, 0, {PAD, none}, []);

%@doc Start lan-gpib connection on {IP, Name}.
start(lan, {IP, Name}) -> 
    start(lan, 0, {IP, Name}, []);

%@doc Start lan-gpib (vxi11.3) connection on {IP, Name}.
start(vxi11, {IP, Name}) -> 
    gen_server:start_link(?MODULE, [vxi11, {IP, Name}], []);

%@doc Start raw tcp based gpib connection on {IP, Port}.
start(tcp, {IP, Port}) -> 
    gen_server:start_link(?MODULE, [tcp, {IP, Port}], []);

%@doc Start HiSLIP based gpib connection on {IP, Name}.
start(hislip, {IP, Name}) -> 
    gen_server:start_link(?MODULE, [hislip, {IP, Name}], []).

%@doc Start gpib connection on a parimary/secondary address ({PAD, SAD}) with options,
%     Handle specfies the gpib device.
start(gpib, Handle, {PAD, SAD}, Opts) when is_integer(PAD) -> 
    gen_server:start_link(?MODULE, [gpib, Handle, {PAD, SAD}, Opts], []);

%@doc Start gpib connection on a parimary/secondary address ({PAD, SAD}) with options var visa driver,
%     Handle specfies the gpib device.
start(visa, Handle, {PAD, SAD}, Opts) when is_integer(PAD) -> 
    gen_server:start_link(?MODULE, [visa, Handle, {PAD, SAD}, Opts], []);

%@doc Start lan-gpib connection on {IP, Name} with options,
%     Board specfies the index of used LAN interface.
start(lan, Board, {IP, Name}, Opts) -> 
    gen_server:start_link(?MODULE, [visa, Board, {IP, Name}, Opts], []).

%@doc Stop a gpib connection.
stop(Server) ->
    gen_server:cast(Server, stop).

%@doc Get the name/revision of the connected device.
dev_name(Server) ->
    send_query(Server, "*IDN?").

%@doc Query operation is completed or not.
completed(Server) ->
    send_query(Server, "*OPC?") == "1".

%@doc Wait until measurement is done
wait_op(Server, TimeoutSec) ->
    uetest_utils:wait_until(fun () -> 
                tester_control:send_query(Server, "*OPC?") == "1" 
        end, TimeoutSec).

%@doc Set anything as an identifier for this tester/cell.
set_id(Server, Id) ->
    gen_server:call(Server, {set_id, Id}).

%@doc Get the identifier of this tester/cell.
get_id(Server) ->
    gen_server:call(Server, get_id).

trim([$ ]) -> [];
trim([9]) -> [];
trim([$  | T]) -> trim(T);
trim([9 | T]) -> trim(T);
trim(T) -> T.

%@doc Send a command or make a query to device by checking if there is `"?"' at the end of Command.
% See {@link send_command/2} and  {@link send_query/2}.
cmd(Server, Command) ->
    Command10R = trim(lists:reverse(Command)),
    Command10  = lists:reverse(Command10R),
    case Command10R of
        [$? | _T] -> send_query(Server, Command10);
        _ -> send_command(Server, Command10)
    end.

%@doc Send a list of command to device.
batch(Server, L) ->
    lists:foreach(fun (Command) -> 
                send_command(Server, Command),
                receive after 100 -> ok end
        end, L).

%@doc Send a command and try to verify the command is executed correctly.
-spec exec_confirm(Server :: pid(), Cmd :: iolist()) -> unknown | true | false.
exec_confirm(Server, Cmd) ->
    case lists:splitwith(fun (C) -> C /= $  end, Cmd) of
        {Prefix, Param} when length(Param) > 0 -> 
            send_command(Server, Cmd),
            case send_query(Server, Prefix ++ "?") of
                error -> unknown;
                Response -> response_compare(Response, string:strip(Param))
            end
    end.

response_compare([], _Param) -> false;
response_compare("1", "ON") -> true;
response_compare("0", "OFF") -> true;
response_compare(Response, Param) -> 
    F = (Response =:= Param) orelse lists:prefix(Response, Param),
    case F of
        false -> 
            V1 = uetest_utils:str_to_num(Response),
            V2 = uetest_utils:str_to_num(Param),
            is_number(V1) andalso is_number(V2) andalso V1 == V2;
        true -> true
    end.

%@doc Send a command to device.
-spec send_command(Server :: pid(), Command :: iolist()) -> any().
send_command(Server, Command) ->
    gen_server:call(Server, {send, self(), 0, Command}).

%@doc Send a query to device and read response.
-spec send_query(Server :: pid(), Command :: iolist()) -> string() | error.
send_query(Server, Command) ->
    case send_query(Server, Command, 1) of
        [Response] -> Response;
        _ -> error
    end.

%@doc Send a query to device and read N lines of response.
send_query(Server, Command, N) ->
    send_query(Server, Command, N, 4000).

%@doc Send a query to device and read N lines of response with time-out. 
send_query(Server, Command, N, Timeout) ->
    send_query(Server, Command, N, Timeout, []).

send_query(Server, Command, N, Timeout, Acc) ->
    Ref = make_ref(),
    gen_server:call(Server, {send, self(), Ref, Command}),
    collect(Server, Ref, N, Timeout, Acc).

collect(_Server, _Ref, 0, _Timeout, Acc) -> lists:reverse(Acc);
collect(Server, Ref, N, Timeout, Acc) ->
    gen_server:call(Server, read),
    receive
        {Ref, S} ->
            collect(Server, Ref, N - 1, Timeout, [S | Acc])
    after
        Timeout ->
            lists:reverse(Acc)
    end.

%
%  gen_server behaviour
%

%@private
init([vxi11, {IP, Name}]) ->
    {ok, Gpib} = vxi_gpib:open(IP, Name, self()),
    {ok, #state{channel = {gpib, Gpib}, driver = vxi_gpib}};
init([hislip, {IP, Name}]) ->
    {ok, Gpib} = hislip_gpib:open(IP, Name, self()),
    {ok, #state{channel = {gpib, Gpib}, driver = hislip_gpib}};
init([visa, {IP, Name}]) ->
    {ok, Gpib} = vxi_gpib:open(IP, Name, self()),
    {ok, #state{channel = {gpib, Gpib}, driver = vxi_gpib}};
init([tcp, {IP, Port}]) ->
    {ok, Gpib} = raw_gpib:open(IP, Port, self()),
    {ok, #state{channel = {gpib, Gpib}, driver = raw_gpib}};
init([Driver, P0, {P1, P2}, Opts]) ->
    Gpib = tester_gpib:open(Driver, P0, {P1, P2}, self(), Opts),
    {ok, #state{channel = {gpib, Gpib}, driver = tester_gpib}}.

%@private
handle_call({send, Pid, Ref, L}, _From, #state{channel = {gpib, Gpib}, driver = Mod} = State) ->
    Mod:send(Gpib, L),
    {reply, ok, State#state{caller = {Pid, Ref}}};
handle_call(read, _From, #state{channel = {gpib, Gpib}, driver = Mod} = State) ->
    Mod:read(Gpib),
    {reply, ok, State};
handle_call({set_id, Id}, _From, State) ->
    {reply, ok, State#state{id = Id}};
handle_call(get_id, _From, #state{id = Id} = State) ->
    {reply, Id, State}.

%@private
handle_cast({gpib_read, _Pid, L}, #state{caller = {Pid, Ref}} = State) when is_pid(Pid) ->
    Pid ! {Ref, string:strip(L, right, 10)},
    {noreply, State};
handle_cast({gpib_lines, _Pid, _L}, State) ->
    {noreply, State};
handle_cast(stop, #state{channel = {gpib, Gpib}, driver = Mod} = State) ->
    Mod:close(Gpib),
    {stop, normal, State}.

%@private
handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("~p~n", [{'EXIT', Pid, Reason}]),
    {noreply, State}.

%@private
terminate(normal, _State) ->
    ok.

%@private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
