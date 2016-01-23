%@doc UE control through AT commands.
-module(ue_control).

-behaviour(gen_server).

-export([start/1, stop/1, wait_then_start/1, wait_then_start/2]).
-export([exec/2, exec/3]).
-export([reboot/0, ready/1, set_cfun/2, plmn/1]).
-export([signal_power/1, extended_signal_quality/1, scell/1]).
-export([dial/2, hangup/1, answer/1, get_scell/1]).
-export([send_sms/3, pdp_context/1, engineering_mode/2, all_cells_quality/1, vzw_rsrp/1]).
-export([set_band/2]).
-export([stay_on_while_plugged_in/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
    {
        mod,
        uart,
        data,
        cmd_ref,
        final,
        final_set = sets:from_list(["OK", "NO CARRIER", "ERROR", "+CME ERROR", "+CMS ERROR",
                                    "COMMAND NOT SUPPORT", "TOO MANY PARAMETERS"])
    }).

-type gsm_quality() :: {gsm, RxLev_dBm :: integer(), Ber :: integer()}.
-type tdscdma_quality() :: {tdscdma, Rscp_dBm :: integer()}.
-type wcdma_quality() :: {wcdma, Rscp_dBm :: integer(), Ecno :: float()}.
-type lte_quality() :: {lte, Rsrp_dBm :: integer(), Rsrq_dB :: float()}.
-type cell_quality() :: gsm_quality() | tdscdma_quality() | wcdma_quality() | lte_quality().

%@doc Start on PortNo
-spec start(PortNo :: integer()) -> {ok, Server :: pid()} | ignore | {error, Error :: any()}.
start(PortNo) when is_integer(PortNo) ->
    gen_server:start_link(?MODULE, [PortNo], []);
start(adb) ->
    gen_server:start_link(?MODULE, [adb], []).

%@doc Reboot UE by `adb reboot'
reboot() ->
    os:cmd("adb reboot").

%@doc Toggle Android's stay_on_while_plugged_in property
stay_on_while_plugged_in(Value) ->
    adb_modify_settings("global", "stay_on_while_plugged_in", if Value -> "3"; true -> "0" end).

%@doc Wait util AT is ready on PortNo.
wait_then_start(PortNo) ->
    wait_then_start(PortNo, 50000).

%@doc Wait util AT is ready on PortNo with a time out.
wait_then_start(_PortNo, Timeout) when Timeout =< 0 -> timeout;
wait_then_start(PortNo, Timeout) ->
    This = self(),
    spawn(fun () ->
                {ok, Pid} = start(PortNo),
                case ready(Pid) of
                    ok ->
                        receive 
                        after
                            5000 ->
                              This ! {started, Pid}
                        end;
                    _ ->
                        stop(Pid),
                        exit(normal)
                end
        end),
    receive
        {started, AServer} ->
            {ok, AServer}
    after 
        8000 -> wait_then_start(PortNo, Timeout - 8000)
    end.

%@doc CS call.
dial(Server, Number) when is_integer(Number) -> dial(Server, integer_to_list(Number));
dial(Server, Number) when is_list(Number) ->
    case exec(Server, "ATD" ++ Number ++ ";") of
        {"OK", _X} ->
            ok;
        {Error, _} -> 
            {error, Error}
    end.

%@doc Hang up CS call.
hangup(Server) ->
    case exec(Server, "AT+CHUP") of
        {"OK", _X} ->
            ok;
        {Error, _} -> 
            {error, Error}
    end.

%@doc Answer an incoming call.
answer(Server) ->
    case exec(Server, "ATA") of
        {"OK", _X} ->
            ok;
        {Error, _} -> 
            {error, Error}
    end.

%@doc Check if AT is ready.
-spec ready(Server :: pid()) -> ok | {error, Reason :: any()}.
ready(Server) ->
    case exec(Server, "AT", 1000) of
        {"OK", _X} ->
            ok;
        {Error, _} -> 
            {error, Error}
    end.

%@doc Send an SMS (ASCII only).
send_sms(Server, Receiver, Msg) ->
    case exec(Server, "AT+CMGF=1") of
        {"OK", _} ->
            Ref = make_ref(),
            gen_server:call(Server, {send, self(), Ref, "AT+CMGS=" ++ Receiver}),
            timer:sleep(100),
            case peek_uart_out(Server) of
                [[],{"> ",[]}] ->
                    gen_server:call(Server, {send, self(), Ref, Msg ++ [26]}),
                    receive
                        {Ref, {"OK", _}} ->
                            ok;
                        {Ref, ErrResponse} -> 
                            {error, ErrResponse}
                    after
                        120000 ->
                            {error, timeout}
                    end;
                CMGS ->
                    {error, CMGS}
            end;
        {Error, F} -> 
            {error, {Error, F}}
    end.

%@doc Set cfun value.
set_cfun(Server, N) ->
    case exec(Server, "AT+CFUN=" ++ integer_to_list(N)) of
        {"OK", _} ->
            ok;
        {Error, _} ->
            {error, Error}
    end.

%@doc Set UE band mode using AT*BAND.
%     Possible to set to LTE mode only, or triple mode
set_band(Server, lte_only) -> set_band(Server, 5);
set_band(Server, triple) -> set_band(Server, 12);
set_band(Server, Band) when is_integer(Band) ->
    case exec(Server, "AT*BAND=" ++ integer_to_list(Band)) of
        {"OK", _} ->
            ok;
        {Error, _} ->
            {error, Error}
    end.

%@doc Set engineering mode.
engineering_mode(Server, Mode) when is_integer(Mode) ->
    case exec(Server, "AT+EEMOPT=" ++ integer_to_list(Mode)) of
        {"OK", _} ->
            ok;
        {Error, _} ->
            {error, vender_specific}
    end.

%@doc get scell
scell(Server) ->
    case all_cells_quality(Server) of
        {ok, L} ->
            case proplists:get_value(scell, L) of
                X when is_tuple(X) -> {ok, X};
                _ -> error
            end;
        _ -> error
    end.

%@doc Query signal information of all cells.
%     Engineering mode must be 1.
all_cells_quality(Server) ->
    peek_uart_out(Server),
    case exec(Server, "AT+EEMGINFO?") of
        {"OK", Res} ->
            timer:sleep(100),
            case peek_uart_out(Server) of
                [[] | T] ->
                    {ok, lists:foldl(fun handle_eem/2, [], T)};
                EEMGINFO ->
                    {error, {Res, EEMGINFO}}
            end;
        _ -> error
    end.

%@doc Try to identify serving cell
get_scell(Server) ->
    case vzw_rsrp(Server) of
        {ok, [{Scell, _Rsrp} | _T]} -> {ok, Scell};
        _ -> 
            case all_cells_quality(Server) of
                {ok, L} -> proplists:get_value(scell, L, error);
                _ -> error
            end
    end.

%@doc Verizon rsrp
vzw_rsrp(Server) ->
    case exec(Server, "AT+VZWRSRP?") of
        {"OK", Response} ->
            case proplists:get_value("+VZWRSRP", Response) of
                undefined -> error;
                VZWRSRP ->
                    {ok, handle_vzw_rsrp(string:tokens(VZWRSRP, ", "), [])}
            end;
        _ -> error
    end.

handle_vzw_rsrp([Id, Earfcn, Rsrp | T], Acc) ->
    handle_vzw_rsrp(T, [{{list_to_integer(Earfcn), list_to_integer(Id)}, list_to_float(string:strip(Rsrp,both, $"))} | Acc]);
handle_vzw_rsrp(_, Acc) -> lists:reverse(Acc).

nth_int(N, L) -> list_to_integer(lists:nth(N, L)).
    
eem_error(Cmd) ->
    io:format("unhandled eem: ~p~n", [Cmd]).

handle_eem({"+EEMLTESVC", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case length(L10) of
        20 -> [{scell, {lte, nth_int(6, L10), nth_int(5, L10)}}, {{lte, nth_int(6, L10), nth_int(5, L10)}, nth_int(10, L10) - 140} | Acc];
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMLTEINTRA", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case length(L10) of
        5 -> [{{lte, nth_int(3, L10), nth_int(2, L10)}, nth_int(4, L10) - 140} | Acc];
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMLTEINTER", Line}, Acc) -> handle_eem({"+EEMLTEINTRA", Line}, Acc);
handle_eem({"+EEMLTEINTERRAT", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case length(L10) of
        10 ->
            [{{tds, nth_int(7, L10), nth_int(8, L10)}, nth_int(9, L10) - 120} | Acc]; % umts
        9 ->
            [{{gsm, nth_int(7, L10), nth_int(8, L10)}, nth_int(9, L10) - 110} | Acc]; % gsm
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMUMTSSVC", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case L10 of
        [_, "1", "1" | T] when length(T) >= 23 ->
            [{scell, {tds, nth_int(17, L10), nth_int(16, L10)}}, {{tds, nth_int(17, L10), nth_int(16, L10)}, nth_int(5, L10)} | Acc];
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMUMTSINTRA", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case length(L10) of
        10 ->
            [{{tds, nth_int(9, L10), nth_int(10, L10)}, nth_int(2, L10)} | Acc];
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMUMTSINTER", Line}, Acc) -> handle_eem({"+EEMUMTSINTRA", Line}, Acc);
handle_eem({"+EEMUMTSINTERRAT", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case length(L10) of
        11 ->
            [{{gsm, nth_int(10, L10), nth_int(11, L10)}, nth_int(2, L10)} | Acc];
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMGINFOBASIC", _Line}, Acc) -> Acc;
handle_eem({"+EEMGINFOSVC", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case length(L10) of
        36 ->
            [{scell, {gsm, nth_int(23, L10), nth_int(7, L10)}}, {{gsm, nth_int(23, L10), nth_int(7, L10)}, nth_int(12, L10)} | Acc];
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMGINFOPS", _Line}, Acc) -> Acc;
handle_eem({"+EEMGINFONC", Line} = Response, Acc) ->
    L10 = string:tokens(Line, " ,"),
    case L10 of
        [N | T] ->
            Num = min(list_to_integer(N), length(T) div 13),
            R = lists:map(fun (Index) ->
                        SubList = lists:sublist(T, Index * 13 + 1, 13),
                        {{gsm, nth_int(11, SubList), nth_int(8, SubList)}, nth_int(7, SubList)}
                end, lists:seq(0, Num - 1)),
            R ++ Acc;
        _ -> 
            eem_error(Response),
            Acc
    end;
handle_eem({"+EEMGINBFTM", _Line}, Acc) -> Acc;
handle_eem(Unknown, Acc) -> 
    eem_error(Unknown),
    Acc.

%@doc PLMN search.
plmn(Server) ->
    case exec(Server, "AT+COPS=?", 60000 * 3) of
        {"OK", Response} ->
            L = proplists:get_value("+COPS", Response, ""),
            case string:str(L, ",,") of
                X when X > 1 ->
                    L10 = string:sub_string(L, 1, X - 1),
                    {ok, lists:foldl(fun (Ele, Acc) ->
                                case string:tokens(Ele, ",") of
                                    [Stat, NameL, NameS, PLMN, AcT] -> 
                                        [{parse_cops_stat(list_to_integer(string:strip(Stat))), 
                                          strip_quote(NameL), strip_quote(NameS), strip_quote(PLMN), 
                                          list_to_integer(string:strip(AcT))} | Acc];
                                    _ -> Acc
                                end 
                        end, [], string:tokens(L10, "()"))};
                _ -> {error, L}
            end;
        {Error, _} ->
            {error, Error}
    end.

strip_quote(S) ->
    string:strip(string:strip(S), both, $").

parse_cops_stat(1) -> available;
parse_cops_stat(2) -> current;
parse_cops_stat(3) -> forbidden;
parse_cops_stat(_) -> unknown.

%@doc Read extended signal quality.
%@end
% +CESQ: <rxlev>,<ber>,<rscp>,<ecno>,<rsrq>,<rsrp>
-spec extended_signal_quality(Server :: pid()) -> error | {ok, cell_quality()}.
extended_signal_quality(Server) ->
    case exec(Server, "AT+CESQ") of
        {"OK", Response} ->
            case proplists:get_value("+CESQ", Response) of
                undefined -> error;
                CESQ ->
                    case string:tokens(CESQ, ",") of
                        ["99", "99", "255","255","255","255"] ->
                            error;
                        [RxLev, Ber, "255","255","255","255"] ->
                            {ok, {gsm, list_to_integer(RxLev) - 110, list_to_integer(Ber)}};
                        ["99", "99", Rscp, "255","255","255"] ->
                            {ok, {tdscdma, list_to_integer(Rscp) - 120}};
                        ["99", "99", RscpW, Ecno,"255","255"] ->
                            {ok, {wcdma, list_to_integer(RscpW) - 120, list_to_integer(Ecno) / 2 - 24}};
                        ["99", "99", "255", "255",Rsrq, Rsrp] ->
                            {ok, {lte, list_to_integer(Rsrp) - 140, list_to_integer(Rsrq) / 2 - 19.5}};
                        _ ->
                            error
                    end
            end;
        _ -> 
            error
    end.

%@doc Read a generic signal power for all RAT.
-spec signal_power(Server :: pid()) -> error | {ok, RxLev_dBm :: integer()}.
signal_power(Server) ->
    case extended_signal_quality(Server) of
        {ok, {gsm, RxLev, _}} -> {ok, RxLev};
        {ok, {tdscdma, Rscp}} -> {ok, Rscp};
        {ok, {wcdma, RscpW, _}} -> {ok, RscpW};
        {ok, {lte, Rsrp, _}} -> {ok, Rsrp};
        _ -> error
    end.

%@doc Query PDP context.
-spec pdp_context(Server :: pid()) -> {ok, {APN::string(), IP::string()}} | error | none.
pdp_context(Server) ->
    case exec(Server, "AT+CGDCONT?") of
        {"OK", Response} ->
            case proplists:get_value("+CGDCONT", Response) of
                undefined -> none;
                CONT ->
                    case string:tokens(CONT, ",") of
                        [_Cid, _PdpType, _APN, "\"\"" | _T] ->
                            none;
                        [_Cid, _PdpType, APN, IP | _T] ->
                            {ok, {string:strip(APN, both, $"), string:strip(IP, both, $")}};
                        _ ->
                            error
                    end
            end;
        _ -> 
            error
    end.

%@doc Execute a AT command.
exec(Server, L) -> exec(Server, L, 120000).

%@doc Execute a AT command with a time out.
exec(Server, L, Timeout) ->
    Ref = make_ref(),
    gen_server:call(Server, {send, self(), Ref, L}),
    receive
        {Ref, X} ->
            X
    after
        Timeout ->
            {error, timeout}
    end.

peek_uart_out(Server) ->
    gen_server:call(Server, peek_uart_out).

%@doc Stop AT port.
stop(Server) ->
    gen_server:cast(Server, stop).

%  
% gen_server
%

%@private
init([adb]) ->
    Uart = at_adb:open(),
    {ok, #state{mod = at_adb, uart = Uart}};
init([PortNo]) ->
    Uart = at_uart:open(PortNo),
    {ok, #state{mod = at_uart, uart = Uart}}.

%@private
handle_call({send, Pid, Ref, L}, _From, #state{mod = M, uart = Uart} = State) ->
    %io:format("M:send: ~p~n", [L]),
    M:send(Uart, L),
    {reply, ok, State#state{data = [], cmd_ref = {Pid, Ref}, final = false}};
handle_call(peek_uart_out, _From, #state{mod = M, uart = Uart, data = Data} = State) ->
    {reply, [M:get_read_buf(Uart) | Data], State}.

%@private
handle_cast({uart_lines, _Pid, L}, #state{cmd_ref = {Pid, Ref}} = State) ->
    %io:format("uart_lines cmd_ref: ~p~n", [L]),
    NewState = lists:foldl(fun process_line/2, State, L),
    case NewState#state.final of
        false ->
            {noreply, NewState};
        Final ->
            Pid ! {Ref, {Final, NewState#state.data}},
            {noreply, NewState#state{data = [], cmd_ref = undefined}}
    end;
handle_cast({uart_lines, _Pid, L}, State) ->
    %io:format("uart_lines: ~p~n", [L]),
    NewState = lists:foldl(fun process_line/2, State, L),
    {noreply, NewState};
handle_cast(stop, #state{mod = M, uart = Uart} = State) ->
    M:close(Uart),
    {stop, normal, State}.

%@private
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {stop, normal, State}.

%@private
terminate(normal, _State) ->
    ok.

%@private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_line([], State) ->
    State;
process_line(L, #state{data = Ls, final_set = S} = State) ->
    I = string:chr(L, $:),
    {Cmd, Param} = case I of
        I when I > 0 ->
            {string:to_upper(string:substr(L, 1, I - 1)),
                string:substr(L, I + 1, length(L))};
        _ -> {string:to_upper(L), ""}
    end,
    check_assert(Cmd),
    NewState = State#state{data = [{Cmd, Param} | Ls]},    
    case sets:is_element(Cmd, S) of
        true ->
            NewState#state{final = Cmd};
        false ->
            NewState
    end.

check_assert("CP ASSERT") ->
    throw(cp_assert);
check_assert("CPASSERT") ->
    throw(cp_assert);
check_assert("LINKDOWN") ->
    throw(linkdown);
check_assert(_) ->
    ok.

%sms_pdu_decode(Pdu) ->
%    .

% 0891683108100005F0040d91683118205733f400004110305191902304f1f8fd0e
sca_decode([C1, _C2, T1, T2 | Pdu]) ->
    Len = list_to_integer([C1, C1], 16),
    {Number, Remaining} = lists:split(2 * (Len - 1), Pdu),
    VT1 = list_to_integer([T1], 16),
    VT2 = list_to_integer([T2], 16),
    true = (VT1 band $8) > 0,
    {[{type, VT1 band $7}, {number_plan, VT2}, {number, pair_reverse(Number, [])}], Remaining}.

mti_sc2mc_decode(0) -> "SMS DELIVER";
mti_sc2mc_decode(1) -> "SMS SUBMIT REPORT";
mti_sc2mc_decode(2) -> "SMS STATUS REPORT";
mti_sc2mc_decode(_) -> "Reserved".

mti_mc2sc_decode(0) -> "SMS DELIVER REPORT";
mti_mc2sc_decode(1) -> "SMS SUBMIT";
mti_mc2sc_decode(2) -> "SMS COMMAND";
mti_mc2sc_decode(_) -> "Reserved".

dcs_decode(V) -> 
    case (V bsr 4) band 2#1111 of
        X when X < 2#0100 ->
            case (V bsr 2) band 2#11 of
                0 -> bit7;
                1 -> bit8;
                2 -> ucs2;
                _ -> unknown
            end;
        2#1100 -> bit7;
        2#1101 -> bit7;
        2#1110 -> ucs2;
        2#1111 ->
            case V band 2#100 of
                2#100 -> bit8;
                _ -> bit7
            end;
        _ -> unknown
    end.

pair_reverse([C1, C2 | T], Acc) ->
    pair_reverse(T, [C2, C1 | Acc]);
pair_reverse([], Acc) ->
    lists:reverse(Acc).

adb_modify_settings(Table, Name, Value) ->
    adb_shell(["sqlite3 /data/data/com.android.providers.settings/databases/settings.db",
               "insert into " ++ Table ++ " (name, value) values(\"" ++ Name ++ "\"," ++ Value ++ ");",
               ".exit",
               "exit"]).

adb_shell(Commands) ->
    os:cmd("adb root"),
    ServerPid = self(),
    Pid = spawn_link(fun () ->
            process_flag(trap_exit, true),
            Port = open_port({spawn, "adb shell"}, [{line, 2048}]),
            loop(Port, ServerPid)
        end),
    timer:sleep(500),
    lists:foreach(fun (Cmd) -> Pid ! {send, Cmd}, timer:sleep(500) end, Commands),
    Pid ! stop,
    ok.

loop(Port, ServerPid) ->
    receive 
        {Port, {data, {eol, Line}}} ->
            io:format("~ts~n", [Line]),
            loop(Port, ServerPid);
        {send, Line} ->
            %io:format("send: ~p~n", [Line]),
            Port ! {self(), {command, Line ++ "\n"}},
            loop(Port, ServerPid);
        stop ->
            Port ! {self(), close};
        {'EXIT', Port, normal} ->
            normal;
        {'EXIT', Port, Reason} ->
            io:format("~p~n", [{'EXIT', Port, Reason}]),
            error;
        X ->
            io:format("adb shell: unknown message: ~p~n", [X]),
            loop(Port, ServerPid)
    end.

