%@doc Helper function for dealing with Datang LinkTester's 3110.

-module(dt_3110_helper).

-export([clear_error/1, initiate/1, abort/1, prepare_cell/4]).
-export([set_rsepre/2, get_rsepre/1, prepare_blind_ho/3, blind_handover/2,
         cell_activate/2, establish/1, end_call/1, get_freq_id/1, 
         wait_conn/2, wait_idle/2, wait_reg/2, rat_tech/1,
         fmt_cell_id/1, set_band/2, set_pathloss/3, get_pathloss/1,
         start_ftp/2, meas_ftp/2, meas_ftp_bler/2, set_awgn/2, get_plmn/1,
         run_throughput/2, add_ncell/2, set_ta_code/2, enable_reselection/2,
         gsm_set_channel/2, gsm_set_level/2]).

-import(uetest_utils, [delay/1, str_to_num/1, wait_until/2]).

-define(_id, tester_control:get_id(Tester)).

%@doc Clear errors.
clear_error(Tester) ->
    tester_control:send_command(Tester, "ERR:CLE").

%@doc Abort measurement.
abort(Tester) ->
    tester_control:send_command(Tester, "ABOR").

%@doc Initiate measurement.
initiate(Tester) ->
    tester_control:send_command(Tester, "INIT").

%@doc prepare tester with specified PLMN and auth (on or off).
-spec prepare_cell(pid(), Cell :: {atom(), integer()}, PLMN :: list(), Auth :: on | off) -> any().
prepare_cell(Tester, {Duplex, _Index} = Cell, [MCC0, MCC1, MCC2, MNC0, MNC1], Auth) when (Duplex == tdd) or (Duplex == fdd) ->
    tester_control:set_id(Tester,fmt_cell_id(Cell)),
    CellId = ?_id,
    S1 = [
            "CALL:CELL:BAS:MCC " ++ [MCC0, MCC1, MCC2],
            "CALL:CELL:BAS:MNC " ++ [MNC0, MNC1],    % 00 is invalid
            "CALL:CELL:SEC:RRC:CONN:REL ON"
        ],
    S = case Auth of
        on -> S1;
        _ -> ["CALL:CELL:SEC:AUTH:SWIT OFF",
              "CALL:CELL:SEC:RRC:ALG:INT EIA0" | S1]
    end,
    Cmds = ["SENS:" ++ CellId ++ ":" ++ X || X <- S],
    tester_control:batch(Tester,
        ["SENS:" ++ get_cur_mod(Tester) ++ ":CALL:CELL:OPER:MOD OFF",
         "INSTrument:SELect " ++ CellId,
         "SYSTem:PRESet" | Cmds]);
prepare_cell(Tester, {gsm, _Index} = Cell, [MCC0, MCC1, MCC2, MNC0, MNC1], _Auth) ->
    tester_control:set_id(Tester,fmt_cell_id(Cell)),
    CellId = ?_id,
    S1 = [
            "CALL:CELL:MCC " ++ [MCC0, MCC1, MCC2],
            "CALL:CELL:MNC " ++ [MNC0, MNC1]
        ],
    Cmds = ["SENS:" ++ CellId ++ ":" ++ X || X <- S1],
    tester_control:batch(Tester,
        ["SENS:" ++ get_cur_mod(Tester) ++ ":CALL:CELL:OPER:MOD OFF",
         "INSTrument:SELect " ++ CellId,
         "SYSTem:PRESet" | Cmds]);
prepare_cell(Tester, {tds, _Index} = Cell, [MCC0, MCC1, MCC2, MNC0, MNC1], Auth) ->
    tester_control:set_id(Tester,fmt_cell_id(Cell)),
    CellId = ?_id,
    S1 = [
            "CALL:CELL:MCC " ++ [MCC0, MCC1, MCC2],
            "CALL:CELL:MNC " ++ [MNC0, MNC1]
        ],
    S = ["CALL:CELL:SEC:AUTH " ++ if Auth == on -> "ON"; true -> "OFF" end  | S1],
    Cmds = ["SENS:" ++ CellId ++ ":" ++ X || X <- S],
    tester_control:batch(Tester,
        ["SENS:" ++ get_cur_mod(Tester) ++ ":CALL:CELL:OPER:MOD OFF",
         "INSTrument:SELect " ++ CellId,
         "SYSTem:PRESet" | Cmds]).

get_cur_mod(Tester) ->
    tester_control:send_query(Tester, "INSTrument:SELect?").

%@doc Get PLMN
get_plmn(Tester) ->
    get_plmn(rat_tech(Tester), Tester).

get_plmn(lte, Tester) ->
    CellId = ?_id,
    tester_control:send_query(Tester, "SENS:" ++ CellId ++ ":CALL:CELL:BAS:MCC?")
     ++ tester_control:send_query(Tester, "SENS:" ++ CellId ++ ":CALL:CELL:BAS:MNC?");
get_plmn(_Other, Tester) ->
    CellId = ?_id,
    tester_control:send_query(Tester, "SENS:" ++ CellId ++ ":CALL:CELL:MCC?")
     ++ tester_control:send_query(Tester, "SENS:" ++ CellId ++ ":CALL:CELL:MNC?").

%@doc prepare blind handover
prepare_blind_ho(Tester, [MCC0, MCC1, MCC2, MNC0, MNC1], Auth) ->
    S1 = [
            "CALL:CELL:HAND:SET:SYST:TDSC:MCC " ++ [MCC0, MCC1, MCC2],
            "CALL:CELL:HAND:SET:SYST:TDSC:MNC " ++ [MNC0, MNC1],
            "CALL:CELL:HANDover:SETup:SYSTem:TDSCdma:SPACe 2",
            "CALL:CELL:HANDover:SETup:SYSTem:TDSCdma:DELay 300"
            % "CALL:CELL:HAND:SET:SYST:GSM:MCC " ++ [MCC0, MCC1, MCC2],
            % "CALL:CELL:HAND:SET:SYST:GSM:MNC " ++ [MNC0, MNC1],
            % "CALL:CELL:HANDover:SETup:SYSTem:GSM:SPACe 2",
            % "CALL:CELL:HANDover:SETup:SYSTem:GSM:DELay 300",
            % "CALL:CELL:HAND:SET:SYST:WCDMa:MCC " ++ [MCC0, MCC1, MCC2],
            % "CALL:CELL:HAND:SET:SYST:WCDMa:MNC " ++ [MNC0, MNC1],
            % "CALL:CELL:HANDover:SETup:SYSTem:WCDMa:SPACe 2",
            % "CALL:CELL:HANDover:SETup:SYSTem:WCDMa:DELay 300"
        ],
    S = case Auth of
        on -> ["CALL:CELL:HAND:SET:SYST:TDSC:SEC:AUTH ON" | S1];
        _ ->  ["CALL:CELL:HAND:SET:SYST:TDSC:SEC:AUTH OFF" | S1]
    end,
    Cmds = ["SENS:" ++ ?_id ++ ":" ++ X || X <- S],
    tester_control:batch(Tester, Cmds).

%@doc Blind handover to another RAT tech
blind_handover(Tester, tds) ->
    blind_handover(Tester, "TDSC");
blind_handover(Tester, gsm) ->
    blind_handover(Tester, "GSM");
blind_handover(Tester, wcdma) ->
    blind_handover(Tester, "WCDM");
blind_handover(Tester, Rat) when is_list(Rat) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:HAND:TO " ++ Rat),
    delay(5),
    wait_until(fun () -> 
                lists:prefix(Rat, tester_control:send_query(Tester, "INSTrument:SELect?"))
        end, 5).

%@doc Cell activation and de-activation.
cell_activate(Tester, true) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:OPER:MOD ACT");
cell_activate(Tester, false) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:OPER:MOD OFF").

%@doc set RS energy per RE
set_rsepre(Tester, DBM)  ->    
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:RSEP " ++ uetest_utils:num_to_str(DBM) ++ "DBM").

%@doc get RS energy per RE
get_rsepre(Tester)  ->    
    str_to_num(tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:RSEP?")).

%@doc Paing UE for connection (start call).
establish(Tester) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:ORIG").

%@doc End call.
end_call(Tester) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:END").

%@doc Get RAT technology of a cell.
rat_tech(Tester) ->
    case tester_control:get_id(Tester) of
        [$L | _] -> lte;
        [$T | _] -> tds;
        [$W | _] -> wcdma;
        [$G | _] -> gsm;
        _ -> unknown
    end.

%@doc Wait until UE becomes connected or timed out.
wait_conn(Tester, TimeoutSec) ->
    Cmd = "SENS:" ++ ?_id ++ ":CALL:CELL:CONN?",
    wait_until(fun () -> 
                tester_control:send_query(Tester, Cmd) == "1" 
        end, TimeoutSec).

%@doc Wait until UE becomes idle or timed out.
wait_idle(Tester, TimeoutSec) ->
    Cmd = "SENS:" ++ ?_id ++ ":CALL:CELL:CONN?",
    wait_until(fun () -> 
                tester_control:send_query(Tester, Cmd) == "0" 
        end, TimeoutSec).

%@doc Wait until UE is registered or timed out.
wait_reg(Tester, TimeoutSec) ->
    Cmd = "SENS:" ++ ?_id ++ ":CALL:CELL:REG?",
    wait_until(fun () -> 
                tester_control:send_query(Tester, Cmd) == "1" 
        end, TimeoutSec).

%@doc Format cell identification
fmt_cell_id({fdd, I}) -> "LTEF" ++ integer_to_list(I);
fmt_cell_id({tdd, I}) -> "LTET" ++ integer_to_list(I);
fmt_cell_id({tds, I}) -> "TDSC" ++ integer_to_list(I);
fmt_cell_id({gsm, I}) -> "GSM" ++ integer_to_list(I);
fmt_cell_id({wcdma, I}) -> "WCDm" ++ integer_to_list(I).

%@doc Set frequency band. Note: only possible when cell is off.
set_band(Tester, N) when is_integer(N) ->
    Cmd = "SENS:" ++ ?_id ++ ":CALL:CELL:BAND " ++ integer_to_list(N),
    tester_control:exec_confirm(Tester, Cmd).

%@doc Set RX/TX path loss
set_pathloss(Tester, RX, TX) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:ATT:TX " ++ integer_to_list(TX)),
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:ATT:RX " ++ integer_to_list(RX)).

%@doc Get Rx/Tx path loss
get_pathloss(Tester) ->
    {str_to_num(tester_control:send_query(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:ATT:RX?")),
     str_to_num(tester_control:send_query(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:ATT:TX?"))}.

%@doc Set AWGN power
set_awgn(Tester, off) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:AWGN:STAT OFF");
set_awgn(Tester, DB) when is_number(DB) ->
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:AWGN:STAT ON"),
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":CALL:CELL:AWGN:POW " ++ uetest_utils:num_to_str(DB) ++ "DB").

%@doc Start DL/UL data transfer (FTP)
start_ftp(Tester, Dir) ->
    tester_control:send_command(Tester, "CONF:" ++ ?_id ++ " FTP"),
    case Dir of
        dl ->
            tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":FTP:MCS:SWIT:DL 27"),
            tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":FTP:BO DLB");
        ul ->
            tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":FTP:MCS:SWIT:UL 27"),
            tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":FTP:BO ULB")
    end,
    initiate(Tester).

%@doc Measure current dl/ul speed.
% Return speed in Mbps.
meas_ftp(Tester, Dir) ->
    Cmd = "FETC:" ++ ?_id ++ ":FTP:SPE:" ++ case Dir of dl -> "DL"; _ -> "UL" end ++ "?",
    case tester_control:send_query(Tester, Cmd) of
        error ->
            -1;
        Str ->
            str_to_num(Str)
    end.

%@doc Measure current BLER.
meas_ftp_bler(Tester, Dir) ->
    Cmd = "FETC:" ++ ?_id ++ ":FTP:BLER:" ++ case Dir of dl -> "DL"; _ -> "UL" end ++ "?",
    case tester_control:send_query(Tester, Cmd) of
        error ->
            100;
        Str ->
            str_to_num(Str)
    end.

%@doc Run throughput test on PDSCH or PUSCH (return true if passed).
run_throughput(Tester, Dir) ->
    tester_control:send_command(Tester, "CONF:" ++ ?_id ++ " THR"),
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":THR:CHAN " ++ case Dir of dl -> "PDSCh"; _ -> "PUSCh" end),
    tester_control:send_command(Tester, "SENS:" ++ ?_id ++ ":THR:CONT OFF"),    
    initiate(Tester),
    case tester_control:wait_op(Tester, 10) of
        ok ->
            tester_control:send_query(Tester, "FETC:" ++ ?_id ++ ":THR?") == "1";
        X -> X
    end.

%@doc Get EARFCN and cell id
get_freq_id(Tester) ->
    Band   = tester_control:send_query(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:BAND?"),
    Earfcn = case list_to_integer(Band) > 32 of
        true -> tester_control:send_query(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:EARFcn?");
        _    -> tester_control:send_query(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:EARFcn:DL?")
    end,
    Id = tester_control:send_query(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:BASic:PHYCell?"),
    {list_to_integer(Earfcn), list_to_integer(Id)}.

%@doc Add cell2 as neighbour cell of cell1
add_ncell(Tester1, Tester2) ->
    add_ncell0({rat_tech(Tester1), Tester1}, {rat_tech(Tester2), Tester2}).

add_ncell0({lte, Tester1}, {lte, Tester2}) ->
    CellId1 = tester_control:get_id(Tester1),
    CellId2 = tester_control:get_id(Tester2),
    Band   = tester_control:send_query(Tester2, "SENSe:" ++ CellId2 ++ ":CALL:CELL:BAND?"),
    Earfcn = case uetest_utils:band_type(list_to_integer(Band)) of
        tdd -> tester_control:send_query(Tester2, "SENSe:" ++ CellId2 ++ ":CALL:CELL:EARFcn?");
        _    -> tester_control:send_query(Tester2, "SENSe:" ++ CellId2 ++ ":CALL:CELL:EARFcn:DL?")
    end,
    Id = tester_control:send_query(Tester2, "SENSe:" ++ CellId2 ++ ":CALL:CELL:BASic:PHYCell?"),

    % 3110 might not apply following settings to SIB4/5, so turn OFF first.
    tester_control:send_command(Tester1, "SENSe:" ++ CellId1 ++ ":CALL:CELL:ADJCell:SWITch OFF"), 
    tester_control:send_command(Tester1, "SENSe:" ++ CellId1 ++ ":CALL:CELL:ADJCell:BAND " ++ Band),
    tester_control:send_command(Tester1, "SENSe:" ++ CellId1 ++ ":CALL:CELL:ADJCell:EARFcn:DL " ++ Earfcn),
    tester_control:send_command(Tester1, "SENSe:" ++ CellId1 ++ ":CALL:CELL:ADJCell:ID:PHYCell " ++ Id),
    tester_control:send_command(Tester1, "SENSe:" ++ CellId1 ++ ":CALL:CELL:ADJCell:ID:EUTRa:PHYCell" ++ Id),
    tester_control:send_command(Tester1, "SENSe:" ++ CellId1 ++ ":CALL:CELL:ADJCell:SWITch ON");
add_ncell0({R1, _Tester1}, {R2, _Tester2}) ->
    io:format("WARNING: add_ncell0 RAT not supported: ~p", [{R1, R2}]).

%@doc enable cell reselection
enable_reselection(Tester, Flag) ->
    Cmd = if Flag -> "ON"; true -> "OFF" end,
    % 3110 changes REL to ON after flag truned on, 
    Rel = tester_control:send_query(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:SEC:RRC:CONN:REL?"),
    tester_control:send_command(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:BASic:RESelect:FLAG " ++ Cmd),
    case Rel of 
        "OFF" -> 
            tester_control:send_command(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:SEC:RRC:CONN:REL OFF");
        _ -> ok        
    end.

%@doc Set tracking area code
set_ta_code(Tester, Code) when 0 =< Code, Code =< 65535 ->
    tester_control:send_command(Tester, "SENSe:" ++ ?_id ++ ":CALL:CELL:BASic:TRACking:AREacode " ++ integer_to_list(Code)).

%@doc Set GSM TCH channal
gsm_set_channel(Tester, Chan) ->
    tester_control:send_command(Tester,
        "SENS:" ++ ?_id ++ ":CALL:CELL:CHAN " ++ uetest_utils:num_to_str(Chan)).

%@doc Set GSM ouput level
gsm_set_level(Tester, Level) ->
    tester_control:send_command(Tester,
        "SENS:" ++ ?_id ++ ":CALL:CELL:LEV " ++ uetest_utils:num_to_str(Level)).

