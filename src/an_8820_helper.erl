%@doc Helper function for dealing with Anristru's 8820.

-module(an_8820_helper).

-export([clear_error/1, initiate/1, abort/1, prepare_cell/4, prepare_ca/2, config_ca/2]).
-export([set_rsepre/2, get_rsepre/1, rat_tech/1,
         cell_activate/2, establish/1, end_call/1, get_freq_id/1, 
         wait_conn/2, wait_idle/2, wait_reg/2,
         fmt_cell_id/1, set_band/2, set_pathloss/3, get_pathloss/1,
         start_ftp/2, meas_ftp/2, meas_ftp_bler/2, set_awgn/2, get_plmn/1,
         run_throughput/2, add_ncell/2, set_ta_code/2, enable_reselection/2,
         gsm_set_channel/2, gsm_set_level/2]).

-import(uetest_utils, [delay/1, str_to_num/1, wait_until/2]).

%@doc Clear errors.
clear_error(_Tester) ->
    ok.

%@doc Abort measurement.
abort(_Tester) ->
    ok. %tester_control:send_command(Tester, "MEASSTOP").

%@doc Initiate measurement.
initiate(Tester) ->
    tester_control:send_command(Tester, "CONTS").

%@doc prepare tester with specified PLMN and auth (on or off).
-spec prepare_cell(Tester :: pid(), CellId :: {Rat :: atom(), Index :: integer()}, PLMN :: list(), Auth :: on | off) -> any().
prepare_cell(Tester, {Duplex, _Index} = Cell, [MCC0, MCC1, MCC2, MNC0, MNC1], Auth) when (Duplex == tdd) or (Duplex == fdd) ->
    tester_control:set_id(Tester, Cell),
    S1 = [
            "MCC " ++ [MCC0, MCC1, MCC2],
            "MNC " ++ [MNC0, MNC1]
        ],
    S = case Auth of
        on -> S1;
        _ -> ["AUTHENT OFF",
              "INTEGRITY SNOW3G" | S1]
    end,
    tester_control:batch(Tester,
        ["CALLPROC OFF",
         "STDSEL LTE",
         "PRESET",
         "LVL OFF" | S]);
prepare_cell(Tester, {gsm, _Index} = Cell, [MCC0, MCC1, MCC2, MNC0, MNC1], _Auth) ->
    tester_control:set_id(Tester, Cell),
    S1 = [
            "MCC " ++ [MCC0, MCC1, MCC2],
            "MNC " ++ [MNC0, MNC1]
        ],
    tester_control:batch(Tester,
        ["CALLPROC OFF",
         "STDSEL GSM",
         "PRESET" ,
         "LVL OFF" | S1]);
prepare_cell(Tester, {tds, _Index} = Cell, [MCC0, MCC1, MCC2, MNC0, MNC1], Auth) ->
    tester_control:set_id(Tester, Cell),
    S1 = [
            "MCC " ++ [MCC0, MCC1, MCC2],
            "MNC " ++ [MNC0, MNC1]
        ],
    S = case Auth of
        on -> S1;
        _ -> ["AUTHENT OFF",
              "INTEGRITY SNOW3G" | S1]
    end,
    tester_control:batch(Tester,
        ["CALLPROC OFF",
         "STDSEL TDSCDMA",
         "PRESET_3GPP",
         "LVL OFF" | S]).

%@doc Prepare for CA: enter CA mode
prepare_ca([T1, T2], siso) ->
    tester_control:send_command(T1, "CHCODING RMC_DL_CA_PCC"),
    tester_control:send_command(T2, "CHCODING RMC_DL_CA_SCC"),
    tester_control:send_command(T2, "CALLPROC OFF"),
    % sync two cells
    tester_control:send_command(T2, "ENTERSYNC INT_SLAVE"),
    tester_control:send_command(T1, "ENTERSYNC MASTER"),
    tester_control:wait_op(T2, 5000),
    true = (tester_control:send_query(T2, "ENTERSYNC?") == "1").

%@doc Configure CA parameters
config_ca([T1, T2], siso) ->
    Run = fun ([Ta, Tb], Cmd, Postfix) ->
            case tester_control:send_query(Ta, Cmd ++ "?") of
                "-1" -> ok;
                X -> tester_control:send_command(Tb, Cmd ++ Postfix ++ [$ , X])
            end
    end,
    lists:foreach(fun (Cmd) -> Run([T1, T2], Cmd, "_PCC") end,
        ["BANDWIDTH", "ULCHAN", "ULRMC_RB", "ULRB_START", "ULIMCS"]),
    lists:foreach(fun (Cmd) -> Run([T2, T1], Cmd, "_SCC") end,
        ["BANDWIDTH", "DLCHAN", "DLRMC_RB", "DLIMCS1", "DLIMCS2", "DLIMCS3", "DLIMCS4"]).

%@doc Cell activation and de-activation.
cell_activate(Tester, true) ->
    %tester_control:send_command(Tester, "CALLPROC ON"),
    tester_control:send_command(Tester, "LVL ON");
cell_activate(Tester, false) ->
    %tester_control:send_command(Tester, "CALLPROC OFF"),
    tester_control:send_command(Tester, "LVL OFF").

%@doc set RS energy per RE
set_rsepre(Tester, DBM)  ->    
    tester_control:send_command(Tester, "OLVL_EPRE " ++ uetest_utils:num_to_str(DBM)).

%@doc get RS energy per RE
get_rsepre(Tester)  ->    
    str_to_num(tester_control:send_query(Tester, "OLVL_EPRE?")).

%@doc Paing UE for connection (start call).
establish(Tester) ->
    tester_control:send_command(Tester, "CALLSA").

%@doc End call.
end_call(Tester) ->
    tester_control:send_command(Tester, "CALLSO").

%@doc Get RAT technology of a cell.
rat_tech(Tester) ->
    get_std(Tester).

get_std(Tester) ->
    {Std, _} = tester_control:get_id(Tester),
    case Std of
        fdd -> lte;
        tdd -> lte;
        X -> X
    end.

%@doc Get PLMN
get_plmn(Tester) ->
    tester_control:send_query(Tester, "MCC?") ++ tester_control:send_query(Tester, "MNC?").

%@doc Wait until UE becomes connected or timed out.
wait_conn(Tester, TimeoutSec) ->
    Cmd = "CALLSTAT?",
    Std = get_std(Tester),
    wait_until(fun () -> 
                R = tester_control:send_query(Tester, Cmd),
                case Std of
                    gsm -> R == "7";
                    _ -> R == "6"
                end
        end, TimeoutSec).

%@doc Wait until UE becomes idle or timed out.
wait_idle(Tester, TimeoutSec) ->
    Cmd = "CALLSTAT?",
    Std = get_std(Tester),
    case Std of
        gsm -> 
            case timer:tc(fun () -> wait_reg(Tester, TimeoutSec) end) of
                {Used, ok} ->
                    wait_until(fun () -> 
                            tester_control:send_query(Tester, Cmd) == "1" 
                        end, TimeoutSec - Used div 1000000);
                {_Used, R} -> R
            end;
        _ ->
            wait_until(fun () -> 
                        tester_control:send_query(Tester, Cmd) == "2" 
                end, TimeoutSec)
    end.

%@doc Wait until UE is registered or timed out.
wait_reg(Tester, TimeoutSec) ->
    Std = get_std(Tester),
    case Std of
        gsm -> 
            wait_until(fun () -> 
                tester_control:send_query(Tester, "CALLIMSI? flag") == "1"
            end, TimeoutSec);
        _ ->
            wait_until(fun () -> 
                R = tester_control:send_query(Tester, "CALLSTAT?"),
                (R == "2") or (R == "6")
        end, TimeoutSec)
    end.

%@doc Format cell identification
fmt_cell_id({fdd, I}) -> "LTEF" ++ integer_to_list(I);
fmt_cell_id({tdd, I}) -> "LTET" ++ integer_to_list(I).

%@doc Set frequency band. Note: only possible when cell is off.
set_band(Tester, N) when is_integer(N) ->
    tester_control:exec_confirm(Tester, "BAND " ++ integer_to_list(N)).

%@doc Set RX/TX path loss
set_pathloss(Tester, RX, TX) ->
    tester_control:send_command(Tester, "EXTLOSSW ON"),
    tester_control:send_command(Tester, "DLEXTLOSS " ++ if TX > 0 -> uetest_utils:num_to_str(TX); true -> "0" end),
    tester_control:send_command(Tester, "ULEXTLOSS " ++ if RX > 0 -> uetest_utils:num_to_str(RX); true -> "0" end).

%@doc Set AWGN power
set_awgn(Tester, off) ->
    tester_control:send_command(Tester, "AWGNLVL OFF");
set_awgn(Tester, DB) when is_number(DB) ->
    tester_control:send_command(Tester, "AWGNLVL ON"),
    tester_control:send_command(Tester, "AWGNPWR " ++ uetest_utils:num_to_str(DB)).

%@doc Get RX/TX path loss
get_pathloss(Tester) ->
    case tester_control:send_query(Tester, "EXTLOSSW?") of
        "ON" ->
            {
                str_to_num(tester_control:send_query(Tester, "ULEXTLOSS?")),
                str_to_num(tester_control:send_query(Tester, "DLEXTLOSS?"))
            };
        _ -> {0, 0}
    end.

%@doc Start DL/UL data transfer (FTP)
start_ftp(Tester, Dir) ->
    tester_control:send_command(Tester, "TPUT_MEAS ON"),
    case Dir of
        dl ->
            tester_control:send_command(Tester, "DLIMCS 28");
        ul ->
            tester_control:send_command(Tester, "ULIMCS 28")
    end,
    tester_control:send_command(Tester, "CONTS").

%@doc Measure current dl/ul speed.
% Return speed in Mbps.
meas_ftp(Tester, Dir) ->
    Cmd = case Dir of dl -> "TPUT?"; _ -> "UL_TPUT?" end,
    case tester_control:send_query(Tester, Cmd) of
        error ->
            -1;
        Str ->
            str_to_num(Str) / 1000
    end.

%@doc Measure current BLER.
meas_ftp_bler(Tester, Dir) ->
    Cmd = case Dir of dl -> "TPUT_BLER?"; _ -> "UL_TPUT_BLER?" end,
    case tester_control:send_query(Tester, Cmd) of
        error ->
            100;
        Str ->
            str_to_num(Str) * 100
    end.

%@doc Run throughput test on PDSCH or PUSCH (return true if passed).
run_throughput(Tester, _Dir) ->
    tester_control:send_command(Tester, "TPUT_MEAS ON"),
    tester_control:send_command(Tester, "TESTPRM RX_SENS"),
    tester_control:send_command(Tester, "SWP"),
    case tester_control:wait_op(Tester, 10) of
        ok ->
            tester_control:send_query(Tester, "TPUTPASS?") == "PASS";
        X -> X
    end.

%@doc Get EARFCN and cell id
get_freq_id(Tester) ->
    case get_std(Tester) of
        lte ->
            Earfcn = tester_control:send_query(Tester, "DLCHAN?"),
            Id = tester_control:send_query(Tester, "CELLID?"),
            {list_to_integer(Earfcn), list_to_integer(Id)};
        tds ->
            Freq = tester_control:send_query(Tester, "CHAN?"),
            Id = tester_control:send_query(Tester, "SCRCDODEID?"),
            {list_to_integer(Freq), list_to_integer(Id)};
        gsm ->
            Chan = tester_control:send_query(Tester, "CHAN?"),
            Id = list_to_integer(tester_control:send_query(Tester, "NIDNCC?")) * 8 + 
                 list_to_integer(tester_control:send_query(Tester, "NIDBCC?")),
            {list_to_integer(Chan), Id}
    end.

%@doc Add cell2 as neighbour cell of cell1
add_ncell(Tester1, Tester2) ->
    add_ncell0({get_std(Tester1), Tester1}, {get_std(Tester2), Tester2}).

add_ncell0({lte, Tester1}, {lte, Tester2}) ->
    Earfcn1 = tester_control:send_query(Tester1, "DLCHAN?"),
    Earfcn2 = tester_control:send_query(Tester2, "DLCHAN?"),
    case Earfcn1 == Earfcn2 of
        true ->
            Id = tester_control:send_query(Tester2, "CELLID?"),
            tester_control:send_command(Tester1, "NCAINTRAFREQ 1," ++ Id);
        _ ->
            tester_control:send_command(Tester1, "NCAINTERFREQ 1," ++ Earfcn2)
    end;
add_ncell0({lte, Tester1}, {tds, Tester2}) ->
    Chan = tester_control:send_query(Tester2, "CHAN?"),
    tester_control:send_command(Tester1, "NCATDSDLUARFCN 1," ++ Chan),
    tester_control:send_command(Tester1, "IRAT REDIRECT_TDSCDMA"),
    tester_control:send_command(Tester1, "IRATW_CH " ++ Chan);
add_ncell0({lte, Tester1}, {w, Tester2}) ->
    Chan = tester_control:send_query(Tester2, "CHAN?"),
    tester_control:send_command(Tester1, "NCATDSDLUARFCN 1," ++ Chan),
    tester_control:send_command(Tester1, "IRAT REDIRECT_WCDMA"),
    tester_control:send_command(Tester1, "IRATW_CH " ++ Chan);
add_ncell0({lte, Tester1}, {gsm, Tester2}) ->
    Chan = tester_control:send_query(Tester2, "CHAN?"),
    % TODO: gsm ncell?
    %tester_control:send_command(Tester1, "NCATDSDLUARFCN 1," ++ UARFCN),
    tester_control:send_command(Tester1, "IRAT REDIRECT_GSM"),
    tester_control:send_command(Tester1, "IRATG_CH " ++ Chan);
add_ncell0({tds, Tester1}, {lte, Tester2}) ->
    Chan = tester_control:send_query(Tester2, "DLCHAN?"),
    tester_control:send_command(Tester1, "NCAEARFCN 1," ++ Chan),
    tester_control:send_command(Tester1, "MEAS_INTERRAT_LTE ON");
add_ncell0({gsm, Tester1}, {lte, Tester2}) ->
    Chan = tester_control:send_query(Tester2, "DLCHAN?"),
    tester_control:send_command(Tester1, "CALLECA 1," ++ Chan),
    tester_control:send_command(Tester1, "IRAT REDIRECT_LTE"),
    tester_control:send_command(Tester1, "IRATL_CH " ++ Chan).

%@doc enable cell reselection
enable_reselection(_Tester, true) ->
    ok.
% enable_reselection(Tester, false) ->
%     disable_reselection0(get_std(Tester), Tester).
% 
% disable_reselection0(lte, Tester) ->
%     [tester_control:send_command(Tester, "NCAINTRAFREQ " ++ integer_to_list(I) ++ ",-1") || 
%       I <- lists:seq(1,16)],
%     [tester_control:send_command(Tester, "NCAINTERFREQ " ++ integer_to_list(I) ++ ",-1") || 
%       I <- lists:seq(1,9)].

%@doc Set tracking area code
set_ta_code(Tester, Code) when 0 =< Code, Code =< 65535 ->
    tester_control:send_command(Tester, "TAC " ++ integer_to_list(Code)).

%@doc Set GSM TCH channal
gsm_set_channel(Tester, Chan) ->
    tester_control:send_command(Tester,
        "CHAN " ++ uetest_utils:num_to_str(Chan)).

%@doc Set GSM ouput level
gsm_set_level(Tester, Level) ->
    tester_control:send_command(Tester,
        "OLVL " ++ uetest_utils:num_to_str(Level)).
