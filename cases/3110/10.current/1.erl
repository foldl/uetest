[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 38150",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 1",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -85DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 10MHZ",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:PL:PAGing:CYCLe MS1280"
        ]}
].

%                     tester_control:send_command(Tester, "SENS:LTE0:CALL:CELL:PL:DRX:FLAG OFF");
%                 _ ->
%                     tester_control:send_command(Tester, "SENS:LTE0:CALL:CELL:PL:DRX:FLAG ON"),
%                     tester_control:send_command(Tester, "SENS:LTE0:CALL:CELL:PL:DRX:CYCL:LONG:STAR:OFFS SF" ++ integer_to_list(LongCycle))
