[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 39150",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 1",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -70DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ",
            %"SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:GENeral CF5", % 5(DSUDDDDDDD)
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:MACSwitch:CFI 1"
        ]},
    {conn_cmds, []},
    {vs_awgn, [{mcs, "SENS:<LTE|LTET|LTEF><0|1|2>:FTP:MCS:SWIT:DL"}, {range, {0, -30}}, {step, 1}, {min_speed, 2}]}
].

