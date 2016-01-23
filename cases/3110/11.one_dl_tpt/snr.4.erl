[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 39150",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 1",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -85DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ",
            %"SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:GENeral CF5", % 5(DSUDDDDDDD)
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:MACSwitch:CFI 1"
        ]},
    {conn_cmds,
        [
            "SENS:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:AWGN:STAT ON",
            "SENS:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:AWGN:POW -15DB",
            "SENS:<LTE|LTET|LTEF><0|1|2>:FTP:MCS:SWIT:DL 25"
        ]},
    {vs_power, [{range, {-52, -97}}, {step, 5}, {min_speed, 25}]}
].

