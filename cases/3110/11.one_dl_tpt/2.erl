[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 38950",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 1",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -85DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ",
            %"SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:GENeral CF5", % 5(DSUDDDDDDD)
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:SPECial CF3",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:MACSwitch:CFI 1"
        ]},
    {conn_cmds,
        [
            "SENS::<LTE|LTET|LTEF><0|1|2>:FTP:MCS:SWIT:DL 28"
        ]},
    {vs_power, [{range, {-60, -95}}, {step, 2}, {min_ratio, 0.8}, {min_speed, 33}]}, % {bler, 0.001}: BLER is mistaken as average BLER
    {vs_power, [{range, {-95, -60}}, {step, 2}, {min_speed, 33}]}
].

