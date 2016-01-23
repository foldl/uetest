[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 38150",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 1",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -85DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ",
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:GENeral CF2", % 5(DSUDDDSUDD)
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:SPECial CF7",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:SRS:CONFiguration:INDex 11",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:SRS:DURation FORever",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:SRS:BW:EDGe BW2"
                   ]},
    {conn_cmds,
        [
            "SENSe:<LTE|LTET|LTEF><0|1|2>:FTP:MCS:SWITch:UL 8"
        ]},
        {vs_power, [{range, {-70, -70}}, {step, 2}, {min_ratio, 0.8}, {min_speed, 1}, {bler, 0.05}]} % {bler, 0.001}: BLER is mistaken as average BLER
         
].

