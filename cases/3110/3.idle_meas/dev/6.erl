[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {init, [
                "SENSe:<LTE|LTET><0>:CALL:CELL:EARFcn 38249",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BASic:PHYCell[:ID] 1",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -85DBM",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BAND:WIDTh[:ALL] 10MHZ",

                "SENSe:<LTE|LTET><1>:CALL:CELL:EARFcn 38250",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BASic:PHYCell[:ID] 10",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -95DBM",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ"
           ]},
    {timeline,
        [
            {20, {check_rsrp, [{{38249, 1}, [-90,-80]}, {{38250, 10}, [-100,-90]}]}}
        ]}
].
