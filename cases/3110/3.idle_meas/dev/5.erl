[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {init, [
                "SENSe:<LTE|LTET><0>:CALL:CELL:EARFcn 37750",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BASic:PHYCell[:ID] 199",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -85DBM",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BAND:WIDTh[:ALL] 5MHZ",

                "SENSe:<LTE|LTET><1>:CALL:CELL:EARFcn 38250",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BASic:PHYCell[:ID] 20",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -95DBM",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BAND:WIDTh[:ALL] 5MHZ"
           ]},
    {timeline,
        [
            {20, {check_rsrp, [{{37750, 199}, [-90,-80]}, {{38250, 20}, [-100,-90]}]}}
        ]}
].
