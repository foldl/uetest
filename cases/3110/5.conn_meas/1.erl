[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {init, [
                [
                    "SENSe:<LTE|LTET><0>:CALL:CELL:EARFcn 38150",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BASic:PHYCell[:ID] 1",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -85DBM"
                ],
                [
                    "SENSe:<LTE|LTET><1>:CALL:CELL:EARFcn 38150",
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BASic:PHYCell[:ID] 11",
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -100DBM"
                ]
           ]},
    {timeline,
        [
            {5, [
                    [],
                    [
                        "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -90DBM"
                    ]
                ]},
            {5, [
                    [],
                    [
                        "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -86DBM"
                    ]
                ]},
            {50, {check_rsrp, [{{38150, 1}, -85}, {{38150, 11}, -88}]}}
        ]}
].
