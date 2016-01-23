[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {tau, off},
    {init, [
                [
                    "SENSe:<LTE|LTET><0>:CALL:CELL:EARFcn 38150",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BASic:PHYCell[:ID] 1",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -85DBM"
                ],
                [
                    "SENSe:<LTE|LTET><1>:CALL:CELL:EARFcn 38350",
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BASic:PHYCell[:ID] 10",
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -160DBM"
                ]
           ]},
    {timeline,
        [
            {5, [
                    [],
                    [
                        "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -79DBM"
                    ]
                ]}
        ]}
].
