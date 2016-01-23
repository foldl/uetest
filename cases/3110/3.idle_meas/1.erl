[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {init, [[
                "SENSe:<LTE|LTET><0>:CALL:CELL:EARFcn 38150",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BASic:PHYCell[:ID] 1",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -85DBM"
            ],
            [
                "SENSe:<LTE|LTET><1>:CALL:CELL:EARFcn 38150",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BASic:PHYCell[:ID] 11",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -140DBM"
            ]
           ]},
    {timeline,
        [
            {1, [
                    [ "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -90DBM" ],
                    []
                ]},
            {5, [
                    [ "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -100DBM" ],
                    []
                ]},
            {5, [
                    [ "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -110DBM" ],
                    [ "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -120DBM" ]
                ]},
            {5, [
                    [ "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -115DBM" ],
                    [ "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -118DBM" ]                    
                ]},

            {30, {check_rsrp, [{{38150, 1}, [-120,-110]}, {{38150, 11}, [-123,-113]}]}}
        ]}
].
