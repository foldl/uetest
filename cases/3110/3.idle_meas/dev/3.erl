[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {init, [
                "SENSe:<LTE|LTET><0>:CALL:CELL:EARFcn 38600",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BASic:PHYCell[:ID] 9",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -85DBM",
                "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BAND:WIDTh[:ALL] 10MHZ",

                "SENSe:<LTE|LTET><1>:CALL:CELL:EARFcn 38600",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BASic:PHYCell[:ID] 501",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -140DBM",
                "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ"
           ]},
    {timeline,
        [
            {1, [
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -90DBM"
                ]},
            {5, [
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -95DBM"
                ]},
            {5, [
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -100DBM"
                ]},
            {5, [
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -105DBM"
                ]},
            {5, [
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -110DBM"
                ]},
            {5, [
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -115DBM",
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -120DBM"
                ]},

            {30, {check_rsrp, [{{38150, 9}, [-118,-112]}, {{38150, 501}, [-125,-115]}]}},
            {5, [
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -125DBM"
                ]},
            {5, {check_rsrp, [{{38150, 9}, [-120,-110]}, {{38150, 501}, [-130,-120]}]}}
        ]}
].
