[
    {cell, [{tdd, 0}, {tds, 1}]},
    {init, [[
                "DLCHAN 38150",
                "CELLID 1",
                "BANDWIDTH 20MHZ",
                "OLVL_EPRE -85DBM"
            ],
            [
                "CHAN 10054",
                "SCRCDODEID 1",
                "OLVL -95DBM"
            ]
           ]},
    {timeline,
        [
            {30, {check_signal, [{{lte, 38150, 1}, [-90,-80]}, {{tds, 10054, 0}, [-100,-90]}]}},
            {5, [
                    [],
                    [
                        "OLVL -92DBM"
                    ]
                ]},
            {5, {check_signal, [{{lte, 38150, 1}, [-90,-80]}, {{tds, 10054, 0}, [-97,-87]}]}}
        ]}
].
