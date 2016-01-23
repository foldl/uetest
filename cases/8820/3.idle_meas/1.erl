[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {init, [[
                "DLCHAN 38150",
                "CELLID 1",
                "BANDWIDTH 20MHZ",
                "OLVL_EPRE -85DBM"
            ],
            [
                "DLCHAN 38150",
                "CELLID 10",
                "BANDWIDTH 20MHZ",
                "OLVL_EPRE -90DBM"
            ]
           ]},
    {timeline,
        [
            {30, {check_rsrp, [{{38150, 1}, [-90,-80]}, {{38150, 10}, [-95,-85]}]}},
            {5, [
                    [],
                    [
                        "OLVL_EPRE -92DBM"
                    ]
                ]},
            {5, {check_rsrp, [{{38150, 1}, [-90,-80]}, {{38150, 10}, [-97,-87]}]}}
        ]}
].
