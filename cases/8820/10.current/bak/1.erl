[
    {cell, {tdd, 0}},
    {init, 
        [
            "BAND 38",
            "BANDWIDTH 10MHZ"
        ]},
    {timeline,
        [
            wait_and_meas_idle,
            establish,
            {meas_current, [0, 10, 22.5]}
        ]}
].

