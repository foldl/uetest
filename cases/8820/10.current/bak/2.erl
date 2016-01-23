[
    {cell, {tdd, 0}},
    {init, 
        [
            "BAND 40",
            "BANDWIDTH 10MHZ"
        ]},
    {timeline,
        [
            wait_idle,
            establish,
            {meas_current, [0, 10, 22.5]}
        ]}
].


