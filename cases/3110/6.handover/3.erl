[
    {cell, [{tdd, 0}, {tdd, 1}]},
    {tau, off},
    {init, [
                [
                    "SENSe:<LTE|LTET><0>:CALL:CELL:EARFcn 37850", 
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:BASic:PHYCell[:ID] 1",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:RSEPre -85DBM",

                    "SENSe:LTE0:CALL:CELL:SEC:RRC:CONN:REL OFF", % no release
                    "SENSe:LTE0:CALL:CELL:SEC:RRC:CONN:REL OFFx", % no release

                    %  drx-Config setup : ref to NS-IOT 8.3.3 
                    %  {
                    %    onDurationTimer psf2,
                    %    drx-InactivityTimer psf100,
                    %    drx-RetransmissionTimer psf16,
                    %    longDRX-CycleStartOffset sf40 : 4
                    %  },
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:PL:DRX:FLAG ON",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:PL:DRX:TIMer:DURation PSF2",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:PL:DRX:TIMer:INACtivity PSF100",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:PL:DRX:TIMer:RETRansmission PSF16",
                    "SENSe:<LTE|LTET|LTEF><0>:CALL:CELL:PL:DRX:CYCLe:LONG:STARt:OFFSet SF40"
                ],
                [
                    "SENSe:<LTE|LTET><1>:CALL:CELL:EARFcn 37850",
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:BASic:PHYCell[:ID] 2",
                    "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -85DBM"
                ]
           ]},
    {timeline,
        [
            {5, [
                    [],
                    [
                        "SENSe:<LTE|LTET|LTEF><1>:CALL:CELL:RSEPre -75DBM"
                    ]
                ]}
        ]}
].
