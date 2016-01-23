[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 38550",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 310",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -85DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ",

            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:PL:PAGing:CYCLe MS640",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:PL:PAGing:SUBFrame ONESixteenth",
            
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:PREamble:NUMBer 8",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:PREamble:GROupa:CONFig UNConfigured",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:PREamble:GROupa:SIZe 12",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:GROupa:MESSage:SIZe B56",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:OFFSet:POWer:GROupb DB10",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:RACH:ROOT:SEQuence 550",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:CONFiguration:INDex 7",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:HIGHspeed:FLAG OFF",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:NCS:CONFiguration 5",
           "SENSe:LTET0:CALL:CELL:CHANnel:PRACh:FREQuence:OFFSet 45",

            %0：DSUUUDSUUU 1：DSUUDDSUUD 2：DSUDDDSUDD 3：DSUUUDDDDD 4：DSUUDDDDDD 5：DSUDDDDDDD 6：DSUUUDSUUD
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:MACSwitch:CFI 1", 
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:GENeral CF0", 
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:SPECial CF1",            
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame0:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame1:DF DF1A", 
            %"SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame4:DF DF1A",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame5:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame6:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame9:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame0:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame1:STATe ON",
            %"SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame4:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame5:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame6:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame9:STATe ON"
        ]}
].

