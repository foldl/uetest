[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 37750",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 110",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -85DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 20MHZ",

            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:PL:PAGing:CYCLe MS320",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:PL:PAGing:SUBFrame QUARter",
            
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:PREamble:GROupa:CONFig CONFigured",
            "SENSe:<LTE><0|1|2>:CALL:CELL:CHANnel:PRACh:PREamble:NUMBer 40",            
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:PREamble:GROupa:SIZe 4",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:GROupa:MESSage:SIZe B256",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:OFFSet:POWer:GROupb DB18",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:RACH:ROOT:SEQuence 800",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:CONFiguration:INDex 61",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:HIGHspeed:FLAG ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:NCS:CONFiguration 13",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:FREQuence:OFFSet 10",
            
            %0：DSUUUDSUUU 1：DSUUDDSUUD 2：DSUDDDSUDD 3：DSUUUDDDDD 4：DSUUDDDDDD 5：DSUDDDDDDD 6：DSUUUDSUUD
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:MACSwitch:CFI 1", 
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:GENeral CF2", 
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:SPECial CF0",            
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame0:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame1:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame4:DF DF1A",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame5:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame6:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame9:DF DF1A", 
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame0:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame1:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame4:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame5:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame6:STATe ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:SCHeduling:DL:SUBFrame9:STATe ON"
        ]}
].

