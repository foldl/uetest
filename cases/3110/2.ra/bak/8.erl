[
    {cell, {tdd, 0}},
    {cmds, 
        [
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:EARFcn 39250",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BASic:PHYCell[:ID] 503",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:RSEPre -85DBM",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:BAND:WIDTh[:ALL] 10MHZ",

            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:PL:PAGing:CYCLe MS640",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:PL:PAGing:SUBFrame ONEeighth",
            
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:PREamble:GROupa:CONFig CONFigured",
            "SENSe:<LTE><0|1|2>:CALL:CELL:CHANnel:PRACh:PREamble:NUMBer 40",            
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:PREamble:GROupa:SIZe 32",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:GROupa:MESSage:SIZe B144",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:OFFSet:POWer:GROupb DB0",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:RACH:ROOT:SEQuence 831",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:CONFiguration:INDex 0",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:HIGHspeed:FLAG ON",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:NCS:CONFiguration 9",
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:CHANnel:PRACh:FREQuence:OFFSet 20",
            
            %0：DSUUUDSUUU 1：DSUUDDSUUD 2：DSUDDDSUDD 3：DSUUUDDDDD 4：DSUUDDDDDD 5：DSUDDDDDDD 6：DSUUUDSUUD
            "SENSe:<LTE|LTET|LTEF><0|1|2>:CALL:CELL:MACSwitch:CFI 2", 
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:GENeral CF1", 
            "SENSe:<LTE|LTET><0|1|2>:CALL:CELL:BASic:SUBFrame:ASSignment:SPECial CF7",            
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

