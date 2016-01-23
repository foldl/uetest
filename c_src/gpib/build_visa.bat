call "C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT"

set THIS=gpib_visa.exe

del %THIS%

cl /TP -I./visa gpib_visa.c /link ./visa/visa32.lib

del "..\..\priv\%THIS%"
move %THIS% "..\..\priv\"

pause

