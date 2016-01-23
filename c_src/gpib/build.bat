set THIS=gpib.exe

del %THIS%
g++ -fpermissive -o gpib.exe -I .\ni .\ni\gpib-32.obj GPIB.c

del "..\..\priv\%THIS%"
move %THIS% "..\..\priv\"

pause
