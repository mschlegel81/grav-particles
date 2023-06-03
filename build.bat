..\lazarus64\lazbuild.exe gravityMain.lpi --bm=default
..\lazarus64\fpc\3.2.2\bin\x86_64-win64\delp.exe -r .
upx.exe --best --ultra-brute -o gravity_upx.exe gravity.exe
move /Y gravity_upx.exe gravity.exe