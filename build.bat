..\lazarus64\lazbuild.exe gravityMain.lpi --bm=default
..\lazarus64\fpc\3.2.2\bin\x86_64-win64\delp.exe -r .
@if not "%1"=="upx" goto end
upx.exe --best --ultra-brute -o gravity_upx.exe gravity.exe
move /Y gravity_upx.exe gravity.exe
:end