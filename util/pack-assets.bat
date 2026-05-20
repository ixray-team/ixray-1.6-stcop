@echo off
setlocal

:: Set release
if "%~1"=="" (
    echo Usage: %~0 release
    exit /b 1
)
set "release=%~1"

:: Create publish directory if not exists
if not exist publish mkdir publish

:: Keep original gamedata
if exist gamedata (
    move gamedata gamedata_main
)

:: Pack CoP assets
if exist patch (
    xcopy /e /i patch gamedata >nul
    "%ProgramFiles%"\7-Zip\7z.exe a -tzip "publish\ixray-1.6-r%release%-assets-cop.zip" ^
        "gamedata" ^
        -ir!"fsgame.ltx" ^
        -ir!".xrignore" ^
        -xr!"temp\fsgame.ltx"
    rd /s /q gamedata
)

:: Pack CS assets
if exist patch_cs (
    xcopy /e /i patch_cs gamedata >nul
    "%ProgramFiles%"\7-Zip\7z.exe a -tzip "publish\ixray-1.6-r%release%-assets-cs.zip" ^
        "gamedata" ^
        -ir!"fsgame.ltx" ^
        -ir!".xrignore" ^
        -xr!"temp\fsgame.ltx"
    rd /s /q gamedata
)

:: Pack SoC assets
if exist patch_soc (
    xcopy /e /i patch_soc gamedata >nul
    "%ProgramFiles%"\7-Zip\7z.exe a -tzip "publish\ixray-1.6-r%release%-assets-soc.zip" ^
        "gamedata" ^
        -ir!"fsgame.ltx" ^
        -ir!".xrignore" ^
        -xr!"temp\fsgame.ltx"
    rd /s /q gamedata
)

:: Restore original gamedata
if exist gamedata_main (
    move gamedata_main gamedata
)

echo All packages created successfully!
endlocal
