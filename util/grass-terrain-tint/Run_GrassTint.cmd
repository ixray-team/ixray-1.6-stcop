@echo off
setlocal DisableDelayedExpansion
pushd "%~dp0" || exit /b 1
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Prepare_GameFolder.ps1"
if errorlevel 1 (
    echo Startup configuration could not be prepared. See the error above.
    pause
    popd
    exit /b 1
)
"bin\xrEngine.exe" -r4 -fsltx fsgame_grass_tint.ltx %*
set "GAME_RESULT=%ERRORLEVEL%"
popd
exit /b %GAME_RESULT%
