@echo off
setlocal DisableDelayedExpansion
pushd "%~dp0" || exit /b 1
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Install_GrassTint_Update.ps1" -GameRoot "%CD%"
set "INSTALL_RESULT=%ERRORLEVEL%"
if not "%INSTALL_RESULT%"=="0" echo Update failed. See the error above.
pause
popd
exit /b %INSTALL_RESULT%
