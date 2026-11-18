@echo off
setlocal DisableDelayedExpansion
pushd "%~dp0" || exit /b 1
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Build_Install_GrassTint_Mask.ps1" -GameRoot "%CD%" %*
set "BUILD_RESULT=%ERRORLEVEL%"
if not "%BUILD_RESULT%"=="0" echo Build or installation failed. See the error above.
pause
popd
exit /b %BUILD_RESULT%
