@echo off
setlocal
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Build_GrassTint.ps1" %*
set "BUILD_RESULT=%ERRORLEVEL%"
if not "%BUILD_RESULT%"=="0" echo Build failed. See the message above.
pause
exit /b %BUILD_RESULT%
