@echo off
setlocal
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Prepare_GrassTint.ps1" -Start %*
set "TEST_RESULT=%ERRORLEVEL%"
if not "%TEST_RESULT%"=="0" pause
exit /b %TEST_RESULT%
