@echo off

title IX-Ray

if exist "%windir%\SysWOW64" (
    copy "%~dp0\sdk\components\ElPack\bpl\*.BPL" "%windir%\SysWOW64" /y
) else (
    copy "%~dp0\sdk\components\ElPack\bpl\*.BPL" "%windir%\System32" /y
)

pause
