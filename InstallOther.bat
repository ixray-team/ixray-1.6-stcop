@echo off

title IX-Ray

if exist "%programfiles(x86)%\Borland\CBuilder6" (
    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Include\Vcl\*.h" "%programfiles(x86)%\Borland\CBuilder6\Include\Vcl" /y

    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Projects\Bpl\*.bpl" "%programfiles(x86)%\Borland\CBuilder6\Projects\Bpl" /y

    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Projects\Lib\*.bpi" "%programfiles(x86)%\Borland\CBuilder6\Projects\Lib" /y
    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Projects\Lib\*.lib" "%programfiles(x86)%\Borland\CBuilder6\Projects\Lib" /y
) else (
    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Include\Vcl\*.h" "%programfiles%\Borland\CBuilder6\Include\Vcl" /y

    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Projects\Bpl\*.bpl" "%programfiles%\Borland\CBuilder6\Projects\Bpl" /y

    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Projects\Lib\*.bpi" "%programfiles%\Borland\CBuilder6\Projects\Lib" /y
    copy "%~dp0\sdk\bcb6\Borland\CBuilder6\Projects\Lib\*.lib" "%programfiles%\Borland\CBuilder6\Projects\Lib" /y
)

pause
