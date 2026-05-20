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

:: ============================================
:: Pack developer build (CoP, CS, SoC)
:: ============================================
call :PackDeveloper cop patch
call :PackDeveloper cs patch_cs
call :PackDeveloper soc patch_soc

:: ============================================
:: Pack game build (CoP, CS, SoC)
:: ============================================
call :PackGame cop patch
call :PackGame cs patch_cs
call :PackGame soc patch_soc

:: ============================================
:: Pack editors build (CoP, CS, SoC)
:: ============================================
call :PackEditors cop patch
call :PackEditors cs patch_cs
call :PackEditors soc patch_soc

:: ============================================
:: Cleanup temp directories
:: ============================================
rd /s /q build\x64\Server-Windows\temp-developer 2>nul
rd /s /q build\x64\Server-Windows\temp-game 2>nul
rd /s /q build\x64\Editors-Windows\temp-editor 2>nul

echo All packages created successfully!
endlocal
exit /b 0

:: ============================================
:: Pack developer build
:: ============================================
:PackDeveloper
setlocal
set "variant=%~1"
set "patch_dir=%~2"

if not exist "%patch_dir%" (
    echo Warning: %patch_dir% not found, skipping developer-%variant%
    endlocal
    exit /b 0
)

set "temp_dir=build\x64\Server-Windows\temp-developer"
if exist "%temp_dir%" rd /s /q "%temp_dir%"
mkdir "%temp_dir%"

copy fsgame.ltx "%temp_dir%" >nul
copy .xrignore "%temp_dir%" >nul
xcopy "%patch_dir%" "%temp_dir%\gamedata" /e /i /q >nul
xcopy "build\x64\Server-Windows\bin\RelWithDebInfo" "%temp_dir%\bin" /e /i /q >nul

pushd "%temp_dir%"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-develop-%variant%.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!".xrignore" ^
    -xr!"*.pdb"
popd

move "%temp_dir%\ixray-1.6-r%release%-engine-x64-develop-%variant%.zip" ^
    "publish\ixray-1.6-r%release%-engine-x64-develop-%variant%.zip" >nul

rd /s /q "%temp_dir%" 2>nul
endlocal
exit /b 0

:: ============================================
:: Pack game build
:: ============================================
:PackGame
setlocal
set "variant=%~1"
set "patch_dir=%~2"

if not exist "%patch_dir%" (
    echo Warning: %patch_dir% not found, skipping game-%variant%
    endlocal
    exit /b 0
)

set "temp_dir=build\x64\Server-Windows\temp-game"
if exist "%temp_dir%" rd /s /q "%temp_dir%"
mkdir "%temp_dir%"

copy fsgame.ltx "%temp_dir%" >nul
copy .xrignore "%temp_dir%" >nul
xcopy "%patch_dir%" "%temp_dir%\gamedata" /e /i /q >nul
xcopy "build\x64\Server-Windows\bin\Release" "%temp_dir%\bin" /e /i /q >nul

pushd "%temp_dir%"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-game-%variant%.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!".xrignore" ^
    -xr!"*.pdb"
popd

move "%temp_dir%\ixray-1.6-r%release%-engine-x64-game-%variant%.zip" ^
    "publish\ixray-1.6-r%release%-engine-x64-game-%variant%.zip" >nul

rd /s /q "%temp_dir%" 2>nul
endlocal
exit /b 0

:: ============================================
:: Pack editors build
:: ============================================
:PackEditors
setlocal
set "variant=%~1"
set "patch_dir=%~2"

if not exist "%patch_dir%" (
    echo Warning: %patch_dir% not found, skipping editors-%variant%
    endlocal
    exit /b 0
)

set "temp_dir=build\x64\Editors-Windows\temp-editor"
if exist "%temp_dir%" rd /s /q "%temp_dir%"
mkdir "%temp_dir%"

copy fs*.ltx "%temp_dir%" >nul
copy .xrignore "%temp_dir%" >nul
copy ActorEditorLevel.cform "%temp_dir%" >nul
xcopy "%patch_dir%" "%temp_dir%\gamedata" /e /i /q >nul
xcopy "rawdata" "%temp_dir%\rawdata" /e /i /q >nul
xcopy "build\x64\Editors-Windows\bin\RelWithDebInfo" "%temp_dir%\bin" /e /i /q >nul

pushd "%temp_dir%"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-editors-x64-%variant%.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"rawdata" ^
    -ir!"fs*.ltx" ^
    -ir!".xrignore" ^
    -ir!"ActorEditorLevel.cform" ^
    -xr!"*.pdb"
popd

move "%temp_dir%\ixray-1.6-r%release%-editors-x64-%variant%.zip" ^
    "publish\ixray-1.6-r%release%-editors-x64-%variant%.zip" >nul

rd /s /q "%temp_dir%" 2>nul
endlocal
exit /b 0
