:: Deleting unnecessary files
del "bin\*.ilk" /s
del "bin\*.lib" /s
del "bin\*.exp" /s
del "lib\*.idb" /s
del "lib\*.pdb" /s

:: Packing packages
"%ProgramFiles%"\7-Zip\7z.exe a "packages.zip" "packages"

:: Packing engine binaries
"%ProgramFiles%"\7-Zip\7z.exe a "bin-engine-debug.zip" "bin\Win32\Debug" ^
    -ir!"bin\Win32\Debug\*" ^
    -xr!"*.pdb" -xr!"bin\utils\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "bin-engine-mixed.zip" "bin\Win32\Mixed" ^
    -ir!"bin\Win32\Mixed\*" ^
    -xr!"*.pdb" -xr!"bin\utils\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "bin-engine-release.zip" "bin\Win32\Release" ^
    -ir!"bin\Win32\Release\*" ^
    -xr!"*.pdb" -xr!"bin\utils\Win32\*" -xr!"bin\dedicated\Win32\*"

:: Packing server binaries
"%ProgramFiles%"\7-Zip\7z.exe a "bin-server-debug.zip" "bin\dedicated\Win32\Debug" ^
    -ir!"bin\dedicated\Win32\Debug\*" ^
    -xr!"*.pdb" -xr!"bin\Win32\*" -xr!"bin\utils\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "bin-server-mixed.zip" "bin\dedicated\Win32\Mixed" ^
    -ir!"bin\dedicated\Win32\Mixed\*" ^
    -xr!"*.pdb" -xr!"bin\Win32\*" -xr!"bin\utils\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "bin-server-release.zip" "bin\dedicated\Win32\Release" ^
    -ir!"bin\dedicated\Win32\Release\*" ^
    -xr!"*.pdb" -xr!"bin\Win32\*" -xr!"bin\utils\Win32\*"

:: Packing utility binaries
"%ProgramFiles%"\7-Zip\7z.exe a "bin-utils-debug.zip" "bin\utils\Win32\Debug" ^
    -ir!"bin\utils\Win32\Debug\*" ^
    -xr!"*.pdb" -xr!"bin\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "bin-utils-mixed.zip" "bin\utils\Win32\Mixed" ^
    -ir!"bin\utils\Win32\Mixed\*" ^
    -xr!"*.pdb" -xr!"bin\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "bin-utils-release.zip" "bin\utils\Win32\Release" ^
    -ir!"bin\utils\Win32\Release\*" ^
    -xr!"*.pdb" -xr!"bin\Win32\*" -xr!"bin\dedicated\Win32\*"

:: Packing engine libraries
"%ProgramFiles%"\7-Zip\7z.exe a "lib-engine-debug.zip" "lib\Win32\Debug" ^
    -ir!"lib\Win32\Debug\*" ^
    -xr!"lib\utils\Win32\*" -xr!"lib\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "lib-engine-mixed.zip" "lib\Win32\Mixed" ^
    -ir!"lib\Win32\Mixed\*" ^
    -xr!"lib\utils\Win32\*" -xr!"lib\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "lib-engine-release.zip" "lib\Win32\Release" ^
    -ir!"lib\Win32\Release\*" ^
    -xr!"lib\utils\Win32\*" -xr!"lib\dedicated\Win32\*"

:: Packing server libraries
"%ProgramFiles%"\7-Zip\7z.exe a "lib-server-debug.zip" "lib\dedicated\Win32\Debug" ^
    -ir!"lib\dedicated\Win32\Debug\*" ^
    -xr!"lib\Win32\*" -xr!"lib\utils\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "lib-server-mixed.zip" "lib\dedicated\Win32\Mixed" ^
    -ir!"lib\dedicated\Win32\Mixed\*" ^
    -xr!"lib\Win32\*" -xr!"lib\utils\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "lib-server-release.zip" "lib\dedicated\Win32\Release" ^
    -ir!"lib\dedicated\Win32\Release\*" ^
    -xr!"lib\Win32\*" -xr!"lib\utils\Win32\*"

:: Packing utility libraries
"%ProgramFiles%"\7-Zip\7z.exe a "lib-utils-debug.zip" "lib\utils\Win32\Debug" ^
    -ir!"lib\utils\Win32\Debug\*" ^
    -xr!"lib\Win32\*" -xr!"lib\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "lib-utils-mixed.zip" "lib\utils\Win32\Mixed" ^
    -ir!"lib\utils\Win32\Mixed\*" ^
    -xr!"lib\Win32\*" -xr!"lib\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "lib-utils-release.zip" "lib\utils\Win32\Release" ^
    -ir!"lib\utils\Win32\Release\*" ^
    -xr!"lib\Win32\*" -xr!"lib\dedicated\Win32\*"

:: Packing engine symbols
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-engine-debug.zip" "bin\Win32\Debug" ^
    -ir!"bin\Win32\Debug\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\utils\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-engine-mixed.zip" "bin\Win32\Mixed" ^
    -ir!"bin\Win32\Mixed\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\utils\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-engine-release.zip" "bin\Win32\Release" ^
    -ir!"bin\Win32\Release\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\utils\Win32\*" -xr!"bin\dedicated\Win32\*"

:: Packing server symbols
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-server-debug.zip" "bin\dedicated\Win32\Debug" ^
    -ir!"bin\dedicated\Win32\Debug\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\Win32\*" -xr!"bin\utils\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-server-mixed.zip" "bin\dedicated\Win32\Mixed" ^
    -ir!"bin\dedicated\Win32\Mixed\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\Win32\*" -xr!"bin\utils\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-server-release.zip" "bin\dedicated\Win32\Release" ^
    -ir!"bin\dedicated\Win32\Release\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\Win32\*" -xr!"bin\utils\Win32\*"

:: Packing utility symbols
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-utils-debug.zip" "bin\utils\Win32\Debug" ^
    -ir!"bin\utils\Win32\Debug\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-utils-mixed.zip" "bin\utils\Win32\Mixed" ^
    -ir!"bin\utils\Win32\Mixed\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\Win32\*" -xr!"bin\dedicated\Win32\*"
"%ProgramFiles%"\7-Zip\7z.exe a "pdb-utils-release.zip" "bin\utils\Win32\Release" ^
    -ir!"bin\utils\Win32\Release\*" ^
    -xr!"*.exe" -xr!"*.dll" -xr!"bin\Win32\*" -xr!"bin\dedicated\Win32\*"
