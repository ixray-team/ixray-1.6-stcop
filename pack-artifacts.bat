:: Packing packages
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-packages.zip" ^
    "packages"
popd
move build\x64\Utilities\ixray-1.6-r0.7-packages.zip ixray-1.6-r0.7-packages.zip

:: Packing assets
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-assets.zip" ^
    "gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!"patches\xpatch_03.db" ^
    -xr!"temp\fsgame.ltx"

:: Packing x86 engine binaries
pushd build\x86\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-debug-bin.zip ixray-1.6-r0.7-engine-x86-debug-bin.zip
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-mixed-bin.zip ixray-1.6-r0.7-engine-x86-mixed-bin.zip
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-release-bin.zip ixray-1.6-r0.7-engine-x86-release-bin.zip
:: Packing x86 engine libraries
pushd build\x86\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-debug-lib.zip ixray-1.6-r0.7-engine-x86-debug-lib.zip
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-release-lib.zip ixray-1.6-r0.7-engine-x86-release-lib.zip
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-mixed-lib.zip ixray-1.6-r0.7-engine-x86-mixed-lib.zip
:: Packing x86 engine symbols
pushd build\x86\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x86-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-debug-pdb.zip ixray-1.6-r0.7-engine-x86-debug-pdb.zip
move build\x86\Engine\ixray-1.6-r0.7-engine-x86-mixed-pdb.zip ixray-1.6-r0.7-engine-x86-mixed-pdb.zip

:: Packing x64 engine binaries
pushd build\x64\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-debug-bin.zip ixray-1.6-r0.7-engine-x64-debug-bin.zip
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-mixed-bin.zip ixray-1.6-r0.7-engine-x64-mixed-bin.zip
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-release-bin.zip ixray-1.6-r0.7-engine-x64-release-bin.zip
:: Packing x64 engine libraries
pushd build\x64\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-debug-lib.zip ixray-1.6-r0.7-engine-x64-debug-lib.zip
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-release-lib.zip ixray-1.6-r0.7-engine-x64-release-lib.zip
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-mixed-lib.zip ixray-1.6-r0.7-engine-x64-mixed-lib.zip
:: Packing x64 engine symbols
pushd build\x64\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-engine-x64-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-debug-pdb.zip ixray-1.6-r0.7-engine-x64-debug-pdb.zip
move build\x64\Engine\ixray-1.6-r0.7-engine-x64-mixed-pdb.zip ixray-1.6-r0.7-engine-x64-mixed-pdb.zip

:: Packing x86 server binaries
pushd build\x86\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x86\Server\ixray-1.6-r0.7-server-x86-debug-bin.zip ixray-1.6-r0.7-server-x86-debug-bin.zip
move build\x86\Server\ixray-1.6-r0.7-server-x86-mixed-bin.zip ixray-1.6-r0.7-server-x86-mixed-bin.zip
move build\x86\Server\ixray-1.6-r0.7-server-x86-release-bin.zip ixray-1.6-r0.7-server-x86-release-bin.zip
:: Packing x86 server libraries
pushd build\x86\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x86\Server\ixray-1.6-r0.7-server-x86-debug-lib.zip ixray-1.6-r0.7-server-x86-debug-lib.zip
move build\x86\Server\ixray-1.6-r0.7-server-x86-mixed-lib.zip ixray-1.6-r0.7-server-x86-mixed-lib.zip
move build\x86\Server\ixray-1.6-r0.7-server-x86-release-lib.zip ixray-1.6-r0.7-server-x86-release-lib.zip
:: Packing x86 server symbols
pushd build\x86\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x86-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x86\Server\ixray-1.6-r0.7-server-x86-debug-pdb.zip ixray-1.6-r0.7-server-x86-debug-pdb.zip
move build\x86\Server\ixray-1.6-r0.7-server-x86-mixed-pdb.zip ixray-1.6-r0.7-server-x86-mixed-pdb.zip

:: Packing x64 server binaries
pushd build\x64\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x64\Server\ixray-1.6-r0.7-server-x64-debug-bin.zip ixray-1.6-r0.7-server-x64-debug-bin.zip
move build\x64\Server\ixray-1.6-r0.7-server-x64-mixed-bin.zip ixray-1.6-r0.7-server-x64-mixed-bin.zip
move build\x64\Server\ixray-1.6-r0.7-server-x64-release-bin.zip ixray-1.6-r0.7-server-x64-release-bin.zip
:: Packing x64 server libraries
pushd build\x64\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x64\Server\ixray-1.6-r0.7-server-x64-debug-lib.zip ixray-1.6-r0.7-server-x64-debug-lib.zip
move build\x64\Server\ixray-1.6-r0.7-server-x64-mixed-lib.zip ixray-1.6-r0.7-server-x64-mixed-lib.zip
move build\x64\Server\ixray-1.6-r0.7-server-x64-release-lib.zip ixray-1.6-r0.7-server-x64-release-lib.zip
:: Packing x64 server symbols
pushd build\x64\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-server-x64-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x64\Server\ixray-1.6-r0.7-server-x64-debug-pdb.zip ixray-1.6-r0.7-server-x64-debug-pdb.zip
move build\x64\Server\ixray-1.6-r0.7-server-x64-mixed-pdb.zip ixray-1.6-r0.7-server-x64-mixed-pdb.zip

:: Packing x86 utilities binaries
pushd build\x86\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-debug-bin.zip ixray-1.6-r0.7-utilities-x86-debug-bin.zip
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-mixed-bin.zip ixray-1.6-r0.7-utilities-x86-mixed-bin.zip
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-release-bin.zip ixray-1.6-r0.7-utilities-x86-release-bin.zip
:: Packing x86 utilities libraries
pushd build\x86\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-debug-lib.zip ixray-1.6-r0.7-utilities-x86-debug-lib.zip
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-mixed-lib.zip ixray-1.6-r0.7-utilities-x86-mixed-lib.zip
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-release-lib.zip ixray-1.6-r0.7-utilities-x86-release-lib.zip
:: Packing x86 utilities symbols
pushd build\x86\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x86-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-debug-pdb.zip ixray-1.6-r0.7-utilities-x86-debug-pdb.zip
move build\x86\Utilities\ixray-1.6-r0.7-utilities-x86-mixed-pdb.zip ixray-1.6-r0.7-utilities-x86-mixed-pdb.zip

:: Packing x64 utilities binaries
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-debug-bin.zip ixray-1.6-r0.7-utilities-x64-debug-bin.zip
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-mixed-bin.zip ixray-1.6-r0.7-utilities-x64-mixed-bin.zip
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-release-bin.zip ixray-1.6-r0.7-utilities-x64-release-bin.zip
:: Packing x64 utilities libraries
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-debug-lib.zip ixray-1.6-r0.7-utilities-x64-debug-lib.zip
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-mixed-lib.zip ixray-1.6-r0.7-utilities-x64-mixed-lib.zip
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-release-lib.zip ixray-1.6-r0.7-utilities-x64-release-lib.zip
:: Packing x64 utilities symbols
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.7-utilities-x64-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-debug-pdb.zip ixray-1.6-r0.7-utilities-x64-debug-pdb.zip
move build\x64\Utilities\ixray-1.6-r0.7-utilities-x64-mixed-pdb.zip ixray-1.6-r0.7-utilities-x64-mixed-pdb.zip
