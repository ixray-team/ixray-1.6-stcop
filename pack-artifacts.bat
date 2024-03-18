:: Delete x86 files
del build\x86\Utilities\bin\Debug\xrAbstractions.dll
del build\x86\Utilities\bin\Debug\xrAbstractions.pdb
del build\x86\Utilities\bin\Debug\xrEngine.exe
del build\x86\Utilities\bin\Debug\xrEngine.pdb
del build\x86\Utilities\bin\Debug\xrGame.dll
del build\x86\Utilities\bin\Debug\xrGame.pdb
del build\x86\Utilities\bin\Debug\xrGameSpy.dll
del build\x86\Utilities\bin\Debug\xrGameSpy.pdb
del build\x86\Utilities\bin\Debug\xrRender_R1.dll
del build\x86\Utilities\bin\Debug\xrRender_R1.pdb
del build\x86\Utilities\bin\Debug\xrRender_R2.dll
del build\x86\Utilities\bin\Debug\xrRender_R2.pdb
del build\x86\Utilities\bin\Debug\xrRender_R4.dll
del build\x86\Utilities\bin\Debug\xrRender_R4.pdb
del build\x86\Utilities\bin\Debug\xrServer.exe
del build\x86\Utilities\bin\Debug\xrServer.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrAbstractions.dll
del build\x86\Utilities\bin\RelWithDebInfo\xrAbstractions.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrEngine.exe
del build\x86\Utilities\bin\RelWithDebInfo\xrEngine.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrGame.dll
del build\x86\Utilities\bin\RelWithDebInfo\xrGame.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrGameSpy.dll
del build\x86\Utilities\bin\RelWithDebInfo\xrGameSpy.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrRender_R1.dll
del build\x86\Utilities\bin\RelWithDebInfo\xrRender_R1.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrRender_R2.dll
del build\x86\Utilities\bin\RelWithDebInfo\xrRender_R2.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrRender_R4.dll
del build\x86\Utilities\bin\RelWithDebInfo\xrRender_R4.pdb
del build\x86\Utilities\bin\RelWithDebInfo\xrServer.exe
del build\x86\Utilities\bin\RelWithDebInfo\xrServer.pdb
del build\x86\Utilities\bin\Release\xrAbstractions.dll
del build\x86\Utilities\bin\Release\xrEngine.exe
del build\x86\Utilities\bin\Release\xrGame.dll
del build\x86\Utilities\bin\Release\xrGameSpy.dll
del build\x86\Utilities\bin\Release\xrRender_R1.dll
del build\x86\Utilities\bin\Release\xrRender_R2.dll
del build\x86\Utilities\bin\Release\xrRender_R4.dll
del build\x86\Utilities\bin\Release\xrServer.exe

:: Delete x64 files
del build\x64\Utilities\bin\Debug\xrAbstractions.dll
del build\x64\Utilities\bin\Debug\xrAbstractions.pdb
del build\x64\Utilities\bin\Debug\xrEngine.exe
del build\x64\Utilities\bin\Debug\xrEngine.pdb
del build\x64\Utilities\bin\Debug\xrGame.dll
del build\x64\Utilities\bin\Debug\xrGame.pdb
del build\x64\Utilities\bin\Debug\xrGameSpy.dll
del build\x64\Utilities\bin\Debug\xrGameSpy.pdb
del build\x64\Utilities\bin\Debug\xrRender_R1.dll
del build\x64\Utilities\bin\Debug\xrRender_R1.pdb
del build\x64\Utilities\bin\Debug\xrRender_R2.dll
del build\x64\Utilities\bin\Debug\xrRender_R2.pdb
del build\x64\Utilities\bin\Debug\xrRender_R4.dll
del build\x64\Utilities\bin\Debug\xrRender_R4.pdb
del build\x64\Utilities\bin\Debug\xrServer.exe
del build\x64\Utilities\bin\Debug\xrServer.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrAbstractions.dll
del build\x64\Utilities\bin\RelWithDebInfo\xrAbstractions.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrEngine.exe
del build\x64\Utilities\bin\RelWithDebInfo\xrEngine.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrGame.dll
del build\x64\Utilities\bin\RelWithDebInfo\xrGame.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrGameSpy.dll
del build\x64\Utilities\bin\RelWithDebInfo\xrGameSpy.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrRender_R1.dll
del build\x64\Utilities\bin\RelWithDebInfo\xrRender_R1.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrRender_R2.dll
del build\x64\Utilities\bin\RelWithDebInfo\xrRender_R2.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrRender_R4.dll
del build\x64\Utilities\bin\RelWithDebInfo\xrRender_R4.pdb
del build\x64\Utilities\bin\RelWithDebInfo\xrServer.exe
del build\x64\Utilities\bin\RelWithDebInfo\xrServer.pdb
del build\x64\Utilities\bin\Release\xrAbstractions.dll
del build\x64\Utilities\bin\Release\xrEngine.exe
del build\x64\Utilities\bin\Release\xrGame.dll
del build\x64\Utilities\bin\Release\xrGameSpy.dll
del build\x64\Utilities\bin\Release\xrRender_R1.dll
del build\x64\Utilities\bin\Release\xrRender_R2.dll
del build\x64\Utilities\bin\Release\xrRender_R4.dll
del build\x64\Utilities\bin\Release\xrServer.exe

:: Packing packages
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-packages.zip" ^
    "packages"
popd
move build\x64\Utilities\ixray-1.6-r0.8-packages.zip ixray-1.6-r0.8-packages.zip

:: Packing dependencies
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-dependencies.zip" ^
    "dep"
popd
move build\x64\Utilities\ixray-1.6-r0.8-dependencies.zip ixray-1.6-r0.8-dependencies.zip

:: Packing assets
move gamedata gamedata_main
move patch gamedata
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-assets.zip" ^
    "gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!"patches\xpatch_03.db" ^
    -xr!"temp\fsgame.ltx"
move gamedata patch
move gamedata_main gamedata

:: Packing x86 engine binaries
pushd build\x86\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-debug-bin.zip ixray-1.6-r0.8-engine-x86-debug-bin.zip
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-mixed-bin.zip ixray-1.6-r0.8-engine-x86-mixed-bin.zip
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-release-bin.zip ixray-1.6-r0.8-engine-x86-release-bin.zip
:: Packing x86 engine libraries
pushd build\x86\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-debug-lib.zip ixray-1.6-r0.8-engine-x86-debug-lib.zip
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-release-lib.zip ixray-1.6-r0.8-engine-x86-release-lib.zip
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-mixed-lib.zip ixray-1.6-r0.8-engine-x86-mixed-lib.zip
:: Packing x86 engine symbols
pushd build\x86\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x86-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-debug-pdb.zip ixray-1.6-r0.8-engine-x86-debug-pdb.zip
move build\x86\Engine\ixray-1.6-r0.8-engine-x86-mixed-pdb.zip ixray-1.6-r0.8-engine-x86-mixed-pdb.zip

:: Packing x64 engine binaries
pushd build\x64\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-debug-bin.zip ixray-1.6-r0.8-engine-x64-debug-bin.zip
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-mixed-bin.zip ixray-1.6-r0.8-engine-x64-mixed-bin.zip
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-release-bin.zip ixray-1.6-r0.8-engine-x64-release-bin.zip
:: Packing x64 engine libraries
pushd build\x64\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-debug-lib.zip ixray-1.6-r0.8-engine-x64-debug-lib.zip
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-release-lib.zip ixray-1.6-r0.8-engine-x64-release-lib.zip
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-mixed-lib.zip ixray-1.6-r0.8-engine-x64-mixed-lib.zip
:: Packing x64 engine symbols
pushd build\x64\Engine
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-engine-x64-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-debug-pdb.zip ixray-1.6-r0.8-engine-x64-debug-pdb.zip
move build\x64\Engine\ixray-1.6-r0.8-engine-x64-mixed-pdb.zip ixray-1.6-r0.8-engine-x64-mixed-pdb.zip

:: Packing x86 server binaries
pushd build\x86\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x86\Server\ixray-1.6-r0.8-server-x86-debug-bin.zip ixray-1.6-r0.8-server-x86-debug-bin.zip
move build\x86\Server\ixray-1.6-r0.8-server-x86-mixed-bin.zip ixray-1.6-r0.8-server-x86-mixed-bin.zip
move build\x86\Server\ixray-1.6-r0.8-server-x86-release-bin.zip ixray-1.6-r0.8-server-x86-release-bin.zip
:: Packing x86 server libraries
pushd build\x86\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x86\Server\ixray-1.6-r0.8-server-x86-debug-lib.zip ixray-1.6-r0.8-server-x86-debug-lib.zip
move build\x86\Server\ixray-1.6-r0.8-server-x86-mixed-lib.zip ixray-1.6-r0.8-server-x86-mixed-lib.zip
move build\x86\Server\ixray-1.6-r0.8-server-x86-release-lib.zip ixray-1.6-r0.8-server-x86-release-lib.zip
:: Packing x86 server symbols
pushd build\x86\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x86-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x86\Server\ixray-1.6-r0.8-server-x86-debug-pdb.zip ixray-1.6-r0.8-server-x86-debug-pdb.zip
move build\x86\Server\ixray-1.6-r0.8-server-x86-mixed-pdb.zip ixray-1.6-r0.8-server-x86-mixed-pdb.zip

:: Packing x64 server binaries
pushd build\x64\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x64\Server\ixray-1.6-r0.8-server-x64-debug-bin.zip ixray-1.6-r0.8-server-x64-debug-bin.zip
move build\x64\Server\ixray-1.6-r0.8-server-x64-mixed-bin.zip ixray-1.6-r0.8-server-x64-mixed-bin.zip
move build\x64\Server\ixray-1.6-r0.8-server-x64-release-bin.zip ixray-1.6-r0.8-server-x64-release-bin.zip
:: Packing x64 server libraries
pushd build\x64\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x64\Server\ixray-1.6-r0.8-server-x64-debug-lib.zip ixray-1.6-r0.8-server-x64-debug-lib.zip
move build\x64\Server\ixray-1.6-r0.8-server-x64-mixed-lib.zip ixray-1.6-r0.8-server-x64-mixed-lib.zip
move build\x64\Server\ixray-1.6-r0.8-server-x64-release-lib.zip ixray-1.6-r0.8-server-x64-release-lib.zip
:: Packing x64 server symbols
pushd build\x64\Server
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-server-x64-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x64\Server\ixray-1.6-r0.8-server-x64-debug-pdb.zip ixray-1.6-r0.8-server-x64-debug-pdb.zip
move build\x64\Server\ixray-1.6-r0.8-server-x64-mixed-pdb.zip ixray-1.6-r0.8-server-x64-mixed-pdb.zip

:: Packing x86 utilities binaries
pushd build\x86\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-debug-bin.zip ixray-1.6-r0.8-utilities-x86-debug-bin.zip
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-mixed-bin.zip ixray-1.6-r0.8-utilities-x86-mixed-bin.zip
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-release-bin.zip ixray-1.6-r0.8-utilities-x86-release-bin.zip
:: Packing x86 utilities libraries
pushd build\x86\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-debug-lib.zip ixray-1.6-r0.8-utilities-x86-debug-lib.zip
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-mixed-lib.zip ixray-1.6-r0.8-utilities-x86-mixed-lib.zip
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-release-lib.zip ixray-1.6-r0.8-utilities-x86-release-lib.zip
:: Packing x86 utilities symbols
pushd build\x86\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x86-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-debug-pdb.zip ixray-1.6-r0.8-utilities-x86-debug-pdb.zip
move build\x86\Utilities\ixray-1.6-r0.8-utilities-x86-mixed-pdb.zip ixray-1.6-r0.8-utilities-x86-mixed-pdb.zip

:: Packing x64 utilities binaries
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-debug-bin.zip ixray-1.6-r0.8-utilities-x64-debug-bin.zip
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-mixed-bin.zip ixray-1.6-r0.8-utilities-x64-mixed-bin.zip
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-release-bin.zip ixray-1.6-r0.8-utilities-x64-release-bin.zip
:: Packing x64 utilities libraries
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-debug-lib.zip ixray-1.6-r0.8-utilities-x64-debug-lib.zip
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-mixed-lib.zip ixray-1.6-r0.8-utilities-x64-mixed-lib.zip
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-release-lib.zip ixray-1.6-r0.8-utilities-x64-release-lib.zip
:: Packing x64 utilities symbols
pushd build\x64\Utilities
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r0.8-utilities-x64-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-debug-pdb.zip ixray-1.6-r0.8-utilities-x64-debug-pdb.zip
move build\x64\Utilities\ixray-1.6-r0.8-utilities-x64-mixed-pdb.zip ixray-1.6-r0.8-utilities-x64-mixed-pdb.zip
