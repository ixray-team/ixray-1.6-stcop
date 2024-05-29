:: Get assets
if not exist gamedata_origin (
    git clone ./.git ./temp
    cd temp
    git checkout 2045fd565ee903b1ea1a386afd8da73466e65d1d
    cd ..
    move temp/gamedata gamedata_origin
)

:: Generate patch
build\x64\Compressor-Windows\bin\Release\xrCompress.exe -diff gamedata gamedata_origin -out patch

:: Pack patch
build\x64\Compressor-Windows\bin\Release\xrCompress.exe patch -ltx datapack.ltx
mkdir patches
move patch.pack_#0 patches\xpatch_03.db
