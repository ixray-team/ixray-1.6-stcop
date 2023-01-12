:: Get assets
git clone ./.git ./temp
cd temp
git checkout c87b43f286f4a4fa5873a2eb4977d77571e51991
cd ..
move temp/gamedata gamedata_origin

:: Generate patch
bin\xrCompress.exe -diff gamedata gamedata_origin -out patch

:: Pack patch
bin\xrCompress.exe patch -ltx datapack.ltx
mkdir patches
move patch.pack_#0 patches\xpatch_03.db
