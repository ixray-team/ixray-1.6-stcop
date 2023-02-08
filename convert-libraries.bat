@echo off

set coff2omf="%programfiles(x86)%\Embarcadero\Studio\21.0\bin\coff2omf.exe"

%coff2omf% sdk\dxsdk_jun2010\Lib\x86\dinput8.lib lib\dinput8B.lib -v
%coff2omf% sdk\dxsdk_jun2010\Lib\x86\dsound.lib lib\dsoundB.lib -v
%coff2omf% sdk\libraries\eax.lib lib\eaxB.lib -v
%coff2omf% sdk\libraries\FreeImage.lib lib\FreeImageB.lib -v
%coff2omf% sdk\libraries\MagicFM.lib lib\MagicFMB.lib

%coff2omf% lib\Debug\libogg.lib lib\liboggB.lib -v
%coff2omf% lib\Debug\libtheora.lib lib\libtheoraB.lib -v
%coff2omf% lib\Debug\libvorbis.lib lib\libvorbisB.lib -v
%coff2omf% lib\Debug\libvorbisfile.lib lib\libvorbisfileB.lib -v

%coff2omf% lib\Release\libogg.lib lib\liboggB.lib -v
%coff2omf% lib\Release\libtheora.lib lib\libtheoraB.lib -v
%coff2omf% lib\Release\libvorbis.lib lib\libvorbisB.lib -v
%coff2omf% lib\Release\libvorbisfile.lib lib\libvorbisfileB.lib -v

%coff2omf% lib\openal32.lib lib\openal32B.lib -v
%coff2omf% lib\DXT.lib lib\DXTB.lib -v
%coff2omf% lib\ETools.lib lib\EToolsB.lib -v
%coff2omf% lib\LWO.lib lib\LWOB.lib -v
%coff2omf% lib\xrPhysics.lib lib\xrPhysicsB.lib -v
