@echo off

set coff2omf="%programfiles(x86)%\Embarcadero\Studio\21.0\bin\coff2omf.exe"

%coff2omf% sdk\dxsdk_jun2010\Lib\x86\dsound.lib lib\dsoundB.lib
%coff2omf% sdk\libraries\eax.lib lib\eaxB.lib

%coff2omf% lib\Debug\libogg.lib lib\liboggB.lib
%coff2omf% lib\Debug\libtheora.lib lib\libtheoraB.lib
%coff2omf% lib\Debug\libvorbis.lib lib\libvorbisB.lib
%coff2omf% lib\Debug\libvorbisfile.lib lib\libvorbisfileB.lib

%coff2omf% lib\openal32.lib lib\openal32B.lib
%coff2omf% lib\ETools.lib lib\EToolsB.lib -v

pause
