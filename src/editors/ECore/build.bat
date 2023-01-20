call "%ProgramFiles(x86)%"\Embarcadero\Studio\21.0\bin\rsvars.bat
msbuild /v:diag "%~dp0xrECoreB.cbproj"
