:: Delete incremental linking information
if exist bin\*.ilk (del /s bin\*.ilk)
if exist bin\*.idb (del /s bin\*.idb)
if exist lib\*.idb (del /s lib\*.idb)

:: Delete symbols
if exist bin\*.pdb (del /s bin\*.pdb)
if exist lib\*.pdb (del /s lib\*.pdb)
