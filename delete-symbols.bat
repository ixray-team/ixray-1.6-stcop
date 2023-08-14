:: Delete incremental linking information
cd bin
for /r %i in (*.ilk) do @del "%i"
for /r %i in (*.idb) do @del "%i"
cd ..\lib
for /r %i in (*.idb) do @del "%i"

:: Delete symbols
cd ..\bin
for /r %i in (*.pdb) do @del "%i"
cd ..\lib
for /r %i in (*.pdb) do @del "%i"
