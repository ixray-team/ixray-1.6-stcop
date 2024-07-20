:: Configure compressor
cmake --preset Compressor-x64-Windows

:: Build compressor
cmake --build --preset Compressor-x64-Windows-Debug
cmake --build --preset Compressor-x64-Windows-RelWithDebInfo
cmake --build --preset Compressor-x64-Windows-Release
