:: Configure server
cmake --preset Server-x64-Windows

:: Build server
cmake --build --preset Server-x64-Windows-Debug
cmake --build --preset Server-x64-Windows-RelWithDebInfo
cmake --build --preset Server-x64-Windows-Release
