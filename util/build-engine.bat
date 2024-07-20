:: Configure engine
cmake --preset Engine-x64-Windows

:: Build engine
cmake --build --preset Engine-x64-Windows-Debug
cmake --build --preset Engine-x64-Windows-RelWithDebInfo
cmake --build --preset Engine-x64-Windows-Release
