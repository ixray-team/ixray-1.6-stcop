:: Configure utilities
cmake --preset Utilities-x64-Windows

:: Build utilities
cmake --build --preset Utilities-x64-Windows-Debug
cmake --build --preset Utilities-x64-Windows-RelWithDebInfo
cmake --build --preset Utilities-x64-Windows-Release
