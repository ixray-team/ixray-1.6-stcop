:: Configure editors
cmake --preset Editors-x64-Windows

:: Build editors
cmake --build --preset Editors-x64-Windows-Debug
cmake --build --preset Editors-x64-Windows-RelWithDebInfo
