:: Configure editors
cmake -S . -B build/x64/Editors-Windows -G "Visual Studio 17 2022" -A x64 -DIXRAY_EDITORS=ON || exit /b 1

:: Build editors
cmake --build build/x64/Editors-Windows --config Debug --target LevelEditor || exit /b 1
cmake --build build/x64/Editors-Windows --config RelWithDebInfo --target LevelEditor || exit /b 1
