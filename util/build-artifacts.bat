:: Set location
cd ..

:: Build engine
cmake --preset Engine-x86
cmake --build --preset Engine-x86-Debug
cmake --build --preset Engine-x86-RelWithDebInfo
cmake --build --preset Engine-x86-Release
cmake --preset Engine-x64
cmake --build --preset Engine-x64-Debug
cmake --build --preset Engine-x64-RelWithDebInfo
cmake --build --preset Engine-x64-Release

:: Build server
cmake --preset Server-x86
cmake --build --preset Server-x86-Debug
cmake --build --preset Server-x86-RelWithDebInfo
cmake --build --preset Server-x86-Release
cmake --preset Server-x64
cmake --build --preset Server-x64-Debug
cmake --build --preset Server-x64-RelWithDebInfo
cmake --build --preset Server-x64-Release

:: Build utilities
cmake --preset Utilities-x86
cmake --build --preset Utilities-x86-Debug
cmake --build --preset Utilities-x86-RelWithDebInfo
cmake --build --preset Utilities-x86-Release
cmake --preset Utilities-x64
cmake --build --preset Utilities-x64-Debug
cmake --build --preset Utilities-x64-RelWithDebInfo
cmake --build --preset Utilities-x64-Release

:: Build compressor
cmake --preset Compressor-x64
cmake --build --preset Compressor-x64-Release
