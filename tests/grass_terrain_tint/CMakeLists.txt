cmake_minimum_required(VERSION 3.16)
project(GrassTerrainTintChecks LANGUAGES CXX)
enable_testing()

add_executable(grass_tint_math_tests test_math.cpp)
target_compile_features(grass_tint_math_tests PRIVATE cxx_std_17)
if(MSVC)
    target_compile_options(grass_tint_math_tests PRIVATE /W4 /WX)
else()
    target_compile_options(grass_tint_math_tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
add_test(NAME grass_tint_math COMMAND grass_tint_math_tests)

if(WIN32)
    add_executable(grass_tint_gpu_tests test_gpu_d3d11.cpp)
    target_compile_features(grass_tint_gpu_tests PRIVATE cxx_std_17)
    target_compile_definitions(grass_tint_gpu_tests PRIVATE NOMINMAX WIN32_LEAN_AND_MEAN)
    target_link_libraries(grass_tint_gpu_tests PRIVATE d3d11 d3dcompiler)
    add_test(NAME grass_tint_gpu COMMAND grass_tint_gpu_tests
        "${CMAKE_CURRENT_SOURCE_DIR}/../../gamedata/shaders/d3d11")
endif()

find_program(POWERSHELL_EXECUTABLE NAMES pwsh powershell)
if(POWERSHELL_EXECUTABLE)
    add_test(NAME grass_tint_runtime_setup COMMAND
        "${POWERSHELL_EXECUTABLE}" -NoProfile -ExecutionPolicy Bypass -File
        "${CMAKE_CURRENT_SOURCE_DIR}/test_prepare_runtime.ps1")
endif()

find_package(Python3 COMPONENTS Interpreter QUIET)
find_program(GLSLANG_EXECUTABLE NAMES glslangValidator glslang)
if(Python3_FOUND AND GLSLANG_EXECUTABLE)
    add_test(NAME grass_tint_shader_variants COMMAND
        "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/validate_shaders.py"
        --compiler "${GLSLANG_EXECUTABLE}" --output "${CMAKE_CURRENT_BINARY_DIR}/shaders")
endif()
