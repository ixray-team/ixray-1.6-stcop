project(xrEngine)
set(ENGINE_SRC_FILES)

# Files import
file(GLOB_RECURSE ENGINE_SOURCE_INTERFACE_FILES
    "I*.cpp"
    "I*.h"
)
file(GLOB_RECURSE ENGINE_SOURCE_ALL_FILES
    "ai*.cpp"
    "ai*.h"
)
file(GLOB_RECURSE ENGINE_SOURCE_DEVICE_FILES
    "device*.*"
    "Device*.*"
)
file(GLOB_RECURSE ENGINE_SOURCE_ENV_FILES
    "Environment*.*"
)
file(GLOB_RECURSE ENGINE_SOURCE_WEDIT_FILES
    "editor*.*"
)
file(GLOB_RECURSE ENGINE_SOURCE_ALL_FILES
    "*.cpp"
    "*.c"
    "*.h"
)

# File groups
source_group("core" FILES ${ENGINE_SOURCE_ALL_FILES})
source_group("interfaces" FILES ${ENGINE_SOURCE_INTERFACE_FILES})
source_group("lua" FILES ${ENGINE_SOURCE_LUA_FILES})
source_group("device" FILES ${ENGINE_SOURCE_DEVICE_FILES})
source_group("weather_editor" FILES ${ENGINE_SOURCE_WEDIT_FILES})
source_group("environment" FILES ${ENGINE_SOURCE_ENV_FILES})

# Apply list
list(APPEND ENGINE_SRC_FILES ${ENGINE_SOURCE_ALL_FILES})

# Remove files
#list(FILTER ENGINE_SRC_FILES EXCLUDE REGEX "")

# xrEngine project
add_library(xrEngine SHARED ${ENGINE_SRC_FILES})
set_property(TARGET xrEngine PROPERTY ENABLE_EXPORTS ON)

# Include directories
target_include_directories(xrEngine PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrEngine PUBLIC "${IXRAY_SDK_INC}")

# Precompiled header
target_precompile_headers(xrEngine PUBLIC "stdafx.h")

# Project defines
target_compile_definitions(xrEngine PRIVATE ENGINE_BUILD)

add_compile_options(/fp:fast)
target_compile_definitions(xrEngine PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrEngine PUBLIC xrCore)
target_link_libraries(xrEngine PUBLIC xrCDB)
target_link_libraries(xrEngine PUBLIC xrAPI)
target_link_libraries(xrEngine PUBLIC xrSound)
target_link_libraries(xrEngine PUBLIC luabind)
target_link_libraries(xrEngine PUBLIC xrNetServer)
target_link_libraries(xrEngine PUBLIC "Winmm.lib")
target_link_libraries(xrEngine PUBLIC "Vfw32.lib")

target_link_libraries(xrEngine PUBLIC ${IXRAY_SDK_LIB}/FreeImage.lib)

# Nuget
set(ENGINE_THRA ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/)

add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${CMAKE_HOME_DIRECTORY}/dep/nuget/nuget restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${ENGINE_THRA}native/bin/${CMAKE_GENERATOR_PLATFORM}/Release/libtheora.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}FreeImage.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

## Theora
target_link_libraries(xrEngine PUBLIC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/native/lib/${CMAKE_GENERATOR_PLATFORM}/Release/libtheora.lib)
target_include_directories(xrEngine PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/native/include/")

## OGG
target_link_libraries(xrEngine PUBLIC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/lib/${CMAKE_GENERATOR_PLATFORM}/Release/libogg.lib)
target_include_directories(xrEngine PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/include/")
