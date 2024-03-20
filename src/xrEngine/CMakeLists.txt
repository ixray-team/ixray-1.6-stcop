project(xrAbstractions)
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
    "Rain.*"
    "thunderbolt.*"
)
file(GLOB_RECURSE ENGINE_SOURCE_WEDIT_FILES
    "editor*.*"
    "engine_impl*.*"
)
file(GLOB_RECURSE ENGINE_SOURCE_KERNEL_FILES
    "x_ray.*"
    "Engine.*"
    "EngineAPI.*"
    "EngineFactory.*"
    "EngineExternal*.*"
    "defines.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_INPUT_FILES
    "xr_input.*"
    "xr_level_controller.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_CONSOLE_FILES
    "xr_ioc_cmd.*"
    "XR_IOConsole*.*"
    "line_editor.*"
    "line_edit_control.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_FONTS_FILES
    "FontManager.*"
    "GameFont.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.c"
    "*.h"
)

# File groups
source_group("src" FILES ${ENGINE_SOURCE_ALL_FILES})
source_group("core/fonts" FILES ${ENGINE_SOURCE_FONTS_FILES})
source_group("core/device" FILES ${ENGINE_SOURCE_DEVICE_FILES})
source_group("core/input" FILES ${ENGINE_SOURCE_INPUT_FILES})
source_group("core/console" FILES ${ENGINE_SOURCE_CONSOLE_FILES})
source_group("kernel" FILES ${ENGINE_SOURCE_KERNEL_FILES})
source_group("interfaces" FILES ${ENGINE_SOURCE_INTERFACE_FILES})
source_group("lua" FILES ${ENGINE_SOURCE_LUA_FILES})
source_group("environment" FILES ${ENGINE_SOURCE_ENV_FILES})
source_group("environment/weather_editor_old" FILES ${ENGINE_SOURCE_WEDIT_FILES})

# Apply list
list(APPEND ENGINE_SRC_FILES ${ENGINE_SOURCE_ALL_FILES})

# Remove files
list(FILTER ENGINE_SRC_FILES EXCLUDE REGEX "xrLoadSurface.cpp")

# xrAbstractions project
add_library(xrAbstractions SHARED ${ENGINE_SRC_FILES})
set_property(TARGET xrAbstractions PROPERTY ENABLE_EXPORTS ON)

# Include directories
target_include_directories(xrAbstractions PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrAbstractions PUBLIC "${IXRAY_SDK_INC}")

# Move precompiled header
set_target_properties(xrAbstractions PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrAbstractions PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrAbstractions PUBLIC "stdafx.h")

file(GLOB_RECURSE ENGINE_SOURCE_PCH_FILES "stdafx.*")
file(GLOB_RECURSE ENGINE_SOURCE_ALL_C_FILES "*.c")

set_source_files_properties(${ENGINE_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
source_group("pch" FILES ${ENGINE_SOURCE_PCH_FILES})

# Project defines
target_compile_definitions(xrAbstractions PRIVATE ENGINE_BUILD)

add_compile_options(/fp:fast)
target_compile_definitions(xrAbstractions PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrAbstractions PUBLIC xrCore)
target_link_libraries(xrAbstractions PUBLIC xrCDB)
target_link_libraries(xrAbstractions PUBLIC xrAPI)
target_link_libraries(xrAbstractions PUBLIC xrSound)
target_link_libraries(xrAbstractions PUBLIC luabind)
target_link_libraries(xrAbstractions PUBLIC xrNetServer)
target_link_libraries(xrAbstractions PUBLIC "Winmm.lib")
target_link_libraries(xrAbstractions PUBLIC "Vfw32.lib")

# Nuget
set(ENGINE_THRA ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/)
set(ENGINE_FREETYPE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeType.2.13.2/)

add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${ENGINE_THRA}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${ENGINE_FREETYPE}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/freetype.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

## FreeType
target_link_libraries(xrAbstractions PRIVATE ${ENGINE_FREETYPE}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/freetype.lib)
target_include_directories(xrAbstractions PRIVATE "${ENGINE_FREETYPE}native/include/")

## Theora
target_link_libraries(xrAbstractions PUBLIC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.lib)
target_include_directories(xrAbstractions PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/native/include/")

## OGG
target_link_libraries(xrAbstractions PUBLIC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
target_include_directories(xrAbstractions PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/include/")

target_include_directories(xrAbstractions PUBLIC "${RENDERDOC_API}")