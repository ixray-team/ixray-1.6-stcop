project(xrAbstractions)
set(ENGINE_SRC_FILES)

# Files import
file(GLOB_RECURSE ENGINE_SOURCE_INTERFACE_FILES "I*.*")
file(GLOB_RECURSE ENGINE_SOURCE_ALL_FILES "ai*.*")

file(GLOB_RECURSE ENGINE_SOURCE_DEVICE_FILES
    "device*.*"
    "Device*.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_ENV_FILES
    "Environment*.*"
    "Rain.*"
    "thunderbolt.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_FEEL_FILES
    "Feel_*.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_RENDER_FILES
    "Bone.*"
    "fmesh.*"
    "Render.*"
    "SkeletonMotions*.*"
    "motion.*"
)
file(GLOB_RECURSE ENGINE_SOURCE_KERNEL_FILES
    "x_ray.*"
    "Engine.*"
    "EngineAPI.*"
    "EngineFactory.*"
    "EngineExternal*.*"
    "Application*.*"
    "defines.*"
    "pure*.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_INPUT_FILES
    "xr_input.*"
    "xr_input_xinput.*"
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

file(GLOB_RECURSE ENGINE_SOURCE_AI_FILES
    "AI/.*"
)

# File groups
source_group("src" FILES ${ENGINE_SOURCE_ALL_FILES})
source_group("core/fonts" FILES ${ENGINE_SOURCE_FONTS_FILES})
source_group("core/device" FILES ${ENGINE_SOURCE_DEVICE_FILES})
source_group("core/input" FILES ${ENGINE_SOURCE_INPUT_FILES})
source_group("core/console" FILES ${ENGINE_SOURCE_CONSOLE_FILES})
source_group("core/feel" FILES ${ENGINE_SOURCE_FEEL_FILES})
source_group("kernel" FILES ${ENGINE_SOURCE_KERNEL_FILES})
source_group("interfaces" FILES ${ENGINE_SOURCE_INTERFACE_FILES})
source_group("subsystem/lua" FILES ${ENGINE_SOURCE_LUA_FILES})
source_group("subsystem/render" FILES ${ENGINE_SOURCE_RENDER_FILES})
source_group("environment" FILES ${ENGINE_SOURCE_ENV_FILES})
source_group("ai" FILES ${ENGINE_SOURCE_AI_FILES})

# Apply list
list(APPEND ENGINE_SRC_FILES ${ENGINE_SOURCE_ALL_FILES})

# Remove files
list(FILTER ENGINE_SRC_FILES EXCLUDE REGEX "xrLoadSurface.cpp")

# xrAbstractions project
add_library(xrAbstractions SHARED ${ENGINE_SRC_FILES})
set_property(TARGET xrAbstractions PROPERTY ENABLE_EXPORTS ON)

# Include directories
target_include_directories(xrAbstractions PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/AI")
target_include_directories(xrAbstractions PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrAbstractions PUBLIC "${IXRAY_SDK_INC}")

# Move precompiled header
target_validate_pch(xrAbstractions ${CMAKE_CURRENT_SOURCE_DIR})

# Project defines
target_compile_definitions(xrAbstractions PRIVATE ENGINE_BUILD)

# Linker list
target_link_libraries(xrAbstractions PUBLIC xrCore)
target_link_libraries(xrAbstractions PUBLIC xrCDB)
target_link_libraries(xrAbstractions PUBLIC xrSound)
target_link_libraries(xrAbstractions PUBLIC luabind)
target_link_libraries(xrAbstractions PUBLIC xrNetServer)
target_link_libraries(xrAbstractions PUBLIC imgui)
target_link_libraries(xrAbstractions PUBLIC xrRenderInterface)

target_link_libraries(xrAbstractions PRIVATE "Winmm.lib")
target_link_libraries(xrAbstractions PRIVATE "Vfw32.lib")

# Copy binaries
add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${ENGINE_THRA}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${ENGINE_FREETYPE}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/freetype.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

## FreeType
target_link_libraries(xrAbstractions PRIVATE ${ENGINE_FREETYPE}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/freetype.lib)
target_include_directories(xrAbstractions PRIVATE "${ENGINE_FREETYPE}native/include/")

## Theora
target_link_libraries(xrAbstractions PUBLIC ${ENGINE_THRA}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.lib)
target_include_directories(xrAbstractions PUBLIC "${ENGINE_THRA}/native/include/")

## OGG
target_link_libraries(xrAbstractions PUBLIC ${SND_OGG}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
target_include_directories(xrAbstractions PUBLIC "${SND_OGG}/native/include/")

target_include_directories(xrAbstractions PUBLIC "${RENDERDOC_API}")
