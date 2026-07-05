project(xrEngineCore)
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

# xrEngineCore project
add_library(xrEngineCore SHARED ${ENGINE_SRC_FILES})
set_property(TARGET xrEngineCore PROPERTY ENABLE_EXPORTS ON)

# Include directories
target_include_directories(xrEngineCore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/AI")
target_include_directories(xrEngineCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrEngineCore PUBLIC "${IXRAY_SDK_INC}")
target_include_directories(xrEngineCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../xrCore/")

if(WIN32)
    target_include_directories(xrEngineCore PUBLIC ${CORE_DXMATH}/include/)
endif()

# Move precompiled header
target_validate_pch(xrEngineCore ${CMAKE_CURRENT_SOURCE_DIR})

# Project defines
target_compile_definitions(xrEngineCore PRIVATE ENGINE_BUILD)

# Linker list
target_link_libraries(xrEngineCore PUBLIC xrCore)
target_link_libraries(xrEngineCore PUBLIC xrSound)
target_link_libraries(xrEngineCore PUBLIC luabind)
target_link_libraries(xrEngineCore PUBLIC xrNetServer)
target_link_libraries(xrEngineCore PUBLIC imgui)
target_link_libraries(xrEngineCore PUBLIC xrRHI)
target_link_libraries(xrEngineCore PUBLIC FreeType::FreeType)
target_link_libraries(xrEngineCore PUBLIC Ogg::ogg)
target_link_libraries(xrEngineCore PUBLIC Theora::theora)

if (WIN32)
    target_link_libraries(xrEngineCore PRIVATE WindowsSDK::WinMM)
    target_link_libraries(xrEngineCore PRIVATE WindowsSDK::VFW32)
    target_link_libraries(xrEngineCore PRIVATE HIDAPI::HIDAPI)
endif()