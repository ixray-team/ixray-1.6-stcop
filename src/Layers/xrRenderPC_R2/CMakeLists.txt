project(xrRender_R2)
set(XRR2_SRC_FILES)

# Files import
file(GLOB_RECURSE XRR2_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XRR2_SOURCE_COMP_FILES CONFIGURE_DEPENDS
    "../xrRender/*.cpp"
    "../xrRender/*.h"
)
file(GLOB_RECURSE XRR2_SOURCE_INCL_FILES CONFIGURE_DEPENDS
    "../../Include/*.h"
)
file(GLOB_RECURSE XRR2_SOURCE_COMP_BLEND_FILES
    "../xrRender/Blender_*.cpp"
    "../xrRender/Blender_*.h"
)
file(GLOB_RECURSE XRR2_SOURCE_BLEND_FILES
    "Blender_*.cpp"
    "Blender_*.h"
)
file(GLOB_RECURSE XRR2_SOURCE_DX9_FILES
    "../xrRenderDX9/*.cpp"
    "../xrRenderDX9/*.h"
)
file(GLOB_RECURSE XRR2_SOURCE_LUA_FILES
    "../../xrEngine/ai_script_lua*.cpp"
    "../../xrEngine/ai_script_lua*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_PARTICLE_CORE CONFIGURE_DEPENDS
	"../xrRender/particle_core/*.cpp"
	"../xrRender/particle_core/*.h"
)

# Source groups
source_group("render/src" FILES ${XRR2_SOURCE_ALL_FILES})
source_group("render/include" FILES ${XRR2_SOURCE_INCL_FILES})
source_group("layers" FILES ${XRR2_SOURCE_COMP_FILES})
source_group("general_dx9" FILES ${XRR2_SOURCE_DX9_FILES})
source_group("lua_ext" FILES ${XRR2_SOURCE_LUA_FILES})
source_group("blend" FILES ${XRR2_SOURCE_COMP_BLEND_FILES})
source_group("blend" FILES ${XRR2_SOURCE_BLEND_FILES})
source_group("particle_core" FILES ${XRR4_SOURCE_PARTICLE_CORE})

# Apply list
list(APPEND XRR2_SRC_FILES ${XRR2_SOURCE_ALL_FILES})
list(APPEND XRR2_SRC_FILES ${XRR2_SOURCE_COMP_FILES})
list(APPEND XRR2_SRC_FILES ${XRR2_SOURCE_DX9_FILES})
list(APPEND XRR2_SRC_FILES ${XRR2_SOURCE_LUA_FILES})
list(APPEND XRR2_SRC_FILES ${XRR2_SOURCE_INCL_FILES})

# Remove unused files
#list(FILTER XRR2_SRC_FILES EXCLUDE REGEX "../xrRender/r__pixel_calculator.cpp")

# xrRender_R2 project
add_library(xrRender_R2 SHARED ${XRR2_SRC_FILES})
target_precompile_headers(xrRender_R2 PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_R2 PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRender_R2 PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrRender_R2 PRIVATE XRRENDER_R2_EXPORTS)
target_compile_definitions(xrRender_R2 PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrRender_R2 PRIVATE xrEngineCore)
target_link_libraries(xrRender_R2 PUBLIC lunasvg)

if (WIN32)
    target_link_libraries(xrRender_R2 PRIVATE DirectX::Tex)
    target_link_libraries(xrRender_R2 PRIVATE DirectX::Mesh)
endif()