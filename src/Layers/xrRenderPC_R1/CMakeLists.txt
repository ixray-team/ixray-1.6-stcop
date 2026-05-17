project(xrRender_R1)
set(XRR1_SRC_FILES)

# Files import
file(GLOB_RECURSE XRR1_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XRR1_SOURCE_COMP_FILES CONFIGURE_DEPENDS
    "../xrRender/*.cpp"
    "../xrRender/*.h"
)
file(GLOB_RECURSE XRR1_SOURCE_INCL_FILES CONFIGURE_DEPENDS
    "../../Include/*.h"
)
file(GLOB_RECURSE XRR1_SOURCE_COMP_BLEND_FILES
    "../xrRender/Blender_*.cpp"
    "../xrRender/Blender_*.h"
)
file(GLOB_RECURSE XRR1_SOURCE_BLEND_FILES
    "Blender_*.cpp"
    "Blender_*.h"
)
file(GLOB_RECURSE XRR1_SOURCE_DX9_FILES CONFIGURE_DEPENDS
    "../xrRenderDX9/*.cpp"
    "../xrRenderDX9/*.h"
)
file(GLOB_RECURSE XRR1_SOURCE_LUA_FILES
    "../../xrEngine/ai_script_lua*.cpp"
    "../../xrEngine/ai_script_lua*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_PARTICLE_CORE CONFIGURE_DEPENDS
	"../xrRender/particle_core/*.cpp"
	"../xrRender/particle_core/*.h"
)

# Source groups
source_group("render/src" FILES ${XRR1_SOURCE_ALL_FILES})
source_group("render/include" FILES ${XRR1_SOURCE_INCL_FILES})
source_group("layers" FILES ${XRR1_SOURCE_COMP_FILES})
source_group("general_dx9" FILES ${XRR1_SOURCE_DX9_FILES})
source_group("lua_ext" FILES ${XRR1_SOURCE_LUA_FILES})
source_group("blend" FILES ${XRR1_SOURCE_COMP_BLEND_FILES})
source_group("blend" FILES ${XRR1_SOURCE_BLEND_FILES})
source_group("particle_core" FILES ${XRR4_SOURCE_PARTICLE_CORE})

# Apply list
list(APPEND XRR1_SRC_FILES ${XRR1_SOURCE_ALL_FILES})
list(APPEND XRR1_SRC_FILES ${XRR1_SOURCE_COMP_FILES})
list(APPEND XRR1_SRC_FILES ${XRR1_SOURCE_DX9_FILES})
list(APPEND XRR1_SRC_FILES ${XRR1_SOURCE_LUA_FILES})
list(APPEND XRR1_SRC_FILES ${XRR1_SOURCE_INCL_FILES})

# Remove unused files
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/RenderPhaseAccumulator.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/RenderPhaseAccumPointGeom.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/RenderPhaseAccumOmnipartGeom.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/RenderPhaseAccumSpotGeom.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/r__pixel_calculator.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/R_sun.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/Light_Render_Direct_ComputeXFS.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/R_sun_support.cpp")

list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/light_smapvis.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/light_smapvis.h")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/light_vis.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/lights_render.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/r__blenders.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/R_calculate.cpp")
list(FILTER XRR1_SRC_FILES EXCLUDE REGEX "../xrRender/r__types.h")
# xrRender_R1 project
add_library(xrRender_R1 SHARED ${XRR1_SRC_FILES})
target_precompile_headers(xrRender_R1 PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_R1 PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrRender_R1 PRIVATE XRRENDER_R1_EXPORTS)
target_compile_definitions(xrRender_R1 PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrRender_R1 PRIVATE xrEngineCore)
target_link_libraries(xrRender_R1 PUBLIC lunasvg)
target_link_libraries(xrRender_R1 PUBLIC NvTriStrip)
target_link_libraries(xrRender_R1 PUBLIC DirectX::Mesh)

if (WIN32)
    target_link_libraries(xrRender_R1 PUBLIC DirectX::Tex)
endif()

if (LINUX)
    target_compile_definitions(xrRender_R1 PRIVATE XR_USE_DXVK_NATIVE=1 DXVK_WSI_DRIVER_SDL3=1)
endif()