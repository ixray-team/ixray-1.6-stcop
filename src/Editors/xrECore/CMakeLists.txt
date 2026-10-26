project(xrECore)

# Files import
file(GLOB_RECURSE ECORE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.h"
	"*.cpp"
)

file(GLOB_RECURSE ECORE_SOURCE_ENGINE_FILES CONFIGURE_DEPENDS
	"Engine/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_EDITOR_FILES CONFIGURE_DEPENDS
	"Editor/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_EDITOR_PUBLIC_FILES CONFIGURE_DEPENDS
	"../Public/VIMP_Processor.*"
	"../Public/object_sliding.*"
)

file(GLOB_RECURSE ECORE_SOURCE_WM_FILES CONFIGURE_DEPENDS
	"WildMagic/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_ERR_FILES CONFIGURE_DEPENDS
	"dxerr/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_RES_FILES 
	"*.ico"
	"*.rc"
	"*.bmp"
)

file(GLOB_RECURSE ECORE_SOURCE_EXT_FILES CONFIGURE_DEPENDS
	"../../xrEngine/ObjectAnimator.cpp"
	"../../xrServerEntities/LevelGameDef.cpp"

	"../../Layers/xrRenderPC_R4/Blender_Blur.*"
	"../../Layers/xrRenderPC_R4/Blender_LaEmB.*"
	"../../Layers/xrRenderPC_R4/Blender_Screen_GRAY.*"
	"../../Layers/xrRenderPC_R4/Blender_Shadow_World.*"
)

set(ECORE_SRC_FILES)

source_group("Engine" FILES ${ECORE_SOURCE_ENGINE_FILES})
source_group("Editor" FILES ${ECORE_SOURCE_EDITOR_FILES})
source_group("MeshTools" FILES ${ECORE_SOURCE_EDITOR_PUBLIC_FILES})
source_group("WildMagic" FILES ${ECORE_SOURCE_WM_FILES})
source_group("Other"  FILES ${ECORE_SOURCE_EXT_FILES})
source_group("dxerr"  FILES ${ECORE_SOURCE_ERR_FILES})
source_group("Resources"  FILES ${ECORE_SOURCE_RES_FILES})

# Apply list
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_ALL_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_EXT_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_RES_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_EDITOR_PUBLIC_FILES})

# Remove unused files
list(FILTER ECORE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrECore project
add_library(xrECore SHARED ${ECORE_SRC_FILES})
target_include_directories(xrECore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrECore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrECore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/Public")
target_include_directories(xrECore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/../")
target_include_directories(xrECore PUBLIC "${IXRAY_SDK_INC}/lua/")
target_precompile_headers(xrECore PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrECore PRIVATE XRECORE_EXPORTS)
target_compile_definitions(xrECore PRIVATE _WINDOWS)
target_compile_definitions(xrECore PRIVATE _USRDLL)
target_compile_definitions(xrECore PUBLIC XR_EDITOR_NEW)
target_compile_definitions(xrECore PUBLIC _EDITOR)

target_compile_options(xrECore PRIVATE /W0)
target_compile_definitions(xrECore PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrECore PUBLIC xrEUI)
target_link_libraries(xrECore PUBLIC xrEProps)
target_link_libraries(xrECore PUBLIC xrRender_R4)
target_link_libraries(xrECore PUBLIC NvTriStrip)
target_link_libraries(xrECore PUBLIC luabind)
target_link_libraries(xrECore PUBLIC xrEngineCore)
target_link_libraries(xrECore PUBLIC xrPhysics)
target_link_libraries(xrECore PUBLIC xrDXT)
target_link_libraries(xrECore PUBLIC MagicFM)
target_link_libraries(xrECore PUBLIC NvMender2003)

target_link_libraries(xrECore PRIVATE Ogg::ogg)
target_link_libraries(xrECore PRIVATE Vorbis::vorbis)
target_link_libraries(xrECore PRIVATE Vorbis::vorbisfile)

if (WIN32)
	target_link_libraries(xrECore PUBLIC DirectX::Mesh)
	target_link_libraries(xrECore PUBLIC DirectX::Tex)
	target_link_libraries(xrECore PRIVATE WindowsSDK::D3D11)
	target_link_libraries(xrECore PRIVATE WindowsSDK::DXGI)
	target_link_libraries(xrECore PRIVATE WindowsSDK::DXGUID)
	target_link_libraries(xrECore PRIVATE WindowsSDK::D3DCompiler)
	target_link_libraries(xrECore PUBLIC NVIDIA::DLSS)
endif()

target_validate_pch(xrECore ${CMAKE_CURRENT_SOURCE_DIR})

## lunaSVG
target_link_libraries(${PROJECT_NAME} PUBLIC lunasvg)