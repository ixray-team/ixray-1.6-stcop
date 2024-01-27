project(xrAI)
set(XAI_SRC_FILES)

# Files import
file(GLOB_RECURSE XAI_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XAI_SOURCE_ENTT_FILES
    "../../xrEngine/xrLoadSurface.cpp"
    "../../xrServerEntities/alife_human_brain.cpp"
    "../../xrServerEntities/alife_monster_brain.cpp"
    "../../xrServerEntities/alife_space.cpp"
    "../../xrServerEntities/PHNetState.cpp"
    "../../xrServerEntities/PHSynchronize.cpp"
    "../../xrServerEntities/smart_cast.cpp"
    "../../xrServerEntities/smart_cast_stats.cpp"
    "../../xrServerEntities/xrServer_Objects.cpp"
    "../../xrServerEntities/xrServer_Objects_Abstract.cpp"
    "../../xrServerEntities/xrServer_Objects_ALife.cpp"
    "../../xrServerEntities/xrServer_Objects_ALife_Items.cpp"
    "../../xrServerEntities/xrServer_Objects_ALife_Monsters.cpp"
    "../../xrServerEntities/xrServer_Object_Base.cpp"
)
file(GLOB_RECURSE XAI_SOURCE_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)

# Source groups
source_group("kernel" FILES ${XAI_SOURCE_ALL_FILES})
source_group("entt" FILES ${XAI_SOURCE_ENTT_FILES})
source_group("res" FILES ${XAI_SOURCE_RES_FILES})

# Apply list
list(APPEND XAI_SRC_FILES ${XAI_SOURCE_ALL_FILES})
list(APPEND XAI_SRC_FILES ${XAI_SOURCE_ENTT_FILES})
list(APPEND XAI_SRC_FILES ${XAI_SOURCE_RES_FILES})

# Remove unused files
#list(FILTER XAI_SRC_FILES EXCLUDE REGEX "lzo/*.ch")

# xrAI project
add_executable(xrAI WIN32 ${XAI_SRC_FILES})

target_include_directories(xrAI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrAI PRIVATE "../../xrServerEntities/")

target_link_directories(xrAI PRIVATE "${IXRAY_SDK_LIB}")

target_precompile_headers(xrAI PRIVATE "stdafx.h")

# project defines 
target_compile_definitions(xrAI PRIVATE AI_COMPILER)
target_compile_definitions(xrAI PRIVATE dSINGLE)
target_compile_definitions(xrAI PRIVATE _WINDOWS)

target_compile_definitions(xrAI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
target_compile_definitions(xrAI PRIVATE "$<$<CONFIG:Release>:LUABIND_NO_ERROR_CHECKING>")

add_compile_options(/fp:fast)

# Linker list
target_link_libraries(xrAI PRIVATE xrCDB)
target_link_libraries(xrAI PRIVATE luabind)
target_link_libraries(xrAI PRIVATE xrSE_Factory)

# Nuget
add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

## DirectX SDK
target_include_directories(xrAI PRIVATE "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(xrAI PRIVATE "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")
