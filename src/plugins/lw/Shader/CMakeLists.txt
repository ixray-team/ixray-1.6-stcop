project(xrLW_Shader)

# Files import
file(GLOB_RECURSE LW_SHARED_SOURCE_ALL_FILES
    "*.*"
)
file(GLOB_RECURSE LW_SHADER_FILES
    "../../Shared/ELog.cpp"
    "../../Shared/BlenderListLoader.cpp"
    "../../Shared/GameMtlLib.cpp"
)

set(LW_SHARED_SRC_FILES)

source_group("Shared" FILES ${LW_SHADER_FILES})
source_group("Src" FILES ${LW_SHARED_SOURCE_ALL_FILES})

# Apply list
list(APPEND LW_SHARED_SRC_FILES ${LW_SHARED_SOURCE_ALL_FILES})
list(APPEND LW_SHARED_SRC_FILES ${LW_SHADER_FILES})

# Remove unused files
#list(FILTER LW_SHARED_SRC_FILES EXCLUDE REGEX "ExportKeys.cpp$")

# xrLW_Shader project
add_library(xrLW_Shader SHARED ${LW_SHARED_SRC_FILES})
target_include_directories(xrLW_Shader PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrLW_Shader PUBLIC "../../Shader/")
target_include_directories(xrLW_Shader PUBLIC "${IXR_LW_SDK}/include")

set_target_properties(
    xrLW_Shader
    PROPERTIES
        OUTPUT_NAME "ixray_shader"
        SUFFIX ".p"
)

# Project defines
target_compile_definitions(xrLW_Shader PRIVATE _USRDLL)
target_compile_definitions(xrLW_Shader PRIVATE _EXPORT)
target_compile_definitions(xrLW_Shader PRIVATE _LW_SHADER)
target_compile_definitions(xrLW_Shader PRIVATE _LW_PLUGIN)

add_compile_options(/fp:fast)
target_compile_definitions(xrLW_Shader PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrLW_Shader PUBLIC xrCore)
target_link_libraries(xrLW_Shader PUBLIC lwo)
target_link_libraries(xrLW_Shader PUBLIC xrLW_Server)

# Move precompiled header
set_target_properties(xrLW_Shader PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE LW_SHARED_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${LW_SHARED_SOURCE_PCH_FILES})
