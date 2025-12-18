project(xrSE_Factory)
set(SEF_SRC_FILES)

# Files import

file(GLOB_RECURSE SEF_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE SEF_SOURCE_ENTT_FILES
    "../../xrServerEntities/*.cpp"
    "../../xrServerEntities/*.h"
    "../../xrEngine/IGame_ObjectFactory.*"
)
file(GLOB_RECURSE SEF_SOURCE_SCR_FILES
    "../../xrScripts/script_debugger*.*"
    "../../xrScripts/script_engine*.*"
    "../../xrScripts/script_storage.*"
    "../../xrScripts/script_callStack.*"
    "../../xrScripts/script_stack_tracker.*"
    "../../xrScripts/script_lua_helper.*"
)

# Source groups
source_group("kernel" FILES ${SEF_SOURCE_ALL_FILES})
source_group("entt" FILES ${SEF_SOURCE_ENTT_FILES})
source_group("script" FILES ${SEF_SOURCE_SCR_FILES})

# Apply list
list(APPEND SEF_SRC_FILES ${SEF_SOURCE_ALL_FILES})
list(APPEND SEF_SRC_FILES ${SEF_SOURCE_ENTT_FILES})
list(APPEND SEF_SRC_FILES ${SEF_SOURCE_SCR_FILES})

# Remove unused files
#list(FILTER SEF_SRC_FILES EXCLUDE REGEX "lzo/*.ch")

# xrSE_Factory project
add_library(xrSE_Factory SHARED ${SEF_SRC_FILES})
target_include_directories(xrSE_Factory PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrSE_Factory PRIVATE "../../xrServerEntities/")
target_precompile_headers(xrSE_Factory PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrSE_Factory PRIVATE XRSE_FACTORY_EXPORTS)
target_compile_definitions(xrSE_Factory PRIVATE dSINGLE)
target_compile_definitions(xrSE_Factory PRIVATE _USRDLL)
target_compile_definitions(xrSE_Factory PRIVATE "$<$<CONFIG:Release>:LUABIND_NO_ERROR_CHECKING>")

# Linker list
target_link_libraries(xrSE_Factory PUBLIC xrCore)
target_link_libraries(xrSE_Factory PUBLIC luabind)
target_link_libraries(xrSE_Factory PUBLIC xrScripts)
target_link_libraries(xrSE_Factory PUBLIC xrEProps)

target_link_libraries(xrSE_Factory PRIVATE "shlwapi.lib")
