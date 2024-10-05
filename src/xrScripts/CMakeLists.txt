project(xrScripts)
set(XRPS_SRC_FILES)

# Files import
file(GLOB_RECURSE XRPS_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.c"
    "*.h"
)

file(GLOB_RECURSE XRPS_PLUG_PLUG CONFIGURE_DEPENDS "lua_ext.*")
file(GLOB_RECURSE XRPS_SOURCE_EXP_FILES CONFIGURE_DEPENDS "exports/*.*")
file(GLOB_RECURSE XRPS_PLUG_SOCKET CONFIGURE_DEPENDS "luasocket/*.*")
file(GLOB_RECURSE XRPS_PLUG_PANDA CONFIGURE_DEPENDS "luapanda.*")
file(GLOB_RECURSE XRPS_PLUG_LFS CONFIGURE_DEPENDS "lfs.*")
file(GLOB_RECURSE XRPS_PLUG_MARSHAL CONFIGURE_DEPENDS "lmarshal.*")
file(GLOB_RECURSE XRPS_PLUG_UTF8 CONFIGURE_DEPENDS "luautf8/*.*")
file(GLOB_RECURSE XRPS_PLUG_XML CONFIGURE_DEPENDS "luaxml.*")
file(GLOB_RECURSE XRPS_CALLBACK CONFIGURE_DEPENDS "script_callback_ex.*" "callback_info.*")

# Source groups
source_group("core" FILES ${XRPS_SOURCE_ALL_FILES})
source_group("exports" FILES ${XRPS_SOURCE_EXP_FILES})
source_group("callback" FILES ${XRPS_CALLBACK})
source_group("plugins" FILES ${XRPS_PLUG_PLUG})
source_group("plugins/socket" FILES ${XRPS_PLUG_SOCKET})
source_group("plugins/panda" FILES ${XRPS_PLUG_PANDA})
source_group("plugins/marshal" FILES ${XRPS_PLUG_LFS})
source_group("plugins/lfs" FILES ${XRPS_PLUG_MARSHAL})
source_group("plugins/xml" FILES ${XRPS_PLUG_XML})
source_group("plugins/utf8" FILES ${XRPS_PLUG_UTF8})

# Apply list
list(APPEND XRPS_SRC_FILES ${XRPS_SOURCE_ALL_FILES})

# Remove unused files
if (WIN32)
    list(FILTER XRPS_SRC_FILES EXCLUDE REGEX "luasocket/unix*.*")
    list(FILTER XRPS_SRC_FILES EXCLUDE REGEX "luasocket/usocket*.*")
else()
    list(FILTER XRPS_SRC_FILES EXCLUDE REGEX "luasocket/wsocket*.*")
endif()

# xrScripts project
add_library(xrScripts SHARED ${XRPS_SRC_FILES})
set_target_properties(xrScripts PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Include directories
target_include_directories(xrScripts PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrScripts PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrScripts PRIVATE XR_SCRIPTS_EXPORTS)
target_compile_definitions(xrScripts PRIVATE XRGAME_EXPORTS)
target_compile_definitions(xrScripts PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrScripts PUBLIC xrCore)
target_link_libraries(xrScripts PUBLIC xrAbstractions)
target_link_libraries(xrScripts PRIVATE "ws2_32.lib")
