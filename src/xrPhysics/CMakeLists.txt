project(xrPhysics)
set(XPH_SRC_FILES)

# Files import
file(GLOB_RECURSE XPH_SOURCE_ALL_FILES
    "*.cpp"
	"*.h"
)

file(GLOB_RECURSE XPH_SOURCE_COMP_FILES
    "PH*.cpp"
	"PH*.h"
)

file(GLOB XPH_SOURCE_NET_FILES
    "../xrServerEntities/PH*.cpp"
	"../xrServerEntities/PH*.h"
)

file(GLOB_RECURSE XPH_SOURCE_CORE_FILES
    "Physics*.cpp"
	"Physics*.h"
)

# Source groups
source_group("kernel" FILES ${XPH_SOURCE_ALL_FILES})
source_group("components" FILES ${XPH_SOURCE_COMP_FILES})
source_group("core" FILES ${XPH_SOURCE_CORE_FILES})
source_group("net" FILES ${XPH_SOURCE_NET_FILES})

# Apply list
list(APPEND XPH_SRC_FILES ${XPH_SOURCE_ALL_FILES})
list(APPEND XPH_SRC_FILES ${XPH_SOURCE_NET_FILES})

# Remove unused files
list(FILTER XPH_SRC_FILES EXCLUDE REGEX "tri-colliderknoopc/dcTriListCollider.*")

# xrPhysics project
add_library(xrPhysics SHARED ${XPH_SRC_FILES})
target_precompile_headers(xrPhysics PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrPhysics PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrPhysics PUBLIC "${IXRAY_SDK_INC}")

# project defines 
target_compile_definitions(xrPhysics PRIVATE XRPHYSICS_EXPORTS)
target_compile_definitions(xrPhysics PRIVATE _WINDOWS)
target_compile_definitions(xrPhysics PRIVATE dSINGLE)
target_compile_definitions(xrPhysics PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrPhysics PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrPhysics PUBLIC xrCore)
target_link_libraries(xrPhysics PUBLIC xrCDB)
target_link_libraries(xrPhysics PUBLIC ODE)