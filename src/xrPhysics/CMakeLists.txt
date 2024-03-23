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
    "xrPhysics.*"
)
file(GLOB_RECURSE XPH_SOURCE_DTRI_FILES
    "dTri*.*"
    "dCylinder*.*"
    "dxTriList*.*"
    "dcTriangle*.*"
    "TriPrimitiveCollideClassDef*.*"
    "dSortTriPrimitive*.*"
)

# Source groups
source_group("kernel" FILES ${XPH_SOURCE_ALL_FILES})
source_group("components" FILES ${XPH_SOURCE_COMP_FILES})
source_group("core" FILES ${XPH_SOURCE_CORE_FILES})
source_group("net" FILES ${XPH_SOURCE_NET_FILES})
source_group("dTri" FILES ${XPH_SOURCE_DTRI_FILES})

# Apply list
list(APPEND XPH_SRC_FILES ${XPH_SOURCE_ALL_FILES})
list(APPEND XPH_SRC_FILES ${XPH_SOURCE_NET_FILES})

# Remove unused files
list(FILTER XPH_SRC_FILES EXCLUDE REGEX "tri-colliderknoopc/dcTriListCollider.*")

# xrPhysics project
add_library(xrPhysics SHARED ${XPH_SRC_FILES})

# Include directories
target_include_directories(xrPhysics PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrPhysics PUBLIC "${IXRAY_SDK_INC}")

# Project defines
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
target_link_libraries(xrPhysics PUBLIC xrAbstractions)

# Move precompiled header
set_target_properties(xrPhysics PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrPhysics PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrPhysics PRIVATE "stdafx.h")

file(GLOB_RECURSE XPH_SOURCE_PCH_FILES "stdafx.*")
file(GLOB_RECURSE XPH_SOURCE_ALL_C_FILES "*.c")

set_source_files_properties(${XPH_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
source_group("pch" FILES ${XPH_SOURCE_PCH_FILES})
