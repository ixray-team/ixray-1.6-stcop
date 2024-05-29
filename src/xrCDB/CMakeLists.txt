project(xrCDB)

# Files import
file(GLOB_RECURSE CDB_SOURCE_OPC_FILES
    "override/*.cpp"
    "override/*.h"
)
file(GLOB_RECURSE CDB_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

set(CDB_SRC_FILES)

source_group("cdb" FILES ${CDB_SOURCE_ALL_FILES})
source_group("opcode override" FILES ${CDB_SOURCE_OPC_FILES})

# Apply list
list(APPEND CDB_SRC_FILES ${CDB_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER CDB_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrCDB project
add_library(xrCDB SHARED ${CDB_SRC_FILES})
target_include_directories(xrCDB PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrCDB PRIVATE XRCDB_EXPORTS)
target_compile_definitions(xrCDB PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrCDB PUBLIC xrCore)
target_link_libraries(xrCDB PUBLIC opcode-prosectors)

# Move precompiled header
target_validate_pch(xrCDB ${CMAKE_CURRENT_SOURCE_DIR})