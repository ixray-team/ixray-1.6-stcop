project(xrCDB)
# Files import

file(GLOB_RECURSE CDB_SOURCE_OPC_FILES
    "OPC_*.cpp"
	"OPC_*.h"
)

file(GLOB_RECURSE CDB_SOURCE_ALL_FILES
    "*.cpp"
	"*.h"
)

set(CDB_SRC_FILES)

source_group("cdb" FILES ${CDB_SOURCE_ALL_FILES})
source_group("opcode" FILES ${CDB_SOURCE_OPC_FILES})

# Apply list
list(APPEND CDB_SRC_FILES ${CDB_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER CDB_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrCDB project
add_library(xrCDB SHARED ${CDB_SRC_FILES})
target_include_directories(xrCDB PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrCDB PUBLIC "stdafx.h")	

# project defines 
target_compile_definitions(xrCDB PRIVATE XRCDB_EXPORTS)
target_compile_definitions(xrCDB PRIVATE _WINDOWS)
target_compile_definitions(xrCDB PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrCDB PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrCDB PUBLIC xrCore)
target_link_libraries(xrCDB PUBLIC xrAPI)