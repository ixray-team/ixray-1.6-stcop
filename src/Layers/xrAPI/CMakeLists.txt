project(xrAPI)

file(GLOB_RECURSE API_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

set(API_SRC_FILES)

source_group("api" FILES ${API_SOURCE_ALL_FILES})

# Apply list
list(APPEND API_SRC_FILES ${API_SOURCE_ALL_FILES})

add_library(xrAPI SHARED ${API_SRC_FILES})

target_include_directories(xrAPI PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(xrAPI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
target_compile_definitions(xrAPI PRIVATE XRAPI_EXPORTS)

# Move precompiled header
set_target_properties(xrAPI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrAPI PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrAPI PRIVATE "stdafx.h")

file(GLOB_RECURSE API_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${API_SOURCE_PCH_FILES})
