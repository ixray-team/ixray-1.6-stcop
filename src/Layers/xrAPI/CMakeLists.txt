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
target_precompile_headers(xrAPI PUBLIC "stdafx.h")
target_compile_definitions(xrAPI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
target_compile_definitions(xrAPI PRIVATE XRAPI_EXPORTS)