project(xrCompress)
set(COMPR_SRC_FILES)

# Files import
file(GLOB_RECURSE COMPR_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.c"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${COMPR_SOURCE_ALL_FILES})

# Apply list
list(APPEND COMPR_SRC_FILES ${COMPR_SOURCE_ALL_FILES})

# xrCompress project
add_executable(xrCompress ${COMPR_SRC_FILES})
target_include_directories(xrCompress PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Move precompiled header
if (WIN32)
    set_target_properties(xrCompress PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
    set_target_properties(xrCompress PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
    set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
    target_precompile_headers(xrCompress PUBLIC "stdafx.h")

    file(GLOB_RECURSE COMPRESSOR_PCH_FILES "stdafx.*")
    file(GLOB_RECURSE COMPRESSOR_SOURCE_ALL_C_FILES "*.c")

    set_source_files_properties(${COMPRESSOR_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
    source_group("pch" FILES ${COMPRESSOR_PCH_FILES})
endif()

# Project defines
if (WIN32)
    target_compile_definitions(xrCompress PRIVATE _USRDLL)
endif()

# includes 
target_include_directories(xrCompress PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../3rd party/lzo/include")

# Linker list
target_link_libraries(xrCompress PUBLIC xrCore)
target_link_libraries(xrCompress PRIVATE lzo_static_lib)

if (WIN32)
    target_link_libraries(xrCompress PRIVATE "winmm.lib")
endif()

# Set relative runpath
if(NOT WIN32 AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set_target_properties(${PROJECT_NAME} PROPERTIES
        BUILD_RPATH ""
        BUILD_RPATH_USE_ORIGIN ON
    )
    set_target_properties(${PROJECT_NAME} PROPERTIES
        BUILD_RPATH "$ORIGIN"
    )
endif()
