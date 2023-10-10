project(xrCore)
set(CORE_SRC_FILES)

# Files import
file(GLOB CORE_SOURCE_STL_FILES
    "*_std*.*"
    "*_stl*.*"
    "buffer_vector*.*"
    "intrusive_ptr*.*"
    "Fixed*.*"
)
file(GLOB CORE_SOURCE_MATH_FILES
    "vector.h"
    "_*.cpp"
    "_*.h"
)
file(GLOB CORE_SOURCE_LZO_FILES
    "lzo_*.*"
    "rt_lzo*.*"
)
file(GLOB CORE_SOURCE_RT_FILES
    "rt_c*.*"
)
file(GLOB CORE_SOURCE_PPMD_FILES
    "ppmd_*.*"
)
file(GLOB CORE_SOURCE_LZ_FILES
    "LzHuf*.cpp"
    "lzhuf.h"
)
file(GLOB CORE_SOURCE_XML_FILES
    "XmlParser/*.cpp"
    "XmlParser/.h"
)
file(GLOB CORE_SOURCE_MEMORY_FILES
    "xrMemory*.*"
    "memory_*.*"
    "*malloc*.*"
    "doug_lea_allocator.*"
)
file(GLOB_RECURSE CORE_SOURCE_MEMORY_P3_FILES
    "ptmalloc3/*.*"
)
file(GLOB CORE_SOURCE_DEBUG_FILES
    "xrDebug*.*"
    "log.*"
)
file(GLOB CORE_SOURCE_FS_FILES
    "FS*.*"
    "File*.*"
    "file*.*"
    "Locator*.*"
)
file(GLOB_RECURSE CORE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.c"
    "*.h"
)

source_group("core" FILES ${CORE_SOURCE_ALL_FILES})
source_group("math" FILES ${CORE_SOURCE_MATH_FILES})
source_group("memory" FILES ${CORE_SOURCE_MEMORY_FILES})
source_group("memory/p3malloc" FILES ${CORE_SOURCE_MEMORY_P3_FILES})
source_group("debug" FILES ${CORE_SOURCE_DEBUG_FILES})
source_group("compressors/rt" FILES ${CORE_SOURCE_RT_FILES})
source_group("compressors/lz" FILES ${CORE_SOURCE_LZ_FILES})
source_group("compressors/lzo" FILES ${CORE_SOURCE_LZO_FILES})
source_group("compressors/ppmd" FILES ${CORE_SOURCE_PPMD_FILES})
source_group("fs" FILES ${CORE_SOURCE_FS_FILES})
source_group("stl" FILES ${CORE_SOURCE_STL_FILES})
source_group("XML" FILES ${CORE_SOURCE_XML_FILES})

# Apply list
list(APPEND CORE_SRC_FILES ${CORE_SOURCE_ALL_FILES})

# Remove borlands files
list(FILTER CORE_SRC_FILES EXCLUDE REGEX  "ELocatorAPI.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "LocatorAPI_Notifications.cpp$")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "xrMemory_subst_borland.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "FileSystem_borland.cpp")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "blackbox/*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/t-test*.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/ptmalloc3.c")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/tst-independent-alloc.c")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "rt_lzo*.cpp")

# xrCore project
add_library(xrCore SHARED ${CORE_SRC_FILES})
target_include_directories(xrCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrCore PUBLIC "${IXRAY_SDK_INC}")

if(IXRAY_ASAN)
    add_compile_definitions(PURE_ONLY)
endif()

# Move precompiled header
set_target_properties(xrCore PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrCore PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrCore PUBLIC "stdafx.h")

file(GLOB_RECURSE CORE_SOURCE_PCH_FILES "stdafx.*")
file(GLOB_RECURSE CORE_SOURCE_ALL_C_FILES "*.c")

set_source_files_properties(${CORE_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
source_group("pch" FILES ${CORE_SOURCE_PCH_FILES})

# Project defines
target_compile_definitions(xrCore PRIVATE XRCORE_EXPORTS)
target_compile_definitions(xrCore PRIVATE _WINDOWS)
target_compile_definitions(xrCore PRIVATE _USRDLL)

## Disable user-cheats for Shipping build
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Release>:MASTER_GOLD>")

# Linker list
target_link_libraries(xrCore PRIVATE "dbghelp.lib")
target_link_options(xrCore PUBLIC "$<$<CONFIG:DEBUG>:/SAFESEH:NO>")

add_compile_options(/fp:fast)
target_compile_definitions(xrCore PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
