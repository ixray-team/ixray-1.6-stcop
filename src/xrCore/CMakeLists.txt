project(xrCore)
# Files import

file(GLOB CORE_SOURCE_STL_FILES CONFIGURE_DEPENDS
    "*_std*.*"
    "*_stl*.*"
    "Fixed*.*"
)

file(GLOB CORE_SOURCE_MATH_FILES CONFIGURE_DEPENDS
    "_*.cpp"
    "_*.h"
)

file(GLOB CORE_SOURCE_LZO_FILES CONFIGURE_DEPENDS
    "lzo_*.*"
    "rt_lzo*.*"
)

file(GLOB CORE_SOURCE_RT_FILES CONFIGURE_DEPENDS
    "rt_c*.*"
)

file(GLOB CORE_SOURCE_PPMD_FILES CONFIGURE_DEPENDS
    "ppmd_*.*"
)

file(GLOB CORE_SOURCE_LZ_FILES CONFIGURE_DEPENDS
    "LzHuf*.cpp"
    "lzhuf.h"
)

file(GLOB CORE_SOURCE_XML_FILES CONFIGURE_DEPENDS
    "XMLCore/*.cpp"
    "XMLCore/.h"
)

file(GLOB CORE_SOURCE_MEMORY_FILES CONFIGURE_DEPENDS
    "xrMemory*.*"
    "memory_*.*"
    "*malloc*.*"
    "ptmalloc3/*.*"
)

file(GLOB CORE_SOURCE_FS_FILES CONFIGURE_DEPENDS
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

set(CORE_SRC_FILES)

source_group("core" FILES ${CORE_SOURCE_ALL_FILES})
source_group("math" FILES ${CORE_SOURCE_MATH_FILES})
source_group("memory" FILES ${CORE_SOURCE_MEMORY_FILES})
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

target_precompile_headers(xrCore PUBLIC "stdafx.h")	

# project defines 
target_compile_definitions(xrCore PRIVATE XRCORE_EXPORTS)
target_compile_definitions(xrCore PRIVATE _WINDOWS)
target_compile_definitions(xrCore PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrCore PRIVATE "dbghelp.lib")
target_link_options(xrCore PUBLIC "$<$<CONFIG:DEBUG>:/SAFESEH:NO>")

add_compile_options(/fp:fast)
target_compile_definitions(xrCore PRIVATE "$<$<CONFIG:Debug>:DEBUG>")