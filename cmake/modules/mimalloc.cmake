# On macOS the engine ships several dylibs: a static mimalloc inside xrCore frees memory
# that other modules allocated through the system allocator, which crashes in mi_free.
if(APPLE)
    set(IXRAY_MIMALLOC_AVAILABLE FALSE)
    message(STATUS "Engine heap: CRT (mimalloc is Windows-only here)")
    return()
endif()
set(IXRAY_MIMALLOC_AVAILABLE TRUE)

if(NOT IXRAY_USE_MIMALLOC OR IXRAY_ASAN)
    if(IXRAY_USE_MIMALLOC AND IXRAY_ASAN)
        message(STATUS "Engine heap: CRT (mimalloc disabled for Address Sanitizer)")
    else()
        message(STATUS "Engine heap: CRT")
    endif()
    return()
endif()

include(FetchContent)

set(MI_OVERRIDE OFF CACHE BOOL "" FORCE)
set(MI_BUILD_SHARED OFF CACHE BOOL "" FORCE)
set(MI_BUILD_STATIC ON CACHE BOOL "" FORCE)
set(MI_BUILD_OBJECT OFF CACHE BOOL "" FORCE)
set(MI_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(MI_WIN_REDIRECT OFF CACHE BOOL "" FORCE)
set(MI_OSX_INTERPOSE OFF CACHE BOOL "" FORCE)
set(MI_OSX_ZONE OFF CACHE BOOL "" FORCE)
set(MI_OPT_ARCH OFF CACHE STRING "" FORCE)
set(MI_STATS ON CACHE STRING "" FORCE)

FetchContent_Declare(mimalloc
    URL https://github.com/microsoft/mimalloc/archive/refs/tags/v3.5.3.tar.gz
    URL_HASH SHA256=3b4a15153a59905995f7070296ed604bb5ccc00cabb8b93446931aff77224d47
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)
FetchContent_MakeAvailable(mimalloc)
set_target_properties(mimalloc-static PROPERTIES FOLDER "3rd Party")
message(STATUS "Engine heap: mimalloc 3.5.3 (static, no CRT override)")
