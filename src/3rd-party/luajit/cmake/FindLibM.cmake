if(NOT LINUX)
    return()
endif()

include(FindPackageHandleStandardArgs)

if(LUAJIT_TARGET_ARCH STREQUAL "x86")
    set(CMAKE_FIND_LIBRARY_CUSTOM_LIB_SUFFIX
        "i386-linux-gnu;i686-linux-gnu;32"
    )
    set(LibM_SEARCH_PATHS
        /usr/lib/i386-linux-gnu
        /usr/lib32
        /lib/i386-linux-gnu
        /usr/lib/i686-linux-gnu
        /usr/lib
    )
elseif(LUAJIT_TARGET_ARCH STREQUAL "x64")
    set(LibM_SEARCH_PATHS
        /usr/lib/x86_64-linux-gnu
        /usr/lib64
        /usr/lib
    )
elseif(LUAJIT_TARGET_ARCH STREQUAL "arm64")
    set(LibM_SEARCH_PATHS
        /usr/lib/aarch64-linux-gnu
        /usr/lib
    )
else()
    set(LibM_SEARCH_PATHS
        /usr/lib
        /lib
    )
endif()

find_library(LibM_LIBRARIES
    NAMES
        m
    PATHS
        ${LibM_SEARCH_PATHS}
    NO_DEFAULT_PATH
)

find_package_handle_standard_args(LibM
    REQUIRED_VARS
        LibM_LIBRARIES
)

if(LibM_FOUND)
    mark_as_advanced(LibM_LIBRARIES)
endif()

if(LibM_FOUND AND NOT LibM::Library)
    add_library(LibM::Library UNKNOWN IMPORTED)
    set_target_properties(LibM::Library
        PROPERTIES
            IMPORTED_LOCATION ${LibM_LIBRARIES}
    )
else()
    add_library(LibM::Library INTERFACE)
    target_compile_options(LibM::Library INTERFACE -lm)
endif()
