# LZO
if (WIN32)
    set(LZO ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Lzo.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2.10.0)
    set(LZO_INCLUDE_DIR ${LZO}/build/native/include)
    set(LZO_LIB_PATH ${LZO}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/lzo2.lib)
    set(LZO_BIN_PATH ${LZO}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/lzo2.dll)

    add_imported_lib(LZO::lzo2 "${LZO_INCLUDE_DIR}" "${LZO_LIB_PATH}" "${LZO_BIN_PATH}")
else()
    # Try system LZO first
    find_path(LZO_INCLUDE_DIR
        NAMES lzo/lzo1x.h
        PATHS /usr/include /usr/local/include
    )
    find_library(LZO_LIBRARY
        NAMES lzo2
        PATHS /usr/lib /usr/lib64 /usr/local/lib
    )

    if (LZO_INCLUDE_DIR AND LZO_LIBRARY)
        add_library(LZO::lzo2 INTERFACE IMPORTED)
        set_target_properties(LZO::lzo2 PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${LZO_INCLUDE_DIR}"
            INTERFACE_LINK_LIBRARIES "${LZO_LIBRARY}"
        )
    else()
        message(WARNING "System lzo2 not found — falling back to damageboy/lzo2 via FetchContent")
        FetchContent_Declare(
            lzo2_fallback
            GIT_REPOSITORY https://github.com/damageboy/lzo2.git
            GIT_TAG master
        )
        FetchContent_MakeAvailable(lzo2_fallback)

        add_library(LZO::lzo2 INTERFACE IMPORTED)
        set_target_properties(LZO::lzo2 PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${lzo2_fallback_SOURCE_DIR}/include"
            INTERFACE_LINK_LIBRARIES lzo2
        )
    endif()
endif()