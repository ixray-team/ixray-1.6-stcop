# RedImage
if (WIN32)
    set(REDIMAGE ${CMAKE_BINARY_DIR}/packages/IXRay.RedImage.0.1.1/)
    set(REDIMAGE_INCLUDE ${REDIMAGE}/include)
    set(REDIMAGE_LIB ${REDIMAGE}/lib/RedImageTool.lib)
    set(REDIMAGE_BIN ${REDIMAGE}/bin/RedImageTool.dll)

    add_imported_lib(
        RedImage::RedImage
        "${REDIMAGE_INCLUDE}"
        "${REDIMAGE_LIB}"
        "${REDIMAGE_BIN}"
    )
else()
    if(APPLE AND CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64")
        # libsquish enables SSE2 by default, which cannot be compiled for arm64.
        set(BUILD_SQUISH_WITH_SSE2 OFF CACHE BOOL "Build squish with SSE2" FORCE)
    endif()

    FetchContent_Declare(
        redimage
        GIT_REPOSITORY https://github.com/RedPandaProjects/RedImage.git
        # Pinned: master previously contained x86-only ISPC targets and a missing
        # Vector.inl include, both fixed upstream in this merge commit.
        GIT_TAG 9999e29e0e1196a408220636d87a273414b887e0
    )
    FetchContent_MakeAvailable(redimage)

    if(NOT TARGET RedImage::RedImage)
        add_library(RedImage::RedImage INTERFACE IMPORTED)
        set_target_properties(RedImage::RedImage PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${redimage_SOURCE_DIR}/RedImage"
            INTERFACE_LINK_LIBRARIES RedImage
        )


        add_library(NVTT::nvtt INTERFACE IMPORTED)
        target_link_libraries(NVTT::nvtt INTERFACE RedImage)
        
        target_include_directories(NVTT::nvtt INTERFACE 
            "${redimage_SOURCE_DIR}/deps/nvtt/src"
        )
    endif()
endif()