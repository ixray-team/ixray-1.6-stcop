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
    FetchContent_Declare(
        redimage
        GIT_REPOSITORY https://github.com/RedPandaProjects/RedImage.git
        GIT_TAG master
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