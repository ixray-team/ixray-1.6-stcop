include(FetchContent)

if(WIN32)
    if(NOT CMAKE_VS_PLATFORM_NAME)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            set(ARCH_DIR "x64")
        else()
            set(ARCH_DIR "Win32")
        endif()
    else()
        set(ARCH_DIR ${CMAKE_VS_PLATFORM_NAME})
    endif()

    set(SND_OGG ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/)
    add_imported_lib(
        Ogg::ogg
        "${SND_OGG}/native/include"
        "${SND_OGG}/native/lib/${ARCH_DIR}/Release/libogg.lib"
        "${SND_OGG}/native/bin/${ARCH_DIR}/Release/libogg.dll"
    )
else()
   pkg_check_modules(OGG QUIET ogg)
    if(OGG_FOUND)
        add_library(Ogg::ogg INTERFACE IMPORTED)
        set_target_properties(Ogg::ogg PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${OGG_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${OGG_LIBRARIES}"
        )
    else()
        # FetchContent fallback
        set(CMAKE_POSITION_INDEPENDENT_CODE ON)
        FetchContent_Declare(
            libogg
            GIT_REPOSITORY https://github.com/xiph/ogg.git
            GIT_TAG v1.3.5
        )
        FetchContent_MakeAvailable(libogg)

        if(NOT TARGET Ogg::ogg)
            add_library(Ogg::ogg INTERFACE IMPORTED)
            set_target_properties(Ogg::ogg PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${libogg_SOURCE_DIR}/include"
                INTERFACE_LINK_LIBRARIES "ogg"
            )
        endif()
    endif()
endif()