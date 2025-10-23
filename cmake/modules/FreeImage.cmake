include(FetchContent)

if(WIN32)
    set(ENGINE_FREETYPE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeType.2.13.2/)
    add_imported_lib(
        FreeType::FreeType
        "${ENGINE_FREETYPE}native/include/"
        "${ENGINE_FREETYPE}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/freetype.lib"
        "${ENGINE_FREETYPE}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/freetype.dll"
    )
else()
    find_package(PkgConfig QUIET)
    if(PkgConfig_FOUND)
        pkg_check_modules(FREETYPE QUIET freetype2)
    endif()

    if(FREETYPE_FOUND)
        add_library(FreeType::FreeType INTERFACE IMPORTED)
        set_target_properties(FreeType::FreeType PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${FREETYPE_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${FREETYPE_LIBRARIES}"
        )
    else()
        set(CMAKE_POSITION_INDEPENDENT_CODE ON)
        FetchContent_Declare(
            freetype
            GIT_REPOSITORY https://gitlab.freedesktop.org/freetype/freetype.git
            GIT_TAG VER-2-13-2
        )
        FetchContent_MakeAvailable(freetype)

        if(TARGET freetype)
            add_library(FreeType::FreeType ALIAS freetype)
        elseif(NOT TARGET FreeType::FreeType)
            add_library(FreeType::FreeType INTERFACE IMPORTED)
            set_target_properties(FreeType::FreeType PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${freetype_SOURCE_DIR}/include"
                INTERFACE_LINK_LIBRARIES "freetype"
            )
        endif()
    endif()
endif()
