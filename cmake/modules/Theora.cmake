if(WIN32)
    set(ENGINE_THRA ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/)
    
    add_imported_lib(
        Theora::theora
        "${ENGINE_THRA}/native/include"
        "${ENGINE_THRA}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.lib"
        "${ENGINE_THRA}/native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.dll"
    )
else()
    pkg_check_modules(THEORA QUIET theora)
    if(THEORA_FOUND)
        add_library(Theora::theora INTERFACE IMPORTED)
        set_target_properties(Theora::theora PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${THEORA_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${THEORA_LIBRARIES}"
        )
    else()
        FetchContent_Declare(
            libtheora
            GIT_REPOSITORY https://github.com/xiph/theora.git
            GIT_TAG v1.1.1
        )
        FetchContent_MakeAvailable(libtheora)
        
        if(NOT TARGET Theora::theora)
            add_library(Theora::theora INTERFACE IMPORTED)
            
            # Theora требует Ogg
            find_package(Ogg REQUIRED)
            
            set_target_properties(Theora::theora PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES 
                    "${libtheora_SOURCE_DIR}/include"
                    "${libtheora_BINARY_DIR}/include"
                INTERFACE_LINK_LIBRARIES 
                    "theora"
                    Ogg::ogg
            )
        endif()
    endif()
endif()