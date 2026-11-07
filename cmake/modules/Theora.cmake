if(WIN32)
    set(ENGINE_THRA ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/)
    
    add_imported_lib(
        Theora::theora
        "${ENGINE_THRA}/native/include"
        "${ENGINE_THRA}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.lib"
        "${ENGINE_THRA}/native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.dll"
    )
else()
    message(STATUS "Setting up Theora build...")
    
    set(THEORA_BUILD_DIR ${CMAKE_BINARY_DIR}/theora_build)
    set(THEORA_INSTALL_DIR ${THEORA_BUILD_DIR}/install)
    
    if(NOT EXISTS ${THEORA_INSTALL_DIR}/lib/libtheora.a OR NOT EXISTS ${THEORA_INSTALL_DIR}/include/theora/theora.h)
        message(STATUS "Theora not found, will build from source...")
        
        file(MAKE_DIRECTORY ${THEORA_INSTALL_DIR}/include)
        file(MAKE_DIRECTORY ${THEORA_INSTALL_DIR}/lib)

        set(THEORA_ARCHIVE ${THEORA_BUILD_DIR}/libtheora-1.1.1.tar.gz)
        if(NOT EXISTS ${THEORA_ARCHIVE})
            file(MAKE_DIRECTORY ${THEORA_BUILD_DIR})
            file(DOWNLOAD
                https://downloads.xiph.org/releases/theora/libtheora-1.1.1.tar.gz
                ${THEORA_ARCHIVE}
                TLS_VERIFY ON
                STATUS THEORA_DOWNLOAD_STATUS
            )
            list(GET THEORA_DOWNLOAD_STATUS 0 THEORA_DOWNLOAD_RESULT)
            if(NOT THEORA_DOWNLOAD_RESULT EQUAL 0)
                message(FATAL_ERROR "Failed to download Theora: ${THEORA_DOWNLOAD_STATUS}")
            endif()
        endif()

        add_custom_command(
            OUTPUT 
                ${THEORA_INSTALL_DIR}/lib/libtheora.a
                ${THEORA_INSTALL_DIR}/include/theora/theora.h
            COMMAND ${CMAKE_COMMAND} -E make_directory ${THEORA_BUILD_DIR}
            COMMAND ${CMAKE_COMMAND} -E chdir ${THEORA_BUILD_DIR} ${CMAKE_COMMAND} -E tar xzf ${THEORA_ARCHIVE}
            COMMAND cd ${THEORA_BUILD_DIR}/libtheora-1.1.1 &&
                CC=${CMAKE_C_COMPILER}
                CXX=${CMAKE_CXX_COMPILER}
                CFLAGS=${CMAKE_C_FLAGS}
                ./configure --prefix=${THEORA_INSTALL_DIR} --disable-shared --enable-static --with-pic --disable-examples &&
                make -j${CMAKE_BUILD_PARALLEL_LEVEL} &&
                make install
            COMMENT "Building Theora from source"
            VERBATIM
        )
        
        add_custom_target(theora_build_target DEPENDS ${THEORA_INSTALL_DIR}/lib/libtheora.a)
    else()
        message(STATUS "Theora already built, skipping...")
        add_custom_target(theora_build_target ALL)
    endif()
    
    add_library(Theora::theora INTERFACE IMPORTED)
    add_dependencies(Theora::theora theora_build_target)
    
    target_include_directories(Theora::theora INTERFACE $<BUILD_INTERFACE:${THEORA_INSTALL_DIR}/include> $<INSTALL_INTERFACE:include>)
    target_link_libraries(Theora::theora INTERFACE $<BUILD_INTERFACE:${THEORA_INSTALL_DIR}/lib/libtheora.a> $<INSTALL_INTERFACE:-ltheora> Ogg::ogg)
    
    message(STATUS "Theora will be built during compilation")
endif()