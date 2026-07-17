if(WIN32)
    set(STEAM_SOCKETS_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.GameNetworkingSockets.Vcpkg.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.1.4.1)

    set(STEAM_SOCKETS_INCLUDE "${STEAM_SOCKETS_PATH}/native/include/GameNetworkingSockets")
    set(STEAM_SOCKETS_LIB "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/GameNetworkingSockets.lib")
    set(STEAM_SOCKETS_DLLS
        "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/GameNetworkingSockets.dll"
        "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/legacy.dll"
        "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libcrypto-3-x64.dll"
        "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libssl-3-x64.dll"
        "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libprotobuf-lite.dll"
        "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libprotobuf.dll"
        "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libprotoc.dll"
    )

    add_imported_lib(Steam::Sockets "${STEAM_SOCKETS_INCLUDE}" "${STEAM_SOCKETS_LIB}" "${STEAM_SOCKETS_DLLS}")
else()
        message(STATUS "Fetching GameNetworkingSockets from system...")
    find_package(GameNetworkingSockets QUIET)

    if(GameNetworkingSockets_FOUND)
        add_library(Steam::Sockets INTERFACE IMPORTED)
        set_target_properties(Steam::Sockets PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${GameNetworkingSockets_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${GameNetworkingSockets_LIBRARIES}"
        )
    else()
        message(STATUS "Fetching GameNetworkingSockets from source...")
        set(CMAKE_POSITION_INDEPENDENT_CODE ON)
        FetchContent_Declare(
            GameNetworkingSockets
            GIT_REPOSITORY https://github.com/ValveSoftware/GameNetworkingSockets.git
            GIT_BRANCH master
        )
        
        add_compile_definitions(STEAMNETWORKINGSOCKETS_SNP_PARANOIA=0)
        FetchContent_MakeAvailable(GameNetworkingSockets)

        if(NOT TARGET Steam::Sockets)
            add_library(Steam::Sockets INTERFACE IMPORTED)
            set_target_properties(Steam::Sockets PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${gamenetworkingsockets_SOURCE_DIR}/include"
                INTERFACE_LINK_LIBRARIES "GameNetworkingSockets"
            )
        endif()
    endif()
endif()
