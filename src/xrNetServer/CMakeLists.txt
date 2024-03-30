project(xrNetServer)
set(NETSERV_SRC_FILES)

# Files import
file(GLOB_RECURSE NETSERV_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)

# Source groups
source_group("kernel" FILES ${NETSERV_SOURCE_ALL_FILES})

# Apply list
list(APPEND NETSERV_SRC_FILES ${NETSERV_SOURCE_ALL_FILES})

# Remove unused files
if (NOT IXRAY_MP)
	list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "SteamNetClient.cpp$")
	list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "SteamNetServer.cpp$")
endif()

# xrNetServer project
add_library(xrNetServer SHARED ${NETSERV_SRC_FILES})
target_include_directories(xrNetServer PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrNetServer PRIVATE XR_NETSERVER_EXPORTS)
target_compile_definitions(xrNetServer PRIVATE _USRDLL)

if (IXRAY_MP)
	target_compile_definitions(xrNetServer PUBLIC XR_MP_BUILD)
endif()

target_compile_definitions(xrNetServer PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrNetServer PUBLIC xrCore)
target_link_libraries(xrNetServer PUBLIC "Ws2_32.lib")

# Move precompiled header
set_target_properties(xrNetServer PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrNetServer PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrNetServer PRIVATE "stdafx.h")

file(GLOB_RECURSE NETSERV_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${NETSERV_SOURCE_PCH_FILES})

# NuGet
add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

if(WIN32)
    if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
        set(STEAM_SOCKETS_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.GameNetworkingSockets.Vcpkg.Runtimes.win-x86.1.4.1)
    else()
        set(STEAM_SOCKETS_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.GameNetworkingSockets.Vcpkg.Runtimes.win-x64.1.4.1)
    endif()
endif()

target_include_directories(xrNetServer PUBLIC "${STEAM_SOCKETS_PATH}/native/include/")

if(IXRAY_MP)
    if(MSVC AND CMAKE_SIZEOF_VOID_P EQUAL 4)
        target_link_libraries(xrNetServer PUBLIC "${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/GameNetworkingSockets.lib")

        add_custom_command(TARGET ${PROJECT_NAME}
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/GameNetworkingSockets.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/legacy.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/libcrypto-3.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/libprotobuf-lite.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/libprotobuf.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/libprotoc.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x86/native/Release/libssl-3.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        )
    else()
        target_link_libraries(xrNetServer PUBLIC "${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/GameNetworkingSockets.lib")

        add_custom_command(TARGET ${PROJECT_NAME}
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/GameNetworkingSockets.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/legacy.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libcrypto-3-x64.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libprotobuf-lite.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libprotobuf.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libprotoc.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-x64/native/Release/libssl-3-x64.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        )
    endif()
endif()
