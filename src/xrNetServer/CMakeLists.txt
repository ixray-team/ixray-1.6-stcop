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
target_include_directories(xrNetServer PUBLIC "${STEAM_SOCKETS_PATH}/native/include/")

# Project defines
target_compile_definitions(xrNetServer PRIVATE XR_NETSERVER_EXPORTS)
target_compile_definitions(xrNetServer PRIVATE _USRDLL)

if (IXRAY_MP)
	target_compile_definitions(xrNetServer PUBLIC XR_MP_BUILD)
endif()

# Linker list
target_link_libraries(xrNetServer PUBLIC xrCore)

target_link_libraries(xrNetServer PRIVATE "Ws2_32.lib")
target_link_libraries(xrNetServer PRIVATE "dxguid.lib")

# Move precompiled header
target_validate_pch(xrNetServer ${CMAKE_CURRENT_SOURCE_DIR})

# Steam Sockets
if(IXRAY_MP)
    target_link_libraries(xrNetServer PUBLIC "${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/GameNetworkingSockets.lib")

    if("${NUGET_PACKAGE_PLATFORM}" MATCHES "(x64)")
        set(LIBSSL_POSTFIX "-x64")
    endif()

    add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/GameNetworkingSockets.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/legacy.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/libcrypto-3${LIBSSL_POSTFIX}.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/libprotobuf-lite.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/libprotobuf.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/libprotoc.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${STEAM_SOCKETS_PATH}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/libssl-3${LIBSSL_POSTFIX}.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
endif()
