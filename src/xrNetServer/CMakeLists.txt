project(xrNetServer)
set(NETSERV_SRC_FILES)

# Files import
file(GLOB_RECURSE NETSERV_SOURCE_ALL_FILES CONFIGURE_DEPENDS "*.cpp" "*.h")

# Source groups
source_group("kernel" FILES ${NETSERV_SOURCE_ALL_FILES})

# Apply list
list(APPEND NETSERV_SRC_FILES ${NETSERV_SOURCE_ALL_FILES})

# Remove unused files
if (NOT IXRAY_MP)
	list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "SteamNetClient.cpp$")
	list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "SteamNetServer.cpp$")
	list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "SQLConnect.*")
elseif (NOT IXRAY_MP_SQL)
	list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "SQLConnect.*")
endif()
if (WIN32)
    list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "PosixSocksHelper.cpp")
else()
    list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX "WinsocksHelper.cpp")
endif()

# xrNetServer project
add_library(xrNetServer SHARED ${NETSERV_SRC_FILES})
target_include_directories(xrNetServer PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrNetServer PRIVATE XR_NETSERVER_EXPORTS)
target_compile_definitions(xrNetServer PRIVATE _USRDLL)

if (IXRAY_MP)
    if (IXRAY_MP_SQL)
	    target_compile_definitions(xrNetServer PUBLIC IXR_MP_SQL)
    endif()
	target_compile_definitions(xrNetServer PUBLIC XR_MP_BUILD)
endif()

# Linker list
target_link_libraries(xrNetServer PUBLIC xrCore)

if (WIN32)
    target_link_libraries(xrNetServer PRIVATE "Ws2_32.lib")
    target_link_libraries(xrNetServer PRIVATE "dxguid.lib")
endif()

# Move precompiled header
target_validate_pch(xrNetServer ${CMAKE_CURRENT_SOURCE_DIR})

# Steam Sockets
if(IXRAY_MP)
    target_link_libraries(xrNetServer PUBLIC Steam::Sockets)
endif()

# MySQL Connector
if (IXRAY_MP_SQL)
    target_include_directories(xrNetServer PUBLIC "${MYSQLCONNECTOR}include/")
    target_link_libraries(xrNetServer PUBLIC "${MYSQLCONNECTOR}lib/mysqlcppconn.lib")
    add_custom_command(TARGET ${PROJECT_NAME} 
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MYSQLCONNECTOR}bin/mysqlcppconn-9-vs14.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MYSQLCONNECTOR}bin/libcrypto-1_1-x64.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MYSQLCONNECTOR}bin/libssl-1_1-x64.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
endif()