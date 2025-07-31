project(SQL_Gen)
set(SQL_Gen_SRC_FILES)

#set(USE_NVTT ON)

# Files import
file(GLOB_RECURSE SQL_Gen_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)

file(GLOB_RECURSE SQL_Gen_SOURCE_IMGUI_FILES CONFIGURE_DEPENDS
    "../xrForms/imgui_impl_sdlrenderer3.*"
    "../xrForms/imgui_impl_sdl3.*"
    "../../Editors/xrEUI/spectrum.*"
    "ImGuiSpinner.*"
)

# Source groups
source_group("kernel" FILES ${SQL_Gen_SOURCE_ALL_FILES})
source_group("imgui" FILES ${SQL_Gen_SOURCE_IMGUI_FILES})

# Apply list
list(APPEND SQL_Gen_SRC_FILES ${SQL_Gen_SOURCE_ALL_FILES})
list(APPEND SQL_Gen_SRC_FILES ${SQL_Gen_SOURCE_IMGUI_FILES})

# SQL_Gen project
add_executable(SQL_Gen WIN32 ${SQL_Gen_SRC_FILES})
target_include_directories(SQL_Gen PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(SQL_Gen PUBLIC "${IXRAY_SDK_LIB}/")

# Project defines
target_compile_definitions(SQL_Gen PRIVATE SQL_Gen_EXPORTS)
target_compile_definitions(SQL_Gen PRIVATE _USRDLL)

# Linker list
target_link_libraries(SQL_Gen PUBLIC xrCore)
target_link_libraries(SQL_Gen PUBLIC imgui)

# MySQL Connector
target_include_directories(SQL_Gen PUBLIC "${MYSQLCONNECTOR}include/")
target_link_libraries(SQL_Gen PUBLIC "${MYSQLCONNECTOR}lib/mysqlcppconn.lib")
add_custom_command(TARGET ${PROJECT_NAME} 
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MYSQLCONNECTOR}bin/mysqlcppconn-9-vs14.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MYSQLCONNECTOR}bin/libcrypto-1_1-x64.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MYSQLCONNECTOR}bin/libssl-1_1-x64.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)