# Создаем временный CMakeLists.txt для imgui-node-editor
file(WRITE ${CMAKE_BINARY_DIR}/imgui_node_editor_CMakeLists.txt
    "cmake_minimum_required(VERSION 3.14)\n"
    "project(imgui_node_editor)\n"
    "set(SOURCES imgui_node_editor.cpp imgui_canvas.cpp crude_json.cpp imgui_node_editor_api.cpp)\n"
    "add_library(imgui_node_editor STATIC \${SOURCES})\n"
    "target_include_directories(imgui_node_editor PUBLIC \${CMAKE_CURRENT_SOURCE_DIR})\n"
    "if(TARGET imgui)\n"
    "    target_link_libraries(imgui_node_editor PUBLIC imgui)\n"
    "endif()\n"
    "if(MSVC)\n"
    "    target_compile_options(imgui_node_editor PRIVATE /W0)\n"
    "else()\n"
    "    target_compile_options(imgui_node_editor PRIVATE -w)\n"
    "endif()\n"
)

FetchContent_Declare(
    imgui_node_editor
    GIT_REPOSITORY https://github.com/thedmd/imgui-node-editor.git
    GIT_TAG master
    GIT_SHALLOW TRUE
    PATCH_COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/imgui_node_editor_CMakeLists.txt ${CMAKE_CURRENT_BINARY_DIR}/_deps/imgui_node_editor-src/CMakeLists.txt
)

FetchContent_MakeAvailable(imgui_node_editor)