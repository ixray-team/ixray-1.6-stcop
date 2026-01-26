project(imgui)
set(IMGUI_SRC_FILES)

file(GLOB_RECURSE IMGUI_SRC CONFIGURE_DEPENDS "*.cpp" "*.h")
#file(GLOB IMGUI_PLUGINS_SRC CONFIGURE_DEPENDS "plugins\*.cpp" "plugins\*.h")

list(APPEND IMGUI_SRC_FILES ${IMGUI_SRC})
#list(APPEND IMGUI_SRC_FILES ${IMGUI_PLUGINS_SRC})

add_library(imgui SHARED ${IMGUI_SRC_FILES})
add_library(ImGui::imgui ALIAS imgui)

if (WIN32)
	target_compile_definitions(imgui PUBLIC IMGUI_DLL)
	target_compile_definitions(imgui PRIVATE IMGUI_EXPORTS)
endif()

target_include_directories(imgui PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(imgui PUBLIC xrCore)