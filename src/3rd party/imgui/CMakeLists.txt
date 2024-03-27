project(imgui)

file(GLOB IMGUI_SRC CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)

add_library(imgui SHARED ${IMGUI_SRC})
add_library(ImGui::imgui ALIAS imgui)

if (WIN32)
	target_compile_definitions(imgui PUBLIC IMGUI_DLL)
	target_compile_definitions(imgui PRIVATE IMGUI_EXPORTS)
endif()
target_include_directories(imgui PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(imgui PUBLIC xrCore)