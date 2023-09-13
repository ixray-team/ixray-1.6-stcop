file(GLOB IMGUI_SRC CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)

add_library(imgui STATIC ${IMGUI_SRC})
add_library(ImGui::imgui ALIAS imgui)
target_include_directories(imgui PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})