project(glew)

file(GLOB GLEW_SRC CONFIGURE_DEPENDS
	"glew/src/glew.c"
	"glew/include/GL/glew.h"
	"glew/include/GL/wglew.h"
)

add_library(glew STATIC ${GLEW_SRC})
target_compile_options(glew PRIVATE "/wd4005")
target_include_directories(glew PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})