file(GLOB DXERR_SRC CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)

add_library(dxerr STATIC ${DXERR_SRC})
target_compile_options(dxerr PRIVATE "/wd4005")
target_include_directories(dxerr PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})