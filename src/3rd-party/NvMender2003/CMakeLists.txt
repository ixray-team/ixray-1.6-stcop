file(GLOB NvMender2003_SRC CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)

add_library(NvMender2003 STATIC ${NvMender2003_SRC})

target_include_directories(NvMender2003 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(NvMender2003 PUBLIC xrCore)