file(GLOB NvTriStrip_SRC CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)

add_library(NvTriStrip STATIC ${NvTriStrip_SRC})

target_link_libraries(NvTriStrip PUBLIC xrCore)
target_include_directories(NvTriStrip PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(NvTriStrip PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../..")