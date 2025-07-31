file(GLOB NvMender2003_SRC CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)

add_library(NvMender2003 STATIC ${NvMender2003_SRC})
target_compile_options(NvMender2003 PRIVATE "/wd4005")

target_include_directories(NvMender2003 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(NvMender2003 PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_link_libraries(NvMender2003 PUBLIC xrCore)