project(CxImage)

set(CXI_SRC_FILES)
file(GLOB_RECURSE CXI_SRC
    "CxImage/*.cpp"
    "CxImage/*.c"
    "CxImage/*.h"
)
file(GLOB_RECURSE JPG_SRC
    "jpeg/*.cpp"
    "jpeg/*.c"
    "jpeg/*.h"
)

list(APPEND CXI_SRC_FILES ${CXI_SRC})
list(APPEND CXI_SRC_FILES ${JPG_SRC})

add_library(CxImage STATIC ${CXI_SRC_FILES})

target_include_directories(CxImage PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(CxImage PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
target_include_directories(CxImage PRIVATE "${IXRAY_SDK_INC}")