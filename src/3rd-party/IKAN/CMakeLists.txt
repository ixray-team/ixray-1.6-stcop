project(IKAN)
set(IKAN_SRC_FILES)

file(GLOB_RECURSE IKAN_SRC CONFIGURE_DEPENDS "*.cxx" "*.cpp" "*.h")
list(APPEND IKAN_SRC_FILES ${IKAN_SRC})

add_library(IKAN STATIC ${IKAN_SRC_FILES})

target_include_directories(IKAN PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_definitions(IKAN PRIVATE IKAN_INTERNAL)