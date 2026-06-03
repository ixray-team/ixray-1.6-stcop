project(TestBenchmark)
set(XR_TEST_BENCHSRC_FILES)

# Files import
file(GLOB_RECURSE XR_TEST_BENCHSOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)

# Apply list
list(APPEND XR_TEST_BENCHSRC_FILES ${XR_TEST_BENCHSOURCE_ALL_FILES})

# TestBenchmark project
add_executable(TestBenchmark ${XR_TEST_BENCHSOURCE_ALL_FILES})
target_include_directories(TestBenchmark PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(TestBenchmark PRIVATE "${IXRAY_SDK_INC}")
target_include_directories(TestBenchmark PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../")
set_target_properties(TestBenchmark PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Project defines
target_compile_definitions(TestBenchmark PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(TestBenchmark PUBLIC xrEngineCore)