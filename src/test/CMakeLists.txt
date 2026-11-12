set(IXRAY_FOLDER_TESTS FOLDER "Test")

add_subdirectory("benchmark")
set_target_properties(TestBenchmark PROPERTIES ${IXRAY_FOLDER_TESTS})

add_subdirectory("collision")
set_target_properties(TestCollisionStreaming PROPERTIES ${IXRAY_FOLDER_TESTS})
