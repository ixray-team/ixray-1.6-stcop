set(IXRAY_FOLDER_TESTS FOLDER "Test")

add_subdirectory("benchmark")
set_target_properties(TestBenchmark PROPERTIES ${IXRAY_FOLDER_TESTS})

if(TARGET xrUI)
    add_executable(TestUIShaderCache macos/UIShaderCache.cpp)
    target_link_libraries(TestUIShaderCache PRIVATE xrEngineCore)
    set_target_properties(TestUIShaderCache PROPERTIES ${IXRAY_FOLDER_TESTS})
endif()
