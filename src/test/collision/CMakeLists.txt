add_executable(TestCollisionStreaming CollisionStreaming.cpp)
target_link_libraries(TestCollisionStreaming PRIVATE xrCore)
set_target_properties(TestCollisionStreaming PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
target_compile_definitions(TestCollisionStreaming PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
