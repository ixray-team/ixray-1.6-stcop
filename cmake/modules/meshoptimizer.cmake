include(FetchContent)

FetchContent_Declare(
    meshoptimizer
    GIT_REPOSITORY https://github.com/zeux/meshoptimizer.git
    GIT_TAG v0.22
    GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(meshoptimizer)

# The engine uses the release DLL CRT in every configuration.
set_target_properties(meshoptimizer PROPERTIES FOLDER "3rd Party" MSVC_RUNTIME_LIBRARY "MultiThreadedDLL")
