include(FetchContent)

FetchContent_Declare(
    Tracy
    GIT_REPOSITORY https://github.com/wolfpld/tracy.git
    GIT_TAG v0.13.1
)

set(TRACY_STATIC OFF CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(Tracy)