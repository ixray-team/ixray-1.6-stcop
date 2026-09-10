include(FetchContent)

FetchContent_Declare(
    Tracy
    GIT_REPOSITORY https://github.com/wolfpld/tracy.git
    GIT_TAG v0.14.1
)

set(TRACY_STATIC OFF CACHE BOOL "" FORCE)
set(TRACY_NO_VSYNC_CAPTURE 1 CACHE BOOL "" FORCE)
set(TRACY_ON_DEMAND 1 CACHE BOOL "" FORCE)
set(TRACY_CALLSTACK 1 CACHE BOOL "" FORCE)
set(TRACY_FIBERS 1 CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(Tracy)