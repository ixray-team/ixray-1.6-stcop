include(FetchContent)

FetchContent_Declare(
    Tracy
    GIT_REPOSITORY https://github.com/wolfpld/tracy.git
    GIT_TAG v0.13.1
)

set(TRACY_STATIC OFF CACHE BOOL "" FORCE)
set(TRACY_NO_VSYNC_CAPTURE ON CACHE BOOL "" FORCE)
set(TRACY_ON_DEMAND ON CACHE BOOL "" FORCE)
set(TRACY_CALLSTACK ON CACHE BOOL "" FORCE)
set(TRACY_FIBERS ON CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(Tracy)