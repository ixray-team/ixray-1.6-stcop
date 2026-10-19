include(FetchContent)

FetchContent_Declare(
    Tracy
    GIT_REPOSITORY https://github.com/wolfpld/tracy.git
    GIT_TAG v0.14.1
)

set(TRACY_ENABLE 1 CACHE BOOL "" FORCE)
set(TRACY_STATIC OFF CACHE BOOL "" FORCE)
set(TRACY_NO_VSYNC_CAPTURE 1 CACHE BOOL "" FORCE)
set(TRACY_ON_DEMAND 1 CACHE BOOL "" FORCE)
set(TRACY_CALLSTACK 1 CACHE BOOL "" FORCE)
if(WIN32)
    # Tracy installs its filter on connection and otherwise bypasses the engine's Windows dump helper.
    set(TRACY_NO_CRASH_HANDLER ON CACHE BOOL "" FORCE)
endif()
# Engine workers are OS threads; use Tracy's per-thread queues.
set(TRACY_FIBERS OFF CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(Tracy)

if (MSVC AND TARGET TracyClient)
    target_compile_options(TracyClient PRIVATE $<$<CONFIG:DEBUG>:/Zi>)
endif()
