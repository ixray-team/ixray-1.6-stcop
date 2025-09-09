# ======================
# openal-soft
# ======================
find_package(OpenAL QUIET)

if(NOT OpenAL_FOUND)
    message(STATUS "openal-soft not found, fetching from source...")
    FetchContent_Declare(
        openal-soft
        GIT_REPOSITORY https://github.com/kcat/openal-soft.git
        GIT_TAG        1.23.1
    )
    set(ALSOFT_UTILS OFF CACHE BOOL "" FORCE)
    set(ALSOFT_EXAMPLES OFF CACHE BOOL "" FORCE)
    set(ALSOFT_TESTS OFF CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(openal-soft)
endif()
