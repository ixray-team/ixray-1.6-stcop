set(FETCHCONTENT_BASE_DIR ${CMAKE_BINARY_DIR}/dep)
include(FetchContent)

FetchContent_Declare(
    tbb
    GIT_REPOSITORY https://github.com/oneapi-src/oneTBB.git
    GIT_TAG v2021.5.0
)

FetchContent_MakeAvailable(tbb)