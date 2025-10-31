include(FetchContent)

FetchContent_Declare(
    eigen
    GIT_REPOSITORY https://gitlab.com/libeigen/eigen.git
    GIT_TAG master
)
FetchContent_MakeAvailable(eigen)

add_compile_definitions(EIGEN_MPL2_ONLY)
include_directories(${PROJECT_SOURCE_DIR})