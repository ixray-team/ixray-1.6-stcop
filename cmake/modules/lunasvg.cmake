include(FetchContent)
message(STATUS "[packages] downloading repositories: ")

FetchContent_Declare(
    lunasvg
    GIT_REPOSITORY https://github.com/sammycage/lunasvg.git
    GIT_TAG v3.4.0
)

set(LUNASVG_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(lunasvg)

message(STATUS "[packages] lunasvg - downloaded!")