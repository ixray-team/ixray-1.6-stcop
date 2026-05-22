include(FetchContent)

FetchContent_Declare(
    SDL3
    URL https://github.com/libsdl-org/SDL/archive/refs/tags/release-3.2.22.tar.gz
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

set(SDL_SHARED ON CACHE BOOL "" FORCE)
set(SDL_STATIC OFF CACHE BOOL "" FORCE)
set(SDL_TEST_LIBRARY OFF CACHE BOOL "" FORCE)
set(SDL_REVISION "3.2.22" CACHE STRING "SDL revision" FORCE)

FetchContent_MakeAvailable(SDL3)

set_target_properties(SDL_uclibc PROPERTIES FOLDER "3rd Party")
set_target_properties(SDL3-shared PROPERTIES FOLDER "3rd Party")

# SDL Image
download_single_header(
    "https://raw.githubusercontent.com/libsdl-org/SDL_image/refs/heads/main/include/SDL3_image/SDL_image.h"
    "SDL_Ext/SDL_image.h"
)