# ======================
# OpenAL
# ======================

if(WIN32)
    set(SND_OAL "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/")

    add_imported_lib(
        OpenAL::OpenAL
        "${SND_OAL}/native/include"
        "${SND_OAL}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.lib"
        "${SND_OAL}/native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.dll"
    )
else()
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
endif()
