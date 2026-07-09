# ======================
# OpenAL
# ======================

if(WIN32)
    set(SND_OAL "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/")
    set(OPENAL_INCLUDE_DIR "${SND_OAL}/native/include")
    set(OPENAL_LIBRARY "${SND_OAL}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.lib")
    set(OPENAL_DLL "${SND_OAL}/native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.dll")

    if(EXISTS "${OPENAL_INCLUDE_DIR}" AND EXISTS "${OPENAL_LIBRARY}")
        # Используем готовый NuGet‑пакет
        add_imported_lib(
            OpenAL::OpenAL
            "${OPENAL_INCLUDE_DIR}"
            "${OPENAL_LIBRARY}"
            "${OPENAL_DLL}"
        )
    else()
        message(STATUS "OpenAL NuGet package not found, fetching from source...")
        set(CMAKE_POLICY_VERSION_MINIMUM 3.5 CACHE STRING "Minimum CMake version for openal-soft" FORCE)

        FetchContent_Declare(
            openal-soft
            GIT_REPOSITORY https://github.com/kcat/openal-soft.git
            GIT_TAG        1.23.1
        )
        set(ALSOFT_UTILS OFF CACHE BOOL "" FORCE)
        set(ALSOFT_EXAMPLES OFF CACHE BOOL "" FORCE)
        set(ALSOFT_TESTS OFF CACHE BOOL "" FORCE)
        FetchContent_MakeAvailable(openal-soft)
        # openal-soft сам создаёт цель OpenAL::OpenAL, поэтому НЕ создаём алиас
    endif()
else()
    # Не-Windows: сначала ищем системный OpenAL
    find_package(OpenAL QUIET)

    if(NOT OpenAL_FOUND)
        message(STATUS "openal-soft not found, fetching from source...")
        set(CMAKE_POLICY_VERSION_MINIMUM 3.5 CACHE STRING "Minimum CMake version for openal-soft" FORCE)

        FetchContent_Declare(
            openal-soft
            GIT_REPOSITORY https://github.com/kcat/openal-soft.git
            GIT_TAG        1.23.1
        )
        set(ALSOFT_UTILS OFF CACHE BOOL "" FORCE)
        set(ALSOFT_EXAMPLES OFF CACHE BOOL "" FORCE)
        set(ALSOFT_TESTS OFF CACHE BOOL "" FORCE)
        FetchContent_MakeAvailable(openal-soft)
        # цель OpenAL::OpenAL уже создана внутри openal-soft
    else()
        # find_package(OpenAL) в современных версиях CMake создаёт цель OpenAL::OpenAL
        # если этого не происходит, можно создать вручную:
        # if(NOT TARGET OpenAL::OpenAL)
        #     add_library(OpenAL::OpenAL INTERFACE IMPORTED)
        #     set_target_properties(OpenAL::OpenAL PROPERTIES
        #         INTERFACE_INCLUDE_DIRECTORIES "${OPENAL_INCLUDE_DIR}"
        #         INTERFACE_LINK_LIBRARIES "${OPENAL_LIBRARY}"
        #     )
        # endif()
    endif()
endif()