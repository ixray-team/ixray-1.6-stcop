include(FetchContent)

FetchContent_Declare(
    pffft
    URL https://bitbucket.org/jpommier/pffft/get/master.tar.gz
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

FetchContent_MakeAvailable(pffft)

if (NOT TARGET pffft)
    add_library(pffft STATIC ${pffft_SOURCE_DIR}/fftpack.c ${pffft_SOURCE_DIR}/pffft.c)
    target_include_directories(pffft PUBLIC ${pffft_SOURCE_DIR})

    if(WIN32)
        target_compile_definitions(pffft PRIVATE _USE_MATH_DEFINES)
    else()
        if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
            target_compile_definitions(pffft PRIVATE COMPILER_GCC)
        endif()
        set_target_properties(pffft PROPERTIES POSITION_INDEPENDENT_CODE ON)
    endif()
endif()
