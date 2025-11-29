# NVTT
if (WIN32)
    set(NVTT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Nvtt.Runtimes.win-x64.2024.6.1-open/)
    set(NVTT_INCLUDE ${NVTT}/build/native/include)
    set(NVTT_LIB ${NVTT}/runtimes/win-x64/native/Release/nvtt.lib)
    set(NVTT_BIN ${NVTT}/runtimes/win-x64/native/Release/nvtt.dll)

    add_imported_lib(
        NVTT::nvtt
        "${NVTT_INCLUDE}"
        "${NVTT_LIB}"
        "${NVTT_BIN}"
    )
else()
    FetchContent_Declare(
        nvtt
        GIT_REPOSITORY https://github.com/imesense-forks/castano-nvidia-texture-tools.git
        GIT_TAG default
    )
    FetchContent_MakeAvailable(nvtt)

    if(NOT TARGET NVTT::nvtt)
        add_library(NVTT::nvtt INTERFACE IMPORTED)
        set_target_properties(NVTT::nvtt PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${nvtt_SOURCE_DIR}/src"
            INTERFACE_LINK_LIBRARIES nvtt
        )
    endif()
endif()