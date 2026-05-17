if(WIN32)
    set(DXTEX_INCLUDE "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/include/")
    set(DXTEX_LIB     "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/native/lib/x64/Release/DirectXTex.lib")
    add_imported_lib(DirectX::Tex "${DXTEX_INCLUDE}" "${DXTEX_LIB}" "")
else()
    FetchContent_Declare(
        directxtex
        GIT_REPOSITORY https://github.com/microsoft/DirectXTex.git
        GIT_TAG main
    )
    FetchContent_MakeAvailable(directxtex)
    
    target_include_directories(DirectXTex PUBLIC ${CMAKE_BINARY_DIR}/_deps_header_only/linux)
    add_library(DirectX::Tex ALIAS DirectXTex)
endif()