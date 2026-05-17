# DirectXMesh
if(WIN32)
    set(DXMESH_INCLUDE "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/")
    set(DXMESH_LIB     "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/native/lib/x64/Release/DirectXMesh.lib")
    add_imported_lib(DirectX::Mesh "${DXMESH_INCLUDE}" "${DXMESH_LIB}" "")
else()
    include(FetchContent)
    # -------------------------------------------------------------------------
    # 1. DirectX-Headers (заголовки Direct3D 12 для Linux)
    # -------------------------------------------------------------------------
    FetchContent_Declare(
        directx-headers
        GIT_REPOSITORY https://github.com/microsoft/DirectX-Headers.git
        GIT_TAG        v1.711.3-preview
        GIT_SHALLOW    TRUE
    )
    FetchContent_MakeAvailable(directx-headers)

    # Создаём конфигурационный файл для directx-headers
    set(DIRECTX_HEADERS_CONFIG_DIR ${CMAKE_BINARY_DIR}/directx-headers-config)
    file(MAKE_DIRECTORY ${DIRECTX_HEADERS_CONFIG_DIR})
    
    file(WRITE ${DIRECTX_HEADERS_CONFIG_DIR}/directx-headers-config.cmake "
        # DirectX-Headers config file
        if(NOT TARGET Microsoft::DirectX-Headers)
            add_library(Microsoft::DirectX-Headers INTERFACE IMPORTED)
            set_target_properties(Microsoft::DirectX-Headers PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES \"${directx-headers_SOURCE_DIR}/include\")
        endif()
        set(directx-headers_FOUND TRUE)
    ")
    
    set(directx-headers_DIR ${DIRECTX_HEADERS_CONFIG_DIR})

    # -------------------------------------------------------------------------
    # 2. DirectXMath
    # -------------------------------------------------------------------------
    FetchContent_Declare(
        DirectXMath
        GIT_REPOSITORY https://github.com/microsoft/DirectXMath.git
        GIT_TAG        dec2023
        GIT_SHALLOW    TRUE
    )
    FetchContent_MakeAvailable(DirectXMath)

    # Создаём конфигурационный файл для DirectXMath
    set(DIRECTXMATH_CONFIG_DIR ${CMAKE_BINARY_DIR}/directxmath-config)
    file(MAKE_DIRECTORY ${DIRECTXMATH_CONFIG_DIR})

    file(WRITE ${DIRECTXMATH_CONFIG_DIR}/directxmath-config.cmake "
        if(NOT TARGET Microsoft::DirectXMath)
            add_library(Microsoft::DirectXMath INTERFACE IMPORTED)
            set_target_properties(Microsoft::DirectXMath PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES \"${DirectXMath_SOURCE_DIR}/Inc\")
        endif()

        set(DirectXMath_FOUND TRUE)
    ")
    
    set(directxmath_DIR ${DIRECTXMATH_CONFIG_DIR})

    # -------------------------------------------------------------------------
    # 3. DirectXMesh
    # -------------------------------------------------------------------------
    FetchContent_Declare(
        DirectXMesh
        GIT_REPOSITORY https://github.com/microsoft/DirectXMesh.git
        GIT_TAG        oct2023
        GIT_SHALLOW    TRUE
    )

    set(BUILD_X86 OFF CACHE BOOL "" FORCE)
    set(BUILD_X64 ON CACHE BOOL "" FORCE)
    set(BUILD_DX12 OFF CACHE BOOL "" FORCE)
    set(BUILD_TOOLS OFF CACHE BOOL "" FORCE)
    set(BUILD_TESTING OFF CACHE BOOL "" FORCE)
    set(CMAKE_SKIP_INSTALL_RULES ON)

    FetchContent_MakeAvailable(DirectXMesh)
    target_include_directories(DirectXMesh PUBLIC ${CMAKE_SOURCE_DIR}/linux)
    
    if(TARGET DirectXMesh AND NOT TARGET DirectX::Mesh)
        add_library(DirectX::Mesh ALIAS DirectXMesh)
    endif()
endif()