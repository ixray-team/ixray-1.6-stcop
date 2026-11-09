add_library(DXC::Headers INTERFACE IMPORTED)

if(NOT WIN32)
    # Keep DXC's native COM declarations isolated from DXVK and DXMT headers.
    set(DXC_VERSION "v1.8.2505.1")
    file(MAKE_DIRECTORY "${EXTERNAL_INCLUDE_DIR}dxc")
    foreach(DXC_HEADER dxcapi.h WinAdapter.h)
        download_single_header(
            "https://raw.githubusercontent.com/microsoft/DirectXShaderCompiler/${DXC_VERSION}/include/dxc/${DXC_HEADER}"
            "dxc/${DXC_HEADER}"
        )
    endforeach()
    set_target_properties(DXC::Headers PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${EXTERNAL_INCLUDE_DIR}dxc"
    )
endif()
