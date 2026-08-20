
# AMD FidelityFX FSR3
set(AMD_FSR3 "${CMAKE_BINARY_DIR}/packages/IXRay.FSR3-DX11.3.1.2")
set(FSR3_INCLUDE_DIR "${AMD_FSR3}/include/")
set(FSR3_LIB_PATH "${AMD_FSR3}/lib/ffx_fsr3upscaler_x64.lib")
set(FSR3_DLL "${AMD_FSR3}/bin/ffx_fsr3upscaler_x64.dll")

set(FSR3_LIB_DX11_PATH "${AMD_FSR3}/lib/ffx_backend_dx11_x64.lib")
set(FSR3_DLL_DX11 "${AMD_FSR3}/bin/ffx_backend_dx11_x64.dll")


add_imported_lib(AMD::FSR3_DX11 "${FSR3_INCLUDE_DIR}" "${FSR3_LIB_DX11_PATH}" "${FSR3_DLL_DX11}")
add_imported_lib(AMD::FSR3 "${FSR3_INCLUDE_DIR}" "${FSR3_LIB_PATH}" "${FSR3_DLL}")