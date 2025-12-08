# AMD FidelityFX FSR2
set(AMD_FSR2 ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FidelityFX.FSR2.DirectX11.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2.2.1.1)
set(FSR2_INCLUDE_DIR "${AMD_FSR2}/build/native/include/")
set(FSR2_LIB_PATH "${AMD_FSR2}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/ffx_fsr2_api_${NUGET_PACKAGE_PLATFORM}.lib")
set(FSR2_LIB_DX11_PATH "${AMD_FSR2}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/ffx_fsr2_api_dx11_${NUGET_PACKAGE_PLATFORM}.lib")

set(FSR2_DLL_LIST
    "${AMD_FSR2}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/ffx_fsr2_api_${NUGET_PACKAGE_PLATFORM}.dll"
    "${AMD_FSR2}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/ffx_fsr2_api_dx11_${NUGET_PACKAGE_PLATFORM}.dll"
)

add_imported_lib(AMD::FSR2 "${FSR2_INCLUDE_DIR}" "${FSR2_LIB_PATH}" "${FSR2_DLL_LIST}")
add_imported_lib(AMD::FSR2_DX11 "${FSR2_INCLUDE_DIR}" "${FSR2_LIB_DX11_PATH}" "${FSR2_DLL_LIST}")