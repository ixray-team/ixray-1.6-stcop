# AMD AGS SDK
set(AMD_AGS_SDK_ZIP ${DEP_DIR}/Amd.Ags.Sdk.5.4.2.zip)
set(AMD_AGS_SDK_URL https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Amd.Ags.Sdk.5.4.2.zip)
set(AMD_AGS_SDK_DIR ${DEP_DIR}/amd_ags_sdk)
download_and_extract_sdk(${AMD_AGS_SDK_URL} ${AMD_AGS_SDK_ZIP} ${AMD_AGS_SDK_DIR})
set(AMD_AGS_SDK ${AMD_AGS_SDK_DIR}/)

set(AMD_AGS_INCLUDE "${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/inc/")
set(AMD_AGS_LIB     "")  # SDK линковка через DLL
set(AMD_AGS_DLL     "${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/lib/amd_ags_${NUGET_PACKAGE_PLATFORM}.dll")

add_imported_lib(AMD::AMD_AGS "${AMD_AGS_INCLUDE}" "${AMD_AGS_LIB}" "${AMD_AGS_DLL}")