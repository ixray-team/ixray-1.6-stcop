# AMD AGS SDK
set(AMD_AGS_INCLUDE "${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/inc/")
set(AMD_AGS_LIB     "")  # SDK линковка через DLL
set(AMD_AGS_DLL     "${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/lib/amd_ags_${NUGET_PACKAGE_PLATFORM}.dll")

add_imported_lib(AMD::AMD_AGS "${AMD_AGS_INCLUDE}" "${AMD_AGS_LIB}" "${AMD_AGS_DLL}")