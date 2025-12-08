set(NVIDIA_DLSS ${CMAKE_BINARY_DIR}/packages/IXRay.DLSS.310.4.0/)
set(DLSS_INCLUDE_DIR "${NVIDIA_DLSS}/include")
set(DLSS_LIB_PATH "${NVIDIA_DLSS}/lib/nvsdk_ngx_d.lib")
set(DLSS_DLL_LIST "${NVIDIA_DLSS}/bin/nvngx_dlss.dll")

add_imported_lib(NVIDIA::DLSS "${DLSS_INCLUDE_DIR}" "${DLSS_LIB_PATH}" "${DLSS_DLL_LIST}")