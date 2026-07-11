set(HIDAPI_SDK_URL "https://github.com/libusb/hidapi/releases/download/hidapi-0.15.0/hidapi-win.zip")
set(HIDAPI_SDK_ZIP "${DEP_DIR}/hidapi-win.zip")
set(HIDAPI_SDK_DIR "${DEP_DIR}/hidapi")
download_and_extract_sdk(${HIDAPI_SDK_URL} ${HIDAPI_SDK_ZIP} ${HIDAPI_SDK_DIR})
set(HIDAPI_ROOT ${HIDAPI_SDK_DIR})

if(NOT TARGET HIDAPI::HIDAPI)
	set(HIDAPI_LIBRARIES "${HIDAPI_ROOT}/x64/hidapi.lib")

	set(HIDAPI_DLLS "${HIDAPI_ROOT}/x64/hidapi.dll")
	add_imported_lib(HIDAPI::HIDAPI "${HIDAPI_ROOT}/include" "${HIDAPI_LIBRARIES}" "${HIDAPI_DLLS}")
endif()

mark_as_advanced(HIDAPI_INCLUDE_DIR HIDAPI_LIBRARY HIDAPI_DLL)