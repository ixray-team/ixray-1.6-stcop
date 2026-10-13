set(DLSS_VERSION "310.9.1")
set(DLSS_SDK_URL "https://github.com/NVIDIA/DLSS/archive/refs/tags/v${DLSS_VERSION}.zip")
set(DLSS_SDK_SHA256 "c2ada58c49cacde5d736c44bf1a6129c2e5ca43c9308388d4689d717f48da062")
set(DLSS_SDK_ZIP "${DEP_DIR}/DLSS-${DLSS_VERSION}.zip")
set(NVIDIA_DLSS "${DEP_DIR}/DLSS-${DLSS_VERSION}")
set(DLSS_INCLUDE_DIR "${NVIDIA_DLSS}/include")
set(DLSS_LIB_PATH "${NVIDIA_DLSS}/lib/Windows_x86_64/x64/nvsdk_ngx_d.lib")
set(DLSS_DLL_LIST "${NVIDIA_DLSS}/lib/Windows_x86_64/rel/nvngx_dlss.dll")

set(DLSS_REQUIRED_FILES
    "${DLSS_INCLUDE_DIR}/nvsdk_ngx.h"
    "${DLSS_INCLUDE_DIR}/nvsdk_ngx_helpers.h"
    "${DLSS_LIB_PATH}"
    "${DLSS_DLL_LIST}"
    "${NVIDIA_DLSS}/LICENSE.txt"
)
set(DLSS_NEED_EXTRACT FALSE)
foreach(DLSS_FILE IN LISTS DLSS_REQUIRED_FILES)
    if(NOT EXISTS "${DLSS_FILE}")
        set(DLSS_NEED_EXTRACT TRUE)
    endif()
endforeach()

if(DLSS_NEED_EXTRACT)
    file(MAKE_DIRECTORY "${DEP_DIR}")
    if(NOT EXISTS "${DLSS_SDK_ZIP}")
        if(IXRAY_OFFLINE)
            message(FATAL_ERROR "DLSS ${DLSS_VERSION} is missing and IXRAY_OFFLINE=ON. Place the SDK archive at ${DLSS_SDK_ZIP}, or configure once with network access.")
        endif()
        message(STATUS "Downloading NVIDIA DLSS ${DLSS_VERSION}...")
        file(DOWNLOAD "${DLSS_SDK_URL}" "${DLSS_SDK_ZIP}"
            TLS_VERIFY ON
            TIMEOUT 600
            INACTIVITY_TIMEOUT 30
            STATUS DLSS_DOWNLOAD_STATUS
        )
        list(GET DLSS_DOWNLOAD_STATUS 0 DLSS_DOWNLOAD_CODE)
        if(NOT DLSS_DOWNLOAD_CODE EQUAL 0)
            file(REMOVE "${DLSS_SDK_ZIP}")
            list(GET DLSS_DOWNLOAD_STATUS 1 DLSS_DOWNLOAD_ERROR)
            message(FATAL_ERROR "Failed to download DLSS: ${DLSS_DOWNLOAD_ERROR}")
        endif()
    endif()

    file(SHA256 "${DLSS_SDK_ZIP}" DLSS_ARCHIVE_SHA256)
    if(NOT DLSS_ARCHIVE_SHA256 STREQUAL DLSS_SDK_SHA256)
        file(REMOVE "${DLSS_SDK_ZIP}")
        message(FATAL_ERROR "DLSS SDK archive SHA256 mismatch. Configure again with network access to download a fresh archive.")
    endif()
    file(ARCHIVE_EXTRACT INPUT "${DLSS_SDK_ZIP}" DESTINATION "${DEP_DIR}")
endif()

foreach(DLSS_FILE IN LISTS DLSS_REQUIRED_FILES)
    if(NOT EXISTS "${DLSS_FILE}")
        message(FATAL_ERROR "DLSS SDK is incomplete: ${DLSS_FILE}")
    endif()
endforeach()

add_imported_lib(NVIDIA::DLSS "${DLSS_INCLUDE_DIR}" "${DLSS_LIB_PATH}" "${DLSS_DLL_LIST}")
add_custom_command(TARGET copy_all_dlls PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${NVIDIA_DLSS}/LICENSE.txt" "${IXRAY_OUTPUT}/DLSS-LICENSE.txt"
)
