
# Intel XeSS Super Resolution SDK (D3D11 backend)
set(XESS_VERSION "3.0.2")
set(XESS_SDK_URL "https://github.com/intel/xess/releases/download/v${XESS_VERSION}/XeSS_SDK_${XESS_VERSION}.zip")
set(XESS_SDK_SHA256 "88b8a373f30e33f3558a77a93e634f11b8132fc3047ea1a8edeead32b8471990")
set(XESS_SDK_ZIP "${DEP_DIR}/XeSS_SDK_${XESS_VERSION}.zip")
set(INTEL_XESS "${DEP_DIR}/XeSS-${XESS_VERSION}")
set(XESS_INCLUDE_DIR "${INTEL_XESS}/inc")
set(XESS_LIB_PATH "${INTEL_XESS}/lib/libxess_dx11.lib")
set(XESS_DLL_LIST "${INTEL_XESS}/bin/libxess_dx11.dll")

set(XESS_REQUIRED_FILES
    "${XESS_INCLUDE_DIR}/xess/xess.h"
    "${XESS_INCLUDE_DIR}/xess/xess_d3d11.h"
    "${XESS_LIB_PATH}"
    "${XESS_DLL_LIST}"
    "${INTEL_XESS}/LICENSE.txt"
)
set(XESS_NEED_EXTRACT FALSE)
foreach(XESS_FILE IN LISTS XESS_REQUIRED_FILES)
    if(NOT EXISTS "${XESS_FILE}")
        set(XESS_NEED_EXTRACT TRUE)
    endif()
endforeach()

if(XESS_NEED_EXTRACT)
    file(MAKE_DIRECTORY "${DEP_DIR}")
    if(NOT EXISTS "${XESS_SDK_ZIP}")
        if(IXRAY_OFFLINE)
            message(FATAL_ERROR "XeSS ${XESS_VERSION} is missing and IXRAY_OFFLINE=ON. Place the SDK archive at ${XESS_SDK_ZIP}, or configure once with network access.")
        endif()
        message(STATUS "Downloading Intel XeSS ${XESS_VERSION}...")
        file(DOWNLOAD "${XESS_SDK_URL}" "${XESS_SDK_ZIP}"
            TLS_VERIFY ON
            TIMEOUT 600
            INACTIVITY_TIMEOUT 30
            STATUS XESS_DOWNLOAD_STATUS
        )
        list(GET XESS_DOWNLOAD_STATUS 0 XESS_DOWNLOAD_CODE)
        if(NOT XESS_DOWNLOAD_CODE EQUAL 0)
            file(REMOVE "${XESS_SDK_ZIP}")
            list(GET XESS_DOWNLOAD_STATUS 1 XESS_DOWNLOAD_ERROR)
            message(FATAL_ERROR "Failed to download XeSS: ${XESS_DOWNLOAD_ERROR}")
        endif()
    endif()

    file(SHA256 "${XESS_SDK_ZIP}" XESS_ARCHIVE_SHA256)
    if(NOT XESS_ARCHIVE_SHA256 STREQUAL XESS_SDK_SHA256)
        file(REMOVE "${XESS_SDK_ZIP}")
        message(FATAL_ERROR "XeSS SDK archive SHA256 mismatch. Configure again with network access to download a fresh archive.")
    endif()
    file(ARCHIVE_EXTRACT INPUT "${XESS_SDK_ZIP}" DESTINATION "${INTEL_XESS}")
endif()

foreach(XESS_FILE IN LISTS XESS_REQUIRED_FILES)
    if(NOT EXISTS "${XESS_FILE}")
        message(FATAL_ERROR "XeSS SDK is incomplete: ${XESS_FILE}")
    endif()
endforeach()

add_imported_lib(Intel::XeSS "${XESS_INCLUDE_DIR}" "${XESS_LIB_PATH}" "${XESS_DLL_LIST}")
add_custom_command(TARGET copy_all_dlls PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${INTEL_XESS}/LICENSE.txt" "${IXRAY_OUTPUT}/XeSS-LICENSE.txt"
)
