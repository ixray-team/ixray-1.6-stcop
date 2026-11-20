# ============================================================================
# d3dcompiler_47.dll — загрузка библиотеки с nuget.org
# ============================================================================
string(TOLOWER "${CMAKE_SYSTEM_PROCESSOR}" _D3D_PROC)
if(_D3D_PROC MATCHES "^(arm64|aarch64)$")
    set(D3DCOMPILER_ARCH "arm64")
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(D3DCOMPILER_ARCH "x64")
else()
    set(D3DCOMPILER_ARCH "x86")
endif()
message(STATUS "Windows SDK: target DLL architecture = ${D3DCOMPILER_ARCH}")

set(WINSDK_PACKAGE_VERSION "10.0.28000.2957")
set(WINSDK_SDK_VERSION     "10.0.28000.0")
set(WINSDK_URL     "https://www.nuget.org/api/v2/package/Microsoft.Windows.SDK.CPP/${WINSDK_PACKAGE_VERSION}")
set(WINSDK_SHA256  "f0a200dae58858f418b30dc9e2f52b4188f97eb7837bd4f73074652a01937ddd")

set(WINSDK_ARCHIVE      "${DEP_DIR}/Microsoft.Windows.SDK.CPP-${WINSDK_PACKAGE_VERSION}.zip")
set(WINSDK_EXTRACT_DIR  "${DEP_DIR}/Microsoft.Windows.SDK.CPP-${WINSDK_PACKAGE_VERSION}")
set(WINSDK_D3DCOMPILER  "${WINSDK_EXTRACT_DIR}/c/bin/${WINSDK_SDK_VERSION}/${D3DCOMPILER_ARCH}/d3dcompiler_47.dll")

set(WINSDK_REQUIRED_FILES
        "${WINSDK_D3DCOMPILER}"
)

file(MAKE_DIRECTORY "${DEP_DIR}")

if(NOT EXISTS "${WINSDK_ARCHIVE}")
    if(IXRAY_OFFLINE)
        message(FATAL_ERROR "Windows SDK ${WINSDK_PACKAGE_VERSION} is missing and IXRAY_OFFLINE=ON. Place the SDK archive at ${WINSDK_ARCHIVE}, or configure once with network access.")
    endif()
    message(STATUS "Downloading Microsoft Windows SDK C++ ${WINSDK_PACKAGE_VERSION}...")
    file(DOWNLOAD "${WINSDK_URL}" "${WINSDK_ARCHIVE}"
            TLS_VERIFY ON
            TIMEOUT 600
            INACTIVITY_TIMEOUT 30
            STATUS WINSDK_DOWNLOAD_STATUS
    )
    list(GET WINSDK_DOWNLOAD_STATUS 0 WINSDK_DOWNLOAD_CODE)
    if(NOT WINSDK_DOWNLOAD_CODE EQUAL 0)
        file(REMOVE "${WINSDK_ARCHIVE}")
        list(GET WINSDK_DOWNLOAD_STATUS 1 WINSDK_DOWNLOAD_ERROR)
        message(FATAL_ERROR "Failed to download Windows SDK: ${WINSDK_DOWNLOAD_ERROR}")
    endif()
endif()

file(SHA256 "${WINSDK_ARCHIVE}" WINSDK_ARCHIVE_SHA256)
if(NOT WINSDK_ARCHIVE_SHA256 STREQUAL WINSDK_SHA256)
    file(REMOVE "${WINSDK_ARCHIVE}")
    message(FATAL_ERROR "Windows SDK archive SHA256 mismatch. Expected ${WINSDK_SHA256}, got ${WINSDK_ARCHIVE_SHA256}. Configure again with network access to download a fresh archive.")
endif()

file(MAKE_DIRECTORY "${WINSDK_EXTRACT_DIR}")
file(ARCHIVE_EXTRACT INPUT "${WINSDK_ARCHIVE}" DESTINATION "${WINSDK_EXTRACT_DIR}")

foreach(WINSDK_FILE IN LISTS WINSDK_REQUIRED_FILES)
    if(NOT EXISTS "${WINSDK_FILE}")
        message(FATAL_ERROR "Windows SDK is incomplete: ${WINSDK_FILE}")
    endif()
endforeach()

add_custom_command(TARGET copy_all_dlls PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${WINSDK_D3DCOMPILER}" "${IXRAY_OUTPUT}/d3dcompiler_47.dll"
)