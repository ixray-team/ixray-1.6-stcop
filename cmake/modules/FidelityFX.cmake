# AMD FidelityFX SDK 2.4.0 / FSR4
include(FetchContent)

FetchContent_Declare(
    fidelityfx_sdk
    GIT_REPOSITORY https://github.com/GPUOpen-LibrariesAndSDKs/FidelityFX-SDK.git
    GIT_TAG        v2.4.0
    GIT_SHALLOW    TRUE
)

FetchContent_GetProperties(fidelityfx_sdk)
if (NOT fidelityfx_sdk_POPULATED)
    FetchContent_MakeAvailable(fidelityfx_sdk)
endif()

set(FidelityFX_SDK_ROOT "${fidelityfx_sdk_SOURCE_DIR}")
set(FidelityFX_SDK_INCLUDE_DIRS "${FidelityFX_SDK_ROOT}/Kits")

add_library(FidelityFX_SDK INTERFACE)

target_include_directories(FidelityFX_SDK INTERFACE ${FidelityFX_SDK_INCLUDE_DIRS})

# -----------------------------------------------------------------------------
# Автоматическое копирование runtime-DLL FidelityFX (DX12)
# -----------------------------------------------------------------------------
# FidelityFX SDK кладёт подписанные DLL в Kits/FidelityFX/signedbin
set(FFX_SIGNEDBIN_DIR "${FidelityFX_SDK_ROOT}/Kits/FidelityFX/signedbin")

# Список DLL, которые должны оказаться рядом с exe (можно расширить при необходимости)
set(FFX_RUNTIME_DLLS
    "${FFX_SIGNEDBIN_DIR}/amd_fidelityfx_upscaler_dx12.dll"
    "${FFX_SIGNEDBIN_DIR}/amd_fidelityfx_loader_dx12.dll"
)

if(TARGET copy_all_dlls)
    add_custom_command(TARGET copy_all_dlls PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${FFX_RUNTIME_DLLS}
            "${IXRAY_OUTPUT}"
        COMMENT "Copying FidelityFX runtime DLLs to ${IXRAY_OUTPUT}"
    )
endif()

# target_compile_definitions(FidelityFX_SDK INTERFACE IXRAY_USE_FSR4=1)
