include(FetchContent)

FetchContent_Declare(
    NRI
    GIT_REPOSITORY https://github.com/NVIDIA-RTX/NRI.git
    # Renderer baseline проверен с v180. Ветка main не является
    # воспроизводимой зависимостью и уже меняла публичный API без фиксации кода.
    GIT_TAG v180
)

set(NRI_ENABLE_IMGUI_EXTENSION TRUE)

# RenderDoc 1.45 не перехватывает ID3D12Device15 из Agility SDK 619.3 и
# оставляет созданный через него swapchain вне capture. Текущий Tiramisu
# использует API уровня ID3D12Device8, поэтому системный D3D12 сохраняет
# функциональность renderer и делает Vulkan/D3D12 validation симметричной.
option(
    IXRAY_NRI_ENABLE_AGILITY_SDK
    "Enable NRI D3D12 Agility SDK interfaces newer than RenderDoc 1.45"
    OFF
)
set(
    NRI_ENABLE_AGILITY_SDK_SUPPORT
    ${IXRAY_NRI_ENABLE_AGILITY_SDK}
    CACHE BOOL "Enable NRI Agility SDK support" FORCE
)

FetchContent_MakeAvailable(NRI)

if(WIN32 AND NOT IXRAY_NRI_ENABLE_AGILITY_SDK)
    # Windows SDK 10.0.26100 уже объявляет OMM-типы, но v180 проверяет только
    # одноимённый macro и пытается объявить несовместимые заглушки повторно.
    target_compile_definitions(
        NRI_D3D12
        PRIVATE D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_BYTE_ALIGNMENT=128
    )
endif()

set_target_properties(NRI PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_Shared PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_NONE PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_D3D12 PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_VK PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_Validation PROPERTIES FOLDER "3rd Party")
