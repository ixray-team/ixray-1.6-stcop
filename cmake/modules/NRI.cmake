include(FetchContent)

# NRI declares cmake_minimum_required(3.30) for its own directory; the engine floor stays lower.
if(CMAKE_VERSION VERSION_LESS 3.30)
    message(FATAL_ERROR "NRI requires CMake 3.30 or newer (found ${CMAKE_VERSION})")
endif()

set(NRI_STATIC_LIBRARY OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_D3D11_SUPPORT ${WIN32} CACHE BOOL "" FORCE)
set(NRI_ENABLE_D3D12_SUPPORT ${WIN32} CACHE BOOL "" FORCE)
set(NRI_ENABLE_VK_SUPPORT ON CACHE BOOL "" FORCE)
set(NRI_ENABLE_METAL_SUPPORT OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_WGPU_SUPPORT OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_NONE_SUPPORT ON CACHE BOOL "" FORCE)
set(NRI_ENABLE_VALIDATION_SUPPORT ON CACHE BOOL "" FORCE)
set(NRI_ENABLE_IMGUI_EXTENSION OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_NVTX_SUPPORT OFF CACHE BOOL "" FORCE)
# The engine links its own AGS and NVAPI, and the native D3D12 backend runs on the system runtime.
set(NRI_ENABLE_AMDAGS OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_NVAPI OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_AGILITY_SDK_SUPPORT OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_NIS_SDK OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_NGX_SDK OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_FFX_SDK OFF CACHE BOOL "" FORCE)
set(NRI_ENABLE_XESS_SDK OFF CACHE BOOL "" FORCE)

FetchContent_Declare(
    nri
    GIT_REPOSITORY https://github.com/NVIDIA-RTX/NRI.git
    GIT_TAG v191
    GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(nri)

foreach(NRI_TARGET NRI NRI_Shared NRI_NONE NRI_Validation NRI_D3D11 NRI_D3D12 NRI_VK)
    if(TARGET ${NRI_TARGET})
        # The engine uses the release DLL CRT in every configuration.
        set_target_properties(${NRI_TARGET} PROPERTIES FOLDER "3rd Party/NRI" MSVC_RUNTIME_LIBRARY "MultiThreadedDLL")
        # NRI builds with warnings as errors; the engine's global flags must not break it.
        if(MSVC)
            target_compile_options(${NRI_TARGET} PRIVATE /WX-)
        else()
            target_compile_options(${NRI_TARGET} PRIVATE -Wno-error)
        endif()
    endif()
endforeach()

# Without the Agility SDK, NRI declares stub micromap structs unless this macro exists. Windows SDK
# 10.0.26100 (D3D12_SDK_VERSION 616) has the real structs but not the macro, so the stubs collide.
if(TARGET NRI_D3D12)
    target_compile_definitions(NRI_D3D12 PRIVATE D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_BYTE_ALIGNMENT=128)
endif()
