#pragma once

#include <d3d11_4.h>
#include <wrl/client.h>

#include <FidelityFX/api/include/ffx_api.h>
#include <FidelityFX/api/include/ffx_api_types.h>

// ---------------------------------------------------------------------------
// Shared types. Fsr4DrawParameters / Fsr4ContextParameters are the public
// shapes used by both FSR4Wrapper (public façade) and Fsr4Backend (internal).
// FFX-specific stuff stays inside Fsr4Backend.
// ---------------------------------------------------------------------------

enum class Fsr4Status : int
{
    Ok                 = 0,
    NotInitialized     = 1,
    LoaderMissing      = 2,
    EntrypointsMissing = 3,
    DeviceFailed       = 4,
    InteropUnsupported = 5,
    NoVersions         = 6,
    ContextFailed      = 7,
    DispatchFailed     = 8,
    DeviceLost         = 9,
};

// Context-affecting flags (bits 0..6) and per-dispatch flags (bits 7..11).
enum Fsr4Flags : int
{
    Fsr4Flag_Hdr            = 1 << 0,
    Fsr4Flag_DepthInverted  = 1 << 1,
    Fsr4Flag_DepthInfinite  = 1 << 2,
    Fsr4Flag_AutoExposure   = 1 << 3,
    Fsr4Flag_DebugChecking  = 1 << 4,
    Fsr4Flag_DynamicRes     = 1 << 5,
    Fsr4Flag_PreferFsr4     = 1 << 6,
    Fsr4Flag_Reset          = 1 << 7,
    Fsr4Flag_MvJitterCancel = 1 << 8,
    Fsr4Flag_Reactive       = 1 << 9,
    Fsr4Flag_ModelFix       = 1 << 10,
    Fsr4Flag_Optic          = 1 << 11,
};

inline constexpr int Fsr4PerDispatchFlagsMask = Fsr4Flag_Reset | Fsr4Flag_Reactive | Fsr4Flag_Optic;
inline constexpr ffxReturnCode_t Fsr4FfxSehCrashCode = 0x0BADC0DE;

struct Fsr4ContextParameters
{
    u32                 flags         = 0;
    FfxApiDimensions2D  maxRenderSize = { 0, 0 };
    FfxApiDimensions2D  displaySize   = { 0, 0 };
    ID3D11Device*       device        = nullptr;
};

struct Fsr4DrawParameters
{
    ID3D11DeviceContext* deviceContext = nullptr;

    // Inputs (render resolution, game-owned)
    ID3D11Resource* unresolvedColorResource            = nullptr;
    ID3D11Resource* motionvectorResource               = nullptr;
    ID3D11Resource* depthbufferResource                = nullptr;
    ID3D11Resource* reactiveMapResource                = nullptr;
    ID3D11Resource* transparencyAndCompositionResource = nullptr;

    // Optional for auto-reactive-mask generation
    ID3D11Resource* opaqueColorResource     = nullptr;
    ID3D11Resource* afterAlphaColorResource = nullptr;

    // Output (display resolution, game-owned)
    ID3D11Resource* resolvedColorResource = nullptr;

    // Sizes
    u32 renderWidth   = 0;
    u32 renderHeight  = 0;
    u32 displayWidth  = 0;
    u32 displayHeight = 0;

    // Camera
    bool  cameraReset   = false;
    float cameraJitterX = 0.f;
    float cameraJitterY = 0.f;

    // Sharpening
    bool  enableSharpening = true;
    float sharpness        = 0.f;

    // Frame timing & frustum
    float frameTimeDelta = 0.f;
    float nearPlane      = 1.f;
    float farPlane       = 10.f;
    float fovYRadians    = 1.57f;

    // Fsr4Flags
    int flags = 0;
};

inline const char* Fsr4StatusToString(Fsr4Status s)
{
	switch (s)
	{
		case Fsr4Status::Ok:                 return "OK";
		case Fsr4Status::NotInitialized:     return "Not initialized";
		case Fsr4Status::LoaderMissing:      return "FFX loader DLL missing";
		case Fsr4Status::EntrypointsMissing: return "FFX entry points missing";
		case Fsr4Status::DeviceFailed:       return "D3D12 device failed";
		case Fsr4Status::InteropUnsupported: return "Interop unsupported";
		case Fsr4Status::NoVersions:         return "No FFX providers";
		case Fsr4Status::ContextFailed:      return "Context creation failed";
		case Fsr4Status::DispatchFailed:     return "Dispatch failed";
		case Fsr4Status::DeviceLost:         return "Device lost";
	}
	return "Unknown";
}
