#pragma once

#include "Fsr4BridgeCommon.h"
#include "Fsr4SharedTexture.h"

#include <string>
#include <vector>
#include <FidelityFX/upscalers/include/ffx_upscale.h>

// ---------------------------------------------------------------------------
// Fsr4Backend — the D3D11↔D3D12 bridge and FFX host.
//
// Owns everything except the public façade:
//   • the D3D12 device paired with the game's D3D11 device (lazily created);
//   • the cross-API timeline fence (D3D12Fence ↔ ID3D11Fence);
//   • the ffx-api library, providers, and the upscale context;
//   • a ring of D3D12 command allocators and one command list;
//   • shared textures for color/depth/mvec/output (+opaque/afterAlpha);
//   • a D3D11 fullscreen depth→R32_FLOAT pass for the depth input.
//
// The wrapper calls Initialize() once, then DrawFrame() per frame, Shutdown()
// when done. No preset/render-scale logic lives here.
// ---------------------------------------------------------------------------
class Fsr4Backend
{
public:
    Fsr4Backend();
    ~Fsr4Backend();

    Fsr4Backend(const Fsr4Backend&) = delete;
    Fsr4Backend& operator=(const Fsr4Backend&) = delete;

    struct FrameTextures
    {
        Microsoft::WRL::ComPtr<ID3D11Texture2D> color, depth, mvec, output, opaque, afterAlpha;
    };

    bool Initialize(ID3D11Device* d3d11Device, const wchar_t* ffxDllDir);
    void Shutdown();

    bool DrawFrame(const Fsr4DrawParameters& params);

    Fsr4Status GetStatus()    const { return m_Status; }
    bool       IsFsr4Active() const { return m_ChosenIsFsr4; }
	
	const xr_string& GetActiveVersionString() const { return m_ActiveVersionName; }

private:
    // ---- frame pipeline steps ----
    bool ResolveFrameTextures(const Fsr4DrawParameters& p, FrameTextures& out);
    bool EnsureDevices(ID3D11Texture2D* anyTexture);
    bool EnsureSharedTextures(const FrameTextures& tex);
    bool NeedsContextRebuild(const Fsr4DrawParameters& p) const;
    bool EnsureContext(const Fsr4DrawParameters& p);
    bool PrepareReactiveMask(const Fsr4DrawParameters& p, const FrameTextures& tex);
    void CopyInputsToShared(const FrameTextures& tex, bool reactive);
    bool RecordAndSubmit(const Fsr4DrawParameters& p, const FrameTextures& tex, bool reactive);
    void CopyOutputToGame(const FrameTextures& tex);
    bool HandleDispatchFailure(ffxReturnCode_t rc);

    // ---- depth copy (D24_UNORM_S8_UINT → R32_FLOAT) ----
    bool InitDepthCopyPipeline();
    void ReleaseDepthCopyPipeline();
    bool CopyDepthToShared(ID3D11Texture2D* gameDepth);

    // ---- device management ----
    bool CreateD3D12Device(IDXGIAdapter* adapter);
    bool ShareFenceToD3D11();
    bool InitCommandRing();
    void ReleaseCommandRing();
    void ReleaseDevices();
    void ResetSharedTextures();
    void ReleaseUpscalerState();
    bool IsDeviceLost() const;

    // ---- FFX context ----
    bool     LoadFfxLibrary(const wchar_t* dllDir);
    void     QueryVersionsOnce();
    u64      PickProviderVersionId(bool preferFsr4);
    static u32 TranslateFfxFlags(int ourFlags);
    void     DestroyContext();
    bool     EnsureReactiveMask(UINT w, UINT h);
    void     DestroyReactiveMask();

    // ---- command list / sync ----
    ID3D12GraphicsCommandList* BeginFrame(UINT64 waitForD3D11Value);
    void                       AbortFrame();
    UINT64                     EndFrameAndSubmit();
    UINT64                     SignalD3D11Inputs();
    void                       WaitD3D11Output(UINT64 value);
    void                       WaitIdle();

    // ---- FFX dispatch ----
    ffxReturnCode_t DispatchUpscale(ID3D12GraphicsCommandList* cmd, ffxDispatchDescUpscale* desc);
    ffxReturnCode_t DispatchReactive(ID3D12GraphicsCommandList* cmd, ffxDispatchDescUpscaleGenerateReactiveMask* desc);

    // ---- SEH shims ----
    ffxReturnCode_t SafeQuery(ffxContext* ctx, ffxQueryDescHeader* desc);
    ffxReturnCode_t SafeCreateContext(ffxContext* ctx, ffxCreateContextDescHeader* desc);
    ffxReturnCode_t SafeDispatch(ffxContext* ctx, ffxDispatchDescHeader* desc);
    void            SafeDestroyContext(ffxContext* ctx);

    // ---- state ----
    Fsr4Status m_Status = Fsr4Status::NotInitialized;

    // D3D11
    Microsoft::WRL::ComPtr<ID3D11Device>         m_D3D11Device;
    Microsoft::WRL::ComPtr<ID3D11Device1>        m_D3D11Device1;
    Microsoft::WRL::ComPtr<ID3D11Device5>        m_D3D11Device5;
    Microsoft::WRL::ComPtr<ID3D11DeviceContext>  m_D3D11Context;
    Microsoft::WRL::ComPtr<ID3D11DeviceContext4> m_D3D11Context4;

    // D3D12
    Microsoft::WRL::ComPtr<ID3D12Device>       m_D3D12Device;
    Microsoft::WRL::ComPtr<ID3D12CommandQueue> m_D3D12Queue;
    Microsoft::WRL::ComPtr<ID3D12Fence>        m_D3D12Fence;
    Microsoft::WRL::ComPtr<ID3D11Fence>        m_D3D11Fence;
    UINT64                                     m_FenceValue = 0;
    HANDLE                                     m_FenceEvent = nullptr;

    // Ring из 4 аллокаторов: CPU может убежать на 3 кадра вперёд, что снижает
    // частоту блокировок в BeginFrame() при пиковой GPU-нагрузке.
    static constexpr int kRingSize = 4;
    Microsoft::WRL::ComPtr<ID3D12CommandAllocator>    m_Alloc[kRingSize];
    UINT64                                            m_AllocFence[kRingSize] = {};
    int                                               m_AllocIdx   = 0;
    int                                               m_CurrentIdx = -1;
    Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> m_CmdList;

    // FFX
    HMODULE m_LoaderModule   = nullptr;
    HMODULE m_UpscalerModule = nullptr;
    PfnFfxCreateContext  m_pfnCreateContext  = nullptr;
    PfnFfxDestroyContext m_pfnDestroyContext = nullptr;
    PfnFfxQuery          m_pfnQuery          = nullptr;
    PfnFfxDispatch       m_pfnDispatch       = nullptr;

    bool                 m_VersionsQueried = false;
    bool                 m_VersionsFailed  = false;
    xr_vector<u64>       m_VersionIds;
    xr_vector<xr_string> m_VersionNames;
    u64                  m_VersionIdFsr4   = 0;
    u64                  m_VersionId3x     = 0;
    bool                 m_ForceProvider3x = false;
    bool                 m_ChosenIsFsr4    = false;
    xr_string            m_ActiveVersionName = "none";

    ffxContext                           m_Context = nullptr;
    Microsoft::WRL::ComPtr<ID3D12Device> m_ContextDevice;

    // Shared textures (mirror game-owned D3D11 render targets)
    Fsr4SharedTexture m_Color;
    Fsr4SharedTexture m_Depth;
    Fsr4SharedTexture m_Mvec;
    Fsr4SharedTexture m_Output;
    Fsr4SharedTexture m_Opaque;
    Fsr4SharedTexture m_AfterAlpha;

    // Context sizing / state
    UINT m_CtxOutW    = 0;
    UINT m_CtxOutH    = 0;
    UINT m_CtxRenderW = 0;
    UINT m_CtxRenderH = 0;
    int  m_CtxFlags   = -1;
    bool m_FirstDispatch = true;

    // Reactive mask (D3D12-only, not shared with D3D11)
    Microsoft::WRL::ComPtr<ID3D12Resource> m_ReactiveMask;
    UINT                                   m_ReactiveW = 0;
    UINT                                   m_ReactiveH = 0;

    // ---- depth copy pipeline (D24 → R32_FLOAT) ----
    Microsoft::WRL::ComPtr<ID3D11VertexShader>      m_DepthCopyVS;
    Microsoft::WRL::ComPtr<ID3D11PixelShader>       m_DepthCopyPS;
    Microsoft::WRL::ComPtr<ID3D11DepthStencilState> m_DepthCopyDSS;
    Microsoft::WRL::ComPtr<ID3D11BlendState>        m_DepthCopyBS;
    Microsoft::WRL::ComPtr<ID3D11RasterizerState>   m_DepthCopyRS;
    Microsoft::WRL::ComPtr<ID3D11SamplerState>      m_DepthCopySampler;
    Microsoft::WRL::ComPtr<ID3D11Device>            m_DepthCopyDevice;
    bool                                            m_DepthCopyReady = false;

    // ---- CPU-side caches (hot-path optimizations) ----
    struct FrameTextureCache
    {
        ID3D11Resource* colorSrc      = nullptr;
        ID3D11Resource* depthSrc      = nullptr;
        ID3D11Resource* mvecSrc       = nullptr;
        ID3D11Resource* outputSrc     = nullptr;
        ID3D11Resource* opaqueSrc     = nullptr;
        ID3D11Resource* afterAlphaSrc = nullptr;
        FrameTextures   cached;
    };
    FrameTextureCache m_TexCache;

    // Fast-path в EnsureDevices: тот же anyTexture + D3D12-device жив.
    ID3D11Texture2D* m_LastAnyTexture = nullptr;
};