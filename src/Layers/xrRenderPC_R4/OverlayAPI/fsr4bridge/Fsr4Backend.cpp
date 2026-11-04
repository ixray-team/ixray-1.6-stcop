#include "stdafx.h"
#include "Fsr4Backend.h"
#include "FidelityFX/api/include/dx12/ffx_api_dx12.h"
#include <d3dcompiler.h>

// ===========================================================================
// Construction
// ===========================================================================
Fsr4Backend::Fsr4Backend()  = default;
Fsr4Backend::~Fsr4Backend() { Shutdown(); }

bool Fsr4Backend::Initialize(ID3D11Device* d3d11Device, const wchar_t* ffxDllDir)
{
    Shutdown();
    if (!d3d11Device || !ffxDllDir)
        return false;

    m_D3D11Device = d3d11Device;
    m_D3D11Device->GetImmediateContext(&m_D3D11Context);

    if (FAILED(m_D3D11Device.As(&m_D3D11Device1)) ||
        FAILED(m_D3D11Device.As(&m_D3D11Device5)) ||
        FAILED(m_D3D11Context.As(&m_D3D11Context4)))
    {
        Msg("[FSR4] D3D11.4 interfaces unavailable (Windows 10 1709+ required)");
        m_Status = Fsr4Status::InteropUnsupported;
        return false;
    }

    if (!LoadFfxLibrary(ffxDllDir))
        return false;

    m_Status = Fsr4Status::NotInitialized;
    Msg("[FSR4] Backend initialized (lazy device/context).");
    return true;
}

void Fsr4Backend::Shutdown()
{
    WaitIdle();
    ReleaseUpscalerState();
    ReleaseDepthCopyPipeline();
    ReleaseCommandRing();
    ReleaseDevices();

    m_D3D11Context4.Reset();
    m_D3D11Context.Reset();
    m_D3D11Device5.Reset();
    m_D3D11Device1.Reset();
    m_D3D11Device.Reset();

    m_pfnCreateContext  = nullptr;
    m_pfnDestroyContext = nullptr;
    m_pfnQuery          = nullptr;
    m_pfnDispatch       = nullptr;

    if (m_LoaderModule)   { FreeLibrary(m_LoaderModule);   m_LoaderModule   = nullptr; }
    if (m_UpscalerModule) { FreeLibrary(m_UpscalerModule); m_UpscalerModule = nullptr; }

    m_Status = Fsr4Status::NotInitialized;
}

// ===========================================================================
// SEH shims
// ===========================================================================
ffxReturnCode_t Fsr4Backend::SafeQuery(ffxContext* ctx, ffxQueryDescHeader* desc)
{
    __try { return m_pfnQuery(ctx, desc); }
    __except (EXCEPTION_EXECUTE_HANDLER) { return Fsr4FfxSehCrashCode; }
}
ffxReturnCode_t Fsr4Backend::SafeCreateContext(ffxContext* ctx, ffxCreateContextDescHeader* desc)
{
    __try { return m_pfnCreateContext(ctx, desc, nullptr); }
    __except (EXCEPTION_EXECUTE_HANDLER) { return Fsr4FfxSehCrashCode; }
}
ffxReturnCode_t Fsr4Backend::SafeDispatch(ffxContext* ctx, ffxDispatchDescHeader* desc)
{
    __try { return m_pfnDispatch(ctx, desc); }
    __except (EXCEPTION_EXECUTE_HANDLER) { return Fsr4FfxSehCrashCode; }
}
void Fsr4Backend::SafeDestroyContext(ffxContext* ctx)
{
    __try { m_pfnDestroyContext(ctx, nullptr); }
    __except (EXCEPTION_EXECUTE_HANDLER) { }
}

// ===========================================================================
// FFX library loading & versions
// ===========================================================================
static HMODULE LoadFromDir(const wchar_t* dir, const wchar_t* name)
{
    wchar_t path[MAX_PATH];
    swprintf_s(path, L"%s\\%s", dir, name);
    return LoadLibraryExW(path, nullptr, LOAD_WITH_ALTERED_SEARCH_PATH);
}

static bool ResolveFrom(HMODULE mod,
                        PfnFfxCreateContext&  create,
                        PfnFfxDestroyContext& destroy,
                        PfnFfxQuery&          query,
                        PfnFfxDispatch&       dispatch)
{
    if (!mod) return false;
    create   = (PfnFfxCreateContext) GetProcAddress(mod, "ffxCreateContext");
    destroy  = (PfnFfxDestroyContext)GetProcAddress(mod, "ffxDestroyContext");
    query    = (PfnFfxQuery)         GetProcAddress(mod, "ffxQuery");
    dispatch = (PfnFfxDispatch)      GetProcAddress(mod, "ffxDispatch");
    return create && destroy && query && dispatch;
}

bool Fsr4Backend::LoadFfxLibrary(const wchar_t* dllDir)
{
    if (m_pfnCreateContext) return true;

    m_UpscalerModule = LoadFromDir(dllDir, L"amd_fidelityfx_upscaler_dx12.dll");
    m_LoaderModule   = LoadFromDir(dllDir, L"amd_fidelityfx_loader_dx12.dll");
    if (!m_LoaderModule)
    {
        Msg("[FSR4] FAILED to load amd_fidelityfx_loader_dx12.dll (err %lu)", GetLastError());
        m_Status = Fsr4Status::LoaderMissing;
        return false;
    }

    if (!ResolveFrom(m_LoaderModule, m_pfnCreateContext, m_pfnDestroyContext,
                     m_pfnQuery, m_pfnDispatch) &&
        !ResolveFrom(m_UpscalerModule, m_pfnCreateContext, m_pfnDestroyContext,
                     m_pfnQuery, m_pfnDispatch))
    {
        Msg("[FSR4] ffx-api entry points not found");
        m_Status = Fsr4Status::EntrypointsMissing;
        return false;
    }
    return true;
}

void Fsr4Backend::QueryVersionsOnce()
{
    if (m_VersionsQueried || !m_D3D12Device) return;
    m_VersionsQueried = true;

    ffxQueryDescGetVersions q = {};
    q.header.type    = FFX_API_QUERY_DESC_TYPE_GET_VERSIONS;
    q.createDescType = FFX_API_CREATE_CONTEXT_DESC_TYPE_UPSCALE;
    q.device         = m_D3D12Device.Get();
    u64 count        = 0;
    q.outputCount    = &count;

    ffxReturnCode_t rc = SafeQuery(nullptr, &q.header);
    if (rc != FFX_API_RETURN_OK || count == 0)
    {
        Msg("[FSR4] Version query failed (rc=0x%X, count=%llu)", rc, (unsigned long long)count);
        m_VersionsFailed = true;
        return;
    }

    xr_vector<u64>         ids(count);
    xr_vector<const char*> names(count);
    q.versionIds   = ids.data();
    q.versionNames = names.data();
    if (SafeQuery(nullptr, &q.header) != FFX_API_RETURN_OK)
    {
        Msg("[FSR4] Version id/name query failed");
        m_VersionsFailed = true;
        return;
    }

    m_VersionIds.clear();
    m_VersionNames.clear();

    int best4 = -1, best3 = -1;
    for (u64 i = 0; i < count; ++i)
    {
        const char* name = names[i] ? names[i] : "?";
        m_VersionIds.push_back(ids[i]);
        m_VersionNames.emplace_back(name);

        const char* p = name;
        while (*p && (*p < '0' || *p > '9')) ++p;
        const bool isV4 = (p[0] == '4' && p[1] == '.');
        const bool isV3 = (p[0] == '3' && p[1] == '.');
        if (isV4 && (best4 < 0 || strcmp(name, m_VersionNames[best4].c_str()) > 0)) best4 = (int)i;
        if (isV3 && (best3 < 0 || strcmp(name, m_VersionNames[best3].c_str()) > 0)) best3 = (int)i;
    }
    m_VersionIdFsr4 = best4 >= 0 ? m_VersionIds[best4] : 0;
    m_VersionId3x   = best3 >= 0 ? m_VersionIds[best3] : 0;
    Msg("[FSR4] Providers: FSR4=%s, 3.1.x=%s",
            best4 >= 0 ? m_VersionNames[best4].c_str() : "(none)",
            best3 >= 0 ? m_VersionNames[best3].c_str() : "(none)");
}

u64 Fsr4Backend::PickProviderVersionId(bool preferFsr4)
{
    if (preferFsr4 && !m_ForceProvider3x && m_VersionIdFsr4 != 0)
    {
        m_ChosenIsFsr4 = true;
        return m_VersionIdFsr4;
    }
    m_ChosenIsFsr4 = false;
    return m_VersionId3x;
}

u32 Fsr4Backend::TranslateFfxFlags(int ourFlags)
{
    u32 out = 0;
    if (ourFlags & Fsr4Flag_Hdr)            out |= FFX_UPSCALE_ENABLE_HIGH_DYNAMIC_RANGE;
    if (ourFlags & Fsr4Flag_DepthInverted)  out |= FFX_UPSCALE_ENABLE_DEPTH_INVERTED;
    if (ourFlags & Fsr4Flag_DepthInfinite)  out |= FFX_UPSCALE_ENABLE_DEPTH_INFINITE;
    if (ourFlags & Fsr4Flag_AutoExposure)   out |= FFX_UPSCALE_ENABLE_AUTO_EXPOSURE;
    if (ourFlags & Fsr4Flag_DebugChecking)  out |= FFX_UPSCALE_ENABLE_DEBUG_CHECKING;
    if (ourFlags & Fsr4Flag_DynamicRes)     out |= FFX_UPSCALE_ENABLE_DYNAMIC_RESOLUTION;
    if (ourFlags & Fsr4Flag_MvJitterCancel) out |= FFX_UPSCALE_ENABLE_MOTION_VECTORS_JITTER_CANCELLATION;
    return out;
}

// ===========================================================================
// Device management
// ===========================================================================
bool Fsr4Backend::CreateD3D12Device(IDXGIAdapter* adapter)
{
    Microsoft::WRL::ComPtr<ID3D12SDKConfiguration1> sdkConfig;
    if (SUCCEEDED(D3D12GetInterface(CLSID_D3D12SDKConfiguration, IID_PPV_ARGS(&sdkConfig))))
    {
        Microsoft::WRL::ComPtr<ID3D12DeviceFactory> factory;
        if (SUCCEEDED(sdkConfig->CreateDeviceFactory(616, ".\\", IID_PPV_ARGS(&factory))) && factory)
        {
            if (SUCCEEDED(factory->CreateDevice(adapter, D3D_FEATURE_LEVEL_12_0,
                                                IID_PPV_ARGS(&m_D3D12Device))) &&
                m_D3D12Device)
            {
                Msg("[FSR4] D3D12 device via Agility SDK v616");
                return true;
            }
            Msg("[FSR4] Agility CreateDevice failed — falling back to OS D3D12");
        }
    }
    return SUCCEEDED(D3D12CreateDevice(adapter, D3D_FEATURE_LEVEL_12_0, IID_PPV_ARGS(&m_D3D12Device)));
}

bool Fsr4Backend::ShareFenceToD3D11()
{
    if (FAILED(m_D3D12Device->CreateFence(0, D3D12_FENCE_FLAG_SHARED, IID_PPV_ARGS(&m_D3D12Fence))))
        return false;

    HANDLE handle = nullptr;
    if (FAILED(m_D3D12Device->CreateSharedHandle(m_D3D12Fence.Get(), nullptr, GENERIC_ALL, nullptr, &handle)) ||
        FAILED(m_D3D11Device5->OpenSharedFence(handle, IID_PPV_ARGS(&m_D3D11Fence))))
    {
        if (handle) CloseHandle(handle);
        return false;
    }
    CloseHandle(handle);
    m_FenceEvent = CreateEventW(nullptr, FALSE, FALSE, nullptr);
    return m_FenceEvent != nullptr;
}

bool Fsr4Backend::InitCommandRing()
{
    if (m_CmdList) return true;

    for (int i = 0; i < kRingSize; ++i)
    {
        if (FAILED(m_D3D12Device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT,
                                                         IID_PPV_ARGS(&m_Alloc[i]))))
        {
            Msg("[FSR4] CreateCommandAllocator failed");
            ReleaseCommandRing();
            return false;
        }
    }
    if (FAILED(m_D3D12Device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
                                                m_Alloc[0].Get(), nullptr, IID_PPV_ARGS(&m_CmdList))))
    {
        Msg("[FSR4] CreateCommandList failed");
        ReleaseCommandRing();
        return false;
    }
    m_CmdList->Close();
    m_AllocIdx   = 0;
    m_CurrentIdx = -1;
    return true;
}

void Fsr4Backend::ReleaseCommandRing()
{
    for (int i = 0; i < kRingSize; ++i)
    {
        m_Alloc[i].Reset();
        m_AllocFence[i] = 0;
    }
    m_CmdList.Reset();
    m_AllocIdx   = 0;
    m_CurrentIdx = -1;
}

void Fsr4Backend::ReleaseDevices()
{
    m_LastAnyTexture = nullptr;
    m_D3D11Fence.Reset();
    m_D3D12Fence.Reset();
    m_D3D12Queue.Reset();
    m_D3D12Device.Reset();
    if (m_FenceEvent) { CloseHandle(m_FenceEvent); m_FenceEvent = nullptr; }
    m_FenceValue = 0;
    m_VersionsQueried = false;
    m_VersionsFailed  = false;
}

void Fsr4Backend::ResetSharedTextures()
{
    m_Color.Destroy();
    m_Depth.Destroy();
    m_Mvec.Destroy();
    m_Output.Destroy();
    m_Opaque.Destroy();
    m_AfterAlpha.Destroy();
}

void Fsr4Backend::ReleaseUpscalerState()
{
    DestroyContext();
    DestroyReactiveMask();
    ResetSharedTextures();
    m_TexCache = {};
    m_ContextDevice.Reset();
    m_CtxOutW = m_CtxOutH = m_CtxRenderW = m_CtxRenderH = 0;
    m_CtxFlags = -1;
    m_FirstDispatch = true;
}

bool Fsr4Backend::EnsureDevices(ID3D11Texture2D* anyTexture)
{
    // Fast path: тот же anyTexture + D3D12-device жив → пропускаем GetDevice().
    if (anyTexture == m_LastAnyTexture && m_D3D12Device &&
        !FAILED(m_D3D12Device->GetDeviceRemovedReason()))
    {
        return true;
    }

    Microsoft::WRL::ComPtr<ID3D11Device> currentDevice;
    anyTexture->GetDevice(&currentDevice);
    m_LastAnyTexture = anyTexture;

    const HRESULT devReason    = m_D3D12Device ? m_D3D12Device->GetDeviceRemovedReason() : S_OK;
    const bool    deviceLost   = FAILED(devReason);
    const bool    d3d11Changed = m_D3D12Device && currentDevice.Get() != m_D3D11Device.Get();

    if (m_D3D12Device && !deviceLost && !d3d11Changed)
        return true;

    if (m_D3D12Device)
    {
        if (deviceLost) Msg("[FSR4] D3D12 device REMOVED (0x%08lX) — full rebuild", (unsigned long)devReason);
        else            Msg("[FSR4] D3D11 device changed — full rebuild");

        if (!deviceLost) WaitIdle();

        ReleaseUpscalerState();
        ReleaseDepthCopyPipeline();
        ReleaseCommandRing();
        ReleaseDevices();
    }

    if (currentDevice.Get() != m_D3D11Device.Get())
    {
        m_D3D11Device = currentDevice;
        m_D3D11Device->GetImmediateContext(&m_D3D11Context);
        if (FAILED(m_D3D11Device.As(&m_D3D11Device1)) ||
            FAILED(m_D3D11Device.As(&m_D3D11Device5)) ||
            FAILED(m_D3D11Context.As(&m_D3D11Context4)))
        {
            Msg("[FSR4] D3D11.4 interfaces unavailable");
            m_Status = Fsr4Status::InteropUnsupported;
            return false;
        }
    }

    Microsoft::WRL::ComPtr<IDXGIDevice>  dxgiDevice;
    Microsoft::WRL::ComPtr<IDXGIAdapter> adapter;
    if (FAILED(m_D3D11Device.As(&dxgiDevice)) ||
        FAILED(dxgiDevice->GetAdapter(&adapter)) ||
        !CreateD3D12Device(adapter.Get()))
    {
        Msg("[FSR4] Failed to create matching D3D12 device");
        m_Status = Fsr4Status::DeviceFailed;
        ReleaseDevices();
        return false;
    }

    D3D12_COMMAND_QUEUE_DESC qd = {};
    qd.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
    if (FAILED(m_D3D12Device->CreateCommandQueue(&qd, IID_PPV_ARGS(&m_D3D12Queue))) || !ShareFenceToD3D11())
    {
        Msg("[FSR4] Failed to create D3D12 queue or share fence");
        m_Status = Fsr4Status::InteropUnsupported;
        ReleaseDevices();
        return false;
    }

    if (!InitCommandRing())
    {
        m_Status = Fsr4Status::DeviceFailed;
        ReleaseDevices();
        return false;
    }

    DXGI_ADAPTER_DESC ad = {};
    adapter->GetDesc(&ad);
    Msg("[FSR4] D3D12 device ready: %ws", ad.Description);
    m_Status = Fsr4Status::NotInitialized;
    return true;
}

bool Fsr4Backend::IsDeviceLost() const
{
    return m_D3D12Device && FAILED(m_D3D12Device->GetDeviceRemovedReason());
}

// ===========================================================================
// FFX context
// ===========================================================================
void Fsr4Backend::DestroyContext()
{
    if (m_Context)
    {
        SafeDestroyContext(&m_Context);
        m_Context = nullptr;
    }
}

bool Fsr4Backend::NeedsContextRebuild(const Fsr4DrawParameters& p) const
{
    if (!m_Context) return true;
    if (m_ContextDevice.Get() != m_D3D12Device.Get()) return true;

    const int ctxFlags = p.flags & ~Fsr4PerDispatchFlagsMask;
    if (m_CtxOutW != p.displayWidth || m_CtxOutH != p.displayHeight) return true;
    if (m_CtxFlags != ctxFlags) return true;

    const bool renderSizeChanged = (m_CtxRenderW != p.renderWidth || m_CtxRenderH != p.renderHeight);
    return renderSizeChanged && !(ctxFlags & Fsr4Flag_DynamicRes);
}

bool Fsr4Backend::EnsureContext(const Fsr4DrawParameters& p)
{
    WaitIdle();
    DestroyContext();

    QueryVersionsOnce();
    if (m_VersionsFailed) { m_Status = Fsr4Status::NoVersions; return false; }

    const int      ctxFlags   = p.flags & ~Fsr4PerDispatchFlagsMask;
    const bool     dynamicRes = (ctxFlags & Fsr4Flag_DynamicRes) != 0;
    const bool     preferFsr4 = (ctxFlags & Fsr4Flag_PreferFsr4) != 0;
    const u32      ffxFlags   = TranslateFfxFlags(ctxFlags);

    const UINT maxRenderW  = dynamicRes ? p.displayWidth  : p.renderWidth;
    const UINT maxRenderH  = dynamicRes ? p.displayHeight : p.renderHeight;
    const UINT maxUpscaleW = p.displayWidth;
    const UINT maxUpscaleH = p.displayHeight;

    ffxCreateContextDescUpscale desc = {};
    desc.header.type    = FFX_API_CREATE_CONTEXT_DESC_TYPE_UPSCALE;
    desc.flags          = ffxFlags;
    desc.maxRenderSize  = { maxRenderW, maxRenderH };
    desc.maxUpscaleSize = { maxUpscaleW, maxUpscaleH };

    ffxCreateBackendDX12Desc backend = {};
    backend.header.type = FFX_API_CREATE_CONTEXT_DESC_TYPE_BACKEND_DX12;
    backend.device      = m_D3D12Device.Get();
    desc.header.pNext   = &backend.header;

    ffxCreateContextDescUpscaleVersion apiVersion = {};
    apiVersion.header.type = FFX_API_CREATE_CONTEXT_DESC_TYPE_UPSCALE_VERSION;
    apiVersion.version     = FFX_UPSCALER_VERSION;
    backend.header.pNext   = &apiVersion.header;

    ffxOverrideVersion over = {};
    over.header.type = FFX_API_DESC_TYPE_OVERRIDE_VERSION;

    u64 overrideId = PickProviderVersionId(preferFsr4);
    if (overrideId != 0)
    {
        over.versionId          = overrideId;
        apiVersion.header.pNext = &over.header;
    }

    ffxReturnCode_t rc = SafeCreateContext(&m_Context, &desc.header);

    if (rc == Fsr4FfxSehCrashCode && m_ChosenIsFsr4 && !m_ForceProvider3x)
    {
        Msg("[FSR4] FSR4 context creation crashed — forcing 3.1.x");
        m_ForceProvider3x = true;
        overrideId = PickProviderVersionId(preferFsr4);
        over.versionId = overrideId;
        apiVersion.header.pNext = overrideId != 0 ? &over.header : nullptr;
        rc = SafeCreateContext(&m_Context, &desc.header);
    }
    else if (rc != FFX_API_RETURN_OK && rc != Fsr4FfxSehCrashCode && overrideId != 0)
    {
        Msg("[FSR4] Override create failed (rc=0x%X) — retrying default", rc);
        apiVersion.header.pNext = nullptr;
        m_ChosenIsFsr4 = false;
        rc = SafeCreateContext(&m_Context, &desc.header);
    }

    if (rc != FFX_API_RETURN_OK)
    {
        Msg("[FSR4] ffxCreateContext failed (rc=0x%X)", rc);
        m_Context = nullptr;
        m_Status  = Fsr4Status::ContextFailed;
        return false;
    }

    ffxQueryGetProviderVersion pv = {};
    pv.header.type = FFX_API_QUERY_DESC_TYPE_GET_PROVIDER_VERSION;
    if (SafeQuery(&m_Context, &pv.header) == FFX_API_RETURN_OK && pv.versionName)
    {
        m_ActiveVersionName = pv.versionName;
        Msg("[FSR4] Context created: %s", pv.versionName);
    }

    m_CtxOutW       = p.displayWidth;
    m_CtxOutH       = p.displayHeight;
    m_CtxRenderW    = p.renderWidth;
    m_CtxRenderH    = p.renderHeight;
    m_CtxFlags      = ctxFlags;
    m_ContextDevice = m_D3D12Device;
    m_FirstDispatch = true;
    return true;
}

// ===========================================================================
// Reactive mask
// ===========================================================================
bool Fsr4Backend::EnsureReactiveMask(UINT w, UINT h)
{
    if (m_ReactiveMask && m_ReactiveW == w && m_ReactiveH == h)
        return true;
    m_ReactiveMask.Reset();

    D3D12_HEAP_PROPERTIES heap = {};
    heap.Type = D3D12_HEAP_TYPE_DEFAULT;
    D3D12_RESOURCE_DESC rd = {};
    rd.Dimension        = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
    rd.Width            = w;
    rd.Height           = h;
    rd.DepthOrArraySize = 1;
    rd.MipLevels        = 1;
    rd.Format           = DXGI_FORMAT_R8_UNORM;
    rd.SampleDesc       = {1, 0};
    rd.Layout           = D3D12_TEXTURE_LAYOUT_UNKNOWN;
    rd.Flags            = D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS;

    HRESULT hr = m_D3D12Device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &rd,
                                                        D3D12_RESOURCE_STATE_COMMON, nullptr,
                                                        IID_PPV_ARGS(&m_ReactiveMask));
    if (FAILED(hr))
    {
        Msg("[FSR4] Reactive mask create failed 0x%08lX", hr);
        return false;
    }
    m_ReactiveW = w;
    m_ReactiveH = h;
    return true;
}

void Fsr4Backend::DestroyReactiveMask()
{
    m_ReactiveMask.Reset();
    m_ReactiveW = m_ReactiveH = 0;
}

// ===========================================================================
// Sync
// ===========================================================================
UINT64 Fsr4Backend::SignalD3D11Inputs()
{
    const UINT64 v = ++m_FenceValue;
    m_D3D11Context4->Signal(m_D3D11Fence.Get(), v);

    // Flush обязателен: без него D3D11-драйвер может батчить CopyResource+Signal,
    // и D3D12-очередь простаивает в ожидании fence'а.
    m_D3D11Context->Flush();
    return v;
}

void Fsr4Backend::WaitD3D11Output(UINT64 value)
{
    // GPU-side wait на D3D11 device — не блокирует CPU.
    m_D3D11Context4->Wait(m_D3D11Fence.Get(), value);
}

void Fsr4Backend::WaitIdle()
{
    if (!m_D3D12Queue || !m_D3D12Fence) return;
    const UINT64 v = ++m_FenceValue;
    m_D3D12Queue->Signal(m_D3D12Fence.Get(), v);
    if (m_D3D12Fence->GetCompletedValue() < v)
    {
        m_D3D12Fence->SetEventOnCompletion(v, m_FenceEvent);
        WaitForSingleObject(m_FenceEvent, 5000);
    }
}

// ===========================================================================
// Command list
// ===========================================================================
ID3D12GraphicsCommandList* Fsr4Backend::BeginFrame(UINT64 waitForD3D11Value)
{
    if (!m_CmdList) return nullptr;

    const int idx = m_AllocIdx;
    m_AllocIdx = (m_AllocIdx + 1) % kRingSize;

    if (m_AllocFence[idx] != 0 && m_D3D12Fence->GetCompletedValue() < m_AllocFence[idx])
    {
        m_D3D12Fence->SetEventOnCompletion(m_AllocFence[idx], m_FenceEvent);
        if (WaitForSingleObject(m_FenceEvent, 1000) == WAIT_TIMEOUT)
        {
            Msg("[FSR4] Ring wait timed out — bailing");
            return nullptr;
        }
    }

    if (waitForD3D11Value != 0)
        m_D3D12Queue->Wait(m_D3D12Fence.Get(), waitForD3D11Value);

    if (FAILED(m_Alloc[idx]->Reset()) || FAILED(m_CmdList->Reset(m_Alloc[idx].Get(), nullptr)))
        return nullptr;

    m_CurrentIdx = idx;
    return m_CmdList.Get();
}

void Fsr4Backend::AbortFrame()
{
    if (m_CmdList) m_CmdList->Close();
    m_CurrentIdx = -1;
}

UINT64 Fsr4Backend::EndFrameAndSubmit()
{
    if (!m_CmdList || m_CurrentIdx < 0) return 0;

    m_CmdList->Close();
    ID3D12CommandList* lists[] = { m_CmdList.Get() };
    m_D3D12Queue->ExecuteCommandLists(1, lists);

    const UINT64 done = ++m_FenceValue;
    m_D3D12Queue->Signal(m_D3D12Fence.Get(), done);
    m_AllocFence[m_CurrentIdx] = done;
    m_CurrentIdx = -1;
    return done;
}

ffxReturnCode_t Fsr4Backend::DispatchUpscale(ID3D12GraphicsCommandList* cmd, ffxDispatchDescUpscale* desc)
{
    desc->commandList = cmd;
    return SafeDispatch(&m_Context, &desc->header);
}

ffxReturnCode_t Fsr4Backend::DispatchReactive(ID3D12GraphicsCommandList* cmd, ffxDispatchDescUpscaleGenerateReactiveMask* desc)
{
    desc->commandList = cmd;
    return SafeDispatch(&m_Context, &desc->header);
}

// ===========================================================================
// Depth copy pipeline (D24_UNORM_S8_UINT → R32_FLOAT)
// ===========================================================================
static const char* kDepthCopyHLSL = R"(
struct VSOut { float4 pos : SV_Position; float2 uv : TEXCOORD0; };

VSOut VSMain(uint vid : SV_VertexID)
{
    // Fullscreen triangle без vertex buffer.
    float2 p;
    if (vid == 0)      p = float2(-1.0,  1.0);
    else if (vid == 1) p = float2( 3.0,  1.0);
    else               p = float2(-1.0, -3.0);

    VSOut o;
    o.pos = float4(p, 0.0, 1.0);
    // NDC (-1..+1) → UV (0..1), Y инвертируется (D3D NDC Y=+1 — верх).
    o.uv  = float2((p.x + 1.0) * 0.5, (1.0 - p.y) * 0.5);
    return o;
}

Texture2D<float> DepthTex  : register(t0);
SamplerState     PointSmp  : register(s0);

float4 PSDepthToFloat(VSOut i) : SV_Target
{
    float d = DepthTex.SampleLevel(PointSmp, i.uv, 0);
    return float4(d, 0.0, 0.0, 0.0);
}
)";

bool Fsr4Backend::InitDepthCopyPipeline()
{
    if (m_DepthCopyReady && m_DepthCopyDevice.Get() == m_D3D11Device.Get())
        return true;

    ReleaseDepthCopyPipeline();
    if (!m_D3D11Device)
        return false;

    UINT flags = D3DCOMPILE_OPTIMIZATION_LEVEL3;
#ifdef _DEBUG
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif

    Microsoft::WRL::ComPtr<ID3DBlob> vsBlob, psBlob, errBlob;

    HRESULT hr = D3DCompile(kDepthCopyHLSL, strlen(kDepthCopyHLSL), nullptr, nullptr, nullptr,
                            "VSMain", "vs_5_0", flags, 0, &vsBlob, &errBlob);
    if (FAILED(hr))
    {
        Msg("[FSR4] DepthCopy VS compile failed: %s",
            errBlob ? (const char*)errBlob->GetBufferPointer() : "?");
        return false;
    }

    errBlob.Reset();
    hr = D3DCompile(kDepthCopyHLSL, strlen(kDepthCopyHLSL), nullptr, nullptr, nullptr,
                    "PSDepthToFloat", "ps_5_0", flags, 0, &psBlob, &errBlob);
    if (FAILED(hr))
    {
        Msg("[FSR4] DepthCopy PS compile failed: %s",
            errBlob ? (const char*)errBlob->GetBufferPointer() : "?");
        return false;
    }

    if (FAILED(m_D3D11Device->CreateVertexShader(vsBlob->GetBufferPointer(), vsBlob->GetBufferSize(),
                                                 nullptr, &m_DepthCopyVS)) ||
        FAILED(m_D3D11Device->CreatePixelShader(psBlob->GetBufferPointer(), psBlob->GetBufferSize(),
                                                nullptr, &m_DepthCopyPS)))
    {
        Msg("[FSR4] DepthCopy shader creation failed");
        return false;
    }

    D3D11_DEPTH_STENCIL_DESC dsd = {};
    dsd.DepthEnable    = FALSE;
    dsd.DepthWriteMask = D3D11_DEPTH_WRITE_MASK_ZERO;
    dsd.DepthFunc      = D3D11_COMPARISON_ALWAYS;
    dsd.StencilEnable  = FALSE;
    if (FAILED(m_D3D11Device->CreateDepthStencilState(&dsd, &m_DepthCopyDSS)))
        return false;

    D3D11_BLEND_DESC bd = {};
    bd.RenderTarget[0].BlendEnable           = FALSE;
    bd.RenderTarget[0].RenderTargetWriteMask = D3D11_COLOR_WRITE_ENABLE_ALL;
    if (FAILED(m_D3D11Device->CreateBlendState(&bd, &m_DepthCopyBS)))
        return false;

    D3D11_RASTERIZER_DESC rd = {};
    rd.FillMode        = D3D11_FILL_SOLID;
    rd.CullMode        = D3D11_CULL_NONE;
    rd.DepthClipEnable = FALSE;
    rd.ScissorEnable   = FALSE;
    if (FAILED(m_D3D11Device->CreateRasterizerState(&rd, &m_DepthCopyRS)))
        return false;

    D3D11_SAMPLER_DESC sd = {};
    sd.Filter         = D3D11_FILTER_MIN_MAG_MIP_POINT;
    sd.AddressU       = sd.AddressV = sd.AddressW = D3D11_TEXTURE_ADDRESS_CLAMP;
    sd.ComparisonFunc = D3D11_COMPARISON_NEVER;
    sd.MaxLOD         = D3D11_FLOAT32_MAX;
    if (FAILED(m_D3D11Device->CreateSamplerState(&sd, &m_DepthCopySampler)))
        return false;

    m_DepthCopyDevice = m_D3D11Device;
    m_DepthCopyReady  = true;
    return true;
}

void Fsr4Backend::ReleaseDepthCopyPipeline()
{
    m_DepthCopySampler.Reset();
    m_DepthCopyRS.Reset();
    m_DepthCopyBS.Reset();
    m_DepthCopyDSS.Reset();
    m_DepthCopyPS.Reset();
    m_DepthCopyVS.Reset();
    m_DepthCopyDevice.Reset();
    m_DepthCopyReady = false;
}

bool Fsr4Backend::CopyDepthToShared(ID3D11Texture2D* gameDepth)
{
    if (!gameDepth || !m_Depth.GetD3D11Texture())
        return false;

    if (!InitDepthCopyPipeline())
        return false;

    D3D11_TEXTURE2D_DESC desc = {};
    gameDepth->GetDesc(&desc);

    // ---- SRV на исходный depth (в depth-совместимом формате) ----
    DXGI_FORMAT srvFormat;
    switch (desc.Format)
    {
    case DXGI_FORMAT_R24G8_TYPELESS:
    case DXGI_FORMAT_D24_UNORM_S8_UINT:     srvFormat = DXGI_FORMAT_R24_UNORM_X8_TYPELESS;   break;
    case DXGI_FORMAT_R32G8X24_TYPELESS:
    case DXGI_FORMAT_D32_FLOAT_S8X24_UINT:  srvFormat = DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS; break;
    case DXGI_FORMAT_R32_TYPELESS:
    case DXGI_FORMAT_D32_FLOAT:             srvFormat = DXGI_FORMAT_R32_FLOAT;                break;
    case DXGI_FORMAT_R16_TYPELESS:
    case DXGI_FORMAT_D16_UNORM:             srvFormat = DXGI_FORMAT_R16_UNORM;                break;
    default:                                srvFormat = desc.Format;                          break;
    }

    D3D11_SHADER_RESOURCE_VIEW_DESC srvDesc = {};
    srvDesc.Format                    = srvFormat;
    srvDesc.ViewDimension             = D3D11_SRV_DIMENSION_TEXTURE2D;
    srvDesc.Texture2D.MipLevels       = 1;
    srvDesc.Texture2D.MostDetailedMip = 0;

    Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> srv;
    if (FAILED(m_D3D11Device->CreateShaderResourceView(gameDepth, &srvDesc, &srv)))
    {
        Msg("[FSR4] CopyDepthToShared: SRV failed (src fmt=%d)", desc.Format);
        return false;
    }

    // ---- RTV на shared R32_FLOAT ----
    Microsoft::WRL::ComPtr<ID3D11RenderTargetView> rtv;
    if (FAILED(m_D3D11Device->CreateRenderTargetView(m_Depth.GetD3D11Texture(), nullptr, &rtv)))
    {
        Msg("[FSR4] CopyDepthToShared: RTV failed");
        return false;
    }

    // ---- Сохранить состояние контекста ----
    Microsoft::WRL::ComPtr<ID3D11InputLayout>        savedLayout;
    Microsoft::WRL::ComPtr<ID3D11VertexShader>       savedVS;
    Microsoft::WRL::ComPtr<ID3D11PixelShader>        savedPS;
    Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> savedSRV0;
    Microsoft::WRL::ComPtr<ID3D11SamplerState>       savedSampler0;
    Microsoft::WRL::ComPtr<ID3D11DepthStencilState>  savedDSS;
    Microsoft::WRL::ComPtr<ID3D11BlendState>         savedBS;
    Microsoft::WRL::ComPtr<ID3D11RasterizerState>    savedRS;
    Microsoft::WRL::ComPtr<ID3D11DepthStencilView>   savedDSV;

    UINT          savedStencilRef = 0;
    FLOAT         savedBlendFactor[4] = { 0, 0, 0, 0 };
    UINT          savedSampleMask = 0;
    D3D11_PRIMITIVE_TOPOLOGY savedTopology = D3D11_PRIMITIVE_TOPOLOGY_UNDEFINED;

    ID3D11Buffer* savedVBs[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT] = {};
    UINT          savedStrides[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT] = {};
    UINT          savedOffsets[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT] = {};

    ID3D11RenderTargetView* savedRTVs[D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT] = {};

    UINT         numViewports = D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE;
    D3D11_VIEWPORT savedViewports[D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE] = {};
    UINT         numScissors = numViewports;
    D3D11_RECT   savedScissors[D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE] = {};

    ID3D11DeviceContext* ctx = m_D3D11Context.Get();

    ctx->IAGetInputLayout(&savedLayout);
    ctx->IAGetPrimitiveTopology(&savedTopology);
    ctx->IAGetVertexBuffers(0, D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT,
                            savedVBs, savedStrides, savedOffsets);

    ctx->VSGetShader(&savedVS, nullptr, nullptr);
    ctx->PSGetShader(&savedPS, nullptr, nullptr);
    ctx->PSGetShaderResources(0, 1, &savedSRV0);
    ctx->PSGetSamplers(0, 1, &savedSampler0);

    ctx->OMGetRenderTargets(D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT, savedRTVs, &savedDSV);
    ctx->OMGetDepthStencilState(&savedDSS, &savedStencilRef);
    ctx->OMGetBlendState(&savedBS, savedBlendFactor, &savedSampleMask);

    ctx->RSGetState(&savedRS);
    ctx->RSGetViewports(&numViewports, savedViewports);
    ctx->RSGetScissorRects(&numScissors, savedScissors);

    // ---- Установить состояние прохода ----
    D3D11_VIEWPORT vp = {};
    vp.Width    = (float)desc.Width;
    vp.Height   = (float)desc.Height;
    vp.MaxDepth = 1.0f;
    ctx->RSSetViewports(1, &vp);

    ID3D11ShaderResourceView* nullSRV = nullptr;
    ctx->PSSetShaderResources(0, 1, &nullSRV);

    ID3D11RenderTargetView* rtvRaw = rtv.Get();
    ctx->OMSetRenderTargets(1, &rtvRaw, nullptr);
    ctx->OMSetDepthStencilState(m_DepthCopyDSS.Get(), 0);
    ctx->OMSetBlendState(m_DepthCopyBS.Get(), nullptr, 0xFFFFFFFF);
    ctx->RSSetState(m_DepthCopyRS.Get());

    ctx->VSSetShader(m_DepthCopyVS.Get(), nullptr, 0);
    ctx->PSSetShader(m_DepthCopyPS.Get(), nullptr, 0);

    ID3D11ShaderResourceView* srvRaw = srv.Get();
    ctx->PSSetShaderResources(0, 1, &srvRaw);
    ID3D11SamplerState* smpRaw = m_DepthCopySampler.Get();
    ctx->PSSetSamplers(0, 1, &smpRaw);

    ctx->IASetInputLayout(nullptr);
    ctx->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

    ctx->Draw(3, 0);

    // ---- Восстановить состояние ----
    ctx->IASetInputLayout(savedLayout.Get());
    ctx->IASetPrimitiveTopology(savedTopology);
    ctx->IASetVertexBuffers(0, D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT,
                            savedVBs, savedStrides, savedOffsets);

    ctx->VSSetShader(savedVS.Get(), nullptr, 0);
    ctx->PSSetShader(savedPS.Get(), nullptr, 0);
    ctx->PSSetShaderResources(0, 1, savedSRV0.GetAddressOf());
    ctx->PSSetSamplers(0, 1, savedSampler0.GetAddressOf());

    ctx->OMSetRenderTargets(D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT, savedRTVs, savedDSV.Get());
    ctx->OMSetDepthStencilState(savedDSS.Get(), savedStencilRef);
    ctx->OMSetBlendState(savedBS.Get(), savedBlendFactor, savedSampleMask);
    ctx->RSSetState(savedRS.Get());
    ctx->RSSetViewports(numViewports, savedViewports);
    ctx->RSSetScissorRects(numScissors, savedScissors);

    // Освободить временные ссылки, полученные геттерами.
    for (auto* vb : savedVBs) if (vb) vb->Release();
    for (auto* rt : savedRTVs) if (rt) rt->Release();

    return true;
}

// ===========================================================================
// Frame pipeline
// ===========================================================================
static bool QueryTexture(ID3D11Resource* src, Microsoft::WRL::ComPtr<ID3D11Texture2D>& dst)
{
    return src && SUCCEEDED(((IUnknown*)src)->QueryInterface(IID_PPV_ARGS(&dst)));
}

bool Fsr4Backend::ResolveFrameTextures(const Fsr4DrawParameters& p, FrameTextures& out)
{
    // Fast path: те же входные ресурсы, что и в предыдущем кадре.
    if (m_TexCache.colorSrc      == p.unresolvedColorResource &&
        m_TexCache.depthSrc      == p.depthbufferResource    &&
        m_TexCache.mvecSrc       == p.motionvectorResource   &&
        m_TexCache.outputSrc     == p.resolvedColorResource  &&
        m_TexCache.opaqueSrc     == p.opaqueColorResource    &&
        m_TexCache.afterAlphaSrc == p.afterAlphaColorResource &&
        m_TexCache.cached.color && m_TexCache.cached.depth &&
        m_TexCache.cached.mvec  && m_TexCache.cached.output)
    {
        out = m_TexCache.cached;
        return true;
    }

    FrameTextures t;
    if (!QueryTexture(p.unresolvedColorResource, t.color) ||
        !QueryTexture(p.depthbufferResource,     t.depth) ||
        !QueryTexture(p.motionvectorResource,    t.mvec)  ||
        !QueryTexture(p.resolvedColorResource,   t.output))
    {
        Msg("[FSR4] Failed to resolve frame textures");
        return false;
    }
    if (p.opaqueColorResource)
    {
        QueryTexture(p.opaqueColorResource, t.opaque);
        if (p.afterAlphaColorResource)
            QueryTexture(p.afterAlphaColorResource, t.afterAlpha);
    }

    m_TexCache.colorSrc      = p.unresolvedColorResource;
    m_TexCache.depthSrc      = p.depthbufferResource;
    m_TexCache.mvecSrc       = p.motionvectorResource;
    m_TexCache.outputSrc     = p.resolvedColorResource;
    m_TexCache.opaqueSrc     = p.opaqueColorResource;
    m_TexCache.afterAlphaSrc = p.afterAlphaColorResource;
    m_TexCache.cached        = t;
    out = t;
    return true;
}

bool Fsr4Backend::EnsureSharedTextures(const FrameTextures& tex)
{
    if (!m_Color.Ensure(m_D3D11Device1.Get(), m_D3D12Device.Get(), tex.color.Get(),
                        false, /*isDepthIntermediate=*/false, "color") ||
        !m_Depth.Ensure(m_D3D11Device1.Get(), m_D3D12Device.Get(), tex.depth.Get(),
                        false, /*isDepthIntermediate=*/true,  "depth") ||
        !m_Mvec.Ensure(m_D3D11Device1.Get(), m_D3D12Device.Get(), tex.mvec.Get(),
                        false, /*isDepthIntermediate=*/false, "mvec") ||
        !m_Output.Ensure(m_D3D11Device1.Get(), m_D3D12Device.Get(), tex.output.Get(),
                        true,  /*isDepthIntermediate=*/false, "output"))
    {
        m_Status = IsDeviceLost() ? Fsr4Status::DeviceLost : Fsr4Status::DeviceFailed;
        return false;
    }
    return true;
}

bool Fsr4Backend::PrepareReactiveMask(const Fsr4DrawParameters& p, const FrameTextures& tex)
{
    if (!p.opaqueColorResource || !tex.opaque)
        return false;

    if (!m_Opaque.Ensure(m_D3D11Device1.Get(), m_D3D12Device.Get(), tex.opaque.Get(),
                         false, /*isDepthIntermediate=*/false, "opaque"))
        return false;
    if (tex.afterAlpha &&
        !m_AfterAlpha.Ensure(m_D3D11Device1.Get(), m_D3D12Device.Get(), tex.afterAlpha.Get(),
                             false, /*isDepthIntermediate=*/false, "afterAlpha"))
        return false;

    return EnsureReactiveMask(p.renderWidth, p.renderHeight);
}

void Fsr4Backend::CopyInputsToShared(const FrameTextures& tex, bool reactive)
{
    m_D3D11Context->CopyResource(m_Color.GetD3D11Texture(), tex.color.Get());
    m_D3D11Context->CopyResource(m_Mvec.GetD3D11Texture(),  tex.mvec.Get());

    // depth — через полноэкранный проход (D24 → R32_FLOAT).
    if (!CopyDepthToShared(tex.depth.Get()))
        Msg("[FSR4] CopyDepthToShared failed — FSR4 will receive stale depth");

    if (reactive)
    {
        m_D3D11Context->CopyResource(m_Opaque.GetD3D11Texture(), tex.opaque.Get());
        if (tex.afterAlpha)
            m_D3D11Context->CopyResource(m_AfterAlpha.GetD3D11Texture(), tex.afterAlpha.Get());
    }
}

static FfxApiResource R12(ID3D12Resource* res)
{
    return ffxApiGetResourceDX12(res, FFX_API_RESOURCE_STATE_COMMON);
}

bool Fsr4Backend::RecordAndSubmit(const Fsr4DrawParameters& p, const FrameTextures& tex, bool reactive)
{
    const UINT64 inputReady = SignalD3D11Inputs();
    ID3D12GraphicsCommandList* cmd = BeginFrame(inputReady);
    if (!cmd)
    {
        Msg("[FSR4] BeginFrame failed");
        m_Status = Fsr4Status::DispatchFailed;
        return false;
    }

    bool reactiveReady = false;
    if (reactive)
    {
        ffxDispatchDescUpscaleGenerateReactiveMask gr = {};
        gr.header.type     = FFX_API_DISPATCH_DESC_TYPE_UPSCALE_GENERATEREACTIVEMASK;
        gr.colorOpaqueOnly = R12(m_Opaque.GetD3D12Resource());
        gr.colorPreUpscale = R12((tex.afterAlpha ? m_AfterAlpha : m_Color).GetD3D12Resource());
        gr.outReactive     = R12(m_ReactiveMask.Get());
        gr.renderSize      = { p.renderWidth, p.renderHeight };
        gr.scale           = 1.0f;
        gr.cutoffThreshold = (p.flags & Fsr4Flag_Optic) ? 0.05f : 0.2f;
        gr.binaryValue     = 0.9f;
        gr.flags           = FFX_UPSCALE_AUTOREACTIVEFLAGS_APPLY_TONEMAP
                           | FFX_UPSCALE_AUTOREACTIVEFLAGS_APPLY_THRESHOLD
                           | FFX_UPSCALE_AUTOREACTIVEFLAGS_USE_COMPONENTS_MAX;

        ffxReturnCode_t grc = DispatchReactive(cmd, &gr);
        reactiveReady = (grc == FFX_API_RETURN_OK);
        if (!reactiveReady)
            Msg("[FSR4] Reactive dispatch failed (rc=0x%X) — upscaling without it", grc);
    }

    ffxDispatchDescUpscale d = {};
    d.header.type   = FFX_API_DISPATCH_DESC_TYPE_UPSCALE;
    d.color         = R12(m_Color.GetD3D12Resource());
    d.depth         = R12(m_Depth.GetD3D12Resource());
    d.motionVectors = R12(m_Mvec.GetD3D12Resource());
    d.output        = R12(m_Output.GetD3D12Resource());
    if (reactiveReady)
        d.reactive  = R12(m_ReactiveMask.Get());

    d.jitterOffset      = { p.cameraJitterX, p.cameraJitterY };
    d.motionVectorScale = { -float(p.renderWidth) * 0.5f, float(p.renderHeight) * 0.5f };
    d.renderSize        = { p.renderWidth,  p.renderHeight  };
    d.upscaleSize       = { p.displayWidth, p.displayHeight };
    d.enableSharpening  = p.enableSharpening;
    d.sharpness         = p.sharpness;
    d.frameTimeDelta    = p.frameTimeDelta;
    d.preExposure       = 1.0f;
    d.reset             = m_FirstDispatch || p.cameraReset || (p.flags & Fsr4Flag_Reset);
    d.cameraNear        = p.nearPlane;
    d.cameraFar         = p.farPlane;
    d.cameraFovAngleVertical  = p.fovH;
    d.viewSpaceToMetersFactor = 1.0f;

    const ffxReturnCode_t rc = DispatchUpscale(cmd, &d);
    if (rc != FFX_API_RETURN_OK)
    {
        AbortFrame();
        return HandleDispatchFailure(rc);
    }
    m_FirstDispatch = false;

    const UINT64 done = EndFrameAndSubmit();
    WaitD3D11Output(done);
    return true;
}

void Fsr4Backend::CopyOutputToGame(const FrameTextures& tex)
{
    m_D3D11Context->CopyResource(tex.output.Get(), m_Output.GetD3D11Texture());
}

bool Fsr4Backend::HandleDispatchFailure(ffxReturnCode_t rc)
{
    if (rc == Fsr4FfxSehCrashCode && m_ChosenIsFsr4 && !m_ForceProvider3x)
    {
        Msg("[FSR4] FSR4 dispatch crashed — switching to FSR 3.1.x provider");
        m_ForceProvider3x = true;
        DestroyContext();
        m_CtxFlags = -1;
        m_Status = Fsr4Status::Ok;
        return false;
    }
    Msg("[FSR4] ffxDispatch failed (rc=0x%X)", rc);
    m_Status = Fsr4Status::DispatchFailed;
    return false;
}

// ===========================================================================
// Public entry point
// ===========================================================================
bool Fsr4Backend::DrawFrame(const Fsr4DrawParameters& params)
{
    if (!m_D3D11Device || !m_pfnCreateContext) return false;

    FrameTextures tex;
    if (!ResolveFrameTextures(params, tex))
        return false;

    if (!EnsureDevices(tex.color.Get()))
        return false;

    if (!EnsureSharedTextures(tex))
        return false;

    if (NeedsContextRebuild(params))
    {
        if (!EnsureContext(params))
            return false;
    }

    const bool reactive = PrepareReactiveMask(params, tex);

    CopyInputsToShared(tex, reactive);

    if (!RecordAndSubmit(params, tex, reactive))
        return false;

    CopyOutputToGame(tex);

    if (m_Status == Fsr4Status::DispatchFailed || m_Status == Fsr4Status::NotInitialized)
        m_Status = Fsr4Status::Ok;
    return true;
}