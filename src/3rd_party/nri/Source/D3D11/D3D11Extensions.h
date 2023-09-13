#pragma once

#include "AGS/inc/amd_ags.h"
#include "NVAPI/nvapi.h"

typedef AGSReturnCode (*PFN_agsInit)(AGSContext** agsContext, const AGSConfiguration* config, AGSGPUInfo* gpuInfo);
typedef AGSReturnCode (*PFN_agsDeInit)(AGSContext* agsContext);
typedef AGSReturnCode (*PFN_agsDriverExtensionsDX11_CreateDevice)(AGSContext* agsContext, const AGSDX11DeviceCreationParams* creationParams, const AGSDX11ExtensionParams* extensionParams, AGSDX11ReturnedParams* returnedParams);
typedef AGSReturnCode (*PFN_agsDriverExtensionsDX11_BeginUAVOverlap)(AGSContext* agsContext, ID3D11DeviceContext* dxContext);
typedef AGSReturnCode (*PFN_agsDriverExtensionsDX11_EndUAVOverlap)(AGSContext* agsContext, ID3D11DeviceContext* dxContext);
typedef AGSReturnCode (*PFN_agsDriverExtensionsDX11_MultiDrawIndexedInstancedIndirect)(AGSContext* agsContext, ID3D11DeviceContext* dxContext, unsigned int drawCount, ID3D11Buffer* pBufferForArgs, unsigned int alignedByteOffsetForArgs, unsigned int byteStrideForArgs);
typedef AGSReturnCode (*PFN_agsDriverExtensionsDX11_SetDepthBounds)(AGSContext* agsContext, ID3D11DeviceContext* dxContext, bool enabled, float minDepth, float maxDepth);
typedef AGSReturnCode (*PFN_agsDriverExtensionsDX11_MultiDrawInstancedIndirect)(AGSContext* agsContext, ID3D11DeviceContext* dxContext, unsigned int drawCount, ID3D11Buffer* pBufferForArgs, unsigned int alignedByteOffsetForArgs, unsigned int byteStrideForArgs);

struct AGSFunctionTable
{
    PFN_agsInit Init;
    PFN_agsDeInit DeInit;
    PFN_agsDriverExtensionsDX11_CreateDevice CreateDevice;
    PFN_agsDriverExtensionsDX11_BeginUAVOverlap BeginUAVOverlap;
    PFN_agsDriverExtensionsDX11_EndUAVOverlap EndUAVOverlap;
    PFN_agsDriverExtensionsDX11_MultiDrawIndexedInstancedIndirect MultiDrawIndexedInstancedIndirect;
    PFN_agsDriverExtensionsDX11_SetDepthBounds SetDepthBounds;
    PFN_agsDriverExtensionsDX11_MultiDrawInstancedIndirect MultiDrawInstancedIndirect;
};

struct D3D11Extensions
{
    ~D3D11Extensions();

    constexpr bool IsAvailable() const;
    constexpr bool IsNvAPIAvailable() const;
    constexpr bool IsAGSAvailable() const;

    void Create(const Log& log, nri::Vendor vendor, AGSContext* agsContext, bool isImported);
    void BeginUAVOverlap(const VersionedContext& deferredContext) const;
    void EndUAVOverlap(const VersionedContext& deferredContext) const;
    void WaitForDrain(const VersionedContext& deferredContext, nri::BarrierDependency dependency) const;
    void SetDepthBounds(const VersionedContext& deferredContext, float minBound, float maxBound) const;
    void MultiDrawIndirect(const VersionedContext& deferredContext, ID3D11Buffer* buffer, uint64_t offset, uint32_t drawNum, uint32_t stride) const;
    void MultiDrawIndexedIndirect(const VersionedContext& deferredContext, ID3D11Buffer* buffer, uint64_t offset, uint32_t drawNum, uint32_t stride) const;

    ID3D11Device* CreateDeviceUsingAGS(IDXGIAdapter* adapter, const D3D_FEATURE_LEVEL* featureLevels, const size_t featureLevelNum, UINT flags);

private:
    bool LoadAGS();

    const Log* m_Log = nullptr;
    bool m_IsNvAPIAvailable = false;
    bool m_IsAGSAvailable = false;
    AGSContext* m_AGSContext = nullptr;
    AGSFunctionTable m_AGS = {};
    HMODULE m_AGSLibrary = nullptr;
    bool m_IsImported = false;
};

constexpr bool D3D11Extensions::IsAvailable() const
{
    return m_IsNvAPIAvailable || m_IsAGSAvailable;
}

constexpr bool D3D11Extensions::IsNvAPIAvailable() const
{
    return m_IsNvAPIAvailable;
}
constexpr bool D3D11Extensions::IsAGSAvailable() const
{
    return m_IsAGSAvailable;
}
