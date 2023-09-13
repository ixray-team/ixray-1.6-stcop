/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D11.h"
#include "D3D11Extensions.h"

D3D11Extensions::~D3D11Extensions()
{
    if (m_IsNvAPIAvailable)
        NvAPI_Unload();

    if (m_IsAGSAvailable)
    {
        if (!m_IsImported)
            m_AGS.DeInit(m_AGSContext);
        m_AGSContext = nullptr;

        FreeLibrary(m_AGSLibrary);
        m_AGSLibrary = nullptr;
    }
}

void D3D11Extensions::Create(const Log& log, nri::Vendor vendor, AGSContext* agsContext, bool isImported)
{
    m_Log = &log;

    switch (vendor)
    {
    case nri::Vendor::NVIDIA:
        {
            const NvAPI_Status status = NvAPI_Initialize();
            m_IsNvAPIAvailable = (status == NVAPI_OK);
            if (!m_IsNvAPIAvailable)
                REPORT_ERROR(*m_Log, "Failed to initialize NvAPI: %d", (int32_t)status);
        }
        break;
    case nri::Vendor::AMD:
        {
            if (isImported && !agsContext)
                break;

            if (LoadAGS())
            {
                m_AGSContext = agsContext;
                m_IsImported = isImported;

                if (m_AGSContext == nullptr)
                {
                    const AGSReturnCode result = m_AGS.Init(&m_AGSContext, nullptr, nullptr);

                    if (result != AGS_SUCCESS)
                        REPORT_ERROR(*m_Log, "Failed to initialize AGS: %d", (int32_t)result);
                }

                if (m_AGSContext == nullptr)
                {
                    FreeLibrary(m_AGSLibrary);
                    m_AGSLibrary = nullptr;
                }

                m_IsAGSAvailable = m_AGSContext != nullptr;
            }
        }
        break;
    }
}

bool D3D11Extensions::LoadAGS()
{
    m_AGSLibrary = LoadLibraryW(L"amd_ags_x64");

    if (m_AGSLibrary == nullptr)
        return false;

    m_AGS.Init = (PFN_agsInit)GetProcAddress(m_AGSLibrary, "agsInit");
    m_AGS.DeInit = (PFN_agsDeInit)GetProcAddress(m_AGSLibrary, "agsDeInit");
    m_AGS.CreateDevice = (PFN_agsDriverExtensionsDX11_CreateDevice)GetProcAddress(m_AGSLibrary, "agsDriverExtensionsDX11_CreateDevice");
    m_AGS.BeginUAVOverlap = (PFN_agsDriverExtensionsDX11_BeginUAVOverlap)GetProcAddress(m_AGSLibrary, "agsDriverExtensionsDX11_BeginUAVOverlap");
    m_AGS.EndUAVOverlap = (PFN_agsDriverExtensionsDX11_EndUAVOverlap)GetProcAddress(m_AGSLibrary, "agsDriverExtensionsDX11_EndUAVOverlap");
    m_AGS.MultiDrawIndexedInstancedIndirect = (PFN_agsDriverExtensionsDX11_MultiDrawIndexedInstancedIndirect)GetProcAddress(m_AGSLibrary, "agsDriverExtensionsDX11_MultiDrawIndexedInstancedIndirect");
    m_AGS.SetDepthBounds = (PFN_agsDriverExtensionsDX11_SetDepthBounds)GetProcAddress(m_AGSLibrary, "agsDriverExtensionsDX11_SetDepthBounds");
    m_AGS.MultiDrawInstancedIndirect = (PFN_agsDriverExtensionsDX11_MultiDrawInstancedIndirect)GetProcAddress(m_AGSLibrary, "agsDriverExtensionsDX11_MultiDrawInstancedIndirect");

    const void** functionArray = (const void**)&m_AGS;
    const size_t functionArraySize = sizeof(AGSFunctionTable) / sizeof(void*);
    size_t i = 0;
    for (; i < functionArraySize && functionArray[i] != nullptr; i++);

    if (i != functionArraySize)
    {
        REPORT_ERROR(*m_Log, "Failed to get function address from 'amd_ags_x64.dll'.");
        FreeLibrary(m_AGSLibrary);
        m_AGSLibrary = nullptr;
        return false;
    }

    return true;
}

void D3D11Extensions::BeginUAVOverlap(const VersionedContext& deferredContext) const
{
    if (m_IsNvAPIAvailable)
    {
        const NvAPI_Status res = NvAPI_D3D11_BeginUAVOverlap(deferredContext.ptr);
        CHECK(*m_Log, res == NVAPI_OK, "NvAPI_D3D11_BeginUAVOverlap() - FAILED!");
    }
    else if (m_IsAGSAvailable)
    {
        const AGSReturnCode res = m_AGS.BeginUAVOverlap(m_AGSContext, deferredContext.ptr);
        CHECK(*m_Log, res == AGS_SUCCESS, "agsDriverExtensionsDX11_BeginUAVOverlap() - FAILED!");
    }
}

void D3D11Extensions::EndUAVOverlap(const VersionedContext& deferredContext) const
{
    if (m_IsNvAPIAvailable)
    {
        const NvAPI_Status status = NvAPI_D3D11_EndUAVOverlap(deferredContext.ptr);
        CHECK(*m_Log, status == NVAPI_OK, "NvAPI_D3D11_EndUAVOverlap() - FAILED!");
    }
    else if (m_IsAGSAvailable)
    {
        const AGSReturnCode res = m_AGS.EndUAVOverlap(m_AGSContext, deferredContext.ptr);
        CHECK(*m_Log, res == AGS_SUCCESS, "agsDriverExtensionsDX11_EndUAVOverlap() - FAILED!");
    }
}

void D3D11Extensions::WaitForDrain(const VersionedContext& deferredContext, nri::BarrierDependency dependency) const
{
    if (m_IsNvAPIAvailable)
    {
        uint32_t flags;

        if (dependency == nri::BarrierDependency::GRAPHICS_STAGE)
            flags = NVAPI_D3D_BEGIN_UAV_OVERLAP_GFX_WFI;
        else if (dependency == nri::BarrierDependency::COMPUTE_STAGE)
            flags = NVAPI_D3D_BEGIN_UAV_OVERLAP_COMP_WFI;
        else
            flags = NVAPI_D3D_BEGIN_UAV_OVERLAP_GFX_WFI | NVAPI_D3D_BEGIN_UAV_OVERLAP_COMP_WFI;

        const NvAPI_Status res = NvAPI_D3D11_BeginUAVOverlapEx(deferredContext.ptr, flags);
        CHECK(*m_Log, res == NVAPI_OK, "NvAPI_D3D11_BeginUAVOverlap() - FAILED!");
    }
    else if (m_IsAGSAvailable)
    {
        REPORT_WARNING(*m_Log, "Verify that this code actually works on AMD!");

        const AGSReturnCode res1 = m_AGS.EndUAVOverlap(m_AGSContext, deferredContext.ptr);
        CHECK(*m_Log, res1 == AGS_SUCCESS, "agsDriverExtensionsDX11_EndUAVOverlap() - FAILED!");
        const AGSReturnCode res2 = m_AGS.BeginUAVOverlap(m_AGSContext, deferredContext.ptr);
        CHECK(*m_Log, res2 == AGS_SUCCESS, "agsDriverExtensionsDX11_BeginUAVOverlap() - FAILED!");
    }
}

void D3D11Extensions::SetDepthBounds(const VersionedContext& deferredContext, float minBound, float maxBound) const
{
    bool isEnabled = minBound != 0.0f || maxBound != 1.0f;

    if (m_IsNvAPIAvailable)
    {
        const NvAPI_Status status = NvAPI_D3D11_SetDepthBoundsTest(deferredContext.ptr, isEnabled, minBound, maxBound);
        CHECK(*m_Log, status == NVAPI_OK, "NvAPI_D3D11_SetDepthBoundsTest() - FAILED!");
    }
    else if (m_IsAGSAvailable)
    {
        const AGSReturnCode res = m_AGS.SetDepthBounds(m_AGSContext, deferredContext.ptr, isEnabled, minBound, maxBound);
        CHECK(*m_Log, res == AGS_SUCCESS, "agsDriverExtensionsDX11_SetDepthBounds() - FAILED!");
    }
}

void D3D11Extensions::MultiDrawIndirect(const VersionedContext& deferredContext, ID3D11Buffer* buffer, uint64_t offset, uint32_t drawNum, uint32_t stride) const
{
    if (m_IsNvAPIAvailable)
    {
        const NvAPI_Status status = NvAPI_D3D11_MultiDrawInstancedIndirect(deferredContext.ptr, drawNum, buffer, (uint32_t)offset, stride);
        CHECK(*m_Log, status == NVAPI_OK, "NvAPI_D3D11_MultiDrawInstancedIndirect() - FAILED!");
    }
    else if (m_IsAGSAvailable)
    {
        const AGSReturnCode res = m_AGS.MultiDrawInstancedIndirect(m_AGSContext, deferredContext.ptr, drawNum, buffer, (uint32_t)offset, stride);
        CHECK(*m_Log, res == AGS_SUCCESS, "agsDriverExtensionsDX11_MultiDrawIndexedInstancedIndirect() - FAILED!");
    }
    else
    {
        for (uint32_t i = 0; i < drawNum; i++)
        {
            deferredContext->DrawInstancedIndirect(buffer, (uint32_t)offset);
            offset += stride;
        }
    }
}

void D3D11Extensions::MultiDrawIndexedIndirect(const VersionedContext& deferredContext, ID3D11Buffer* buffer, uint64_t offset, uint32_t drawNum, uint32_t stride) const
{
    if (m_IsNvAPIAvailable)
    {
        const NvAPI_Status status = NvAPI_D3D11_MultiDrawIndexedInstancedIndirect(deferredContext.ptr, drawNum, buffer, (uint32_t)offset, stride);
        CHECK(*m_Log, status == NVAPI_OK, "NvAPI_D3D11_MultiDrawInstancedIndirect() - FAILED!");
    }
    else if (m_IsAGSAvailable)
    {
        const AGSReturnCode res = m_AGS.MultiDrawIndexedInstancedIndirect(m_AGSContext, deferredContext.ptr, drawNum, buffer, (uint32_t)offset, stride);
        CHECK(*m_Log, res == AGS_SUCCESS, "agsDriverExtensionsDX11_MultiDrawIndexedInstancedIndirect() - FAILED!");
    }
    else
    {
        for (uint32_t i = 0; i < drawNum; i++)
        {
            deferredContext->DrawIndexedInstancedIndirect(buffer, (uint32_t)offset);
            offset += stride;
        }
    }
}

ID3D11Device* D3D11Extensions::CreateDeviceUsingAGS(IDXGIAdapter* adapter, const D3D_FEATURE_LEVEL* featureLevels, const size_t featureLevelNum, UINT flags)
{
    CHECK(*m_Log, m_IsAGSAvailable, "Can't create a device using AGS: AGS is not available.");

    AGSDX11DeviceCreationParams deviceCreationParams = {};
    deviceCreationParams.pAdapter = adapter;
    deviceCreationParams.DriverType = D3D_DRIVER_TYPE_UNKNOWN;
    deviceCreationParams.Flags = flags;
    deviceCreationParams.pFeatureLevels = featureLevels;
    deviceCreationParams.FeatureLevels = (uint32_t)featureLevelNum;
    deviceCreationParams.SDKVersion = D3D11_SDK_VERSION;

    AGSDX11ExtensionParams extensionsParams = {};
    extensionsParams.uavSlot = 63;

    AGSDX11ReturnedParams returnedParams = {};
    AGSReturnCode result = m_AGS.CreateDevice(m_AGSContext, &deviceCreationParams, &extensionsParams, &returnedParams);

    if (flags != 0 && result != AGS_SUCCESS)
    {
        deviceCreationParams.Flags = 0;
        result = m_AGS.CreateDevice(m_AGSContext, &deviceCreationParams, &extensionsParams, &returnedParams);
    }

    RETURN_ON_FAILURE(*m_Log, result == AGS_SUCCESS, nullptr, "agsDriverExtensionsDX11_CreateDevice() failed: %d", (int32_t)result);

    return returnedParams.pDevice;
}