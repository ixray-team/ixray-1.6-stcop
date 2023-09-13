/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

namespace nri
{

struct DeviceD3D11;

struct QueryPoolD3D11
{
    inline QueryPoolD3D11(DeviceD3D11& device) :
        m_Device(device)
        , m_Pool(device.GetStdAllocator())
    {}

    inline ~QueryPoolD3D11()
    {}

    inline DeviceD3D11& GetDevice() const
    { return m_Device; }

    Result Create(const QueryPoolDesc& queryPoolDesc);
    void BeginQuery(const VersionedContext& deferredContext, uint32_t offset);
    void EndQuery(const VersionedContext& deferredContext, uint32_t offset);
    void GetData(uint8_t* dstMemory, uint32_t offset, uint32_t num) const;

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);
    uint32_t GetQuerySize() const;

private:
    DeviceD3D11& m_Device;
    Vector<ComPtr<ID3D11Query>> m_Pool;
    QueryType m_Type = QueryType::MAX_NUM;
};

}
