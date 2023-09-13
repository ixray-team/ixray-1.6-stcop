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
struct MemoryD3D11;
struct QueryPoolD3D11;
struct TextureD3D11;

struct QueryRange
{
    const QueryPoolD3D11* pool;
    uint32_t offset;
    uint32_t num;
    uint64_t bufferOffset;
};

struct BufferD3D11
{
    inline BufferD3D11(DeviceD3D11& device, const BufferDesc& bufferDesc) :
        m_Device(device)
        , m_Desc(bufferDesc)
    {}

    inline BufferD3D11(DeviceD3D11& device) :
        m_Device(device)
    {}

    inline operator ID3D11Buffer*() const
    { return m_Buffer; }

    inline const BufferDesc& GetDesc() const
    { return m_Desc; }

    inline DeviceD3D11& GetDevice() const
    { return m_Device; }

    inline void AssignQueryPoolRange(const QueryPoolD3D11* queryPool, uint32_t offset, uint32_t num, uint64_t bufferOffset)
    {
        m_QueryRange.pool = queryPool;
        m_QueryRange.offset = offset;
        m_QueryRange.num = num;
        m_QueryRange.bufferOffset = bufferOffset;
    }

    ~BufferD3D11();

    Result Create(const MemoryD3D11& memory);
    Result Create(const BufferD3D11Desc& bufferDesc);
    void* Map(MapType mapType, uint64_t offset);
    void FinalizeQueries();
    void FinalizeReadback();
    TextureD3D11& RecreateReadbackTexture(const TextureD3D11& srcTexture, const TextureRegionDesc& srcRegionDesc, const TextureDataLayoutDesc& readbackDataLayoutDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { SET_D3D_DEBUG_OBJECT_NAME(m_Buffer, name); }

    void GetMemoryInfo(MemoryLocation memoryLocation, MemoryDesc& memoryDesc) const;
    void* Map(uint64_t offset, uint64_t size);
    void Unmap();

private:
    DeviceD3D11& m_Device;
    ComPtr<ID3D11Buffer> m_Buffer;
    TextureD3D11* m_ReadbackTexture = nullptr;
    BufferDesc m_Desc = {};
    BufferType m_Type = BufferType::DEVICE;
    QueryRange m_QueryRange = {};
    bool m_IsReadbackDataChanged = false;
    TextureDataLayoutDesc m_ReadbackDataLayoutDesc = {};
};

}
