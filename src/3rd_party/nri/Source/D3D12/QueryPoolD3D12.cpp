/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D12.h"
#include "QueryPoolD3D12.h"

using namespace nri;

extern D3D12_QUERY_TYPE GetQueryType(QueryType queryType);
extern D3D12_QUERY_HEAP_TYPE GetQueryHeapType(QueryType queryType);
extern uint32_t GetQueryElementSize(D3D12_QUERY_TYPE queryType);

Result QueryPoolD3D12::Create(const QueryPoolDesc& queryPoolDesc)
{
    m_QueryType = GetQueryType(queryPoolDesc.queryType);

    if (queryPoolDesc.queryType == QueryType::ACCELERATION_STRUCTURE_COMPACTED_SIZE)
        return CreateReadbackBuffer(queryPoolDesc);

    m_QuerySize = GetQueryElementSize(m_QueryType);

    D3D12_QUERY_HEAP_DESC desc;
    desc.Type = GetQueryHeapType(queryPoolDesc.queryType);
    desc.Count = queryPoolDesc.capacity;
    desc.NodeMask = NRI_TEMP_NODE_MASK;

    HRESULT hr = ((ID3D12Device*)m_Device)->CreateQueryHeap(&desc, IID_PPV_ARGS(&m_QueryHeap));
    if (FAILED(hr))
    {
        REPORT_ERROR(m_Device.GetLog(), "ID3D12Device::CreateQueryHeap() failed, error code: 0x%X.", hr);
        return Result::FAILURE;
    }

    return Result::SUCCESS;
}

Result QueryPoolD3D12::CreateReadbackBuffer(const QueryPoolDesc& queryPoolDesc)
{
    m_QuerySize = sizeof(uint64_t);

    D3D12_RESOURCE_DESC resourceDesc = {};
    resourceDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    resourceDesc.Alignment = D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT;
    resourceDesc.Width = (uint64_t)queryPoolDesc.capacity * m_QuerySize;
    resourceDesc.Height = 1;
    resourceDesc.DepthOrArraySize = 1;
    resourceDesc.MipLevels = 1;
    resourceDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
    resourceDesc.Flags = D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS;

    D3D12_HEAP_PROPERTIES heapProperties = {};
    heapProperties.Type = D3D12_HEAP_TYPE_READBACK;

    HRESULT hr = ((ID3D12Device*)m_Device)->CreateCommittedResource(&heapProperties, D3D12_HEAP_FLAG_CREATE_NOT_ZEROED, &resourceDesc,
        D3D12_RESOURCE_STATE_UNORDERED_ACCESS, nullptr, IID_PPV_ARGS(&m_ReadbackBuffer));

    if (FAILED(hr))
    {
        REPORT_ERROR(m_Device.GetLog(), "ID3D12Device::CreateCommittedResource() failed, error code: 0x%X.", hr);
        return Result::FAILURE;
    }

    return Result::SUCCESS;
}

//================================================================================================================
// NRI
//================================================================================================================

#include "QueryPoolD3D12.hpp"
