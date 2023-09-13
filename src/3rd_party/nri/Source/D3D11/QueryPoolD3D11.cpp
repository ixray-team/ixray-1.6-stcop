/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D11.h"
#include "QueryPoolD3D11.h"

using namespace nri;

Result QueryPoolD3D11::Create(const QueryPoolDesc& queryPoolDesc)
{
    m_Pool.reserve(queryPoolDesc.capacity);
    m_Type = queryPoolDesc.queryType;

    D3D11_QUERY_DESC queryDesc = {};

    if (m_Type == QueryType::TIMESTAMP)
        queryDesc.Query = D3D11_QUERY_TIMESTAMP;
    else if (m_Type == QueryType::OCCLUSION)
        queryDesc.Query = D3D11_QUERY_OCCLUSION;
    else if (m_Type == QueryType::PIPELINE_STATISTICS)
        queryDesc.Query = D3D11_QUERY_PIPELINE_STATISTICS;
    else
        return Result::UNSUPPORTED;

    for (uint32_t i = 0; i < queryPoolDesc.capacity; i++)
    {
        ID3D11Query* query = nullptr;
        HRESULT hr = m_Device.GetDevice()->CreateQuery(&queryDesc, &query);
        RETURN_ON_BAD_HRESULT(m_Device.GetLog(), hr, "ID3D11Device::CreateQuery() - FAILED!");

        m_Pool.push_back(query);
    }

    return Result::SUCCESS;
}

void QueryPoolD3D11::BeginQuery(const VersionedContext& deferredContext, uint32_t offset)
{
    ID3D11Query* query = m_Pool[offset];

    deferredContext->Begin(query);
}

void QueryPoolD3D11::EndQuery(const VersionedContext& deferredContext, uint32_t offset)
{
    ID3D11Query* query = m_Pool[offset];

    deferredContext->End(query);
}

void QueryPoolD3D11::GetData(uint8_t* dstMemory, uint32_t offset, uint32_t num) const
{
    uint32_t dataSize = GetQuerySize();

    num += offset;

    for (uint32_t i = offset; i < num; i++)
    {
        ID3D11Query* query = m_Pool[i];
        m_Device.GetImmediateContext()->GetData(query, dstMemory, dataSize, 0);

        dstMemory += dataSize;
    }
}

//================================================================================================================
// NRI
//================================================================================================================

inline void QueryPoolD3D11::SetDebugName(const char* name)
{
    for(ComPtr<ID3D11Query>& query : m_Pool)
        SET_D3D_DEBUG_OBJECT_NAME(query, name);
}

inline uint32_t QueryPoolD3D11::GetQuerySize() const
{
    if (m_Type == QueryType::PIPELINE_STATISTICS)
        return sizeof(D3D11_QUERY_DATA_PIPELINE_STATISTICS);

    return sizeof(uint64_t);
}

#include "QueryPoolD3D11.hpp"
