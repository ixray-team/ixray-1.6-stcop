#include "RHIStateCache.h"

RHIStateCache::RHIStateCache(ID3D11Device* InDevice)
    : Device(InDevice)
{
}

RHIStateCache::~RHIStateCache()
{
    ClearCache();
}

ID3D11RasterizerState* RHIStateCache::GetRasterizerState(const D3D11_RASTERIZER_DESC& Desc)
{
    auto It = RSCache.find(Desc);
    if (It != RSCache.end())
        return It->second;

    ID3D11RasterizerState* State = nullptr;
    HRESULT Hr = Device->CreateRasterizerState(&Desc, &State);
    if (SUCCEEDED(Hr))
    {
        RSCache[Desc] = State;
        return State;
    }

    return nullptr;
}

ID3D11DepthStencilState* RHIStateCache::GetDepthStencilState(const D3D11_DEPTH_STENCIL_DESC& Desc)
{
    auto It = DSSCache.find(Desc);
    if (It != DSSCache.end())
        return It->second;

    ID3D11DepthStencilState* State = nullptr;
    HRESULT Hr = Device->CreateDepthStencilState(&Desc, &State);
    if (SUCCEEDED(Hr))
    {
        DSSCache[Desc] = State;
        return State;
    }

    return nullptr;
}

ID3D11BlendState* RHIStateCache::GetBlendState(const D3D11_BLEND_DESC& Desc)
{
    auto It = BSCache.find(Desc);
    if (It != BSCache.end())
        return It->second;

    ID3D11BlendState* State = nullptr;
    HRESULT Hr = Device->CreateBlendState(&Desc, &State);
    if (SUCCEEDED(Hr))
    {
        BSCache[Desc] = State;
        return State;
    }

    return nullptr;
}

void RHIStateCache::ClearCache()
{
    for (auto& Pair : RSCache)
        if (Pair.second) Pair.second->Release();
    RSCache.clear();

    for (auto& Pair : DSSCache)
        if (Pair.second) Pair.second->Release();
    DSSCache.clear();

    for (auto& Pair : BSCache)
        if (Pair.second) Pair.second->Release();
    BSCache.clear();
}