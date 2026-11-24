/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
    {
        xrSRWLockGuard Guard(Lock, true);
        auto It = RSCache.find(Desc);
        if (It != RSCache.end())
            return It->second;
    }

    xrSRWLockGuard Guard(Lock);
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
    {
        xrSRWLockGuard Guard(Lock, true);
        auto It = DSSCache.find(Desc);
        if (It != DSSCache.end())
            return It->second;
    }

    xrSRWLockGuard Guard(Lock);
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
    {
        xrSRWLockGuard Guard(Lock, true);
        auto It = BSCache.find(Desc);
        if (It != BSCache.end())
            return It->second;
    }

    xrSRWLockGuard Guard(Lock);
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
    xrSRWLockGuard Guard(Lock);
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
