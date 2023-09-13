/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  SwapChain  ]

static void NRI_CALL SetSwapChainDebugName(SwapChain& swapChain, const char* name)
{
    ((SwapChainVal&)swapChain).SetDebugName(name);
}

static Texture* const* NRI_CALL GetSwapChainTextures(const SwapChain& swapChain, uint32_t& textureNum, Format& format)
{
    return ((SwapChainVal&)swapChain).GetTextures(textureNum, format);
}

static uint32_t NRI_CALL AcquireNextSwapChainTexture(SwapChain& swapChain)
{
    return ((SwapChainVal&)swapChain).AcquireNextTexture();
}

static Result NRI_CALL SwapChainPresent(SwapChain& swapChain)
{
    return ((SwapChainVal&)swapChain).Present();
}

static Result NRI_CALL SetSwapChainHdrMetadata(SwapChain& swapChain, const HdrMetadata& hdrMetadata)
{
    return ((SwapChainVal&)swapChain).SetHdrMetadata(hdrMetadata);
}

#pragma endregion

Define_SwapChain_PartiallyFillFunctionTable(Val)
