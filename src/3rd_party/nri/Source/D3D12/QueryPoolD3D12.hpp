/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  Core  ]

static void NRI_CALL SetQueryPoolDebugName(QueryPool& queryPool, const char* name)
{
    ((QueryPoolD3D12&)queryPool).SetDebugName(name);
}

static uint32_t NRI_CALL GetQuerySize(const QueryPool& queryPool)
{
    return ((QueryPoolD3D12&)queryPool).GetQuerySize();
}

#pragma endregion

Define_Core_QueryPool_PartiallyFillFunctionTable(D3D12)
