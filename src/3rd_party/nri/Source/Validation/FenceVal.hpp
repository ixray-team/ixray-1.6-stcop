/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma region [  Core  ]

static uint64_t NRI_CALL GetFenceValue(Fence& fence)
{
    return ((FenceVal&)fence).GetFenceValue();
}

static void NRI_CALL QueueSignal(CommandQueue& commandQueue, Fence& fence, uint64_t value)
{
    return ((FenceVal&)fence).QueueSignal((CommandQueueVal&)commandQueue, value);
}

static void NRI_CALL QueueWait(CommandQueue& commandQueue, Fence& fence, uint64_t value)
{
    return ((FenceVal&)fence).QueueWait((CommandQueueVal&)commandQueue, value);
}

static void NRI_CALL Wait(Fence& fence, uint64_t value)
{
    ((FenceVal&)fence).Wait(value);
}

static void NRI_CALL SetFenceDebugName(Fence& fence, const char* name)
{
    ((FenceVal&)fence).SetDebugName(name);
}

#pragma endregion

Define_Core_Fence_PartiallyFillFunctionTable(Val)

