/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedExternal.h"
#include "SharedVal.h"
#include "FenceVal.h"
#include "CommandQueueVal.h"

using namespace nri;

//================================================================================================================
// NRI
//================================================================================================================

inline uint64_t FenceVal::GetFenceValue() const
{
    return m_CoreAPI.GetFenceValue(m_ImplObject);
}

inline void FenceVal::QueueSignal(CommandQueueVal& commandQueue, uint64_t value)
{
    m_CoreAPI.QueueSignal(commandQueue.GetImpl(), m_ImplObject, value);
}

inline void FenceVal::QueueWait(CommandQueueVal& commandQueue, uint64_t value)
{
    m_CoreAPI.QueueWait(commandQueue.GetImpl(), m_ImplObject, value);
}

inline void FenceVal::Wait(uint64_t value)
{
    m_CoreAPI.Wait(m_ImplObject, value);
}

inline void FenceVal::SetDebugName(const char* name)
{
    m_Name = name;
    m_CoreAPI.SetFenceDebugName(m_ImplObject, name);
}

#include "FenceVal.hpp"
