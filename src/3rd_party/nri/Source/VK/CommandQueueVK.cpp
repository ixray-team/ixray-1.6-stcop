/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedVK.h"
#include "CommandBufferVK.h"
#include "CommandQueueVK.h"

using namespace nri;

Result CommandQueueVK::Create(const CommandQueueVulkanDesc& commandQueueDesc)
{
    m_Handle = (VkQueue)commandQueueDesc.vkQueue;
    m_FamilyIndex = commandQueueDesc.familyIndex;
    m_Type = commandQueueDesc.commandQueueType;

    return Result::SUCCESS;
}

//================================================================================================================
// NRI
//================================================================================================================

inline void CommandQueueVK::SetDebugName(const char* name)
{
    m_Device.SetDebugNameToTrivialObject(VK_OBJECT_TYPE_QUEUE, (uint64_t)m_Handle, name);
}

inline void CommandQueueVK::Submit(const QueueSubmitDesc& queueSubmitDesc)
{
    VkCommandBuffer* commandBuffers = STACK_ALLOC(VkCommandBuffer, queueSubmitDesc.commandBufferNum);

    VkSubmitInfo submission = {
        VK_STRUCTURE_TYPE_SUBMIT_INFO,
        nullptr,
        0,
        nullptr,
        nullptr,
        queueSubmitDesc.commandBufferNum,
        commandBuffers,
        0,
        nullptr
    };

    for (uint32_t i = 0; i < queueSubmitDesc.commandBufferNum; i++)
        *(commandBuffers++) = *(CommandBufferVK*)queueSubmitDesc.commandBuffers[i];

    VkDeviceGroupSubmitInfo deviceGroupInfo = {};
    uint32_t* commandBufferDeviceMasks = STACK_ALLOC(uint32_t, queueSubmitDesc.commandBufferNum);

    if (m_Device.GetPhysicalDeviceGroupSize() > 1)
    {
        for (uint32_t i = 0; i < queueSubmitDesc.commandBufferNum; i++)
            commandBufferDeviceMasks[i] = 1u << queueSubmitDesc.physicalDeviceIndex;

        deviceGroupInfo = {
            VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
            nullptr,
            0,
            nullptr,
            queueSubmitDesc.commandBufferNum,
            commandBufferDeviceMasks,
            0,
            nullptr
        };

        submission.pNext = &deviceGroupInfo;
    }

    const auto& vk = m_Device.GetDispatchTable();
    const VkResult result = vk.QueueSubmit(m_Handle, 1, &submission, VK_NULL_HANDLE);

    RETURN_ON_FAILURE(m_Device.GetLog(), result == VK_SUCCESS, ReturnVoid(),
        "Can't submit work to a command queue: vkQueueSubmit returned %d.", (int32_t)result);
}

inline Result CommandQueueVK::ChangeResourceStates(const TransitionBarrierDesc& transitionBarriers)
{
    HelperResourceStateChange resourceStateChange(m_Device.GetCoreInterface(), (Device&)m_Device, (CommandQueue&)*this);

    return resourceStateChange.ChangeStates(transitionBarriers);
}

inline Result CommandQueueVK::UploadData(const TextureUploadDesc* textureUploadDescs, uint32_t textureUploadDescNum,
    const BufferUploadDesc* bufferUploadDescs, uint32_t bufferUploadDescNum)
{
    HelperDataUpload helperDataUpload(m_Device.GetCoreInterface(), (Device&)m_Device, m_Device.GetStdAllocator(), (CommandQueue&)*this);

    return helperDataUpload.UploadData(textureUploadDescs, textureUploadDescNum, bufferUploadDescs, bufferUploadDescNum);
}

inline Result CommandQueueVK::WaitForIdle()
{
    const auto& vk = m_Device.GetDispatchTable();

    VkResult result = vk.QueueWaitIdle(m_Handle);

    RETURN_ON_FAILURE(m_Device.GetLog(), result == VK_SUCCESS, GetReturnCode(result),
        "Can't wait for idle: vkQueueWaitIdle returned %d.", (int32_t)result);

    return Result::SUCCESS;
}

#include "CommandQueueVK.hpp"