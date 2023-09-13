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

constexpr uint32_t ATTACHMENT_MAX_NUM = 8;

struct DeviceVK;

struct FrameBufferVK
{
    inline FrameBufferVK(DeviceVK& device) :
        m_Device(device)
    {}

    inline DeviceVK& GetDevice() const
    { return m_Device; }

    inline VkFramebuffer GetHandle(uint32_t physicalDeviceIndex) const
    { return m_Handles[physicalDeviceIndex]; }

    inline VkRenderPass GetRenderPass(RenderPassBeginFlag renderPassBeginFlag) const
    { return (renderPassBeginFlag == RenderPassBeginFlag::SKIP_FRAME_BUFFER_CLEAR) ? m_RenderPass : m_RenderPassWithClear; }

    inline uint32_t GetLayerNum() const
    { return m_LayerNum; }

    inline const VkRect2D& GetRenderArea() const
    { return m_RenderArea; }

    inline uint32_t GetAttachmentNum() const
    { return m_AttachmentNum; }

    inline void GetClearValues(VkClearValue* values) const
    { memcpy(values, m_ClearValues.data(), m_AttachmentNum * sizeof(VkClearValue)); }

    ~FrameBufferVK();

    Result Create(const FrameBufferDesc& frameBufferDesc);

    //================================================================================================================
    // NRI
    //================================================================================================================

    void SetDebugName(const char* name);

private:
    void FillDescriptionsAndFormats(const FrameBufferDesc& frameBufferDesc, VkAttachmentDescription* descriptions, VkFormat* formats);
    Result SaveClearColors(const FrameBufferDesc& frameBufferDesc);
    Result CreateRenderPass(const FrameBufferDesc& frameBufferDesc);

private:
    DeviceVK& m_Device;
    std::array<VkFramebuffer, PHYSICAL_DEVICE_GROUP_MAX_SIZE> m_Handles = {};
    VkRenderPass m_RenderPassWithClear = VK_NULL_HANDLE;
    VkRenderPass m_RenderPass = VK_NULL_HANDLE;
    std::array<ClearValueDesc, ATTACHMENT_MAX_NUM> m_ClearValues = {};
    VkRect2D m_RenderArea = {};
    uint32_t m_LayerNum = 0;
    uint32_t m_AttachmentNum = 0;
};

}