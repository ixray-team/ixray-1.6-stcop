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

struct DeviceD3D11;

struct CommandQueueD3D11
{
    inline CommandQueueD3D11(DeviceD3D11& device) :
        m_Device(device)
    {}

    inline ~CommandQueueD3D11()
    {}

    inline DeviceD3D11& GetDevice() const
    { return m_Device; }

    //================================================================================================================
    // NRI
    //================================================================================================================

    inline void SetDebugName(const char* name)
    { MaybeUnused(name); }

    void Submit(const QueueSubmitDesc& queueSubmitDesc);
    Result ChangeResourceStates(const TransitionBarrierDesc& transitionBarriers);
    Result UploadData(const TextureUploadDesc* textureUploadDescs, uint32_t textureUploadDescNum,
        const BufferUploadDesc* bufferUploadDescs, uint32_t bufferUploadDescNum);
    Result WaitForIdle();

private:
    DeviceD3D11& m_Device;
};

}
