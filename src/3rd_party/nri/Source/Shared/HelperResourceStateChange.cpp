#include "SharedExternal.h"

using namespace nri;

HelperResourceStateChange::HelperResourceStateChange(const CoreInterface& NRI, Device& device, CommandQueue& commandQueue) :
    NRI(NRI),
    m_Device(device),
    m_CommandQueue(commandQueue),
    m_HelperWaitIdle(NRI, device, commandQueue)
{
    if (NRI.CreateCommandAllocator(commandQueue, WHOLE_DEVICE_GROUP, m_CommandAllocator) == Result::SUCCESS)
        NRI.CreateCommandBuffer(*m_CommandAllocator, m_CommandBuffer);
}

HelperResourceStateChange::~HelperResourceStateChange()
{
    if (m_CommandBuffer)
        NRI.DestroyCommandBuffer(*m_CommandBuffer);
    m_CommandBuffer = nullptr;

    if (m_CommandAllocator)
        NRI.DestroyCommandAllocator(*m_CommandAllocator);
    m_CommandAllocator = nullptr;
}

Result HelperResourceStateChange::ChangeStates(const TransitionBarrierDesc& transitionBarriers)
{
    if (m_CommandBuffer == nullptr)
        return Result::FAILURE;

    const uint32_t physicalDeviceNum = NRI.GetDeviceDesc(m_Device).physicalDeviceNum;

    for (uint32_t i = 0; i < physicalDeviceNum; i++)
    {
        NRI.BeginCommandBuffer(*m_CommandBuffer, nullptr, i);
        {
            NRI.CmdPipelineBarrier(*m_CommandBuffer, &transitionBarriers, nullptr, BarrierDependency::ALL_STAGES);
        }
        NRI.EndCommandBuffer(*m_CommandBuffer);

        QueueSubmitDesc queueSubmitDesc = {};
        queueSubmitDesc.physicalDeviceIndex = i;
        queueSubmitDesc.commandBufferNum = 1;
        queueSubmitDesc.commandBuffers = &m_CommandBuffer;

        NRI.QueueSubmit(m_CommandQueue, queueSubmitDesc);
    }

    return m_HelperWaitIdle.WaitIdle();
}