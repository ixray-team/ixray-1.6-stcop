#include "SharedExternal.h"

using namespace nri;

HelperWaitIdle::HelperWaitIdle(const CoreInterface& NRI, Device& device, CommandQueue& commandQueue) :
    NRI(NRI),
    m_Device(device),
    m_CommandQueue(commandQueue)
{
    NRI.CreateFence(device, 0, m_Fence);
}

HelperWaitIdle::~HelperWaitIdle()
{
    if (m_Fence)
        NRI.DestroyFence(*m_Fence);
    m_Fence = nullptr;
}

Result HelperWaitIdle::WaitIdle()
{
    if (!m_Fence)
        return Result::FAILURE;

    const uint32_t physicalDeviceNum = NRI.GetDeviceDesc(m_Device).physicalDeviceNum;

    for (uint32_t i = 0; i < physicalDeviceNum; i++)
    {
        NRI.QueueSignal(m_CommandQueue, *m_Fence, 1);
        NRI.Wait(*m_Fence, 1);
    }

    return Result::SUCCESS;
}
