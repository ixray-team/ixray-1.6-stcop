#include "stdafx.h"
#include "RenderCommandQueue.h"

namespace Tiramisu::RenderCommands
{
namespace
{
TRenderCommandQueue GRenderCommandQueue;
} // namespace

void TRenderCommandQueue::Enqueue(const char* debugName, CommandFunction&& function)
{
    xrSRWLockGuard guard(Lock);
    Pending.push_back({ debugName, std::move(function) });
}

xr_vector<TRenderCommandQueue::Command> TRenderCommandQueue::Drain()
{
    xr_vector<Command> commands;

    {
        xrSRWLockGuard guard(Lock);
        commands.swap(Pending);
    }

    return commands;
}

void TRenderCommandQueue::Execute()
{
    xr_vector<Command> commands = Drain();

    for (Command& command : commands)
    {
        command.Function();
    }
}

void TRenderCommandQueue::Clear()
{
    xrSRWLockGuard guard(Lock);
    Pending.clear();
}

bool TRenderCommandQueue::Empty() const
{
    xrSRWLockGuard guard(Lock, true);
    return Pending.empty();
}

TRenderCommandQueue& GetRenderCommandQueue()
{
    return GRenderCommandQueue;
}

void ExecuteRenderCommands()
{
    CheckIsRenderThread();
    GRenderCommandQueue.Execute();
}

void FlushRenderCommands()
{
    if (IsRenderThreadRunning())
    {
        std::promise<void> done;
        auto future = done.get_future();

        ENQUEUE_RENDER_COMMAND(FlushRenderCommands)([&done]
        {
            done.set_value();
        });
        GRender->SubmitFrame();
        future.wait();
    }
    else
    {
        if (GRender)
        {
            GRender->WaitGPU_RenderThread();
        }
        ExecuteRenderCommands();
    }
}
} // namespace XRay::RenderCommands
