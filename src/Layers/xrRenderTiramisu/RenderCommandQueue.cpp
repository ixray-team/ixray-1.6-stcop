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
    xrCriticalSectionGuard guard(Lock);
    Pending.push_back({ debugName, std::move(function) });
}

xr_vector<TRenderCommandQueue::Command> TRenderCommandQueue::Drain()
{
    xr_vector<Command> commands;

    {
        xrCriticalSectionGuard guard(Lock);
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
    xrCriticalSectionGuard guard(Lock);
    Pending.clear();
}

bool TRenderCommandQueue::Empty() const
{
    xrCriticalSectionGuard guard(Lock);
    return Pending.empty();
}

TRenderCommandQueue& GetRenderCommandQueue()
{
    return GRenderCommandQueue;
}

void ExecuteRenderCommands()
{
    GRenderCommandQueue.Execute();
}

void FlushRenderCommands()
{
    ExecuteRenderCommands();
}
} // namespace XRay::RenderCommands
