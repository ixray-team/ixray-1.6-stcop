#include "RenderCommandQueue.h"

namespace Tiramisu::RenderCommands
{
namespace
{
TiramisuRenderCommandQueue GRenderCommandQueue;
} // namespace

void TiramisuRenderCommandQueue::Enqueue(const char* debugName, CommandFunction&& function)
{
	xrSRWLockGuard guard(Lock);
	Pending.push_back({debugName, std::move(function)});
}

xr_vector<TiramisuRenderCommandQueue::Command> TiramisuRenderCommandQueue::Drain()
{
	xr_vector<Command> Commands;
	{
		xrSRWLockGuard guard(Lock);
		Commands.swap(Pending);
	}
	return Commands;
}

void TiramisuRenderCommandQueue::Execute()
{
	xr_vector<Command> Commands = Drain();
	for (Command& Command : Commands)
	{
		Command.Function();
	}
}

void TiramisuRenderCommandQueue::Clear()
{
	xrSRWLockGuard guard(Lock);
	Pending.clear();
}

bool TiramisuRenderCommandQueue::Empty() const
{
	xrSRWLockGuard guard(Lock, true);
	return Pending.empty();
}

TiramisuRenderCommandQueue& GetRenderCommandQueue()
{
	return GRenderCommandQueue;
}
} // namespace Tiramisu::RenderCommands
