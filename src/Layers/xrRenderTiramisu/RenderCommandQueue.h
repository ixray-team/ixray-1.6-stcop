#pragma once

#include "../../xrCore/xrCore.h"
#include "../../xrCore/xrSyncronize.h"

#include <functional>
#include <utility>

namespace Tiramisu::RenderCommands
{
class TRenderCommandQueue
{
public:
    using CommandFunction = std::function<void()>;

    struct Command
    {
        const char* DebugName = nullptr;
        CommandFunction Function;
    };

    void Enqueue(const char* debugName, CommandFunction&& function);
    xr_vector<Command> Drain();
    void Execute();
    void Clear();
    bool Empty() const;

private:
    mutable xrSRWLock Lock;
    xr_vector<Command> Pending;
};

TRenderCommandQueue& GetRenderCommandQueue();
void ExecuteRenderCommands();
void FlushRenderCommands();

class TEnqueueRenderCommand
{
public:
    explicit TEnqueueRenderCommand(const char* debugName)
        : DebugName(debugName)
    {
    }

    template <class TCallable>
    void operator()(TCallable&& callable) const
    {
        CheckIsGameThread();
        GetRenderCommandQueue().Enqueue(DebugName, CommandFunction(std::forward<TCallable>(callable)));
    }

private:
    using CommandFunction = TRenderCommandQueue::CommandFunction;

    const char* DebugName;
};
} // namespace XRay::RenderCommands

#define ENQUEUE_RENDER_COMMAND(CommandName) \
    ::Tiramisu::RenderCommands::TEnqueueRenderCommand(#CommandName)
