#pragma once

#include "TiramisuRenderTypes.h"

#include "../../xrCore/xrCore.h"
#include "../../xrCore/xrSyncronize.h"

#include <functional>
#include <utility>

namespace Tiramisu::RenderCommands
{
// Потокобезопасная очередь владеющих команд, передаваемых из game thread в render thread.
class TiramisuRenderCommandQueue
{
public:
    using CommandFunction = std::function<void()>;

    // Одна отложенная команда с именем для диагностики и собственным вызываемым объектом.
    struct Command
    {
        const char* DebugName = nullptr;
        CommandFunction Function;
    };

    // Добавляет команду из game thread; выполнение произойдёт только в render thread.
    void Enqueue(const char* debugName, CommandFunction&& function);
    // Атомарно забирает пакет, не удерживая блокировку во время выполнения.
    xr_vector<Command> Drain();
    // Выполняет текущий пакет команд в порядке добавления.
    void Execute();
    void Clear();
    bool Empty() const;

private:
    mutable xrSRWLock Lock;
    xr_vector<Command> Pending;
};

TiramisuRenderCommandQueue& GetRenderCommandQueue();
void ExecuteRenderCommands();
void FlushRenderCommands();

// Типизированный помощник ENQUEUE_RENDER_COMMAND с проверкой вызывающего потока.
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
#ifdef CheckIsGameThread
        CheckIsGameThread();
#endif
        GetRenderCommandQueue().Enqueue(DebugName, CommandFunction(std::forward<TCallable>(callable)));
    }

private:
    using CommandFunction = TiramisuRenderCommandQueue::CommandFunction;

    const char* DebugName;
};
} // namespace XRay::RenderCommands

#define ENQUEUE_RENDER_COMMAND(CommandName) \
    ::Tiramisu::RenderCommands::TEnqueueRenderCommand(#CommandName)
