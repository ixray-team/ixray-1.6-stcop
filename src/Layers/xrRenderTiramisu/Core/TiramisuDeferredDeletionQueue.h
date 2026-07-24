#pragma once

#include "TiramisuRenderTypes.h"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <vector>

namespace Tiramisu
{
// Откладывает уничтожение GPU-ресурсов до завершения использующего их fence.
class TiramisuDeferredDeletionQueue
{
public:
    using FDeleteFunction = std::function<void()>;

    // Регистрирует удаление после fence, на котором GPU закончит использовать ресурс.
    [[nodiscard]] bool Enqueue(u64 RetireFence,
        FDeleteFunction Function);
    // Выполняет готовые удаления, сохраняя порядок для одинакового fence.
    size_t Collect(u64 CompletedFence);
    // Принудительно выполняет всё после полного ожидания GPU при остановке renderer.
    size_t Flush();

    [[nodiscard]] size_t Size() const noexcept { return Entries.size(); }
    [[nodiscard]] bool Empty() const noexcept { return Entries.empty(); }

private:
    // Владеющая запись отложенного удаления GPU-ресурса.
    struct FEntry
    {
        u64 RetireFence = 0;
        u64 Sequence = 0;
        FDeleteFunction Function;
    };

    xr_vector<FEntry> Entries;
    u64 NextSequence = 0;
};
} // namespace Tiramisu
