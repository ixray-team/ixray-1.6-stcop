#pragma once

#include "TiramisuRenderTypes.h"

#include <compare>
#include <cstdint>
#include <limits>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace Tiramisu::RenderGraph
{
template <typename Tag>
// Generation-counted handle, защищающий render graph от устаревших индексов.
struct THandle
{
	static constexpr u32 InvalidIndex =
		std::numeric_limits<u32>::max();

	u32 Index = InvalidIndex;
	u32 Generation = 0;

	[[nodiscard]] bool IsValid() const noexcept
	{
		return Index != InvalidIndex && Generation != 0;
	}

	auto operator<=>(const THandle&) const = default;
};

using FResourceHandle = THandle<struct FResourceHandleTag>;
using FPassHandle = THandle<struct FPassHandleTag>;

enum class EQueue : u8
{
	Graphics,
	Compute,
	Copy
};

enum class EAccess : u8
{
	None,
	ShaderRead,
	StorageRead,
	StorageWrite,
	ColorAttachmentWrite,
	DepthStencilRead,
	DepthStencilWrite,
	CopySource,
	CopyDestination,
	IndirectArgument,
	Present
};

// Логическое описание импортированного или временного ресурса render graph.
struct FResourceDesc
{
	xr_string Name;
	// Ресурсы могут делить память только при одинаковом классе формата/размера.
	// Пустой класс явно запрещает aliasing для этого ресурса.
	xr_string CompatibilityClass;
	bool Transient = true;
	bool Imported = false;
};

// Требуемый тип доступа pass к одному логическому ресурсу.
struct FResourceUse
{
	FResourceHandle Resource;
	EAccess Access = EAccess::None;
};

// Декларация pass до анализа зависимостей и barriers.
struct FPassDesc
{
	xr_string Name;
	EQueue Queue = EQueue::Graphics;
	xr_vector<FResourceUse> Uses;
};

// Структурированная ошибка компиляции render graph.
struct FDiagnostic
{
	xr_string Code;
	xr_string Message;
};

// Вычисленный lifetime ресурса и назначенный transient alias slot.
struct FCompiledResource
{
	static constexpr u32 NoAliasSlot =
		std::numeric_limits<u32>::max();

	FResourceHandle Handle;
	u32 FirstPass = 0;
	u32 LastPass = 0;
	u32 AliasSlot = NoAliasSlot;
};

// Переход состояния ресурса между двумя passes.
struct FBarrier
{
	FResourceHandle Resource;
	EAccess Before = EAccess::None;
	EAccess After = EAccess::None;
	FPassHandle SourcePass;
	FPassHandle DestinationPass;
	bool QueueTransfer = false;
};

// Pass после топологической сортировки с полным набором зависимостей.
struct FCompiledPass
{
	FPassHandle Handle;
	xr_string Name;
	EQueue Queue = EQueue::Graphics;
	xr_vector<FPassHandle> Dependencies;
};

// Межочередное ожидание результата pass из другой GPU queue.
struct FQueueWait
{
	FPassHandle SourcePass;
	EQueue SourceQueue = EQueue::Graphics;
};

// Первая runtime-реализация намеренно создаёт по одному submission на pass.
// Так межочередная синхронизация остаётся явной и детерминированной; позже соседние
// совместимые submissions можно объединить без изменения ABI графа.
struct FQueueSubmission
{
	FPassHandle Pass;
	EQueue Queue = EQueue::Graphics;
	xr_vector<FQueueWait> Waits;
	xr_vector<FBarrier> Barriers;
};

// Готовый план исполнения: passes, ресурсы, barriers и submissions.
struct FCompiledGraph
{
	xr_vector<FCompiledPass> Passes;
	xr_vector<FCompiledResource> Resources;
	xr_vector<FBarrier> Barriers;
	xr_vector<FQueueSubmission> Submissions;
	u32 AliasSlotCount = 0;
};

// Результат компиляции с валидным графом либо полным списком диагностик.
struct FCompileResult
{
	xr_optional<FCompiledGraph> Value;
	xr_vector<FDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept
	{
		return Value.has_value() && Diagnostics.empty();
	}
};

// Декларативно строит render graph и компилирует его в план исполнения.
class TiramisuRenderGraphBuilder
{
public:
	// Регистрирует ресурсы и passes текущего поколения builder.
	[[nodiscard]] FResourceHandle CreateResource(FResourceDesc Desc);
	[[nodiscard]] FResourceHandle ImportResource(FResourceDesc Desc);
	[[nodiscard]] FPassHandle AddPass(FPassDesc Desc);
	void AddDependency(FPassHandle Before, FPassHandle After);

	// Проверяет handles и access, строит barriers, lifetimes, queues и aliasing.
	[[nodiscard]] FCompileResult Compile() const;
	// Начинает новое поколение, инвалидируя ранее выданные handles.
	void Reset() noexcept;

private:
	// Явная зависимость, дополняющая зависимости от доступа к ресурсам.
	struct FDependency
	{
		FPassHandle Before;
		FPassHandle After;
	};

	u32 Generation = 1;
	xr_vector<FResourceDesc> Resources;
	xr_vector<FPassDesc> Passes;
	xr_vector<FDependency> Dependencies;
};

[[nodiscard]] bool IsReadAccess(EAccess Access) noexcept;
[[nodiscard]] bool IsWriteAccess(EAccess Access) noexcept;
[[nodiscard]] bool IsAccessSupportedByQueue(EAccess Access, EQueue Queue) noexcept;
[[nodiscard]] xr_string_view ToString(EQueue Queue) noexcept;
[[nodiscard]] xr_string_view ToString(EAccess Access) noexcept;
} // namespace Tiramisu::RenderGraph
