#pragma once

#include "MaterialPass.h"

#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <vector>

// Запрос всех material passes, необходимых конкретной сцене или preview.
struct FMaterialPassRequest
{
	EMaterialPass Pass = EMaterialPass::GBuffer;
	xr_string VertexFactory = "level_static";

	auto operator<=>(const FMaterialPassRequest&) const = default;
};

// Набор успешно скомпилированных pass proxies одной material revision.
struct FMaterialPipelineSet
{
	FMaterialAssetId MaterialId;
	u64 Revision = 0;
	xr_vector<FMaterialPassProxy> Passes;

	[[nodiscard]] const FMaterialPassProxy* Find(EMaterialPass Pass, xr_string_view VertexFactory) const noexcept;
};

// Результат background-компиляции одной shader stage.
struct FMaterialPipelineStageResult
{
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Результат атомарной публикации нового pipeline set с сохранением last-good при ошибке.
struct FMaterialPipelinePublishResult
{
	u64 FrameId = 0;
	size_t PublishedCount = 0;
	xr_vector<xr_shared_ptr<const FMaterialPipelineSet>> Retired;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Background compiler threads готовят полностью созданные renderer proxies. Render thread
// публикует все валидные candidates вместе на границе кадра. Устаревшие snapshots
// возвращаются renderer для отложенного удаления GPU-ресурсов.
class TiramisuMaterialPipelineCache
{
public:
	// Stage выполняется вне render thread; PublishFrameBoundary атомарно заменяет last-good set.
	[[nodiscard]] FMaterialPipelineStageResult Stage(
		FMaterialPipelineSet Candidate, xr_span<const FMaterialPassRequest> RequiredPasses
	);
	[[nodiscard]] FMaterialPipelinePublishResult PublishFrameBoundary(u64 FrameId);
	[[nodiscard]] xr_shared_ptr<const FMaterialPipelineSet> Acquire(const FMaterialAssetId& MaterialId) const;
	[[nodiscard]] xr_optional<FMaterialPassProxy> Resolve(
		const FMaterialAssetId& MaterialId, EMaterialPass Pass, xr_string_view VertexFactory
	) const;
	[[nodiscard]] size_t GetPendingCount() const;

private:
	mutable std::mutex Mutex;
	xr_map<FMaterialAssetId, xr_shared_ptr<const FMaterialPipelineSet>> Active;
	xr_map<FMaterialAssetId, xr_shared_ptr<const FMaterialPipelineSet>> Pending;
	u64 LastPublishedFrame = 0;
};
