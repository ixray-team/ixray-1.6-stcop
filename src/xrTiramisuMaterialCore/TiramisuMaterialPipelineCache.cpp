#include "TiramisuMaterialPipelineCache.h"

#include <algorithm>
#include <ranges>
#include <set>
#include <tuple>
#include <utility>

namespace
{
bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
    return std::ranges::any_of(Diagnostics,
        [](const FMaterialDiagnostic& Diagnostic) { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void AddError(xr_vector<FMaterialDiagnostic>& Diagnostics, xr_string Code, xr_string Message)
{
    Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, std::move(Code), std::move(Message), {}, {}});
}
} // namespace

const FMaterialPassProxy* FMaterialPipelineSet::Find(
    const EMaterialPass Pass, const xr_string_view VertexFactory) const noexcept
{
    const auto Found = std::ranges::find_if(Passes, [Pass, VertexFactory](const FMaterialPassProxy& Proxy)
    {
        return Proxy.Pass == Pass && Proxy.VertexFactory == VertexFactory;
    });
    return Found == Passes.end() ? nullptr : &*Found;
}

bool FMaterialPipelineStageResult::Succeeded() const noexcept
{
    return !HasErrors(Diagnostics);
}

bool FMaterialPipelinePublishResult::Succeeded() const noexcept
{
    return !HasErrors(Diagnostics);
}

FMaterialPipelineStageResult TiramisuMaterialPipelineCache::Stage(
    FMaterialPipelineSet Candidate, const xr_span<const FMaterialPassRequest> RequiredPasses)
{
    FMaterialPipelineStageResult Result;
    if (!Candidate.MaterialId.IsValid())
        AddError(Result.Diagnostics, "pipeline_cache.invalid_material", "Pipeline publication has no material id.");
    if (Candidate.Revision == 0)
        AddError(Result.Diagnostics, "pipeline_cache.invalid_revision", "Pipeline publication revision must be non-zero.");
    if (Candidate.Passes.empty())
        AddError(Result.Diagnostics, "pipeline_cache.empty", "Pipeline publication contains no pass proxies.");

    xr_set<xr_pair<EMaterialPass, xr_string>> UniquePasses;
    xr_set<u64> UniqueKeys;
    for (const FMaterialPassProxy& Proxy : Candidate.Passes)
    {
        if (!Proxy.IsValid())
            AddError(Result.Diagnostics, "pipeline_cache.invalid_proxy", "Pipeline publication contains an invalid pass proxy.");
        if (Proxy.VertexFactory.empty())
            AddError(Result.Diagnostics, "pipeline_cache.missing_vertex_factory", "Pass proxy has no vertex factory.");
        if (!UniquePasses.emplace(Proxy.Pass, Proxy.VertexFactory).second)
            AddError(Result.Diagnostics, "pipeline_cache.duplicate_pass", "Pipeline publication contains a duplicate pass/vertex-factory pair.");
        if (Proxy.PipelineKey != 0 && !UniqueKeys.insert(Proxy.PipelineKey).second)
            AddError(Result.Diagnostics, "pipeline_cache.duplicate_key", "Pipeline publication contains a duplicate pipeline key.");
    }
    for (const FMaterialPassRequest& Required : RequiredPasses)
    {
        if (!Candidate.Find(Required.Pass, Required.VertexFactory))
            AddError(Result.Diagnostics, "pipeline_cache.missing_pass",
                "Pipeline publication is missing pass '" + xr_string(ToString(Required.Pass)) +
                "' for vertex factory '" + Required.VertexFactory + "'.");
    }
    if (HasErrors(Result.Diagnostics))
        return Result;

    auto Snapshot = std::make_shared<const FMaterialPipelineSet>(std::move(Candidate));
    std::scoped_lock Lock(Mutex);
    const auto ActiveFound = Active.find(Snapshot->MaterialId);
    const auto PendingFound = Pending.find(Snapshot->MaterialId);
    const u64 ActiveRevision = ActiveFound == Active.end() ? 0 : ActiveFound->second->Revision;
    const u64 PendingRevision = PendingFound == Pending.end() ? 0 : PendingFound->second->Revision;
    if (Snapshot->Revision <= ActiveRevision || Snapshot->Revision <= PendingRevision)
    {
        AddError(Result.Diagnostics, "pipeline_cache.stale_revision", "Pipeline publication revision is not newer than the cached revision.");
        return Result;
    }
    Pending[Snapshot->MaterialId] = std::move(Snapshot);
    return Result;
}

FMaterialPipelinePublishResult TiramisuMaterialPipelineCache::PublishFrameBoundary(const u64 FrameId)
{
    FMaterialPipelinePublishResult Result;
    Result.FrameId = FrameId;
    std::scoped_lock Lock(Mutex);
    if (FrameId == 0 || FrameId <= LastPublishedFrame)
    {
        AddError(Result.Diagnostics, "pipeline_cache.invalid_frame", "Frame-boundary publication requires a monotonically increasing frame id.");
        return Result;
    }

    for (auto& [MaterialId, Snapshot] : Pending)
    {
        const auto Existing = Active.find(MaterialId);
        if (Existing != Active.end())
            Result.Retired.push_back(std::move(Existing->second));
        Active[MaterialId] = std::move(Snapshot);
        ++Result.PublishedCount;
    }
    Pending.clear();
    LastPublishedFrame = FrameId;
    return Result;
}

xr_shared_ptr<const FMaterialPipelineSet> TiramisuMaterialPipelineCache::Acquire(const FMaterialAssetId& MaterialId) const
{
    std::scoped_lock Lock(Mutex);
    const auto Found = Active.find(MaterialId);
    return Found == Active.end() ? nullptr : Found->second;
}

xr_optional<FMaterialPassProxy> TiramisuMaterialPipelineCache::Resolve(const FMaterialAssetId& MaterialId,
    const EMaterialPass Pass, const xr_string_view VertexFactory) const
{
    const xr_shared_ptr<const FMaterialPipelineSet> Snapshot = Acquire(MaterialId);
    if (!Snapshot) return std::nullopt;
    const FMaterialPassProxy* Proxy = Snapshot->Find(Pass, VertexFactory);
    return Proxy ? xr_optional<FMaterialPassProxy>(*Proxy) : std::nullopt;
}

size_t TiramisuMaterialPipelineCache::GetPendingCount() const
{
    std::scoped_lock Lock(Mutex);
    return Pending.size();
}
