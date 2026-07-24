#include "TiramisuMaterialPipelineCache.h"
#include "MaterialTestHarness.h"

#include <array>
#include <memory>
#include <vector>

namespace
{
FMaterialPassProxy Proxy(const EMaterialPass Pass, const u64 Key,
    const u32 Index, const u64 Revision, xr_string VertexFactory = "level_static")
{
    FMaterialPassProxy Result;
    Result.Pass = Pass;
    Result.PipelineKey = Key;
    Result.MaterialInstanceIndex = 7;
    Result.Pipeline = {Index, 1};
    Result.VertexFactory = std::move(VertexFactory);
    Result.Revision = Revision;
    return Result;
}

FMaterialPipelineSet SurfaceSet(const xr_string& Id, const u64 Revision, const u64 KeyBase)
{
    FMaterialPipelineSet Result;
    Result.MaterialId = {Id};
    Result.Revision = Revision;
    Result.Passes = {
        Proxy(EMaterialPass::Depth, KeyBase + 1, static_cast<u32>(KeyBase + 1), Revision),
        Proxy(EMaterialPass::Shadow, KeyBase + 2, static_cast<u32>(KeyBase + 2), Revision),
        Proxy(EMaterialPass::GBuffer, KeyBase + 3, static_cast<u32>(KeyBase + 3), Revision),
    };
    return Result;
}

const xr_array RequiredSurface = {
    FMaterialPassRequest{EMaterialPass::Depth, "level_static"},
    FMaterialPassRequest{EMaterialPass::Shadow, "level_static"},
    FMaterialPassRequest{EMaterialPass::GBuffer, "level_static"},
};

void TestFrameBoundaryPublication(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialPipelineCache Cache;
    const FMaterialPipelineStageResult Staged = Cache.Stage(SurfaceSet("master-a", 1, 100), RequiredSurface);
    MATERIAL_CHECK(Runner, Staged.Succeeded());
    MATERIAL_CHECK(Runner, Cache.GetPendingCount() == 1);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-a"}) == nullptr);

    const FMaterialPipelinePublishResult Published = Cache.PublishFrameBoundary(1);
    MATERIAL_CHECK(Runner, Published.Succeeded());
    MATERIAL_CHECK(Runner, Published.PublishedCount == 1);
    MATERIAL_CHECK(Runner, Published.Retired.empty());
    MATERIAL_CHECK(Runner, Cache.GetPendingCount() == 0);

    const xr_shared_ptr<const FMaterialPipelineSet> Snapshot = Cache.Acquire({"master-a"});
    MATERIAL_CHECK(Runner, Snapshot != nullptr);
    MATERIAL_CHECK(Runner, Snapshot->Revision == 1);
    const xr_optional<FMaterialPassProxy> GBuffer = Cache.Resolve(
        {"master-a"}, EMaterialPass::GBuffer, "level_static");
    MATERIAL_CHECK(Runner, GBuffer.has_value());
    MATERIAL_CHECK(Runner, GBuffer->PipelineKey == 103);
    MATERIAL_CHECK(Runner, !Cache.Resolve({"master-a"}, EMaterialPass::Forward, "level_static").has_value());
}

void TestFailedReloadKeepsOldSnapshot(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialPipelineCache Cache;
    MATERIAL_CHECK(Runner, Cache.Stage(SurfaceSet("master-a", 1, 100), RequiredSurface).Succeeded());
    MATERIAL_CHECK(Runner, Cache.PublishFrameBoundary(1).Succeeded());
    const xr_shared_ptr<const FMaterialPipelineSet> OldSnapshot = Cache.Acquire({"master-a"});

    FMaterialPipelineSet Broken = SurfaceSet("master-a", 2, 200);
    Broken.Passes.erase(Broken.Passes.begin() + 1);
    const FMaterialPipelineStageResult Rejected = Cache.Stage(std::move(Broken), RequiredSurface);
    MATERIAL_CHECK(Runner, !Rejected.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Rejected.Diagnostics, "pipeline_cache.missing_pass"));
    MATERIAL_CHECK(Runner, Cache.GetPendingCount() == 0);

    const FMaterialPipelinePublishResult EmptyBoundary = Cache.PublishFrameBoundary(2);
    MATERIAL_CHECK(Runner, EmptyBoundary.Succeeded());
    MATERIAL_CHECK(Runner, EmptyBoundary.PublishedCount == 0);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-a"}) == OldSnapshot);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-a"})->Revision == 1);

    MATERIAL_CHECK(Runner, Cache.Stage(SurfaceSet("master-a", 2, 200), RequiredSurface).Succeeded());
    const FMaterialPipelinePublishResult Reloaded = Cache.PublishFrameBoundary(3);
    MATERIAL_CHECK(Runner, Reloaded.Succeeded());
    MATERIAL_CHECK(Runner, Reloaded.PublishedCount == 1);
    MATERIAL_CHECK(Runner, Reloaded.Retired.size() == 1);
    MATERIAL_CHECK(Runner, Reloaded.Retired[0] == OldSnapshot);
    MATERIAL_CHECK(Runner, Reloaded.Retired[0]->Revision == 1);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-a"})->Revision == 2);
    MATERIAL_CHECK(Runner, OldSnapshot->Find(EMaterialPass::GBuffer, "level_static")->PipelineKey == 103);
}

void TestValidationAndMonotonicity(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialPipelineCache Cache;

    FMaterialPipelineSet Duplicate = SurfaceSet("master-a", 1, 100);
    Duplicate.Passes[1].PipelineKey = Duplicate.Passes[0].PipelineKey;
    const FMaterialPipelineStageResult DuplicateKey = Cache.Stage(std::move(Duplicate), RequiredSurface);
    MATERIAL_CHECK(Runner, !DuplicateKey.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(DuplicateKey.Diagnostics, "pipeline_cache.duplicate_key"));

    FMaterialPipelineSet InvalidHandle = SurfaceSet("master-a", 1, 100);
    InvalidHandle.Passes[0].Pipeline = {};
    const FMaterialPipelineStageResult Invalid = Cache.Stage(std::move(InvalidHandle), RequiredSurface);
    MATERIAL_CHECK(Runner, !Invalid.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Invalid.Diagnostics, "pipeline_cache.invalid_proxy"));

    FMaterialPipelineSet DuplicatePass = SurfaceSet("master-a", 1, 100);
    DuplicatePass.Passes.push_back(Proxy(EMaterialPass::Depth, 999, 999, 1));
    const FMaterialPipelineStageResult DuplicatePair = Cache.Stage(std::move(DuplicatePass), RequiredSurface);
    MATERIAL_CHECK(Runner, !DuplicatePair.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(DuplicatePair.Diagnostics, "pipeline_cache.duplicate_pass"));

    MATERIAL_CHECK(Runner, Cache.Stage(SurfaceSet("master-a", 2, 200), RequiredSurface).Succeeded());
    const FMaterialPipelineStageResult OlderPending = Cache.Stage(SurfaceSet("master-a", 1, 100), RequiredSurface);
    MATERIAL_CHECK(Runner, !OlderPending.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(OlderPending.Diagnostics, "pipeline_cache.stale_revision"));
    MATERIAL_CHECK(Runner, Cache.PublishFrameBoundary(5).Succeeded());

    const FMaterialPipelineStageResult SameActive = Cache.Stage(SurfaceSet("master-a", 2, 300), RequiredSurface);
    MATERIAL_CHECK(Runner, !SameActive.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(SameActive.Diagnostics, "pipeline_cache.stale_revision"));
    const FMaterialPipelinePublishResult RepeatedFrame = Cache.PublishFrameBoundary(5);
    MATERIAL_CHECK(Runner, !RepeatedFrame.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(RepeatedFrame.Diagnostics, "pipeline_cache.invalid_frame"));
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-a"})->Revision == 2);
}

void TestAtomicMultiMaterialPublish(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialPipelineCache Cache;
    MATERIAL_CHECK(Runner, Cache.Stage(SurfaceSet("master-a", 1, 100), RequiredSurface).Succeeded());
    MATERIAL_CHECK(Runner, Cache.Stage(SurfaceSet("master-b", 1, 200), RequiredSurface).Succeeded());
    MATERIAL_CHECK(Runner, Cache.GetPendingCount() == 2);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-a"}) == nullptr);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-b"}) == nullptr);
    const FMaterialPipelinePublishResult Published = Cache.PublishFrameBoundary(10);
    MATERIAL_CHECK(Runner, Published.Succeeded());
    MATERIAL_CHECK(Runner, Published.PublishedCount == 2);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-a"})->Revision == 1);
    MATERIAL_CHECK(Runner, Cache.Acquire({"master-b"})->Revision == 1);
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrMaterialPipelineCacheTests");
    TestFrameBoundaryPublication(Runner);
    TestFailedReloadKeepsOldSnapshot(Runner);
    TestValidationAndMonotonicity(Runner);
    TestAtomicMultiMaterialPublish(Runner);
    return Runner.Finish();
}
