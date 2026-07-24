#include "Core/RenderGraph/TiramisuRenderGraph.h"
#include "Core/RenderGraph/TiramisuRenderGraphNri.h"

#include <cstdlib>
#include <iostream>
#include <string_view>

using namespace Tiramisu::RenderGraph;

namespace
{
int Failures = 0;

void Check(const bool Condition, const char* Expression, const int Line)
{
    if (Condition) return;
    ++Failures;
    std::cerr << "line " << Line << ": check failed: " << Expression << '\n';
}

#define GRAPH_CHECK(Expression) Check((Expression), #Expression, __LINE__)

bool HasDiagnostic(const FCompileResult& Result, const xr_string_view Code)
{
    for (const FDiagnostic& Diagnostic : Result.Diagnostics)
        if (Diagnostic.Code == Code) return true;
    return false;
}

const FCompiledResource* FindResource(const FCompiledGraph& Graph,
    const FResourceHandle Handle)
{
    for (const FCompiledResource& Resource : Graph.Resources)
        if (Resource.Handle == Handle) return &Resource;
    return nullptr;
}

const FQueueSubmission* FindSubmission(const FCompiledGraph& Graph,
    const xr_string_view PassName)
{
    for (const FCompiledPass& Pass : Graph.Passes)
    {
        if (Pass.Name != PassName)
            continue;
        for (const FQueueSubmission& Submission : Graph.Submissions)
            if (Submission.Pass == Pass.Handle) return &Submission;
    }
    return nullptr;
}

void TestAsyncComputeFlow()
{
    TiramisuRenderGraphBuilder Builder;
    const FResourceHandle BackBuffer = Builder.ImportResource({"BackBuffer", {}, false, true});
    const FResourceHandle Depth = Builder.CreateResource({"Depth", "d32"});
    const FResourceHandle HiZ = Builder.CreateResource({"HiZ", "r32_mips"});
    const FResourceHandle Visibility = Builder.CreateResource({"Visibility", "u32_buffer"});

    (void)Builder.AddPass({"DepthPrepass", EQueue::Graphics,
        {{Depth, EAccess::DepthStencilWrite}}});
    (void)Builder.AddPass({"BuildHiZ", EQueue::Compute,
        {{Depth, EAccess::ShaderRead}, {HiZ, EAccess::StorageWrite}}});
    (void)Builder.AddPass({"Cull", EQueue::Compute,
        {{HiZ, EAccess::StorageRead}, {Visibility, EAccess::StorageWrite}}});
    (void)Builder.AddPass({"DrawVisible", EQueue::Graphics,
        {{Visibility, EAccess::ShaderRead}, {BackBuffer, EAccess::ColorAttachmentWrite}}});
    (void)Builder.AddPass({"Present", EQueue::Graphics,
        {{BackBuffer, EAccess::Present}}});

    const FCompileResult Result = Builder.Compile();
    GRAPH_CHECK(Result.Succeeded());
    if (!Result.Succeeded()) return;
    GRAPH_CHECK(Result.Value->Passes.size() == 5);
    GRAPH_CHECK(Result.Value->Passes[0].Name == "DepthPrepass");
    GRAPH_CHECK(Result.Value->Passes[1].Name == "BuildHiZ");
    GRAPH_CHECK(Result.Value->Passes[2].Name == "Cull");
    GRAPH_CHECK(Result.Value->Passes[3].Name == "DrawVisible");
    GRAPH_CHECK(Result.Value->Passes[4].Name == "Present");
    GRAPH_CHECK(Result.Value->Submissions.size() == Result.Value->Passes.size());

    const FQueueSubmission* BuildHiZ = FindSubmission(*Result.Value, "BuildHiZ");
    const FQueueSubmission* Cull = FindSubmission(*Result.Value, "Cull");
    const FQueueSubmission* DrawVisible = FindSubmission(*Result.Value, "DrawVisible");
    GRAPH_CHECK(BuildHiZ && Cull && DrawVisible);
    if (BuildHiZ && Cull && DrawVisible)
    {
        GRAPH_CHECK(BuildHiZ->Waits.size() == 1);
        GRAPH_CHECK(BuildHiZ->Waits[0].SourceQueue == EQueue::Graphics);
        GRAPH_CHECK(Cull->Waits.empty());
        GRAPH_CHECK(DrawVisible->Waits.size() == 1);
        GRAPH_CHECK(DrawVisible->Waits[0].SourceQueue == EQueue::Compute);
    }

    bool DepthQueueTransfer = false;
    bool VisibilityQueueTransfer = false;
    for (const FBarrier& Barrier : Result.Value->Barriers)
    {
        if (Barrier.Resource == Depth)
            DepthQueueTransfer = Barrier.QueueTransfer;
        if (Barrier.Resource == Visibility)
            VisibilityQueueTransfer = Barrier.QueueTransfer;
    }
    GRAPH_CHECK(DepthQueueTransfer);
    GRAPH_CHECK(VisibilityQueueTransfer);
}

void TestReadOnlyQueueTransfer()
{
    TiramisuRenderGraphBuilder Builder;
    const FResourceHandle Imported = Builder.ImportResource({"Imported", "r32"});
    (void)Builder.AddPass({"GraphicsRead", EQueue::Graphics,
        {{Imported, EAccess::ShaderRead}}});
    (void)Builder.AddPass({"ComputeRead", EQueue::Compute,
        {{Imported, EAccess::ShaderRead}}});

    const FCompileResult Result = Builder.Compile();
    GRAPH_CHECK(Result.Succeeded());
    if (!Result.Succeeded()) return;

    bool FoundReadOnlyTransfer = false;
    for (const FBarrier& Barrier : Result.Value->Barriers)
    {
        if (Barrier.Resource == Imported && Barrier.Before == EAccess::ShaderRead &&
            Barrier.After == EAccess::ShaderRead && Barrier.QueueTransfer)
        {
            FoundReadOnlyTransfer = true;
        }
    }
    GRAPH_CHECK(FoundReadOnlyTransfer);

    const FQueueSubmission* ComputeRead = FindSubmission(*Result.Value, "ComputeRead");
    GRAPH_CHECK(ComputeRead != nullptr);
    if (ComputeRead)
    {
        GRAPH_CHECK(ComputeRead->Waits.size() == 1);
        GRAPH_CHECK(ComputeRead->Waits[0].SourceQueue == EQueue::Graphics);
    }
}

void TestTransientAliasing()
{
    TiramisuRenderGraphBuilder Builder;
    const FResourceHandle A = Builder.CreateResource({"A", "rgba16f"});
    const FResourceHandle B = Builder.CreateResource({"B", "rgba16f"});
    const FResourceHandle C = Builder.CreateResource({"C", "rgba16f"});
    const FResourceHandle Imported = Builder.ImportResource({"Imported", "rgba16f"});

    (void)Builder.AddPass({"WriteA", EQueue::Graphics, {{A, EAccess::ColorAttachmentWrite},
        {C, EAccess::ColorAttachmentWrite}}});
    (void)Builder.AddPass({"ReadA", EQueue::Graphics, {{A, EAccess::ShaderRead}}});
    (void)Builder.AddPass({"WriteB", EQueue::Graphics, {{B, EAccess::ColorAttachmentWrite}}});
    (void)Builder.AddPass({"ReadBAndC", EQueue::Graphics, {{B, EAccess::ShaderRead},
        {C, EAccess::ShaderRead}, {Imported, EAccess::ColorAttachmentWrite}}});

    const FCompileResult Result = Builder.Compile();
    GRAPH_CHECK(Result.Succeeded());
    if (!Result.Succeeded()) return;
    const FCompiledResource* CompiledA = FindResource(*Result.Value, A);
    const FCompiledResource* CompiledB = FindResource(*Result.Value, B);
    const FCompiledResource* CompiledC = FindResource(*Result.Value, C);
    const FCompiledResource* CompiledImported = FindResource(*Result.Value, Imported);
    GRAPH_CHECK(CompiledA && CompiledB && CompiledC && CompiledImported);
    if (!CompiledA || !CompiledB || !CompiledC || !CompiledImported) return;
    GRAPH_CHECK(CompiledA->AliasSlot == CompiledB->AliasSlot);
    GRAPH_CHECK(CompiledC->AliasSlot != CompiledA->AliasSlot);
    GRAPH_CHECK(CompiledImported->AliasSlot == FCompiledResource::NoAliasSlot);
    GRAPH_CHECK(Result.Value->AliasSlotCount == 2);
}

void TestValidation()
{
    {
        TiramisuRenderGraphBuilder Builder;
        const FResourceHandle Transient = Builder.CreateResource({"Transient", "r32"});
        (void)Builder.AddPass({"ReadFirst", EQueue::Compute, {{Transient, EAccess::ShaderRead}}});
        const FCompileResult Result = Builder.Compile();
        GRAPH_CHECK(!Result.Succeeded());
        GRAPH_CHECK(HasDiagnostic(Result, "render_graph.read_before_write"));
    }
    {
        TiramisuRenderGraphBuilder Builder;
        const FResourceHandle Resource = Builder.CreateResource({"Resource", "r32"});
        const FPassHandle A = Builder.AddPass({"A", EQueue::Compute,
            {{Resource, EAccess::StorageWrite}}});
        const FPassHandle B = Builder.AddPass({"B", EQueue::Compute,
            {{Resource, EAccess::StorageRead}}});
        Builder.AddDependency(A, B);
        Builder.AddDependency(B, A);
        const FCompileResult Result = Builder.Compile();
        GRAPH_CHECK(!Result.Succeeded());
        GRAPH_CHECK(HasDiagnostic(Result, "render_graph.cycle"));
    }
    {
        TiramisuRenderGraphBuilder Builder;
        const FResourceHandle Stale = Builder.CreateResource({"Old", "r32"});
        Builder.Reset();
        (void)Builder.AddPass({"UsesStale", EQueue::Compute,
            {{Stale, EAccess::StorageWrite}}});
        const FCompileResult Result = Builder.Compile();
        GRAPH_CHECK(!Result.Succeeded());
        GRAPH_CHECK(HasDiagnostic(Result, "render_graph.invalid_resource"));
    }
    {
        TiramisuRenderGraphBuilder Builder;
        const FResourceHandle Color = Builder.CreateResource({"Color", "rgba8"});
        (void)Builder.AddPass({"InvalidComputeAttachment", EQueue::Compute,
            {{Color, EAccess::ColorAttachmentWrite}}});
        const FCompileResult Result = Builder.Compile();
        GRAPH_CHECK(!Result.Succeeded());
        GRAPH_CHECK(HasDiagnostic(Result, "render_graph.queue_access_mismatch"));
    }
}

void TestNriStateMapping()
{
    const auto PixelRead = ToNriAccessState(EAccess::ShaderRead, EQueue::Graphics);
    GRAPH_CHECK(PixelRead.has_value());
    if (PixelRead)
    {
        GRAPH_CHECK(PixelRead->Access == nri::AccessBits::SHADER_RESOURCE);
        GRAPH_CHECK(PixelRead->TextureLayout == nri::Layout::SHADER_RESOURCE);
        GRAPH_CHECK(PixelRead->Stages == nri::StageBits::GRAPHICS_SHADERS);
    }

    const auto ComputeWrite = ToNriAccessState(EAccess::StorageWrite, EQueue::Compute);
    GRAPH_CHECK(ComputeWrite.has_value());
    if (ComputeWrite)
    {
        GRAPH_CHECK(ComputeWrite->Access == nri::AccessBits::SHADER_RESOURCE_STORAGE);
        GRAPH_CHECK(ComputeWrite->TextureLayout == nri::Layout::SHADER_RESOURCE_STORAGE);
        GRAPH_CHECK(ComputeWrite->Stages == nri::StageBits::COMPUTE_SHADER);
    }

    const auto Indirect = ToNriAccessState(EAccess::IndirectArgument, EQueue::Graphics);
    GRAPH_CHECK(Indirect.has_value());
    if (Indirect)
    {
        GRAPH_CHECK(Indirect->Access == nri::AccessBits::ARGUMENT_BUFFER);
        GRAPH_CHECK(Indirect->Stages == nri::StageBits::INDIRECT);
    }

    GRAPH_CHECK(!ToNriAccessState(EAccess::ColorAttachmentWrite,
        EQueue::Compute).has_value());
    GRAPH_CHECK(!ToNriAccessState(EAccess::ShaderRead, EQueue::Copy).has_value());
    GRAPH_CHECK(!ToNriAccessState(EAccess::Present, EQueue::Compute).has_value());
}

void TestNriBarrierBatch()
{
    TiramisuRenderGraphBuilder Builder;
    const FResourceHandle Depth = Builder.CreateResource({"Depth", "d32"});
    const FResourceHandle Visibility = Builder.CreateResource({"Visibility", "u32_buffer"});
    (void)Builder.AddPass({"Depth", EQueue::Graphics,
        {{Depth, EAccess::DepthStencilWrite}}});
    (void)Builder.AddPass({"Cull", EQueue::Compute,
        {{Depth, EAccess::ShaderRead}, {Visibility, EAccess::StorageWrite}}});
    (void)Builder.AddPass({"Draw", EQueue::Graphics,
        {{Visibility, EAccess::IndirectArgument}}});

    const FCompileResult Compiled = Builder.Compile();
    GRAPH_CHECK(Compiled.Succeeded());
    if (!Compiled.Succeeded()) return;

    const FQueueSubmission* Cull = FindSubmission(*Compiled.Value, "Cull");
    const FQueueSubmission* Draw = FindSubmission(*Compiled.Value, "Draw");
    GRAPH_CHECK(Cull && Draw);
    if (!Cull || !Draw) return;

    auto* DepthTexture = reinterpret_cast<nri::Texture*>(std::uintptr_t{0x1000});
    auto* VisibilityBuffer = reinterpret_cast<nri::Buffer*>(std::uintptr_t{0x2000});
    auto* GraphicsQueue = reinterpret_cast<nri::Queue*>(std::uintptr_t{0x3000});
    auto* ComputeQueue = reinterpret_cast<nri::Queue*>(std::uintptr_t{0x4000});
    const FNriResourceBinding Bindings[] = {
        {.Resource = Depth, .Texture = DepthTexture,
            .ExclusiveQueueOwnership = true},
        {.Resource = Visibility, .Buffer = VisibilityBuffer},
    };
    const FNriQueueBindings Queues{GraphicsQueue, ComputeQueue, nullptr};

    const FNriBarrierBatch CullBatch = BuildNriBarrierBatch(
        *Compiled.Value, *Cull, Bindings, Queues);
    GRAPH_CHECK(CullBatch.Succeeded());
    GRAPH_CHECK(CullBatch.TextureBarriers.size() == 1);
    GRAPH_CHECK(CullBatch.BufferBarriers.size() == 1);
    if (!CullBatch.TextureBarriers.empty())
    {
        GRAPH_CHECK(CullBatch.TextureBarriers[0].srcQueue == GraphicsQueue);
        GRAPH_CHECK(CullBatch.TextureBarriers[0].dstQueue == ComputeQueue);
        GRAPH_CHECK(CullBatch.TextureBarriers[0].after.layout ==
            nri::Layout::SHADER_RESOURCE);
    }

    const FNriBarrierBatch DrawBatch = BuildNriBarrierBatch(
        *Compiled.Value, *Draw, Bindings, Queues);
    GRAPH_CHECK(DrawBatch.Succeeded());
    GRAPH_CHECK(DrawBatch.TextureBarriers.empty());
    GRAPH_CHECK(DrawBatch.BufferBarriers.size() == 1);
    if (!DrawBatch.BufferBarriers.empty())
    {
        GRAPH_CHECK(DrawBatch.BufferBarriers[0].after.access ==
            nri::AccessBits::ARGUMENT_BUFFER);
        GRAPH_CHECK(DrawBatch.BufferBarriers[0].after.stages ==
            nri::StageBits::INDIRECT);
    }

    const FNriBarrierBatch MissingBinding = BuildNriBarrierBatch(
        *Compiled.Value, *Cull, {}, Queues);
    GRAPH_CHECK(!MissingBinding.Succeeded());
}
} // namespace

int main()
{
    TestAsyncComputeFlow();
    TestReadOnlyQueueTransfer();
    TestTransientAliasing();
    TestValidation();
    TestNriStateMapping();
    TestNriBarrierBatch();
    if (Failures != 0)
        return EXIT_FAILURE;
    std::cout << "Tiramisu render graph tests passed.\n";
    return EXIT_SUCCESS;
}
