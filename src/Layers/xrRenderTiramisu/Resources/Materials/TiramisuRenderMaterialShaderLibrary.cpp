#include "stdafx.h"
#include "TiramisuRenderMaterialShaderLibrary.h"

#include <vector>

TiramisuRenderMaterialShaderLibrary::TiramisuRenderMaterialShaderLibrary()
{
    CheckIsGameThread();
    VERIFY(!IsRenderThreadRunning());

    IReader* Reader = FS.r_open("$game_render_materials$", "materials.xrm");
    if (!Reader)
    {
        Msg("! Tiramisu: render_materials\\materials.xrm is missing; cooked material pipelines are disabled.");
        return;
    }

    xr_vector<u8> Data(static_cast<size_t>(Reader->length()));
    if (!Data.empty())
        CopyMemory(Data.data(), Reader->pointer(), Data.size());
    FS.r_close(Reader);

    FMaterialShaderLibraryBuildOptions Options;
    Options.Format = GRenderDevice.GraphicsApi == nri::GraphicsAPI::D3D12 ? EMaterialShaderBlobFormat::Dxil : EMaterialShaderBlobFormat::SpirV;
    // Bundle v2 still contains development JSON records. Cooked mode will switch
    // this to true when binary flattened material records land in bundle v3.
    Options.RequireCompleteShaderSet = false;

    FMaterialShaderLibraryBuildResult Loaded =
        TiramisuMaterialShaderLibrary::Deserialize(Data, Options);
    for (const FMaterialDiagnostic& Diagnostic : Loaded.Diagnostics)
    {
        Msg("%s Tiramisu material bundle [%s]: %s",
            Diagnostic.Severity == EMaterialDiagnosticSeverity::Error ? "!" : "*",
            Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
    }
    if (!Loaded.Succeeded())
    {
        Msg("! Tiramisu: material shader bundle was rejected; legacy pipelines remain active.");
        return;
    }

    Library = std::move(Loaded.Value);
    Msg("* Tiramisu: loaded %zu material shader programs for %s.",
        Library->GetProgramCount(),
        Options.Format == EMaterialShaderBlobFormat::Dxil ? "D3D12" : "Vulkan");
}

TiramisuRenderMaterialShaderLibrary::~TiramisuRenderMaterialShaderLibrary()
{
    CheckIsGameThread();
    VERIFY(!IsRenderThreadRunning());
}

xr_optional<FMaterialShaderProgramView>
TiramisuRenderMaterialShaderLibrary::Find_RenderThread(
    const FMaterialAssetId& MaterialId,
    const EMaterialPass Pass) const
{
    CheckIsRenderThread();
    if (!Library)
        return std::nullopt;

    const FMaterialPassDefinition* Definition =
        FindMaterialPassDefinition(Pass);
    if (!Definition)
        return std::nullopt;
    return Library->Find(MaterialId, Pass, Definition->VertexFactory,
        Definition->RenderPassSignature);
}

const FResolvedMaterialInstance*
TiramisuRenderMaterialShaderLibrary::ResolveMaterial_RenderThread(
    const FMaterialAssetId& MaterialId) const
{
    CheckIsRenderThread();
    return Library ? Library->ResolveMaterial(MaterialId) : nullptr;
}

const FMaterialAsset*
TiramisuRenderMaterialShaderLibrary::ResolveMaster_RenderThread(
    const FMaterialAssetId& MaterialId) const
{
    CheckIsRenderThread();
    return Library ? Library->ResolveMasterMaterial(MaterialId) : nullptr;
}
