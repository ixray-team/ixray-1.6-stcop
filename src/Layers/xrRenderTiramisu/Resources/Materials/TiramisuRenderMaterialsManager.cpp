#include "TiramisuRenderMaterialsManager.h"

#include "Resources/TiramisuRenderResourcesManager.h"
#include "Resources/Textures/TiramisuRenderTexturesManager.h"

#include <optional>
#include <span>
#include <string>
#include <vector>

namespace
{
xr_optional<xr_string> ReadMaterialAssetSource(const char* FileName)
{
    IReader* Reader = FS.r_open("$game_render_materials$", FileName);
    if (!Reader)
        return std::nullopt;

    xr_string Json(static_cast<const char*>(Reader->pointer()),
        static_cast<size_t>(Reader->length()));
    FS.r_close(Reader);
    return Json;
}

void LogMaterialAssetDiagnostics(
    const xr_span<const FMaterialDiagnostic> Diagnostics,
    const char* Source)
{
    for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
    {
        Msg("%s Tiramisu source material '%s' [%s]: %s",
            Diagnostic.Severity == EMaterialDiagnosticSeverity::Error ? "!" : "*",
            Source, Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
    }
}
}

TiramisuRenderMaterialsManager::TiramisuRenderMaterialsManager()
{
    CheckIsGameThread();
    LoadSourceMaterialAssets();
    LoadLegacyMaterialMap();
}

TiramisuRenderMaterialsManager::~TiramisuRenderMaterialsManager()
{
    CheckIsGameThread();
    VERIFY(Materials.empty());
}

TiramisuRenderMaterialInterface* TiramisuRenderMaterialsManager::Get(const shared_str& InName)
{
    CheckIsGameThread();
    auto Iterator = Materials.find(InName);
    if (Iterator != Materials.end())
    {
        Iterator->second->Counter++;
    }
    else
    {
        Iterator = Materials.emplace(InName, new TiramisuRenderMaterial(InName)).first;
        Iterator->second->Name = InName;
    }
    return Iterator->second;
}

TiramisuRenderMaterialInstanceDynamic* TiramisuRenderMaterialsManager::CreateInstanceDynamic(const shared_str& InName, TiramisuRenderMaterialInterface* Parent)
{
    CheckIsGameThread();
    VERIFY (!Materials.contains(InName));
    VERIFY(Parent);
    
    TiramisuRenderMaterialInstanceDynamic* NewInstanceDynamic= new TiramisuRenderMaterialInstanceDynamic(Copy(Parent));
    NewInstanceDynamic->Name = InName;
    Materials.emplace(InName, NewInstanceDynamic);
    return NewInstanceDynamic;
}

TiramisuRenderMaterialInstanceDynamic* TiramisuRenderMaterialsManager::CreateLegacyInstanceDynamic(
    const shared_str& InName, const shared_str& ShaderName, const xr_vector<shared_str>& TextureNames)
{
    CheckIsGameThread();

    FLegacyMaterialRequest Request;
    Request.ShaderName = ShaderName.c_str();
    Request.Textures.reserve(TextureNames.size());
    for (const shared_str& TextureName : TextureNames)
        Request.Textures.emplace_back(TextureName.c_str());

    const FResolvedLegacyMaterial Resolved =
        ResolveLegacyMaterial(LegacyMaterialMap, Request);
    const shared_str CachedName =
        MakeLegacyMaterialInstanceCacheKey(Resolved).c_str();
    if (const auto Existing = Materials.find(CachedName); Existing != Materials.end())
    {
        Existing->second->Counter++;
        return static_cast<TiramisuRenderMaterialInstanceDynamic*>(Existing->second);
    }

    TiramisuRenderMaterialInterface* Parent = Get(Resolved.Material.c_str());
    TiramisuRenderMaterialInstanceDynamic* Instance = CreateInstanceDynamic(CachedName, Parent);
    Free(Parent);

    const FMaterialParameterMap RuntimeOverrides =
        MakeLegacyMaterialRuntimeOverrides(Resolved);
    for (const auto& [Parameter, Value] : RuntimeOverrides)
    {
        if (!std::holds_alternative<xr_string>(Value))
            continue;
        Instance->SetTextureParameter(Parameter,
            GRenderResourcesManager->TexturesManager->GetTexture(
                std::get<xr_string>(Value).c_str()));
    }

    for (const FMaterialDiagnostic& Diagnostic : Resolved.Diagnostics)
        Msg("! Tiramisu legacy material [%s]: %s", Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
    Msg("* Tiramisu legacy material '%s' -> parent instance '%s' (%s)", ShaderName.c_str(),
        Resolved.Material.c_str(), InName.c_str());
    return Instance;
}

void TiramisuRenderMaterialsManager::LoadSourceMaterialAssets()
{
    CheckIsGameThread();
    if (GRenderResourcesManager->IsCookedMode())
        return;

    FS_FileSet MasterFiles;
    FS_FileSet InstanceFiles;
    FS.file_list(MasterFiles, "$game_render_materials$", FS_ListFiles,
        "*.material.json");
    FS.file_list(InstanceFiles, "$game_render_materials$", FS_ListFiles,
        "*.material-instance.json");

    xr_vector<FMaterialAssetId> RegisteredAssets;
    RegisteredAssets.reserve(MasterFiles.size() + InstanceFiles.size());

    for (const FS_File& File : MasterFiles)
    {
        const xr_optional<xr_string> Json =
            ReadMaterialAssetSource(File.name.c_str());
        if (!Json)
        {
            Msg("! Tiramisu source material '%s' could not be read.",
                File.name.c_str());
            continue;
        }

        FMaterialAssetParseResult Parsed =
            ParseMaterialAssetJson(*Json, File.name.c_str());
        LogMaterialAssetDiagnostics(Parsed.Diagnostics, File.name.c_str());
        if (!Parsed.Succeeded())
            continue;

        const FMaterialAssetId AssetId = Parsed.Value.Id;
        FMaterialRegistrationResult Registered =
            SourceMaterialLibrary.RegisterMaster(std::move(Parsed.Value));
        LogMaterialAssetDiagnostics(Registered.Diagnostics, File.name.c_str());
        if (Registered.Succeeded())
            RegisteredAssets.push_back(AssetId);
    }

    // Register every master first and resolve only after all instances are
    // present. Asset discovery order therefore cannot break parent chains.
    for (const FS_File& File : InstanceFiles)
    {
        const xr_optional<xr_string> Json =
            ReadMaterialAssetSource(File.name.c_str());
        if (!Json)
        {
            Msg("! Tiramisu source material instance '%s' could not be read.",
                File.name.c_str());
            continue;
        }

        FMaterialInstanceParseResult Parsed =
            ParseMaterialInstanceJson(*Json, File.name.c_str());
        LogMaterialAssetDiagnostics(Parsed.Diagnostics, File.name.c_str());
        if (!Parsed.Succeeded())
            continue;

        const FMaterialAssetId AssetId = Parsed.Value.Id;
        FMaterialRegistrationResult Registered =
            SourceMaterialLibrary.RegisterInstance(std::move(Parsed.Value));
        LogMaterialAssetDiagnostics(Registered.Diagnostics, File.name.c_str());
        if (Registered.Succeeded())
            RegisteredAssets.push_back(AssetId);
    }

    for (const FMaterialAssetId& AssetId : RegisteredAssets)
    {
        FMaterialResolveResult Resolved =
            SourceMaterialLibrary.Resolve(AssetId.Value);
        LogMaterialAssetDiagnostics(Resolved.Diagnostics, AssetId.Value.c_str());
        if (Resolved.Succeeded())
            SourceResolvedMaterials.emplace(AssetId, std::move(Resolved.Value));
    }

    Msg("* Tiramisu: loaded %zu/%zu development source material assets.",
        SourceResolvedMaterials.size(), RegisteredAssets.size());
}

const FResolvedMaterialInstance*
TiramisuRenderMaterialsManager::ResolveSourceMaterial_RenderThread(
    const FMaterialAssetId& MaterialId) const
{
    CheckIsRenderThread();
    const auto Material = SourceResolvedMaterials.find(MaterialId);
    return Material == SourceResolvedMaterials.end() ? nullptr : &Material->second;
}

const FMaterialAsset*
TiramisuRenderMaterialsManager::ResolveSourceMaster_RenderThread(
    const FMaterialAssetId& MaterialId) const
{
    CheckIsRenderThread();
    const FResolvedMaterialInstance* Resolved =
        ResolveSourceMaterial_RenderThread(MaterialId);
    return Resolved ? SourceMaterialLibrary.GetMaster(Resolved->MasterHandle) : nullptr;
}

void TiramisuRenderMaterialsManager::LoadLegacyMaterialMap()
{
    CheckIsGameThread();
    LegacyMaterialMap.StandardMaterial = "ee5ffbc0-bd24-4aa8-9e16-50651ca1c269";
    LegacyMaterialMap.ErrorMaterial = "e67b251d-7905-4583-8450-0903c46ec652";

    IReader* Reader = FS.r_open("$game_render_materials$", "legacy-map.json");
    if (!Reader)
    {
        Msg("! Tiramisu: render_materials\\legacy-map.json is missing; using standard fallback.");
        return;
    }

    const xr_string Json(static_cast<const char*>(Reader->pointer()),
        static_cast<size_t>(Reader->length()));
    FS.r_close(Reader);
    const FLegacyMaterialMapParseResult Parsed =
        ParseLegacyMaterialMapJson(Json);
    if (!Parsed.Succeeded())
    {
        for (const FMaterialDiagnostic& Diagnostic : Parsed.Diagnostics)
            Msg("! Tiramisu legacy map [%s]: %s", Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
        return;
    }
    LegacyMaterialMap = Parsed.Value;
}

void TiramisuRenderMaterialsManager::Free(TiramisuRenderMaterialInterface* Material)
{
    CheckIsGameThread();
    if (!Material)
    {
        return;
    }
    
    if (--Material->Counter == 0)
    {
        Materials.erase(Material->Name);
        delete Material;
    }
}

TiramisuRenderMaterialInterface* TiramisuRenderMaterialsManager::Copy(TiramisuRenderMaterialInterface* Material)
{
    CheckIsGameThread();
    Material->Counter++;
    return Material;
}
