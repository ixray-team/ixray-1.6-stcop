#include "LegacyMaterialResolver.h"
#include "MaterialAsset.h"
#include "MaterialParameterLayout.h"
#include "MaterialTestHarness.h"

#include <array>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

namespace
{
xr_string ReadText(const std::filesystem::path& Path)
{
    std::ifstream Stream(Path, std::ios::binary);
    std::ostringstream Text;
    Text << Stream.rdbuf();
    return Text.str();
}

struct FLegacyCase
{
    const char* Shader;
    const char* Master;
    xr_optional<bool> UseVertexColor;
    xr_optional<bool> UseLightmap;
};

template <typename T>
T ReadPackedValue(const xr_vector<u8>& Data, const u32 Offset)
{
    T Value{};
    std::memcpy(&Value, Data.data() + Offset, sizeof(T));
    return Value;
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrLegacyMaterialBridgeTests");
    TiramisuMaterialLibrary Library;
    xr_vector<FMaterialAssetId> SourceAssets;

    for (const char* Path : {
        "gamedata/render_materials/error.material.json",
        "gamedata/render_materials/example_graph.material.json",
        "gamedata/render_materials/legacy_opaque.material.json",
        "gamedata/render_materials/legacy_masked.material.json",
        "gamedata/render_materials/legacy_emissive.material.json",
        "gamedata/render_materials/standard_surface.material.json"})
    {
        FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(ReadText(Path), Path);
        MATERIAL_CHECK(Runner, Parsed.Succeeded());
        SourceAssets.push_back(Parsed.Value.Id);
        MATERIAL_CHECK(Runner, Library.RegisterMaster(std::move(Parsed.Value)).Succeeded());
    }

    for (const char* Path : {
        "gamedata/render_materials/example_normal_enabled.material-instance.json",
        "gamedata/render_materials/example_red.material-instance.json",
        "gamedata/render_materials/legacy_default.material-instance.json",
        "gamedata/render_materials/legacy_vertex.material-instance.json",
        "gamedata/render_materials/legacy_lmap.material-instance.json",
        "gamedata/render_materials/legacy_default_aref.material-instance.json",
        "gamedata/render_materials/legacy_vertex_aref.material-instance.json",
        "gamedata/render_materials/legacy_lmap_aref.material-instance.json",
        "gamedata/render_materials/legacy_selflight.material-instance.json"})
    {
        FMaterialInstanceParseResult Parsed = ParseMaterialInstanceJson(ReadText(Path), Path);
        MATERIAL_CHECK(Runner, Parsed.Succeeded());
        SourceAssets.push_back(Parsed.Value.Id);
        MATERIAL_CHECK(Runner, Library.RegisterInstance(std::move(Parsed.Value)).Succeeded());
    }

    MATERIAL_CHECK(Runner, SourceAssets.size() == 15);
    for (const FMaterialAssetId& AssetId : SourceAssets)
    {
        const FMaterialResolveResult SourceResolved = Library.Resolve(AssetId.Value);
        MATERIAL_CHECK(Runner, SourceResolved.Succeeded());
        MATERIAL_CHECK(Runner,
            Library.GetMaster(SourceResolved.Value.MasterHandle) != nullptr);
    }

    const FLegacyMaterialMapParseResult Map =
        ParseLegacyMaterialMapJson(ReadText("gamedata/render_materials/legacy-map.json"));
    MATERIAL_CHECK(Runner, Map.Succeeded());

    const FMaterialParameterId UseVertexColor{"0d987e11-951d-43f3-b2a8-5d47d3b10ba2"};
    const FMaterialParameterId UseLightmap{"e5d7660f-f7d5-4ccd-9be2-602174fa12aa"};
    const xr_array Cases = {
        FLegacyCase{"DEFAULT", "a0f4d9f3-749c-4bc5-bf64-733927e19b20", false, false},
        FLegacyCase{"vertex", "a0f4d9f3-749c-4bc5-bf64-733927e19b20", true, false},
        FLegacyCase{"lmap", "a0f4d9f3-749c-4bc5-bf64-733927e19b20", false, true},
        FLegacyCase{"default_aref", "5f4b3a8e-85fe-42bc-9080-f71eb497f04b", false, false},
        FLegacyCase{"vertex_aref", "5f4b3a8e-85fe-42bc-9080-f71eb497f04b", true, false},
        FLegacyCase{"lmap_aref", "5f4b3a8e-85fe-42bc-9080-f71eb497f04b", false, true},
        FLegacyCase{"models/selflight", "f8ab2e31-cc13-4c4e-9dce-e621d92353aa", {}, {}},
    };

    for (const FLegacyCase& Case : Cases)
    {
        FLegacyMaterialRequest Request;
        Request.ShaderName = Case.Shader;
        Request.Textures = {"levels\\test\\brick", "levels\\test\\brick_lmap"};
        const FResolvedLegacyMaterial Legacy = ResolveLegacyMaterial(Map.Value, Request);
        MATERIAL_CHECK(Runner, Legacy.Resolution == ELegacyMaterialResolution::LegacyMap);
        const FMaterialResolveResult Resolved = Library.Resolve(Legacy.Material);
        MATERIAL_CHECK(Runner, Resolved.Succeeded());
        MATERIAL_CHECK(Runner, Resolved.Value.MasterId.Value == Case.Master);
        if (Case.UseVertexColor)
            MATERIAL_CHECK(Runner, std::get<bool>(Resolved.Value.StaticParameters.at(UseVertexColor)) == *Case.UseVertexColor);
        if (Case.UseLightmap)
            MATERIAL_CHECK(Runner, std::get<bool>(Resolved.Value.StaticParameters.at(UseLightmap)) == *Case.UseLightmap);

        TiramisuMaterialInstanceDynamic Dynamic = Library.CreateDynamic(Resolved.Value);
        MATERIAL_CHECK(Runner, Dynamic.GetMaster() == Resolved.Value.MasterId);
        MATERIAL_CHECK(Runner, Dynamic.GetStaticParameters() == Resolved.Value.StaticParameters);

        const FMaterialParameterMap RuntimeOverrides =
            MakeLegacyMaterialRuntimeOverrides(Legacy);
        MATERIAL_CHECK(Runner, std::get<xr_string>(RuntimeOverrides.at(
            FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)})) ==
            "levels\\test\\brick");
        MATERIAL_CHECK(Runner, std::get<xr_string>(RuntimeOverrides.at(
            FMaterialParameterId{xr_string(LegacyLightmapTextureParameterId)})) ==
            "levels\\test\\brick_lmap");

        if (xr_string_view(Case.Shader) == "lmap")
        {
            const FMaterialAsset* Master = Library.GetMaster(Resolved.Value.MasterHandle);
            MATERIAL_CHECK(Runner, Master != nullptr);
            TiramisuMaterialInstanceDynamic RuntimeInstance = Library.CreateDynamic(Resolved.Value);
            for (const auto& [ParameterId, Value] : RuntimeOverrides)
            {
                const FMaterialParameterDefinition* Definition =
                    Master->FindParameter(ParameterId);
                MATERIAL_CHECK(Runner, Definition != nullptr);
                MATERIAL_CHECK(Runner,
                    RuntimeInstance.SetParameter(*Definition, Value) ==
                        EMaterialUpdateError::None);
            }

            const FMaterialParameterLayoutResult Layout =
                BuildMaterialParameterLayout(Master->Parameters);
            MATERIAL_CHECK(Runner, Layout.Succeeded());
            const FMaterialParameterPackResult Packed = PackMaterialParameters(
                Layout.Value, Master->Parameters,
                RuntimeInstance.GetRuntimeParameters());
            MATERIAL_CHECK(Runner, Packed.Succeeded());

            const FMaterialParameterId BaseTexture{
                xr_string(LegacyBaseTextureParameterId)};
            const FMaterialParameterId LightmapTexture{
                xr_string(LegacyLightmapTextureParameterId)};
            const FMaterialParameterLayoutField* BaseField =
                Layout.Value.Find(BaseTexture);
            const FMaterialParameterLayoutField* LightmapField =
                Layout.Value.Find(LightmapTexture);
            MATERIAL_CHECK(Runner, BaseField != nullptr);
            MATERIAL_CHECK(Runner, LightmapField != nullptr);
            MATERIAL_CHECK(Runner, BaseField->Offset != LightmapField->Offset);

            const FMaterialParameterPackResult Patched =
                PatchMaterialParameterResources(Packed.Value,
                    [&BaseTexture, &LightmapTexture](
                        const FMaterialParameterResourceReference& Reference)
                        -> xr_optional<FDescriptorHeapIndex>
                    {
                        if (Reference.Parameter == BaseTexture)
                            return FDescriptorHeapIndex{101};
                        if (Reference.Parameter == LightmapTexture)
                            return FDescriptorHeapIndex{202};
                        return std::nullopt;
                    });
            MATERIAL_CHECK(Runner, Patched.Succeeded());
            MATERIAL_CHECK(Runner,
                ReadPackedValue<u32>(Patched.Value.Data,
                    BaseField->Offset) == 101);
            MATERIAL_CHECK(Runner,
                ReadPackedValue<u32>(Patched.Value.Data,
                    LightmapField->Offset) == 202);
        }
    }

    FLegacyMaterialRequest CachedRequest;
    CachedRequest.ShaderName = "default";
    CachedRequest.Textures = {"brick"};
    const FResolvedLegacyMaterial Cached = ResolveLegacyMaterial(Map.Value, CachedRequest);
    MATERIAL_CHECK(Runner, MakeLegacyMaterialInstanceCacheKey(Cached) == MakeLegacyMaterialInstanceCacheKey(Cached));
    CachedRequest.Textures = {"metal"};
    MATERIAL_CHECK(Runner, MakeLegacyMaterialInstanceCacheKey(Cached) !=
        MakeLegacyMaterialInstanceCacheKey(ResolveLegacyMaterial(Map.Value, CachedRequest)));

    return Runner.Finish();
}
