#include "../SceneAsset.h"
#include "../SceneConversionDump.h"

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <limits>

using namespace Tiramisu::Scene;

namespace
{
int Fail(const char* Message)
{
    std::cerr << Message << '\n';
    return 1;
}

bool HasDiagnostic(const xr_vector<FSceneDiagnostic>& Diagnostics,
    const xr_string_view Code)
{
    for (const FSceneDiagnostic& Diagnostic : Diagnostics)
    {
        if (Diagnostic.Code == Code)
            return true;
    }
    return false;
}
} // namespace

int main()
{
    FStaticMeshAsset Mesh;
    Mesh.Id = "a520c30d-b7a7-4a65-9915-558343d43d9a";
    Mesh.Name = "Triangle";
    Mesh.MaterialSlots.push_back(
        {"Surface", "128e21af-5c6f-4ec4-a2e3-8b44f90cb553", true});
    Mesh.Vertices = {
        {{{-1.0f, -1.0f, 0.0f}}},
        {{{0.0f, 1.0f, 0.0f}}},
        {{{1.0f, -1.0f, 0.0f}}}};
    Mesh.Indices = {0, 1, 2};
    Mesh.Sections.push_back({0, 3, 0});

    FStaticMeshAsset LegacyMesh = Mesh;
    LegacyMesh.Version = LegacyInlineStaticMeshAssetVersion;
    const xr_string LegacyMeshJson =
        SerializeStaticMeshAssetJson(LegacyMesh);
    const FStaticMeshAssetParseResult ParsedMesh =
        ParseStaticMeshAssetJson(
            LegacyMeshJson, "triangle.static-mesh.json");
    if (!ParsedMesh.Succeeded() || ParsedMesh.Value.Vertices.size() != 3 ||
        ParsedMesh.Value.MaterialSlots[0].Material !=
            Mesh.MaterialSlots[0].Material ||
        !ParsedMesh.Value.MaterialSlots[0].TwoSided ||
        CalculateStaticMeshRevision(ParsedMesh.Value) == 0)
    {
        return Fail("Legacy inline static-mesh compatibility failed");
    }

    const std::filesystem::path BinaryRoot =
        std::filesystem::temp_directory_path() /
        ("ixray-scene-binary-" + std::to_string(
            std::chrono::steady_clock::now().time_since_epoch().count()));
    struct FBinaryCleanup
    {
        std::filesystem::path Path;
        ~FBinaryCleanup()
        {
            std::error_code Error;
            std::filesystem::remove_all(Path, Error);
        }
    } BinaryCleanup{BinaryRoot};
    std::error_code BinaryError;
    std::filesystem::create_directories(BinaryRoot, BinaryError);
    if (BinaryError)
        return Fail("Cannot create binary static-mesh test directory");
    const std::filesystem::path BinaryMeshPath =
        BinaryRoot / "triangle.static-mesh.json";
    const FStaticMeshAssetWriteResult BinaryWrite =
        SaveStaticMeshAsset(BinaryMeshPath, Mesh);
    const FStaticMeshAssetParseResult BinaryMesh =
        LoadStaticMeshAsset(BinaryMeshPath);
    std::ifstream MetadataInput(BinaryMeshPath, std::ios::binary);
    const std::string StandardBinaryMetadata{
        std::istreambuf_iterator<char>(MetadataInput),
        std::istreambuf_iterator<char>()};
    const xr_string BinaryMetadata(StandardBinaryMetadata);
    MetadataInput.close();
    if (!BinaryWrite.Succeeded() || !BinaryMesh.Succeeded() ||
        BinaryMesh.Value.Version != StaticMeshAssetVersion ||
        BinaryMesh.Value.Vertices.size() != 3 ||
        BinaryMesh.Value.Indices != Mesh.Indices ||
        BinaryMetadata.find("\"vertices\"") != xr_string::npos ||
        BinaryMetadata.find("\"indices\"") != xr_string::npos ||
        !std::filesystem::is_regular_file(BinaryWrite.GeometryPath))
    {
        return Fail("Binary static-mesh round trip failed");
    }
    {
        std::fstream Corrupt(BinaryWrite.GeometryPath,
            std::ios::binary | std::ios::in | std::ios::out);
        Corrupt.seekg(-1, std::ios::end);
        char Byte = 0;
        Corrupt.read(&Byte, 1);
        Byte ^= 0x1;
        Corrupt.seekp(-1, std::ios::end);
        Corrupt.write(&Byte, 1);
    }
    const FStaticMeshAssetParseResult CorruptBinary =
        LoadStaticMeshAsset(BinaryMeshPath);
    if (CorruptBinary.Succeeded() ||
        !HasDiagnostic(CorruptBinary.Diagnostics,
            "static_mesh.binary_header_mismatch"))
    {
        return Fail("Corrupt static-mesh binary hash was accepted");
    }
    if (!SaveStaticMeshAsset(BinaryMeshPath, Mesh).Succeeded())
        return Fail("Cannot restore binary static-mesh test asset");
    std::filesystem::remove(
        MakeStaticMeshGeometryPath(BinaryMeshPath), BinaryError);
    const FStaticMeshAssetParseResult MissingBinary =
        LoadStaticMeshAsset(BinaryMeshPath);
    if (MissingBinary.Succeeded() ||
        !HasDiagnostic(MissingBinary.Diagnostics,
            "static_mesh.binary_read_failed"))
    {
        return Fail("Missing static-mesh binary payload was accepted");
    }

    FStaticMeshAsset InvalidMesh = Mesh;
    InvalidMesh.Indices[2] = 3;
    const FStaticMeshAssetWriteResult InvalidIndex =
        SaveStaticMeshAsset(BinaryRoot / "invalid.static-mesh.json",
            InvalidMesh);
    if (InvalidIndex.Succeeded() ||
        !HasDiagnostic(InvalidIndex.Diagnostics,
            "static_mesh.index_out_of_range"))
    {
        return Fail("Out-of-range static-mesh index was accepted");
    }

    InvalidMesh = Mesh;
    InvalidMesh.MaterialSlots[0].Material.clear();
    const FStaticMeshAssetWriteResult MissingMaterial =
        SaveStaticMeshAsset(BinaryRoot / "missing.static-mesh.json",
            InvalidMesh);
    if (MissingMaterial.Succeeded() ||
        !HasDiagnostic(MissingMaterial.Diagnostics,
            "static_mesh.invalid_material_slot"))
    {
        return Fail("Static-mesh slot without a material was accepted");
    }

    FRenderSceneAsset Scene;
    Scene.Id = "eb39395e-57b0-4bd7-a79b-f81062cf36ec";
    Scene.Name = "Native scene";
    FStaticMeshComponent Component;
    Component.Id = "294d16b8-ac73-4555-ae4c-4b56cde96256";
    Component.Name = "Triangle component";
    Component.StaticMesh = "triangle.static-mesh.json";
    Component.MaterialOverrides.push_back(
        {0, "74dfac8a-6739-4253-804a-5d3369df759b", false});
    Scene.StaticMeshComponents.push_back(Component);
    FLightComponent Light;
    Light.Id = "5ad630fa-765d-42cf-93f8-c07ed68b9232";
    Light.Name = "Warm spot";
    Light.Type = ELightType::Spot;
    Light.LocalToWorld[12] = 2.0f;
    Light.LocalToWorld[13] = 3.0f;
    Light.LocalToWorld[14] = 4.0f;
    Light.Color = {1.0f, 0.75f, 0.5f};
    Light.Intensity = 8.0f;
    Light.Range = 25.0f;
    Light.InnerConeAngleDegrees = 15.0f;
    Light.OuterConeAngleDegrees = 35.0f;
    Scene.LightComponents.push_back(Light);
    const FRenderSceneAssetParseResult ParsedScene =
        ParseRenderSceneAssetJson(SerializeRenderSceneAssetJson(Scene));
    if (!ParsedScene.Succeeded() ||
        ParsedScene.Value.Version != RenderSceneAssetVersion ||
        ParsedScene.Value.StaticMeshComponents.size() != 1 ||
        ParsedScene.Value.StaticMeshComponents[0].MaterialOverrides.size() !=
            1 ||
        ParsedScene.Value.LightComponents.size() != 1 ||
        ParsedScene.Value.LightComponents[0].Type != ELightType::Spot ||
        ParsedScene.Value.LightComponents[0].Color != Light.Color ||
        ParsedScene.Value.LightComponents[0].Intensity != Light.Intensity ||
        ToString(ParsedScene.Value.LightComponents[0].Type) != "spot")
    {
        return Fail("Valid render-scene round trip failed");
    }
    ELightType ParsedLightType = ELightType::Point;
    if (!TryParseLightType("directional", ParsedLightType) ||
        ParsedLightType != ELightType::Directional ||
        TryParseLightType("area", ParsedLightType))
    {
        return Fail("Light type conversion failed");
    }

    FRenderSceneAsset LegacyScene = Scene;
    LegacyScene.Version = LegacyStaticMeshOnlyRenderSceneAssetVersion;
    LegacyScene.LightComponents.clear();
    const FRenderSceneAssetParseResult ParsedLegacyScene =
        ParseRenderSceneAssetJson(
            SerializeRenderSceneAssetJson(LegacyScene));
    if (!ParsedLegacyScene.Succeeded() ||
        ParsedLegacyScene.Value.Version !=
            LegacyStaticMeshOnlyRenderSceneAssetVersion ||
        !ParsedLegacyScene.Value.LightComponents.empty())
    {
        return Fail("Render-scene v1 compatibility failed");
    }
    LegacyScene.LightComponents.push_back(Light);
    const FRenderSceneAssetParseResult LegacySceneWithLight =
        ParseRenderSceneAssetJson(
            SerializeRenderSceneAssetJson(LegacyScene));
    if (LegacySceneWithLight.Succeeded() ||
        !HasDiagnostic(LegacySceneWithLight.Diagnostics,
            "scene.light_requires_version_2"))
    {
        return Fail("Render-scene v1 accepted a light component");
    }

    FRenderSceneAsset InvalidLightScene = Scene;
    InvalidLightScene.LightComponents[0].Id = Component.Id;
    const FRenderSceneAssetParseResult DuplicateLight =
        ParseRenderSceneAssetJson(
            SerializeRenderSceneAssetJson(InvalidLightScene));
    if (DuplicateLight.Succeeded() ||
        !HasDiagnostic(DuplicateLight.Diagnostics,
            "scene.invalid_light_guid"))
    {
        return Fail("Duplicate cross-type scene GUID was accepted");
    }
    InvalidLightScene = Scene;
    InvalidLightScene.LightComponents[0].Range = 0.0f;
    const FRenderSceneAssetParseResult InvalidLightRange =
        ParseRenderSceneAssetJson(
            SerializeRenderSceneAssetJson(InvalidLightScene));
    if (InvalidLightRange.Succeeded() ||
        !HasDiagnostic(InvalidLightRange.Diagnostics,
            "scene.invalid_light_range"))
    {
        return Fail("Invalid local-light range was accepted");
    }
    InvalidLightScene = Scene;
    InvalidLightScene.LightComponents[0].Color[1] = -0.1f;
    const FRenderSceneAssetParseResult InvalidLightColor =
        ParseRenderSceneAssetJson(
            SerializeRenderSceneAssetJson(InvalidLightScene));
    if (InvalidLightColor.Succeeded() ||
        !HasDiagnostic(InvalidLightColor.Diagnostics,
            "scene.invalid_light_radiometry"))
    {
        return Fail("Negative light color was accepted");
    }
    InvalidLightScene = Scene;
    InvalidLightScene.LightComponents[0].InnerConeAngleDegrees = 50.0f;
    const FRenderSceneAssetParseResult InvalidLightCone =
        ParseRenderSceneAssetJson(
            SerializeRenderSceneAssetJson(InvalidLightScene));
    if (InvalidLightCone.Succeeded() ||
        !HasDiagnostic(InvalidLightCone.Diagnostics,
            "scene.invalid_light_cone"))
    {
        return Fail("Invalid spot-light cone was accepted");
    }
    xr_string InvalidLightType = SerializeRenderSceneAssetJson(Scene);
    const size_t LightType =
        InvalidLightType.find("\"type\": \"spot\"");
    if (LightType == xr_string::npos)
        return Fail("Serialized scene has no spot-light type");
    InvalidLightType.replace(
        LightType, xr_string("\"type\": \"spot\"").size(),
        "\"type\": \"area\"");
    const FRenderSceneAssetParseResult UnknownLightType =
        ParseRenderSceneAssetJson(InvalidLightType);
    if (UnknownLightType.Succeeded() ||
        !HasDiagnostic(UnknownLightType.Diagnostics,
            "scene.invalid_light_fields"))
    {
        return Fail("Unknown light type was accepted");
    }

    Scene.StaticMeshComponents.push_back(Component);
    const FRenderSceneAssetParseResult DuplicateComponent =
        ParseRenderSceneAssetJson(SerializeRenderSceneAssetJson(Scene));
    if (DuplicateComponent.Succeeded() ||
        !HasDiagnostic(DuplicateComponent.Diagnostics,
            "scene.invalid_component_guid"))
    {
        return Fail("Duplicate scene component GUID was accepted");
    }
    Scene.StaticMeshComponents.pop_back();
    Scene.StaticMeshComponents[0].LocalToWorld[0] =
        std::numeric_limits<float>::quiet_NaN();
    // JSON cannot represent NaN portably; validate the wrong field type path.
    xr_string InvalidTransform = SerializeRenderSceneAssetJson(Scene);
    const size_t Transform = InvalidTransform.find("\"transform\"");
    if (Transform == xr_string::npos)
        return Fail("Serialized scene has no transform");
    const size_t ArrayBegin = InvalidTransform.find('[', Transform);
    const size_t NumberEnd = InvalidTransform.find(',', ArrayBegin);
    InvalidTransform.replace(ArrayBegin + 1, NumberEnd - ArrayBegin - 1,
        "\"nan\"");
    const FRenderSceneAssetParseResult NonFinite =
        ParseRenderSceneAssetJson(InvalidTransform);
    if (NonFinite.Succeeded() ||
        !HasDiagnostic(NonFinite.Diagnostics,
            "scene.invalid_component_fields"))
    {
        return Fail("Invalid scene transform was accepted");
    }

    const std::filesystem::path TemporaryRoot =
        std::filesystem::temp_directory_path() /
        ("ixray-scene-core-" + std::to_string(
            std::chrono::steady_clock::now().time_since_epoch().count()));
    struct FCleanup
    {
        std::filesystem::path Path;
        ~FCleanup()
        {
            std::error_code Error;
            std::filesystem::remove_all(Path, Error);
        }
    } Cleanup{TemporaryRoot};
    std::error_code Error;
    std::filesystem::create_directories(TemporaryRoot, Error);
    if (Error)
        return Fail("Cannot create scene test directory");
    Scene.StaticMeshComponents[0].LocalToWorld = FStaticMeshComponent{}.LocalToWorld;
    const FStaticMeshAssetWriteResult SceneMeshWrite =
        SaveStaticMeshAsset(
            TemporaryRoot / "triangle.static-mesh.json", Mesh);
    if (!SceneMeshWrite.Succeeded())
        return Fail("Cannot write scene static-mesh pair");
    {
        std::ofstream Output(TemporaryRoot / "native.render-scene.json",
            std::ios::binary);
        Output << SerializeRenderSceneAssetJson(Scene);
    }
    const FResolvedRenderSceneResult Loaded = LoadRenderSceneAsset(
        TemporaryRoot / "native.render-scene.json");
    if (!Loaded.Succeeded() || Loaded.Value.StaticMeshes.size() != 1 ||
        Loaded.Value.Scene.StaticMeshComponents[0]
                .MaterialOverrides[0].MaterialSlot != 0)
        return Fail("Scene and referenced static mesh did not load");

    FRenderSceneAsset InvalidOverrideScene = Scene;
    InvalidOverrideScene.StaticMeshComponents[0]
        .MaterialOverrides[0].MaterialSlot = 1;
    {
        std::ofstream Output(TemporaryRoot /
            "invalid-override.render-scene.json", std::ios::binary);
        Output << SerializeRenderSceneAssetJson(InvalidOverrideScene);
    }
    const FResolvedRenderSceneResult InvalidOverride =
        LoadRenderSceneAsset(TemporaryRoot /
            "invalid-override.render-scene.json");
    if (InvalidOverride.Succeeded() ||
        !HasDiagnostic(InvalidOverride.Diagnostics,
            "scene.material_override_out_of_range"))
    {
        return Fail("Out-of-range component material override was accepted");
    }

    FSceneConversionDump Dump;
    Dump.Status = ESceneConversionStatus::Succeeded;
    Dump.Importer = "legacy_object";
    Dump.SourceType = "object";
    Dump.SourcePath = "rawdata/objects/triangle.object";
    Dump.SourceHash = "0123456789abcdef";
    Dump.TargetPath =
        "gamedata/render_static_meshes/triangle.static-mesh.json";
    Dump.TargetPayloadPath =
        "gamedata/render_static_meshes/triangle.static-mesh.bin";
    Dump.TargetAssetId = Mesh.Id;
    Dump.MeshCount = 1;
    Dump.VertexCount = 3;
    Dump.IndexCount = 3;
    Dump.CreatedMaterialInstances = 1;
    Dump.AssetMappings.push_back({
        Dump.SourcePath, Dump.TargetPath,
        Dump.TargetPath + ".migration.json", Mesh.Id,
        Dump.TargetPayloadPath});
    Dump.MaterialMappings.push_back(
        {"Surface", "legacy-key", Mesh.MaterialSlots[0].Material,
            false, true});
    Dump.Diagnostics.push_back(
        {"warning", "import.test", "Test diagnostic"});
    const FSceneConversionDumpParseResult ParsedDump =
        ParseSceneConversionDumpJson(
            SerializeSceneConversionDumpJson(Dump));
    if (!ParsedDump.Succeeded() ||
        ParsedDump.Value.Status != ESceneConversionStatus::Succeeded ||
        ParsedDump.Value.AssetMappings.size() != 1 ||
        ParsedDump.Value.TargetPayloadPath != Dump.TargetPayloadPath ||
        ParsedDump.Value.AssetMappings[0].TargetPayload !=
            Dump.TargetPayloadPath ||
        ParsedDump.Value.MaterialMappings.size() != 1 ||
        ParsedDump.Value.Diagnostics.size() != 1)
    {
        return Fail("Conversion dump round trip failed");
    }

    return 0;
}
