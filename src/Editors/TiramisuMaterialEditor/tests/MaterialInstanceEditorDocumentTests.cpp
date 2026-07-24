#include "MaterialInstanceEditorDocument.h"
#include "MaterialInstanceParentResolver.h"
#include "MaterialEditorFileIO.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <filesystem>
#include <string>

using namespace Tiramisu::Editor;

namespace
{
FMaterialAsset MakeParent()
{
    FMaterialAsset Parent;
    Parent.Id.Value = "instance-parent-master";
    Parent.Name = "Parent";
    Parent.HlslTemplate = "materials/MaterialTemplate.hlsl";
    Parent.Implementation.Source = "materials/StandardSurface.hlsl";
    Parent.Parameters = {
        {{"roughness-guid"}, "Roughness", EMaterialParameterType::Scalar, 0.5f},
        {{"tint-guid"}, "Tint", EMaterialParameterType::Color,
            FFloat4{1.0f, 1.0f, 1.0f, 1.0f}},
        {{"feature-guid"}, "Feature", EMaterialParameterType::StaticBool, false},
    };
    return Parent;
}

void TestInstanceEditing(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialInstanceEditorDocument Document;
    MATERIAL_CHECK(Runner, Document.GetInstance().Id.Value.size() == 36);
    MATERIAL_CHECK(Runner, !Document.IsDirty());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetParent("").Diagnostics,
            "editor.instance_parent_missing"));
    MATERIAL_CHECK(Runner,
        Document.SetParent("instance-parent-master").Succeeded());
    Document.SetParentMaterial(MakeParent());

    MATERIAL_CHECK(Runner,
        Document.SetOverride({"roughness-guid"}, 0.25f, false).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.SetOverride({"feature-guid"}, true, true).Succeeded());
    MATERIAL_CHECK(Runner, Document.GetInstance().Overrides.size() == 1);
    MATERIAL_CHECK(Runner, Document.GetInstance().StaticOverrides.size() == 1);
    MATERIAL_CHECK(Runner, Document.ValidateOverrides().Succeeded());
    MATERIAL_CHECK(Runner, Document.IsDirty());

    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetOverride(
            {"roughness-guid"}, true, false).Diagnostics,
            "editor.instance_override_type_mismatch"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetOverride(
            {"roughness-guid"}, 0.25f, true).Diagnostics,
            "editor.instance_override_storage_mismatch"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetOverride(
            {"missing-guid"}, 1.0f, false).Diagnostics,
            "editor.instance_unknown_parameter"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetParent("different-parent").Diagnostics,
            "editor.instance_parent_has_overrides"));

    MATERIAL_CHECK(Runner,
        Document.RemoveOverride({"roughness-guid"}).Succeeded());
    MATERIAL_CHECK(Runner, !Document.GetInstance().Overrides.contains({"roughness-guid"}));
    MATERIAL_CHECK(Runner, Document.Undo());
    MATERIAL_CHECK(Runner, Document.GetInstance().Overrides.contains({"roughness-guid"}));
    MATERIAL_CHECK(Runner, Document.Redo());
}

void TestInstanceJsonAndFileRoundTrip(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialInstanceEditorDocument Document;
    MATERIAL_CHECK(Runner,
        Document.SetParent("instance-parent-master").Succeeded());
    Document.SetParentMaterial(MakeParent());
    MATERIAL_CHECK(Runner,
        Document.SetOverride({"tint-guid"},
            FFloat4{0.2f, 0.3f, 0.4f, 1.0f}, false).Succeeded());

    const xr_string Json = Document.SerializeInstance();
    TiramisuMaterialInstanceEditorDocument Parsed;
    MATERIAL_CHECK(Runner,
        Parsed.OpenInstanceJson(Json, "memory.material-instance.json").Succeeded());
    MATERIAL_CHECK(Runner, Parsed.GetInstance().Id == Document.GetInstance().Id);
    MATERIAL_CHECK(Runner, !Parsed.IsDirty());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Parsed.OpenInstanceJson("{ invalid json").Diagnostics,
            "asset.invalid_json"));

    const std::filesystem::path Path = std::filesystem::temp_directory_path() /
        ("xr-material-instance-editor-" + GenerateMaterialGuid() +
            ".material-instance.json");
    std::error_code Error;
    std::filesystem::remove(Path, Error);
    MATERIAL_CHECK(Runner, Document.SaveInstanceFile(Path).Succeeded());
    MATERIAL_CHECK(Runner, std::filesystem::exists(Path));
    MATERIAL_CHECK(Runner, !Document.IsDirty());

    MATERIAL_CHECK(Runner, Document.SetName("Atomic Instance"));
    MATERIAL_CHECK(Runner, Document.SaveInstanceFile(Path).Succeeded());

    TiramisuMaterialInstanceEditorDocument Loaded;
    MATERIAL_CHECK(Runner, Loaded.OpenInstanceFile(Path).Succeeded());
    MATERIAL_CHECK(Runner, Loaded.GetInstance().Id == Document.GetInstance().Id);
    MATERIAL_CHECK(Runner, Loaded.GetInstance().Name == "Atomic Instance");

    const std::filesystem::path RecoveryPath = Path.string() + ".autosave";
    MATERIAL_CHECK(Runner, Document.SetName("Recovered Instance"));
    MATERIAL_CHECK(Runner, Document.SaveRecoveryFile(RecoveryPath).Succeeded());
    MATERIAL_CHECK(Runner, Document.IsDirty());
    TiramisuMaterialInstanceEditorDocument Recovered;
    MATERIAL_CHECK(Runner,
        Recovered.OpenRecoveryFile(RecoveryPath, Path).Succeeded());
    MATERIAL_CHECK(Runner, Recovered.GetInstance().Name == "Recovered Instance");
    MATERIAL_CHECK(Runner, Recovered.GetInstance().SourcePath == ToXrString(Path.generic_string()));
    MATERIAL_CHECK(Runner, Recovered.IsDirty());

    std::filesystem::remove(Path, Error);
    MATERIAL_CHECK(Runner, !std::filesystem::exists(Path));
    std::filesystem::remove(RecoveryPath, Error);
    MATERIAL_CHECK(Runner, !std::filesystem::exists(RecoveryPath));
}

void TestInstanceMigrationMarksDirty(TiramisuMaterialTestRunner& Runner)
{
    constexpr xr_string_view LegacyJson = R"json({
      "version": 1,
      "id": "legacy-editor-instance",
      "name": "Legacy Instance",
      "parent": "instance-parent-master",
      "overrides": {},
      "static_overrides": {}
    })json";
    TiramisuMaterialInstanceEditorDocument Document;
    const FMaterialEditorOperationResult Result = Document.OpenInstanceJson(
        LegacyJson, "legacy.material-instance.json");
    MATERIAL_CHECK(Runner, Result.Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Result.Diagnostics, "asset.migrated_version_field"));
    MATERIAL_CHECK(Runner, HasDiagnostic(Result.Diagnostics, "asset.migrated_id_field"));
    MATERIAL_CHECK(Runner, Document.IsDirty());
}

void TestInstanceRequiresParentSchemaForOverrides(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialInstanceEditorDocument Document;
    MATERIAL_CHECK(Runner,
        Document.SetParent("instance-parent-master").Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetOverride(
            {"roughness-guid"}, 0.5f, false).Diagnostics,
            "editor.instance_parent_schema_missing"));

    FMaterialInstanceAsset Existing;
    Existing.Id.Value = GenerateMaterialGuid();
    Existing.Parent = "instance-parent-master";
    Existing.Overrides.emplace(FMaterialParameterId{"roughness-guid"}, 0.5f);
    Document.OpenInstance(std::move(Existing));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.ValidateOverrides().Diagnostics,
            "editor.instance_parent_schema_missing"));
}

void TestParentChainResolutionAndFlattening(TiramisuMaterialTestRunner& Runner)
{
    const std::filesystem::path Root = std::filesystem::temp_directory_path() /
        ("xr-material-parent-chain-" + GenerateMaterialGuid());
    std::error_code Error;
    std::filesystem::create_directories(Root, Error);
    MATERIAL_CHECK(Runner, !Error);

    FMaterialAsset Master = MakeParent();
    const std::filesystem::path MasterPath = Root / "parent.material.json";
    MATERIAL_CHECK(Runner, WriteTextFileAtomically(
        MasterPath, SerializeMaterialAssetJson(Master)).Success);

    FMaterialInstanceAsset Parent;
    Parent.Id.Value = "parent-chain-middle";
    Parent.Name = "Middle";
    Parent.Parent = "parent.material.json";
    Parent.Overrides.emplace(FMaterialParameterId{"roughness-guid"}, 0.2f);
    Parent.StaticOverrides.emplace(FMaterialParameterId{"feature-guid"}, true);
    const std::filesystem::path ParentPath = Root /
        "middle.material-instance.json";
    MATERIAL_CHECK(Runner, WriteTextFileAtomically(
        ParentPath, SerializeMaterialInstanceJson(Parent)).Success);

    FMaterialInstanceAsset Child;
    Child.Id.Value = "parent-chain-child";
    Child.Name = "Child";
    Child.Parent = "middle.material-instance.json";
    Child.Overrides.emplace(FMaterialParameterId{"tint-guid"},
        FFloat4{0.1f, 0.2f, 0.3f, 1.0f});

    FMaterialInstanceParentResolution Resolution =
        ResolveMaterialInstanceParent(Root, Child);
    MATERIAL_CHECK(Runner, Resolution.Succeeded());
    if (Resolution.Succeeded())
    {
        MATERIAL_CHECK(Runner, Resolution.Master.Id == Master.Id);
        MATERIAL_CHECK(Runner, Resolution.Parent.ParentChain.size() == 1);
        MATERIAL_CHECK(Runner,
            Resolution.Parent.ParentChain.front() == Parent.Id);
        MATERIAL_CHECK(Runner,
            std::get<float>(Resolution.Parent.Parameters.at(
                FMaterialParameterId{"roughness-guid"})) == 0.2f);
        MATERIAL_CHECK(Runner,
            std::get<bool>(Resolution.Parent.StaticParameters.at(
                FMaterialParameterId{"feature-guid"})));
        MATERIAL_CHECK(Runner, Resolution.Instance.ParentChain.size() == 2);
        MATERIAL_CHECK(Runner, Resolution.AssetDependencies.size() == 2);
        MATERIAL_CHECK(Runner,
            std::ranges::find(Resolution.AssetDependencies, MasterPath) !=
                Resolution.AssetDependencies.end());
        MATERIAL_CHECK(Runner,
            std::ranges::find(Resolution.AssetDependencies, ParentPath) !=
                Resolution.AssetDependencies.end());

        TiramisuMaterialInstanceEditorDocument Document;
        Document.OpenInstance(Child);
        MATERIAL_CHECK(Runner, Document.SetParentResolution(
            Resolution.Master, Resolution.Parent).Succeeded());
        const FMaterialValue* Inherited = Document.GetInheritedValue(
            {"roughness-guid"}, false);
        MATERIAL_CHECK(Runner, Inherited != nullptr);
        if (Inherited)
            MATERIAL_CHECK(Runner, std::get<float>(*Inherited) == 0.2f);
        MATERIAL_CHECK(Runner,
            std::get<bool>(Document.GetEffectiveStaticParameters().at(
                FMaterialParameterId{"feature-guid"})));

        const FMaterialInstanceParseResult Flattened =
            ParseMaterialInstanceJson(Document.SerializeFlattenedInstance());
        MATERIAL_CHECK(Runner, Flattened.Succeeded());
        if (Flattened.Succeeded())
        {
            MATERIAL_CHECK(Runner, Flattened.Value.Parent == Master.Id.Value);
            MATERIAL_CHECK(Runner,
                std::get<float>(Flattened.Value.Overrides.at(
                    FMaterialParameterId{"roughness-guid"})) == 0.2f);
            MATERIAL_CHECK(Runner,
                std::get<FFloat4>(Flattened.Value.Overrides.at(
                    FMaterialParameterId{"tint-guid"})) ==
                    FFloat4({0.1f, 0.2f, 0.3f, 1.0f}));
            MATERIAL_CHECK(Runner,
                std::get<bool>(Flattened.Value.StaticOverrides.at(
                    FMaterialParameterId{"feature-guid"})));
        }
    }

    Parent.Parent = Child.Id.Value;
    MATERIAL_CHECK(Runner, WriteTextFileAtomically(
        ParentPath, SerializeMaterialInstanceJson(Parent)).Success);
    const FMaterialInstanceParentResolution Cycle =
        ResolveMaterialInstanceParent(Root, Child);
    MATERIAL_CHECK(Runner, !Cycle.Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Cycle.Diagnostics, "instance.parent_cycle"));

    std::filesystem::remove_all(Root, Error);
    MATERIAL_CHECK(Runner, !std::filesystem::exists(Root));
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrMaterialInstanceEditorDocumentTests");
    TestInstanceEditing(Runner);
    TestInstanceJsonAndFileRoundTrip(Runner);
    TestInstanceRequiresParentSchemaForOverrides(Runner);
    TestInstanceMigrationMarksDirty(Runner);
    TestParentChainResolutionAndFlattening(Runner);
    return Runner.Finish();
}
