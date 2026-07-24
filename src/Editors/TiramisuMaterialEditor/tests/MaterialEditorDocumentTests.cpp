#include "MaterialEditorDocument.h"
#include "MaterialTestHarness.h"

#include <array>
#include <filesystem>
#include <ranges>
#include <string>

using namespace Tiramisu::Editor;

namespace
{
const FMaterialGraphPin& Pin(const FMaterialGraphNode& Node, const xr_string_view Name,
    const EMaterialPinDirection Direction)
{
    for (const FMaterialGraphPin& Candidate : Node.Pins)
        if (Candidate.Name == Name && Candidate.Direction == Direction)
            return Candidate;
    throw std::runtime_error("missing test pin");
}

void TestNewGraphAndCompile(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.size() == 1);
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.front().Type == "material_output");
    MATERIAL_CHECK(Runner, !Document.IsDirty());

    MATERIAL_CHECK(Runner,
        Document.AddNode("constant", {"roughness-constant"}, {20.0f, 40.0f}).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"roughness-constant"}, "value", 0.65f).Succeeded());
    const FMaterialGraph& Graph = Document.GetGraph();
    const FMaterialGraphPin& Value = Pin(Graph.Nodes.back(), "Value", EMaterialPinDirection::Output);
    const FMaterialGraphPin& Roughness = Pin(Graph.Nodes.front(), "Roughness", EMaterialPinDirection::Input);
    MATERIAL_CHECK(Runner, Document.Connect("roughness-link", Value.Id, Roughness.Id).Succeeded());
    MATERIAL_CHECK(Runner, Document.Compile().Succeeded());
    MATERIAL_CHECK(Runner, Document.IsDirty());

    Document.MarkSaved();
    MATERIAL_CHECK(Runner, !Document.IsDirty());
}

void TestTypedNodePropertiesAndHistory(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    MATERIAL_CHECK(Runner,
        Document.AddNode("constant", {"scalar-constant"}).Succeeded());

    Document.OpenGraph(Document.GetGraph());
    MATERIAL_CHECK(Runner, !Document.CanUndo());
    MATERIAL_CHECK(Runner, !Document.IsDirty());
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"scalar-constant"}, "value", 0.0f).Succeeded());
    MATERIAL_CHECK(Runner, !Document.CanUndo());
    MATERIAL_CHECK(Runner, !Document.IsDirty());

    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"scalar-constant"}, "value", 0.75f).Succeeded());
    MATERIAL_CHECK(Runner, Document.CanUndo());
    MATERIAL_CHECK(Runner, Document.IsDirty());
    MATERIAL_CHECK(Runner,
        std::get<float>(Document.GetGraph().Nodes.back().Properties.at("value")) == 0.75f);

    MATERIAL_CHECK(Runner, Document.Undo());
    MATERIAL_CHECK(Runner,
        std::get<float>(Document.GetGraph().Nodes.back().Properties.at("value")) == 0.0f);
    MATERIAL_CHECK(Runner, Document.Redo());
    MATERIAL_CHECK(Runner,
        std::get<float>(Document.GetGraph().Nodes.back().Properties.at("value")) == 0.75f);

    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty(
            {"scalar-constant"}, "value", xr_string{"wrong"}).Diagnostics,
            "graph.node_property_type_mismatch"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty(
            {"scalar-constant"}, "unknown", 1.0f).Diagnostics,
            "graph.unknown_node_property"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty(
            {"missing"}, "value", 1.0f).Diagnostics,
            "editor.missing_node"));

    MATERIAL_CHECK(Runner,
        Document.AddNode("constant", {"vector-constant"}, {},
            EMaterialValueType::Float3).Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty(
            {"vector-constant"}, "value", 1.0f).Diagnostics,
            "graph.node_property_type_mismatch"));
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty(
            {"vector-constant"}, "value", FFloat3{1.0f, 2.0f, 3.0f}).Succeeded());
}

void TestRejectedOperations(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.AddNode("unknown", {"unknown"}).Diagnostics,
            "editor.unknown_node_type"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.AddNode("material_output", {"second-output"}).Diagnostics,
            "editor.duplicate_material_output"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.RemoveNode(
            Document.GetGraph().Nodes.front().Id).Diagnostics,
            "editor.cannot_remove_material_output"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.Disconnect("missing").Diagnostics, "editor.missing_link"));
}

void TestUndoRedoAndCascadeDelete(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    MATERIAL_CHECK(Runner, Document.AddNode("constant", {"constant"}).Succeeded());
    const FMaterialGraph& Graph = Document.GetGraph();
    const FMaterialGraphPin Value = Pin(Graph.Nodes.back(), "Value", EMaterialPinDirection::Output);
    const FMaterialGraphPin Roughness = Pin(Graph.Nodes.front(), "Roughness", EMaterialPinDirection::Input);
    MATERIAL_CHECK(Runner, Document.Connect("link", Value.Id, Roughness.Id).Succeeded());
    MATERIAL_CHECK(Runner, Document.GetGraph().Links.size() == 1);

    MATERIAL_CHECK(Runner, Document.RemoveNode({"constant"}).Succeeded());
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.size() == 1);
    MATERIAL_CHECK(Runner, Document.GetGraph().Links.empty());
    MATERIAL_CHECK(Runner, Document.CanUndo());

    MATERIAL_CHECK(Runner, Document.Undo());
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.size() == 2);
    MATERIAL_CHECK(Runner, Document.GetGraph().Links.size() == 1);
    MATERIAL_CHECK(Runner, Document.Redo());
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.size() == 1);
}

void TestTypedLinksAndOpenGraph(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    MATERIAL_CHECK(Runner, Document.AddNode("texcoord0", {"uv"}).Succeeded());
    MATERIAL_CHECK(Runner, Document.AddNode("texture_sample", {"sample"}).Succeeded());
    const FMaterialGraph& Graph = Document.GetGraph();
    const FMaterialGraphPin& Uv = Pin(Graph.Nodes[1], "UV", EMaterialPinDirection::Output);
    const FMaterialGraphPin& Texture = Pin(Graph.Nodes[2], "Texture", EMaterialPinDirection::Input);
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.Connect("invalid", Uv.Id, Texture.Id).Diagnostics,
            "graph.link_type_mismatch"));
    MATERIAL_CHECK(Runner, Document.GetGraph().Links.empty());

    FMaterialGraph Replacement;
    Replacement.Nodes.push_back(*CreateMaterialGraphNode("material_output", {"replacement-output"}));
    Document.OpenGraph(std::move(Replacement));
    MATERIAL_CHECK(Runner, !Document.IsDirty());
    MATERIAL_CHECK(Runner, !Document.CanUndo());
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.front().Id.Value == "replacement-output");
}

void TestMasterMaterialDocument(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    const xr_string OriginalId = Document.GetMaterial().Id.Value;
    MATERIAL_CHECK(Runner, OriginalId.size() == 36);
    MATERIAL_CHECK(Runner, OriginalId[8] == '-' && OriginalId[13] == '-' &&
        OriginalId[18] == '-' && OriginalId[23] == '-');
    MATERIAL_CHECK(Runner, Document.GetMaterial().Implementation.Type ==
        EMaterialImplementationType::Graph);
    MATERIAL_CHECK(Runner, !Document.IsDirty());

    MATERIAL_CHECK(Runner, Document.SetMaterialName("Editor Test Material"));
    MATERIAL_CHECK(Runner, Document.SetMaterialDomain(EMaterialDomain::Decal));
    MATERIAL_CHECK(Runner, Document.SetMaterialBlendMode(EMaterialBlendMode::Masked));
    MATERIAL_CHECK(Runner,
        Document.SetMaterialShadingModel(EMaterialShadingModel::Unlit));
    MATERIAL_CHECK(Runner, Document.SetMaterialTwoSided(true));
    MATERIAL_CHECK(Runner,
        Document.SetMaterialHlslTemplate("materials/TestTemplate.hlsl"));
    MATERIAL_CHECK(Runner, Document.IsDirty());
    MATERIAL_CHECK(Runner,
        std::ranges::contains(Document.GetMaterial().Dependencies,
            xr_string{"materials/TestTemplate.hlsl"}));

    const FMaterialAssetParseResult Serialized =
        ParseMaterialAssetJson(Document.SerializeMaterial());
    MATERIAL_CHECK(Runner, Serialized.Succeeded());
    MATERIAL_CHECK(Runner, Serialized.Value.Id.Value == OriginalId);
    MATERIAL_CHECK(Runner, Serialized.Value.Name == "Editor Test Material");
    MATERIAL_CHECK(Runner, Serialized.Value.Domain == EMaterialDomain::Decal);

    const xr_string BeforeInvalidOpen = Document.SerializeMaterial();
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.OpenMaterialJson("{ invalid json").Diagnostics,
            "asset.invalid_json"));
    MATERIAL_CHECK(Runner, Document.SerializeMaterial() == BeforeInvalidOpen);

    TiramisuMaterialEditorDocument Reopened;
    MATERIAL_CHECK(Runner,
        Reopened.OpenMaterialJson(Document.SerializeMaterial(), "memory.material.json").Succeeded());
    MATERIAL_CHECK(Runner, Reopened.GetMaterial().Id.Value == OriginalId);
    MATERIAL_CHECK(Runner, Reopened.GetMaterial().SourcePath == "memory.material.json");
    MATERIAL_CHECK(Runner, !Reopened.IsDirty());
}

void TestMaterialFileRoundTrip(TiramisuMaterialTestRunner& Runner)
{
    const std::filesystem::path Path = std::filesystem::temp_directory_path() /
        ("xr-material-editor-" + GenerateMaterialGuid() + ".material.json");
    std::error_code Error;
    std::filesystem::remove(Path, Error);

    TiramisuMaterialEditorDocument Document;
    const xr_string Id = Document.GetMaterial().Id.Value;
    MATERIAL_CHECK(Runner, Document.SetMaterialName("File Round Trip"));
    MATERIAL_CHECK(Runner, Document.SaveMaterialFile(Path).Succeeded());
    MATERIAL_CHECK(Runner, std::filesystem::exists(Path));
    MATERIAL_CHECK(Runner, !Document.IsDirty());
    MATERIAL_CHECK(Runner, Document.GetMaterial().SourcePath == ToXrString(Path.generic_string()));

    MATERIAL_CHECK(Runner, Document.SetMaterialName("Atomic Replacement"));
    MATERIAL_CHECK(Runner, Document.IsDirty());
    MATERIAL_CHECK(Runner, Document.SaveMaterialFile(Path).Succeeded());
    MATERIAL_CHECK(Runner, !Document.IsDirty());

    TiramisuMaterialEditorDocument Loaded;
    MATERIAL_CHECK(Runner, Loaded.OpenMaterialFile(Path).Succeeded());
    MATERIAL_CHECK(Runner, Loaded.GetMaterial().Id.Value == Id);
    MATERIAL_CHECK(Runner, Loaded.GetMaterial().Name == "Atomic Replacement");
    MATERIAL_CHECK(Runner, !Loaded.IsDirty());

    const std::filesystem::path RecoveryPath = Path.string() + ".autosave";
    MATERIAL_CHECK(Runner, Document.SetMaterialName("Recovered Material"));
    MATERIAL_CHECK(Runner, Document.SaveRecoveryFile(RecoveryPath).Succeeded());
    MATERIAL_CHECK(Runner, Document.IsDirty());
    MATERIAL_CHECK(Runner, Document.GetMaterial().SourcePath == ToXrString(Path.generic_string()));
    MATERIAL_CHECK(Runner, std::filesystem::exists(RecoveryPath));

    TiramisuMaterialEditorDocument Recovered;
    MATERIAL_CHECK(Runner,
        Recovered.OpenRecoveryFile(RecoveryPath, Path).Succeeded());
    MATERIAL_CHECK(Runner, Recovered.GetMaterial().Name == "Recovered Material");
    MATERIAL_CHECK(Runner, Recovered.GetMaterial().SourcePath == ToXrString(Path.generic_string()));
    MATERIAL_CHECK(Runner, Recovered.IsDirty());

    const xr_string TemporaryPrefix =
        ToXrString(Path.filename().string()) + ".tmp-";
    bool TemporaryFileFound = false;
    for (const std::filesystem::directory_entry& Entry :
        std::filesystem::directory_iterator(Path.parent_path()))
    {
        TemporaryFileFound |= Entry.path().filename().string().starts_with(TemporaryPrefix.c_str());
    }
    MATERIAL_CHECK(Runner, !TemporaryFileFound);

    std::filesystem::remove(Path, Error);
    MATERIAL_CHECK(Runner, !std::filesystem::exists(Path));
    std::filesystem::remove(RecoveryPath, Error);
    MATERIAL_CHECK(Runner, !std::filesystem::exists(RecoveryPath));
}

void TestMigrationMarksDocumentDirty(TiramisuMaterialTestRunner& Runner)
{
    constexpr xr_string_view LegacyJson = R"json({
      "version": 1,
      "id": "legacy-editor-material",
      "name": "Legacy",
      "domain": "surface",
      "blend_mode": "opaque",
      "shading_model": "default_lit",
      "template": "materials/MaterialTemplate.hlsl",
      "implementation": {"type": "hlsl", "source": "materials/Legacy.hlsl"},
      "parameters": [],
      "static_parameters": []
    })json";
    TiramisuMaterialEditorDocument Document;
    const FMaterialEditorOperationResult Result = Document.OpenMaterialJson(
        LegacyJson, "legacy.material.json");
    MATERIAL_CHECK(Runner, Result.Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Result.Diagnostics, "asset.migrated_version_field"));
    MATERIAL_CHECK(Runner, HasDiagnostic(Result.Diagnostics, "asset.migrated_id_field"));
    MATERIAL_CHECK(Runner, Document.IsDirty());
    MATERIAL_CHECK(Runner,
        Document.SerializeMaterial().find("\"asset_version\"") != xr_string::npos);
}

void TestAtomicSaveFailureKeepsDocumentDirty(TiramisuMaterialTestRunner& Runner)
{
    const std::filesystem::path Directory = std::filesystem::temp_directory_path() /
        ("xr-material-editor-save-failure-" + GenerateMaterialGuid());
    const std::filesystem::path DirectoryTarget = Directory / "blocked.material.json";
    std::error_code Error;
    MATERIAL_CHECK(Runner, std::filesystem::create_directory(Directory));
    MATERIAL_CHECK(Runner, std::filesystem::create_directory(DirectoryTarget));

    TiramisuMaterialEditorDocument Document;
    MATERIAL_CHECK(Runner, Document.SetMaterialName("Must Remain Dirty"));
    const FMaterialEditorOperationResult SaveResult = Document.SaveMaterialFile(DirectoryTarget);
    MATERIAL_CHECK(Runner, !SaveResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(SaveResult.Diagnostics, "editor.material_save_failed"));
    MATERIAL_CHECK(Runner, Document.IsDirty());
    MATERIAL_CHECK(Runner, std::filesystem::is_directory(DirectoryTarget));

    bool TemporaryFileFound = false;
    for (const std::filesystem::directory_entry& Entry :
        std::filesystem::directory_iterator(Directory))
    {
        TemporaryFileFound |= Entry.path().filename().string().starts_with("blocked.material.json.tmp-");
    }
    MATERIAL_CHECK(Runner, !TemporaryFileFound);

    std::filesystem::remove(DirectoryTarget, Error);
    MATERIAL_CHECK(Runner, !Error);
    Error.clear();
    std::filesystem::remove(Directory, Error);
    MATERIAL_CHECK(Runner, !Error);
}

void TestHlslMasterRejectsGraphEditing(TiramisuMaterialTestRunner& Runner)
{
    FMaterialAsset Asset;
    Asset.Id.Value = GenerateMaterialGuid();
    Asset.Name = "HLSL Master";
    Asset.HlslTemplate = "materials/MaterialTemplate.hlsl";
    Asset.Implementation.Type = EMaterialImplementationType::Hlsl;
    Asset.Implementation.Source = "materials/StandardSurface.hlsl";

    TiramisuMaterialEditorDocument Document;
    Document.OpenMaterial(std::move(Asset));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.AddNode("constant", {"not-allowed"}).Diagnostics,
            "editor.graph_implementation_required"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.Compile().Diagnostics,
            "editor.graph_implementation_required"));
}

void TestParameterEditingAndReferences(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    FMaterialParameterDefinition Scalar;
    Scalar.Id.Value = "editor-scalar-guid";
    Scalar.Name = "Roughness";
    Scalar.Type = EMaterialParameterType::Scalar;
    Scalar.DefaultValue = 0.5f;
    Scalar.DisplayName = "Roughness";
    Scalar.Minimum = 0.0f;
    Scalar.Maximum = 1.0f;
    MATERIAL_CHECK(Runner, Document.AddParameter(Scalar).Succeeded());
    MATERIAL_CHECK(Runner, Document.GetMaterial().Parameters.size() == 1);
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.AddParameter(Scalar).Diagnostics,
            "editor.duplicate_parameter_id"));

    FMaterialParameterDefinition Invalid = Scalar;
    Invalid.Id.Value = "invalid-default-guid";
    Invalid.DefaultValue = xr_string{"wrong"};
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.AddParameter(Invalid).Diagnostics,
            "editor.parameter_default_type_mismatch"));

    FMaterialParameterDefinition Renamed = Scalar;
    Renamed.Name = "Microsurface";
    MATERIAL_CHECK(Runner,
        Document.UpdateParameter(Scalar.Id, Renamed).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.GetMaterial().FindParameter(Scalar.Id)->Name == "Microsurface");

    MATERIAL_CHECK(Runner,
        Document.AddNode("parameter", {"roughness-parameter"}, {},
            EMaterialValueType::Float1).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"roughness-parameter"}, "parameter_id",
            Scalar.Id.Value).Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.RemoveParameter(Scalar.Id).Diagnostics,
            "editor.parameter_in_use"));

    FMaterialParameterDefinition ChangedType = Renamed;
    ChangedType.Type = EMaterialParameterType::Float3;
    ChangedType.DefaultValue = FFloat3{0.5f, 0.5f, 0.5f};
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.UpdateParameter(Scalar.Id, ChangedType).Diagnostics,
            "editor.parameter_type_in_use"));

    MATERIAL_CHECK(Runner, Document.RemoveNode({"roughness-parameter"}).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.UpdateParameter(Scalar.Id, ChangedType).Succeeded());
    MATERIAL_CHECK(Runner, Document.RemoveParameter(Scalar.Id).Succeeded());
    MATERIAL_CHECK(Runner, Document.GetMaterial().Parameters.empty());
    MATERIAL_CHECK(Runner, Document.Undo());
    MATERIAL_CHECK(Runner, Document.GetMaterial().Parameters.size() == 1);
}

void TestTypedNodeParameterBindings(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;

    FMaterialParameterDefinition Scalar{{"binding-scalar-guid"}, "Scalar",
        EMaterialParameterType::Scalar, 0.5f};
    FMaterialParameterDefinition Texture{{"binding-texture-guid"}, "Texture",
        EMaterialParameterType::Texture2D, xr_string{"textures/default"}};
    FMaterialParameterDefinition Switch{{"binding-switch-guid"}, "Switch",
        EMaterialParameterType::StaticBool, false};
    MATERIAL_CHECK(Runner, Document.AddParameter(Scalar).Succeeded());
    MATERIAL_CHECK(Runner, Document.AddParameter(Texture).Succeeded());
    MATERIAL_CHECK(Runner, Document.AddParameter(Switch).Succeeded());

    MATERIAL_CHECK(Runner,
        Document.AddNode("parameter", {"binding-parameter-node"}, {},
            EMaterialValueType::Float1).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"binding-parameter-node"}, "parameter_id",
            Scalar.Id.Value).Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty({"binding-parameter-node"},
            "parameter_id", Texture.Id.Value).Diagnostics,
            "editor.node_parameter_type_mismatch"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty({"binding-parameter-node"},
            "parameter_id", xr_string{"missing-parameter"}).Diagnostics,
            "editor.unknown_node_parameter"));

    MATERIAL_CHECK(Runner,
        Document.AddNode("static_switch", {"binding-switch-node"}).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"binding-switch-node"}, "parameter_id",
            Switch.Id.Value).Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty({"binding-switch-node"},
            "parameter_id", Scalar.Id.Value).Diagnostics,
            "editor.node_parameter_type_mismatch"));

    MATERIAL_CHECK(Runner,
        Document.AddNode("texture_sample", {"binding-texture-node"}).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"binding-texture-node"}, "texture_parameter_id",
            Texture.Id.Value).Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.SetNodeProperty({"binding-texture-node"},
            "texture_parameter_id", Scalar.Id.Value).Diagnostics,
            "editor.node_parameter_type_mismatch"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.RemoveParameter(Texture.Id).Diagnostics,
            "editor.parameter_in_use"));
}

void TestGraphCopyPasteAndSingleUndo(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialEditorDocument Document;
    MATERIAL_CHECK(Runner,
        Document.AddNode("constant", {"clipboard-constant"}, {10.0f, 20.0f}).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.SetNodeProperty({"clipboard-constant"}, "value", 0.25f).Succeeded());
    MATERIAL_CHECK(Runner,
        Document.AddNode("add", {"clipboard-add"}, {50.0f, 60.0f},
            EMaterialValueType::Float1).Succeeded());

    const FMaterialGraphNode* Constant = FindMaterialGraphNode(
        Document.GetGraph(), {"clipboard-constant"});
    const FMaterialGraphNode* Add = FindMaterialGraphNode(
        Document.GetGraph(), {"clipboard-add"});
    MATERIAL_CHECK(Runner, Constant != nullptr);
    MATERIAL_CHECK(Runner, Add != nullptr);
    MATERIAL_CHECK(Runner, Document.Connect("clipboard-internal-link",
        Pin(*Constant, "Value", EMaterialPinDirection::Output).Id,
        Pin(*Add, "A", EMaterialPinDirection::Input).Id).Succeeded());

    const xr_array Selected{
        FMaterialNodeId{"clipboard-constant"}, FMaterialNodeId{"clipboard-add"}};
    xr_string ClipboardJson;
    MATERIAL_CHECK(Runner, Document.CopyNodes(Selected, ClipboardJson).Succeeded());
    MATERIAL_CHECK(Runner, !ClipboardJson.empty());

    Document.MarkSaved();
    const size_t OriginalNodeCount = Document.GetGraph().Nodes.size();
    const size_t OriginalLinkCount = Document.GetGraph().Links.size();
    xr_vector<FMaterialNodeId> Pasted;
    MATERIAL_CHECK(Runner,
        Document.PasteNodes(ClipboardJson, {100.0f, 200.0f}, Pasted).Succeeded());
    MATERIAL_CHECK(Runner, Pasted.size() == 2);
    MATERIAL_CHECK(Runner, Pasted[0] != Selected[0] && Pasted[1] != Selected[1]);
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.size() == OriginalNodeCount + 2);
    MATERIAL_CHECK(Runner, Document.GetGraph().Links.size() == OriginalLinkCount + 1);
    MATERIAL_CHECK(Runner, Document.IsDirty());

    const FMaterialGraphNode* PastedConstant = FindMaterialGraphNode(
        Document.GetGraph(), Pasted.front());
    MATERIAL_CHECK(Runner, PastedConstant != nullptr);
    MATERIAL_CHECK(Runner, PastedConstant->EditorPosition == FFloat2({110.0f, 220.0f}));
    MATERIAL_CHECK(Runner,
        std::get<float>(PastedConstant->Properties.at("value")) == 0.25f);

    MATERIAL_CHECK(Runner, Document.Undo());
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.size() == OriginalNodeCount);
    MATERIAL_CHECK(Runner, Document.GetGraph().Links.size() == OriginalLinkCount);
    MATERIAL_CHECK(Runner, Document.Redo());
    MATERIAL_CHECK(Runner, Document.GetGraph().Nodes.size() == OriginalNodeCount + 2);

    xr_string OutputClipboard;
    const xr_array OutputOnly{Document.GetGraph().Nodes.front().Id};
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.CopyNodes(OutputOnly, OutputClipboard).Diagnostics,
            "editor.clipboard_empty_selection"));

    Pasted.clear();
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.PasteNodes("{ invalid json", {}, Pasted).Diagnostics,
            "graph.invalid_json"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(Document.PasteNodes(Document.Serialize(), {}, Pasted).Diagnostics,
            "editor.clipboard_material_output"));
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrMaterialEditorDocumentTests");
    TestNewGraphAndCompile(Runner);
    TestRejectedOperations(Runner);
    TestUndoRedoAndCascadeDelete(Runner);
    TestTypedLinksAndOpenGraph(Runner);
    TestTypedNodePropertiesAndHistory(Runner);
    TestMasterMaterialDocument(Runner);
    TestMaterialFileRoundTrip(Runner);
    TestMigrationMarksDocumentDirty(Runner);
    TestAtomicSaveFailureKeepsDocumentDirty(Runner);
    TestHlslMasterRejectsGraphEditing(Runner);
    TestParameterEditingAndReferences(Runner);
    TestTypedNodeParameterBindings(Runner);
    TestGraphCopyPasteAndSingleUndo(Runner);
    return Runner.Finish();
}
