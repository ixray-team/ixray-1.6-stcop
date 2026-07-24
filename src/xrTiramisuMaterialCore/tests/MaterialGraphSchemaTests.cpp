#include "MaterialGraphSchema.h"
#include "MaterialTestHarness.h"

#include <set>
#include <stdexcept>
#include <string>

namespace
{
const FMaterialGraphPin& Pin(const FMaterialGraphNode& Node, const xr_string_view Name,
    const EMaterialPinDirection Direction)
{
    for (const FMaterialGraphPin& Candidate : Node.Pins)
    {
        if (Candidate.Name == Name && Candidate.Direction == Direction)
            return Candidate;
    }
    throw std::runtime_error("missing test pin");
}

void TestCatalogAndFactory(TiramisuMaterialTestRunner& Runner)
{
    xr_set<xr_string> Types;
    for (const FMaterialNodeDefinition& Definition : GetMaterialNodeDefinitions())
    {
        MATERIAL_CHECK(Runner, !Definition.Type.empty());
        MATERIAL_CHECK(Runner, Types.emplace(Definition.Type).second);
        const auto Node = CreateMaterialGraphNode(
            Definition.Type, {xr_string("node-") + xr_string(Definition.Type)});
        MATERIAL_CHECK(Runner, Node.has_value());
        MATERIAL_CHECK(Runner, Node->Type == Definition.Type);
        MATERIAL_CHECK(Runner, Node->TypeVersion == Definition.TypeVersion);

        xr_set<xr_string> PinIds;
        for (const FMaterialGraphPin& GraphPin : Node->Pins)
        {
            MATERIAL_CHECK(Runner, GraphPin.Id.IsValid());
            MATERIAL_CHECK(Runner, GraphPin.Id.Value.size() == 36);
            MATERIAL_CHECK(Runner, GraphPin.Id.Value[8] == '-' &&
                GraphPin.Id.Value[13] == '-' && GraphPin.Id.Value[18] == '-' &&
                GraphPin.Id.Value[23] == '-');
            MATERIAL_CHECK(Runner, PinIds.emplace(GraphPin.Id.Value).second);
            MATERIAL_CHECK(Runner, GraphPin.Type != EMaterialValueType::Invalid);
        }

        const auto Recreated = CreateMaterialGraphNode(Definition.Type,
            {xr_string("node-") + xr_string(Definition.Type)});
        MATERIAL_CHECK(Runner, Recreated.has_value());
        MATERIAL_CHECK(Runner, Recreated->Pins.size() == Node->Pins.size());
        for (size_t Index = 0; Index < Node->Pins.size(); ++Index)
            MATERIAL_CHECK(Runner, Recreated->Pins[Index].Id == Node->Pins[Index].Id);
    }
    MATERIAL_CHECK(Runner, Types.contains("material_output"));
    MATERIAL_CHECK(Runner, Types.contains("custom_hlsl"));
    MATERIAL_CHECK(Runner, FindMaterialNodeDefinition("not-a-node") == nullptr);
}

void TestTypedCreation(TiramisuMaterialTestRunner& Runner)
{
    const auto Add = CreateMaterialGraphNode(
        "add", {"vector-add"}, {10.0f, 20.0f}, EMaterialValueType::Float3);
    MATERIAL_CHECK(Runner, Add.has_value());
    MATERIAL_CHECK(Runner, Pin(*Add, "A", EMaterialPinDirection::Input).Type == EMaterialValueType::Float3);
    MATERIAL_CHECK(Runner, Pin(*Add, "Result", EMaterialPinDirection::Output).Type == EMaterialValueType::Float3);
    MATERIAL_CHECK(Runner, Add->EditorPosition == FFloat2({10.0f, 20.0f}));

    const auto Dot = CreateMaterialGraphNode(
        "dot", {"vector-dot"}, {}, EMaterialValueType::Float4);
    MATERIAL_CHECK(Runner, Dot.has_value());
    MATERIAL_CHECK(Runner, Pin(*Dot, "A", EMaterialPinDirection::Input).Type == EMaterialValueType::Float4);
    MATERIAL_CHECK(Runner, Pin(*Dot, "Result", EMaterialPinDirection::Output).Type == EMaterialValueType::Float1);
    MATERIAL_CHECK(Runner, !CreateMaterialGraphNode("unknown", {"unknown-node"}).has_value());
}

void TestLinkValidationAndCompilation(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Graph;
    Graph.Nodes.push_back(*CreateMaterialGraphNode(
        "constant", {"constant-node"}, {}, EMaterialValueType::Float1));
    Graph.Nodes.back().Properties["value"] = 0.42f;
    Graph.Nodes.push_back(*CreateMaterialGraphNode("material_output", {"output-node"}));

    const FMaterialGraphPin& Constant = Pin(Graph.Nodes[0], "Value", EMaterialPinDirection::Output);
    const FMaterialGraphPin& Roughness = Pin(Graph.Nodes[1], "Roughness", EMaterialPinDirection::Input);
    MATERIAL_CHECK(Runner, ValidateMaterialGraphLink(Graph, Constant.Id, Roughness.Id).Succeeded());

    Graph.Links.push_back({"roughness-link", Constant.Id, Roughness.Id});
    MATERIAL_CHECK(Runner,
        HasDiagnostic(ValidateMaterialGraphLink(Graph, Constant.Id, Roughness.Id).Diagnostics,
            "graph.multiple_input_links"));

    const FMaterialGraphCompileResult Compiled = CompileMaterialGraph(Graph, {});
    MATERIAL_CHECK(Runner, Compiled.Succeeded());
    MATERIAL_CHECK(Runner, Compiled.GeneratedHlsl.find("Result.Roughness = 0.419999") != xr_string::npos);

    const xr_string Json = SerializeMaterialGraphJson(Graph);
    const FMaterialGraphParseResult Reparsed = ParseMaterialGraphJson(Json);
    MATERIAL_CHECK(Runner, Reparsed.Succeeded());
    MATERIAL_CHECK(Runner, SerializeMaterialGraphJson(Reparsed.Graph) == Json);
}

void TestRejectedLinks(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Graph;
    Graph.Nodes.push_back(*CreateMaterialGraphNode("texcoord0", {"uv-node"}));
    Graph.Nodes.push_back(*CreateMaterialGraphNode("texture_sample", {"sample-node"}));
    Graph.Nodes.push_back(*CreateMaterialGraphNode("material_output", {"output-node"}));

    const FMaterialGraphPin& Uv = Pin(Graph.Nodes[0], "UV", EMaterialPinDirection::Output);
    const FMaterialGraphPin& Texture = Pin(Graph.Nodes[1], "Texture", EMaterialPinDirection::Input);
    const FMaterialGraphPin& BaseColor = Pin(Graph.Nodes[2], "BaseColor", EMaterialPinDirection::Input);

    MATERIAL_CHECK(Runner,
        HasDiagnostic(ValidateMaterialGraphLink(Graph, Uv.Id, Texture.Id).Diagnostics,
            "graph.link_type_mismatch"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(ValidateMaterialGraphLink(Graph, BaseColor.Id, Uv.Id).Diagnostics,
            "graph.invalid_link_direction"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(ValidateMaterialGraphLink(Graph, {"missing"}, BaseColor.Id).Diagnostics,
            "graph.dangling_link"));
}

void TestPropertySchema(TiramisuMaterialTestRunner& Runner)
{
    const auto Scalar = CreateMaterialGraphNode("constant", {"scalar"});
    MATERIAL_CHECK(Runner, Scalar.has_value());
    MATERIAL_CHECK(Runner, GetMaterialNodePropertyDefinitions("constant").size() == 1);
    MATERIAL_CHECK(Runner,
        ValidateMaterialGraphNodeProperty(*Scalar, "value", FMaterialValue{0.5f}).Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(ValidateMaterialGraphNodeProperty(
            *Scalar, "value", FMaterialValue{xr_string{"wrong"}}).Diagnostics,
            "graph.node_property_type_mismatch"));
    MATERIAL_CHECK(Runner,
        HasDiagnostic(ValidateMaterialGraphNodeProperty(
            *Scalar, "missing", FMaterialValue{0.5f}).Diagnostics,
            "graph.unknown_node_property"));

    const auto Parameter = CreateMaterialGraphNode("parameter", {"parameter"});
    MATERIAL_CHECK(Runner, Parameter.has_value());
    MATERIAL_CHECK(Runner,
        ValidateMaterialGraphNodeProperty(
            *Parameter, "parameter_id", FMaterialValue{xr_string{"base-color"}}).Succeeded());
    MATERIAL_CHECK(Runner,
        HasDiagnostic(ValidateMaterialGraphNodeProperty(
            *Parameter, "parameter_id", FMaterialValue{xr_string{"bad id"}}).Diagnostics,
            "graph.invalid_parameter_id_property"));

    const auto Custom = CreateMaterialGraphNode("custom_hlsl", {"custom"});
    MATERIAL_CHECK(Runner, Custom.has_value());
    const auto Properties = GetMaterialNodePropertyDefinitions(Custom->Type);
    MATERIAL_CHECK(Runner, Properties.size() == 1);
    MATERIAL_CHECK(Runner, Properties.front().Multiline);

    const auto TextureSample = CreateMaterialGraphNode("texture_sample", {"texture-sample"});
    MATERIAL_CHECK(Runner, TextureSample.has_value());
    const auto TextureProperties = GetMaterialNodePropertyDefinitions(TextureSample->Type);
    MATERIAL_CHECK(Runner, TextureProperties.size() == 1);
    MATERIAL_CHECK(Runner,
        TextureProperties.front().Kind == EMaterialNodePropertyKind::ParameterId);
    MATERIAL_CHECK(Runner,
        TextureSample->Properties.contains("texture_parameter_id"));
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrMaterialGraphSchemaTests");
    TestCatalogAndFactory(Runner);
    TestTypedCreation(Runner);
    TestLinkValidationAndCompilation(Runner);
    TestRejectedLinks(Runner);
    TestPropertySchema(Runner);
    return Runner.Finish();
}
