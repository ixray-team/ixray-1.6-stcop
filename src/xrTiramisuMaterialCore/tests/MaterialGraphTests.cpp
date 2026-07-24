#include "MaterialAsset.h"
#include "MaterialGraph.h"
#include "MaterialGraphSchema.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace
{
FMaterialGraphPin Pin(const xr_string& Id, const xr_string& Name, const EMaterialPinDirection Direction,
    const EMaterialValueType Type)
{
    return {{Id}, Name, Direction, Type};
}

const FMaterialGraphPin& GraphPin(const FMaterialGraphNode& Node,
    const xr_string_view Name, const EMaterialPinDirection Direction)
{
    const auto Result = std::ranges::find_if(Node.Pins,
        [Name, Direction](const FMaterialGraphPin& Candidate)
        { return Candidate.Name == Name && Candidate.Direction == Direction; });
    if (Result == Node.Pins.end())
        throw std::runtime_error("missing test pin");
    return *Result;
}

xr_string ReadText(const xr_string& Path)
{
    std::ifstream Stream(Path.c_str(), std::ios::binary);
    std::ostringstream Text;
    Text << Stream.rdbuf();
    return Text.str();
}

FMaterialGraph MakeScalarOutputGraph(const float Value)
{
    FMaterialGraph Graph;
    FMaterialGraphNode Constant{{"constant-node"}, "constant"};
    Constant.Pins = {Pin("constant-output", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    Constant.Properties["value"] = Value;
    FMaterialGraphNode Output{{"material-output-node"}, "material_output"};
    Output.Pins = {Pin("roughness-input", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    Graph.Nodes = {Constant, Output};
    Graph.Links = {{"output-link", {"constant-output"}, {"roughness-input"}}};
    return Graph;
}

void TestConstantFoldingAndDce(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Graph;
    FMaterialGraphNode A{{"fold-a"}, "constant"};
    A.Pins = {Pin("fold-a-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    A.Properties["value"] = 0.25f;
    FMaterialGraphNode B{{"fold-b"}, "constant"};
    B.Pins = {Pin("fold-b-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    B.Properties["value"] = 0.5f;
    FMaterialGraphNode Add{{"fold-add"}, "add"};
    Add.Pins = {
        Pin("fold-add-a", "A", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("fold-add-b", "B", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("fold-add-out", "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1),
    };
    FMaterialGraphNode Dead{{"dead-node"}, "unknown_dead_node"};
    Dead.Pins = {Pin("dead-out", "Out", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    FMaterialGraphNode Output{{"fold-output"}, "material_output"};
    Output.Pins = {Pin("fold-roughness", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    Graph.Nodes = {A, B, Add, Dead, Output};
    Graph.Links = {
        {"fold-link-a", {"fold-a-out"}, {"fold-add-a"}},
        {"fold-link-b", {"fold-b-out"}, {"fold-add-b"}},
        {"fold-link-output", {"fold-add-out"}, {"fold-roughness"}},
    };

    const FMaterialGraphCompileResult Result = CompileMaterialGraph(Graph, {});
    MATERIAL_CHECK(Runner, Result.Succeeded());
    MATERIAL_CHECK(Runner, Result.GeneratedHlsl.find("0.75f") != xr_string::npos);
    MATERIAL_CHECK(Runner, Result.GeneratedHlsl.find("unknown_dead_node") == xr_string::npos);
}

void TestGraphStructuralDiagnostics(TiramisuMaterialTestRunner& Runner)
{
    const FMaterialGraphParseResult InvalidJson = ParseMaterialGraphJson("{ invalid json");
    MATERIAL_CHECK(Runner, !InvalidJson.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidJson.Diagnostics, "graph.invalid_json"));

    const FMaterialGraphParseResult InvalidRoot = ParseMaterialGraphJson("[]");
    MATERIAL_CHECK(Runner, !InvalidRoot.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidRoot.Diagnostics, "graph.invalid_root"));

    constexpr xr_string_view WrongFieldTypes = R"json({
      "version": "one",
      "nodes": [{
        "guid": 42,
        "type": [],
        "type_version": false,
        "position": [0.0, "bad"],
        "pins": [{"guid": 1, "name": {}, "direction": 2, "type": true}],
        "properties": {"value": [1.0, "bad"]}
      }],
      "links": {}
    })json";
    const FMaterialGraphParseResult WrongFieldTypesResult = ParseMaterialGraphJson(WrongFieldTypes);
    MATERIAL_CHECK(Runner, !WrongFieldTypesResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongFieldTypesResult.Diagnostics, "graph.invalid_field_type"));
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongFieldTypesResult.Diagnostics, "graph.invalid_position"));
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongFieldTypesResult.Diagnostics, "graph.invalid_property"));

    FMaterialGraph MissingOutput;
    MATERIAL_CHECK(Runner, HasDiagnostic(CompileMaterialGraph(MissingOutput, {}).Diagnostics, "graph.missing_output"));

    FMaterialGraph MultipleOutputs;
    MultipleOutputs.Nodes = {{{"output-a"}, "material_output"}, {{"output-b"}, "material_output"}};
    const FMaterialGraphCompileResult MultipleResult = CompileMaterialGraph(MultipleOutputs, {});
    MATERIAL_CHECK(Runner, !MultipleResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(MultipleResult.Diagnostics, "graph.multiple_outputs"));

    FMaterialGraph Dangling = MakeScalarOutputGraph(0.5f);
    Dangling.Links.front().FromPin.Value = "missing-pin";
    const FMaterialGraphCompileResult DanglingResult = CompileMaterialGraph(Dangling, {});
    MATERIAL_CHECK(Runner, HasDiagnostic(DanglingResult.Diagnostics, "graph.dangling_link"));

    FMaterialGraph WrongDirection = MakeScalarOutputGraph(0.5f);
    WrongDirection.Links.front() = {"wrong-direction", {"roughness-input"}, {"constant-output"}};
    const FMaterialGraphCompileResult DirectionResult = CompileMaterialGraph(WrongDirection, {});
    MATERIAL_CHECK(Runner, HasDiagnostic(DirectionResult.Diagnostics, "graph.invalid_link_direction"));

    FMaterialGraph TypeMismatch;
    FMaterialGraphNode Vector{{"vector-node"}, "constant"};
    Vector.Pins = {Pin("vector-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float3)};
    Vector.Properties["value"] = FFloat3{1.0f, 1.0f, 1.0f};
    FMaterialGraphNode Output{{"mismatch-output"}, "material_output"};
    Output.Pins = {Pin("mismatch-roughness", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    TypeMismatch.Nodes = {Vector, Output};
    TypeMismatch.Links = {{"mismatch-link", {"vector-out"}, {"mismatch-roughness"}}};
    const FMaterialGraphCompileResult TypeResult = CompileMaterialGraph(TypeMismatch, {});
    MATERIAL_CHECK(Runner, HasDiagnostic(TypeResult.Diagnostics, "graph.link_type_mismatch"));
}

void TestCyclesAndDivisionByZero(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Cycle;
    FMaterialGraphNode Constant{{"cycle-constant"}, "constant"};
    Constant.Pins = {Pin("cycle-constant-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    Constant.Properties["value"] = 1.0f;
    FMaterialGraphNode Add{{"cycle-add"}, "add"};
    Add.Pins = {
        Pin("cycle-add-a", "A", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("cycle-add-b", "B", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("cycle-add-out", "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1),
    };
    FMaterialGraphNode Output{{"cycle-output"}, "material_output"};
    Output.Pins = {Pin("cycle-output-in", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    Cycle.Nodes = {Constant, Add, Output};
    Cycle.Links = {
        {"cycle-self", {"cycle-add-out"}, {"cycle-add-a"}},
        {"cycle-b", {"cycle-constant-out"}, {"cycle-add-b"}},
        {"cycle-result", {"cycle-add-out"}, {"cycle-output-in"}},
    };
    const FMaterialGraphCompileResult CycleResult = CompileMaterialGraph(Cycle, {});
    MATERIAL_CHECK(Runner, !CycleResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(CycleResult.Diagnostics, "graph.cycle"));

    FMaterialGraph Divide;
    FMaterialGraphNode One{{"divide-one"}, "constant"};
    One.Pins = {Pin("divide-one-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    One.Properties["value"] = 1.0f;
    FMaterialGraphNode Zero{{"divide-zero"}, "constant"};
    Zero.Pins = {Pin("divide-zero-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    Zero.Properties["value"] = 0.0f;
    FMaterialGraphNode DivideNode{{"divide-node"}, "divide"};
    DivideNode.Pins = {
        Pin("divide-a", "A", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("divide-b", "B", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("divide-out", "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1),
    };
    FMaterialGraphNode DivideOutput{{"divide-output"}, "material_output"};
    DivideOutput.Pins = {Pin("divide-output-in", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    Divide.Nodes = {One, Zero, DivideNode, DivideOutput};
    Divide.Links = {
        {"divide-link-a", {"divide-one-out"}, {"divide-a"}},
        {"divide-link-b", {"divide-zero-out"}, {"divide-b"}},
        {"divide-link-out", {"divide-out"}, {"divide-output-in"}},
    };
    const FMaterialGraphCompileResult DivideResult = CompileMaterialGraph(Divide, {});
    MATERIAL_CHECK(Runner, HasDiagnostic(DivideResult.Diagnostics, "graph.divide_by_zero"));
}

void TestStaticSwitch(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Graph;
    FMaterialGraphNode FalseValue{{"switch-false"}, "constant"};
    FalseValue.Pins = {Pin("switch-false-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    FalseValue.Properties["value"] = 0.1f;
    FMaterialGraphNode TrueValue{{"switch-true"}, "constant"};
    TrueValue.Pins = {Pin("switch-true-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    TrueValue.Properties["value"] = 0.9f;
    FMaterialGraphNode Switch{{"switch-node"}, "static_switch"};
    Switch.Pins = {
        Pin("switch-true-in", "True", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("switch-false-in", "False", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("switch-out", "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1),
    };
    Switch.Properties["parameter_id"] = xr_string("static-switch-guid");
    FMaterialGraphNode Output{{"switch-output"}, "material_output"};
    Output.Pins = {Pin("switch-roughness", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    Graph.Nodes = {FalseValue, TrueValue, Switch, Output};
    Graph.Links = {
        {"switch-true-link", {"switch-true-out"}, {"switch-true-in"}},
        {"switch-false-link", {"switch-false-out"}, {"switch-false-in"}},
        {"switch-output-link", {"switch-out"}, {"switch-roughness"}},
    };

    FMaterialGraphCompileOptions TrueOptions;
    TrueOptions.StaticParameters.emplace(FMaterialParameterId{"static-switch-guid"}, true);
    const FMaterialGraphCompileResult TrueResult = CompileMaterialGraph(Graph, TrueOptions);
    MATERIAL_CHECK(Runner, TrueResult.Succeeded());
    MATERIAL_CHECK(Runner, TrueResult.GeneratedHlsl.find("0.899999976f") != xr_string::npos);
    MATERIAL_CHECK(Runner, TrueResult.GeneratedHlsl.find("0.100000001f") == xr_string::npos);

    FMaterialGraphCompileOptions FalseOptions;
    FalseOptions.StaticParameters.emplace(FMaterialParameterId{"static-switch-guid"}, false);
    const FMaterialGraphCompileResult FalseResult = CompileMaterialGraph(Graph, FalseOptions);
    MATERIAL_CHECK(Runner, FalseResult.Succeeded());
    MATERIAL_CHECK(Runner, FalseResult.GeneratedHlsl.find("0.100000001f") != xr_string::npos);
}

void TestCustomHlslRestrictions(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Graph;
    FMaterialGraphNode Constant{{"custom-constant"}, "constant"};
    Constant.Pins = {Pin("custom-constant-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    Constant.Properties["value"] = 0.25f;
    FMaterialGraphNode Custom{{"custom-node"}, "custom_hlsl"};
    Custom.Pins = {
        Pin("custom-input", "Value", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("custom-output", "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1),
    };
    Custom.Properties["code"] = xr_string("saturate({Value} * 2.0f)");
    FMaterialGraphNode Output{{"custom-material-output"}, "material_output"};
    Output.Pins = {Pin("custom-roughness", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    Graph.Nodes = {Constant, Custom, Output};
    Graph.Links = {
        {"custom-input-link", {"custom-constant-out"}, {"custom-input"}},
        {"custom-output-link", {"custom-output"}, {"custom-roughness"}},
    };
    const FMaterialGraphCompileResult Valid = CompileMaterialGraph(Graph, {});
    MATERIAL_CHECK(Runner, Valid.Succeeded());
    MATERIAL_CHECK(Runner, Valid.GeneratedHlsl.find("saturate((0.25f) * 2.0f)") != xr_string::npos);

    Graph.Nodes[1].Properties["code"] = xr_string("Texture2D IllegalResource");
    const FMaterialGraphCompileResult Invalid = CompileMaterialGraph(Graph, {});
    MATERIAL_CHECK(Runner, !Invalid.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Invalid.Diagnostics, "graph.custom_hlsl_forbidden_token"));
}

void TestParameterIdentityAndDescriptorIndexing(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Graph;
    FMaterialGraphNode Parameter{{"parameter-node"}, "parameter"};
    Parameter.Pins = {Pin("parameter-out", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float3)};
    Parameter.Properties["parameter_id"] = xr_string("stable-parameter-guid");
    FMaterialGraphNode Output{{"parameter-output"}, "material_output"};
    Output.Pins = {Pin("parameter-base-color", "BaseColor", EMaterialPinDirection::Input, EMaterialValueType::Float3)};
    Graph.Nodes = {Parameter, Output};
    Graph.Links = {{"parameter-link", {"parameter-out"}, {"parameter-base-color"}}};

    FMaterialGraphCompileOptions Before;
    Before.Parameters.push_back({{"stable-parameter-guid"}, "OldName", EMaterialParameterType::Float3, FFloat3{1.0f, 1.0f, 1.0f}});
    FMaterialGraphCompileOptions After = Before;
    After.Parameters.front().Name = "RenamedParameter";
    const FMaterialGraphCompileResult BeforeResult = CompileMaterialGraph(Graph, Before);
    const FMaterialGraphCompileResult AfterResult = CompileMaterialGraph(Graph, After);
    MATERIAL_CHECK(Runner, BeforeResult.Succeeded());
    MATERIAL_CHECK(Runner, BeforeResult.GeneratedHlsl == AfterResult.GeneratedHlsl);
    MATERIAL_CHECK(Runner, BeforeResult.GeneratedHlsl.find("Parameters.P_stable_parameter_guid") != xr_string::npos);

    const xr_string Template = ReadText("gamedata/shaders/r5/materials/MaterialTemplate.hlsl");
    MATERIAL_CHECK(Runner, Template.find("ResourceDescriptorHeap[NonUniformResourceIndex(ResourceIndex)]") != xr_string::npos);
    MATERIAL_CHECK(Runner, Template.find("SamplerDescriptorHeap[NonUniformResourceIndex(SamplerIndex)]") != xr_string::npos);
}

void TestTextureSampleParameterBinding(TiramisuMaterialTestRunner& Runner)
{
    FMaterialGraph Graph;
    Graph.Nodes.push_back(*CreateMaterialGraphNode("texcoord0", {"bound-uv"}));
    Graph.Nodes.push_back(*CreateMaterialGraphNode("texture_sample", {"bound-sample"}));
    Graph.Nodes.push_back(*CreateMaterialGraphNode("material_output", {"bound-output"}));
    Graph.Nodes[1].Properties["texture_parameter_id"] = xr_string{"bound-texture-guid"};

    Graph.Links = {
        {"bound-uv-link",
            GraphPin(Graph.Nodes[0], "UV", EMaterialPinDirection::Output).Id,
            GraphPin(Graph.Nodes[1], "UV", EMaterialPinDirection::Input).Id},
        {"bound-color-link",
            GraphPin(Graph.Nodes[1], "RGB", EMaterialPinDirection::Output).Id,
            GraphPin(Graph.Nodes[2], "BaseColor", EMaterialPinDirection::Input).Id},
    };

    FMaterialGraphCompileOptions Options;
    Options.Parameters.push_back({{"bound-texture-guid"}, "Albedo",
        EMaterialParameterType::Texture2D, xr_string{"textures/default"}});
    const FMaterialGraphCompileResult Result = CompileMaterialGraph(Graph, Options);
    MATERIAL_CHECK(Runner, Result.Succeeded());
    MATERIAL_CHECK(Runner,
        Result.GeneratedHlsl.find("Parameters.P_bound_texture_guid") != xr_string::npos);
    MATERIAL_CHECK(Runner,
        std::ranges::contains(Result.UsedParameters, FMaterialParameterId{"bound-texture-guid"}));
}

void TestGraphJsonAndGoldenHlsl(TiramisuMaterialTestRunner& Runner)
{
    const FMaterialAssetParseResult Asset = ParseMaterialAssetJson(
        ReadText("gamedata/render_materials/example_graph.material.json"));
    MATERIAL_CHECK(Runner, Asset.Succeeded());
    const xr_string Serialized = SerializeMaterialGraphJson(Asset.Value.Implementation.Graph);
    const FMaterialGraphParseResult Reparsed = ParseMaterialGraphJson(Serialized);
    MATERIAL_CHECK(Runner, Reparsed.Succeeded());
    MATERIAL_CHECK(Runner, SerializeMaterialGraphJson(Reparsed.Graph) == Serialized);

    FMaterialGraphCompileOptions Options;
    Options.Parameters = Asset.Value.Parameters;
    const FMaterialGraphCompileResult Compiled = CompileMaterialGraph(Asset.Value.Implementation.Graph, Options);
    MATERIAL_CHECK(Runner, Compiled.Succeeded());
    const xr_string Golden = ReadText("src/xrTiramisuMaterialCore/tests/golden/example_graph.hlsl");
    MATERIAL_CHECK(Runner, !Golden.empty());
    MATERIAL_CHECK(Runner, Compiled.GeneratedHlsl == Golden);
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrMaterialGraphTests");
    TestConstantFoldingAndDce(Runner);
    TestGraphStructuralDiagnostics(Runner);
    TestCyclesAndDivisionByZero(Runner);
    TestStaticSwitch(Runner);
    TestCustomHlslRestrictions(Runner);
    TestParameterIdentityAndDescriptorIndexing(Runner);
    TestTextureSampleParameterBinding(Runner);
    TestGraphJsonAndGoldenHlsl(Runner);
    return Runner.Finish();
}
