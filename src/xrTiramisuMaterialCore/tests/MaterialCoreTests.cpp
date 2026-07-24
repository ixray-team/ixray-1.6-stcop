#include "LegacyMaterialResolver.h"
#include "MaterialBundle.h"
#include "MaterialAsset.h"
#include "MaterialGraph.h"
#include "MaterialTypes.h"

#include <cstdlib>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <iostream>
#include <string>


namespace
{
int Failures = 0;

#define CHECK(Expression) Check((Expression), #Expression, __FILE__, __LINE__)

void Check(const bool Condition, const char* Expression, const char* File, const int Line)
{
    if (Condition)
        return;
    ++Failures;
    std::cerr << File << ':' << Line << ": check failed: " << Expression << '\n';
}

FMaterialGraphPin Pin(const xr_string& Id, const xr_string& Name, const EMaterialPinDirection Direction,
    const EMaterialValueType Type)
{
    return {{Id}, Name, Direction, Type};
}

FMaterialAsset MakeMaster()
{
    FMaterialAsset Master;
    Master.Id.Value = "master-standard";
    Master.Name = "Standard";
    Master.SourcePath = "standard.material.json";
    Master.HlslTemplate = "materials/MaterialTemplate.hlsl";
    Master.Implementation.Source = "materials/StandardSurface.hlsl";
    Master.Parameters = {
        {{"roughness"}, "Roughness", EMaterialParameterType::Scalar, 0.5f},
        {{"base-color"}, "BaseColor", EMaterialParameterType::Color, FFloat4{1.0f, 1.0f, 1.0f, 1.0f}},
        {{"base-texture"}, "BaseTexture", EMaterialParameterType::Texture2D, xr_string("textures/default.dds")},
        {{"use-detail"}, "UseDetail", EMaterialParameterType::StaticBool, false},
    };
    return Master;
}

void TestAssetParsingAndMigration()
{
    constexpr xr_string_view Json = R"json(
    {
      "version": 1,
      "id": "master-json",
      "name": "JSON master",
      "domain": "surface",
      "blend_mode": "opaque",
      "shading_model": "default_lit",
      "two_sided": false,
      "template": "materials/MaterialTemplate.hlsl",
      "implementation": {"type": "hlsl", "source": "materials/StandardSurface.hlsl"},
      "parameters": [
        {"guid": "roughness", "name": "Roughness", "type": "scalar", "default": 0.5},
        {"guid": "base-texture", "name": "BaseTexture", "type": "texture2d", "default": "textures/default.dds"}
      ],
      "static_parameters": [
        {"guid": "use-detail", "name": "UseDetail", "type": "static_bool", "default": false}
      ],
      "dependencies": []
    })json";

    const FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(Json, "master-json.material.json");
    CHECK(Parsed.Succeeded());
    CHECK(Parsed.Value.Id.Value == "master-json");
    CHECK(Parsed.Value.Parameters.size() == 3);
    CHECK(Parsed.Value.FindParameter({"base-texture"}) != nullptr);
    CHECK(SerializeMaterialAssetJson(Parsed.Value).find("asset_version") != xr_string::npos);

    const FMaterialAssetParseResult Invalid = ParseMaterialAssetJson("{}", "invalid.material.json");
    CHECK(!Invalid.Succeeded());
}

void TestInstancesAndHandles()
{
    TiramisuMaterialLibrary Library;
    const FMaterialRegistrationResult MasterRegistration = Library.RegisterMaster(MakeMaster());
    CHECK(MasterRegistration.Succeeded());

    FMaterialInstanceAsset Parent;
    Parent.Id.Value = "instance-parent";
    Parent.Parent = "master-standard";
    Parent.Overrides.emplace(FMaterialParameterId{"roughness"}, 0.25f);
    const FMaterialRegistrationResult ParentRegistration = Library.RegisterInstance(Parent);
    CHECK(ParentRegistration.Succeeded());

    FMaterialInstanceAsset Child;
    Child.Id.Value = "instance-child";
    Child.Parent = "instance-parent";
    Child.Overrides.emplace(FMaterialParameterId{"base-color"}, FFloat4{0.1f, 0.2f, 0.3f, 1.0f});
    Child.StaticOverrides.emplace(FMaterialParameterId{"use-detail"}, true);
    const FMaterialRegistrationResult ChildRegistration = Library.RegisterInstance(Child);
    CHECK(ChildRegistration.Succeeded());

    const FMaterialResolveResult Resolved = Library.Resolve("instance-child");
    CHECK(Resolved.Succeeded());
    CHECK(std::get<float>(Resolved.Value.Parameters.at({"roughness"})) == 0.25f);
    CHECK(std::get<bool>(Resolved.Value.StaticParameters.at({"use-detail"})));
    CHECK(Resolved.Value.ParentChain.size() == 2);

    TiramisuMaterialInstanceDynamic Dynamic = Library.CreateDynamic(Resolved.Value);
    const FMaterialParameterDefinition* Roughness = Library.GetMaster(MasterRegistration.Handle)->FindParameter({"roughness"});
    const FMaterialParameterDefinition* StaticSwitch = Library.GetMaster(MasterRegistration.Handle)->FindParameter({"use-detail"});
    CHECK(Dynamic.SetParameter(*Roughness, 0.75f) == EMaterialUpdateError::None);
    CHECK(Dynamic.SetParameter(*Roughness, true) == EMaterialUpdateError::TypeMismatch);
    CHECK(Dynamic.SetStaticParameter(*StaticSwitch, false) == EMaterialUpdateError::StaticParameterIsImmutable);

    const FMaterialHandle RemovedHandle = ChildRegistration.Handle;
    CHECK(Library.RemoveInstance(RemovedHandle));
    CHECK(Library.GetInstance(RemovedHandle) == nullptr);

    FMaterialInstanceAsset Replacement;
    Replacement.Id.Value = "instance-replacement";
    Replacement.Parent = "master-standard";
    const FMaterialRegistrationResult ReplacementRegistration = Library.RegisterInstance(Replacement);
    CHECK(ReplacementRegistration.Succeeded());
    CHECK(ReplacementRegistration.Handle.Index == RemovedHandle.Index);
    CHECK(ReplacementRegistration.Handle.Generation != RemovedHandle.Generation);
}

void TestParentCyclesAndTypeMismatch()
{
    TiramisuMaterialLibrary Library;
    CHECK(Library.RegisterMaster(MakeMaster()).Succeeded());

    FMaterialInstanceAsset A;
    A.Id.Value = "cycle-a";
    A.Parent = "cycle-b";
    FMaterialInstanceAsset B;
    B.Id.Value = "cycle-b";
    B.Parent = "cycle-a";
    CHECK(Library.RegisterInstance(A).Succeeded());
    CHECK(Library.RegisterInstance(B).Succeeded());
    CHECK(!Library.Resolve("cycle-a").Succeeded());

    FMaterialInstanceAsset Mismatch;
    Mismatch.Id.Value = "mismatch";
    Mismatch.Parent = "master-standard";
    Mismatch.Overrides.emplace(FMaterialParameterId{"roughness"}, true);
    CHECK(Library.RegisterInstance(Mismatch).Succeeded());
    CHECK(!Library.Resolve("mismatch").Succeeded());
}

void TestPipelineKeyDeterminism()
{
    FMaterialPipelineKey A;
    A.MasterMaterial.Value = "master";
    A.StaticParameters.emplace(FMaterialParameterId{"b"}, true);
    A.StaticParameters.emplace(FMaterialParameterId{"a"}, s32{2});
    A.VertexFactory = "level_static";
    A.RenderPassSignature = "gbuffer-v1";
    A.Backend = "vulkan";
    A.ShaderModel = "6_7";

    FMaterialPipelineKey B = A;
    CHECK(A.StableHash() == B.StableHash());
    B.Backend = "d3d12";
    CHECK(A.StableHash() != B.StableHash());
}

void TestGraphCompilerAndFolding()
{
    FMaterialGraph Graph;
    FMaterialGraphNode A{{"node-a"}, "constant"};
    A.Pins = {Pin("pin-a", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    A.Properties["value"] = 0.25f;
    FMaterialGraphNode B{{"node-b"}, "constant"};
    B.Pins = {Pin("pin-b", "Value", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    B.Properties["value"] = 0.5f;
    FMaterialGraphNode Add{{"node-add"}, "add"};
    Add.Pins = {
        Pin("pin-add-a", "A", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("pin-add-b", "B", EMaterialPinDirection::Input, EMaterialValueType::Float1),
        Pin("pin-add-out", "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1),
    };
    FMaterialGraphNode Dead{{"dead-node"}, "not_a_real_node"};
    Dead.Pins = {Pin("dead-pin", "Out", EMaterialPinDirection::Output, EMaterialValueType::Float1)};
    FMaterialGraphNode Output{{"node-output"}, "material_output"};
    Output.Pins = {Pin("pin-roughness", "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1)};
    Graph.Nodes = {A, B, Add, Dead, Output};
    Graph.Links = {
        {"link-a", {"pin-a"}, {"pin-add-a"}},
        {"link-b", {"pin-b"}, {"pin-add-b"}},
        {"link-output", {"pin-add-out"}, {"pin-roughness"}},
    };

    const FMaterialGraphCompileResult Compiled = CompileMaterialGraph(Graph, {});
    CHECK(Compiled.Succeeded());
    CHECK(Compiled.GeneratedHlsl.find("0.75f") != xr_string::npos);
    CHECK(Compiled.GeneratedHlsl.find("not_a_real_node") == xr_string::npos);

    FMaterialGraph Invalid = Graph;
    Invalid.Links.push_back({"duplicate", {"pin-b"}, {"pin-roughness"}});
    CHECK(!CompileMaterialGraph(Invalid, {}).Succeeded());
}

void TestDescriptorHeapGraphContract()
{
    FMaterialGraph Graph;
    FMaterialGraphNode Texture{{"node-texture"}, "parameter"};
    Texture.Pins = {Pin("pin-texture", "Texture", EMaterialPinDirection::Output, EMaterialValueType::Texture2D)};
    Texture.Properties["parameter_id"] = xr_string("base-texture");
    FMaterialGraphNode UV{{"node-uv"}, "texcoord0"};
    UV.Pins = {Pin("pin-uv", "UV", EMaterialPinDirection::Output, EMaterialValueType::Float2)};
    FMaterialGraphNode Sample{{"node-sample"}, "texture_sample"};
    Sample.Pins = {
        Pin("pin-sample-texture", "Texture", EMaterialPinDirection::Input, EMaterialValueType::Texture2D),
        Pin("pin-sample-uv", "UV", EMaterialPinDirection::Input, EMaterialValueType::Float2),
        Pin("pin-sample-rgb", "RGB", EMaterialPinDirection::Output, EMaterialValueType::Float3),
    };
    FMaterialGraphNode Output{{"node-output-texture"}, "material_output"};
    Output.Pins = {Pin("pin-base-color", "BaseColor", EMaterialPinDirection::Input, EMaterialValueType::Float3)};
    Graph.Nodes = {Texture, UV, Sample, Output};
    Graph.Links = {
        {"texture-link", {"pin-texture"}, {"pin-sample-texture"}},
        {"uv-link", {"pin-uv"}, {"pin-sample-uv"}},
        {"color-link", {"pin-sample-rgb"}, {"pin-base-color"}},
    };

    FMaterialGraphCompileOptions Options;
    Options.Parameters.push_back({{"base-texture"}, "BaseTexture", EMaterialParameterType::Texture2D,
        xr_string("textures/default.dds")});
    const FMaterialGraphCompileResult Compiled = CompileMaterialGraph(Graph, Options);
    CHECK(Compiled.Succeeded());
    CHECK(Compiled.GeneratedHlsl.find("SampleMaterialTexture2D(Parameters.P_base_texture") != xr_string::npos);
    CHECK(Compiled.GeneratedHlsl.find("Parameters.MaterialSamplerIndex") != xr_string::npos);
}

void TestLegacyResolutionOrder()
{
    const FLegacyMaterialMapParseResult Invalid =
        ParseLegacyMaterialMapJson("{ invalid json");
    CHECK(!Invalid.Succeeded());

    constexpr xr_string_view WrongFieldTypes = R"json({
      "asset_version": "one",
      "standard_material": 1,
      "error_material": false,
      "mappings": {"effects\\wallmark": 7}
    })json";
    CHECK(!ParseLegacyMaterialMapJson(WrongFieldTypes).Succeeded());

    constexpr xr_string_view Json = R"json({
      "asset_version": 1,
      "standard_material": "master-standard",
      "error_material": "master-error",
      "mappings": {"effects\\wallmark": "master-wallmark"}
    })json";
    const FLegacyMaterialMapParseResult Parsed = ParseLegacyMaterialMapJson(Json);
    CHECK(Parsed.Succeeded());

    FLegacyMaterialRequest Explicit;
    Explicit.ExplicitMaterial = "master-explicit";
    Explicit.ShaderName = "effects\\wallmark";
    CHECK(ResolveLegacyMaterial(Parsed.Value, Explicit).Resolution == ELegacyMaterialResolution::ExplicitMaterial);

    FLegacyMaterialRequest Mapped;
    Mapped.ShaderName = " Effects/Wallmark ";
    const FResolvedLegacyMaterial MappedResult = ResolveLegacyMaterial(Parsed.Value, Mapped);
    CHECK(MappedResult.Resolution == ELegacyMaterialResolution::LegacyMap);
    CHECK(MappedResult.LegacyShaderName == "effects\\wallmark");

    FLegacyMaterialRequest SameDynamic;
    SameDynamic.ShaderName = "effects\\wallmark";
    SameDynamic.Textures = {"brick", "brick_lmap"};
    const FResolvedLegacyMaterial FirstDynamic = ResolveLegacyMaterial(Parsed.Value, SameDynamic);
    const FResolvedLegacyMaterial SecondDynamic = ResolveLegacyMaterial(Parsed.Value, SameDynamic);
    CHECK(MakeLegacyMaterialInstanceCacheKey(FirstDynamic) ==
        MakeLegacyMaterialInstanceCacheKey(SecondDynamic));
    SameDynamic.Textures[1] = "other_lmap";
    CHECK(MakeLegacyMaterialInstanceCacheKey(FirstDynamic) !=
        MakeLegacyMaterialInstanceCacheKey(ResolveLegacyMaterial(Parsed.Value, SameDynamic)));

    FLegacyMaterialRequest Automatic;
    Automatic.ShaderName = "levels\\old";
    Automatic.Textures = {"levels\\old_wall"};
    CHECK(ResolveLegacyMaterial(Parsed.Value, Automatic).Resolution == ELegacyMaterialResolution::AutomaticStandard);

    FLegacyMaterialRequest Missing;
    CHECK(ResolveLegacyMaterial(Parsed.Value, Missing).Resolution == ELegacyMaterialResolution::ErrorMaterial);
}
} // namespace
xr_string ReadTextFile(const xr_string& Path)
{
    std::ifstream Stream(Path.c_str(), std::ios::binary);
    std::ostringstream Text;
    Text << Stream.rdbuf();
    return Text.str();
}

void TestRepositoryAssets()
{
    const xr_string StandardJson = ReadTextFile("gamedata/render_materials/standard_surface.material.json");
    const xr_string ErrorJson = ReadTextFile("gamedata/render_materials/error.material.json");
    const xr_string GraphJson = ReadTextFile("gamedata/render_materials/example_graph.material.json");
    const xr_string InstanceJson = ReadTextFile("gamedata/render_materials/example_red.material-instance.json");
    const xr_string LegacyJson = ReadTextFile("gamedata/render_materials/legacy-map.json");
    CHECK(!StandardJson.empty());
    CHECK(ParseMaterialAssetJson(StandardJson).Succeeded());
    CHECK(ParseMaterialAssetJson(ErrorJson).Succeeded());
    const FMaterialAssetParseResult GraphAsset = ParseMaterialAssetJson(GraphJson);
    CHECK(GraphAsset.Succeeded());
    CHECK(ParseMaterialInstanceJson(InstanceJson).Succeeded());
    CHECK(ParseLegacyMaterialMapJson(LegacyJson).Succeeded());

    FMaterialGraphCompileOptions Options;
    Options.Parameters = GraphAsset.Value.Parameters;
    for (const FMaterialParameterDefinition& Parameter : GraphAsset.Value.Parameters)
        if (Parameter.IsStatic())
            Options.StaticParameters.emplace(Parameter.Id, Parameter.DefaultValue);
    const FMaterialGraphCompileResult Compiled = CompileMaterialGraph(GraphAsset.Value.Implementation.Graph, Options);
    CHECK(Compiled.Succeeded());
    CHECK(Compiled.GeneratedHlsl.find("Parameters.P_190412be_3265_452a_94db_d4ee1a545fc8") != xr_string::npos);

    const xr_string Template = ReadTextFile("gamedata/shaders/r5/materials/MaterialTemplate.hlsl");
    CHECK(Template.find("ResourceDescriptorHeap") != xr_string::npos);
    CHECK(Template.find("SamplerDescriptorHeap") != xr_string::npos);
}

void TestMaterialBundle()
{
    FMaterialBundle Bundle;
    Bundle.CompleteShaderSet = true;
    Bundle.Records = {
        {EMaterialBundleRecordType::FlattenedInstance, {"instance-b"}, {"master-a"}, "b.json", "payload-b", "", {"z", "a", "a"}},
        {EMaterialBundleRecordType::Master, {"master-a"}, {"master-a"}, "a.json", "payload-a", "hlsl-a", {"template"}},
    };
    Bundle.ShaderBlobs = {
        {{"master-a"}, 20, EMaterialShaderBlobFormat::SpirV, "main", {4, 5, 6}},
        {{"master-a"}, 10, EMaterialShaderBlobFormat::Dxil, "main", {1, 2, 3}},
    };

    const FMaterialBundleWriteResult First = SerializeMaterialBundle(Bundle);
    CHECK(First.Succeeded());
    std::ranges::reverse(Bundle.Records);
    std::ranges::reverse(Bundle.ShaderBlobs);
    const FMaterialBundleWriteResult Second = SerializeMaterialBundle(Bundle);
    CHECK(Second.Succeeded());
    CHECK(First.Data == Second.Data);

    const FMaterialBundleReadResult Read = DeserializeMaterialBundle(First.Data);
    CHECK(Read.Succeeded());
    CHECK(Read.Value.CompleteShaderSet);
    CHECK(Read.Value.Records.size() == 2);
    CHECK(Read.Value.ShaderBlobs.size() == 2);
    CHECK(Read.Value.Records[0].Dependencies.size() == 2);

    xr_vector<u8> Corrupted = First.Data;
    Corrupted[12] ^= 1;
    CHECK(!DeserializeMaterialBundle(Corrupted).Succeeded());

    FMaterialBundle Invalid;
    Invalid.CompleteShaderSet = true;
    CHECK(!SerializeMaterialBundle(Invalid).Succeeded());
}


int main()
{
    TestAssetParsingAndMigration();
    TestInstancesAndHandles();
    TestParentCyclesAndTypeMismatch();
    TestPipelineKeyDeterminism();
    TestGraphCompilerAndFolding();
    TestDescriptorHeapGraphContract();
    TestLegacyResolutionOrder();
    TestRepositoryAssets();
    TestMaterialBundle();

    if (Failures != 0)
    {
        std::cerr << Failures << " material core test(s) failed.\n";
        return EXIT_FAILURE;
    }
    std::cout << "All xrTiramisuMaterialCore tests passed.\n";
    return EXIT_SUCCESS;
}
