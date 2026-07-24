#include "MaterialAsset.h"
#include "MaterialTestHarness.h"

#include <limits>
#include <string>

namespace
{
FMaterialAsset MakeMaster()
{
    FMaterialAsset Master;
    Master.Id.Value = "asset-master";
    Master.Name = "Master";
    Master.SourcePath = "materials/asset-master.material.json";
    Master.HlslTemplate = "materials/MaterialTemplate.hlsl";
    Master.Implementation.Source = "materials/StandardSurface.hlsl";
    Master.Parameters = {
        {{"roughness-guid"}, "Roughness", EMaterialParameterType::Scalar, 0.5f},
        {{"tint-guid"}, "Tint", EMaterialParameterType::Color, FFloat4{1.0f, 1.0f, 1.0f, 1.0f}},
        {{"texture-guid"}, "Texture", EMaterialParameterType::Texture2D, xr_string("default")},
        {{"switch-guid"}, "UseFeature", EMaterialParameterType::StaticBool, false},
        {{"quality-guid"}, "Quality", EMaterialParameterType::StaticEnum, s32{1}},
    };
    return Master;
}

void TestSchemaValidation(TiramisuMaterialTestRunner& Runner)
{
    const FMaterialAssetParseResult InvalidJson = ParseMaterialAssetJson("{ invalid json");
    MATERIAL_CHECK(Runner, !InvalidJson.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidJson.Diagnostics, "asset.invalid_json"));

    const FMaterialAssetParseResult InvalidRoot = ParseMaterialAssetJson("[]");
    MATERIAL_CHECK(Runner, !InvalidRoot.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidRoot.Diagnostics, "asset.invalid_root"));

    constexpr xr_string_view WrongFieldTypes = R"json({
      "asset_version": "one",
      "guid": 42,
      "name": [],
      "domain": true,
      "blend_mode": {},
      "shading_model": 7,
      "two_sided": "yes",
      "template": false,
      "implementation": {"type": 1},
      "parameters": {},
      "static_parameters": "none",
      "dependencies": ["valid", 9]
    })json";
    const FMaterialAssetParseResult WrongFieldTypesResult = ParseMaterialAssetJson(WrongFieldTypes);
    MATERIAL_CHECK(Runner, !WrongFieldTypesResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongFieldTypesResult.Diagnostics, "asset.invalid_field_type"));
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongFieldTypesResult.Diagnostics, "asset.invalid_dependency"));

    constexpr xr_string_view WrongVectorComponent = R"json({
      "asset_version": 1,
      "guid": "wrong-vector",
      "domain": "surface",
      "blend_mode": "opaque",
      "shading_model": "default_lit",
      "template": "template",
      "implementation": {"type": "hlsl", "source": "source"},
      "parameters": [{"guid": "float2-guid", "name": "Offset", "type": "float2", "default": [1.0, "bad"]}],
      "static_parameters": []
    })json";
    const FMaterialAssetParseResult WrongVectorResult = ParseMaterialAssetJson(WrongVectorComponent);
    MATERIAL_CHECK(Runner, !WrongVectorResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongVectorResult.Diagnostics, "asset.parameter_default_type"));

    constexpr xr_string_view Migrated = R"json({
      "version": 1,
      "id": "migrated-master",
      "name": "Migrated",
      "domain": "surface",
      "blend_mode": "opaque",
      "shading_model": "default_lit",
      "template": "materials/MaterialTemplate.hlsl",
      "implementation": {"type": "hlsl", "source": "materials/StandardSurface.hlsl"},
      "parameters": [],
      "static_parameters": [],
      "dependencies": []
    })json";
    const FMaterialAssetParseResult MigratedResult = ParseMaterialAssetJson(Migrated);
    MATERIAL_CHECK(Runner, MigratedResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(MigratedResult.Diagnostics, "asset.migrated_version_field"));
    MATERIAL_CHECK(Runner, HasDiagnostic(MigratedResult.Diagnostics, "asset.migrated_id_field"));

    constexpr xr_string_view InvalidEnums = R"json({
      "asset_version": 1,
      "guid": "invalid-enums",
      "domain": "volume",
      "blend_mode": "unknown",
      "shading_model": "unknown",
      "template": "template",
      "implementation": {"type": "hlsl", "source": "source"},
      "parameters": [],
      "static_parameters": []
    })json";
    const FMaterialAssetParseResult InvalidEnumsResult = ParseMaterialAssetJson(InvalidEnums);
    MATERIAL_CHECK(Runner, !InvalidEnumsResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidEnumsResult.Diagnostics, "asset.invalid_domain"));
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidEnumsResult.Diagnostics, "asset.invalid_blend"));
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidEnumsResult.Diagnostics, "asset.invalid_shading_model"));

    constexpr xr_string_view DuplicateParameter = R"json({
      "asset_version": 1,
      "guid": "duplicate-parameter",
      "domain": "surface",
      "blend_mode": "opaque",
      "shading_model": "default_lit",
      "template": "template",
      "implementation": {"type": "hlsl", "source": "source"},
      "parameters": [
        {"guid": "same-guid", "name": "A", "type": "scalar", "default": 1.0},
        {"guid": "same-guid", "name": "B", "type": "scalar", "default": 2.0}
      ],
      "static_parameters": []
    })json";
    const FMaterialAssetParseResult DuplicateResult = ParseMaterialAssetJson(DuplicateParameter);
    MATERIAL_CHECK(Runner, !DuplicateResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(DuplicateResult.Diagnostics, "asset.invalid_parameter_id"));

    constexpr xr_string_view WrongDefault = R"json({
      "asset_version": 1,
      "guid": "wrong-default",
      "domain": "surface",
      "blend_mode": "opaque",
      "shading_model": "default_lit",
      "template": "template",
      "implementation": {"type": "hlsl", "source": "source"},
      "parameters": [{"guid": "scalar-guid", "name": "Scalar", "type": "scalar", "default": true}],
      "static_parameters": []
    })json";
    const FMaterialAssetParseResult WrongDefaultResult = ParseMaterialAssetJson(WrongDefault);
    MATERIAL_CHECK(Runner, !WrongDefaultResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongDefaultResult.Diagnostics, "asset.parameter_default_type"));
}

void TestInstanceSchemaRestrictions(TiramisuMaterialTestRunner& Runner)
{
    const FMaterialInstanceParseResult InvalidJson =
        ParseMaterialInstanceJson("{ invalid json");
    MATERIAL_CHECK(Runner, !InvalidJson.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidJson.Diagnostics, "asset.invalid_json"));

    constexpr xr_string_view WrongFieldTypes = R"json({
      "asset_version": 1,
      "guid": "wrong-instance-fields",
      "name": 12,
      "parent": false,
      "overrides": [],
      "static_overrides": "none"
    })json";
    const FMaterialInstanceParseResult WrongFieldTypesResult = ParseMaterialInstanceJson(WrongFieldTypes);
    MATERIAL_CHECK(Runner, !WrongFieldTypesResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(WrongFieldTypesResult.Diagnostics, "asset.invalid_field_type"));

    constexpr xr_string_view Forbidden = R"json({
      "asset_version": 1,
      "guid": "forbidden-instance",
      "name": "Forbidden",
      "parent": "asset-master",
      "domain": "surface",
      "blend_mode": "masked",
      "shading_model": "unlit",
      "overrides": {},
      "static_overrides": {}
    })json";
    const FMaterialInstanceParseResult Result = ParseMaterialInstanceJson(Forbidden);
    MATERIAL_CHECK(Runner, !Result.Succeeded());
    int ForbiddenCount = 0;
    for (const FMaterialDiagnostic& Diagnostic : Result.Diagnostics)
        if (Diagnostic.Code == "instance.forbidden_master_property")
            ++ForbiddenCount;
    MATERIAL_CHECK(Runner, ForbiddenCount == 3);
}

void TestInheritanceAndPrecedence(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialLibrary Library;
    const FMaterialRegistrationResult Master = Library.RegisterMaster(MakeMaster());
    MATERIAL_CHECK(Runner, Master.Succeeded());

    FMaterialInstanceAsset Parent;
    Parent.Id.Value = "asset-parent";
    Parent.SourcePath = "materials/parent.material-instance.json";
    Parent.Parent = ".\\materials\\asset-master.material.json";
    Parent.Overrides.emplace(FMaterialParameterId{"roughness-guid"}, 0.25f);
    Parent.StaticOverrides.emplace(FMaterialParameterId{"quality-guid"}, s32{2});
    MATERIAL_CHECK(Runner, Library.RegisterInstance(Parent).Succeeded());

    FMaterialInstanceAsset Child;
    Child.Id.Value = "asset-child";
    Child.Parent = "materials\\parent.material-instance.json";
    Child.Overrides.emplace(FMaterialParameterId{"roughness-guid"}, 0.75f);
    Child.StaticOverrides.emplace(FMaterialParameterId{"switch-guid"}, true);
    MATERIAL_CHECK(Runner, Library.RegisterInstance(Child).Succeeded());

    FMaterialInstanceAsset DuplicatePath;
    DuplicatePath.Id.Value = "duplicate-source-path";
    DuplicatePath.SourcePath = "materials\\asset-master.material.json";
    DuplicatePath.Parent = "asset-master";
    const FMaterialRegistrationResult DuplicateRegistration =
        Library.RegisterInstance(DuplicatePath);
    MATERIAL_CHECK(Runner, !DuplicateRegistration.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(DuplicateRegistration.Diagnostics,
        "library.duplicate_reference"));

    const FMaterialResolveResult Resolved = Library.Resolve("asset-child");
    MATERIAL_CHECK(Runner, Resolved.Succeeded());
    MATERIAL_CHECK(Runner, std::get<float>(Resolved.Value.Parameters.at({"roughness-guid"})) == 0.75f);
    MATERIAL_CHECK(Runner, std::get<s32>(Resolved.Value.StaticParameters.at({"quality-guid"})) == 2);
    MATERIAL_CHECK(Runner, std::get<bool>(Resolved.Value.StaticParameters.at({"switch-guid"})));
    MATERIAL_CHECK(Runner, Resolved.Value.ParentChain.size() == 2);
    MATERIAL_CHECK(Runner, Resolved.Value.MasterId.Value == "asset-master");
}

void TestStableParameterGuidAcrossRename(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialLibrary Library;
    const FMaterialRegistrationResult MasterRegistration = Library.RegisterMaster(MakeMaster());
    MATERIAL_CHECK(Runner, MasterRegistration.Succeeded());

    FMaterialInstanceAsset Instance;
    Instance.Id.Value = "rename-instance";
    Instance.Parent = "asset-master";
    Instance.Overrides.emplace(FMaterialParameterId{"roughness-guid"}, 0.2f);
    MATERIAL_CHECK(Runner, Library.RegisterInstance(Instance).Succeeded());
    MATERIAL_CHECK(Runner, std::get<float>(Library.Resolve("rename-instance").Value.Parameters.at({"roughness-guid"})) == 0.2f);

    FMaterialAsset Renamed = *Library.GetMaster(MasterRegistration.Handle);
    Renamed.Parameters.front().Name = "Microsurface";
    Renamed.Parameters.front().DisplayName = "Microsurface (renamed)";
    MATERIAL_CHECK(Runner, Library.ReloadMaster(MasterRegistration.Handle, Renamed));

    const FMaterialResolveResult AfterRename = Library.Resolve("rename-instance");
    MATERIAL_CHECK(Runner, AfterRename.Succeeded());
    MATERIAL_CHECK(Runner, std::get<float>(AfterRename.Value.Parameters.at({"roughness-guid"})) == 0.2f);
    MATERIAL_CHECK(Runner, Library.GetMaster(MasterRegistration.Handle)->FindParameter({"roughness-guid"})->Name == "Microsurface");
}

void TestCyclesAndOverrideErrors(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialLibrary Library;
    MATERIAL_CHECK(Runner, Library.RegisterMaster(MakeMaster()).Succeeded());

    FMaterialInstanceAsset A;
    A.Id.Value = "cycle-a";
    A.Parent = "cycle-b";
    FMaterialInstanceAsset B;
    B.Id.Value = "cycle-b";
    B.Parent = "cycle-a";
    MATERIAL_CHECK(Runner, Library.RegisterInstance(A).Succeeded());
    MATERIAL_CHECK(Runner, Library.RegisterInstance(B).Succeeded());
    const FMaterialResolveResult Cycle = Library.Resolve("cycle-a");
    MATERIAL_CHECK(Runner, !Cycle.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Cycle.Diagnostics, "instance.parent_cycle"));

    FMaterialInstanceAsset Invalid;
    Invalid.Id.Value = "invalid-overrides";
    Invalid.Parent = "asset-master";
    Invalid.Overrides.emplace(FMaterialParameterId{"switch-guid"}, true);
    Invalid.StaticOverrides.emplace(FMaterialParameterId{"roughness-guid"}, 0.5f);
    Invalid.Overrides.emplace(FMaterialParameterId{"missing-guid"}, 1.0f);
    MATERIAL_CHECK(Runner, Library.RegisterInstance(Invalid).Succeeded());
    const FMaterialResolveResult InvalidResult = Library.Resolve("invalid-overrides");
    MATERIAL_CHECK(Runner, !InvalidResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidResult.Diagnostics, "instance.static_in_runtime_overrides"));
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidResult.Diagnostics, "instance.runtime_in_static_overrides"));
    MATERIAL_CHECK(Runner, HasDiagnostic(InvalidResult.Diagnostics, "instance.unknown_parameter"));
}

void TestDynamicParametersAndHandles(TiramisuMaterialTestRunner& Runner)
{
    TiramisuMaterialLibrary Library;
    const FMaterialRegistrationResult MasterRegistration = Library.RegisterMaster(MakeMaster());
    MATERIAL_CHECK(Runner, MasterRegistration.Succeeded());
    const FMaterialResolveResult Resolved = Library.Resolve("asset-master");
    MATERIAL_CHECK(Runner, Resolved.Succeeded());
    TiramisuMaterialInstanceDynamic Dynamic = Library.CreateDynamic(Resolved.Value);
    const FMaterialAsset* Master = Library.GetMaster(MasterRegistration.Handle);

    MATERIAL_CHECK(Runner, Dynamic.SetParameter(*Master->FindParameter({"roughness-guid"}), 0.1f) == EMaterialUpdateError::None);
    MATERIAL_CHECK(Runner, Dynamic.SetParameter(*Master->FindParameter({"tint-guid"}), FFloat4{0.1f, 0.2f, 0.3f, 1.0f}) == EMaterialUpdateError::None);
    MATERIAL_CHECK(Runner, Dynamic.SetParameter(*Master->FindParameter({"texture-guid"}), xr_string("new-texture")) == EMaterialUpdateError::None);
    MATERIAL_CHECK(Runner, Dynamic.SetParameter(*Master->FindParameter({"texture-guid"}), 1.0f) == EMaterialUpdateError::TypeMismatch);
    MATERIAL_CHECK(Runner, Dynamic.SetParameter(*Master->FindParameter({"switch-guid"}), true) == EMaterialUpdateError::StaticParameterIsImmutable);

    const FMaterialHandle Removed = MasterRegistration.Handle;
    MATERIAL_CHECK(Runner, Library.RemoveMaster(Removed));
    MATERIAL_CHECK(Runner, Library.GetMaster(Removed) == nullptr);
    const FMaterialRegistrationResult Replacement = Library.RegisterMaster(MakeMaster());
    MATERIAL_CHECK(Runner, Replacement.Handle.Index == Removed.Index);
    MATERIAL_CHECK(Runner, Replacement.Handle.Generation != Removed.Generation);
}

void TestSerializationRoundTrip(TiramisuMaterialTestRunner& Runner)
{
    const FMaterialAsset Master = MakeMaster();
    const FMaterialAssetParseResult MasterRoundTrip = ParseMaterialAssetJson(SerializeMaterialAssetJson(Master));
    MATERIAL_CHECK(Runner, MasterRoundTrip.Succeeded());
    MATERIAL_CHECK(Runner, MasterRoundTrip.Value.Id == Master.Id);
    MATERIAL_CHECK(Runner, MasterRoundTrip.Value.Parameters.size() == Master.Parameters.size());

    FMaterialInstanceAsset Instance;
    Instance.Id.Value = "roundtrip-instance";
    Instance.Parent = Master.Id.Value;
    Instance.Overrides.emplace(FMaterialParameterId{"roughness-guid"}, 0.3f);
    Instance.StaticOverrides.emplace(FMaterialParameterId{"switch-guid"}, true);
    const FMaterialInstanceParseResult InstanceRoundTrip = ParseMaterialInstanceJson(SerializeMaterialInstanceJson(Instance));
    MATERIAL_CHECK(Runner, InstanceRoundTrip.Succeeded());
    MATERIAL_CHECK(Runner, InstanceRoundTrip.Value.Overrides == Instance.Overrides);
    MATERIAL_CHECK(Runner, InstanceRoundTrip.Value.StaticOverrides == Instance.StaticOverrides);
}

void TestPermutationStatistics(TiramisuMaterialTestRunner& Runner)
{
    xr_vector<FMaterialParameterDefinition> Parameters;
    Parameters.push_back({{"bool-a"}, "A", EMaterialParameterType::StaticBool, false});
    Parameters.push_back({{"bool-b"}, "B", EMaterialParameterType::StaticBool, true});
    FMaterialParameterDefinition Quality{{"quality"}, "Quality",
        EMaterialParameterType::StaticEnum, s32{1}};
    Quality.Minimum = 0.0f;
    Quality.Maximum = 3.0f;
    Parameters.push_back(Quality);

    const FMaterialPermutationStatistics Exact =
        CalculateMaterialPermutationStatistics(Parameters);
    MATERIAL_CHECK(Runner, Exact.Exact);
    MATERIAL_CHECK(Runner, !Exact.Overflow);
    MATERIAL_CHECK(Runner, Exact.PermutationCount == 16);
    MATERIAL_CHECK(Runner, Exact.StaticBoolParameters == 2);
    MATERIAL_CHECK(Runner, Exact.StaticEnumParameters == 1);

    Parameters.back().Maximum.reset();
    const FMaterialPermutationStatistics LowerBound =
        CalculateMaterialPermutationStatistics(Parameters);
    MATERIAL_CHECK(Runner, !LowerBound.Exact);
    MATERIAL_CHECK(Runner, LowerBound.PermutationCount == 4);

    Parameters.clear();
    for (size_t Index = 0; Index < 64; ++Index)
        Parameters.push_back({{"overflow-" + std::to_string(Index)}, "Switch",
            EMaterialParameterType::StaticBool, false});
    const FMaterialPermutationStatistics Overflow =
        CalculateMaterialPermutationStatistics(Parameters);
    MATERIAL_CHECK(Runner, Overflow.Overflow);
    MATERIAL_CHECK(Runner, !Overflow.Exact);
    MATERIAL_CHECK(Runner,
        Overflow.PermutationCount == std::numeric_limits<u64>::max());
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrMaterialAssetTests");
    TestSchemaValidation(Runner);
    TestInstanceSchemaRestrictions(Runner);
    TestInheritanceAndPrecedence(Runner);
    TestStableParameterGuidAcrossRename(Runner);
    TestCyclesAndOverrideErrors(Runner);
    TestDynamicParametersAndHandles(Runner);
    TestSerializationRoundTrip(Runner);
    TestPermutationStatistics(Runner);
    return Runner.Finish();
}
