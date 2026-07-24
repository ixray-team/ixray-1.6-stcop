#include "MaterialParameterLayout.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <vector>

namespace
{
xr_vector<FMaterialParameterDefinition> Definitions()
{
	return 
	{
		{{"d-texture"}, "BaseTexture", EMaterialParameterType::Texture2D, xr_string("textures/default")},
		{{"b-scalar"}, "Roughness", EMaterialParameterType::Scalar, 0.5f},
		{{"z-static"}, "UseFeature", EMaterialParameterType::StaticBool, false},
		{{"e-sampler"}, "SurfaceSampler", EMaterialParameterType::SamplerPreset, xr_string("linear_wrap")},
		{{"a-color"}, "BaseColor", EMaterialParameterType::Color, FFloat4{1.0f, 1.0f, 1.0f, 1.0f}},
		{{"c-float3"}, "Direction", EMaterialParameterType::Float3, FFloat3{1.0f, 2.0f, 3.0f}},
	};
}

template <typename T>
T Read(const xr_vector<u8>& Data, const u32 Offset)
{
	T Value{};
	std::memcpy(&Value, Data.data() + Offset, sizeof(T));
	return Value;
}

void TestDeterministicLayout(TiramisuMaterialTestRunner& Runner)
{
	xr_vector<FMaterialParameterDefinition> FirstDefinitions = Definitions();
	const FMaterialParameterLayoutResult First = BuildMaterialParameterLayout(FirstDefinitions);
	MATERIAL_CHECK(Runner, First.Succeeded());
	MATERIAL_CHECK(Runner, First.Value.Version == MaterialParameterLayoutVersion);
	MATERIAL_CHECK(Runner, First.Value.Fields.size() == 5);
	MATERIAL_CHECK(Runner, First.Value.ByteSize == 48);
	MATERIAL_CHECK(Runner, First.Value.ByteSize % MaterialParameterBlockAlignment == 0);
	MATERIAL_CHECK(Runner, First.Value.Fields[0] == FMaterialParameterLayoutField({"a-color"}, EMaterialParameterType::Color, 0, 16));
	MATERIAL_CHECK(Runner, First.Value.Fields[1] == FMaterialParameterLayoutField({"b-scalar"}, EMaterialParameterType::Scalar, 16, 4));
	MATERIAL_CHECK(Runner, First.Value.Fields[2] == FMaterialParameterLayoutField({"c-float3"}, EMaterialParameterType::Float3, 20, 12));
	MATERIAL_CHECK(Runner, First.Value.Fields[3] == FMaterialParameterLayoutField({"d-texture"}, EMaterialParameterType::Texture2D, 32, 4));
	MATERIAL_CHECK(Runner, First.Value.Fields[4] == FMaterialParameterLayoutField({"e-sampler"}, EMaterialParameterType::SamplerPreset, 36, 4));
	MATERIAL_CHECK(Runner, First.Value.Find({"z-static"}) == nullptr);
	MATERIAL_CHECK(Runner, First.Value.Find({"c-float3"}) == &First.Value.Fields[2]);

	std::ranges::reverse(FirstDefinitions);
	FirstDefinitions[0].Name = "Renamed Direction";
	const FMaterialParameterLayoutResult Reordered = BuildMaterialParameterLayout(FirstDefinitions);
	MATERIAL_CHECK(Runner, Reordered.Succeeded());
	MATERIAL_CHECK(Runner, Reordered.Value == First.Value);

	const xr_string Hlsl = GenerateMaterialParameterHlsl(First.Value);
	MATERIAL_CHECK(Runner, Hlsl.find("MATERIAL_PARAMETER_BLOCK_SIZE 48u") != xr_string::npos);
	MATERIAL_CHECK(Runner, Hlsl.find("LoadMaterialParameters(ByteAddressBuffer") != xr_string::npos);
	MATERIAL_CHECK(Runner, Hlsl.find("Load3(DataOffset + 20u)") != xr_string::npos);
	MATERIAL_CHECK(Runner, Hlsl.find("P_d_texture = MaterialData.Load(DataOffset + 32u)") != xr_string::npos);
	MATERIAL_CHECK(Runner, Hlsl.find("Result.MaterialSamplerIndex = DefaultSamplerIndex") != xr_string::npos);
}

void TestPackingAndDescriptorRelocation(TiramisuMaterialTestRunner& Runner)
{
	const xr_vector<FMaterialParameterDefinition> ParameterDefinitions = Definitions();
	const FMaterialParameterLayoutResult Layout = BuildMaterialParameterLayout(ParameterDefinitions);
	MATERIAL_CHECK(Runner, Layout.Succeeded());

	FMaterialParameterMap Overrides;
	Overrides.emplace(FMaterialParameterId{"b-scalar"}, 0.25f);
	Overrides.emplace(FMaterialParameterId{"d-texture"}, xr_string("textures/brick"));
	const FMaterialParameterPackResult Packed = PackMaterialParameters(Layout.Value, ParameterDefinitions, Overrides);

	MATERIAL_CHECK(Runner, Packed.Succeeded());
	MATERIAL_CHECK(Runner, Packed.Value.LayoutHash == Layout.Value.StableHash);
	MATERIAL_CHECK(Runner, Packed.Value.Data.size() == 48);
	MATERIAL_CHECK(Runner, Read<FFloat4>(Packed.Value.Data, 0) == FFloat4({1.0f, 1.0f, 1.0f, 1.0f}));
	MATERIAL_CHECK(Runner, Read<float>(Packed.Value.Data, 16) == 0.25f);
	MATERIAL_CHECK(Runner, Read<FFloat3>(Packed.Value.Data, 20) == FFloat3({1.0f, 2.0f, 3.0f}));
	MATERIAL_CHECK(Runner, Read<u32>(Packed.Value.Data, 32) == FDescriptorHeapIndex::Invalid);
	MATERIAL_CHECK(Runner, Read<u32>(Packed.Value.Data, 36) == FDescriptorHeapIndex::Invalid);
	MATERIAL_CHECK(Runner, Packed.Value.Resources.size() == 2);
	MATERIAL_CHECK(Runner, Packed.Value.Resources[0].AssetPath == "textures/brick");
	MATERIAL_CHECK(Runner, Packed.Value.Resources[1].AssetPath == "linear_wrap");

	const FMaterialParameterPackResult Patched = PatchMaterialParameterResources
	(
		Packed.Value,
		[](const EMaterialParameterType Type, const xr_string_view Path) -> xr_optional<FDescriptorHeapIndex>
		{
			if (Type == EMaterialParameterType::Texture2D && Path == "textures/brick")
			{
				return FDescriptorHeapIndex{17};
			}
			if (Type == EMaterialParameterType::SamplerPreset && Path == "linear_wrap")
			{
				return FDescriptorHeapIndex{3};
			}

			return std::nullopt; 
		}
	);

	MATERIAL_CHECK(Runner, Patched.Succeeded());
	MATERIAL_CHECK(Runner, Read<u32>(Patched.Value.Data, 32) == 17);
	MATERIAL_CHECK(Runner, Read<u32>(Patched.Value.Data, 36) == 3);
	MATERIAL_CHECK(Runner, Read<u32>(Packed.Value.Data, 32) == FDescriptorHeapIndex::Invalid);

	const FMaterialParameterPackResult PatchedByStableParameter = PatchMaterialParameterResources
	(
		Packed.Value, 
		[](const FMaterialParameterResourceReference& Reference) -> xr_optional<FDescriptorHeapIndex>				
		{
			if (Reference.Parameter == FMaterialParameterId{"d-texture"})
			{
				return FDescriptorHeapIndex{29};
			}
			if (Reference.Parameter == FMaterialParameterId{"e-sampler"})
			{
				return FDescriptorHeapIndex{2};
			}
			return std::nullopt; 
		}
	);

	MATERIAL_CHECK(Runner, PatchedByStableParameter.Succeeded());
	MATERIAL_CHECK(Runner, Read<u32>(PatchedByStableParameter.Value.Data, 32) == 29);
	MATERIAL_CHECK(Runner, Read<u32>(PatchedByStableParameter.Value.Data, 36) == 2);

	const FMaterialParameterPackResult Unresolved = PatchMaterialParameterResources(Packed.Value, [](EMaterialParameterType, xr_string_view) -> xr_optional<FDescriptorHeapIndex>
																					{ return std::nullopt; });
	MATERIAL_CHECK(Runner, !Unresolved.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(Unresolved.Diagnostics, "parameter_patch.unresolved_resource"));
}

void TestValidation(TiramisuMaterialTestRunner& Runner)
{
	xr_vector<FMaterialParameterDefinition> Duplicate = Definitions();
	Duplicate.push_back(Duplicate.front());
	const FMaterialParameterLayoutResult DuplicateResult = BuildMaterialParameterLayout(Duplicate);
	MATERIAL_CHECK(Runner, !DuplicateResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(DuplicateResult.Diagnostics, "parameter_layout.duplicate_id"));

	xr_vector<FMaterialParameterDefinition> InvalidDefault = Definitions();
	InvalidDefault[1].DefaultValue = true;
	const FMaterialParameterLayoutResult DefaultResult = BuildMaterialParameterLayout(InvalidDefault);
	MATERIAL_CHECK(Runner, !DefaultResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(DefaultResult.Diagnostics, "parameter_layout.invalid_default"));

	xr_vector<FMaterialParameterDefinition> HlslCollision = 
	{
		{{"same-field"}, "A", EMaterialParameterType::Scalar, 0.0f},
		{{"same_field"}, "B", EMaterialParameterType::Scalar, 0.0f},
	};

	const FMaterialParameterLayoutResult CollisionResult = BuildMaterialParameterLayout(HlslCollision);
	MATERIAL_CHECK(Runner, !CollisionResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(CollisionResult.Diagnostics, "parameter_layout.hlsl_name_collision"));

	const xr_vector<FMaterialParameterDefinition> ParameterDefinitions = Definitions();
	const FMaterialParameterLayoutResult Layout = BuildMaterialParameterLayout(ParameterDefinitions);
	FMaterialParameterMap Unknown = {{{"unknown"}, 1.0f}};
	const FMaterialParameterPackResult UnknownResult = PackMaterialParameters(Layout.Value, ParameterDefinitions, Unknown);
	MATERIAL_CHECK(Runner, !UnknownResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(UnknownResult.Diagnostics, "parameter_pack.unknown_parameter"));

	FMaterialParameterMap Static = {{{"z-static"}, false}};
	const FMaterialParameterPackResult StaticResult = PackMaterialParameters(Layout.Value, ParameterDefinitions, Static);
	MATERIAL_CHECK(Runner, !StaticResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(StaticResult.Diagnostics, "parameter_pack.static_parameter"));

	FMaterialParameterMap WrongType = {{{"b-scalar"}, FFloat2{1.0f, 2.0f}}};
	const FMaterialParameterPackResult TypeResult = PackMaterialParameters(Layout.Value, ParameterDefinitions, WrongType);
	MATERIAL_CHECK(Runner, !TypeResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(TypeResult.Diagnostics, "parameter_pack.type_mismatch"));
}

void TestGpuAbi(TiramisuMaterialTestRunner& Runner)
{
	MATERIAL_CHECK(Runner, MaterialGpuAbiVersion == 2);
	MATERIAL_CHECK(Runner, sizeof(FMaterialLightGpuData) == MaterialLightGpuDataSize);
	MATERIAL_CHECK(Runner, sizeof(FMaterialInstanceGpuData) == MaterialInstanceGpuDataSize);
	MATERIAL_CHECK(Runner, offsetof(FMaterialInstanceGpuData, ParameterDataOffset) == 0);
	MATERIAL_CHECK(Runner, offsetof(FMaterialInstanceGpuData, ParameterDataSize) == 4);
	MATERIAL_CHECK(Runner, offsetof(FMaterialInstanceGpuData, LayoutHashLow) == 8);
	MATERIAL_CHECK(Runner, offsetof(FMaterialInstanceGpuData, LayoutHashHigh) == 12);
	MATERIAL_CHECK(Runner, sizeof(FMaterialDrawGpuData) == MaterialDrawGpuDataSize);
	MATERIAL_CHECK(Runner, offsetof(FMaterialDrawGpuData, LocalToWorld) == 0);
	MATERIAL_CHECK(Runner, offsetof(FMaterialDrawGpuData, PreviousLocalToWorld) == 64);
	MATERIAL_CHECK(Runner, offsetof(FMaterialDrawGpuData, MaterialInstanceIndex) == 128);
	MATERIAL_CHECK(Runner, offsetof(FMaterialDrawGpuData, ObjectId) == 132);
	MATERIAL_CHECK(Runner, offsetof(FMaterialDrawGpuData, Flags) == 136);

	const FMaterialGpuMatrix XRayMatrix = 
	{
		1.0f, 2.0f, 3.0f, 0.0f, 4.0f, 5.0f, 6.0f, 0.0f, 7.0f, 8.0f, 9.0f, 0.0f, 10.0f, 11.0f, 12.0f, 1.0f
	};

	const FMaterialGpuMatrix GpuMatrix = MakeMaterialDrawBufferMatrix(XRayMatrix);

	const FMaterialGpuMatrix ExpectedGpuMatrix = 
	{
		1.0f, 4.0f, 7.0f, 10.0f, 2.0f, 5.0f, 8.0f, 11.0f, 3.0f, 6.0f, 9.0f, 12.0f, 0.0f, 0.0f, 0.0f, 1.0f
	};
	MATERIAL_CHECK(Runner, GpuMatrix == ExpectedGpuMatrix);

	const xr_array<float, 4> Position = {2.0f, 3.0f, 4.0f, 1.0f};
	xr_array<float, 4> XRayResult = {};
	xr_array<float, 4> HlslResult = {};
	for (size_t Component = 0; Component < 4; ++Component)
	{
		for (size_t Axis = 0; Axis < 4; ++Axis)
		{
			XRayResult[Component] += Position[Axis] * XRayMatrix[Axis * 4 + Component];
			HlslResult[Component] += GpuMatrix[Component * 4 + Axis] * Position[Axis];
		}
	}
	MATERIAL_CHECK(Runner, XRayResult == HlslResult);
	MATERIAL_CHECK(Runner, HlslResult[3] == 1.0f);
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrMaterialParameterLayoutTests");
	TestDeterministicLayout(Runner);
	TestPackingAndDescriptorRelocation(Runner);
	TestValidation(Runner);
	TestGpuAbi(Runner);
	return Runner.Finish();
}
