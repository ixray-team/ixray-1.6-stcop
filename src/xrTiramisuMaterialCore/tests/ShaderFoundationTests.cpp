#include "LegacyMaterialResolver.h"
#include "MaterialAsset.h"
#include "MaterialPass.h"
#include "TiramisuMaterialShaderCompiler.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <array>
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

bool HasBackendMagic(const FMaterialShaderCompileResult& Result, const EMaterialShaderBackend Backend)
{
	constexpr xr_array<u8, 4> Dxil = {'D', 'X', 'B', 'C'};
	constexpr xr_array<u8, 4> SpirV = {0x03, 0x02, 0x23, 0x07};
	const auto& Expected = Backend == EMaterialShaderBackend::D3D12 ? Dxil : SpirV;
	return Result.Bytecode.size() >= Expected.size() &&
		   std::equal(Expected.begin(), Expected.end(), Result.Bytecode.begin());
}

FMaterialShaderCompileResult CompileFile(const TiramisuMaterialShaderCompiler& Compiler, const EMaterialShaderBackend Backend, const char* Path, const char* Profile)
{
	FMaterialShaderCompileRequest Request;
	Request.Backend = Backend;
	Request.Source = ReadText(Path);
	Request.SourceName = Path;
	Request.TargetProfile = Profile;
	Request.IncludeDirectories = {"gamedata/shaders/r5", "gamedata/shaders/r5/common", "gamedata/shaders/r5/deferred", "gamedata/shaders/r5/lighting", "gamedata/shaders/r5/postprocess", "gamedata/shaders/r5/global"};
	Request.Defines.emplace_back("NRI_ENABLE_DRAW_PARAMETERS_EMULATION=1");
	return Compiler.Compile(Request);
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrTiramisuShaderFoundationTests");
	const TiramisuMaterialShaderCompiler Compiler;
	MATERIAL_CHECK(Runner, Compiler.IsAvailable());

	const xr_array SystemShaders = {
		xr_pair{"gamedata/shaders/r5/global/fullscreen_triangle.vs.hlsl", "vs_6_6"},
		xr_pair{"gamedata/shaders/r5/global/deferred_directional_light.ps.hlsl", "ps_6_6"},
		xr_pair{"gamedata/shaders/r5/global/deferred_point_light.ps.hlsl", "ps_6_6"},
		xr_pair{"gamedata/shaders/r5/global/postprocess_tonemap.ps.hlsl", "ps_6_6"},
		xr_pair{"gamedata/shaders/r5/global/scene_vertex.ps.hlsl", "ps_6_6"},
		xr_pair{"gamedata/shaders/r5/global/scene_lmap.ps.hlsl", "ps_6_6"},
		xr_pair{"gamedata/shaders/r5/global/ui_screen_transform.vs.hlsl", "vs_6_6"},
		xr_pair{"gamedata/shaders/r5/global/ui_no_transform.vs.hlsl", "vs_6_6"},
		xr_pair{"gamedata/shaders/r5/global/ui.ps.hlsl", "ps_6_6"},
	};
	for (const EMaterialShaderBackend Backend : {EMaterialShaderBackend::D3D12, EMaterialShaderBackend::Vulkan})
	{
		for (const auto& [Path, Profile] : SystemShaders)
		{
			const FMaterialShaderCompileResult Result = CompileFile(Compiler, Backend, Path, Profile);
			MATERIAL_CHECK(Runner, Result.Succeeded());
			MATERIAL_CHECK(Runner, HasBackendMagic(Result, Backend));
		}
	}

	const xr_string GBufferSource = ReadText("gamedata/shaders/r5/common/GBuffer.hlsl");
	const xr_string LightingSource = ReadText("gamedata/shaders/r5/lighting/PbrLighting.hlsl");
	const xr_string DeferredSource = ReadText("gamedata/shaders/r5/deferred/DeferredLightingCommon.hlsl");
	const xr_string MaterialGpuAbiSource =
		ReadText("gamedata/shaders/r5/common/MaterialGpuAbi.hlsl");
	const xr_string MaterialLightingSource =
		ReadText("gamedata/shaders/r5/materials/passes/MaterialLightingCommon.hlsl");
	const xr_string MaterialForwardSource =
		ReadText("gamedata/shaders/r5/materials/passes/MaterialForwardPass.hlsl");
	const xr_string LevelStaticVertexSource = ReadText(
		"gamedata/shaders/r5/materials/vertex/"
		"MaterialLevelStaticVertexFactory.hlsl"
	);
	MATERIAL_CHECK(Runner, GBufferSource.find("EncodeOctahedralNormal") != xr_string::npos);
	MATERIAL_CHECK(Runner, GBufferSource.find("DecodeOctahedralNormal") != xr_string::npos);
	MATERIAL_CHECK(Runner, GBufferSource.find(
		"TIRAMISU_GBUFFER_VERSION 1u"
	) != xr_string::npos);
	MATERIAL_CHECK(Runner, GBufferSource.find(
		"NormalRoughnessMetallic"
	) != xr_string::npos);
	MATERIAL_CHECK(Runner, LightingSource.find("TiramisuDistributionGGX") != xr_string::npos);
	MATERIAL_CHECK(Runner, LightingSource.find("TiramisuGeometrySmith") != xr_string::npos);
	MATERIAL_CHECK(Runner, LightingSource.find("TiramisuFresnelSchlick") != xr_string::npos);
	MATERIAL_CHECK(Runner, DeferredSource.find("ResourceDescriptorHeap") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialGpuAbiSource.find(
		"TIRAMISU_MATERIAL_GPU_ABI_VERSION 5u"
	) != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialGpuAbiSource.find("TIRAMISU_MATERIAL_LIGHT_GPU_DATA_SIZE 64u") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialGpuAbiSource.find(
		"ResourceDescriptorHeap[SkinningPaletteBufferIndex]"
	) != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialGpuAbiSource.find(
		"TIRAMISU_MATERIAL_DRAW_FLAG_EDITOR_DEPTH_BIAS 4u"
	) != xr_string::npos);
	MATERIAL_CHECK(Runner, LevelStaticVertexSource.find(
		"Output.CurrentClipPosition.z -= CurrentBias"
	) != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialGpuAbiSource.find("ResourceDescriptorHeap[LightDataBufferIndex]") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialGpuAbiSource.find("LightDataOffset + LightIndex") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialLightingSource.find("min(LightCount, 64u)") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialLightingSource.find("LoadMaterialLightGpuData(Index)") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialForwardSource.find("EvaluateMaterialSceneDirectLighting") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaterialForwardSource.find(
		"ResourceDescriptorHeap[EnvironmentTextureIndex]"
	) != xr_string::npos);

	TiramisuMaterialLibrary Library;
	for (const char* Path : {
			 "gamedata/render_materials/legacy_opaque.material.json",
			 "gamedata/render_materials/legacy_masked.material.json",
			 "gamedata/render_materials/legacy_emissive.material.json"
		 })
	{
		FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(ReadText(Path), Path);
		MATERIAL_CHECK(Runner, Parsed.Succeeded());
		MATERIAL_CHECK(Runner, Library.RegisterMaster(std::move(Parsed.Value)).Succeeded());
	}
	for (const char* Path : {
			 "gamedata/render_materials/legacy_default.material-instance.json",
			 "gamedata/render_materials/legacy_vertex.material-instance.json",
			 "gamedata/render_materials/legacy_lmap.material-instance.json",
			 "gamedata/render_materials/legacy_default_aref.material-instance.json",
			 "gamedata/render_materials/legacy_vertex_aref.material-instance.json",
			 "gamedata/render_materials/legacy_lmap_aref.material-instance.json",
			 "gamedata/render_materials/legacy_selflight.material-instance.json"
		 })
	{
		FMaterialInstanceParseResult Parsed = ParseMaterialInstanceJson(ReadText(Path), Path);
		MATERIAL_CHECK(Runner, Parsed.Succeeded());
		MATERIAL_CHECK(Runner, Library.RegisterInstance(std::move(Parsed.Value)).Succeeded());
	}

	const xr_string Template = ReadText("gamedata/shaders/r5/materials/MaterialTemplate.hlsl");
	const FMaterialPassDefinition* GBufferPass = FindMaterialPassDefinition(EMaterialPass::GBuffer);
	MATERIAL_CHECK(Runner, GBufferPass != nullptr);
	const xr_string PassSource = ReadText(std::filesystem::path("gamedata/shaders/r5") / GBufferPass->ShaderSource.data());
	for (const char* InstanceReference : {
			 "ee5ffbc0-bd24-4aa8-9e16-50651ca1c269", "1f7e0305-f4a2-4447-a268-e67a74d0f3f6", "c30e92c2-d38c-4423-9f27-e42d245caf54", "68ae697d-7049-46ab-af7d-a1c4ddf2b32d", "0f8efc7b-8438-42c8-ac98-876cd4a5c289", "50400bdb-5aab-4fb9-b15f-dba129a042a4", "1b5644fb-ed55-443f-af03-eaee4b7f56cb"
		 })
	{
		const FMaterialResolveResult Resolved = Library.Resolve(InstanceReference);
		MATERIAL_CHECK(Runner, Resolved.Succeeded());
		const FMaterialAsset* Master = Library.GetMaster(Resolved.Value.MasterHandle);
		MATERIAL_CHECK(Runner, Master != nullptr);
		const xr_string Implementation = ReadText(std::filesystem::path("gamedata/shaders/r5") / Master->Implementation.Source.c_str());
		const FMaterialSourceAssemblyResult Source = AssembleMaterialShaderSourceForPass(*Master, Template, Implementation, Resolved.Value.StaticParameters, PassSource, GBufferPass->ShaderSource);
		MATERIAL_CHECK(Runner, Source.Succeeded());
		for (const EMaterialShaderBackend Backend : {EMaterialShaderBackend::D3D12, EMaterialShaderBackend::Vulkan})
		{
			FMaterialShaderCompileRequest Request;
			Request.Backend = Backend;
			Request.Source = Source.Source;
			Request.SourceName = xr_string(InstanceReference) + ".gbuffer.hlsl";
			Request.TargetProfile = "ps_6_6";
			Request.IncludeDirectories = {"gamedata/shaders/r5", "gamedata/shaders/r5/materials", "gamedata/shaders/r5/materials/passes", "gamedata/shaders/r5/common"};
			const FMaterialShaderCompileResult Result = Compiler.Compile(Request);
			MATERIAL_CHECK(Runner, Result.Succeeded());
			MATERIAL_CHECK(Runner, HasBackendMagic(Result, Backend));
		}
	}

	return Runner.Finish();
}
