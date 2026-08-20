#include "MaterialPreviewCompiler.h"

#include <algorithm>
#include <future>
#include <iomanip>
#include <mutex>
#include <ranges>
#include <sstream>
#include <unordered_map>
#include <utility>

namespace Tiramisu::Editor
{
namespace
{

bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	return std::ranges::any_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void Append(xr_vector<FMaterialDiagnostic>& Destination, const xr_vector<FMaterialDiagnostic>& Source)
{
	Destination.insert(Destination.end(), Source.begin(), Source.end());
}

void AddError(FMaterialPreviewCompileResult& Result, xr_string Code, xr_string Message)
{
	Result.Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, std::move(Code), std::move(Message), {}, {}});
}

void HashBytes(u64& Hash, const xr_string_view Bytes) noexcept
{
	constexpr u64 FnvPrime = 1099511628211ull;
	for (const unsigned char Byte : Bytes)
	{
		Hash ^= Byte;
		Hash *= FnvPrime;
	}
	Hash ^= 0xffu;
	Hash *= FnvPrime;
}

xr_string SourceHash(const FMaterialPreviewCompileRequest& Request, const xr_string_view Implementation, const xr_string_view PassSource)
{
	u64 Hash = 14695981039346656037ull;
	HashBytes(Hash, Request.TemplateSource);
	HashBytes(Hash, Request.VertexFactorySource);
	HashBytes(Hash, Implementation);
	HashBytes(Hash, PassSource);
	for (const xr_string& Dependency : Request.DependencySources)
	{
		HashBytes(Hash, Dependency);
	}
	std::ostringstream Stream;
	Stream << std::hex << std::setfill('0') << std::setw(16) << Hash;
	return Stream.str();
}

FMaterialShaderCompileRequest MakeShaderRequest(
	const FMaterialPreviewCompileRequest& Request,
	xr_string Source,
	const char* SourceName,
	const char* TargetProfile,
	const bool VertexStage
)
{
	FMaterialShaderCompileRequest Shader;
	Shader.Backend = Request.Backend;
	Shader.Source = std::move(Source);
	Shader.SourceName = SourceName;
	Shader.EntryPoint = "Main";
	Shader.TargetProfile = TargetProfile;
	Shader.IncludeDirectories = Request.IncludeDirectories;
	Shader.Debug = Request.Debug;
	if (VertexStage)
	{
		Shader.Defines.emplace_back("MATERIAL_VERTEX_SHADER=1");
	}
	if (Request.Backend == EMaterialShaderBackend::D3D12)
	{
		Shader.Defines.emplace_back("NRI_ENABLE_DRAW_PARAMETERS_EMULATION=1");
	}
	return Shader;
}

struct FMaterialPreviewShaderCacheValue
{
	xr_vector<u8> VertexBytecode;
	xr_vector<u8> PixelBytecode;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept
	{
		return !VertexBytecode.empty() && !PixelBytecode.empty() &&
			   !HasErrors(Diagnostics);
	}
};

struct FMaterialPreviewShaderCache
{
	std::mutex Mutex;
	xr_hash_map<u64, std::shared_future<FMaterialPreviewShaderCacheValue>> Entries;
	FMaterialPreviewShaderCacheStatistics Statistics;
};

FMaterialPreviewShaderCache& ShaderCache()
{
	static FMaterialPreviewShaderCache Cache;
	return Cache;
}
} // namespace

bool FMaterialPreviewCompileResult::Succeeded() const noexcept
{
	return !VertexBytecode.empty() && !PixelBytecode.empty() &&
		   !VertexFactory.empty() && PipelineKey != 0 &&
		   !HasErrors(Diagnostics);
}

FMaterialPreviewCompileResult CompileMaterialPreview(
	const FMaterialPreviewCompileRequest& Request
)
{
	FMaterialPreviewCompileResult Result;

	FMaterialAssetParseResult ParsedMaster = ParseMaterialAssetJson(
		Request.MaterialJson, "material-preview/master.material.json"
	);
	Append(Result.Diagnostics, ParsedMaster.Diagnostics);
	if (!ParsedMaster.Succeeded())
	{
		return Result;
	}

	FMaterialAsset Master = std::move(ParsedMaster.Value);
	Result.MaterialId = Master.Id;
	TiramisuMaterialLibrary Library;
	FMaterialRegistrationResult MasterRegistration =
		Library.RegisterMaster(Master);
	Append(Result.Diagnostics, MasterRegistration.Diagnostics);
	if (!MasterRegistration.Succeeded())
	{
		return Result;
	}

	xr_string ResolveReference = Master.Id.Value;
	if (!Request.MaterialInstanceJson.empty())
	{
		FMaterialInstanceParseResult ParsedInstance = ParseMaterialInstanceJson(
			Request.MaterialInstanceJson,
			"material-preview/instance.material-instance.json"
		);
		Append(Result.Diagnostics, ParsedInstance.Diagnostics);
		if (!ParsedInstance.Succeeded())
		{
			return Result;
		}
		ResolveReference = ParsedInstance.Value.Id.Value;
		Result.MaterialId = ParsedInstance.Value.Id;
		FMaterialRegistrationResult InstanceRegistration =
			Library.RegisterInstance(std::move(ParsedInstance.Value));
		Append(Result.Diagnostics, InstanceRegistration.Diagnostics);
		if (!InstanceRegistration.Succeeded())
		{
			return Result;
		}
	}

	FMaterialResolveResult Resolved = Library.Resolve(ResolveReference);
	Append(Result.Diagnostics, Resolved.Diagnostics);
	if (!Resolved.Succeeded())
	{
		return Result;
	}
	Result.ResolvedMaterial = std::move(Resolved.Value);

	xr_string Implementation = Request.GeneratedHlsl;
	if (Implementation.empty() &&
		Master.Implementation.Type == EMaterialImplementationType::Graph)
	{
		FMaterialGraphCompileOptions Options;
		Options.Parameters = Master.Parameters;
		Options.StaticParameters = Result.ResolvedMaterial.StaticParameters;
		FMaterialGraphCompileResult Graph = CompileMaterialGraph(
			Master.Implementation.Graph, Options
		);
		Append(Result.Diagnostics, Graph.Diagnostics);
		if (Graph.Succeeded())
		{
			Implementation = std::move(Graph.GeneratedHlsl);
		}
	}
	if (Implementation.empty())
	{
		AddError(Result, "preview.empty_implementation", "Material preview requires generated or loaded EvaluateMaterial HLSL.");
		return Result;
	}
	const xr_string_view PassSource = Request.PassSource.empty()
										  ? xr_string_view(Request.PreviewPassSource)
										  : xr_string_view(Request.PassSource);
	if (Request.TemplateSource.empty() || Request.VertexFactorySource.empty() ||
		PassSource.empty())
	{
		AddError(Result, "preview.missing_engine_source", "Material template, vertex factory and validation pass sources are required.");
		return Result;
	}

	const FMaterialParameterLayoutResult Layout =
		BuildMaterialParameterLayout(Master.Parameters);
	Append(Result.Diagnostics, Layout.Diagnostics);
	if (!Layout.Succeeded())
	{
		return Result;
	}
	FMaterialParameterPackResult Packed = PackMaterialParameters(
		Layout.Value, Master.Parameters, Result.ResolvedMaterial.Parameters
	);
	Append(Result.Diagnostics, Packed.Diagnostics);
	if (!Packed.Succeeded())
	{
		return Result;
	}
	Result.ParameterBlock = std::move(Packed.Value);

	const FMaterialPassDefinition* PassDefinition =
		FindMaterialPassDefinition(Request.Pass);
	if (!PassDefinition)
	{
		AddError(Result, "preview.pass_missing", "The requested material pass is absent from the pass manifest.");
		return Result;
	}
	const xr_string_view VertexFactoryName = Request.VertexFactory.empty()
		? PassDefinition->VertexFactory
		: xr_string_view(Request.VertexFactory);
	const FMaterialVertexFactoryDefinition* VertexFactoryDefinition =
		FindMaterialVertexFactoryDefinition(VertexFactoryName);
	if (!VertexFactoryDefinition)
	{
		AddError(
			Result,
			"preview.vertex_factory_missing",
			"The requested material vertex factory is absent from the manifest."
		);
		return Result;
	}
	Result.VertexFactory = VertexFactoryName;
	FMaterialSourceAssemblyResult VertexSource =
		AssembleMaterialShaderSourceForPass(
			Master,
			Request.TemplateSource,
			Implementation,
			Result.ResolvedMaterial.StaticParameters,
			Request.VertexFactorySource,
			VertexFactoryDefinition->ShaderSource
		);
	Append(Result.Diagnostics, VertexSource.Diagnostics);
	FMaterialSourceAssemblyResult PixelSource =
		AssembleMaterialShaderSourceForPass(Master, Request.TemplateSource, Implementation, Result.ResolvedMaterial.StaticParameters, PassSource, PassDefinition->ShaderSource);
	Append(Result.Diagnostics, PixelSource.Diagnostics);
	if (!VertexSource.Succeeded() || !PixelSource.Succeeded())
	{
		return Result;
	}

	FMaterialPipelineKey Pipeline = MakeCookedMaterialPipelineKey(
		Result.ResolvedMaterial, *PassDefinition, Request.Backend == EMaterialShaderBackend::D3D12 ? "d3d12" : "vulkan"
	);
	Pipeline.VertexFactory = Result.VertexFactory;
	if (!Request.RenderPassSignature.empty())
	{
		Pipeline.RenderPassSignature = Request.RenderPassSignature;
	}
	FMaterialPipelineKey SortPipeline = Pipeline;
	SortPipeline.Backend = "backend-neutral";
	SortPipeline.CompilerOptions += ';' + Request.CompilerOptions +
		(Request.Debug ? ";debug=1" : ";debug=0");
	Result.PipelineSortKey = SortPipeline.StableHash();
	Pipeline.CompilerOptions += ';' + Request.CompilerOptions +
								";source_hash=" + SourceHash(Request, Implementation, PassSource) +
								(Request.Debug ? ";debug=1" : ";debug=0");
	Result.PipelineKey = Pipeline.StableHash();
	if (Result.PipelineKey == 0 || Result.PipelineSortKey == 0)
	{
		AddError(Result, "preview.invalid_pipeline_key", "The material preview pipeline key resolved to zero.");
		return Result;
	}

	FMaterialPreviewShaderCache& Cache = ShaderCache();
	std::shared_future<FMaterialPreviewShaderCacheValue> SharedCompilation;
	std::promise<FMaterialPreviewShaderCacheValue> OwnedPromise;
	bool CompileThisRequest = false;
	{
		std::scoped_lock Lock(Cache.Mutex);
		++Cache.Statistics.RequestCount;
		const auto Existing = Cache.Entries.find(Result.PipelineKey);
		if (Existing != Cache.Entries.end())
		{
			++Cache.Statistics.HitCount;
			SharedCompilation = Existing->second;
		}
		else
		{
			++Cache.Statistics.CompileCount;
			CompileThisRequest = true;
			SharedCompilation = OwnedPromise.get_future().share();
			Cache.Entries.emplace(Result.PipelineKey, SharedCompilation);
			Cache.Statistics.EntryCount = Cache.Entries.size();
		}
	}

	if (CompileThisRequest)
	{
		FMaterialPreviewShaderCacheValue Value;
		TiramisuMaterialShaderCompiler Compiler;
		if (!Compiler.IsAvailable())
		{
			Value.Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, "preview.dxc_unavailable", "DXC is unavailable for material preview compilation.", {}, {}});
		}
		else
		{
			FMaterialShaderCompileResult Vertex = Compiler.Compile(
				MakeShaderRequest(Request, std::move(VertexSource.Source), "material-preview.vertex.hlsl", "vs_6_6", true)
			);
			Append(Value.Diagnostics, Vertex.Diagnostics);
			FMaterialShaderCompileResult Pixel = Compiler.Compile(
				MakeShaderRequest(Request, std::move(PixelSource.Source), "material-preview.pixel.hlsl", "ps_6_6", false)
			);
			Append(Value.Diagnostics, Pixel.Diagnostics);
			if (Vertex.Succeeded() && Pixel.Succeeded())
			{
				Value.VertexBytecode = std::move(Vertex.Bytecode);
				Value.PixelBytecode = std::move(Pixel.Bytecode);
			}
		}
		const bool KeepCached = Value.Succeeded();
		OwnedPromise.set_value(std::move(Value));
		if (!KeepCached)
		{
			std::scoped_lock Lock(Cache.Mutex);
			Cache.Entries.erase(Result.PipelineKey);
			Cache.Statistics.EntryCount = Cache.Entries.size();
		}
	}

	const FMaterialPreviewShaderCacheValue Cached = SharedCompilation.get();
	Append(Result.Diagnostics, Cached.Diagnostics);
	Result.VertexBytecode = Cached.VertexBytecode;
	Result.PixelBytecode = Cached.PixelBytecode;
	return Result;
}

FMaterialPreviewShaderCacheStatistics
GetMaterialPreviewShaderCacheStatistics()
{
	FMaterialPreviewShaderCache& Cache = ShaderCache();
	std::scoped_lock Lock(Cache.Mutex);
	FMaterialPreviewShaderCacheStatistics Result = Cache.Statistics;
	Result.EntryCount = Cache.Entries.size();
	return Result;
}

void ResetMaterialPreviewShaderCacheForTests()
{
	FMaterialPreviewShaderCache& Cache = ShaderCache();
	std::scoped_lock Lock(Cache.Mutex);
	Cache.Entries.clear();
	Cache.Statistics = {};
}
} // namespace Tiramisu::Editor
