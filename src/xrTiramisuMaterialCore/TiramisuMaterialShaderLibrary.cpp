#include "TiramisuMaterialShaderLibrary.h"

namespace
{
bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	return std::ranges::any_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void AddDiagnostic(xr_vector<FMaterialDiagnostic>& Diagnostics, const EMaterialDiagnosticSeverity Severity, const xr_string_view Code, xr_string Message)
{
	Diagnostics.push_back({Severity, xr_string(Code), std::move(Message), {}, {}});
}

const char* StageName(const EMaterialShaderStage Stage)
{
	return Stage == EMaterialShaderStage::Vertex ? "vertex" : "pixel";
}

void AppendDiagnostics(xr_vector<FMaterialDiagnostic>& Destination, xr_vector<FMaterialDiagnostic> Source)
{
	Destination.insert(Destination.end(), std::make_move_iterator(Source.begin()), std::make_move_iterator(Source.end()));
}
} // namespace

bool FMaterialShaderLibraryBuildResult::Succeeded() const noexcept
{
	return Value.has_value() && !HasErrors(Diagnostics);
}

FMaterialShaderLibraryBuildResult TiramisuMaterialShaderLibrary::Build(FMaterialBundle InBundle, const FMaterialShaderLibraryBuildOptions& Options)
{
	FMaterialShaderLibraryBuildResult Result;
	TiramisuMaterialShaderLibrary Library;
	Library.Format = Options.Format;
	Library.Bundle = std::move(InBundle);

	if (Library.Bundle.Version != MaterialBundleVersion)
	{
		AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.unsupported_version", "Material shader library requires bundle version " + std::to_string(MaterialBundleVersion) + ".");
	}
	if (Options.RequireCompleteShaderSet && !Library.Bundle.CompleteShaderSet)
	{
		AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.incomplete_bundle", "Cooked material runtime requires a bundle marked CompleteShaderSet.");
	}

	for (const FMaterialBundleRecord& Record : Library.Bundle.Records)
	{
		if (!Record.Id.IsValid())
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.invalid_record_id", "Material bundle record has no stable asset id.");
			continue;
		}

		FMaterialAssetId MasterId = Record.Id;
		if (Record.Type == EMaterialBundleRecordType::FlattenedInstance)
		{
			MasterId = Record.MasterId;
			if (!MasterId.IsValid())
			{
				AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.invalid_instance_master", "Flattened material instance '" + Record.Id.Value + "' has no master id.");
				continue;
			}
		}

		const auto [Alias, Inserted] = Library.MasterAliases.emplace(Record.Id, MasterId);
		if (!Inserted && Alias->second != MasterId)
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.conflicting_alias", "Material asset '" + Record.Id.Value + "' resolves to more than one master material.");
		}
	}

	for (const auto& [AssetId, MasterId] : Library.MasterAliases)
	{
		const auto Master = Library.MasterAliases.find(MasterId);
		if (Master == Library.MasterAliases.end())
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.missing_master", "Material asset '" + AssetId.Value + "' references missing master '" + MasterId.Value + "'.");
		}
		else if (Master->second != MasterId)
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.non_flattened_instance", "Material instance '" + AssetId.Value + "' does not contain a flattened master reference.");
		}
	}

	TiramisuMaterialLibrary MaterialAssets;
	for (const FMaterialBundleRecord& Record : Library.Bundle.Records)
	{
		if (Record.Type != EMaterialBundleRecordType::Master)
		{
			continue;
		}
		FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
			Record.AssetPayload, Record.SourcePath
		);
		const bool ParsedSuccessfully = Parsed.Succeeded();
		AppendDiagnostics(Result.Diagnostics, std::move(Parsed.Diagnostics));
		if (!ParsedSuccessfully)
		{
			continue;
		}
		if (Parsed.Value.Id != Record.Id || Record.MasterId != Record.Id)
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.master_record_mismatch", "Master bundle record '" + Record.Id.Value + "' does not match its serialized asset identity.");
			continue;
		}
		Library.MasterMaterials.emplace(Parsed.Value.Id, Parsed.Value);
		FMaterialRegistrationResult Registered =
			MaterialAssets.RegisterMaster(std::move(Parsed.Value));
		AppendDiagnostics(Result.Diagnostics, std::move(Registered.Diagnostics));
	}

	for (const FMaterialBundleRecord& Record : Library.Bundle.Records)
	{
		if (Record.Type != EMaterialBundleRecordType::FlattenedInstance)
		{
			continue;
		}
		FMaterialInstanceParseResult Parsed = ParseMaterialInstanceJson(
			Record.AssetPayload, Record.SourcePath
		);
		const bool ParsedSuccessfully = Parsed.Succeeded();
		AppendDiagnostics(Result.Diagnostics, std::move(Parsed.Diagnostics));
		if (!ParsedSuccessfully)
		{
			continue;
		}
		if (Parsed.Value.Id != Record.Id || Parsed.Value.Parent != Record.MasterId.Value)
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.instance_record_mismatch", "Flattened instance record '" + Record.Id.Value + "' does not match its serialized asset identity or master.");
			continue;
		}
		FMaterialRegistrationResult Registered =
			MaterialAssets.RegisterInstance(std::move(Parsed.Value));
		AppendDiagnostics(Result.Diagnostics, std::move(Registered.Diagnostics));
	}

	for (const FMaterialBundleRecord& Record : Library.Bundle.Records)
	{
		FMaterialResolveResult Resolved = MaterialAssets.Resolve(Record.Id.Value);
		const bool ResolvedSuccessfully = Resolved.Succeeded();
		AppendDiagnostics(Result.Diagnostics, std::move(Resolved.Diagnostics));
		if (!ResolvedSuccessfully)
		{
			continue;
		}
		if (Resolved.Value.MasterId != Record.MasterId)
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.resolved_master_mismatch", "Material '" + Record.Id.Value + "' resolves to master '" + Resolved.Value.MasterId.Value + "' instead of bundle master '" + Record.MasterId.Value + "'.");
			continue;
		}
		Library.ResolvedMaterials.emplace(Record.Id, std::move(Resolved.Value));
	}

	for (size_t BlobIndex = 0; BlobIndex < Library.Bundle.ShaderBlobs.size(); ++BlobIndex)
	{
		const FMaterialShaderBlob& Blob = Library.Bundle.ShaderBlobs[BlobIndex];
		if (Blob.Format != Options.Format)
		{
			continue;
		}

		if (!Blob.MaterialId.IsValid() || Blob.PipelineKey == 0 || Blob.EntryPoint.empty() ||
			Blob.Bytecode.empty() || Blob.VertexFactory.empty() || Blob.RenderPassSignature.empty())
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.invalid_blob", "Selected backend contains a shader blob with incomplete metadata or bytecode.");
			continue;
		}
		if (!FindMaterialPassDefinition(Blob.Pass))
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.invalid_pass", "Selected backend contains a shader blob with an unknown material pass.");
			continue;
		}
		if (Blob.Stage != EMaterialShaderStage::Vertex && Blob.Stage != EMaterialShaderStage::Pixel)
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.invalid_stage", "Selected backend contains a shader blob with an unknown shader stage.");
			continue;
		}
		if (!Library.MasterAliases.contains(Blob.MaterialId))
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.missing_material_record", "Shader blob references material '" + Blob.MaterialId.Value + "' without a bundle record.");
			continue;
		}

		FMaterialShaderProgramKey Key;
		Key.MaterialId = Library.ResolveMasterMaterialId(Blob.MaterialId);
		Key.PipelineKey = Blob.PipelineKey;
		Key.Pass = Blob.Pass;
		Key.VertexFactory = Blob.VertexFactory;
		Key.RenderPassSignature = Blob.RenderPassSignature;

		FShaderStageIndices& Stages = Library.Programs[std::move(Key)];
		size_t& StageIndex = Blob.Stage == EMaterialShaderStage::Vertex ? Stages.Vertex : Stages.Pixel;
		if (StageIndex != InvalidBlobIndex)
		{
			AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.duplicate_stage", "Material shader program contains more than one " + xr_string(StageName(Blob.Stage)) + " stage.");
			continue;
		}
		StageIndex = BlobIndex;
	}

	if (Library.Programs.empty())
	{
		AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader_library.missing_backend", "Material bundle contains no shader programs for the selected backend.");
	}

	bool AllProgramsComplete = !Library.Programs.empty();
	for (const auto& [Key, Stages] : Library.Programs)
	{
		if (Stages.IsComplete())
		{
			continue;
		}

		AllProgramsComplete = false;
		const EMaterialDiagnosticSeverity Severity = Library.Bundle.CompleteShaderSet ? EMaterialDiagnosticSeverity::Error : EMaterialDiagnosticSeverity::Warning;
		AddDiagnostic(Result.Diagnostics, Severity, "shader_library.missing_stage", "Material '" + Key.MaterialId.Value + "' program " + xr_string(std::to_string(Key.PipelineKey)) + " does not contain both vertex and pixel stages.");
	}

	Library.Complete = Library.Bundle.CompleteShaderSet && AllProgramsComplete;
	if (!HasErrors(Result.Diagnostics))
	{
		Result.Value.emplace(std::move(Library));
	}
	return Result;
}

FMaterialShaderLibraryBuildResult TiramisuMaterialShaderLibrary::Deserialize(const xr_span<const u8> Data, const FMaterialShaderLibraryBuildOptions& Options)
{
	FMaterialShaderLibraryBuildResult Result;
	FMaterialBundleReadResult Read = DeserializeMaterialBundle(Data);
	Result.Diagnostics = std::move(Read.Diagnostics);
	if (!Read.Succeeded())
	{
		return Result;
	}

	FMaterialShaderLibraryBuildResult Built = Build(std::move(Read.Value), Options);
	Result.Diagnostics.insert(Result.Diagnostics.end(), std::make_move_iterator(Built.Diagnostics.begin()), std::make_move_iterator(Built.Diagnostics.end()));
	Result.Value = std::move(Built.Value);
	return Result;
}

FMaterialAssetId TiramisuMaterialShaderLibrary::ResolveMasterMaterialId(const FMaterialAssetId& MaterialId) const
{
	const auto Alias = MasterAliases.find(MaterialId);
	return Alias != MasterAliases.end() ? Alias->second : MaterialId;
}

const FMaterialAsset* TiramisuMaterialShaderLibrary::ResolveMasterMaterial(const FMaterialAssetId& MaterialId) const noexcept
{
	const auto Alias = MasterAliases.find(MaterialId);
	const FMaterialAssetId& MasterId = Alias != MasterAliases.end() ? Alias->second : MaterialId;
	const auto Material = MasterMaterials.find(MasterId);

	return Material != MasterMaterials.end() ? &Material->second : nullptr;
}

const FResolvedMaterialInstance* TiramisuMaterialShaderLibrary::ResolveMaterial(const FMaterialAssetId& MaterialId) const noexcept
{
	const auto Material = ResolvedMaterials.find(MaterialId);
	return Material != ResolvedMaterials.end() ? &Material->second : nullptr;
}

xr_optional<FMaterialShaderProgramView> TiramisuMaterialShaderLibrary::Find(const FMaterialAssetId& MaterialId, const EMaterialPass Pass, const xr_string_view VertexFactory, const xr_string_view RenderPassSignature) const
{
	const FResolvedMaterialInstance* Material = ResolveMaterial(MaterialId);
	const FMaterialPassDefinition* Definition = FindMaterialPassDefinition(Pass);
	if (!Material || !Definition || Definition->VertexFactory != VertexFactory || Definition->RenderPassSignature != RenderPassSignature)
	{
		return std::nullopt;
	}

	const xr_string_view Backend = Format == EMaterialShaderBlobFormat::Dxil
									   ? "d3d12"
									   : "vulkan";

	const FMaterialPipelineKey Key = MakeCookedMaterialPipelineKey(*Material, *Definition, Backend);
	return Find(MaterialId, Key.StableHash(), Pass, VertexFactory, RenderPassSignature);
}

xr_optional<FMaterialShaderProgramView> TiramisuMaterialShaderLibrary::Find(const FMaterialAssetId& MaterialId, const u64 PipelineKey, const EMaterialPass Pass, const xr_string_view VertexFactory, const xr_string_view RenderPassSignature) const
{
	FMaterialShaderProgramKey Key;
	Key.MaterialId = ResolveMasterMaterialId(MaterialId);
	Key.PipelineKey = PipelineKey;
	Key.Pass = Pass;
	Key.VertexFactory = VertexFactory;
	Key.RenderPassSignature = RenderPassSignature;

	const auto Program = Programs.find(Key);
	if (Program == Programs.end())
	{
		return std::nullopt;
	}

	FMaterialShaderProgramView View;
	if (Program->second.Vertex != InvalidBlobIndex)
	{
		View.Vertex = &Bundle.ShaderBlobs[Program->second.Vertex];
	}
	if (Program->second.Pixel != InvalidBlobIndex)
	{
		View.Pixel = &Bundle.ShaderBlobs[Program->second.Pixel];
	}
	return View;
}
