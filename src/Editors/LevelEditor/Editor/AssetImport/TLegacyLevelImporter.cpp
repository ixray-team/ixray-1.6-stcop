#include "stdafx.h"

#include "TLegacyLevelImporter.h"
#include "TLegacyObjectImporter.h"

#include "../../../TiramisuMaterialEditor/LegacyObjectMaterialMigration.h"
#include "../../../TiramisuMaterialEditor/MaterialEditorFileIO.h"
#include "../Entry/StaticObject/SceneObject.h"
#include "../Scene/scene.h"

#include <MaterialTypes.h>
#include <SceneAsset.h>
#include <SceneConversionDump.h>

#include <algorithm>
#include <array>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <ranges>
#include <sstream>
#include <unordered_map>

namespace
{
using namespace Tiramisu;

constexpr u32 LegacyLevelImporterVersion = 2;
constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;

void HashBytes(u64& Hash, const void* Data, const size_t Size)
{
	const auto* Bytes = static_cast<const u8*>(Data);
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Hash ^= Bytes[Index];
		Hash *= FnvPrime;
	}
}

[[nodiscard]] xr_string HashFile(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	u64 Hash = FnvOffset;
	xr_array<char, 64 * 1024> Buffer;
	while (Input)
	{
		Input.read(Buffer.data(), Buffer.size());
		const std::streamsize Count = Input.gcount();
		if (Count > 0)
		{
			HashBytes(Hash, Buffer.data(), static_cast<size_t>(Count));
		}
	}
	std::ostringstream Text;
	Text << std::hex << std::setfill('0') << std::setw(16) << Hash;
	return Text.str();
}

[[nodiscard]] xr_string NormalizeIdentity(
	const std::filesystem::path& Path
)
{
	xr_string Result = Path.lexically_normal().generic_string();
	std::ranges::transform(Result, Result.begin(), [](const char Character)
						   { return static_cast<char>(std::tolower(
								 static_cast<unsigned char>(Character)
							 )); });
	return Result;
}

[[nodiscard]] xr_string ReadText(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	return xr_string(Text);
}

void AddDiagnostic(Scene::FSceneConversionDump& Dump, xr_string Severity, xr_string Code, xr_string Message)
{
	Dump.Diagnostics.push_back(
		{std::move(Severity), std::move(Code), std::move(Message)}
	);
}

void AppendMaterialDiagnostics(Scene::FSceneConversionDump& Dump, const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
	{
		const char* Severity = "info";
		switch (Diagnostic.Severity)
		{
			case EMaterialDiagnosticSeverity::Warning:
				Severity = "warning";
				break;
			case EMaterialDiagnosticSeverity::Error:
				Severity = "error";
				break;
			default:
				break;
		}
		AddDiagnostic(Dump, Severity, Diagnostic.Code, Diagnostic.Message);
	}
}

void AppendSceneDiagnostics(Scene::FSceneConversionDump& Dump, const xr_vector<Scene::FSceneDiagnostic>& Diagnostics)
{
	for (const Scene::FSceneDiagnostic& Diagnostic : Diagnostics)
	{
		AddDiagnostic(Dump, Diagnostic.Severity == Scene::ESceneDiagnosticSeverity::Error ? "error" : "warning", Diagnostic.Code, Diagnostic.Message);
	}
}

[[nodiscard]] std::filesystem::path DumpPathFor(
	const std::filesystem::path& Target, const bool Succeeded
)
{
	std::filesystem::path Result = Target;
	Result += Succeeded
				  ? ".migration.json"
				  : ".migration.failed.json";
	return Result;
}

[[nodiscard]] bool WriteDump(const std::filesystem::path& Path, const Scene::FSceneConversionDump& Dump)
{
	std::error_code Error;
	std::filesystem::create_directories(Path.parent_path(), Error);
	if (Error)
	{
		return false;
	}
	return Editor::WriteTextFileAtomically(Path, Scene::SerializeSceneConversionDumpJson(Dump)).Success;
}

[[nodiscard]] std::filesystem::path ResolveLegacyObjectPath(
	CEditableObject& Object
)
{
	std::filesystem::path Reference(Object.GetName());
	if (!Reference.has_extension())
	{
		Reference.replace_extension(".object");
	}
	std::error_code Error;
	if (Reference.is_absolute() &&
		std::filesystem::is_regular_file(Reference, Error))
	{
		return Reference.lexically_normal();
	}
	string_path Resolved = {};
	FS.update_path(Resolved, "$objects$", Reference.string().c_str());
	return std::filesystem::path(Resolved).lexically_normal();
}

[[nodiscard]] xr_vector<Editor::FLegacyObjectSurfaceDescriptor>
BuildSurfaceDescriptors(const SurfaceVec& Surfaces)
{
	xr_vector<Editor::FLegacyObjectSurfaceDescriptor> Result;
	Result.reserve(Surfaces.size());
	for (const CSurface* Surface : Surfaces)
	{
		Editor::FLegacyObjectSurfaceDescriptor Descriptor;
		if (Surface)
		{
			Descriptor.SurfaceName = Surface->_Name();
			Descriptor.ShaderName = Surface->_ShaderName();
			Descriptor.CompilerShaderName = Surface->_ShaderXRLCName();
			Descriptor.GameMaterialName = Surface->_GameMtlName();
			Descriptor.TextureName = Surface->_Texture();
			Descriptor.VertexMapName = Surface->_VMap();
			Descriptor.Flags = Surface->m_Flags.get();
			Descriptor.VertexFormat = Surface->_FVF();
			Descriptor.TwoSided =
				Surface->m_Flags.is(CSurface::sf2Sided);
		}
		Result.push_back(std::move(Descriptor));
	}
	return Result;
}

[[nodiscard]] xr_string MakeSceneRelativeReference(
	const std::filesystem::path& Asset,
	const std::filesystem::path& ScenePath
)
{
	std::error_code Error;
	const std::filesystem::path Relative = std::filesystem::relative(
		Asset, ScenePath.parent_path(), Error
	);
	return (!Error && !Relative.empty() ? Relative : Asset)
		.lexically_normal()
		.generic_string();
}

struct FImportedStaticMesh
{
	std::filesystem::path Source;
	std::filesystem::path Target;
	std::filesystem::path Dump;
	xr_string Reference;
	Scene::FStaticMeshAsset Asset;
};
} // namespace

std::filesystem::path MakeImportedRenderScenePath(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& RenderSceneRoot
)
{
	std::filesystem::path Relative = SourcePath.filename();
	const std::filesystem::path Normalized =
		SourcePath.lexically_normal();
	xr_vector<std::filesystem::path> Parts;
	for (const std::filesystem::path& Part : Normalized)
	{
		Parts.push_back(Part);
	}
	for (size_t Index = 0; Index < Parts.size(); ++Index)
	{
		xr_string Name = Parts[Index].generic_string();
		std::ranges::transform(Name, Name.begin(), [](const char Character)
							   { return static_cast<char>(std::tolower(
									 static_cast<unsigned char>(Character)
								 )); });
		if ((Name == "levels" || Name == "maps") &&
			Index + 1 < Parts.size())
		{
			Relative.clear();
			for (size_t Tail = Index + 1; Tail < Parts.size(); ++Tail)
			{
				Relative /= Parts[Tail];
			}
			break;
		}
	}
	Relative.replace_extension(".render-scene.json");
	return RenderSceneRoot / "imported" / Relative;
}

FLegacyLevelImportResult WriteLegacyLevelLoadFailureDump(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& RenderSceneRoot,
	xr_string DiagnosticCode,
	xr_string DiagnosticMessage
)
{
	using namespace Tiramisu;
	FLegacyLevelImportResult Result;
	Result.SourcePath = SourcePath.lexically_normal();
	Result.TargetPath = MakeImportedRenderScenePath(
		Result.SourcePath, RenderSceneRoot
	);
	Result.DumpPath = DumpPathFor(Result.TargetPath, false);

	Scene::FSceneConversionDump Dump;
	Dump.Status = Scene::ESceneConversionStatus::Failed;
	Dump.Importer = "tiramisu_legacy_level";
	Dump.ImporterVersion = LegacyLevelImporterVersion;
	Dump.SourceType = "level";
	Dump.SourcePath = Result.SourcePath.generic_string();
	Dump.SourceHash = HashFile(Result.SourcePath);
	Dump.TargetPath = Result.TargetPath.generic_string();
	AddDiagnostic(Dump, "error", std::move(DiagnosticCode), std::move(DiagnosticMessage));
	if (!WriteDump(Result.DumpPath, Dump))
	{
		AddDiagnostic(Dump, "error", "level_import.dump_write_failed", "Mandatory failed level conversion dump could not be "
																	   "published.");
	}
	Result.Diagnostics = std::move(Dump.Diagnostics);
	return Result;
}

FLegacyLevelImportResult ImportLoadedLegacyLevelAsset(
	const std::filesystem::path& SourcePath,
	EScene& LegacyScene,
	const std::filesystem::path& MaterialRoot,
	const std::filesystem::path& StaticMeshRoot,
	const std::filesystem::path& RenderSceneRoot
)
{
	using namespace Tiramisu;
	FLegacyLevelImportResult Result;
	Result.SourcePath = SourcePath.lexically_normal();
	Result.TargetPath = MakeImportedRenderScenePath(
		Result.SourcePath, RenderSceneRoot
	);

	Scene::FSceneConversionDump Dump;
	Dump.Importer = "tiramisu_legacy_level";
	Dump.ImporterVersion = LegacyLevelImporterVersion;
	Dump.SourceType = "level";
	Dump.SourcePath = Result.SourcePath.generic_string();
	Dump.SourceHash = HashFile(Result.SourcePath);
	Dump.TargetPath = Result.TargetPath.generic_string();

	auto Finish = [&](const bool Succeeded)
	{
		Dump.Status = Succeeded
						  ? Scene::ESceneConversionStatus::Succeeded
						  : Scene::ESceneConversionStatus::Failed;
		Result.DumpPath = DumpPathFor(Result.TargetPath, Succeeded);
		if (!WriteDump(Result.DumpPath, Dump))
		{
			AddDiagnostic(Dump, "error", "level_import.dump_write_failed", "Mandatory level conversion dump could not be published.");
			if (Succeeded)
			{
				std::error_code RemoveError;
				std::filesystem::remove(Result.TargetPath, RemoveError);
			}
			Result.Succeeded = false;
		}
		else
		{
			Result.Succeeded = Succeeded;
		}
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	};

	if (Dump.SourceHash.empty())
	{
		AddDiagnostic(Dump, "error", "level_import.source_read_failed", "Cannot read legacy .level source.");
		return Finish(false);
	}

	const ObjectList& Objects =
		LegacyScene.ListObj(OBJCLASS_SCENEOBJECT);
	xr_hash_map<xr_string, FImportedStaticMesh> ImportedMeshes;
	for (CCustomObject* CustomObject : Objects)
	{
		auto* SceneObject = static_cast<CSceneObject*>(CustomObject);
		if (!SceneObject || !SceneObject->GetReference())
		{
			AddDiagnostic(Dump, "error", "level_import.missing_object_reference", "Legacy scene contains a static object without a library "
																				  "reference.");
			continue;
		}
		CEditableObject& Reference = *SceneObject->GetReference();
		const xr_string Key = NormalizeIdentity(
			std::filesystem::path(Reference.GetName())
		);
		if (ImportedMeshes.contains(Key))
		{
			continue;
		}

		const std::filesystem::path ObjectSource =
			ResolveLegacyObjectPath(Reference);
		const FLegacyObjectImportResult Imported =
			ImportLegacyObjectAsset(
				ObjectSource, MaterialRoot, StaticMeshRoot
			);
		Dump.AssetMappings.push_back({Imported.SourcePath.generic_string(), Imported.TargetPath.generic_string(), Imported.DumpPath.generic_string(), Imported.TargetAssetId, Imported.TargetPayloadPath.generic_string()});
		if (!Imported.Succeeded)
		{
			for (const Scene::FSceneConversionDiagnostic& Diagnostic :
				 Imported.Diagnostics)
			{
				Dump.Diagnostics.push_back(Diagnostic);
			}
			AddDiagnostic(Dump, "error", "level_import.object_conversion_failed", "Referenced object '" + ObjectSource.generic_string() + "' could not be converted. Object dump: " + Imported.DumpPath.generic_string());
			continue;
		}

		const Scene::FStaticMeshAssetParseResult Parsed =
			Scene::LoadStaticMeshAsset(Imported.TargetPath);
		AppendSceneDiagnostics(Dump, Parsed.Diagnostics);
		if (!Parsed.Succeeded())
		{
			continue;
		}

		const Scene::FSceneConversionDumpParseResult ObjectDump =
			Scene::ParseSceneConversionDumpJson(
				ReadText(Imported.DumpPath)
			);
		if (ObjectDump.Succeeded())
		{
			Dump.CreatedMaterialInstances +=
				ObjectDump.Value.CreatedMaterialInstances;
			Dump.ReusedMaterialInstances +=
				ObjectDump.Value.ReusedMaterialInstances;
			Dump.MaterialMappings.insert(
				Dump.MaterialMappings.end(),
				ObjectDump.Value.MaterialMappings.begin(),
				ObjectDump.Value.MaterialMappings.end()
			);
		}
		else
		{
			AddDiagnostic(Dump, "error", "level_import.object_dump_invalid", "Referenced object conversion dump could not be parsed: " + Imported.DumpPath.generic_string());
			continue;
		}

		FImportedStaticMesh Mesh;
		Mesh.Source = ObjectSource;
		Mesh.Target = Imported.TargetPath;
		Mesh.Dump = Imported.DumpPath;
		Mesh.Reference = MakeSceneRelativeReference(
			Mesh.Target, Result.TargetPath
		);
		Mesh.Asset = Parsed.Value;
		ImportedMeshes.emplace(Key, std::move(Mesh));
	}

	if (std::ranges::any_of(Dump.Diagnostics, [](const Scene::FSceneConversionDiagnostic& Diagnostic)
							{ return Diagnostic.Severity == "error"; }))
	{
		return Finish(false);
	}

	Editor::TiramisuLegacyObjectMaterialMigrationService MaterialMigration;
	xr_vector<FMaterialDiagnostic> MaterialDiagnostics;
	if (!MaterialMigration.Initialize(
			MaterialRoot, &MaterialDiagnostics
		))
	{
		AppendMaterialDiagnostics(Dump, MaterialDiagnostics);
		AddDiagnostic(Dump, "error", "level_import.material_migration_unavailable", "Material migration database could not be initialized.");
		return Finish(false);
	}

	Scene::FRenderSceneAsset Asset;
	Asset.Id = GenerateDeterministicMaterialGuid(
		"legacy-level-render-scene",
		NormalizeIdentity(Result.SourcePath)
	);
	Asset.Name = Result.SourcePath.stem().string();
	Asset.SourcePath = Result.TargetPath.generic_string();
	Dump.TargetAssetId = Asset.Id;
	Result.TargetAssetId = Asset.Id;

	u32 ComponentIndex = 0;
	for (CCustomObject* CustomObject : Objects)
	{
		auto* SceneObject = static_cast<CSceneObject*>(CustomObject);
		if (!SceneObject || !SceneObject->GetReference())
		{
			continue;
		}
		CEditableObject& Reference = *SceneObject->GetReference();
		const xr_string Key = NormalizeIdentity(
			std::filesystem::path(Reference.GetName())
		);
		const auto Imported = ImportedMeshes.find(Key);
		if (Imported == ImportedMeshes.end())
		{
			continue;
		}

		Scene::FStaticMeshComponent Component;
		const xr_string ComponentIdentity =
			NormalizeIdentity(Result.SourcePath) + "|" +
			xr_string(SceneObject->GetName()) + "|" +
			xr_string(std::to_string(ComponentIndex++)) + "|" + Key;
		Component.Id = GenerateDeterministicMaterialGuid(
			"legacy-level-static-mesh-component",
			ComponentIdentity
		);
		Component.Name = SceneObject->GetName();
		Component.StaticMesh = Imported->second.Reference;
		Fmatrix Transform;
		SceneObject->GetFullTransformToWorld(Transform);
		std::copy_n(Transform.mm, Component.LocalToWorld.size(), Component.LocalToWorld.begin());
		Component.Visible = SceneObject->Visible();

		const SurfaceVec& Surfaces = SceneObject->m_Surfaces.empty()
										 ? Reference.Surfaces()
										 : SceneObject->m_Surfaces;
		const Editor::FLegacyObjectMaterialMigrationResult Overrides =
			MaterialMigration.Migrate(
				Dump.SourcePath + "#" + Component.Name,
				BuildSurfaceDescriptors(Surfaces),
				true
			);
		AppendMaterialDiagnostics(Dump, Overrides.Diagnostics);
		Dump.CreatedMaterialInstances +=
			Overrides.CreatedInstanceCount;
		Dump.ReusedMaterialInstances +=
			Overrides.ReusedInstanceCount;
		if (!Overrides.Succeeded())
		{
			return Finish(false);
		}

		for (const Editor::FLegacyObjectMaterialBinding& Binding :
			 Overrides.Bindings)
		{
			const auto Slot = std::ranges::find(
				Imported->second.Asset.MaterialSlots,
				Binding.SurfaceName,
				&Scene::FStaticMeshMaterialSlot::Name
			);
			if (Slot == Imported->second.Asset.MaterialSlots.end())
			{
				AddDiagnostic(Dump, "error", "level_import.surface_slot_missing", "Scene object '" + Component.Name + "' overrides unknown surface '" + Binding.SurfaceName + "'.");
				continue;
			}
			const u32 SlotIndex =
				static_cast<u32>(std::distance(
					Imported->second.Asset.MaterialSlots.begin(), Slot
				));
			if (Slot->Material != Binding.MaterialInstance ||
				Slot->TwoSided != Binding.TwoSided)
			{
				Component.MaterialOverrides.push_back({SlotIndex, Binding.MaterialInstance, Binding.TwoSided});
			}
			Dump.MaterialMappings.push_back({Component.Name + "/" + Binding.SurfaceName, Binding.SourceKey, Binding.MaterialInstance, Binding.TwoSided, Binding.Created});
		}
		Asset.StaticMeshComponents.push_back(std::move(Component));
	}

	xr_vector<FMaterialDiagnostic> FlushDiagnostics;
	if (!MaterialMigration.FlushDatabase(FlushDiagnostics))
	{
		AppendMaterialDiagnostics(Dump, FlushDiagnostics);
		AddDiagnostic(Dump, "error", "level_import.material_migration_publish_failed", "Batched legacy material migration database could not be "
																					   "published.");
		return Finish(false);
	}
	AppendMaterialDiagnostics(Dump, FlushDiagnostics);

	Dump.MeshCount =
		static_cast<u32>(ImportedMeshes.size());
	Dump.ComponentCount =
		static_cast<u32>(Asset.StaticMeshComponents.size());
	if (Asset.StaticMeshComponents.empty())
	{
		AddDiagnostic(Dump, "error", "level_import.empty_scene", "Legacy level contains no convertible static-mesh components.");
		return Finish(false);
	}

	const xr_string AssetJson =
		Scene::SerializeRenderSceneAssetJson(Asset);
	const Scene::FRenderSceneAssetParseResult Validation =
		Scene::ParseRenderSceneAssetJson(
			AssetJson, Result.TargetPath.generic_string()
		);
	AppendSceneDiagnostics(Dump, Validation.Diagnostics);
	if (!Validation.Succeeded() ||
		std::ranges::any_of(Dump.Diagnostics, [](const Scene::FSceneConversionDiagnostic& Diagnostic)
							{ return Diagnostic.Severity == "error"; }))
	{
		return Finish(false);
	}

	std::error_code DirectoryError;
	std::filesystem::create_directories(
		Result.TargetPath.parent_path(), DirectoryError
	);
	if (DirectoryError)
	{
		AddDiagnostic(Dump, "error", "level_import.target_directory_failed", "Cannot create native render-scene directory: " + DirectoryError.message());
		return Finish(false);
	}
	const Editor::FAtomicTextFileWriteResult Write =
		Editor::WriteTextFileAtomically(Result.TargetPath, AssetJson);
	if (!Write.Success)
	{
		AddDiagnostic(Dump, "error", "level_import.target_write_failed", Write.Error);
		return Finish(false);
	}
	return Finish(true);
}
