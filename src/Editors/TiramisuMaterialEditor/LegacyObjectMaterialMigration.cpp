#include "LegacyObjectMaterialMigration.h"

#include "EditorViewportMaterialResolver.h"
#include "MaterialEditorFileIO.h"

#include <LegacyMaterialResolver.h>
#include <MaterialTypes.h>
#include <nlohmann/json.hpp>

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <ranges>
#include <sstream>
#include <unordered_map>

namespace Tiramisu::Editor
{
namespace
{
using Json = nlohmann::json;

void AddError(xr_vector<FMaterialDiagnostic>& Diagnostics, xr_string Code, xr_string Message)
{
	Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, std::move(Code), std::move(Message), {}, {}});
}

void Append(xr_vector<FMaterialDiagnostic>& Destination, const xr_vector<FMaterialDiagnostic>& Source)
{
	Destination.insert(Destination.end(), Source.begin(), Source.end());
}

[[nodiscard]] bool HasErrors(
	const xr_vector<FMaterialDiagnostic>& Diagnostics
)
{
	return std::ranges::any_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
							   { return Diagnostic.Severity ==
										EMaterialDiagnosticSeverity::Error; });
}

[[nodiscard]] xr_string NormalizePathLike(xr_string_view Value)
{
	xr_string Result;
	Result.reserve(Value.size());
	bool PreviousSeparator = false;
	for (const char Character : Value)
	{
		const bool Separator = Character == '/' || Character == '\\';
		if (Separator)
		{
			if (!PreviousSeparator)
			{
				Result.push_back('\\');
			}
		}
		else
		{
			Result.push_back(static_cast<char>(std::tolower(
				static_cast<unsigned char>(Character)
			)));
		}
		PreviousSeparator = Separator;
	}
	while (Result.starts_with(".\\"))
	{
		Result.erase(0, 2);
	}
	return Result;
}

void AppendKeyField(std::ostringstream& Stream, const xr_string_view Value)
{
	Stream << Value.size() << ':' << Value << '|';
}

[[nodiscard]] xr_string ReadText(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	return ToXrString(Text);
}

[[nodiscard]] xr_string MakeGeneratedAssetReference(
	const xr_string_view Guid
)
{
	return "generated/legacy_objects/" + xr_string(Guid) +
		   ".material-instance.json";
}

[[nodiscard]] bool EqualInstance(const FMaterialInstanceAsset& Left, const FMaterialInstanceAsset& Right)
{
	return Left.Id == Right.Id && Left.Parent == Right.Parent &&
		   Left.Overrides == Right.Overrides &&
		   Left.StaticOverrides == Right.StaticOverrides;
}
} // namespace

bool FLegacyObjectMaterialMigrationResult::Succeeded() const noexcept
{
	return !HasErrors(Diagnostics);
}

xr_string BuildLegacyObjectMaterialKey(
	const FLegacyObjectSurfaceDescriptor& Surface
)
{
	std::ostringstream Stream;
	Stream << "$legacy-object-material-v"
		   << LegacyObjectMaterialMigrationVersion << "$|";
	AppendKeyField(Stream, NormalizeLegacyShaderName(Surface.ShaderName));
	AppendKeyField(Stream, NormalizePathLike(Surface.CompilerShaderName));
	AppendKeyField(Stream, NormalizePathLike(Surface.TextureName));
	Stream << (Surface.TwoSided ? "two-sided" : "one-sided") << '|';
	return Stream.str();
}

bool TiramisuLegacyObjectMaterialMigrationService::Initialize(
	const std::filesystem::path& MaterialRoot,
	xr_vector<FMaterialDiagnostic>* OutDiagnostics
)
{
	xr_vector<FMaterialDiagnostic> Diagnostics;
	Initialized = false;
	PendingDatabaseChanges = false;
	Entries.clear();
	Root = MaterialRoot.lexically_normal();
	GeneratedRoot = Root / "generated" / "legacy_objects";
	DatabasePath = GeneratedRoot / "legacy-object-migration.json";

	if (!Resolver.Load(Root, &Diagnostics))
	{
		if (OutDiagnostics)
		{
			*OutDiagnostics = std::move(Diagnostics);
		}
		return false;
	}
	Initialized = LoadDatabase(Diagnostics);
	if (OutDiagnostics)
	{
		*OutDiagnostics = std::move(Diagnostics);
	}
	return Initialized;
}

bool TiramisuLegacyObjectMaterialMigrationService::LoadDatabase(
	xr_vector<FMaterialDiagnostic>& Diagnostics
)
{
	Entries.clear();
	std::error_code Error;
	if (!std::filesystem::exists(DatabasePath, Error))
	{
		return !Error;
	}
	const xr_string Text = ReadText(DatabasePath);
	const Json RootJson = Json::parse(Text, nullptr, false);
	if (RootJson.is_discarded() || !RootJson.is_object())
	{
		AddError(Diagnostics, "object_migration.invalid_database", "Legacy object material migration database contains invalid JSON.");
		return false;
	}
	const auto Version = RootJson.find("asset_version");
	const auto Items = RootJson.find("entries");
	if (Version == RootJson.end() || !Version->is_number_unsigned() ||
		Version->get<u32>() !=
			LegacyObjectMaterialMigrationVersion ||
		Items == RootJson.end() || !Items->is_array())
	{
		AddError(Diagnostics, "object_migration.unsupported_database", "Legacy object material migration database has an unsupported schema.");
		return false;
	}
	xr_hash_map<xr_string, bool> Keys;
	for (const Json& Item : *Items)
	{
		FDatabaseEntry Entry;
		const auto ReadStringField = [&Item](const char* Name, xr_string& Destination)
		{
			const auto Found = Item.find(Name);
			if (Found == Item.end() || !Found->is_string())
			{
				return false;
			}
			Destination = Found->get<xr_string>();
			return true;
		};
		bool Valid = Item.is_object() &&
					 ReadStringField("key", Entry.Key) &&
					 ReadStringField("instance", Entry.Instance) &&
					 ReadStringField("asset", Entry.AssetPath) &&
					 ReadStringField("parent", Entry.Parent) &&
					 ReadStringField("shader", Entry.ShaderName) &&
					 ReadStringField("compiler_shader", Entry.CompilerShaderName) &&
					 ReadStringField("texture", Entry.TextureName);
		const auto TwoSided = Item.find("two_sided");
		Valid = Valid && TwoSided != Item.end() && TwoSided->is_boolean();
		if (Valid)
		{
			Entry.TwoSided = TwoSided->get<bool>();
		}
		const auto Sources = Item.find("sources");
		Valid = Valid && Sources != Item.end() && Sources->is_array();
		if (Valid)
		{
			for (const Json& Source : *Sources)
			{
				if (!Source.is_string())
				{
					Valid = false;
					break;
				}
				Entry.Sources.push_back(Source.get<xr_string>());
			}
		}
		if (!Valid || Entry.Key.empty() || Entry.Instance.empty() ||
			Entry.AssetPath.empty() || !Keys.emplace(Entry.Key, true).second)
		{
			AddError(Diagnostics, "object_migration.invalid_database_entry", "Legacy object material migration database contains an invalid or duplicate entry.");
			return false;
		}
		Entries.push_back(std::move(Entry));
	}
	return true;
}

bool TiramisuLegacyObjectMaterialMigrationService::SaveDatabase(
	xr_vector<FMaterialDiagnostic>& Diagnostics
) const
{
	Json RootJson = {
		{"asset_version", LegacyObjectMaterialMigrationVersion},
		{"entries", Json::array()}
	};
	xr_vector<const FDatabaseEntry*> Sorted;
	Sorted.reserve(Entries.size());
	for (const FDatabaseEntry& Entry : Entries)
	{
		Sorted.push_back(&Entry);
	}
	std::ranges::sort(Sorted, {}, [](const FDatabaseEntry* Entry)
					  { return Entry->Key; });
	for (const FDatabaseEntry* Entry : Sorted)
	{
		RootJson["entries"].push_back({{"key", Entry->Key}, {"instance", Entry->Instance}, {"asset", Entry->AssetPath}, {"parent", Entry->Parent}, {"shader", Entry->ShaderName}, {"compiler_shader", Entry->CompilerShaderName}, {"texture", Entry->TextureName}, {"two_sided", Entry->TwoSided}, {"sources", Entry->Sources}});
	}
	const FAtomicTextFileWriteResult Write =
		WriteTextFileAtomically(DatabasePath, RootJson.dump(2));
	if (!Write.Success)
	{
		AddError(Diagnostics, "object_migration.database_write_failed", Write.Error);
		return false;
	}
	return true;
}

FLegacyObjectMaterialMigrationResult
TiramisuLegacyObjectMaterialMigrationService::Migrate(
	const xr_string_view ObjectSource,
	const xr_vector<FLegacyObjectSurfaceDescriptor>& Surfaces,
	const bool DeferDatabaseSave
)
{
	FLegacyObjectMaterialMigrationResult Result;
	if (!Initialized || !Resolver.IsLoaded())
	{
		AddError(Result.Diagnostics, "object_migration.not_initialized", "Legacy object material migration service is not initialized.");
		return Result;
	}

	struct FPendingInstance
	{
		FMaterialInstanceAsset Asset;
		std::filesystem::path Path;
	};
	xr_vector<FPendingInstance> PendingInstances;
	// A standalone .object migration keeps copy-then-publish semantics.
	// A deferred level batch already is an unpublished transaction, so
	// copying its growing source table for every component is unnecessary.
	xr_vector<FDatabaseEntry> CandidateEntriesCopy;
	if (!DeferDatabaseSave)
	{
		CandidateEntriesCopy = Entries;
	}
	xr_vector<FDatabaseEntry>& CandidateEntries =
		DeferDatabaseSave ? Entries : CandidateEntriesCopy;
	bool DatabaseChanged = false;

	for (size_t Index = 0; Index < Surfaces.size(); ++Index)
	{
		const FLegacyObjectSurfaceDescriptor& Surface = Surfaces[Index];
		const xr_string Key = BuildLegacyObjectMaterialKey(Surface);
		auto Existing = std::ranges::find(CandidateEntries, Key, &FDatabaseEntry::Key);
		if (Existing != CandidateEntries.end())
		{
			if (!ObjectSource.empty() &&
				std::ranges::find(Existing->Sources, xr_string(ObjectSource)) == Existing->Sources.end())
			{
				Existing->Sources.push_back(xr_string(ObjectSource));
				std::ranges::sort(Existing->Sources);
				DatabaseChanged = true;
			}
			Result.Bindings.push_back({Surface.SurfaceName, Key, Existing->Instance, Existing->AssetPath, Surface.TwoSided, false});
			++Result.ReusedInstanceCount;
			continue;
		}

		FEditorViewportLegacyMaterialSource Source;
		Source.MaterialSlot = static_cast<u64>(Index + 1);
		Source.ShaderName = Surface.ShaderName;
		if (!Surface.TextureName.empty())
		{
			Source.Textures.push_back(Surface.TextureName);
		}
		Source.SurfaceName = Surface.SurfaceName;
		Source.TwoSided = Surface.TwoSided;
		FEditorViewportMaterialResolution Resolution =
			Resolver.Resolve(Source);
		Append(Result.Diagnostics, Resolution.Diagnostics);
		if (!Resolution.Succeeded())
		{
			continue;
		}

		const xr_string Guid = GenerateDeterministicMaterialGuid(
			"legacy-object-material-instance", Key
		);
		FMaterialInstanceAsset Instance;
		Instance.Id.Value = Guid;
		Instance.Name = "Migrated " +
						(Surface.SurfaceName.empty() ? xr_string("surface")
													 : Surface.SurfaceName);
		// Static switches stay in this pre-authored legacy parent. The
		// generated child only captures per-object runtime values.
		Instance.Parent = Resolution.Legacy.Material;
		Instance.Overrides =
			MakeLegacyMaterialRuntimeOverrides(Resolution.Legacy);

		const xr_string RelativePath =
			MakeGeneratedAssetReference(Guid);
		const std::filesystem::path AssetPath = Root / RelativePath.c_str();
		bool NeedsWrite = true;
		std::error_code Error;
		if (std::filesystem::exists(AssetPath, Error) && !Error)
		{
			const FMaterialInstanceParseResult Parsed =
				ParseMaterialInstanceJson(ReadText(AssetPath), RelativePath);
			if (!Parsed.Succeeded() ||
				!EqualInstance(Parsed.Value, Instance))
			{
				AddError(Result.Diagnostics, "object_migration.instance_conflict", "Generated MaterialInstance path already contains different data: '" + ToXrString(AssetPath.string()) + "'.");
				continue;
			}
			NeedsWrite = false;
		}
		if (NeedsWrite)
		{
			PendingInstances.push_back({Instance, AssetPath});
		}

		FDatabaseEntry Entry;
		Entry.Key = Key;
		Entry.Instance = Guid;
		Entry.AssetPath = RelativePath;
		Entry.Parent = Instance.Parent;
		Entry.ShaderName = NormalizeLegacyShaderName(Surface.ShaderName);
		Entry.CompilerShaderName =
			NormalizePathLike(Surface.CompilerShaderName);
		Entry.TextureName = NormalizePathLike(Surface.TextureName);
		Entry.TwoSided = Surface.TwoSided;
		if (!ObjectSource.empty())
		{
			Entry.Sources.push_back(xr_string(ObjectSource));
		}
		CandidateEntries.push_back(std::move(Entry));
		DatabaseChanged = true;
		Result.Bindings.push_back(
			{Surface.SurfaceName, Key, Guid, RelativePath, Surface.TwoSided, true}
		);
		++Result.CreatedInstanceCount;
	}

	if (Result.Bindings.size() != Surfaces.size() ||
		HasErrors(Result.Diagnostics))
	{
		return Result;
	}

	std::error_code DirectoryError;
	std::filesystem::create_directories(GeneratedRoot, DirectoryError);
	if (DirectoryError)
	{
		AddError(Result.Diagnostics, "object_migration.directory_failed", "Cannot create generated material directory: " + ToXrString(DirectoryError.message()));
		return Result;
	}
	for (const FPendingInstance& Pending : PendingInstances)
	{
		const FAtomicTextFileWriteResult Write =
			WriteTextFileAtomically(Pending.Path, SerializeMaterialInstanceJson(Pending.Asset));
		if (!Write.Success)
		{
			AddError(Result.Diagnostics, "object_migration.instance_write_failed", Write.Error);
			return Result;
		}
	}

	if (DatabaseChanged)
	{
		if (DeferDatabaseSave)
		{
			PendingDatabaseChanges = true;
		}
		else
		{
			const xr_vector<FDatabaseEntry> Previous = Entries;
			Entries = std::move(CandidateEntries);
			if (!SaveDatabase(Result.Diagnostics))
			{
				Entries = Previous;
				return Result;
			}
		}
	}
	Result.DatabaseChanged = DatabaseChanged;
	return Result;
}

bool TiramisuLegacyObjectMaterialMigrationService::FlushDatabase(
	xr_vector<FMaterialDiagnostic>& Diagnostics
)
{
	if (!Initialized)
	{
		AddError(Diagnostics, "object_migration.not_initialized", "Legacy object material migration service is not initialized.");
		return false;
	}
	if (!PendingDatabaseChanges)
	{
		return true;
	}
	if (!SaveDatabase(Diagnostics))
	{
		return false;
	}
	PendingDatabaseChanges = false;
	return true;
}
} // namespace Tiramisu::Editor
