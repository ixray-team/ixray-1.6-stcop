#include "TiramisuEditorParticleLibrary.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <cstring>
#include <fstream>
#include <iterator>

namespace
{
constexpr u32 CompressedChunkFlag = 0x80000000u;
constexpr u32 ParticleLibraryVersionChunk = 1;
constexpr u16 ParticleLibraryOriginalVersion = 1;
constexpr u16 ParticleLibraryExtendedVersion = 2;
constexpr u32 OriginalParticleEffectsChunk = 3;
constexpr u32 OriginalParticleGroupsChunk = 4;
constexpr u32 ExtendedParticleEffectsChunk = 2;
constexpr u32 ExtendedParticleGroupsChunk = 3;
constexpr u32 ExtendedParticleCurvesChunk = 4;
constexpr u32 AssetNameChunk = 2;
constexpr u32 EffectDataChunk = 3;
constexpr u32 EffectActionListChunk = 4;
constexpr u32 EffectFlagsChunk = 5;
constexpr u32 EffectFrameChunk = 6;
constexpr u32 EffectSpriteChunk = 7;
constexpr u32 EffectTimeLimitChunk = 8;
constexpr u32 EffectTimeLimit2Chunk = 9;
constexpr u32 EffectVelocityScaleChunk = 0x22;
constexpr u32 EffectAlignToPathChunk = 0x25;
constexpr u32 GroupFlagsChunk = 3;
constexpr u32 GroupEffectsChunk = 4;
constexpr u32 GroupTimeLimitChunk = 5;
constexpr u32 GroupEffectsExtendedChunk = 7;
constexpr u32 GroupEntryOnPlayChildFlag = 1u << 1;
constexpr u32 GroupEntryEnabledFlag = 1u << 2;
constexpr u32 GroupEntryOnBirthChildFlag = 1u << 5;
constexpr u32 GroupEntryOnDeathChildFlag = 1u << 6;
constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;

struct FParticleChunkView
{
	const u8* Data = nullptr;
	size_t Size = 0;
};

[[nodiscard]] u32 ReadU32(const u8* Data) noexcept
{
	u32 Value = 0;
	std::memcpy(&Value, Data, sizeof(Value));
	return Value;
}

[[nodiscard]] u16 ReadU16(const u8* Data) noexcept
{
	u16 Value = 0;
	std::memcpy(&Value, Data, sizeof(Value));
	return Value;
}

[[nodiscard]] s32 ReadS32(const u8* Data) noexcept
{
	s32 Value = 0;
	std::memcpy(&Value, Data, sizeof(Value));
	return Value;
}

[[nodiscard]] float ReadFloat(const u8* Data) noexcept
{
	float Value = 0.0f;
	std::memcpy(&Value, Data, sizeof(Value));
	return Value;
}

template <typename FCallback>
bool ForEachChunk(const FParticleChunkView Parent, FCallback&& Callback)
{
	size_t Offset = 0;
	while (Offset < Parent.Size)
	{
		if (Parent.Size - Offset < sizeof(u32) * 2)
		{
			return false;
		}
		const u32 RawId = ReadU32(Parent.Data + Offset);
		const u32 PayloadSize = ReadU32(Parent.Data + Offset + sizeof(u32));
		Offset += sizeof(u32) * 2;
		if ((RawId & CompressedChunkFlag) != 0 ||
			PayloadSize > Parent.Size - Offset)
		{
			return false;
		}
		Callback(
			RawId,
			FParticleChunkView{Parent.Data + Offset, PayloadSize}
		);
		Offset += PayloadSize;
	}
	return true;
}

[[nodiscard]] bool ReadCString(
	const FParticleChunkView View,
	size_t& Offset,
	xr_string& OutValue
)
{
	if (Offset >= View.Size)
	{
		return false;
	}
	const u8* Begin = View.Data + Offset;
	const void* Terminator = std::memchr(Begin, 0, View.Size - Offset);
	if (!Terminator)
	{
		return false;
	}
	const auto* End = static_cast<const u8*>(Terminator);
	OutValue.assign(
		reinterpret_cast<const char*>(Begin),
		static_cast<size_t>(End - Begin)
	);
	Offset += static_cast<size_t>(End - Begin) + 1;
	return true;
}

void AppendDependency(
	xr_vector<xr_string>& Dependencies,
	const xr_string_view Dependency
)
{
	if (!Dependency.empty())
	{
		Dependencies.emplace_back(Dependency);
	}
}

void ParseEffectRecord(
	const FParticleChunkView Record,
	FEditorParticleAssetInfo& Asset,
	FTiramisuEditorParticleEffectDefinition& Definition
)
{
	Asset.Type = EEditorParticleAssetType::Effect;
	ForEachChunk(Record, [&](const u32 Id, const FParticleChunkView Chunk)
	{
		size_t Offset = 0;
		if (Id == AssetNameChunk)
		{
			(void)ReadCString(Chunk, Offset, Asset.Name);
			Definition.Name = Asset.Name;
		}
		else if (Id == EffectDataChunk && Chunk.Size >= sizeof(u32))
		{
			Definition.MaxParticles = ReadU32(Chunk.Data);
		}
		else if (Id == EffectActionListChunk)
		{
			Definition.CompiledActions.assign(
				Chunk.Data,
				Chunk.Data + Chunk.Size
			);
		}
		else if (Id == EffectFlagsChunk && Chunk.Size >= sizeof(u32))
		{
			Definition.Flags = ReadU32(Chunk.Data);
		}
		else if (Id == EffectFrameChunk && Chunk.Size >= 28)
		{
			Definition.FrameTexSize = {
				ReadFloat(Chunk.Data),
				ReadFloat(Chunk.Data + sizeof(float))
			};
			Definition.FrameDimensionX = ReadS32(Chunk.Data + 16);
			Definition.FrameCount = ReadS32(Chunk.Data + 20);
			Definition.FrameSpeed = ReadFloat(Chunk.Data + 24);
		}
		else if (Id == EffectSpriteChunk)
		{
			(void)ReadCString(Chunk, Offset, Asset.ShaderName);
			(void)ReadCString(Chunk, Offset, Asset.TextureName);
			Definition.ShaderName = Asset.ShaderName;
			Definition.TextureName = Asset.TextureName;
		}
		else if ((Id == EffectTimeLimitChunk || Id == EffectTimeLimit2Chunk) &&
				 Chunk.Size >= sizeof(float))
		{
			Definition.TimeLimit = ReadFloat(Chunk.Data);
		}
		else if (Id == EffectVelocityScaleChunk &&
				 Chunk.Size >= sizeof(float) * 3)
		{
			Definition.VelocityScale = {
				ReadFloat(Chunk.Data),
				ReadFloat(Chunk.Data + sizeof(float)),
				ReadFloat(Chunk.Data + sizeof(float) * 2)
			};
		}
		else if (Id == EffectAlignToPathChunk &&
				 Chunk.Size >= sizeof(float) * 3)
		{
			Definition.AlignToPathDefaultRotation = {
				ReadFloat(Chunk.Data),
				ReadFloat(Chunk.Data + sizeof(float)),
				ReadFloat(Chunk.Data + sizeof(float) * 2)
			};
		}
	});
	Asset.MaxParticles = Definition.MaxParticles;
	Asset.HasCompiledActions = Definition.IsSimulatable();
}

void ParseGroupEntries(
	const FParticleChunkView Chunk,
	FEditorParticleAssetInfo& Asset,
	FTiramisuEditorParticleGroupDefinition& Definition
)
{
	if (Chunk.Size < sizeof(u32))
	{
		return;
	}
	size_t Offset = sizeof(u32);
	const u32 Count = ReadU32(Chunk.Data);
	Definition.Entries.reserve(Count);
	for (u32 Index = 0; Index < Count; ++Index)
	{
		FTiramisuEditorParticleGroupEntry Entry;
		const bool Valid =
			ReadCString(Chunk, Offset, Entry.EffectName) &&
			ReadCString(Chunk, Offset, Entry.OnPlayChildName) &&
			ReadCString(Chunk, Offset, Entry.OnBirthChildName) &&
			ReadCString(Chunk, Offset, Entry.OnDeathChildName);
		if (!Valid || Chunk.Size - Offset < sizeof(float) * 2 + sizeof(u32))
		{
			return;
		}
		Entry.StartTime = ReadFloat(Chunk.Data + Offset);
		Offset += sizeof(float);
		Entry.StopTime = ReadFloat(Chunk.Data + Offset);
		Offset += sizeof(float);
		Entry.Flags = ReadU32(Chunk.Data + Offset);
		Offset += sizeof(u32);
		AppendDependency(Asset.Dependencies, Entry.EffectName);
		AppendDependency(Asset.Dependencies, Entry.OnPlayChildName);
		AppendDependency(Asset.Dependencies, Entry.OnBirthChildName);
		AppendDependency(Asset.Dependencies, Entry.OnDeathChildName);
		Definition.Entries.push_back(std::move(Entry));
	}
}

void ParseGroupRecord(
	const FParticleChunkView Record,
	FEditorParticleAssetInfo& Asset,
	FTiramisuEditorParticleGroupDefinition& Definition
)
{
	Asset.Type = EEditorParticleAssetType::Group;
	ForEachChunk(Record, [&](const u32 Id, const FParticleChunkView Chunk)
	{
		if (Id == AssetNameChunk)
		{
			size_t Offset = 0;
			(void)ReadCString(Chunk, Offset, Asset.Name);
			Definition.Name = Asset.Name;
		}
		else if (Id == GroupFlagsChunk && Chunk.Size >= sizeof(u32))
		{
			Definition.Flags = ReadU32(Chunk.Data);
		}
		else if (Id == GroupTimeLimitChunk &&
				 Chunk.Size >= sizeof(float))
		{
			Definition.TimeLimit = ReadFloat(Chunk.Data);
		}
		else if (Id == GroupEffectsChunk || Id == GroupEffectsExtendedChunk)
		{
			ParseGroupEntries(Chunk, Asset, Definition);
		}
	});
	Asset.GroupEntryCount = static_cast<u32>(Definition.Entries.size());
	Asset.EnabledGroupEntryCount = static_cast<u32>(std::ranges::count_if(
		Definition.Entries,
		[](const FTiramisuEditorParticleGroupEntry& Entry)
		{
			return (Entry.Flags & GroupEntryEnabledFlag) != 0;
		}
	));
	for (const FTiramisuEditorParticleGroupEntry& Entry : Definition.Entries)
	{
		if ((Entry.Flags & GroupEntryEnabledFlag) == 0)
		{
			continue;
		}
		Asset.GroupChildCallbackCount +=
			(Entry.Flags & GroupEntryOnPlayChildFlag) != 0 &&
			!Entry.OnPlayChildName.empty();
		Asset.GroupChildCallbackCount +=
			(Entry.Flags & GroupEntryOnBirthChildFlag) != 0 &&
			!Entry.OnBirthChildName.empty();
		Asset.GroupChildCallbackCount +=
			(Entry.Flags & GroupEntryOnDeathChildFlag) != 0 &&
			!Entry.OnDeathChildName.empty();
	}
}

void ParseCurveRecord(
	const FParticleChunkView Record,
	FEditorParticleAssetInfo& Asset
)
{
	Asset.Type = EEditorParticleAssetType::AnimationCurve;
	ForEachChunk(Record, [&](const u32 Id, const FParticleChunkView Chunk)
	{
		if (Id == AssetNameChunk)
		{
			size_t Offset = 0;
			(void)ReadCString(Chunk, Offset, Asset.Name);
		}
	});
}

void AddOrReplaceAsset(
	xr_vector<FEditorParticleAssetInfo>& Assets,
	FEditorParticleAssetInfo Asset
)
{
	if (Asset.Name.empty())
	{
		return;
	}
	const auto Existing = std::find_if(
		Assets.begin(), Assets.end(),
		[&](const FEditorParticleAssetInfo& Candidate)
		{
			return Candidate.Type == Asset.Type &&
				   Candidate.Name == Asset.Name;
		}
	);
	if (Existing == Assets.end())
	{
		Assets.push_back(std::move(Asset));
	}
	else
	{
		*Existing = std::move(Asset);
	}
}

void AddOrReplaceEffectDefinition(
	xr_vector<FTiramisuEditorParticleEffectDefinition>& Definitions,
	FTiramisuEditorParticleEffectDefinition Definition
)
{
	if (Definition.Name.empty())
	{
		return;
	}
	const auto Existing = std::ranges::find(
		Definitions,
		Definition.Name,
		&FTiramisuEditorParticleEffectDefinition::Name
	);
	if (Existing == Definitions.end())
	{
		Definitions.push_back(std::move(Definition));
	}
	else
	{
		*Existing = std::move(Definition);
	}
}

void AddOrReplaceGroupDefinition(
	xr_vector<FTiramisuEditorParticleGroupDefinition>& Definitions,
	FTiramisuEditorParticleGroupDefinition Definition
)
{
	if (Definition.Name.empty())
	{
		return;
	}
	const auto Existing = std::ranges::find(
		Definitions,
		Definition.Name,
		&FTiramisuEditorParticleGroupDefinition::Name
	);
	if (Existing == Definitions.end())
	{
		Definitions.push_back(std::move(Definition));
	}
	else
	{
		*Existing = std::move(Definition);
	}
}

[[nodiscard]] bool ReadBinaryFile(
	const std::filesystem::path& Path,
	xr_vector<u8>& OutBytes
)
{
	std::ifstream Stream(Path, std::ios::binary);
	if (!Stream)
	{
		return false;
	}
	Stream.seekg(0, std::ios::end);
	const std::streamoff Size = Stream.tellg();
	if (Size <= 0)
	{
		return false;
	}
	Stream.seekg(0, std::ios::beg);
	OutBytes.resize(static_cast<size_t>(Size));
	return static_cast<bool>(Stream.read(
		reinterpret_cast<char*>(OutBytes.data()), Size
	));
}

[[nodiscard]] bool LoadCompiledLibrary(
	const std::filesystem::path& Path,
	xr_vector<FEditorParticleAssetInfo>& Assets,
	xr_vector<FTiramisuEditorParticleEffectDefinition>& EffectDefinitions,
	xr_vector<FTiramisuEditorParticleGroupDefinition>& GroupDefinitions
)
{
	xr_vector<u8> Bytes;
	if (!ReadBinaryFile(Path, Bytes))
	{
		return false;
	}
	u16 LibraryVersion = 0;
	bool Valid = true;
	const FParticleChunkView Root{Bytes.data(), Bytes.size()};
	const bool VersionScanValid = ForEachChunk(
		Root,
		[&](const u32 Id, const FParticleChunkView Chunk)
		{
			if (Id == ParticleLibraryVersionChunk &&
				Chunk.Size >= sizeof(u16))
			{
				LibraryVersion = ReadU16(Chunk.Data);
			}
		}
	);
	if (!VersionScanValid ||
		(LibraryVersion != ParticleLibraryOriginalVersion &&
		 LibraryVersion != ParticleLibraryExtendedVersion))
	{
		return false;
	}
	const bool RootValid = ForEachChunk(Root, [&](const u32 Id, const FParticleChunkView Collection)
	{
		const bool IsEffectCollection =
			(LibraryVersion == ParticleLibraryOriginalVersion &&
			 Id == OriginalParticleEffectsChunk) ||
			(LibraryVersion == ParticleLibraryExtendedVersion &&
			 Id == ExtendedParticleEffectsChunk);
		const bool IsGroupCollection =
			(LibraryVersion == ParticleLibraryOriginalVersion &&
			 Id == OriginalParticleGroupsChunk) ||
			(LibraryVersion == ParticleLibraryExtendedVersion &&
			 Id == ExtendedParticleGroupsChunk);
		const bool IsCurveCollection =
			LibraryVersion == ParticleLibraryExtendedVersion &&
			Id == ExtendedParticleCurvesChunk;
		if (!IsEffectCollection && !IsGroupCollection &&
			!IsCurveCollection)
		{
			return;
		}
		const bool CollectionValid = ForEachChunk(
			Collection,
			[&](const u32, const FParticleChunkView Record)
			{
				FEditorParticleAssetInfo Asset;
				if (IsEffectCollection)
				{
					FTiramisuEditorParticleEffectDefinition Definition;
					ParseEffectRecord(Record, Asset, Definition);
					AddOrReplaceEffectDefinition(
						EffectDefinitions,
						std::move(Definition)
					);
				}
				else if (IsGroupCollection)
				{
					FTiramisuEditorParticleGroupDefinition Definition;
					ParseGroupRecord(Record, Asset, Definition);
					AddOrReplaceGroupDefinition(
						GroupDefinitions,
						std::move(Definition)
					);
				}
				else
				{
					ParseCurveRecord(Record, Asset);
				}
				AddOrReplaceAsset(Assets, std::move(Asset));
			}
		);
		Valid = Valid && CollectionValid;
	});
	return RootValid && Valid;
}

[[nodiscard]] xr_string Trim(xr_string Value)
{
	const auto IsSpace = [](const unsigned char Character)
	{
		return std::isspace(Character) != 0;
	};
	Value.erase(
		Value.begin(),
		std::find_if_not(Value.begin(), Value.end(), IsSpace)
	);
	Value.erase(
		std::find_if_not(Value.rbegin(), Value.rend(), IsSpace).base(),
		Value.end()
	);
	return Value;
}

[[nodiscard]] xr_string ToLower(xr_string Value)
{
	std::transform(
		Value.begin(), Value.end(), Value.begin(),
		[](const unsigned char Character)
		{
			return static_cast<char>(std::tolower(Character));
		}
	);
	return Value;
}

[[nodiscard]] xr_string MakeLooseAssetName(
	const std::filesystem::path& Root,
	const std::filesystem::path& Path
)
{
	std::error_code Error;
	std::filesystem::path Relative = std::filesystem::relative(Path, Root, Error);
	if (Error)
	{
		Relative = Path.filename();
	}
	Relative.replace_extension();
	xr_string Result = Relative.generic_string().c_str();
	std::replace(Result.begin(), Result.end(), '/', '\\');
	return Result;
}

void ParseLooseText(
	const std::filesystem::path& Path,
	FEditorParticleAssetInfo& Asset
)
{
	std::ifstream Stream(Path);
	if (!Stream)
	{
		return;
	}
	xr_string Line;
	while (std::getline(Stream, Line))
	{
		const size_t Separator = Line.find('=');
		if (Separator == xr_string::npos)
		{
			continue;
		}
		const xr_string Key = ToLower(Trim(Line.substr(0, Separator)));
		const xr_string Value = Trim(Line.substr(Separator + 1));
		if (Value.empty())
		{
			continue;
		}
		if (Key == "shader")
		{
			Asset.ShaderName = Value;
		}
		else if (Key == "texture")
		{
			Asset.TextureName = Value;
		}
		else if (Key == "effect_name" || Key == "on_play_child" ||
				 Key == "on_birth_child" || Key == "on_death_child" ||
				 Key == "str_animator")
		{
			AppendDependency(Asset.Dependencies, Value);
		}
		else if (Asset.Type == EEditorParticleAssetType::AnimationCurve &&
				 Key == "name")
		{
			Asset.Name = Value;
		}
	}
}

void LoadLooseAssets(
	const std::filesystem::path& Root,
	xr_vector<FEditorParticleAssetInfo>& Assets
)
{
	std::error_code Error;
	if (!std::filesystem::is_directory(Root, Error))
	{
		return;
	}
	std::filesystem::recursive_directory_iterator Iterator(
		Root,
		std::filesystem::directory_options::skip_permission_denied,
		Error
	);
	const std::filesystem::recursive_directory_iterator End;
	for (; Iterator != End; Iterator.increment(Error))
	{
		if (Error)
		{
			Error.clear();
			continue;
		}
		if (!Iterator->is_regular_file(Error))
		{
			continue;
		}
		const xr_string Extension = ToLower(
			Iterator->path().extension().string().c_str()
		);
		FEditorParticleAssetInfo Asset;
		if (Extension == ".pe")
		{
			Asset.Type = EEditorParticleAssetType::Effect;
		}
		else if (Extension == ".pg")
		{
			Asset.Type = EEditorParticleAssetType::Group;
		}
		else if (Extension == ".pac")
		{
			Asset.Type = EEditorParticleAssetType::AnimationCurve;
		}
		else
		{
			continue;
		}
		Asset.Name = MakeLooseAssetName(Root, Iterator->path());
		ParseLooseText(Iterator->path(), Asset);
		AddOrReplaceAsset(Assets, std::move(Asset));
	}
}

void NormalizeAssets(xr_vector<FEditorParticleAssetInfo>& Assets)
{
	for (FEditorParticleAssetInfo& Asset : Assets)
	{
		std::ranges::sort(Asset.Dependencies);
		Asset.Dependencies.erase(
			std::unique(
				Asset.Dependencies.begin(), Asset.Dependencies.end()
			),
			Asset.Dependencies.end()
		);
	}
	std::ranges::sort(
		Assets,
		[](const FEditorParticleAssetInfo& Left,
		   const FEditorParticleAssetInfo& Right)
		{
			if (Left.Type != Right.Type)
			{
				return Left.Type < Right.Type;
			}
			return Left.Name < Right.Name;
		}
	);
}

void SynchronizeEffectDefinitions(
	xr_vector<FEditorParticleAssetInfo>& Assets,
	xr_vector<FTiramisuEditorParticleEffectDefinition>& Definitions
)
{
	std::ranges::sort(
		Definitions,
		{},
		&FTiramisuEditorParticleEffectDefinition::Name
	);
	for (FEditorParticleAssetInfo& Asset : Assets)
	{
		if (Asset.Type != EEditorParticleAssetType::Effect)
		{
			continue;
		}
		const auto Definition = std::ranges::lower_bound(
			Definitions,
			Asset.Name,
			{},
			&FTiramisuEditorParticleEffectDefinition::Name
		);
		if (Definition == Definitions.end() ||
			Definition->Name != Asset.Name)
		{
			continue;
		}
		// Loose assets переопределяют отображаемые shader/texture, но compiled
		// action list продолжает поступать из particles.xr.
		if (!Asset.ShaderName.empty())
		{
			Definition->ShaderName = Asset.ShaderName;
		}
		if (!Asset.TextureName.empty())
		{
			Definition->TextureName = Asset.TextureName;
		}
		Asset.MaxParticles = Definition->MaxParticles;
		Asset.HasCompiledActions = Definition->IsSimulatable();
	}
}

void SynchronizeGroupDefinitions(
	xr_vector<FEditorParticleAssetInfo>& Assets,
	xr_vector<FTiramisuEditorParticleGroupDefinition>& Definitions
)
{
	std::ranges::sort(
		Definitions,
		{},
		&FTiramisuEditorParticleGroupDefinition::Name
	);
	for (FEditorParticleAssetInfo& Asset : Assets)
	{
		if (Asset.Type != EEditorParticleAssetType::Group)
		{
			continue;
		}
		const auto Definition = std::ranges::lower_bound(
			Definitions,
			Asset.Name,
			{},
			&FTiramisuEditorParticleGroupDefinition::Name
		);
		if (Definition == Definitions.end() ||
			Definition->Name != Asset.Name)
		{
			continue;
		}
		Asset.GroupEntryCount =
			static_cast<u32>(Definition->Entries.size());
		Asset.EnabledGroupEntryCount = static_cast<u32>(
			std::ranges::count_if(
				Definition->Entries,
				[](const FTiramisuEditorParticleGroupEntry& Entry)
				{
					return (Entry.Flags & GroupEntryEnabledFlag) != 0;
				}
			)
		);
		Asset.GroupChildCallbackCount = 0;
		for (const FTiramisuEditorParticleGroupEntry& Entry :
			 Definition->Entries)
		{
			if ((Entry.Flags & GroupEntryEnabledFlag) == 0)
			{
				continue;
			}
			Asset.GroupChildCallbackCount +=
				(Entry.Flags & GroupEntryOnPlayChildFlag) != 0 &&
				!Entry.OnPlayChildName.empty();
			Asset.GroupChildCallbackCount +=
				(Entry.Flags & GroupEntryOnBirthChildFlag) != 0 &&
				!Entry.OnBirthChildName.empty();
			Asset.GroupChildCallbackCount +=
				(Entry.Flags & GroupEntryOnDeathChildFlag) != 0 &&
				!Entry.OnDeathChildName.empty();
		}
	}
}

void HashBytes(u64& Hash, const void* Data, const size_t Size)
{
	const auto* Bytes = static_cast<const u8*>(Data);
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Hash ^= Bytes[Index];
		Hash *= FnvPrime;
	}
}

void HashString(u64& Hash, const xr_string& Value)
{
	HashBytes(Hash, Value.data(), Value.size());
	const u8 Separator = 0;
	HashBytes(Hash, &Separator, sizeof(Separator));
}

[[nodiscard]] u64 CalculateRevision(
	const xr_vector<FEditorParticleAssetInfo>& Assets,
	const xr_vector<FTiramisuEditorParticleEffectDefinition>& EffectDefinitions,
	const xr_vector<FTiramisuEditorParticleGroupDefinition>& GroupDefinitions
)
{
	u64 Hash = FnvOffset;
	for (const FEditorParticleAssetInfo& Asset : Assets)
	{
		HashBytes(Hash, &Asset.Type, sizeof(Asset.Type));
		HashString(Hash, Asset.Name);
		HashString(Hash, Asset.ShaderName);
		HashString(Hash, Asset.TextureName);
		HashBytes(Hash, &Asset.MaxParticles, sizeof(Asset.MaxParticles));
		HashBytes(
			Hash,
			&Asset.GroupEntryCount,
			sizeof(Asset.GroupEntryCount)
		);
		HashBytes(
			Hash,
			&Asset.EnabledGroupEntryCount,
			sizeof(Asset.EnabledGroupEntryCount)
		);
		HashBytes(
			Hash,
			&Asset.GroupChildCallbackCount,
			sizeof(Asset.GroupChildCallbackCount)
		);
		HashBytes(
			Hash,
			&Asset.HasCompiledActions,
			sizeof(Asset.HasCompiledActions)
		);
		for (const xr_string& Dependency : Asset.Dependencies)
		{
			HashString(Hash, Dependency);
		}
	}
	for (const FTiramisuEditorParticleEffectDefinition& Definition :
		 EffectDefinitions)
	{
		HashString(Hash, Definition.Name);
		HashString(Hash, Definition.ShaderName);
		HashString(Hash, Definition.TextureName);
		HashBytes(Hash, &Definition.Flags, sizeof(Definition.Flags));
		HashBytes(
			Hash,
			&Definition.MaxParticles,
			sizeof(Definition.MaxParticles)
		);
		HashBytes(
			Hash,
			Definition.CompiledActions.data(),
			Definition.CompiledActions.size()
		);
		HashBytes(
			Hash,
			Definition.FrameTexSize.data(),
			sizeof(Definition.FrameTexSize)
		);
		HashBytes(
			Hash,
			Definition.VelocityScale.data(),
			sizeof(Definition.VelocityScale)
		);
		HashBytes(
			Hash,
			Definition.AlignToPathDefaultRotation.data(),
			sizeof(Definition.AlignToPathDefaultRotation)
		);
		HashBytes(
			Hash,
			&Definition.FrameDimensionX,
			sizeof(Definition.FrameDimensionX)
		);
		HashBytes(
			Hash,
			&Definition.FrameCount,
			sizeof(Definition.FrameCount)
		);
		HashBytes(
			Hash,
			&Definition.FrameSpeed,
			sizeof(Definition.FrameSpeed)
		);
		HashBytes(
			Hash,
			&Definition.TimeLimit,
			sizeof(Definition.TimeLimit)
		);
	}
	for (const FTiramisuEditorParticleGroupDefinition& Definition :
		 GroupDefinitions)
	{
		HashString(Hash, Definition.Name);
		HashBytes(Hash, &Definition.Flags, sizeof(Definition.Flags));
		HashBytes(
			Hash,
			&Definition.TimeLimit,
			sizeof(Definition.TimeLimit)
		);
		for (const FTiramisuEditorParticleGroupEntry& Entry :
			 Definition.Entries)
		{
			HashString(Hash, Entry.EffectName);
			HashString(Hash, Entry.OnPlayChildName);
			HashString(Hash, Entry.OnBirthChildName);
			HashString(Hash, Entry.OnDeathChildName);
			HashBytes(Hash, &Entry.StartTime, sizeof(Entry.StartTime));
			HashBytes(Hash, &Entry.StopTime, sizeof(Entry.StopTime));
			HashBytes(Hash, &Entry.Flags, sizeof(Entry.Flags));
		}
	}
	return Hash == 0 ? 1 : Hash;
}
} // namespace

bool TiramisuEditorParticleLibrary::Reload(
	const std::filesystem::path& CompiledLibrary,
	const std::filesystem::path& LooseAssetsRoot
)
{
	FEditorParticleLibrarySnapshot Next;
	xr_vector<FTiramisuEditorParticleEffectDefinition> NextEffectDefinitions;
	xr_vector<FTiramisuEditorParticleGroupDefinition> NextGroupDefinitions;
	const bool CompiledLoaded = LoadCompiledLibrary(
		CompiledLibrary,
		Next.Assets,
		NextEffectDefinitions,
		NextGroupDefinitions
	);
	LoadLooseAssets(LooseAssetsRoot, Next.Assets);
	SynchronizeEffectDefinitions(Next.Assets, NextEffectDefinitions);
	SynchronizeGroupDefinitions(Next.Assets, NextGroupDefinitions);
	NormalizeAssets(Next.Assets);
	if (Next.Assets.empty())
	{
		Next.Diagnostic =
			"Tiramisu particle library: assets were not found";
		std::scoped_lock Lock(Mutex);
		Snapshot = std::move(Next);
		EffectDefinitions.clear();
		GroupDefinitions.clear();
		return false;
	}
	Next.Revision = CalculateRevision(
		Next.Assets,
		NextEffectDefinitions,
		NextGroupDefinitions
	);
	Next.Diagnostic = CompiledLoaded
		? "Tiramisu particle library loaded"
		: "Tiramisu particle library loaded from loose assets only";
	std::scoped_lock Lock(Mutex);
	Snapshot = std::move(Next);
	EffectDefinitions = std::move(NextEffectDefinitions);
	GroupDefinitions = std::move(NextGroupDefinitions);
	return true;
}

void TiramisuEditorParticleLibrary::CopySnapshot(
	FEditorParticleLibrarySnapshot& OutSnapshot
) const
{
	std::scoped_lock Lock(Mutex);
	OutSnapshot = Snapshot;
}

bool TiramisuEditorParticleLibrary::CopyEffectDefinition(
	const xr_string_view Name,
	FTiramisuEditorParticleEffectDefinition& OutDefinition
) const
{
	std::scoped_lock Lock(Mutex);
	const auto Definition = std::ranges::find_if(
		EffectDefinitions,
		[Name](const FTiramisuEditorParticleEffectDefinition& Candidate)
		{
			return Candidate.Name == Name;
		}
	);
	if (Definition == EffectDefinitions.end() || Definition->Name != Name)
	{
		OutDefinition = {};
		return false;
	}
	OutDefinition = *Definition;
	return true;
}

bool TiramisuEditorParticleLibrary::CopyGroupDefinition(
	const xr_string_view Name,
	FTiramisuEditorParticleGroupDefinition& OutDefinition
) const
{
	std::scoped_lock Lock(Mutex);
	const auto Definition = std::ranges::find_if(
		GroupDefinitions,
		[Name](const FTiramisuEditorParticleGroupDefinition& Candidate)
		{
			return Candidate.Name == Name;
		}
	);
	if (Definition == GroupDefinitions.end() || Definition->Name != Name)
	{
		OutDefinition = {};
		return false;
	}
	OutDefinition = *Definition;
	return true;
}
