#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorParticleLibrary.h"

#include <chrono>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>

namespace
{
void AppendU32(xr_vector<u8>& Bytes, const u32 Value)
{
	const size_t Offset = Bytes.size();
	Bytes.resize(Offset + sizeof(Value));
	std::memcpy(Bytes.data() + Offset, &Value, sizeof(Value));
}

void AppendFloat(xr_vector<u8>& Bytes, const float Value)
{
	const size_t Offset = Bytes.size();
	Bytes.resize(Offset + sizeof(Value));
	std::memcpy(Bytes.data() + Offset, &Value, sizeof(Value));
}

void AppendString(xr_vector<u8>& Bytes, const xr_string_view Value)
{
	Bytes.insert(Bytes.end(), Value.begin(), Value.end());
	Bytes.push_back(0);
}

void AppendChunk(
	xr_vector<u8>& Parent,
	const u32 Id,
	const xr_vector<u8>& Payload
)
{
	AppendU32(Parent, Id);
	AppendU32(Parent, static_cast<u32>(Payload.size()));
	Parent.insert(Parent.end(), Payload.begin(), Payload.end());
}

[[nodiscard]] xr_vector<u8> MakeNamedRecord(const xr_string_view Name)
{
	xr_vector<u8> Record;
	xr_vector<u8> NameChunk;
	AppendString(NameChunk, Name);
	AppendChunk(Record, 2, NameChunk);
	return Record;
}

[[nodiscard]] xr_vector<u8> MakeCompiledLibrary()
{
	xr_vector<u8> Root;
	xr_vector<u8> Version;
	AppendU32(Version, 2);
	AppendChunk(Root, 1, Version);

	xr_vector<u8> Effect = MakeNamedRecord("effects\\test");
	xr_vector<u8> EffectData;
	AppendU32(EffectData, 64);
	AppendChunk(Effect, 3, EffectData);
	xr_vector<u8> Actions;
	AppendU32(Actions, 0);
	AppendChunk(Effect, 4, Actions);
	xr_vector<u8> Flags;
	AppendU32(Flags, 1u | (1u << 10) | (1u << 14));
	AppendChunk(Effect, 5, Flags);
	xr_vector<u8> Frame;
	AppendFloat(Frame, 0.25f);
	AppendFloat(Frame, 0.5f);
	AppendFloat(Frame, 0.0f);
	AppendFloat(Frame, 0.0f);
	AppendU32(Frame, 4);
	AppendU32(Frame, 8);
	AppendFloat(Frame, 12.0f);
	AppendChunk(Effect, 6, Frame);
	xr_vector<u8> Sprite;
	AppendString(Sprite, "particles\\add");
	AppendString(Sprite, "pfx\\compiled");
	AppendChunk(Effect, 7, Sprite);
	xr_vector<u8> TimeLimit;
	AppendFloat(TimeLimit, 3.5f);
	AppendChunk(Effect, 8, TimeLimit);
	xr_vector<u8> VelocityScale;
	AppendFloat(VelocityScale, 1.0f);
	AppendFloat(VelocityScale, 2.0f);
	AppendFloat(VelocityScale, 3.0f);
	AppendChunk(Effect, 0x22, VelocityScale);
	xr_vector<u8> AlignToPath;
	AppendFloat(AlignToPath, -1.0f);
	AppendFloat(AlignToPath, 0.25f);
	AppendFloat(AlignToPath, 0.5f);
	AppendChunk(Effect, 0x25, AlignToPath);
	xr_vector<u8> Effects;
	AppendChunk(Effects, 0, Effect);
	AppendChunk(Root, 2, Effects);

	xr_vector<u8> Group = MakeNamedRecord("groups\\test");
	xr_vector<u8> GroupFlags;
	AppendU32(GroupFlags, 0x1234u);
	AppendChunk(Group, 3, GroupFlags);
	xr_vector<u8> GroupEffects;
	AppendU32(GroupEffects, 1);
	AppendString(GroupEffects, "effects\\test");
	AppendString(GroupEffects, "effects\\play-child");
	AppendString(GroupEffects, "effects\\birth-child");
	AppendString(GroupEffects, "effects\\death-child");
	AppendFloat(GroupEffects, 0.25f);
	AppendFloat(GroupEffects, 1.5f);
	AppendU32(GroupEffects, 0x45u);
	AppendChunk(Group, 4, GroupEffects);
	xr_vector<u8> GroupTimeLimit;
	AppendFloat(GroupTimeLimit, 2.5f);
	AppendChunk(Group, 5, GroupTimeLimit);
	xr_vector<u8> Groups;
	AppendChunk(Groups, 0, Group);
	AppendChunk(Root, 3, Groups);

	xr_vector<u8> Curves;
	AppendChunk(Curves, 0, MakeNamedRecord("curves\\compiled"));
	AppendChunk(Root, 4, Curves);
	return Root;
}

[[nodiscard]] const FEditorParticleAssetInfo* FindAsset(
	const FEditorParticleLibrarySnapshot& Snapshot,
	const EEditorParticleAssetType Type,
	const xr_string_view Name
)
{
	for (const FEditorParticleAssetInfo& Asset : Snapshot.Assets)
	{
		if (Asset.Type == Type && Asset.Name == Name)
		{
			return &Asset;
		}
	}
	return nullptr;
}

int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main()
{
	const std::filesystem::path Root =
		std::filesystem::temp_directory_path() /
		("ixray-particle-library-" + std::to_string(
			std::chrono::steady_clock::now().time_since_epoch().count()
		));
	struct FCleanup
	{
		std::filesystem::path Path;
		~FCleanup()
		{
			std::error_code Error;
			std::filesystem::remove_all(Path, Error);
		}
	} Cleanup{Root};

	std::filesystem::create_directories(Root / "loose" / "effects");
	{
		const xr_vector<u8> Bytes = MakeCompiledLibrary();
		std::ofstream Stream(Root / "particles.xr", std::ios::binary);
		Stream.write(
			reinterpret_cast<const char*>(Bytes.data()),
			static_cast<std::streamsize>(Bytes.size())
		);
	}
	{
		std::ofstream Stream(Root / "loose" / "effects" / "test.pe");
		Stream << "[sprite]\n"
				   "shader = particles\\alpha\n"
				   "texture = pfx\\loose\n"
				   "str_animator = curves\\compiled\n";
	}
	{
		std::ofstream Stream(Root / "loose" / "extra.pac");
		Stream << "[_anim_curve]\nname = curves\\extra\n";
	}

	TiramisuEditorParticleLibrary Library;
	if (!Library.Reload(Root / "particles.xr", Root / "loose"))
	{
		return Fail("particle library reload failed");
	}
	FEditorParticleLibrarySnapshot First;
	Library.CopySnapshot(First);
	if (!First.IsReady() || First.Assets.size() != 4)
	{
		return Fail("particle catalog count or revision is invalid");
	}
	const FEditorParticleAssetInfo* Effect = FindAsset(
		First, EEditorParticleAssetType::Effect, "effects\\test"
	);
	if (!Effect || Effect->ShaderName != "particles\\alpha" ||
		Effect->TextureName != "pfx\\loose" ||
		Effect->Dependencies.size() != 1 ||
		Effect->MaxParticles != 64 || !Effect->HasCompiledActions)
	{
		return Fail("loose particle effect did not replace compiled metadata");
	}
	FTiramisuEditorParticleEffectDefinition Definition;
	if (!Library.CopyEffectDefinition("effects\\test", Definition) ||
		!Definition.IsSimulatable() || Definition.MaxParticles != 64 ||
		Definition.ShaderName != "particles\\alpha" ||
		Definition.TextureName != "pfx\\loose" ||
		Definition.CompiledActions.size() != sizeof(u32) ||
		Definition.FrameTexSize[0] != 0.25f ||
		Definition.FrameTexSize[1] != 0.5f ||
		Definition.FrameDimensionX != 4 || Definition.FrameCount != 8 ||
		Definition.FrameSpeed != 12.0f || Definition.TimeLimit != 3.5f ||
		Definition.VelocityScale[0] != 1.0f ||
		Definition.VelocityScale[1] != 2.0f ||
		Definition.VelocityScale[2] != 3.0f ||
		Definition.AlignToPathDefaultRotation[0] != -1.0f ||
		Definition.AlignToPathDefaultRotation[1] != 0.25f ||
		Definition.AlignToPathDefaultRotation[2] != 0.5f)
	{
		return Fail("compiled particle effect runtime data was not preserved");
	}
	if (Library.CopyEffectDefinition("effects\\missing", Definition))
	{
		return Fail("missing particle definition unexpectedly resolved");
	}
	const FEditorParticleAssetInfo* Group = FindAsset(
		First, EEditorParticleAssetType::Group, "groups\\test"
	);
	if (!Group || Group->Dependencies.size() != 4 ||
		Group->GroupEntryCount != 1 ||
		Group->EnabledGroupEntryCount != 1 ||
		Group->GroupChildCallbackCount != 1 ||
		!std::ranges::contains(
			Group->Dependencies,
			xr_string("effects\\test")
		) ||
		!std::ranges::contains(
			Group->Dependencies,
			xr_string("effects\\death-child")
		))
	{
		return Fail("particle group dependencies were not parsed");
	}
	FTiramisuEditorParticleGroupDefinition GroupDefinition;
	if (!Library.CopyGroupDefinition("groups\\test", GroupDefinition) ||
		GroupDefinition.Flags != 0x1234u ||
		GroupDefinition.TimeLimit != 2.5f ||
		GroupDefinition.Entries.size() != 1)
	{
		return Fail("compiled particle group runtime data was not preserved");
	}
	const FTiramisuEditorParticleGroupEntry& GroupEntry =
		GroupDefinition.Entries.front();
	if (GroupEntry.EffectName != "effects\\test" ||
		GroupEntry.OnPlayChildName != "effects\\play-child" ||
		GroupEntry.OnBirthChildName != "effects\\birth-child" ||
		GroupEntry.OnDeathChildName != "effects\\death-child" ||
		GroupEntry.StartTime != 0.25f || GroupEntry.StopTime != 1.5f ||
		GroupEntry.Flags != 0x45u)
	{
		return Fail("particle group entry schedule was not parsed");
	}
	if (Library.CopyGroupDefinition("groups\\missing", GroupDefinition))
	{
		return Fail("missing particle group unexpectedly resolved");
	}
	if (!FindAsset(
			First,
			EEditorParticleAssetType::AnimationCurve,
			"curves\\extra"
		))
	{
		return Fail("loose particle curve name was not parsed");
	}

	const u64 Revision = First.Revision;
	First.Assets.clear();
	FEditorParticleLibrarySnapshot Second;
	Library.CopySnapshot(Second);
	if (Second.Assets.size() != 4 || Second.Revision != Revision)
	{
		return Fail("particle snapshot must be an owned deterministic copy");
	}
	if (!Library.Reload(Root / "particles.xr", Root / "loose"))
	{
		return Fail("second particle reload failed");
	}
	FEditorParticleLibrarySnapshot Third;
	Library.CopySnapshot(Third);
	if (Third.Revision != Revision)
	{
		return Fail("unchanged particle assets changed catalog revision");
	}
	return 0;
}
