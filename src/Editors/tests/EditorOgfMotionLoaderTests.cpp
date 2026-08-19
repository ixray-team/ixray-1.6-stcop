#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorOgfMotionLoader.h"
#include "../../xrEngine/Fmesh.h"

#include <cmath>
#include <cstring>
#include <iostream>

namespace
{
#pragma pack(push, 2)
struct FTestRotationKey16
{
	s16 X = 0;
	s16 Y = 0;
	s16 Z = 0;
	s16 W = 32767;
};

struct FTestRotationKey32
{
	float X = 0.0f;
	float Y = 0.0f;
	float Z = 0.0f;
	float W = 1.0f;
};

struct FTestTranslationKey32
{
	float X = 0.0f;
	float Y = 0.0f;
	float Z = 0.0f;
};
#pragma pack(pop)

template <typename T>
void AppendValue(xr_vector<u8>& Bytes, const T& Value)
{
	const size_t Offset = Bytes.size();
	Bytes.resize(Offset + sizeof(T));
	std::memcpy(Bytes.data() + Offset, &Value, sizeof(T));
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
	AppendValue(Parent, Id);
	AppendValue(Parent, static_cast<u32>(Payload.size()));
	Parent.insert(Parent.end(), Payload.begin(), Payload.end());
}

void AppendDefinition(
	xr_vector<u8>& Parameters,
	const char* Name,
	const u32 Flags
)
{
	AppendString(Parameters, Name);
	AppendValue(Parameters, Flags);
	AppendValue(Parameters, u16{0});
	AppendValue(Parameters, u16{0});
	AppendValue(Parameters, 1.0f);
	AppendValue(Parameters, 1.0f);
	AppendValue(Parameters, 0.1f);
	AppendValue(Parameters, 0.1f);
	AppendValue(Parameters, 0u);
}

void AppendTrackPair(
	xr_vector<u8>& Motion,
	const float LastChildTranslation
)
{
	constexpr u8 TranslationPresent = 1u << 0u;
	constexpr u8 RotationAbsent = 1u << 1u;
	constexpr u8 FullFloat = 1u << 3u;
	AppendValue(Motion, static_cast<u8>(TranslationPresent | FullFloat));
	AppendValue(Motion, 0x12345678u);
	AppendValue(Motion, FTestRotationKey32{});
	AppendValue(Motion, FTestRotationKey32{});
	AppendValue(Motion, 0x87654321u);
	AppendValue(Motion, FTestTranslationKey32{});
	FTestTranslationKey32 ChildEnd;
	ChildEnd.X = LastChildTranslation;
	AppendValue(Motion, ChildEnd);

	AppendValue(Motion, RotationAbsent);
	AppendValue(Motion, FTestRotationKey16{});
	Fvector RootTranslation;
	RootTranslation.set(10.0f, 0.0f, 0.0f);
	AppendValue(Motion, RootTranslation);
}

[[nodiscard]] xr_vector<u8> MakeMotionContainer()
{
	xr_vector<u8> Container;
	xr_vector<u8> Parameters;
	AppendValue(Parameters, u16{4});
	AppendValue(Parameters, u16{1});
	AppendString(Parameters, "all");
	AppendValue(Parameters, u16{2});
	AppendString(Parameters, "root");
	AppendValue(Parameters, 1u);
	AppendString(Parameters, "child");
	AppendValue(Parameters, 0u);
	AppendValue(Parameters, u16{2});
	AppendDefinition(Parameters, "idle", 0u);
	AppendDefinition(Parameters, "once", 1u << 1u);
	AppendChunk(Container, OGF_S_SMPARAMS, Parameters);

	xr_vector<u8> Motions;
	xr_vector<u8> CountChunk;
	AppendValue(CountChunk, 2u);
	AppendChunk(Motions, 0u, CountChunk);
	xr_vector<u8> Idle;
	AppendString(Idle, "idle");
	AppendValue(Idle, 2u);
	AppendTrackPair(Idle, 2.0f);
	AppendChunk(Motions, 1u, Idle);
	xr_vector<u8> Once;
	AppendString(Once, "once");
	AppendValue(Once, 2u);
	AppendTrackPair(Once, 4.0f);
	AppendChunk(Motions, 2u, Once);
	AppendChunk(Container, OGF_S_MOTIONS, Motions);
	return Container;
}

bool TestMotionDecodeAndSampling()
{
	xr_vector<u8> Bytes = MakeMotionContainer();
	IReader Reader(Bytes.data(), Bytes.size());
	const xr_array<xr_string, 2> BoneNames = {"root", "child"};
	FTiramisuEditorOgfMotionSet Motions;
	if (!LoadTiramisuEditorOgfMotions(Reader, BoneNames, Motions) ||
		Motions.Clips.size() != 2u ||
		!Motions.FindClip("IDLE") ||
		std::abs(Motions.Clips[0].DurationSeconds() - 2.0f / 30.0f) >
			1.0e-6f)
	{
		std::cerr << "synthetic OMF motions were not decoded: "
			<< Motions.Diagnostic.c_str() << '\n';
		return false;
	}
	xr_vector<Fmatrix> Pose;
	xr_string Diagnostic;
	if (!SampleTiramisuEditorOgfMotion(
			Motions, "idle", 1.0f / 60.0f, Pose, &Diagnostic
		) || Pose.size() != 2u ||
		std::abs(Pose[0]._41 - 10.0f) > 1.0e-5f ||
		std::abs(Pose[1]._41 - 1.0f) > 1.0e-5f)
	{
		std::cerr << "looped OMF sampling or bone remap is invalid\n";
		return false;
	}
	if (!SampleTiramisuEditorOgfMotion(
			Motions, "idle", 2.0f / 30.0f, Pose, &Diagnostic
		) || std::abs(Pose[1]._41) > 1.0e-5f)
	{
		std::cerr << "looped OMF sampling did not wrap\n";
		return false;
	}
	if (!SampleTiramisuEditorOgfMotion(
			Motions, "once", 10.0f, Pose, &Diagnostic
		) || std::abs(Pose[1]._41 - 4.0f) > 1.0e-5f)
	{
		std::cerr << "stop-at-end OMF sampling did not clamp\n";
		return false;
	}
	if (SampleTiramisuEditorOgfMotion(
			Motions, "missing", 0.0f, Pose, &Diagnostic
		) || Diagnostic.empty())
	{
		std::cerr << "missing OMF clip was accepted\n";
		return false;
	}
	return true;
}

bool TestMotionReferences()
{
	xr_vector<u8> Root;
	xr_vector<u8> OldReferences;
	AppendString(OldReferences, " actors\\shared , monsters/shared ");
	AppendChunk(Root, OGF_S_MOTION_REFS, OldReferences);
	IReader OldReader(Root.data(), Root.size());
	xr_vector<xr_string> References;
	xr_string Diagnostic;
	if (!LoadTiramisuEditorOgfMotionReferences(
			OldReader, References, &Diagnostic
		) || References.size() != 2u ||
		References[0] != "actors\\shared" ||
		References[1] != "monsters\\shared")
	{
		std::cerr << "legacy OGF motion references were not decoded\n";
		return false;
	}
	Root.clear();
	xr_vector<u8> References2;
	AppendValue(References2, 2u);
	AppendString(References2, "actors/a");
	AppendString(References2, "actors/b");
	AppendChunk(Root, OGF_S_MOTION_REFS2, References2);
	IReader NewReader(Root.data(), Root.size());
	if (!LoadTiramisuEditorOgfMotionReferences(
			NewReader, References, &Diagnostic
		) || References.size() != 2u ||
		References[0] != "actors\\a" ||
		References[1] != "actors\\b")
	{
		std::cerr << "versioned OGF motion references were not decoded\n";
		return false;
	}
	return true;
}

bool TestInvalidMotionPayload()
{
	xr_vector<u8> Bytes = MakeMotionContainer();
	u32 ParameterChunkSize = 0;
	std::memcpy(&ParameterChunkSize, Bytes.data() + sizeof(u32),
		sizeof(ParameterChunkSize));
	Bytes.resize(2u * sizeof(u32) + ParameterChunkSize);
	IReader Reader(Bytes.data(), Bytes.size());
	const xr_array<xr_string, 2> BoneNames = {"root", "child"};
	FTiramisuEditorOgfMotionSet Motions;
	if (LoadTiramisuEditorOgfMotions(Reader, BoneNames, Motions) ||
		Motions.Diagnostic.empty())
	{
		std::cerr << "truncated OMF payload was accepted\n";
		return false;
	}
	return true;
}
} // namespace

int main()
{
	if (!TestMotionDecodeAndSampling() || !TestMotionReferences() ||
		!TestInvalidMotionPayload())
	{
		return 1;
	}
	std::cout << "Editor OGF/OMF motion loader tests passed\n";
	return 0;
}
