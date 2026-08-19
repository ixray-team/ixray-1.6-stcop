#include "TiramisuEditorOgfMotionLoader.h"

#include "../../../xrEngine/Fmesh.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstring>
#include <limits>

namespace
{
constexpr u32 MaxMotionCount = 0x3fffu;
constexpr u32 MaxMotionFrameCount = 1u << 20u;
constexpr u32 MaxMotionReferenceCount = 64u;
constexpr u32 MaxPartitionCount = 64u;
constexpr u32 MaxMotionMarks = 1u << 16u;
constexpr size_t MaxMotionNameLength = 1024u;
constexpr float SampleFramesPerSecond = 30.0f;
constexpr float RotationQuantizerInverse = 1.0f / 32767.0f;
constexpr u8 TranslationKeyPresent = 1u << 0u;
constexpr u8 RotationKeyAbsent = 1u << 1u;
constexpr u8 TranslationKey16Bit = 1u << 2u;
constexpr u8 FullFloatKeys = 1u << 3u;
constexpr u32 MotionFx = 1u << 0u;
constexpr u32 MotionStopAtEnd = 1u << 1u;

#pragma pack(push, 2)
struct FRotationKey16
{
	s16 X = 0;
	s16 Y = 0;
	s16 Z = 0;
	s16 W = 0;
};

struct FRotationKey32
{
	float X = 0.0f;
	float Y = 0.0f;
	float Z = 0.0f;
	float W = 1.0f;
};

struct FTranslationKey8
{
	s8 X = 0;
	s8 Y = 0;
	s8 Z = 0;
};

struct FTranslationKey16
{
	s16 X = 0;
	s16 Y = 0;
	s16 Z = 0;
};

struct FTranslationKey32
{
	float X = 0.0f;
	float Y = 0.0f;
	float Z = 0.0f;
};
#pragma pack(pop)

static_assert(sizeof(FRotationKey16) == 8u);
static_assert(sizeof(FRotationKey32) == 16u);
static_assert(sizeof(FTranslationKey8) == 3u);
static_assert(sizeof(FTranslationKey16) == 6u);
static_assert(sizeof(FTranslationKey32) == 12u);

struct FScopedReader
{
	IReader* Reader = nullptr;

	explicit FScopedReader(IReader* InReader) : Reader(InReader) {}
	~FScopedReader()
	{
		if (Reader)
		{
			Reader->close();
		}
	}

	IReader* operator->() const { return Reader; }
	IReader& operator*() const { return *Reader; }
	explicit operator bool() const { return Reader != nullptr; }
};

struct FMotionDefinition
{
	xr_string Name;
	u32 Flags = 0;
	float Speed = 1.0f;
};

void SetDiagnostic(xr_string* OutDiagnostic, const char* Diagnostic)
{
	if (OutDiagnostic)
	{
		*OutDiagnostic = Diagnostic;
	}
}

[[nodiscard]] bool ReadBoundedString(
	IReader& Reader,
	xr_string& OutValue
)
{
	const size_t Available = Reader.elapsed();
	const size_t SearchLength =
		std::min(Available, MaxMotionNameLength + 1u);
	const auto* Begin = static_cast<const char*>(Reader.pointer());
	const auto* End = static_cast<const char*>(
		std::memchr(Begin, 0, SearchLength)
	);
	if (!End)
	{
		return false;
	}
	const size_t Length = static_cast<size_t>(End - Begin);
	OutValue.assign(Begin, Length);
	Reader.advance(static_cast<intptr_t>(Length + 1u));
	return true;
}

[[nodiscard]] bool SkipBoundedLine(IReader& Reader)
{
	const size_t Available = Reader.elapsed();
	const size_t SearchLength =
		std::min(Available, MaxMotionNameLength + 2u);
	const auto* Begin = static_cast<const char*>(Reader.pointer());
	size_t Length = 0;
	while (Length < SearchLength && Begin[Length] != '\r' &&
		Begin[Length] != '\n')
	{
		++Length;
	}
	if (Length == SearchLength)
	{
		return false;
	}
	while (Length < Available &&
		(Begin[Length] == '\r' || Begin[Length] == '\n'))
	{
		++Length;
	}
	Reader.advance(static_cast<intptr_t>(Length));
	return true;
}

template <typename T>
[[nodiscard]] bool ReadValues(
	IReader& Reader,
	const size_t Count,
	xr_vector<T>& OutValues
)
{
	if (Count > std::numeric_limits<size_t>::max() / sizeof(T) ||
		Count * sizeof(T) > static_cast<size_t>(Reader.elapsed()))
	{
		return false;
	}
	OutValues.resize(Count);
	Reader.r(OutValues.data(), Count * sizeof(T));
	return true;
}

[[nodiscard]] bool SkipBytes(IReader& Reader, const size_t Count)
{
	if (Count > static_cast<size_t>(Reader.elapsed()))
	{
		return false;
	}
	Reader.advance(static_cast<intptr_t>(Count));
	return true;
}

[[nodiscard]] bool IsFinite(const Fquaternion& Value)
{
	return std::isfinite(Value.x) && std::isfinite(Value.y) &&
		std::isfinite(Value.z) && std::isfinite(Value.w);
}

[[nodiscard]] bool IsFinite(const Fvector& Value)
{
	return std::isfinite(Value.x) && std::isfinite(Value.y) &&
		std::isfinite(Value.z);
}

[[nodiscard]] bool NormalizeRotation(Fquaternion& Rotation)
{
	if (!IsFinite(Rotation) || Rotation.magnitude() <= 1.0e-12f)
	{
		return false;
	}
	Rotation.normalize();
	return IsFinite(Rotation);
}

[[nodiscard]] bool EqualsCaseInsensitive(
	const xr_string_view Left,
	const xr_string_view Right
)
{
	if (Left.size() != Right.size())
	{
		return false;
	}
	for (size_t Index = 0; Index < Left.size(); ++Index)
	{
		const unsigned char LeftCharacter =
			static_cast<unsigned char>(Left[Index]);
		const unsigned char RightCharacter =
			static_cast<unsigned char>(Right[Index]);
		if (std::tolower(LeftCharacter) != std::tolower(RightCharacter))
		{
			return false;
		}
	}
	return true;
}

[[nodiscard]] bool SkipMotionMarks(
	IReader& Parameters,
	const u16 Version
)
{
	if (Version < 4u)
	{
		return true;
	}
	if (Parameters.elapsed() < sizeof(u32))
	{
		return false;
	}
	const u32 MarkCount = Parameters.r_u32();
	if (MarkCount > MaxMotionMarks)
	{
		return false;
	}
	for (u32 Mark = 0; Mark < MarkCount; ++Mark)
	{
		if (!SkipBoundedLine(Parameters) ||
			Parameters.elapsed() < sizeof(u32))
		{
			return false;
		}
		const u32 IntervalCount = Parameters.r_u32();
		if (IntervalCount > MaxMotionMarks ||
			!SkipBytes(
				Parameters,
				static_cast<size_t>(IntervalCount) * 2u * sizeof(float)
			))
		{
			return false;
		}
	}
	return true;
}

[[nodiscard]] bool ParseMotionDefinitions(
	IReader& Reader,
	const xr_span<const xr_string> BoneNames,
	xr_vector<u16>& OutMotionToModelBone,
	xr_vector<FMotionDefinition>& OutDefinitions,
	xr_string& OutDiagnostic
)
{
	FScopedReader Parameters(Reader.open_chunk(OGF_S_SMPARAMS));
	if (!Parameters || Parameters->elapsed() < 2u * sizeof(u16))
	{
		OutDiagnostic = "OGF/OMF has no valid motion parameters";
		return false;
	}
	const u16 Version = Parameters->r_u16();
	if (Version > xrOGF_SMParamsVersion)
	{
		OutDiagnostic = "OGF/OMF motion parameter version is unsupported";
		return false;
	}
	const u16 PartitionCount = Parameters->r_u16();
	if (PartitionCount > MaxPartitionCount || BoneNames.empty() ||
		BoneNames.size() > std::numeric_limits<u16>::max())
	{
		OutDiagnostic = "OGF/OMF motion partitions are invalid";
		return false;
	}
	OutMotionToModelBone.assign(BoneNames.size(),
		std::numeric_limits<u16>::max());
	xr_hash_map<xr_string, u16> ModelBones;
	ModelBones.reserve(BoneNames.size());
	for (size_t BoneIndex = 0; BoneIndex < BoneNames.size(); ++BoneIndex)
	{
		xr_string Name = BoneNames[BoneIndex];
		xr_strlwr(Name);
		if (!ModelBones.try_emplace(
				Name, static_cast<u16>(BoneIndex)
			).second)
		{
			OutDiagnostic = "Model has duplicate bone names";
			return false;
		}
	}
	u32 PartitionBoneCount = 0;
	for (u16 Partition = 0; Partition < PartitionCount; ++Partition)
	{
		xr_string PartitionName;
		if (!ReadBoundedString(*Parameters, PartitionName) ||
			Parameters->elapsed() < sizeof(u16))
		{
			OutDiagnostic = "OGF/OMF motion partition is truncated";
			return false;
		}
		const u16 BoneCount = Parameters->r_u16();
		PartitionBoneCount += BoneCount;
		if (PartitionBoneCount > BoneNames.size())
		{
			OutDiagnostic = "OGF/OMF motion partition bone count is invalid";
			return false;
		}
		for (u16 Bone = 0; Bone < BoneCount; ++Bone)
		{
			xr_string BoneName;
			if (!ReadBoundedString(*Parameters, BoneName) ||
				Parameters->elapsed() < sizeof(u32))
			{
				OutDiagnostic = "OGF/OMF motion bone remap is truncated";
				return false;
			}
			xr_strlwr(BoneName);
			const u32 MotionBoneIndex = Parameters->r_u32();
			const auto ModelBone = ModelBones.find(BoneName);
			if (MotionBoneIndex >= OutMotionToModelBone.size() ||
				ModelBone == ModelBones.end() ||
				OutMotionToModelBone[MotionBoneIndex] !=
					std::numeric_limits<u16>::max())
			{
				OutDiagnostic = "OGF/OMF motion bone remap is invalid";
				return false;
			}
			OutMotionToModelBone[MotionBoneIndex] = ModelBone->second;
		}
	}
	if (PartitionBoneCount != BoneNames.size() ||
		std::ranges::find(
			OutMotionToModelBone,
			std::numeric_limits<u16>::max()
		) != OutMotionToModelBone.end() ||
		Parameters->elapsed() < sizeof(u16))
	{
		OutDiagnostic = "OGF/OMF motion bone remap is incomplete";
		return false;
	}
	const u16 MotionCount = Parameters->r_u16();
	if (MotionCount == 0 || MotionCount >= MaxMotionCount)
	{
		OutDiagnostic = "OGF/OMF motion count is invalid";
		return false;
	}
	OutDefinitions.resize(MotionCount);
	for (u16 Motion = 0; Motion < MotionCount; ++Motion)
	{
		FMotionDefinition& Definition = OutDefinitions[Motion];
		constexpr size_t DefinitionBytes =
			2u * sizeof(u16) + 4u * sizeof(float);
		if (!ReadBoundedString(*Parameters, Definition.Name) ||
			Parameters->elapsed() < sizeof(u32) + DefinitionBytes)
		{
			OutDiagnostic = "OGF/OMF motion definition is truncated";
			return false;
		}
		xr_strlwr(Definition.Name);
		Definition.Flags = Parameters->r_u32();
		(void)Parameters->r_u16();
		(void)Parameters->r_u16();
		Definition.Speed = Parameters->r_float();
		(void)Parameters->r_float();
		(void)Parameters->r_float();
		(void)Parameters->r_float();
		if (!std::isfinite(Definition.Speed) ||
			!SkipMotionMarks(*Parameters, Version))
		{
			OutDiagnostic = "OGF/OMF motion definition is invalid";
			return false;
		}
		Definition.Speed = std::max(0.0f, Definition.Speed);
	}
	return true;
}

[[nodiscard]] bool DecodeRotations(
	IReader& Motion,
	const u8 Flags,
	const u32 FrameCount,
	xr_vector<Fquaternion>& OutRotations
)
{
	const bool Constant = (Flags & RotationKeyAbsent) != 0;
	const bool FullFloat = (Flags & FullFloatKeys) != 0;
	const size_t StoredCount = Constant ? 1u : FrameCount;
	if (!Constant && !SkipBytes(Motion, sizeof(u32)))
	{
		return false;
	}
	OutRotations.resize(FrameCount);
	if (FullFloat)
	{
		xr_vector<FRotationKey32> Keys;
		if (!ReadValues(Motion, StoredCount, Keys))
		{
			return false;
		}
		for (u32 Frame = 0; Frame < FrameCount; ++Frame)
		{
			const FRotationKey32& Key = Keys[Constant ? 0u : Frame];
			Fquaternion& Rotation = OutRotations[Frame];
			Rotation.x = Key.X;
			Rotation.y = Key.Y;
			Rotation.z = Key.Z;
			Rotation.w = Key.W;
			if (!NormalizeRotation(Rotation))
			{
				return false;
			}
		}
	}
	else
	{
		xr_vector<FRotationKey16> Keys;
		if (!ReadValues(Motion, StoredCount, Keys))
		{
			return false;
		}
		for (u32 Frame = 0; Frame < FrameCount; ++Frame)
		{
			const FRotationKey16& Key = Keys[Constant ? 0u : Frame];
			Fquaternion& Rotation = OutRotations[Frame];
			Rotation.x = static_cast<float>(Key.X) *
				RotationQuantizerInverse;
			Rotation.y = static_cast<float>(Key.Y) *
				RotationQuantizerInverse;
			Rotation.z = static_cast<float>(Key.Z) *
				RotationQuantizerInverse;
			Rotation.w = static_cast<float>(Key.W) *
				RotationQuantizerInverse;
			if (!NormalizeRotation(Rotation))
			{
				return false;
			}
		}
	}
	return true;
}

[[nodiscard]] bool DecodeTranslations(
	IReader& Motion,
	const u8 Flags,
	const u32 FrameCount,
	xr_vector<Fvector>& OutTranslations
)
{
	OutTranslations.resize(FrameCount);
	if ((Flags & TranslationKeyPresent) == 0)
	{
		if (Motion.elapsed() < sizeof(Fvector))
		{
			return false;
		}
		Fvector Translation = Motion.r_vec3();
		if (!IsFinite(Translation))
		{
			return false;
		}
		std::ranges::fill(OutTranslations, Translation);
		return true;
	}
	if (!SkipBytes(Motion, sizeof(u32)))
	{
		return false;
	}
	if ((Flags & FullFloatKeys) != 0)
	{
		xr_vector<FTranslationKey32> Keys;
		if (!ReadValues(Motion, FrameCount, Keys))
		{
			return false;
		}
		for (u32 Frame = 0; Frame < FrameCount; ++Frame)
		{
			OutTranslations[Frame].set(
				Keys[Frame].X, Keys[Frame].Y, Keys[Frame].Z
			);
		}
		return true;
	}
	const bool Use16Bit = (Flags & TranslationKey16Bit) != 0;
	xr_vector<FTranslationKey16> Keys16;
	xr_vector<FTranslationKey8> Keys8;
	if ((Use16Bit && !ReadValues(Motion, FrameCount, Keys16)) ||
		(!Use16Bit && !ReadValues(Motion, FrameCount, Keys8)) ||
		Motion.elapsed() < 2u * sizeof(Fvector))
	{
		return false;
	}
	const Fvector Size = Motion.r_vec3();
	const Fvector Initial = Motion.r_vec3();
	if (!IsFinite(Size) || !IsFinite(Initial))
	{
		return false;
	}
	for (u32 Frame = 0; Frame < FrameCount; ++Frame)
	{
		const float X = Use16Bit
			? static_cast<float>(Keys16[Frame].X)
			: static_cast<float>(Keys8[Frame].X);
		const float Y = Use16Bit
			? static_cast<float>(Keys16[Frame].Y)
			: static_cast<float>(Keys8[Frame].Y);
		const float Z = Use16Bit
			? static_cast<float>(Keys16[Frame].Z)
			: static_cast<float>(Keys8[Frame].Z);
		OutTranslations[Frame].set(
			X * Size.x + Initial.x,
			Y * Size.y + Initial.y,
			Z * Size.z + Initial.z
		);
		if (!IsFinite(OutTranslations[Frame]))
		{
			return false;
		}
	}
	return true;
}

[[nodiscard]] bool DecodeMotionTrack(
	IReader& Motion,
	const u32 FrameCount,
	FTiramisuEditorOgfMotionTrack& OutTrack
)
{
	if (Motion.elapsed() < sizeof(u8))
	{
		return false;
	}
	const u8 Flags = Motion.r_u8();
	return DecodeRotations(
			Motion, Flags, FrameCount, OutTrack.Rotations
		) && DecodeTranslations(
			Motion, Flags, FrameCount, OutTrack.Translations
		);
}

[[nodiscard]] bool ParseMotionKeys(
	IReader& Reader,
	const xr_vector<u16>& MotionToModelBone,
	const xr_vector<FMotionDefinition>& Definitions,
	FTiramisuEditorOgfMotionSet& OutMotions
)
{
	FScopedReader Motions(Reader.open_chunk(OGF_S_MOTIONS));
	if (!Motions)
	{
		OutMotions.Diagnostic = "OGF/OMF has no motion key chunk";
		return false;
	}
	u32 MotionCount = 0;
	if (!Motions->r_chunk_safe(0u, &MotionCount, sizeof(MotionCount)) ||
		MotionCount != Definitions.size())
	{
		OutMotions.Diagnostic = "OGF/OMF motion key count is invalid";
		return false;
	}
	OutMotions.Clips.resize(MotionCount);
	for (u32 MotionIndex = 0; MotionIndex < MotionCount; ++MotionIndex)
	{
		FScopedReader Motion(Motions->open_chunk(MotionIndex + 1u));
		if (!Motion)
		{
			OutMotions.Diagnostic = "OGF/OMF motion key record is missing";
			return false;
		}
		xr_string StoredName;
		if (!ReadBoundedString(*Motion, StoredName) ||
			Motion->elapsed() < sizeof(u32))
		{
			OutMotions.Diagnostic = "OGF/OMF motion key record is truncated";
			return false;
		}
		xr_strlwr(StoredName);
		const u32 FrameCount = Motion->r_u32();
		if (FrameCount == 0 || FrameCount > MaxMotionFrameCount ||
			!EqualsCaseInsensitive(
				StoredName, Definitions[MotionIndex].Name
			))
		{
			OutMotions.Diagnostic = "OGF/OMF motion key metadata is invalid";
			return false;
		}
		FTiramisuEditorOgfMotionClip& Clip =
			OutMotions.Clips[MotionIndex];
		Clip.Name = Definitions[MotionIndex].Name;
		Clip.FrameCount = FrameCount;
		Clip.Speed = Definitions[MotionIndex].Speed;
		Clip.StopAtEnd =
			(Definitions[MotionIndex].Flags & MotionStopAtEnd) != 0;
		Clip.Fx = (Definitions[MotionIndex].Flags & MotionFx) != 0;
		Clip.BoneTracks.resize(MotionToModelBone.size());
		for (size_t MotionBone = 0;
			 MotionBone < MotionToModelBone.size(); ++MotionBone)
		{
			const u16 ModelBone = MotionToModelBone[MotionBone];
			if (ModelBone >= Clip.BoneTracks.size() ||
				!DecodeMotionTrack(
					*Motion,
					FrameCount,
					Clip.BoneTracks[ModelBone]
				))
			{
				OutMotions.Diagnostic =
					"OGF/OMF motion bone track is invalid";
				return false;
			}
		}
	}
	return true;
}

void TrimReference(xr_string& Reference)
{
	const size_t First = Reference.find_first_not_of(" \t\r\n");
	if (First == xr_string::npos)
	{
		Reference.clear();
		return;
	}
	const size_t Last = Reference.find_last_not_of(" \t\r\n");
	Reference = Reference.substr(First, Last - First + 1u);
	std::ranges::replace(Reference, '/', '\\');
	xr_strlwr(Reference);
}
} // namespace

float FTiramisuEditorOgfMotionClip::DurationSeconds() const noexcept
{
	return FrameCount == 0 || Speed <= 0.0f
		? 0.0f
		: static_cast<float>(FrameCount) /
			(SampleFramesPerSecond * Speed);
}

const FTiramisuEditorOgfMotionClip*
FTiramisuEditorOgfMotionSet::FindClip(const xr_string_view Name) const noexcept
{
	for (const FTiramisuEditorOgfMotionClip& Clip : Clips)
	{
		if (EqualsCaseInsensitive(Clip.Name, Name))
		{
			return &Clip;
		}
	}
	return nullptr;
}

bool LoadTiramisuEditorOgfMotions(
	IReader& Reader,
	const xr_span<const xr_string> BoneNames,
	FTiramisuEditorOgfMotionSet& OutMotions
)
{
	OutMotions = {};
	xr_vector<u16> MotionToModelBone;
	xr_vector<FMotionDefinition> Definitions;
	if (!ParseMotionDefinitions(
			Reader,
			BoneNames,
			MotionToModelBone,
			Definitions,
			OutMotions.Diagnostic
		) || !ParseMotionKeys(
			Reader,
			MotionToModelBone,
			Definitions,
			OutMotions
		))
	{
		OutMotions.Clips.clear();
		return false;
	}
	OutMotions.Diagnostic.clear();
	return true;
}

bool LoadTiramisuEditorOgfMotionReferences(
	IReader& Reader,
	xr_vector<xr_string>& OutReferences,
	xr_string* OutDiagnostic
)
{
	OutReferences.clear();
	if (OutDiagnostic)
	{
		OutDiagnostic->clear();
	}
	if (FScopedReader References2(
			Reader.open_chunk(OGF_S_MOTION_REFS2)
		); References2)
	{
		if (References2->elapsed() < sizeof(u32))
		{
			SetDiagnostic(OutDiagnostic,
				"OGF motion reference list is truncated");
			return false;
		}
		const u32 Count = References2->r_u32();
		if (Count > MaxMotionReferenceCount)
		{
			SetDiagnostic(OutDiagnostic,
				"OGF motion reference count is invalid");
			return false;
		}
		OutReferences.reserve(Count);
		for (u32 Index = 0; Index < Count; ++Index)
		{
			xr_string Reference;
			if (!ReadBoundedString(*References2, Reference))
			{
				SetDiagnostic(OutDiagnostic,
					"OGF motion reference is truncated");
				return false;
			}
			TrimReference(Reference);
			if (Reference.empty())
			{
				SetDiagnostic(OutDiagnostic,
					"OGF motion reference is empty");
				return false;
			}
			OutReferences.push_back(std::move(Reference));
		}
		return true;
	}
	FScopedReader References(Reader.open_chunk(OGF_S_MOTION_REFS));
	if (!References)
	{
		return true;
	}
	xr_string List;
	if (!ReadBoundedString(*References, List))
	{
		SetDiagnostic(OutDiagnostic, "OGF motion reference list is invalid");
		return false;
	}
	size_t Offset = 0;
	while (Offset <= List.size())
	{
		const size_t End = List.find(',', Offset);
		xr_string Reference = List.substr(
			Offset,
			End == xr_string::npos ? xr_string::npos : End - Offset
		);
		TrimReference(Reference);
		if (!Reference.empty())
		{
			if (OutReferences.size() >= MaxMotionReferenceCount)
			{
				SetDiagnostic(OutDiagnostic,
					"OGF motion reference count is invalid");
				return false;
			}
			OutReferences.push_back(std::move(Reference));
		}
		if (End == xr_string::npos)
		{
			break;
		}
		Offset = End + 1u;
	}
	return true;
}

bool SampleTiramisuEditorOgfMotion(
	const FTiramisuEditorOgfMotionSet& Motions,
	const xr_string_view ClipName,
	const float TimeSeconds,
	xr_vector<Fmatrix>& OutLocalPose,
	xr_string* OutDiagnostic
)
{
	OutLocalPose.clear();
	if (OutDiagnostic)
	{
		OutDiagnostic->clear();
	}
	const FTiramisuEditorOgfMotionClip* Clip = Motions.FindClip(ClipName);
	if (!Clip || Clip->FrameCount == 0 || Clip->BoneTracks.empty() ||
		!std::isfinite(TimeSeconds))
	{
		SetDiagnostic(OutDiagnostic, "OGF/OMF motion clip is unavailable");
		return false;
	}
	float FrameTime =
		std::max(0.0f, TimeSeconds) * Clip->Speed *
		SampleFramesPerSecond;
	if (Clip->StopAtEnd)
	{
		FrameTime = std::min(
			FrameTime, static_cast<float>(Clip->FrameCount - 1u)
		);
	}
	else
	{
		FrameTime = std::fmod(
			FrameTime, static_cast<float>(Clip->FrameCount)
		);
	}
	const u32 FirstFrame = static_cast<u32>(std::floor(FrameTime));
	const u32 SecondFrame = Clip->StopAtEnd
		? std::min(FirstFrame + 1u, Clip->FrameCount - 1u)
		: (FirstFrame + 1u) % Clip->FrameCount;
	const float Alpha = std::clamp(
		FrameTime - static_cast<float>(FirstFrame), 0.0f, 1.0f
	);
	OutLocalPose.resize(Clip->BoneTracks.size());
	for (size_t Bone = 0; Bone < Clip->BoneTracks.size(); ++Bone)
	{
		const FTiramisuEditorOgfMotionTrack& Track =
			Clip->BoneTracks[Bone];
		if (Track.Rotations.size() != Clip->FrameCount ||
			Track.Translations.size() != Clip->FrameCount)
		{
			SetDiagnostic(OutDiagnostic,
				"OGF/OMF motion track has an invalid frame count");
			OutLocalPose.clear();
			return false;
		}
		Fquaternion Rotation;
		Rotation.slerp(
			Track.Rotations[FirstFrame],
			Track.Rotations[SecondFrame],
			Alpha
		);
		Fvector Translation;
		Translation.lerp(
			Track.Translations[FirstFrame],
			Track.Translations[SecondFrame],
			Alpha
		);
		if (!NormalizeRotation(Rotation) || !IsFinite(Translation))
		{
			SetDiagnostic(OutDiagnostic,
				"OGF/OMF sampled transform is invalid");
			OutLocalPose.clear();
			return false;
		}
		OutLocalPose[Bone].mk_xform(Rotation, Translation);
	}
	return true;
}
