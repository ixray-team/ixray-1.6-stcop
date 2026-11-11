#pragma once
#include "imgui.h"
#include "imgui_internal.h"
#include "ImGuizmo.h"
#include "../../Cinema/CinemaPlayer.h"
#include "../../Cinema/CinemaHistory.h"
#include "../../Cinema/CinemaSceneIo.h"

extern float g_fov;
class IRHISurface;
class IRHIShaderResourceView;

namespace Cinema
{
inline constexpr ImVec4 ErrorColor = ImVec4(0.95f, 0.35f, 0.35f, 1.0f);
inline constexpr ImVec4 OverrideColor = ImVec4(1.0f, 165.0f / 255.0f, 0.0f, 1.0f);
inline constexpr const char* TypeNames[8] = { "Actor", "Camera", "Light", "Particle", "Decal", "Sound", "Weather", "Post" };

inline void Timecode(string32& Out, float Time, u32 Rate)
{
	const u32 Frames = static_cast<u32>(std::floor(std::max(Time, 0.0f) * Rate + 1e-4f));
	xr_sprintf(Out, "%02u:%02u:%02u", Frames / Rate / 60, Frames / Rate % 60, Frames % Rate);
}

inline u32 Lighten(u32 Color)
{
	return 0xFF000000u | (((Color & 0x00FEFEFEu) >> 1) + 0x007F7F7Fu);
}

inline xr_string ModelName(const shared_str& Visual)
{
	return Platform::ANSI_TO_UTF8(NameFromVisual(Visual.c_str()));
}

inline xr_string DefaultName(const Object& Item)
{
	const char* const Fixed[8] = { nullptr, "Camera", Item.Light.Kind == LightKind::Spot ? "Spot light" : "Point light", nullptr, nullptr, Item.Sound.File.size() ? Item.Sound.File.c_str() : "Sound", Item.Weather.Cycle.size() ? Item.Weather.Cycle.c_str() : "Weather", "Post" };
	xr_string Name = Fixed[static_cast<u32>(Item.Type)] ? xr_string(Fixed[static_cast<u32>(Item.Type)]) : Item.Type == ObjectType::Decal ? Platform::ANSI_TO_UTF8(EffectName(Item.Decal.Texture.size() ? Item.Decal.Texture : shared_str("Decal"))) : Item.Type == ObjectType::Particle ? Platform::ANSI_TO_UTF8(EffectName(Item.Particle.Effect)) : ModelName(Item.Actor.Visual);
	size_t Size = std::min<size_t>(Name.size(), MaxNameBytes);
	while (Size < Name.size() && (static_cast<u8>(Name[Size]) & 0xC0) == 0x80)
	{
		--Size;
	}
	Name.resize(Size);
	return Name;
}

inline bool IsParamShown(const Object& Target, Param Id)
{
	return (Id != Param::AttachWeight || Target.Type == ObjectType::Camera || Target.Attach.Parent.ObjectId) && (Id != Param::LookWeight || Target.Type == ObjectType::Camera || Target.Attach.LookAt.ObjectId) && (Id != Param::FollowSmooth || Target.Type == ObjectType::Camera || Target.Attach.Parent.ObjectId || Target.Attach.LookAt.ObjectId) && !(Target.Type == ObjectType::Light && Target.Light.Kind == LightKind::Point && (Id == Param::Rotation || Id == Param::Cone));
}

inline const char* DisplayName(const Object* Item)
{
	return !Item ? "missing" : Item->Name.empty() ? TypeNames[static_cast<u32>(Item->Type)] : Item->Name.c_str();
}

inline const char* RuntimeError(const Player& Core, const Object& Target)
{
	const ActorRuntime* Body = Target.Type == ObjectType::Actor ? Core.FindActor(Target.Id) : nullptr;
	const LightRuntime* Lamp = Target.Type == ObjectType::Light ? Core.FindLight(Target.Id) : nullptr;
	const ParticleRuntime* Emitter = Target.Type == ObjectType::Particle ? Core.FindParticle(Target.Id) : nullptr;
	const DecalRuntime* Mark = Target.Type == ObjectType::Decal ? Core.FindDecal(Target.Id) : nullptr;
	const shared_str* Error = Body ? &Body->Error : Lamp ? &Lamp->Error : Emitter ? &Emitter->Error : Mark ? &Mark->Error : nullptr;
	return Error && Error->size() ? Error->c_str() : Body && !Body->IsComplete() ? "missing" : nullptr;
}

inline constexpr s32 ShakeBase = 1024, ClipBase = 2048;
inline float ClipOrigin = 0.0f;
inline constexpr float FrameAspects[9] = { 0.0f, 2.39f, 21.0f / 9.0f, 2.0f, 1.85f, 16.0f / 9.0f, 4.0f / 3.0f, 1.0f, 9.0f / 16.0f };
inline constexpr const char* AspectNames = "2.39:1\0" "21:9\0" "2:1\0" "1.85:1\0" "16:9\0" "4:3\0" "1:1\0" "9:16\0";

inline constexpr s32 ClipSpan = 64, IkBase = 4096;

inline IkLimb* IkOf(Object& Target, s32 Block)
{
	return Block >= IkBase ? Target.Actor.Clips[Block - IkBase].Ik : Target.Actor.Ik;
}

inline const IkLimb* IkOf(const Object& Target, s32 Block)
{
	return IkOf(const_cast<Object&>(Target), Block);
}

inline u32 ClipIndex(s32 Block)
{
	return static_cast<u32>(Block - ClipBase) % ClipSpan;
}

inline u32 ClipLayerOf(s32 Block)
{
	return static_cast<u32>(Block - ClipBase) / ClipSpan;
}

inline ClipTrack& TrackOf(Object& Target, s32 Block)
{
	AnimClip& Clip = Target.Actor.Clips[ClipIndex(Block)];
	const u32 Layer = ClipLayerOf(Block);
	return Layer ? static_cast<ClipTrack&>(Clip.Layers[Layer - 1]) : Clip;
}

inline const ClipTrack& TrackOf(const Object& Target, s32 Block)
{
	return TrackOf(const_cast<Object&>(Target), Block);
}

inline bool HasOwner(const Object& Target, s32 Block)
{
	return Block < 0 || (Block >= IkBase ? Block - IkBase < static_cast<s32>(Target.Actor.Clips.size()) : Block >= ClipBase ? ClipIndex(Block) < Target.Actor.Clips.size() && ClipLayerOf(Block) <= Target.Actor.Clips[ClipIndex(Block)].Layers.size() : Block >= ShakeBase ? Block - ShakeBase < static_cast<s32>(Target.Camera.Shakes.size()) : Block < static_cast<s32>(Target.Camera.Blocks.size()));
}

inline xr_vector<Lane>& LaneList(Object& Target, s32 Block)
{
	return Block < 0 ? Target.Lanes : Block >= IkBase ? Target.Actor.Clips[Block - IkBase].IkLanes : Block >= ClipBase ? TrackOf(Target, Block).Lanes : Block >= ShakeBase ? Target.Camera.Shakes[Block - ShakeBase].Lanes : Target.Camera.Blocks[Block].Lanes;
}

inline const xr_vector<Lane>& LaneList(const Object& Target, s32 Block)
{
	return LaneList(const_cast<Object&>(Target), Block);
}

inline PoseBone* BoneEntry(Object& Target, s32 Block, u8 Layer)
{
	if (Target.Type != ObjectType::Actor)
	{
		return nullptr;
	}
	const u32 Row = Layer / MaxPoseBones;
	xr_vector<PoseBone>* Bones = Block >= ClipBase ? &TrackOf(Target, Block).Bones : Row < Target.Actor.Poses.size() ? &Target.Actor.Poses[Row].Bones : nullptr;
	const u32 Bone = Block >= ClipBase ? Layer : Layer % MaxPoseBones;
	return Bones && Bone < Bones->size() ? &(*Bones)[Bone] : nullptr;
}

inline RowMode BoneMode(const Object& Target, s32 Block, u8 Layer)
{
	return Block >= ClipBase ? TrackOf(Target, Block).Mode : Target.Actor.Poses[Layer / MaxPoseBones].Mode;
}

inline float* StaticField(Object& Target, s32 Block, Param Id, u8 Layer, u8 Component)
{
	if (Block >= ClipBase && Block < IkBase)
	{
		PoseBone* Item = IsBoneParam(Id) ? BoneEntry(Target, Block, Layer) : nullptr;
		return Item ? &(Id == Param::BoneRotation ? Item->Rotation : Id == Param::BonePosition ? Item->Offset : Item->Scale)[Component] : nullptr;
	}
	if (Id == Param::AttachWeight || Id == Param::LookWeight || Id == Param::FollowSmooth)
	{
		Attachment* Links = Block < 0 ? (Target.Type == ObjectType::Camera ? nullptr : &Target.Attach) : Block < ShakeBase ? &Target.Camera.Blocks[Block].Attach : nullptr;
		return !Links ? nullptr : Id == Param::AttachWeight ? &Links->Weight : Id == Param::LookWeight ? &Links->LookWeight : &Links->Smooth;
	}
	if (Block >= IkBase && (Id < Param::IkTarget || Id > Param::IkWeight))
	{
		return nullptr;
	}
	if ((Block < 0 || Block >= IkBase) && Id >= Param::IkTarget && Id <= Param::IkWeight)
	{
		IkLimb* Limb = Target.Type == ObjectType::Actor && Layer < MaxIkLimbs ? &IkOf(Target, Block)[Layer] : nullptr;
		return !Limb ? nullptr : Id == Param::IkWeight ? &Limb->Weight : &(Id == Param::IkTarget ? Limb->Target : Id == Param::IkTurn ? Limb->Turn : Limb->Pole)[Component];
	}
	if (Block < 0 && (Id == Param::DecalWidth || Id == Param::DecalHeight))
	{
		return Target.Type == ObjectType::Decal ? &(Id == Param::DecalWidth ? Target.Decal.Width : Target.Decal.Height) : nullptr;
	}
	if (Block < 0 && Id == Param::ParticleOpacity)
	{
		return Target.Type == ObjectType::Particle ? &Target.Particle.Opacity : nullptr;
	}
	if (Block < 0 && (Id == Param::SoundVolume || Id == Param::SoundPitch))
	{
		return Target.Type == ObjectType::Sound ? &(Id == Param::SoundVolume ? Target.Sound.Volume : Target.Sound.Pitch) : nullptr;
	}
	if (Block < 0 && Id >= Param::WeatherTime && Id <= Param::SkyRotation)
	{
		return Target.Type == ObjectType::Weather ? WeatherField(Target.Weather, Id, Component) : nullptr;
	}
	if (Block < 0 && Id >= Param::PostBase && Id <= Param::PostMotionBlur)
	{
		return Target.Type == ObjectType::Post ? PostField(Target.Post, Id, Component) : nullptr;
	}
	if (Block < 0 && Id == Param::ConstraintWeight)
	{
		return Target.Type == ObjectType::Actor && Layer < Target.Actor.Constraints.size() ? &Target.Actor.Constraints[Layer].Weight : nullptr;
	}
	if (Block < 0 && Id == Param::PoseWeight)
	{
		return Target.Type == ObjectType::Actor && Layer < Target.Actor.Poses.size() ? &Target.Actor.Poses[Layer].Weight : nullptr;
	}
	if (Block < 0 && IsBoneParam(Id))
	{
		const u32 Row = Layer / MaxPoseBones, Bone = Layer % MaxPoseBones;
		PoseBone* Item = Target.Type == ObjectType::Actor && Row < Target.Actor.Poses.size() && Bone < Target.Actor.Poses[Row].Bones.size() ? &Target.Actor.Poses[Row].Bones[Bone] : nullptr;
		return Item ? &(Id == Param::BoneRotation ? Item->Rotation : Id == Param::BonePosition ? Item->Offset : Item->Scale)[Component] : nullptr;
	}
	if (Block < 0 && Id == Param::LayerWeight)
	{
		return Target.Type == ObjectType::Actor && Layer <= Target.Actor.Layers.size() ? &RowOf(Target.Actor, Layer).Weight : nullptr;
	}
	if (Block < 0)
	{
		Fvector* Place = PlacementOf(Target, Id == Param::Rotation ? 1 : 0);
		ActorData& Actor = Target.Actor;
		LightData& Light = Target.Light;
		ParticleData& Particle = Target.Particle;
		float* const Fields[static_cast<u32>(Param::Count)] = { Place ? &(*Place)[Component] : nullptr, Place ? &(*Place)[Component] : nullptr, &Actor.Scale[Component], &Actor.TimeScale, nullptr, &Light.Color[Component], &Light.Intensity, &Light.Range, &Light.Cone, &Light.VirtualSize, &Light.VolumeIntensity, &Light.VolumeDistance, &Light.VolumeQuality, &Particle.Size, &Particle.Speed, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr };
		return (Info(Id).Where & ObjectWhere[static_cast<u32>(Target.Type)]) ? Fields[static_cast<u32>(Id)] : nullptr;
	}
	if (Block >= ShakeBase)
	{
		ShakeClip& Clip = Target.Camera.Shakes[Block - ShakeBase];
		float* const Shaken[3] = { &Clip.Setup.Influence, &Clip.Setup.PositionScale, &Clip.Setup.Speed };
		return Id >= Param::ShakeInfluence && Clip.Row == Layer ? Shaken[static_cast<u32>(Id) - static_cast<u32>(Param::ShakeInfluence)] : nullptr;
	}
	CameraBlock& Shot = Target.Camera.Blocks[Block];
	float* const Shots[] = { &Shot.Position[Component], &Shot.Rotation[Component], &Shot.TimeScale, &Shot.Fov, &Shot.Focus, &Shot.FocusOffset, &Shot.NearRange, &Shot.FarRange, &Shot.Blur, &Shot.Aperture, &Shot.FocalLength, &Shot.Point[Component & 1], &Shot.Limit, &Shot.Squeeze, &Shot.FocusTime, &Shot.TransitionTime, &Shot.Swirl, &Shot.Smooth };
	const u32 At = Id <= Param::Rotation ? static_cast<u32>(Id) : Id == Param::TimeScale ? 2 : Id == Param::Fov ? 3 : Id >= Param::DofFocus && Id <= Param::CameraSmooth ? 4 + static_cast<u32>(Id) - static_cast<u32>(Param::DofFocus) : 18;
	return At < 18 ? Shots[At] : nullptr;
}

inline float TargetStart(const Object& Target, s32 Block)
{
	return Block >= ClipBase ? ClipOrigin : Target.Span.Start + (Block < 0 ? 0.0f : Block >= ShakeBase ? Target.Camera.Shakes[Block - ShakeBase].Start : Cameras::BlockStart(Target.Camera, static_cast<u32>(Block)));
}

inline float LaneLocal(const Object& Target, s32 Block, Param Id, float Time)
{
	const float Local = Time - TargetStart(Target, Block);
	return Block >= 0 && Block < ShakeBase && Id <= Param::Rotation ? Cameras::PathTime(Target.Camera.Blocks[Block], Local) : Local;
}

inline float LaneMoment(const Object& Target, s32 Block, Param Id, float KeyTime)
{
	return TargetStart(Target, Block) + (Block >= 0 && Block < ShakeBase && Id <= Param::Rotation ? Cameras::LocalFromPath(Target.Camera.Blocks[Block], KeyTime) : KeyTime);
}

inline float OwnerLength(const Object& Target, s32 Block)
{
	return Block < 0 ? Target.Span.Length : Block >= IkBase ? Target.Actor.Clips[Block - IkBase].Length : Block >= ClipBase ? Target.Actor.Clips[ClipIndex(Block)].Length : Block >= ShakeBase ? Target.Camera.Shakes[Block - ShakeBase].Length : Target.Camera.Blocks[Block].Length;
}

inline u8 ShakeRows(const CameraData& Data)
{
	u8 Rows = 0;
	for (const ShakeClip& Clip : Data.Shakes)
	{
		Rows = std::max(Rows, static_cast<u8>(Clip.Row + 1));
	}
	return Rows;
}

inline s32 CameraOwnerAt(const Object& Target, Param Id, u8 Layer, float Local)
{
	const bool IsShake = Id >= Param::ShakeInfluence;
	s32 Found = -1;
	for (u32 Index = 0; IsShake && Index < Target.Camera.Shakes.size(); ++Index)
	{
		const ShakeClip& Clip = Target.Camera.Shakes[Index];
		const bool IsInside = Clip.Row == Layer && Local >= Clip.Start && Local <= Clip.Start + Clip.Length;
		Found = IsInside && (Found < 0 || Clip.Start >= Target.Camera.Shakes[Found - ShakeBase].Start) ? ShakeBase + static_cast<s32>(Index) : Found;
	}
	return IsShake ? Found : Cameras::BlockAt(Target.Camera, Local);

}

inline float GameFov()
{
	return std::clamp(g_fov, 1.0f, 179.0f);
}

enum class ModalKind : u8
{
	None,
	Open,
	Import,
	SaveAs,
	Discard,
	Message,
	SavePreset,
	DeletePreset,
	ShiftTime,
	Render,
	SaveMotion
};

struct RenderJob
{
	double Begin = 0.0;
	double Rate = 30.0;
	u32 Count = 0;
	u32 Index = 0;
	float Aspect = 0.0f;
	bool IsActive = false;
	bool HasRequest = false;
	bool WasLooping = false;
	bool IsVideo = false;
	bool IsPng = false;
	u32 Warm = 0;
	u32 WarmIndex = u32(-1);
	string_path Folder = {};
	string_path Video = {};
	string_path Name = {};
};

enum class PendingKind : u8
{
	None,
	New,
	Open,
	Import
};

enum class DragKind : u8
{
	None,
	Scrub,
	LoopRange,
	LoopMove,
	LoopIn,
	LoopOut,
	MarkerMove,
	RowMove,
	ClipMove,
	ClipLength,
	BlockMove,
	BlockIn,
	BlockOut,
	BlockBlend,
	BlockLength,
	BlockStretch,
	PoseKey,
	BlockFadeIn,
	BlockFadeOut,
	ShakeMove,
	ShakeStart,
	ShakeEnd,
	ShakeFadeIn,
	ShakeFadeOut,
	LaneKeys,
	LaneSegment,
	LaneBend,
	HandleIn,
	HandleOut,
	BoxClips,
	BoxKeys
};

enum class ClipPart : u8
{
	None,
	Body,
	Start,
	End,
	FadeIn,
	FadeOut,
	Cross,
	Key
};

enum class Cover : u8
{
	Keep,
	Remove,
	Split,
	CutTail,
	CutHead
};

enum class ChooserKind : u8
{
	Model,
	Item,
	Animation,
	Pack,
	Preset,
	Effect,
	Texture,
	Shader,
	Sound
};

inline constexpr const char* AddChoices[4] = { "Actor...", "Actor from preset...", "Particle...", "Decal..." };
inline constexpr ChooserKind AddKinds[4] = { ChooserKind::Model, ChooserKind::Preset, ChooserKind::Effect, ChooserKind::Texture };

inline ObjectType AddedType(ChooserKind Kind)
{
	return Kind == ChooserKind::Effect ? ObjectType::Particle : Kind == ChooserKind::Texture ? ObjectType::Decal : ObjectType::Actor;
}

enum class KeyState : u8
{
	None,
	Static,
	Animated,
	OnKey
};

enum class RowAction : u8
{
	None,
	Reset,
	RemoveAnimation
};

enum class FocusKind : u8
{
	Objects,
	Block,
	Keys
};

enum class WriteKind : u8
{
	Static,
	OnKey,
	Offset,
	Insert,
	Record
};

enum class GizmoMode : u8
{
	Translate,
	Rotate,
	Scale,
	Range,
	Universal
};

struct GizmoItem
{
	u32 Id = 0;
	u32 Leader = 0;
	s16 Block = -1;
	u16 Written = 0;
	Fvector Values[3] = {};
	Fmatrix Start = Fidentity;
	LinkFrames Frames;
};

struct WriteLatch
{
	u32 ObjectId = 0;
	s16 Block = -1;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Component = 0;
	WriteKind Kind = WriteKind::Static;
	u32 KeyIndex = 0;
	float Begin = 0.0f;
	float Last = 0.0f;
	float Value = 0.0f;
	float Start = 0.0f;
};

struct RowItem
{
	u32 Id = 0;
	bool IsFolder = false;
	u8 Depth = 0;
	u32 Order = 0;
	float Y = 0.0f;
	float Height = 0.0f;
	float Track = 0.0f;
};

struct KeyRef
{
	u32 ObjectId = 0;
	s16 Block = -1;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Component = 0;
	u32 Index = 0;
	auto operator<=>(const KeyRef&) const = default;
};

inline constexpr u32 RigControlCount = 28, RigChainCount = 4, RigFirstFinger = 18;
inline constexpr const char* RigControlKeys[RigControlCount] = { "hips", "chest", "head", "hand_l", "hand_r", "foot_l", "foot_r", "eye_l", "eye_r", "jaw", "root", "spine", "spine1", "neck", "clavicle_l", "clavicle_r", "toe_l", "toe_r", "finger0_l", "finger1_l", "finger2_l", "finger3_l", "finger4_l", "finger0_r", "finger1_r", "finger2_r", "finger3_r", "finger4_r" };
inline constexpr u8 RigControlSides[RigControlCount] = { 0, 0, 0, 1, 2, 1, 2, 1, 2, 0, 0, 0, 0, 0, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 };
inline constexpr u8 RigControlShapes[RigControlCount] = { 0, 16, 16, 4, 4, 4, 4, 16, 16, 16, 0, 16, 16, 16, 16, 16, 4, 4, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16 };
inline constexpr float RigControlSizes[RigControlCount] = { 10.0f, 10.0f, 10.0f, 10.0f, 10.0f, 10.0f, 10.0f, 5.0f, 5.0f, 5.0f, 15.0f, 8.0f, 8.0f, 8.0f, 7.0f, 7.0f, 6.0f, 6.0f, 4.0f, 4.0f, 4.0f, 4.0f, 4.0f, 4.0f, 4.0f, 4.0f, 4.0f, 4.0f };
inline constexpr const char* RigChainKeys[RigChainCount] = { "arm_l", "arm_r", "leg_l", "leg_r" };

struct ActorRig
{
	u16 Controls[RigControlCount] = { BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE, BI_NONE };
	u16 Chains[RigChainCount][3] = { { BI_NONE, BI_NONE, BI_NONE }, { BI_NONE, BI_NONE, BI_NONE }, { BI_NONE, BI_NONE, BI_NONE }, { BI_NONE, BI_NONE, BI_NONE } };
	xr_vector<u16> Mirror;
	bool HasOverride = false;
};

struct PoseValue
{
	shared_str Bone;
	Fvector Turn = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Shift = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Size = Fvector().set(1.0f, 1.0f, 1.0f);
};

struct NamedPose
{
	xr_string Name;
	xr_vector<PoseValue> Bones;
};

inline constexpr u32 MaxNamedPoses = 256;

struct LibraryFolder
{
	xr_string Name;
	xr_vector<xr_string> Items;
};

struct LibraryShelf
{
	xr_vector<xr_string> Favourites;
	xr_vector<LibraryFolder> Folders;
};

struct RecordSample
{
	float Time = 0.0f;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	float Fov = 0.0f;
};

struct LaneSlot
{
	u32 ObjectId = 0;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Mask = 7;
	u8 Shown = 7;
	bool IsFitted = false;
	float Lo[3] = { 0.0f, 0.0f, 0.0f };
	float Hi[3] = { 1.0f, 1.0f, 1.0f };
};

struct CurveTarget
{
	u32 ObjectId = 0;
	s16 Block = -1;
	Param Id = Param::Count;
	u8 Layer = 0;
	u8 Mask = 1;
	float Local = 0.0f;
	float Amount = 1.0f;
};

struct GraphCurve
{
	KeyRef Ref;
	float Start = 0.0f;
	float Lo = 0.0f, Hi = 1.0f;
	u32 Color = 0;
};

struct GraphBuffered
{
	KeyRef Ref;
	Curve Shape;
};

struct GraphKeyStart
{
	KeyRef Ref;
	float Time = 0.0f, Value = 0.0f;
};

struct LaneTarget
{
	s16 Block = -1;
	float Start = 0.0f;
	float Length = 0.0f;
};

struct ShakePin
{
	s32 Block = -1;
	float Offset = 0.0f;
	float Length = 0.0f;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;
	u8 Row = 0;
};

struct LaneDrag
{
	KeyRef Ref;
	float Span = 1.0f;
	Curve Base;
	Curve* Live = nullptr;
};

struct KeyMove
{
	u32 Source = 0;
	u32 Index = 0;
};

enum class ClipKind : u8
{
	None,
	Objects,
	ActorBlocks,
	CameraBlocks,
	Shakes,
	Keys
};

struct ClipKey
{
	u32 ObjectId = 0;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Component = 0;
	Key Item;
};

struct Clipboard
{
	ClipKind Kind = ClipKind::None;
	ObjectType Owner = ObjectType::Actor;
	u32 Generation = 0;
	float Origin = 0.0f;
	float Extent = 0.0f;
	xr_vector<Object> Objects;
	xr_vector<ActorBlock> Blocks;
	xr_vector<shared_str> Packs;
	xr_vector<CameraBlock> Shots;
	xr_vector<ShakeClip> Shakes;
	xr_vector<ClipKey> Keys;
};

struct EditorSettings
{
	bool IsSnapping = true;
	u8 SnapStep = 0;
	float FlySpeed = 4.0f;
	float MoveStep = 0.1f;
	float RotateStep = 5.0f;
	float ScaleStep = 0.1f;
	float RangeStep = 0.1f;
	bool ShowHelpers = true;
	bool ShowHints = true;
	bool IsGizmoSnapping = true;
	bool DofPreview = true;
	bool IsFollowing = true;
	bool ShowInspector = true;
	bool ShowPresets = false;
	bool ShowShots = false;
	bool ShowGraph = false;
	bool ShowOnion = false;
	bool ShowPaths = false;
	bool ShowSafe = false;
	bool ShowThirds = false;
	float FrameRatio = 0.0f;
	float FrameMask = 0.5f;
	float OnionCount = 2.0f;
	float OnionStep = 5.0f;
	float HeaderWidth = 220.0f;
	float Opacity = 0.5f;
	float LookSensitivity = 1.0f;
	float FlySmoothing = 0.08f;
	float RecordSmoothing = 0.1f;
	float RenderWarmup = 8.0f;
	float DefaultRow = 1.0f;
	float DefaultLane = 1.0f;
	bool operator==(const EditorSettings&) const = default;
};

inline constexpr float MinHeaderWidth = 140.0f, MaxHeaderWidth = 600.0f;
inline ActorLayer NewRow(const ActorRuntime* Runtime)
{
	ActorLayer Row;
	for (u16 Part = 0; Runtime && Part < MAX_PARTS; ++Part)
	{
		if (const char* Name = Runtime->PartName(Part))
		{
			Row.Parts.push_back(Name);
		}
	}
	return Row;
}

inline constexpr const char* FrozenClips = "The model or an animation of this actor is missing or still loading; its clips cannot be edited.";

class Editor
{
public:
	Editor();
	void Draw(Player& Core, bool& IsOpen);
	void DrawHidden(const Player& Core);
	void Close(Player& Core);
	float Opacity() const { return Settings.Opacity; }
	bool WasOpen = false;

private:
	void BeginFrame(Player& Core);
	void EndFrame(Player& Core);
	void EndStep(Player& Core);
	void ResetScene(bool IsSaved);
	void DrawMenuBar(Player& Core);
	void DrawModals(Player& Core);
	void RequestScene(Player& Core, PendingKind Kind, bool IsConfirmed);
	void ShowModal(const Player& Core, ModalKind Kind);
	void CheckName();
	void OpenScene(Player& Core, const char* Path, const xr_string& Name, bool IsNamed);
	void Save(Player& Core);
	void SaveScene(Player& Core, const xr_string& File);
	bool SaveClip(Player& Core, u32 Id, u32 Index);
	void SaveLibrary(Player& Core);
	void OpenClip(Player& Core, u32 Id, const char* Name);
	void ImportClip(Player& Core, u32 Id, u8 Layer, u32 Block);
	void DrawRetarget(Player& Core);
	void BuildHudItems();
	void DrawActorPicker(Player& Core);
	void SyncHudLinks(Player& Core);
	void UpdateHudView(Player& Core);
	void AddHud(Player& Core, const char* Item, float Start);
	void AddHudMotion(Player& Core, u32 Id, const char* Alias);
	const Object* HudHands(const Player& Core) const;
	void BuildRetargetMap(Player& Core);
	void SaveRetargetMap(Player& Core);
	void RetargetClip(Player& Core);
	void Message(const char* Title, xr_vector<xr_string> Lines);
	void SetStatus(bool IsError, const char* Format, ...);
	void HandleShortcuts(Player& Core);
	void UndoRedo(Player& Core, bool IsRedo);
	void LoadSettings();
	void SaveSettings();
	void UpdateFly(Player& Core);
	void CommitRecording(Player& Core);
	void UseEditorCamera(Player& Core);
	void DrawHelpers(const Player& Core) const;
	void DrawGhosts(Player& Core);
	void DrawFrameLines(const Player& Core);
	void DrawBones(Player& Core);
	void DrawBoneGizmo(Player& Core);
	void DrawIkHandles(Player& Core);
	void DrawPathKeys(Player& Core);
	void EvenSpeed(Player& Core, s32 Block);
	void SetIk(Player& Core, const Object& Target, u32 Limb, bool IsOn);
	void SwitchIkSpace(Player& Core, const Object& Target, u32 Limb);
	void CaptureConstraint(Player& Core, u32 Id, u32 Slot, bool IsSnap);
	void DrawConstraints(Player& Core, const Object& Target);
	bool PickBone(Player& Core, bool IsToggle);
	s32 BoneUnderMouse(const Player& Core) const;
	void BuildRig(const ActorRuntime& Runtime, const shared_str& Visual);
	void RigPath(string_path& Path) const;
	void SaveRig();
	void DrawRigSection(const ActorRuntime& Runtime);
	void DrawBoneValues(Player& Core, const Object& Target);
	bool DrawAttachSwitches(Player& Core, const Object& Target);
	void AddAttachSwitch(Player& Core, u32 Id, float Local);
	void SelectSet(u32 Set, bool IsAdding);
	bool HasRigControls() const { return RigObject == PoseObject && std::any_of(Rig.Controls, Rig.Controls + RigControlCount, [](u16 Bone) { return Bone != BI_NONE; }); }
	s32 EnsurePoseBone(Object& Item, const shared_str& Bone);
	s32 PoseBlock(const Object& Item) const { return AnimClipIndex >= 0 && AnimObject == Item.Id && AnimClipIndex < static_cast<s32>(Item.Actor.Clips.size()) && AnimLayer <= Item.Actor.Clips[AnimClipIndex].Layers.size() ? ClipBase + static_cast<s32>(AnimLayer) * ClipSpan + AnimClipIndex : -1; }
	float PoseLocal(const Player& Core, const Object& Item) const;
	s32 IkBlock(const Object& Item) const { return PoseBlock(Item) >= 0 ? IkBase + AnimClipIndex : -1; }
	void DrawAnimatorBody(Player& Core);
	void SetWorkspace(bool IsAnimation);
	ImGuiCond LayoutCond() const { return LayoutReset ? ImGuiCond_Always : ImGuiCond_FirstUseEver; }
	void DrawAnimatorKeys(Player& Core, const Object& Target, s32 Block, const ActorBlock* Stock);
	void OpenAnimator(Player& Core, u32 Id, s32 Clip, bool ShouldFocus = true);
	void NewClip(Player& Core, u32 Id);
	void PlaceClip(ActorData& Setup, const shared_str& Name, const ActorRuntime* Runtime);
	void SeekAnimator(Player& Core, const Object& Target, float Time);
	void UpdateAnimator(Player& Core);
	void DeleteAnimKeys(Player& Core);
	void SetAnimMode(Player& Core, Interp Mode);
	void FitAnimKeys(Player& Core);
	void MoveAnimKeys(Player& Core, float Shift);
	void RemoveAnimBone(Player& Core, u8 Bone);
	ClipTrack* EditClip(Player& Core, const char* Label);
	void DrawPoseSection(Player& Core, const Object& Target);
	void KeyBones(Player& Core);
	void ResetBones(Player& Core);
	bool ReadPoseValue(const Object& Item, u16 Bone, PoseValue& Out) const;
	void WritePoses(Player& Core, const char* Label, const xr_vector<PoseValue>& Values, bool IsMirrored);
	void MirrorBones(Player& Core, bool IsSwap);
	void DrawPoseLibrary(Player& Core);
	void LoadPoseLibrary();
	void SavePoseLibrary();
	void RemovePoseRow(Player& Core, u32 Id, u32 Row);
	float FrameAspect() const;
	void HorizontalFovRow(Player& Core, const xr_vector<u32>& Ids, s32 Block);
	void DrawRenderModal(Player& Core);
	void DrawMotionModal(Player& Core);
	void StartRender(Player& Core);
	void StepRender(Player& Core);
	void StopRender(Player& Core, const char* Problem);
	s32 HelperBlock(const Player& Core, const Object& Rig) const;
	bool IsViewportFree() const;
	void CollectGizmo(const Player& Core, bool HasCamera, GizmoMode Mode = GizmoMode::Translate);
	bool HasSelectedParent(const Scene& Data, u32 Parent, GizmoMode Mode) const;
	void PutTransform(Player& Core, GizmoItem& Item, u32 Kind, const Fvector& NewValue);
	void PutWorld(Player& Core, GizmoItem& Item, Fmatrix World, u32 Kinds);
	void DrawGizmo(Player& Core);
	void DrawRangeHandle(Player& Core);
	void DrawTangents(Player& Core);
	void DrawViewCube(Player& Core);
	void Pick(Player& Core);
	void DrawViewportPopup(Player& Core);
	static Fvector FacingRotation(const Fvector& Position, const Fvector* Normal);

	Object* Edit(Player& Core, u32 Id, const char* Label);
	Object* CreateObject(Player& Core, ObjectType Type, float Start);
	Object* AddObject(Player& Core, ObjectType Type, const shared_str& Source, float Start, const Fvector& Position, const Fvector& Rotation);
	void AddLight(Player& Core, float Start, const Fvector& Position);
	void AddWeather(Player& Core, float Start);
	void AddSound(Player& Core, float Start);
	void AddPost(Player& Core, float Start);
	void LoadPresets();
	bool StorePresets(xr_vector<ActorPreset>&& Next, const xr_string& Removed);
	s32 FindPreset(const char* Name) const;
	void ApplyPreset(Player& Core, const xr_vector<u32>& Ids, const char* Name);
	void AddActorFromPreset(Player& Core, const char* Name, float Start, const Fvector& Position, const Fvector& Rotation);
	static void SpawnPose(Fvector& Position, Fvector& Rotation);
	static bool StaticRay(const Fvector& Origin, const Fvector& Direction, float Range, Fvector& Point, Fvector& Normal);
	void Refit(Player& Core, const Object& Target);
	void PoseToClip(Player& Core, u32 Id, u32 Row);
	void RemoveBlock(Player& Core, u32 Id, s32 Block, u8 Layer = 0);
	void AddLayer(Player& Core, u32 Id);
	void RemoveLayer(Player& Core, u32 Id, u8 Layer);
	void SetRowMode(Player& Core, u32 Id, u8 Row, bool IsAdd);
	void AddAnimationAt(Player& Core, u32 Id, u8 Row, float Time, const shared_str& Source, const shared_str& Name);
	void DrawLookSection(Player& Core, const Object& Target);
	bool PartsCombo(Player& Core, const Object& Target, u8 Layer, float Width);
	void AddFolder(Player& Core);
	void AddCameraMove(Player& Core, float Time, const PoseKey* Pose = nullptr);
	void AddShake(Player& Core, float Time, s32 Row);
	void PinShakes(const Object& Rig);
	void FollowShakes(Object& Rig, s32 Stretched = -1, float Factor = 1.0f);
	void Crossfade(Player& Core, s32 Block, bool IsOn);
	bool InsertShots(Player& Core, float Time, xr_vector<CameraBlock>& Shots, bool ShouldFit);
	bool IsClipFree(const CameraData& Data, s32 Skip, float Start, float Length);
	float FitClip(const CameraData& Data, s32 Skip, float Start, float End, u8 Edge);
	void TrimHead(CameraBlock& Shot, const CameraBlock& Base, float Delta) const;
	s32 SortShot(Object& Rig, s32 Block);
	void CaptureStarts(Scene& Data, bool IsLength);
	void AddMarker(Player& Core, float Time);
	void RemoveMarker(Player& Core, s32 Index);
	void ShiftTime(Player& Core, float At, float Amount);
	bool Copy(Player& Core, Clipboard& Out);
	void Paste(Player& Core, const Clipboard& Source, float Time);
	void PasteObjects(Player& Core, const Clipboard& Source, float Time);
	void PasteBlocks(Player& Core, const Clipboard& Source, u32 OnlyId);
	void PasteKeys(Player& Core, const Clipboard& Source, float Time, bool IsSameOwners);
	void Relink(Scene& Data, u32 Owner, const Attachment& Wanted, Attachment& Out, bool IsSameScene);
	void Duplicate(Player& Core);
	void KeyFromView(Player& Core, float Time, bool IsEye);
	void ViewPose(const Player& Core, bool IsEye, PoseKey& Pose) const;
	const BlockCache* PoseAt(const Player& Core, const CameraBlock& Shot, s32 Block, float Local, PoseKey& Pose, u32& At) const;
	void PutPose(CameraBlock& Shot, const BlockCache& Cache, float Local, float HalfFrame, PoseKey Pose);
	static void ToSpace(const LinkFrames& Frames, PoseKey& Pose);
	static u32 PoseKeyAt(const CameraBlock& Shot, const BlockCache& Cache, float Local, float HalfFrame);
	void RemovePose(Player& Core, s32 Block, u32 Index);
	static void UseGameFov(Object& Target, s32 Block, Param Id);
	void SetMuted(Player& Core, u32 Id, bool IsMuted);
	void ResetLength(Player& Core);
	void DeleteSelection(Player& Core);
	void Select(u32 Id, bool IsCtrl, bool IsShift);
	bool IsSelected(u32 Id) const;
	void PruneSelection(const Scene& Data);
	void StartRename(const Scene& Data, u32 Id, bool IsFolder);
	void CommitRename(Player& Core);
	static Folder* FindFolder(Scene& Data, u32 Id);
	static u32 NextRow(const Scene& Data, u32 FolderId);

	void DrawTimeline(Player& Core, bool& IsOpen);
	void DrawTransport(Player& Core);
	void BuildRows(const Scene& Data);
	void DrawRuler(Player& Core, const ImRect& Full);
	void DrawMarkers(const Player& Core, const ImRect& Track, bool IsRuler) const;
	s32 MarkerAt(const SceneHeader& Header, float X) const;
	void DrawRowHeader(Player& Core, const RowItem& Row, const ImRect& Rect);
	void HandleTrack(Player& Core, const Object* Target);
	void DrawClip(const Player& Core, const Object& Target, const ImRect& Rect);
	void DrawActorBlocks(const Object& Target, const ActorRuntime& Runtime, const ImRect& Rect, u8 Layer, u32 Fill);
	s32 BlockHit(const Object& Target, const ActorRuntime& Runtime, u8 Layer, const ImVec2& Mouse, const ImRect& Row, DragKind& Kind, s32& Hit, double& Rep, ClipPart& Part, ImRect& Area) const;
	void ShowPart(ClipPart Part, const ImRect& Area, u32 Color, u32 Type);
	void PressBlock(Player& Core, const Object& Target, const ActorRuntime& Runtime, u8 Layer, DragKind Kind, s32 Hit, double Rep);
	void DrawLayerRow(Player& Core, const Object& Target, u8 Layer, const ImRect& Header, const ImRect& Track);
	u8 LayerAt(const Scene& Data, const Object& Target, float Y) const;
	void DrawPoseRow(Player& Core, const Object& Target, u8 Row, const ImRect& Header, const ImRect& Track);
	void DrawCameraBlocks(const Player& Core, const Object& Target, const ImRect& Rect);
	u8 SubRows(const Object& Target) const;
	void DrawSubRow(Player& Core, const Object& Target, u8 Sub, const ImRect& Header, const ImRect& Track);
	void DrawShakeRow(Player& Core, const Object& Rig, u8 Row, const ImRect& Header, const ImRect& Track);
	void DragShake(Player& Core);
	void MoveShake(Player& Core, const Object& Found);
	static void TrimShake(ShakeClip& Clip, float Trim, float Rate);
	void DrawShakeMenu(Player& Core);
	s32 CameraHit(const Player& Core, const Object& Target, const ImVec2& Mouse, float Top, float Bottom, DragKind& Kind, s32& Pose, ClipPart& Part, ImRect& Area) const;
	void DragCameraBlock(Player& Core, const Object& Target);
	void MoveShot(Player& Core, const Object& Target);
	bool CutShot(Player& Core, s32 Block, float From, float To, bool ShouldApply);
	void DrawPoseMenu(Player& Core);
	void DrawTimelinePopups(Player& Core);
	void Press(Player& Core, DragKind Kind, u32 Id, bool IsFolder);
	void OpenRowMenu(u32 Id, bool IsFolder);
	void UpdateDrag(Player& Core);
	void StartBox(DragKind Kind);
	void UpdateBox(Player& Core);
	void StartClipDrag(Player& Core);
	void ShiftClips(Player& Core, float Offset);
	void DropRow(Player& Core);
	s32 SpanAt(const Object& Target, const ActorRuntime& Runtime, float Local, double& Rep, float& Sequence, u8 Layer = 0) const;
	float SequenceToX(const Object& Target, const ActorRuntime& Runtime, double Rep, float Sequence) const;
	double AreaAt(const Object& Target, const ActorRuntime& Runtime, float Time) const;
	float TimeAt(const Object& Target, const ActorRuntime& Runtime, double Area) const;
	void PlaceBlocks(const Object& Target, const ActorRuntime& Runtime, xr_vector<ActorBlock>& Blocks, const xr_vector<float>& Starts) const;
	bool IsTrackValid(const Object& Target, const ActorRuntime& Runtime, const xr_vector<ActorBlock>& Blocks) const;
	bool RemoveCrossfade(Player& Core, u32 Id, u8 Layer, s32 Block, bool ShouldApply);
	u8 PlaceRow(Object& Item, const ActorRuntime& Runtime, u8 Row, const ActorBlock& Block, bool IsStrict, bool IsSearching);
	u8 Overwrite(Object& Item, const ActorRuntime& Runtime, u8 Row, const ActorBlock& Moved, float Frame);
	void DragBlockEdge(Player& Core);
	void DragActorBlock(Player& Core, const Object& Target, const ActorRuntime& Runtime);
	bool SplitBlock(Player& Core, u32 Id, s32 Block, bool ShouldApply, u8 Layer = 0);
	bool CutSequence(Player& Core, u32 Id, float From, float To, bool ShouldApply, u8 Layer = 0, s32 Only = -1);
	void CollectSnapTargets(const Player& Core, bool HasPlayhead);
	bool Magnet(float Time, float& Offset) const;
	float SnapTime(float Time, bool IsShift) const;
	float SnapSpan(float Start, float Length, bool IsShift) const;
	float TimeToX(float Time) const;
	float XToTime(float X) const;
	float LanesHeight(const Scene& Data, u32 Id) const;
	float SizeOf(const Scene& Data, u32 ObjectId, u8 Kind, Param Id, u8 Layer) const;
	void SizeEdge(Player& Core, const RowSize& Key, float Left, float Right, float Top, float Bottom);
	void SetTangentKey(const Player& Core, s32 Block, s32 Key);
	u8 SubAt(const Scene& Data, const Object& Target, float Y) const;
	LaneSlot* FindSlot(u32 ObjectId, Param Id, u8 Layer);
	void CollectLaneTargets(const Object& Target);
	void DrawLanes(Player& Core, const Object& Target, float Left, float Top);
	void DrawLaneHeader(Player& Core, const Object& Target, LaneSlot& Slot, s32 Component, const ImRect& Rect, bool& ShouldRemove);
	void DrawLane(Player& Core, const Object& Target, LaneSlot& Slot, s32 Component, const ImRect& Rect);
	void DrawKeyTicks(ImDrawList* List, const xr_vector<Lane>& Lanes, float Start, float Y, u32 Color, const CameraBlock* Path = nullptr) const;
	void SetLooping(Player& Core, bool IsOn);
	void LoopSelection(Player& Core);
	void FitLane(const Object& Target, LaneSlot& Slot);
	void AddLaneKey(Player& Core, const Object& Target, const LaneSlot& Slot, s32 Component, float Time, float NewValue);
	void GroupKeys(const Object& Target, s32 Block, const LaneSlot& Slot, s32 Component, const KeyRef& Hit, float HalfFrame);
	void SelectKeys(bool IsToggle);
	bool IsKeySelected(const KeyRef& Ref) const;
	void DragLane(Player& Core);
	void BeginKeyMove(Player& Core, const char* Label);
	void MoveKeys(Player& Core, float Shift, float Rise);
	void DrawKeyMenu(Player& Core);
	void DrawCurveMenu(Player& Core);
	void RemoveKeys(Player& Core);
	template <typename TFunc>
	void ForSelectedKeys(Player& Core, const char* Label, const TFunc& Visit);
	template <typename TFunc>
	void ForMenuCurves(Player& Core, const char* Label, const TFunc& Visit);
	static void RemoveLane(Object& Target, s32 Block, Param Id, u8 Layer, float Local);

	void DrawInspector(Player& Core);
	void DrawPresets();
	void DrawShots(Player& Core);
	void DrawGraph(Player& Core);
	void FrameGraph(Player& Core, bool IsSelection);
	const GraphCurve* GraphCurveOf(const KeyRef& Ref) const;
	bool IsGraphHidden(const KeyRef& Ref) const;
	void SnapGraphKeys(Player& Core);
	void PlaceGraphKeys(Player& Core, float Shift, float Rise, bool IsView, float Factor, float Pivot);
	void SetGraphTangents(Player& Core, u8 Kind);
	template <typename TVisit>
	void ForGraphCurves(Player& Core, const char* Label, const TVisit& Visit);
	void DrawHeader(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawSceneSection(Player& Core);
	void DrawActorSection(Player& Core, const xr_vector<u32>& Ids);
	void DrawBlockSection(Player& Core, const Object& Target);
	void DrawLightSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawParticleSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawDecalSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawSoundSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawWeatherSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawPostSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void ClipRow(Player& Core, const xr_vector<u32>& Ids);
	bool GroupHeader(Player& Core, const xr_vector<u32>& Ids, const char* Title, const char* Label, bool LightData::*Field);
	void PickColor(Player& Core);
	void PickFocus(Player& Core);
	void DrawCameraSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawShake(Player& Core, const xr_vector<u32>& Ids, s32 Owner);
	void ParamRow(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id, u8 Layer, float Speed, const char* Format, u8 ResetMask, float Default);
	void KeyRow(Player& Core, const xr_vector<u32>& Ids);
	void DrawAttach(Player& Core, const xr_vector<u32>& Ids, s32 Block);
	bool AttachCombo(const Player& Core, const char* Label, u32 Owner, AttachLink& Link);
	bool BoneCombo(const Player& Core, const char* Label, AttachLink& Link);
	bool CanAttach(const Scene& Data, u32 From, u32 To);
	void DrawToolbar(Player& Core);
	RowAction BeginRow(const char* Name, bool IsOverridden, bool HasLane);
	bool EndRow(KeyState Mark);
	bool TimeField(const char* Label, float& Seconds, float Lo, float Hi, u32 Rate);
	bool OffsetField(const char* Label, float& Seconds, u32 Rate);
	s32 FocusedBlock(const Player& Core, const Object& Target, u8* Layer = nullptr) const;
	void BuildLibrary();
	bool Chooser(Player& Core, ChooserKind Kind, const char* Label, const char* Preview, const xr_vector<u32>& Owners, shared_str& Source, shared_str& Name);
	bool ChooserList(Player& Core, ChooserKind Kind, const xr_vector<u32>& Owners, shared_str& Source, shared_str& Name);
	void ReadLibrary();
	void WriteLibrary();
	void PreviewTexture(const shared_str& Texture);
	void ReleasePreview();
	void ToggleKey(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id, u8 Layer, bool IsAdding = false);
	void KeyPlayhead(Player& Core);
	void PlayPause(Player& Core, bool IsContinue);
	void SeekBoth(Player& Core, float Time);
	bool SelectionSpan(const Player& Core, float& Lo, float& Hi) const;
	void ZoomTo(float Lo, float Hi);
	void ZoomAround(float Factor, float Pivot);
	void HoverLane(u32 ObjectId, const LaneSlot& Slot);


	void WriteParam(Player& Core, Object& Target, s32 Block, Param Id, u8 Layer, u8 Component, float NewValue, bool IsTotal = false);
	void Record(Player& Core, WriteLatch& Latch);

	History Undo;
	xr_vector<Fvector> GhostLines, PathPoints;
	xr_vector<u32> GhostSpans;
	float GhostTime = -1.0f;
	u64 GhostSerial = 0;
	u64 SavedSerial = 0;
	EditorSettings Settings, SavedSettings;
	u32 SeenGeneration = 0;
	xr_string SceneName;
	xr_string SceneFile;
	ModalKind Modal = ModalKind::None;
	PendingKind Pending = PendingKind::None;
	char NameBuffer[256] = {};
	xr_string NameFile;
	xr_string NameError;
	bool IsNameTaken = false;
	xr_vector<xr_string> FileList;
	s32 FileSelected = -1;
	bool HasUnsavedFile = false;
	xr_string MessageTitle;
	xr_vector<xr_string> MessageLines;
	xr_string StatusText;
	bool IsStatusError = false;
	bool IsOwnWindowFocused = false;
	xr_vector<GraphCurve> GraphCurves;
	xr_vector<KeyRef> GraphHidden;
	xr_vector<GraphBuffered> GraphBuffer;
	xr_vector<GraphKeyStart> GraphStart;
	float GraphView[4] = { 0.0f, 10.0f, -1.0f, 1.0f };
	float GraphField = 0.0f;
	ImVec2 GraphPress;
	ImRect GraphBox, GraphBounds;
	u8 GraphDrag = 0, GraphTipSide = 0;
	KeyRef GraphTip;
	bool IsGraphNormalized = false, IsGraphFocused = false;

	xr_vector<u32> Selected;
	u32 Anchor = 0, SelectedFolder = 0;
	u32 RenameId = 0;
	bool IsRenameFolder = false;
	u8 RenameFrames = 0;
	char RenameBuffer[MaxNameBytes + 1] = {};
	char ShotBuffer[MaxNameBytes + 1] = {};
	s32 ShotRow = -1, ShotRename = -1;
	bool IsShotMarker = false;

	float Zoom = 60.0f, ScrollTime = 0.0f, ZoomAnchor[2] = {};
	u8 RulerMode = 0;
	bool IsFollowPaused = false;
	ImRect Marquee;
	double AddedKeyTime = -1.0;
	xr_vector<RowSize> SizesBefore;
	float SizeGrab = 0.0f;
	bool WasLayoutDirty = false, IsSizeCancelled = false;
	float SizeTop = 0.0f;
	bool IsLayoutDirty = false;
	float TrackLeft = 0.0f, TrackWidth = 1.0f, RowsTop = 0.0f, RowHeight = 1.0f, SnapGrid = 1.0f / 30.0f;
	xr_vector<RowItem> Rows, RowScratch, MemberScratch;
	xr_vector<u32> Collapsed;
	DragKind Drag = DragKind::None;
	float DragTime = 0.0f;
	u32 DragObject = 0;
	bool IsDragFolder = false, IsDragCopy = false, HasDragMoved = false, WasPressSelected = false;
	float DragLoop[2] = {};
	float OverviewWhole = 1.0f;
	xr_vector<u32> DragIds, DragPrevious;
	xr_vector<float> DragStarts, SnapTargets;
	u32 PopupRow = 0;
	bool IsPopupFolder = false;
	float PopupTime = 0.0f;
	const char* PopupRequest = nullptr;
	s32 DragBlock = -1, PopupBlock = -1, DragPose = -1, PopupPose = -1, HoverBlock = -1, HoverPose = -1;
	u32 HoverLaneObject = 0;
	Param HoverLaneId = Param::Count;
	u8 HoverLaneLayer = 0;
	xr_vector<u32> KeyIds;
	float DragSequence = 0.0f, DragSign = 1.0f;
	bool IsDragSpeed = false;
	xr_vector<ActorBlock> DragBlocks[MaxActorLayers + 1], CutBlocks;
	xr_vector<float> DragBlockStarts[MaxActorLayers + 1], BlockStarts;
	u8 DragLayer = 0, PopupLayer = 0, FocusLayer = 0, DragRows = 1, DragValidLayer = 0;
	float DragValidStart = 0.0f;
	s8 UndoRequest = 0;
	CameraBlock DragShot;
	xr_vector<float> DragClipStarts;
	float DragClipSpan = 0.0f;
	xr_vector<Cameras::ClipSpan> SpanScratch;
	xr_vector<s32> InsertedShots;
	ShakeClip DragShakeStart;
	xr_vector<ShakePin> Pins;
	xr_vector<u8> ShakeMoved;
	s32 DragShakeIndex = -1;
	u8 DragRow = 0, PopupShakeRow = 0;
	bool IsShakeSlide = false, IsPopupCross = false;
	xr_vector<CameraBlock> DragShots;
	xr_vector<ShakeClip> DragShakes;
	xr_vector<ShakePin> DragPins;
	xr_vector<u8> CoverPlan;
	s32 DragShotIndex = -1, DragShakePress = -1, DragPickFocus = -1;
	float DragLimit = -1.0f;
	const char* Hint = nullptr;

	float LastBlend = 0.5f;
	bool IsPopupChoosing = false;
	xr_vector<u32> Expanded;
	xr_vector<LaneSlot> Slots;
	xr_vector<LaneTarget> LaneTargets;
	xr_vector<KeyRef> SelectedKeys, KeyGroup, DragKeyPrevious;
	xr_vector<LaneDrag> DragCurves;
	xr_vector<KeyMove> DragMoves;
	xr_vector<float> DragTimes;
	KeyRef DragKey;
	Key DragKeyStart;
	float DragKeyTime = 0.0f, DragOriginY = 0.0f, DragLaneHeight = 1.0f, DragSpan = 1.0f;
	s32 DragMarker = -1, HoverMarker = -1, PopupMarker = -1, RenameMarker = -1;
	ImGuiID CanvasId = 0;
	Clipboard Copied, Scratch;
	xr_vector<std::pair<u32, u32>> IdMap;
	float OffsetValue = 0.0f;
	bool IsOffsetKeys = false;
	float ShiftAmount = 1.0f;
	RenderJob Job;
	s32 RenderRange = 0, RenderRate = 0, RenderAspect = 0, RenderOutput = 0;
	char RenderName[64] = {};
	s32 MotionRange = 0, MotionSource = 0;
	u32 LookEdit = 0;
	u32 MotionObject = 0, MotionRow = 0;
	bool IsRemovingTime = false;
	xr_vector<ActorPreset> Presets;
	xr_vector<shared_str> PresetNames;
	xr_vector<xr_string> HiddenPresets, OldPresetNames;
	xr_vector<u32> PresetRows;
	ActorPreset PresetDraft;
	bool IsPresetsLoaded = false, IsPresetsReadOnly = false;
	s32 PresetSelected = -1, RenamePreset = -1;
	u8 PresetFrames = 0;
	char PresetBuffer[MaxNameBytes + 1] = {}, PresetFilter[128] = {};
	ChooserKind PopupKind = ChooserKind::Model;

	xr_vector<u32> PinnedIds, InspectActors, InspectCameras, InspectLights, InspectParticles, InspectDecals, InspectSounds, InspectWeathers, InspectPosts, ChooserOwners, PickColorIds;
	xr_vector<shared_str> SoundFiles;
	bool IsPinned = false, IsAutoKey = false, IsPickingColor = false, IsPickingFocus = false;
	s32 PickFocusBlock = -1;
	xr_vector<WriteLatch> Latches;
	AttachGraph AttachCheck;
	xr_vector<const Object*> AttachChoices;
	char BoneFilter[64] = {};
	u32 HighlightObject = 0;
	shared_str HighlightBone;
	u32 FocusObject = 0;
	s32 FocusBlock = -1;
	FocusKind Focus = FocusKind::Objects;
	float RowKeyX = 0.0f, RowWidth = 0.0f;
	char NameEdit[MaxNameBytes + 1] = {};

	xr_vector<shared_str> ModelFiles, ItemSections, OmfFiles, EffectNames, TextureFiles;
	shared_str MenuPack;
	LibraryShelf Library[5];
	u8 Scopes[5] = {};
	u8 FolderAction = 0;
	s32 FolderEdited = -1;
	char FolderName[64] = {};
	xr_string LibraryKey, MenuKey;
	IRHISurface* PreviewSurface = nullptr;
	IRHIShaderResourceView* PreviewView = nullptr;
	shared_str PreviewName;
	xr_vector<u32> Filtered;
	xr_vector<u64> Ranked;
	char Filters[9][128] = {};
	s32 ChooserCursor = 0;
	ChooserKind FilteredKind = ChooserKind::Model;
	u32 FilteredObject = 0;
	size_t FilteredSources = 0;
	bool IsLibraryBuilt = false, IsTexturesBuilt = false, IsFilterDirty = true, IsBrowsingPacks = false, IsChooserOpen = false, IsChooserFresh = false;

	bool IsEditorCamera = false, IsEyeValid = false, IsFlying = false, IsFlyArmed = false, IsPanning = false;
	Fvector Eye = Fvector().set(0.0f, 0.0f, 0.0f);
	float Yaw = 0.0f, Pitch = 0.0f, Roll = 0.0f, EyeFov = 0.0f, FlyFov = 0.0f;
	Fvector ShownEye = Fvector().set(0.0f, 0.0f, 0.0f);
	float ShownYaw = 0.0f, ShownPitch = 0.0f, ShownRoll = 0.0f, ShownFov = 0.0f;
	bool IsGliding = false, HasRecorded = false, IsRecordCancelled = false;
	xr_vector<RecordSample> Recorded, Smoothed;
	xr_vector<u8> Kept;
	xr_vector<std::pair<u32, u32>> RdpSpans;
	CurveTarget CurveMenu;
	Lane CurveClip;
	bool HasCurveClip = false;
	xr_vector<float> CurveScratch;


	shared_str EyeLevel;


	GizmoMode Gizmo = GizmoMode::Universal, GizmoShown = GizmoMode::Universal;
	u32 GizmoPart = 0;
	ImDrawList* Overlay = nullptr;
	bool IsGizmoLocalSingle = true, IsGizmoLocalMulti = false, IsGizmoActive = false, IsGizmoCancelled = false, IsGizmoHovered = false, IsGizmoRelative = false;
	bool IsPickArmed = false, ShouldOpenPopup = false, IsPopupAligned = false, HasPopupHit = false;
	Fmatrix GizmoPivot = Fidentity, GizmoInverse = Fidentity;
	bool IsPosing = false;
	u32 PoseObject = 0, ActivePose = 0;
	struct AnimKey
	{
		u8 Bone = 0;
		float Time = 0.0f;
		s8 Channel = -1;
		bool operator==(const AnimKey&) const = default;
	};
	struct AnimRow
	{
		u16 Bone = u16(-1);
		s8 Channel = -1;
	};
	xr_vector<AnimRow> AnimRows;
	xr_vector<u8> AnimOpen;
	s8 AnimSolo = -1;
	u8 AnimLayer = 0;
	bool IsAnimJump = false;
	bool ShowAnimator = false, IsAnimLinked = true, IsAnimPlaying = false, IsAnimDragging = false, HasAnimMoved = false;
	u32 AnimObject = 0;
	s32 AnimClipIndex = -1;
	float AnimTime = 0.0f, AnimZoom = 0.0f, AnimScroll = 0.0f, AnimGrab = 0.0f, AnimShift = 0.0f;
	AnimKey AnimMenu;
	string64 AnimName = {};
	xr_vector<AnimKey> AnimKeys, AnimBaseKeys, AnimBoxKeys;
	bool IsAnimBoxing = false;
	ImVec2 AnimBoxStart;
	xr_vector<Lane> AnimBase;
	xr_vector<float> AnimTimes, AnimTops, AnimSnaps;
	s32 AnimCurveKey = -1, AnimPickKey = -1;
	u8 AnimPickBone = 0, AnimPickAxis = 0, AnimTipSide = 0;
	u8 AnimCurveBone = 0, AnimCurveAxis = 0, AnimLaneKind = 0;
	float AnimLaneLo = 0.0f, AnimLaneHi = 1.0f;
	xr_vector<shared_str> ClipFiles;
	bool ShowRetarget = false, IsHudView = false;
	string64 HudHandsSection = {}, HudFilter = {};
	xr_vector<shared_str> HudItems, ActorModels;
	bool IsActorModelsListed = false;
	xr_vector<ActorBlock> HudBlocks;
	float HudNear = -1.0f;
	bool ShowBoneLines = false, IsSwitchKeeping = true;
	shared_str AttachSwitchBone;
	bool WorkspaceShown[2][4] = { { true, false, false, false }, { true, false, false, false } };
	bool IsAnimFresh = false;
	s32 AnimStockBlock = -1;
	u8 AnimStockLayer = 0;
	u8 LayoutReset = 0;
	ImRect MainBox;
	xr_vector<u8> SharedBones;
	u32 SharedOwner = 0;
	const ActorRuntime* SharedHands = nullptr;
	bool IsBoneShared(u16 Bone) const { return Bone < SharedBones.size() && SharedBones[Bone]; }
	u32 AnimSeenObject = 0;
	s32 AnimSeenBlock = -1;
	u8 AnimSeenLayer = 0;
	u32 RetargetFrom = 0, RetargetTo = 0, RetargetPair[2] = {};
	s32 RetargetClipIndex = -1;
	float RetargetRatio = 1.0f;
	xr_vector<s32> RetargetMap;
	bool IsAnimFocused = false, IsAnimFollowing = false;
	float AnimRate = 1.0f, AnimLastScene = 0.0f, AnimLastTime = 0.0f, AnimTolerance = 0.5f;
	s32 BoneLayer = -1;
	xr_vector<s32> CurlLayers;
	xr_vector<NamedPose> PoseLibrary;
	xr_vector<PoseValue> PoseScratch;
	bool IsPoseLibraryRead = false;
	char PoseName[64] = {};
	s32 IkHandle = -1;
	bool IsIkActive = false, IsIkCancelled = false, IsIkOver = false, IsIkHover = false, IsIkDirect = false;
	bool IsPathKeyActive = false, IsPathKeyCancelled = false;
	u32 PathKeyObject = 0;
	s32 PathKeyBlock = -1;
	float PathKeyTime = 0.0f;
	Lane PathKeyBase;
	Fvector PathKeyStart = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix IkMatrix = Fidentity, IkGrab = Fidentity, IkGoal = Fidentity;
	xr_vector<u16> SelectedBones;
	ActorRig Rig;
	u32 RigObject = 0;
	shared_str RigVisual;
	ImVec2 BoneBoxStart;
	bool IsBoneBox = false;
	xr_vector<u32> PoseIds;
	string64 PoseBoneFilter = {};
	Fmatrix BonePivot = Fidentity, BoneStart = Fidentity, BoneParentModel = Fidentity;
	Fvector BoneTurn = Fvector().set(0.0f, 0.0f, 0.0f), BoneShift = Fvector().set(0.0f, 0.0f, 0.0f), BoneSize = Fvector().set(1.0f, 1.0f, 1.0f);
	xr_vector<GizmoItem> GizmoItems;
	Fvector RangeCentre = Fvector().set(0.0f, 0.0f, 0.0f), RangeAxis = Fvector().set(1.0f, 0.0f, 0.0f);
	float RangeStart = 0.0f;
	s32 TangentBlock = -1, TangentKey = -1;
	u32 TangentCount = 0;
	float TangentTime = 0.0f;
	u8 DragTangentSide = 0;
	bool IsTangentActive = false, IsTangentCancelled = false, IsHandlePicked = false, IsHandleOver = false, IsBreakPress = false;
	Fmatrix HandleMatrix = Fidentity;
	PoseKey TangentBase;
	Fvector TangentGrab = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector TangentIn = Fvector().set(0.0f, 0.0f, 0.0f), TangentOut = Fvector().set(0.0f, 0.0f, 0.0f), TangentStart = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector PopupPoint = Fvector().set(0.0f, 0.0f, 0.0f), PopupNormal = Fvector().set(0.0f, 1.0f, 0.0f);
	ImRect CubeRect;
	float ViewRight = FLT_MAX;
};
}
