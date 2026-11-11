#pragma once

namespace Cinema
{
inline constexpr u32 FormatVersion = 19;
inline constexpr u32 MaxObjects = 1024, MaxFolders = 256, MaxBlocks = 256, MaxPacks = 64;
inline constexpr u32 MaxLaneKeys = 4096, MaxPoseKeys = 1024, MaxShakes = 32, MaxLayer = 31, MaxLanes = 544, MaxShakeClips = 256, MaxShakeRows = 8, MaxActorLayers = 7, MaxAddRows = 4, MaxPoseRows = 4, MaxPoseBones = 64;
inline constexpr u32 MaxNameBytes = 127, MaxIdentifier = 255, MaxAttachDepth = 8, MaxMarkers = 256, MaxTextureBytes = 239;
inline constexpr float MaxTime = 3600.0f, MinLength = 0.01f, MaxLoops = 1000.0f, MaxCameraBlend = 10.0f;
inline constexpr float MaxPreroll = 10.0f, PrerollStep = 1.0f / 30.0f;

enum class ObjectType : u8
{
	Actor,
	Camera,
	Light,
	Particle,
	Decal,
	Sound,
	Weather,
	Post
};

enum class DofMode : u8
{
	Off,
	Manual,
	Object
};

enum class Interp : u8
{
	Linear,
	EaseIn,
	EaseOut,
	EaseInOut,
	Bezier,
	Hold
};

enum class Wave : u8
{
	None,
	Sine,
	Triangle,
	Square,
	Sawtooth,
	NoiseStepped,
	NoiseSmooth,
	NoiseFractal,
	NoisePerlin,
	NoiseContinuous,
	Oscillator
};

enum class EndMode : u8
{
	Hold,
	Loop,
	PingPong
};

enum class Param : u8
{
	Position,
	Rotation,
	Scale,
	TimeScale,
	Fov,
	Color,
	Intensity,
	Range,
	Cone,
	VirtualSize,
	VolumeIntensity,
	VolumeDistance,
	VolumeQuality,
	Size,
	Speed,
	LayerWeight,
	DofFocus,
	DofOffset,
	DofNear,
	DofFar,
	DofBlur,
	DofAperture,
	DofLength,
	DofPoint,
	DofLimit,
	DofSqueeze,
	DofSpeed,
	DofTransition,
	DofSwirl,
	CameraSmooth,
	PoseWeight,
	BoneRotation,
	BonePosition,
	BoneScale,
	AttachWeight,
	Look,
	IkTarget,
	IkTurn,
	IkPole,
	IkWeight,
	ConstraintWeight,
	DecalWidth,
	DecalHeight,
	SoundVolume,
	SoundPitch,
	WeatherTime,
	SunHeading,
	SunHeight,
	SunColor,
	SunBrightness,
	SunShafts,
	AmbientColor,
	HemiColor,
	FogColor,
	FogDensity,
	FogDistance,
	FarPlane,
	RainDensity,
	WindSpeed,
	WindDirection,
	SkyRotation,
	PostBase,
	PostAdd,
	PostGrayColor,
	PostGray,
	PostBlur,
	PostDualityH,
	PostDualityV,
	PostNoise,
	PostGrain,
	PostNoiseFps,
	PostFade,
	PostFadeColor,
	PostExposure,
	PostBloom,
	PostTonemap,
	PostMiddleGray,
	PostMotionBlur,
	ShakeInfluence,
	ShakePositionScale,
	ShakeSpeed,
	Count
};

struct Key
{
	float Time = 0.0f;
	float Value = 0.0f;
	Interp Mode = Interp::EaseInOut;
	float InTime = 0.0f, InValue = 0.0f;
	float OutTime = 0.0f, OutValue = 0.0f;
	bool IsBroken = false;

	bool operator==(const Key&) const = default;
};

struct Generator
{
	Wave Type = Wave::None;
	float Amplitude = 0.0f, Frequency = 1.0f, Phase = 0.0f;
	float Min = -100000.0f, Max = 100000.0f;

	bool operator==(const Generator&) const = default;
};

struct Curve
{
	xr_vector<Key> Keys;
	Generator Gen;
	u8 Pre = 0;
	u8 Post = 0;

	bool operator==(const Curve&) const = default;
};

struct Lane
{
	Param Id = Param::Position;
	u8 Layer = 0;
	Curve Curves[3];

	bool operator==(const Lane&) const = default;
};

struct Clip
{
	float Start = 0.0f;
	float Length = 5.0f;
	bool IsLengthLocked = false;

	bool operator==(const Clip&) const = default;
};

struct AttachLink
{
	u32 ObjectId = 0;
	shared_str Bone;

	bool operator==(const AttachLink&) const = default;
};

struct AttachSwitch
{
	float Time = 0.0f;
	shared_str Bone;

	bool operator==(const AttachSwitch&) const = default;
};

inline constexpr u32 MaxAttachSwitches = 32;

struct Attachment
{
	xr_vector<AttachSwitch> Switches;
	AttachLink Parent;
	bool IsRollLocked = false, IsPitchLocked = false;
	AttachLink LookAt;
	bool IsYawOnly = false;
	float Weight = 1.0f;

	bool operator==(const Attachment&) const = default;
};

struct ActorBlock
{
	shared_str Source;
	shared_str Name;
	float In = 0.0f;
	float Out = 0.0f;
	float Speed = 1.0f;
	float Hold = 0.0f;
	float Start = 0.0f;
	float Loops = 1.0f;
	Interp BlendEase = Interp::EaseInOut;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;

	bool operator==(const ActorBlock&) const = default;
};

enum class RowMode : u8
{
	Override,
	Add
};

struct ActorLayer
{
	xr_vector<shared_str> Parts;
	RowMode Mode = RowMode::Override;
	bool IsMuted = false;
	float Weight = 1.0f;
	xr_vector<ActorBlock> Blocks;

	bool operator==(const ActorLayer&) const = default;
};

struct PoseBone
{
	shared_str Name;
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Offset = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);

	bool operator==(const PoseBone&) const = default;
};

inline bool IsBoneParam(Param Id)
{
	return Id == Param::BoneRotation || Id == Param::BonePosition || Id == Param::BoneScale;
}

inline constexpr u32 MaxLooks = 16, MaxLookSurfaces = 64, ItemSurfaceBase = 64;

struct SurfaceShader
{
	u8 Surface = 0;
	shared_str Shader;
	shared_str Texture;

	bool operator==(const SurfaceShader&) const = default;
};

struct ActorLook
{
	xr_vector<SurfaceShader> Surfaces;

	bool operator==(const ActorLook&) const = default;
};

inline const SurfaceShader* FindSurface(const ActorLook& Look, u32 Surface)
{
	const auto Found = std::find_if(Look.Surfaces.begin(), Look.Surfaces.end(), [Surface](const SurfaceShader& Item) { return Item.Surface == Surface; });
	return Found == Look.Surfaces.end() ? nullptr : &*Found;
}

inline constexpr u32 MaxIkLimbs = 4;

enum class IkSpace : u8
{
	Actor,
	World
};

struct IkLimb
{
	shared_str Bones[3];
	bool IsOn = false;
	IkSpace Space = IkSpace::Actor;
	float Weight = 1.0f;
	Fvector Target = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Turn = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Pole = Fvector().set(0.0f, 0.0f, 0.0f);

	bool operator==(const IkLimb&) const = default;
};

inline constexpr u32 MaxConstraints = 16;

enum class ConstraintKind : u8
{
	Point,
	Orient,
	Parent,
	Aim
};

struct BoneConstraint
{
	shared_str Bone;
	ConstraintKind Kind = ConstraintKind::Parent;
	AttachLink Target;
	bool IsMuted = false;
	float Weight = 1.0f;
	Fvector Shift = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Turn = Fvector().set(0.0f, 0.0f, 0.0f);

	bool operator==(const BoneConstraint&) const = default;
};

struct PoseRow
{
	RowMode Mode = RowMode::Add;
	bool IsMuted = false;
	float Weight = 1.0f;
	xr_vector<PoseBone> Bones;

	bool operator==(const PoseRow&) const = default;
};

struct ClipTrack
{
	RowMode Mode = RowMode::Override;
	xr_vector<PoseBone> Bones;
	xr_vector<Lane> Lanes;

	bool operator==(const ClipTrack&) const = default;
};

struct ClipLayer : ClipTrack
{
	shared_str Name;
	float Weight = 1.0f;
	bool IsMuted = false;

	bool operator==(const ClipLayer&) const = default;
};

struct AnimClip : ClipTrack
{
	shared_str Name;
	float Length = 1.0f;
	bool IsLinked = false;
	xr_vector<ClipLayer> Layers;
	IkLimb Ik[MaxIkLimbs];
	xr_vector<Lane> IkLanes;

	bool operator==(const AnimClip&) const = default;
};

inline constexpr u32 MaxClipLayers = 8;

inline constexpr u32 MaxClips = 64, MaxClipBones = 128;
inline constexpr const char* ClipSource = "clip";

inline bool IsClipBlock(const shared_str& Source)
{
	return Source.size() && !xr_strcmp(Source.c_str(), ClipSource);
}

inline s32 FindClip(const xr_vector<AnimClip>& Clips, const shared_str& Name)
{
	const auto Found = std::find_if(Clips.begin(), Clips.end(), [&Name](const AnimClip& Item) { return Item.Name == Name; });
	return Found == Clips.end() ? -1 : static_cast<s32>(Found - Clips.begin());
}

inline u8 PoseLayer(u32 Row, u32 Bone)
{
	return static_cast<u8>(Row * MaxPoseBones + Bone);
}

struct ActorData
{
	shared_str Visual;
	shared_str Item;
	shared_str Hud;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);
	float TimeScale = 1.0f;
	EndMode End = EndMode::Hold;
	xr_vector<shared_str> Packs;
	ActorLayer Base;
	xr_vector<ActorLayer> Layers;
	xr_vector<PoseRow> Poses;
	xr_vector<AnimClip> Clips;
	xr_vector<ActorLook> Looks;
	IkLimb Ik[MaxIkLimbs];
	xr_vector<BoneConstraint> Constraints;
	bool IsRelative = false;
	bool IsHudLinked = false;

	bool operator==(const ActorData&) const = default;
};

inline bool HasClips(const ActorData& Data)
{
	return !Data.Base.Blocks.empty() || std::any_of(Data.Layers.begin(), Data.Layers.end(), [](const ActorLayer& Layer) { return !Layer.Blocks.empty(); });
}

inline ActorLayer& RowOf(ActorData& Data, u8 Row)
{
	return Row ? Data.Layers[Row - 1] : Data.Base;
}

inline const ActorLayer& RowOf(const ActorData& Data, u8 Row)
{
	return Row ? Data.Layers[Row - 1] : Data.Base;
}

inline xr_vector<ActorBlock>& BlocksOf(ActorData& Data, u8 Layer)
{
	return RowOf(Data, Layer).Blocks;
}

inline const xr_vector<ActorBlock>& BlocksOf(const ActorData& Data, u8 Layer)
{
	return RowOf(Data, Layer).Blocks;
}

struct RowSpan
{
	bool IsCounted = false;
	u8 Row = 0;
	float Start = 0.0f;
	float End = 0.0f;
};

template <typename TSpanOf>
bool IsSpanFree(u32 Count, const TSpanOf& SpanOf, u8 Row, float Start, float End, bool IsStrict)
{
	for (u32 A = 0; A < Count; ++A)
	{
		const RowSpan One = SpanOf(A);
		if (!One.IsCounted || One.Row != Row || !(Start < One.End && One.Start < End))
		{
			continue;
		}
		if (IsStrict || (One.Start <= Start && One.End >= End) || (Start <= One.Start && End >= One.End))
		{
			return false;
		}
		for (u32 B = A + 1; B < Count; ++B)
		{
			const RowSpan Two = SpanOf(B);
			if (Two.IsCounted && Two.Row == Row && std::max({ Start, One.Start, Two.Start }) < std::min({ End, One.End, Two.End }))
			{
				return false;
			}
		}
	}
	return true;
}

inline u32 AddRows(const ActorData& Data)
{
	return (Data.Base.Mode == RowMode::Add) + static_cast<u32>(std::count_if(Data.Layers.begin(), Data.Layers.end(), [](const ActorLayer& Layer) { return Layer.Mode == RowMode::Add; }));
}

enum class TangentMode : u8
{
	Auto,
	Aligned,
	Broken
};

inline constexpr float MaxHandle = 1000.0f;

struct PoseKey
{
	float Time = 0.0f;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	Interp Mode = Interp::EaseInOut;
	TangentMode Tangent = TangentMode::Auto;
	float Tension = 0.0f;
	float Continuity = 0.0f;
	float Bias = 0.0f;
	Fvector InHandle = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector OutHandle = Fvector().set(0.0f, 0.0f, 0.0f);

	bool HasTangent() const
	{
		return Tangent != TangentMode::Auto || Tension != 0.0f || Continuity != 0.0f || Bias != 0.0f;
	}

	bool operator==(const PoseKey&) const = default;
};

struct Shake
{
	u8 Preset = 0;
	bool IsEnabled = true;
	float Influence = 1.0f;
	float PositionScale = 1.0f;
	float Speed = 1.0f;
	float Offset = 0.0f;
	bool IsStretched = false;

	bool operator==(const Shake&) const = default;
};

struct ShakeClip
{
	float Start = 0.0f;
	float Length = 2.0f;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;
	u8 Row = 0;
	Shake Setup;
	xr_vector<Lane> Lanes;
	Interp BlendEase = Interp::EaseInOut;

	bool operator==(const ShakeClip&) const = default;
};

struct CameraBlock
{
	float Start = 0.0f;
	float Length = 5.0f;
	Interp BlendEase = Interp::EaseInOut;
	float Fov = 0.0f;
	float TimeScale = 1.0f;
	DofMode Dof = DofMode::Off;
	float Focus = 5.0f;
	float FocusOffset = 0.0f;
	float NearRange = 2.0f;
	float FarRange = 20.0f;
	float Blur = 5.0f;
	bool IsPhysical = false;
	bool IsAutofocus = false;
	float Aperture = 4.0f;
	float FocalLength = 0.0f;
	float Point[2] = { 0.5f, 0.5f };
	float Limit = 0.0f;
	float Squeeze = 1.0f;
	float FocusTime = 0.1f;
	float TransitionTime = 0.0f;
	float Swirl = 0.0f;
	float Smooth = 0.0f;
	float Tension = 1.0f;
	bool IsShortTurn = false;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	xr_string Name;
	AttachLink FocusTarget;
	xr_vector<PoseKey> Keys;
	xr_vector<Lane> Lanes;
	Attachment Attach;

	bool operator==(const CameraBlock&) const = default;
};

struct CameraData
{
	xr_vector<CameraBlock> Blocks;
	xr_vector<ShakeClip> Shakes;
	u8 OverrideRows = 0;

	bool operator==(const CameraData&) const = default;

};

enum class LightKind : u8
{
	Point,
	Spot
};

struct LightData
{
	LightKind Kind = LightKind::Point;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Color = Fvector().set(1.0f, 1.0f, 1.0f);
	float Intensity = 1.0f;
	float Range = 8.0f;
	float Cone = 60.0f;
	float VirtualSize = 0.1f;
	bool HasShadow = false;
	bool IsVolumetric = false;
	float VolumeIntensity = 1.0f;
	float VolumeDistance = 1.0f;
	float VolumeQuality = 1.0f;
	shared_str Texture;

	bool operator==(const LightData&) const = default;
};

struct ParticleData
{
	shared_str Effect;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	float Size = 1.0f;
	float Speed = 1.0f;
	bool ShouldRetrigger = false;

	bool operator==(const ParticleData&) const = default;
};

struct DecalData
{
	shared_str Texture;
	shared_str Shader;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	float Width = 0.5f;
	float Height = 0.5f;
	bool IsStamp = false;

	bool operator==(const DecalData&) const = default;
};

struct SoundData
{
	shared_str File;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	float Volume = 1.0f;
	float Pitch = 1.0f;
	float Offset = 0.0f;
	float MinDistance = 0.0f;
	float MaxDistance = 0.0f;
	bool IsWorld = false;
	bool IsLooped = false;

	bool operator==(const SoundData&) const = default;
};

struct WeatherData
{
	shared_str Cycle;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	u32 Mask = 0;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;
	float TimeOfDay = 12.0f;
	float SunHeading = 0.0f;
	float SunHeight = 45.0f;
	Fvector SunColor = Fvector().set(1.0f, 1.0f, 1.0f);
	float SunBrightness = 1.0f;
	float SunShafts = 0.3f;
	Fvector AmbientColor = Fvector().set(0.1f, 0.1f, 0.1f);
	Fvector HemiColor = Fvector().set(0.5f, 0.5f, 0.5f);
	Fvector FogColor = Fvector().set(0.5f, 0.5f, 0.5f);
	float FogDensity = 0.5f;
	float FogDistance = 400.0f;
	float FarPlane = 600.0f;
	float RainDensity = 0.0f;
	float WindSpeed = 0.0f;
	float WindDirection = 0.0f;
	float SkyRotation = 0.0f;

	bool operator==(const WeatherData&) const = default;
};

inline float* WeatherField(WeatherData& Setup, Param Id, u8 Component)
{
	const u8 Axis = Component < 3 ? Component : 0;
	float* const Fields[16] = { &Setup.TimeOfDay, &Setup.SunHeading, &Setup.SunHeight, &Setup.SunColor[Axis], &Setup.SunBrightness, &Setup.SunShafts, &Setup.AmbientColor[Axis], &Setup.HemiColor[Axis], &Setup.FogColor[Axis], &Setup.FogDensity, &Setup.FogDistance, &Setup.FarPlane, &Setup.RainDensity, &Setup.WindSpeed, &Setup.WindDirection, &Setup.SkyRotation };
	return Fields[static_cast<u32>(Id) - static_cast<u32>(Param::WeatherTime)];
}

inline bool IsWeatherSet(const WeatherData& Setup, const xr_vector<Lane>& Lanes, Param Id)
{
	return ((Setup.Mask >> (static_cast<u32>(Id) - static_cast<u32>(Param::WeatherTime))) & 1) || std::any_of(Lanes.begin(), Lanes.end(), [Id](const Lane& Item) { return Item.Id == Id; });
}

struct PostData
{
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	u32 Mask = 0;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;
	Fvector Base = Fvector().set(0.5f, 0.5f, 0.5f);
	Fvector Add = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector GrayColor = Fvector().set(0.333f, 0.333f, 0.333f);
	float Gray = 0.0f;
	float Blur = 0.0f;
	float DualityH = 0.0f;
	float DualityV = 0.0f;
	float Noise = 0.0f;
	float Grain = 1.0f;
	float NoiseFps = 30.0f;
	float Fade = 0.0f;
	Fvector FadeColor = Fvector().set(0.0f, 0.0f, 0.0f);
	float Exposure = 0.0f;
	float Bloom = 0.04f;
	float Tonemap = 1.0f;
	float MiddleGray = 1.0f;
	float MotionBlur = 0.25f;

	bool operator==(const PostData&) const = default;
};

inline float* PostField(PostData& Setup, Param Id, u8 Component)
{
	const u8 Axis = Component < 3 ? Component : 0;
	float* const Fields[17] = { &Setup.Base[Axis], &Setup.Add[Axis], &Setup.GrayColor[Axis], &Setup.Gray, &Setup.Blur, &Setup.DualityH, &Setup.DualityV, &Setup.Noise, &Setup.Grain, &Setup.NoiseFps, &Setup.Fade, &Setup.FadeColor[Axis], &Setup.Exposure, &Setup.Bloom, &Setup.Tonemap, &Setup.MiddleGray, &Setup.MotionBlur };
	return Fields[static_cast<u32>(Id) - static_cast<u32>(Param::PostBase)];
}

inline bool IsPostSet(const PostData& Setup, const xr_vector<Lane>& Lanes, Param Id)
{
	return ((Setup.Mask >> (static_cast<u32>(Id) - static_cast<u32>(Param::PostBase))) & 1) || std::any_of(Lanes.begin(), Lanes.end(), [Id](const Lane& Item) { return Item.Id == Id; });
}

struct Object
{
	u32 Id = 0;
	ObjectType Type = ObjectType::Actor;
	xr_string Name;
	bool IsNameLocked = false;
	u32 Color = 0;
	u32 Row = 0;
	u32 FolderId = 0;
	bool IsMuted = false;
	Clip Span;
	xr_vector<Lane> Lanes;
	ActorData Actor;
	CameraData Camera;
	LightData Light;
	ParticleData Particle;
	DecalData Decal;
	SoundData Sound;
	WeatherData Weather;
	PostData Post;
	Attachment Attach;

	bool operator==(const Object&) const = default;
};

struct Folder
{
	u32 Id = 0;
	u32 Row = 0;
	xr_string Name;

	bool operator==(const Folder&) const = default;
};

struct Marker
{
	float Time = 0.0f;
	xr_string Name;

	bool operator==(const Marker&) const = default;
};

struct SceneHeader
{
	u32 FrameRate = 30;
	float CutDistance = 5.0f;
	float CutAngle = 45.0f;
	bool IsWorldFollowing = false;
	u32 NextColor = 0;
	xr_vector<Folder> Folders;
	xr_vector<Marker> Markers;

	bool operator==(const SceneHeader&) const = default;
};

inline constexpr u8 SizeRow = 0, SizeSub = 1, SizeLane = 2;
inline constexpr float MaxRowSize = 6.0f, MaxLaneSize = 8.0f;

struct RowSize
{
	u32 ObjectId = 0;
	u8 Kind = SizeRow;
	Param Id = Param::Position;
	u8 Layer = 0;
	float Rows = 1.0f;

	bool operator==(const RowSize&) const = default;
};

struct Scene
{
	SceneHeader Header;
	u32 NextId = 1;
	shared_str LevelName;
	xr_vector<Object> Objects;
	xr_vector<RowSize> Sizes;

	bool operator==(const Scene&) const = default;
};

inline constexpr u32 Rgb(u32 R, u32 G, u32 B)
{
	return 0xFF000000u | (B << 16) | (G << 8) | R;
}

inline constexpr u32 Palette[12] =
{
	Rgb(230, 103, 103), Rgb(230, 166, 103), Rgb(230, 230, 103), Rgb(166, 230, 103), Rgb(103, 230, 103), Rgb(103, 230, 166),
	Rgb(103, 230, 230), Rgb(103, 166, 230), Rgb(103, 103, 230), Rgb(166, 103, 230), Rgb(230, 103, 230), Rgb(230, 103, 166),
};

inline s32 IndexOf(const Scene& Data, u32 Id)
{
	const auto Found = std::find_if(Data.Objects.begin(), Data.Objects.end(), [Id](const Object& Item) { return Item.Id == Id; });
	return Found == Data.Objects.end() ? -1 : static_cast<s32>(Found - Data.Objects.begin());
}

inline const Object* Find(const Scene& Data, u32 Id)
{
	const s32 Index = IndexOf(Data, Id);
	return Index < 0 ? nullptr : &Data.Objects[Index];
}

inline Object* Find(Scene& Data, u32 Id)
{
	return const_cast<Object*>(Find(static_cast<const Scene&>(Data), Id));
}

inline const Object* FindCamera(const Scene& Data)
{
	const auto Found = std::find_if(Data.Objects.begin(), Data.Objects.end(), [](const Object& Item) { return Item.Type == ObjectType::Camera; });
	return Found == Data.Objects.end() ? nullptr : &*Found;
}

inline Object* FindCamera(Scene& Data)
{
	return const_cast<Object*>(FindCamera(static_cast<const Scene&>(Data)));
}

inline void EnsureCamera(Scene& Data)
{
	if (FindCamera(Data))
	{
		return;
	}
	u32 Row = 0;
	for (const Object& Item : Data.Objects)
	{
		Row = Item.FolderId ? Row : std::max(Row, Item.Row + 1);
	}
	for (const Folder& Item : Data.Header.Folders)
	{
		Row = std::max(Row, Item.Row + 1);
	}
	Object& Rig = Data.Objects.emplace_back();
	Rig.Id = Data.NextId++;
	Rig.Type = ObjectType::Camera;
	Rig.Name = "Camera";
	Rig.Color = Palette[Data.Header.NextColor++ % 12];
	Rig.Row = Row;
	Rig.Span.Length = MinLength;
}

inline Fvector* PlacementOf(Object& Target, u32 Kind)
{
	Fvector* const Fields[8][2] = { { &Target.Actor.Position, &Target.Actor.Rotation }, { nullptr, nullptr }, { &Target.Light.Position, &Target.Light.Rotation }, { &Target.Particle.Position, &Target.Particle.Rotation }, { &Target.Decal.Position, &Target.Decal.Rotation }, { &Target.Sound.Position, &Target.Sound.Rotation }, { &Target.Weather.Position, &Target.Weather.Rotation }, { &Target.Post.Position, &Target.Post.Rotation } };
	return Fields[static_cast<u32>(Target.Type)][Kind];
}

inline const Fvector* PlacementOf(const Object& Target, u32 Kind)
{
	return PlacementOf(const_cast<Object&>(Target), Kind);
}

inline void SetShakeRow(ShakeClip& Clip, u8 Row)
{
	Clip.Row = Row;
	for (Lane& Item : Clip.Lanes)
	{
		Item.Layer = Row;
	}
}

inline float ClipEnd(const Object& Target)
{
	return Target.Span.Start + Target.Span.Length;
}

inline float ContentEnd(const Scene& Data)
{
	float End = 0.0f;
	for (const Object& Item : Data.Objects)
	{
		End = std::max(End, ClipEnd(Item));
	}
	return End;
}

inline float Sanitize(float Input, float Lo, float Hi, float Fallback)
{
	return std::isfinite(Input) ? std::clamp(Input, Lo, Hi) : Fallback;
}

inline xr_string NameFromVisual(const char* Visual)
{
	const xr_string_view Path = Visual ? Visual : "";
	const size_t Slash = Path.find_last_of("\\/");
	const xr_string_view File = Slash == xr_string_view::npos ? Path : Path.substr(Slash + 1);
	const xr_string_view Stem = File.substr(0, File.find_last_of('.'));
	return xr_string(Stem.data(), static_cast<u32>(Stem.size()));
}

inline xr_string EffectName(const shared_str& Effect)
{
	const xr_string_view Path = Effect.size() ? Effect.c_str() : "";
	const xr_string_view Leaf = Path.substr(Path.find_last_of('\\') + 1);
	return xr_string(Leaf.data(), static_cast<u32>(Leaf.size()));
}

inline u32 NextColor(SceneHeader& Header)
{
	return Palette[Header.NextColor++ % 12];
}

inline bool IsMarkerFree(const SceneHeader& Header, float Time, s32 Skip = -1)
{
	const float HalfFrame = 0.5f / static_cast<float>(Header.FrameRate);
	for (u32 Index = 0; Index < Header.Markers.size(); ++Index)
	{
		if (static_cast<s32>(Index) != Skip && std::fabs(Header.Markers[Index].Time - Time) < HalfFrame)
		{
			return false;
		}
	}
	return true;
}

template <typename TVisit>
void ForEachLink(const Object& Item, const TVisit& Visit)
{
	Visit(Item.Attach.Parent.ObjectId);
	Visit(Item.Attach.LookAt.ObjectId);
	for (const BoneConstraint& Rule : Item.Actor.Constraints)
	{
		Visit(Rule.Target.ObjectId);
	}
	for (const CameraBlock& Shot : Item.Camera.Blocks)
	{
		Visit(Shot.Attach.Parent.ObjectId);
		Visit(Shot.Attach.LookAt.ObjectId);
	}
}

class AttachGraph
{
public:
	bool Build(const Scene& Data, u32 From = 0, u32 To = 0)
	{
		const u32 Count = static_cast<u32>(Data.Objects.size());
		Ids.clear();
		for (u32 Index = 0; Index < Count; ++Index)
		{
			Ids.emplace_back(Data.Objects[Index].Id, Index);
		}
		std::sort(Ids.begin(), Ids.end());
		Layer.assign(Count, Unset);
		IsLinked.assign(Count, 0);
		Order.clear();
		for (const Object& Item : Data.Objects)
		{
			ForEachLink(Item, [this](u32 Id)
			{
				const s32 At = IndexOf(Id);
				if (At >= 0)
				{
					IsLinked[At] = 1;
				}
			});
		}
		for (u32 Pass = 0; Pass <= MaxAttachDepth && Order.size() < Count; ++Pass)
		{
			for (u32 Index = 0; Index < Count; ++Index)
			{
				bool IsFree = Layer[Index] == Unset;
				const auto Visit = [&](u32 Id) { const s32 At = IndexOf(Id); IsFree = IsFree && (At < 0 || static_cast<u32>(Layer[At]) < Pass); };
				ForEachLink(Data.Objects[Index], Visit);
				Visit(Data.Objects[Index].Id == From ? To : 0);
				if (IsFree)
				{
					Layer[Index] = static_cast<u8>(Pass);
					Order.push_back(Index);
				}
			}
		}
		const bool IsValid = Order.size() == Count;
		for (u32 Index = 0; Index < Count; ++Index)
		{
			if (Layer[Index] == Unset)
			{
				Order.push_back(Index);
			}
		}
		return IsValid;
	}

	void Measure(const Scene& Data, u32 From)
	{
		IsMeasured = Build(Data);
		Above.assign(Order.size(), 0);
		const s32 Root = IndexOf(From);
		if (Root >= 0)
		{
			Above[Root] = 1;
		}
		Deepest = 1;
		for (u32 Index : Order)
		{
			ForEachLink(Data.Objects[Index], [&](u32 Id) { const s32 At = IndexOf(Id); Above[Index] = At >= 0 && Above[At] ? std::max(Above[Index], static_cast<u8>(Above[At] + 1)) : Above[Index]; });
			Deepest = std::max(Deepest, Above[Index]);
		}
	}

	bool Allows(u32 To) const
	{
		const s32 At = IndexOf(To);
		return At < 0 || (IsMeasured && !Above[At] && static_cast<u32>(Layer[At] + Deepest) <= MaxAttachDepth);
	}

	s32 IndexOf(u32 Id) const
	{
		const auto Found = std::lower_bound(Ids.begin(), Ids.end(), std::pair<u32, u32>(Id, 0));
		return Id && Found != Ids.end() && Found->first == Id ? static_cast<s32>(Found->second) : -1;
	}

	xr_vector<u32> Order;
	xr_vector<u8> IsLinked;

private:
	static constexpr u8 Unset = 0xFF;
	xr_vector<std::pair<u32, u32>> Ids;
	xr_vector<u8> Layer, Above;
	u8 Deepest = 1;
	bool IsMeasured = false;
};
}
