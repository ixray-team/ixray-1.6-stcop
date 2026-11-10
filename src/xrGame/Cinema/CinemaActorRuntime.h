#pragma once
#include "CinemaCurve.h"
#include "CinemaOmf.h"
#include "../../Include/xrRender/KinematicsAnimated.h"

namespace Cinema
{
class VisualHandle
{
public:
	VisualHandle() = default;
	explicit VisualHandle(IRenderVisual* Owned);
	~VisualHandle();
	VisualHandle(VisualHandle&& Other) noexcept;
	VisualHandle& operator=(VisualHandle&& Other) noexcept;
	IRenderVisual* Get() const { return Visual; }
	void Reset();

private:
	IRenderVisual* Visual = nullptr;
};

class ActorObject final : public CObject
{
public:
	ActorObject();
	~ActorObject() override;
	void Bind(IRenderVisual* Model, const Fmatrix& Transform);
	void Unbind();
	void renderable_Render(IDSGraphManager* Manager) override;
	bool renderable_ShadowGenerate() override { return false; }
	bool register_schedule() const override { return false; }
	IRenderVisual* ItemVisual = nullptr;
	Fmatrix ItemTransform = Fidentity;

private:
	bool IsInSpace = false;
};

struct ItemSetup
{
	bool IsWeaponStyle = false;
	s32 HandDependence = 2;
	shared_str BoneName;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	u16 BoneR = u16(-1), BoneL = u16(-1), BoneR2 = u16(-1), BoneAttach = u16(-1);
};

struct BlockSpan
{
	float Start = 0.0f;
	float Duration = 0.0f;
	float Play = 0.0f;
	float Loops = 1.0f;
	float From = 0.0f;
	float To = 0.0f;
	float RawScale = 1.0f;
	float Overlap = 0.0f;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;
	Interp BlendEase = Interp::EaseInOut;
	MotionID Motion;
	u32 Block = 0;
};

inline constexpr u32 MaxTicks = 4096;

struct MarkTick
{
	float Sequence = 0.0f;
	shared_str Name;
};

struct LayerRuntime
{
	xr_vector<BlockSpan> Spans;
	xr_vector<float> Starts;
	xr_vector<MarkTick> Ticks;
	float Length = 0.0f;
	u8 Mask = 0;
	u8 Channel = 0;
};

struct PoseSource
{
	MotionID Motion;
	float Raw = 0.0f;
	float Alpha = 0.0f;
};

struct LayerPose
{
	PoseSource Items[2];
	u8 Count = 0;
	u8 Layer = 0;
	u8 Mask = 0;
	float Weight = 0.0f;
	u8 Channel = 0;
};

struct MixEntry
{
	MotionID Motion;
	u8 Layer = 0;
	u8 Slot = 0;
	float Raw = 0.0f;
	float Weight = 0.0f;
	CBlend* Blend = nullptr;
};

inline constexpr u32 MaxMix = 2 * (MaxActorLayers + 1);

struct MixPart
{
	MixEntry Entries[MaxMix];
	u8 Count = 0;
};

struct AdaptedSource
{
	shared_str Original;
	shared_str Loaded;
};

struct Audition
{
	u32 ObjectId = 0;
	shared_str Source;
	shared_str Name;
	float Start = 0.0f;
	shared_str Visual;
};

struct BoneSolve
{
	Fmatrix Add = Fidentity;
	Fmatrix Over = Fidentity;
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);
	float Weight = 0.0f;
	u16 Parent = BI_NONE;
	IKinematics* Model = nullptr;
	const Fmatrix* Bind = nullptr;
};

inline bool ShaderLess(const shared_str& A, const shared_str& B)
{
	return xr_strcmp(A, B) < 0;
}

class ActorRuntime
{
public:
	explicit ActorRuntime(u32 ObjectId);
	~ActorRuntime();
	void Apply(const Object& Target, float Step, u32& RetargetIndex, const Audition* Listen);
	bool Evaluate(const Object& Target, float Time, const Audition* Listen, bool IsNeeded, bool IsSilent);
	bool ClipMoment(s32 Clip, float& Time) const;
	bool SetSolo(const shared_str& Source, const shared_str& Name, float& Length);
	MotionID SoloMotion;
	bool IsRootPinned = false;
	void PinRoot();
	float SoloTime = 0.0f;
	struct ClipPreview
	{
		s32 Clip = -1;
		float Time = 0.0f;
		u32 Frame = 0;
	};
	ClipPreview Preview;
	void Place(const Fmatrix& Transform);
	bool BoneFrame(const shared_str& Bone, Fmatrix& Out) const;
	void Bones(const Fmatrix& World, xr_vector<Fvector>& Lines) const;
	void Pose(const Object& Target, float Local);
	void PoseBones(const xr_vector<PoseBone>& Bones, const xr_vector<Lane>& Lanes, u32 Row, RowMode Mode, float Weight, float Time);
	void Locals(xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts) const;
	void Dress(const Object& Target, float Local, const xr_vector<shared_str>& Shaders);
	void SolveIk(const Object& Target, float Local, const Fmatrix& World);
	void Constrain(const BoneConstraint& Rule, float Weight, const Fmatrix& Goal, const Fmatrix& World);
	IRenderVisual* SurfaceAt(u32 Surface) const;
	bool SurfaceNames(u32 Surface, shared_str& Shader, shared_str& Texture) const;
	bool BoneWorld(u16 Bone, const Fmatrix& World, Fmatrix& Out) const;
	bool RestLocal(u16 Bone, Fmatrix& Out) const;
	bool RestPose(xr_vector<Fmatrix>& Model) const;
	bool RestModel(u16 Bone, Fmatrix& Out) const;
	u16 BoneParent(u16 Bone) const;
	u16 FindBone(const shared_str& Name) const;
	u16 BoneCount() const;
	const char* BoneName(u16 Bone) const;
	float MotionLength(const shared_str& Source, const shared_str& Name) const;
	float BlockDuration(const ActorBlock& Block) const;
	bool IsComplete() const { return Model.Get() && Unresolved == 0; }
	bool PickSphere(Fsphere& Sphere) const;
	const LayerRuntime& Track(u8 Layer) const { return Tracks[std::min<u32>(Layer, MaxActorLayers)]; }
	bool HasSpans() const { return std::any_of(std::begin(Tracks), std::end(Tracks), [](const LayerRuntime& Item) { return !Item.Spans.empty(); }); }
	const char* PartName(u16 Part) const;
	u32 Id = 0;
	shared_str Error;
	xr_vector<AnimationSource> Sources;
	LayerRuntime Tracks[MaxActorLayers + 1];
	float SequenceLength = 0.0f;
	float First = 0.0f;
	float Last = 0.0f;
	float Extent = 0.0f;
	Curves::AreaTable TimeScaleArea;
	ActorObject Body;
	bool IsPoseOnly = false;

private:
	bool LoadModel(const shared_str& Visual);
	void LoadItem(const shared_str& Section);
	void BuildSources(const xr_vector<shared_str>& Packs);
	bool EnsureSource(const shared_str& Source, u32& RetargetIndex);
	MotionID FindMotion(const shared_str& Source, const shared_str& Name) const;
	void BuildTracks(const Object& Target, u32& RetargetIndex);
	void BuildSpans(const Object& Target, const xr_vector<ActorBlock>& Blocks, LayerRuntime& Track, u32& RetargetIndex);
	void Bound(const Object& Target);
	MotionID IdleMotion();
	void MixParts(const LayerPose* Poses, u32 Count);
	bool SetBindPose();
	void ClearBindPose();
	void ReleaseRig();
	void PlaceItem();
	VisualHandle Model, ItemModel;
	IKinematics* Skeleton = nullptr;
	IKinematicsAnimated* Animated = nullptr;
	xr_vector<shared_str> ModelSources;
	xr_vector<AdaptedSource> Adapted;
	xr_vector<shared_str> Failed;
	xr_vector<Fmatrix> BindPose;
	xr_vector<Fmatrix> BindLocals;
	xr_vector<BoneSolve> Solves;
	bool IsRigged = false;
	struct ClipSample
	{
		s32 Clip = -1;
		float Time = 0.0f;
		float Weight = 0.0f;
	};
	ClipSample Samples[2 * (MaxActorLayers + 1)];
	u32 SampleCount = 0;
	xr_vector<AnimClip> AppliedClips;
	ItemSetup Item;
	shared_str AppliedVisual, AppliedItem;
	xr_vector<shared_str> AppliedPacks;
	ActorLayer AppliedBase;
	xr_vector<ActorLayer> AppliedLayers;
	Curve AppliedWarp;
	float AppliedRate = -1.0f;
	MixPart Mixed[MAX_CHANNELS][MAX_PARTS];
	MotionID IdleId;
	bool IsIdleKnown = false;
	u8 NamedParts = 0;
	shared_str HeardSource, HeardName;
	MotionID HeardMotion;
	Fmatrix Placement = Fidentity;
	u32 Unresolved = 0;
	bool IsPoseValid = false;
	bool IsShown = false;
};
}

namespace Cinema::Actors
{
bool ItemValid(const shared_str& Section);
float FrameAt(const BlockSpan& Span, float Offset);
float Measure(const ActorBlock& Block, float Length, float& In, float& Out);
void TrimBlock(ActorBlock& Block, float Length, float Keep);
}
