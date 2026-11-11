#include "StdAfx.h"
#include "CinemaActorRuntime.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../xrEngine/Render.h"
#include "../CameraRig.h"

namespace Cinema::Actors
{
struct FrozenTracks final : public IUpdateTracksCallback
{
	bool operator()(float, IKinematicsAnimated&) override
	{
		return true;
	}
};

FrozenTracks Frozen;
const Curve NoCurve {};
constexpr u8 AddChannels[MaxAddRows] = { 4, 5, 2, 3 };
constexpr const char* IdleNames[] = { "idle", "stand_idle_0", "stand_idle_1", "stand_idle_2", "norm_idle_0", "norm_idle_1", "idle_0", "idle_1" };

void BindPoseCallback(CBoneInstance* Instance)
{
	if (const Fmatrix* Matrix = static_cast<const Fmatrix*>(Instance->callback_param()))
	{
		Instance->mTransform = *Matrix;
	}
}

void RigCallback(CBoneInstance* Instance)
{
	const BoneSolve& Solve = *static_cast<const BoneSolve*>(Instance->callback_param());
	Fmatrix& Bone = Instance->mTransform;
	Bone = Solve.Bind ? *Solve.Bind : Bone;
	if (Solve.Weight > 0.0f)
	{
		const Fmatrix& Parent = Solve.Parent == BI_NONE ? Fidentity : Solve.Model->LL_GetTransform(Solve.Parent);
		Fmatrix Inverse, Local, Posed;
		Inverse.invert(Parent);
		Local.mul_43(Inverse, Bone);
		Fquaternion From, To, Mixed;
		From.set(Local);
		To.set(Solve.Over);
		Mixed.slerp(From, To, Solve.Weight);
		Posed.rotation(Mixed);
		Posed.c.lerp(Local.c, Solve.Over.c, Solve.Weight);
		Bone.mul_43(Parent, Posed);
	}
	Bone.mulB_43(Solve.Add);
	Bone.i.mul(Solve.Scale.x);
	Bone.j.mul(Solve.Scale.y);
	Bone.k.mul(Solve.Scale.z);
}

const shared_str& LoadedId(const xr_vector<AdaptedSource>& Adapted, const shared_str& Source)
{
	const auto Found = std::find_if(Adapted.begin(), Adapted.end(), [&Source](const AdaptedSource& Item) { return Item.Original == Source; });
	return Found == Adapted.end() ? Source : Found->Loaded;
}

bool HasSlot(IKinematicsAnimated* Animated, const shared_str& Id)
{
	for (u16 Slot = 0, Count = Animated->LL_MotionsSlotCount(); Slot < Count; ++Slot)
	{
		shared_motions Motions = Animated->LL_MotionsSlot(Slot);
		if (Motions.motion_map() && Motions.id() == Id)
		{
			return true;
		}
	}
	return false;
}

bool ContainsIdle(const char* Name)
{
	for (; *Name; ++Name)
	{
		if (!_strnicmp(Name, "idle", 4))
		{
			return true;
		}
	}
	return false;
}

bool ItemValid(const shared_str& Section)
{
	if (!pSettings || !Section.size() || !pSettings->section_exist(Section))
	{
		return false;
	}
	const char* Name = Section.c_str();
	const char* Bone = pSettings->read_if_exists<str_c>(Name, "attach_bone_name", nullptr);
	const bool IsWeapon = pSettings->line_exist(Name, "position") && pSettings->line_exist(Name, "orientation");
	const bool IsAttach = Bone && Bone[0] && pSettings->line_exist(Name, "attach_angle_offset") && pSettings->line_exist(Name, "attach_position_offset");
	return (IsWeapon || IsAttach) && Omf::VisualExists(pSettings->read_if_exists<str_c>(Name, "visual", nullptr));
}

float Measure(const ActorBlock& Block, float Length, float& In, float& Out)
{
	const float Speed = std::fabs(Block.Speed), Last = Length - END_EPS;
	if (!std::isfinite(Length) || Length <= END_EPS || !std::isfinite(Speed) || Speed <= EPS_S)
	{
		return -1.0f;
	}
	In = std::clamp(Block.In, 0.0f, Last);
	Out = Block.Out > 0.0f ? std::clamp(Block.Out, In, Last) : Last;
	return (Out - In) / Speed;
}

float FrameAt(const BlockSpan& Span, float Offset)
{
	if (!(Span.Play > 0.0f))
	{
		return Span.From;
	}
	const float Total = Span.Play * Span.Loops, Played = std::clamp(Offset, 0.0f, Total);
	const float Phase = Played < Total ? std::fmod(Played, Span.Play) : Total - Span.Play * std::ceil(Span.Loops - 1.0f);
	return Span.From + (Span.To - Span.From) * Phase / Span.Play;
}

void TrimBlock(ActorBlock& Block, float Length, float Keep)
{
	float In = 0.0f, Out = 0.0f;
	const float Play = Measure(Block, Length, In, Out), Total = std::max(Play, 0.0f) * std::clamp(Block.Loops, 1.0f, MaxLoops);
	if (!(Play >= 0.0f) || Keep >= Total + std::max(Block.Hold, 0.0f))
	{
		return;
	}
	Block.Hold = std::max(Keep - Total, 0.0f);
	if (Keep >= Total)
	{
		return;
	}
	Block.Loops = Keep >= Play ? Keep / Play : 1.0f;
	const float Moved = Keep * std::max(std::fabs(Block.Speed), 0.05f);
	Block.In = Keep >= Play || Block.Speed >= 0.0f ? Block.In : Out - Moved;
	Block.Out = Keep >= Play ? Block.Out : Block.Speed >= 0.0f ? In + Moved : Out;
}
}

namespace Cinema
{
VisualHandle::VisualHandle(IRenderVisual* Owned) : Visual(Owned)
{
	if (Visual)
	{
		vis_data& Data = Visual->getVisData();
		Data.marker = 0;
		Data.accept_frame = 0;
		Data.hom_frame = 0;
	}
}

VisualHandle::~VisualHandle()
{
	Reset();
}

VisualHandle::VisualHandle(VisualHandle&& Other) noexcept : Visual(std::exchange(Other.Visual, nullptr))
{
}

VisualHandle& VisualHandle::operator=(VisualHandle&& Other) noexcept
{
	if (this != &Other)
	{
		Reset();
		Visual = std::exchange(Other.Visual, nullptr);
	}
	return *this;
}

void VisualHandle::Reset()
{
	if (Visual && ::Render)
	{
		::Render->model_Delete(Visual, true);
	}
	Visual = nullptr;
}

ActorObject::ActorObject()
{
	cName_set("cinema_fake_npc");
	cNameSect_set("cinema_fake_npc");
	SpatialComponent->type = ESPATIAL_TYPE::RENDERABLE;
}

ActorObject::~ActorObject()
{
	Unbind();
}

void ActorObject::Bind(IRenderVisual* Model, const Fmatrix& Transform)
{
	if (!Model)
	{
		Unbind();
		return;
	}
	renderable.visual = Model;
	renderable.xform = Transform;
	if (IsInSpace)
	{
		spatial_move();
		return;
	}
	IsInSpace = true;
	spatial_register();
}

void ActorObject::Unbind()
{
	renderable.visual = nullptr;
	if (IsInSpace)
	{
		IsInSpace = false;
		spatial_unregister();
	}
}

void ActorObject::renderable_Render(IDSGraphManager* Manager)
{
	if (!renderable.visual)
	{
		return;
	}
	Manager->add_Dynamic(renderable.visual, &renderable.xform);
	renderable.visual->getVisData().hom_frame = Device.dwFrame;
	if (ItemVisual)
	{
		Manager->add_Dynamic(ItemVisual, &ItemTransform);
		ItemVisual->getVisData().hom_frame = Device.dwFrame;
	}
}

ActorRuntime::ActorRuntime(u32 ObjectId) : Id(ObjectId)
{
}

ActorRuntime::~ActorRuntime()
{
	Body.Unbind();
	Body.ItemVisual = nullptr;
	ReleaseRig();
	ClearBindPose();
}

void ActorRuntime::Apply(const Object& Target, float Step, u32& RetargetIndex, const Audition* Listen)
{
	const ActorData& Data = Target.Actor;
	const shared_str& Visual = Listen && Listen->Visual.size() ? Listen->Visual : Data.Visual;
	const bool IsVisualChanged = Visual != AppliedVisual;
	const bool IsItemChanged = IsVisualChanged || Data.Item != AppliedItem;
	const bool IsPacksChanged = IsVisualChanged || Data.Packs != AppliedPacks;
	const auto IsSameLayer = [](const ActorLayer& A, const ActorLayer& B) { return A.Blocks == B.Blocks && A.Parts == B.Parts && A.IsMuted == B.IsMuted && A.Mode == B.Mode; };
	const Lane* Warp = Curves::FindLane(Target.Lanes, Param::TimeScale, 0);
	const Curve& Rate = Warp ? Warp->Curves[0] : Actors::NoCurve;
	Curves::Refresh(TimeScaleArea, Rate, Data.TimeScale, Step, Data.IsRelative ? Target.Span.Length : MaxTime, Info(Param::TimeScale));
	const bool IsWarpChanged = !Data.IsRelative && (!(Rate == AppliedWarp) || Data.TimeScale != AppliedRate);
	const bool IsClipsChanged = !std::equal(Data.Clips.begin(), Data.Clips.end(), AppliedClips.begin(), AppliedClips.end(), [](const AnimClip& A, const AnimClip& B) { return A.Name == B.Name && A.Length == B.Length; });
	const bool IsBlocksChanged = IsPacksChanged || IsWarpChanged || IsClipsChanged || !IsSameLayer(Data.Base, AppliedBase) || !std::equal(Data.Layers.begin(), Data.Layers.end(), AppliedLayers.begin(), AppliedLayers.end(), IsSameLayer);
	if (IsVisualChanged)
	{
		AppliedVisual = Visual;
		Error = shared_str();
		LoadModel(Visual);
	}
	if (IsItemChanged)
	{
		AppliedItem = Data.Item;
		LoadItem(Data.Item);
	}
	if (IsPacksChanged)
	{
		AppliedPacks = Data.Packs;
		BuildSources(Data.Packs);
	}
	if (IsBlocksChanged)
	{
		AppliedBase = Data.Base;
		AppliedLayers = Data.Layers;
		AppliedWarp = Rate;
		AppliedRate = Data.TimeScale;
		BuildTracks(Target, RetargetIndex);
		Bound(Target);
	}
	if (Model.Get() && (IsItemChanged || IsBlocksChanged))
	{
		Error = shared_str();
		if (AppliedItem.size() && !ItemModel.Get())
		{
			Error.printf("Item %s is unavailable", AppliedItem.c_str());
		}
		else if (Unresolved)
		{
			Error.printf("%u animation block(s) do not resolve", Unresolved);
		}
	}
	if (!Listen || !Listen->Source.size())
	{
		HeardSource = HeardName = shared_str();
		HeardMotion.invalidate();
	}
	else if (IsPacksChanged || Listen->Source != HeardSource || Listen->Name != HeardName)
	{
		HeardSource = Listen->Source;
		HeardName = Listen->Name;
		HeardMotion = EnsureSource(HeardSource, RetargetIndex) ? FindMotion(HeardSource, HeardName) : MotionID();
	}
}

void ActorRuntime::Bound(const Object& Target)
{
	const ActorData& Data = Target.Actor;
	First = MaxTime;
	Last = Extent = 0.0f;
	for (u8 Layer = 0; !Data.IsRelative && Layer <= std::min<size_t>(Data.Layers.size(), MaxActorLayers); ++Layer)
	{
		const bool IsMuted = RowOf(Data, Layer).IsMuted;
		for (const ActorBlock& Block : BlocksOf(Data, Layer))
		{
			First = IsMuted ? First : std::min(First, Block.Start);
		}
		for (const BlockSpan& Span : Tracks[Layer].Spans)
		{
			const float End = Curves::LaneInverseArea(Target.Lanes, &TimeScaleArea, Param::TimeScale, 0, Data.TimeScale, Span.Start + Span.Duration, MaxTime);
			Extent = std::max(Extent, End);
			Last = IsMuted ? Last : std::max(Last, End);
		}
	}
	First = std::min(First, Last);
}

bool ActorRuntime::Evaluate(const Object& Target, float Time, const Audition* Listen, bool IsNeeded, bool IsSilent)
{
	const bool IsHeard = Listen && HeardMotion.valid();
	const float Elapsed = Time - Target.Span.Start;
	const float Local = std::clamp(Elapsed, 0.0f, Target.Span.Length);
	const bool IsClipped = !Target.Actor.IsRelative && HasClips(Target.Actor);
	IsShown = IsHeard || (Listen && Listen->Visual.size()) || (!IsSilent && (IsClipped ? Elapsed >= First && Elapsed < Last : Elapsed >= 0.0f && Elapsed < Target.Span.Length));
	if (!Model.Get() || !(IsShown || IsNeeded))
	{
		SampleCount = 0;
		Body.Unbind();
		return false;
	}
	const ActorData& Data = Target.Actor;
	LayerPose Poses[MaxActorLayers + 1];
	u32 Count = 0;
	SampleCount = 0;
	if (IsHeard)
	{
		const float Length = Animated->get_animation_length(HeardMotion);
		const float Moment = Length > EPS_S ? std::fmod(std::max(Device.fTimeGlobal - Listen->Start, 0.0f), Length) : 0.0f;
		Poses[Count++] = { { { HeardMotion, Moment * Animated->LL_GetMotionDef(HeardMotion)->Speed(), 1.0f } }, 1, 0, NamedParts, 1.0f };
	}
	const bool IsSolo = SoloMotion.valid();
	if (IsSolo)
	{
		Poses[Count++] = { { { SoloMotion, SoloTime * Animated->LL_GetMotionDef(SoloMotion)->Speed(), 1.0f } }, 1, 0, NamedParts, 1.0f };
	}
	const float Length = SequenceLength;
	float Sequence = static_cast<float>(Curves::LaneArea(Target.Lanes, &TimeScaleArea, Param::TimeScale, 0, Local, Data.TimeScale));
	Sequence = !(Length > 0.0f) ? 0.0f : Data.End == EndMode::Loop ? std::fmod(Sequence, Length) : Data.End == EndMode::PingPong ? Length - std::fabs(std::fmod(Sequence, 2.0f * Length) - Length) : std::min(Sequence, Length);
	const u8 Layers = static_cast<u8>(1 + std::min<size_t>(Data.Layers.size(), MaxActorLayers));
	const auto Ramp = [](const BlockSpan& Span, float Offset)
	{
		Offset = std::clamp(Offset, 0.0f, Span.Duration);
		const float In = Span.FadeIn > 0.0f ? std::clamp(Offset / Span.FadeIn, 0.0f, 1.0f) : 1.0f, Out = Span.FadeOut > 0.0f ? std::clamp((Span.Duration - Offset) / Span.FadeOut, 0.0f, 1.0f) : 1.0f;
		return Curves::Ease(Span.BlendEase, In * Out);
	};
	for (u8 Layer = 0; !IsHeard && !IsPoseOnly && !IsSolo && Layer < Layers; ++Layer)
	{
		const LayerRuntime& Track = Tracks[Layer];
		const xr_vector<BlockSpan>& Spans = Track.Spans;
		const ActorLayer& Over = RowOf(Data, Layer);
		if (Over.IsMuted || (Layer && (Spans.empty() || Sequence < Spans.front().Start || Sequence >= Track.Length)))
		{
			continue;
		}
		LayerPose& Pose = Poses[Count];
		Pose = { {}, 1, Layer, Track.Mask, std::clamp(Curves::LaneScalar(Target.Lanes, Param::LayerWeight, Layer, Local, Over.Weight), 0.0f, 1.0f), Track.Channel };
		if (Spans.empty())
		{
			Pose.Items[0] = { IdleMotion(), 0.0f, 1.0f };
			Pose.Count = Pose.Items[0].Motion.valid() ? 1 : 0;
		}
		else
		{
			const auto Upper = std::upper_bound(Spans.begin(), Spans.end(), Sequence, [](float Moment, const BlockSpan& Candidate) { return Moment < Candidate.Start; });
			const size_t Index = Upper == Spans.begin() ? 0 : static_cast<size_t>(Upper - Spans.begin()) - 1;
			const BlockSpan& Span = Spans[Index];
			const float Offset = Sequence - Span.Start;
			const float Fade = Index > 0 && Offset >= 0.0f && Offset < Span.Overlap ? Curves::Ease(Span.BlendEase, Offset / Span.Overlap) : 1.0f;
			const BlockSpan& Before = Spans[Index > 0 ? Index - 1 : 0];
			const float Rise = Ramp(Span, Offset);
			const bool IsHeld = Index > 0 && Fade >= 1.0f && Rise < 1.0f && Offset < Span.FadeIn;
			const float Near = Fade * Rise, Far = Fade < 1.0f ? (1.0f - Fade) * Ramp(Before, Sequence - Before.Start) : IsHeld ? (1.0f - Rise) * Ramp(Before, Sequence - Before.Start) : 0.0f, Sum = Near + Far;
			Pose.Items[0] = { Span.Motion, Actors::FrameAt(Span, Offset) * Span.RawScale, Sum > 0.0f ? Near / Sum : 1.0f };
			Pose.Items[1] = { Before.Motion, Actors::FrameAt(Before, Sequence - Before.Start) * Before.RawScale, Sum > 0.0f ? Far / Sum : 0.0f };
			Pose.Count = Fade < 1.0f || IsHeld ? 2 : 1;
			Pose.Weight *= std::min(Sum, 1.0f);
			const xr_vector<ActorBlock>& Blocks = Over.Blocks;
			for (u32 Slot = 0; Slot < Pose.Count; ++Slot)
			{
				const BlockSpan& Played = Slot ? Before : Span;
				const s32 Clip = Played.Block < Blocks.size() && IsClipBlock(Blocks[Played.Block].Source) ? FindClip(Data.Clips, Blocks[Played.Block].Name) : -1;
				if (Clip >= 0 && Pose.Weight * Pose.Items[Slot].Alpha > 0.0f)
				{
					Samples[SampleCount++] = { Clip, Actors::FrameAt(Played, Sequence - Played.Start), Pose.Weight * Pose.Items[Slot].Alpha };
				}
			}
		}
		Count += Pose.Count && Pose.Weight > 0.0f ? 1 : 0;
	}
	MixParts(Poses, Count);
	const bool ShouldPin = Target.Actor.Hud.size() && Target.Attach.Parent.ObjectId;
	if (Skeleton && IsRootPinned && !ShouldPin)
	{
		Skeleton->LL_GetBoneInstance(Skeleton->LL_GetBoneRoot()).set_callback_overwrite(false);
	}
	IsRootPinned = ShouldPin;
	PinRoot();
	if (IsSolo && Skeleton)
	{
		ReleaseRig();
		Skeleton->CalculateBones_Invalidate();
		Skeleton->CalculateBones(true);
		return true;
	}
	Pose(Target, Local);
	return true;
}

void ActorRuntime::Pose(const Object& Target, float Local)
{
	const xr_vector<PoseRow>& Rows = Target.Actor.Poses;
	const ClipSample Forced { Preview.Clip, Preview.Time, 1.0f };
	const bool IsForced = Preview.Frame + 1 >= Device.dwFrame && static_cast<u32>(Preview.Clip) < Target.Actor.Clips.size();
	const ClipSample* Played = IsForced ? &Forced : Samples;
	const u32 PlayedCount = IsForced ? 1 : SampleCount;
	if (!Skeleton || (Rows.empty() && !PlayedCount && !IsRigged))
	{
		return;
	}
	const u16 Count = Skeleton->LL_BoneCount();
	if (Solves.size() != Count)
	{
		xr_vector<Fmatrix> Rest;
		Skeleton->LL_GetBindTransform(Rest);
		if (Rest.size() < Count)
		{
			return;
		}
		Solves.assign(Count, BoneSolve {});
		BindLocals.resize(Count);
		for (u16 Bone = 0; Bone < Count; ++Bone)
		{
			const u16 Parent = Skeleton->GetBoneData(Bone).GetParentID();
			Fmatrix Inverse = Fidentity;
			Inverse.invert(Parent < Count ? Rest[Parent] : Fidentity);
			BindLocals[Bone].mul_43(Inverse, Rest[Bone]);
			Solves[Bone].Parent = Parent < Count ? Parent : BI_NONE;
			Solves[Bone].Model = Skeleton;
		}
	}
	for (BoneSolve& Solve : Solves)
	{
		Solve.Add = Fidentity;
		Solve.Scale.set(1.0f, 1.0f, 1.0f);
		Solve.Weight = 0.0f;
	}
	for (u32 Row = 0; Row < Rows.size(); ++Row)
	{
		const PoseRow& Item = Rows[Row];
		const float Weight = Item.IsMuted || (IsForced && IsPoseOnly) ? 0.0f : std::clamp(Curves::LaneScalar(Target.Lanes, Param::PoseWeight, static_cast<u8>(Row), Local, Item.Weight), 0.0f, 1.0f);
		PoseBones(Item.Bones, Target.Lanes, Row, Item.Mode, Weight, Local);
	}
	for (u32 Index = 0; Index < PlayedCount; ++Index)
	{
		const ClipSample& Sample = Played[Index];
		if (static_cast<u32>(Sample.Clip) < Target.Actor.Clips.size())
		{
			const AnimClip& Item = Target.Actor.Clips[Sample.Clip];
			PoseBones(Item.Bones, Item.Lanes, 0, Item.Mode, std::clamp(Sample.Weight, 0.0f, 1.0f), Sample.Time);
			for (const ClipLayer& Over : Item.Layers)
			{
				PoseBones(Over.Bones, Over.Lanes, 0, Over.Mode, Over.IsMuted ? 0.0f : std::clamp(Sample.Weight * Over.Weight, 0.0f, 1.0f), Sample.Time);
			}
		}
	}
	for (u16 Bone = 0; Bone < Count; ++Bone)
	{
		BoneSolve& Solve = Solves[Bone];
		Solve.Bind = BindPose.size() == Count ? &BindPose[Bone] : nullptr;
		Skeleton->LL_GetBoneInstance(Bone).set_callback(bctCustom, Actors::RigCallback, &Solve, Solve.Bind != nullptr);
	}
	IsRigged = true;
	PinRoot();
	Skeleton->CalculateBones_Invalidate();
	Skeleton->CalculateBones(true);
	if (Rows.empty() && !PlayedCount)
	{
		ReleaseRig();
	}
}

void ActorRuntime::PinRoot()
{
	if (!Skeleton || !IsRootPinned)
	{
		return;
	}
	CBoneInstance& Root = Skeleton->LL_GetBoneInstance(Skeleton->LL_GetBoneRoot());
	Root.reset_callback();
	Root.set_callback_overwrite(true);
	Root.mTransform.identity();
}

bool ActorRuntime::SetSolo(const shared_str& Source, const shared_str& Name, float& Length)
{
	SoloMotion = Animated && Name.size() ? FindMotion(Source, Name) : MotionID();
	Length = SoloMotion.valid() ? Animated->get_animation_length(SoloMotion) : 0.0f;
	return SoloMotion.valid();
}

bool ActorRuntime::ClipMoment(s32 Clip, float& Time) const
{
	float Best = 0.0f;
	for (u32 Index = 0; Index < SampleCount; ++Index)
	{
		if (Samples[Index].Clip == Clip && Samples[Index].Weight > Best)
		{
			Best = Samples[Index].Weight;
			Time = Samples[Index].Time;
		}
	}
	return Best > 0.0f;
}

void ActorRuntime::PoseBones(const xr_vector<PoseBone>& Bones, const xr_vector<Lane>& Lanes, u32 Row, RowMode Mode, float Weight, float Time)
{
	const u16 Count = static_cast<u16>(Solves.size());
	for (u32 Index = 0; Weight > 0.0f && Index < Bones.size(); ++Index)
	{
		const u16 Bone = FindBone(Bones[Index].Name);
		if (Bone >= Count)
		{
			continue;
		}
		const u8 Layer = PoseLayer(Row, Index);
		const Fvector Turn = Curves::LaneVector(Lanes, Param::BoneRotation, Time, Bones[Index].Rotation, Layer);
		const Fvector Shift = Curves::LaneVector(Lanes, Param::BonePosition, Time, Bones[Index].Offset, Layer);
		const float Scale = Mode == RowMode::Add ? Weight : 1.0f;
		Fmatrix Delta;
		Delta.setHPB(deg2rad(Turn.x * Scale), deg2rad(Turn.y * Scale), deg2rad(Turn.z * Scale));
		Delta.c.mul(Shift, Scale);
		BoneSolve& Solve = Solves[Bone];
		const Fvector Size = Curves::LaneVector(Lanes, Param::BoneScale, Time, Bones[Index].Scale, Layer);
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			Solve.Scale[Axis] *= 1.0f + (std::clamp(Size[Axis], 0.01f, 100.0f) - 1.0f) * Weight;
		}
		if (Mode == RowMode::Add)
		{
			Solve.Add.mulB_43(Delta);
			continue;
		}
		Solve.Over.mul_43(BindLocals[Bone], Delta);
		Solve.Weight = Weight;
	}
}

void ActorRuntime::Locals(xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts) const
{
	for (u16 Bone = 0, Count = BoneCount(); Bone < Count; ++Bone)
	{
		const u16 Parent = BoneParent(Bone);
		Fmatrix Inverse = Fidentity, Local;
		Inverse.invert(Parent < Count ? Skeleton->LL_GetTransform(Parent) : Fidentity);
		Local.mul_43(Inverse, Skeleton->LL_GetTransform(Bone));
		Local.i.normalize_safe();
		Local.j.normalize_safe();
		Local.k.normalize_safe();
		Turns.push_back(Fquaternion().set(Local).normalize());
		Shifts.push_back(Local.c);
	}
}

void ActorRuntime::Dress(const Object& Target, float Local, const xr_vector<shared_str>& Shaders)
{
	const xr_vector<ActorLook>& Looks = Target.Actor.Looks;
	const u32 Look = Curves::LookAt(Target.Lanes, Local);
	const ActorLook* Active = Look < Looks.size() ? &Looks[Look] : nullptr;
	for (u32 Child = 0; Child < ItemSurfaceBase + MaxLookSurfaces; Child = SurfaceAt(Child) ? Child + 1 : Child < ItemSurfaceBase ? ItemSurfaceBase : ItemSurfaceBase + MaxLookSurfaces)
	{
		IRenderVisual* Surface = SurfaceAt(Child);
		if (!Surface)
		{
			continue;
		}
		const SurfaceShader* Entry = Active ? FindSurface(*Active, Child) : nullptr;
		const bool IsKnown = Entry && Entry->Shader.size() && std::binary_search(Shaders.begin(), Shaders.end(), Entry->Shader, ShaderLess);
		const shared_str Shader = IsKnown ? Entry->Shader : Surface->getOrigShaderName();
		const shared_str Texture = Entry && Entry->Texture.size() ? Entry->Texture : Surface->getOrigTextureName();
		if (Surface->getShaderName() != Shader || Surface->getTextureName() != Texture)
		{
			Surface->set_shader(Shader);
			Surface->set_texture(Texture);
			Surface->reload_shader();
		}
	}
}

IRenderVisual* ActorRuntime::SurfaceAt(u32 Surface) const
{
	const bool IsItem = Surface >= ItemSurfaceBase;
	const u32 Index = Surface - (IsItem ? ItemSurfaceBase : 0);
	IRenderVisual* Owner = IsItem ? ItemModel.Get() : Model.Get();
	IKinematics* Parts = IsItem ? (Owner ? Owner->dcast_PKinematics() : nullptr) : Skeleton;
	if (!Parts)
	{
		return IsItem && Index == 0 && Owner && Owner->getOrigShaderName().size() ? Owner : nullptr;
	}
	const xr_vector<IRenderVisual*>& Children = Parts->LL_GetChilds();
	return Index < std::min(static_cast<u32>(Children.size()), MaxLookSurfaces) ? Children[Index] : nullptr;
}

bool ActorRuntime::SurfaceNames(u32 Surface, shared_str& Shader, shared_str& Texture) const
{
	IRenderVisual* Item = SurfaceAt(Surface);
	if (!Item)
	{
		return false;
	}
	Shader = Item->getOrigShaderName();
	Texture = Item->getOrigTextureName();
	return true;
}

void ActorRuntime::SolveIk(const Object& Target, float Local, const Fmatrix& World)
{
	Fmatrix ToModel;
	ToModel.invert(World);
	const auto Solve = [&](const IkLimb* Limbs, const xr_vector<Lane>& Lanes, float Time, float Share)
	{
		for (u8 Limb = 0; Skeleton && Limb < MaxIkLimbs; ++Limb)
		{
			const IkLimb& Item = Limbs[Limb];
			const u16 Chain[3] = { FindBone(Item.Bones[0]), FindBone(Item.Bones[1]), FindBone(Item.Bones[2]) };
			const float Weight = Item.IsOn ? Share * std::clamp(Curves::LaneScalar(Lanes, Param::IkWeight, Limb, Time, Item.Weight), 0.0f, 1.0f) : 0.0f;
			if (Weight <= 0.0f || std::find(Chain, Chain + 3, BI_NONE) != Chain + 3)
			{
				continue;
			}
			const Fvector Turn = Curves::LaneVector(Lanes, Param::IkTurn, Time, Item.Turn, Limb);
			Fmatrix Goal;
			Goal.setHPB(deg2rad(Turn.x), deg2rad(Turn.y), deg2rad(Turn.z));
			Goal.c = Curves::LaneVector(Lanes, Param::IkTarget, Time, Item.Target, Limb);
			Fvector Pole = Curves::LaneVector(Lanes, Param::IkPole, Time, Item.Pole, Limb);
			if (Item.Space == IkSpace::World)
			{
				Goal.mulA_43(ToModel);
				Goal.i.normalize_safe();
				Goal.j.normalize_safe();
				Goal.k.normalize_safe();
				ToModel.transform_tiny(Pole);
			}
			CCameraRig::SolveLimb(Skeleton, Chain, Goal, Weight, &Pole);
		}
	};
	const ClipSample Forced { Preview.Clip, Preview.Time, 1.0f };
	const bool IsForced = Preview.Frame + 1 >= Device.dwFrame && static_cast<u32>(Preview.Clip) < Target.Actor.Clips.size();
	const ClipSample* Played = IsForced ? &Forced : Samples;
	for (u32 Index = 0; Index < (IsForced ? 1u : SampleCount); ++Index)
	{
		if (static_cast<u32>(Played[Index].Clip) < Target.Actor.Clips.size())
		{
			const AnimClip& Clip = Target.Actor.Clips[Played[Index].Clip];
			Solve(Clip.Ik, Clip.IkLanes, Played[Index].Time, std::clamp(Played[Index].Weight, 0.0f, 1.0f));
		}
	}
	Solve(Target.Actor.Ik, Target.Lanes, Local, 1.0f);
}

void ActorRuntime::LookWith(u8 Groups, const Fvector& Point, float Weight, const Fmatrix& World)
{
	if (!Skeleton || Weight <= 0.0f)
	{
		return;
	}
	Fmatrix ToModel;
	ToModel.invert(World);
	Fvector Goal;
	ToModel.transform_tiny(Goal, Point);
	const auto Turn = [this](u16 Bone, const Fvector& Axis, float Angle)
	{
		if (Bone >= BoneCount() || std::fabs(Angle) <= EPS_S)
		{
			return;
		}
		const Fvector Pivot = Skeleton->LL_GetTransform(Bone).c;
		Fmatrix Delta;
		Delta.rotation(Axis, Angle);
		Fvector Moved;
		Delta.transform_dir(Moved, Pivot);
		Delta.c.sub(Pivot, Moved);
		CCameraRig::TransformBone(Skeleton, Bone, Delta);
	};
	float Yaw = 0.0f;
	const u16 Spine[3] = { FindBone("bip01_spine"), FindBone("bip01_spine1"), FindBone("bip01_spine2") };
	const u32 Links = static_cast<u32>(std::count_if(Spine, Spine + 3, [this](u16 Bone) { return Bone < BoneCount(); }));
	if ((Groups & LookTorso) && Links)
	{
		const Fvector Base = Skeleton->LL_GetTransform(*std::find_if(Spine, Spine + 3, [this](u16 Bone) { return Bone < BoneCount(); })).c;
		Yaw = Weight * std::clamp(atan2f(Goal.x - Base.x, Goal.z - Base.z), -PI / 3.0f, PI / 3.0f);
		for (u16 Bone : Spine)
		{
			Turn(Bone, Fvector().set(0.0f, 1.0f, 0.0f), Yaw / static_cast<float>(Links));
		}
	}
	const u16 Head = FindBone("bip01_head"), Neck = FindBone("bip01_neck");
	if ((Groups & LookHead) && Head < BoneCount())
	{
		const Fvector From = Fvector().set(std::sin(Yaw), 0.0f, std::cos(Yaw));
		Fvector To = Fvector().sub(Goal, Skeleton->LL_GetTransform(Head).c), Axis;
		To.normalize_safe();
		Axis.crossproduct(From, To);
		const float Sine = Axis.magnitude();
		if (Sine > EPS_S)
		{
			const float Angle = Weight * std::min(atan2f(Sine, From.dotproduct(To)), PI * 4.0f / 9.0f);
			Axis.div(Sine);
			const bool HasNeck = Neck < BoneCount();
			Turn(Neck, Axis, HasNeck ? 0.35f * Angle : 0.0f);
			Turn(Head, Axis, HasNeck ? 0.65f * Angle : Angle);
		}
	}
}

void ActorRuntime::Constrain(const BoneConstraint& Rule, float Weight, const Fmatrix& Goal, const Fmatrix& World)
{
	const u16 Bone = FindBone(Rule.Bone);
	if (Bone >= BoneCount())
	{
		return;
	}
	Fmatrix Current, Desired, Offset, Back, Delta, ToModel, Blended;
	Current.mul_43(World, Skeleton->LL_GetTransform(Bone));
	Current.i.normalize_safe();
	Current.j.normalize_safe();
	Current.k.normalize_safe();
	Offset.setHPB(deg2rad(Rule.Turn.x), deg2rad(Rule.Turn.y), deg2rad(Rule.Turn.z));
	Offset.c = Rule.Shift;
	Desired.mul_43(Goal, Offset);
	const Fvector Place = Rule.Kind == ConstraintKind::Point ? Desired.c : Current.c;
	Desired = Rule.Kind == ConstraintKind::Point || Rule.Kind == ConstraintKind::Aim ? Current : Desired;
	Desired.c = Rule.Kind == ConstraintKind::Parent ? Desired.c : Place;
	Fvector From, To, Axis;
	Current.transform_dir(From, Rule.Shift);
	To.sub(Goal.c, Current.c);
	From.normalize_safe();
	To.normalize_safe();
	Axis.crossproduct(From, To);
	const float Sine = Axis.magnitude();
	if (Rule.Kind == ConstraintKind::Aim && Sine > EPS_S)
	{
		Fmatrix Swing;
		Swing.rotation(Axis.div(Sine), atan2f(Sine, From.dotproduct(To)));
		Desired.mulA_43(Swing);
		Desired.c = Current.c;
	}
	Fquaternion A, B, Mixed;
	A.set(Current);
	B.set(Desired);
	Mixed.slerp(A, B, Weight);
	Blended.rotation(Mixed);
	Blended.c.lerp(Current.c, Desired.c, Weight);
	Back.invert(Current);
	Delta.mul_43(Blended, Back);
	ToModel.invert(World);
	Delta.mulA_43(ToModel);
	Delta.mulB_43(World);
	CCameraRig::TransformBone(Skeleton, Bone, Delta);
}

void ActorRuntime::ReleaseRig()
{
	for (u16 Bone = 0, Count = IsRigged && Skeleton ? Skeleton->LL_BoneCount() : 0; Bone < Count; ++Bone)
	{
		CBoneInstance& Instance = Skeleton->LL_GetBoneInstance(Bone);
		if (Instance.callback() == Actors::RigCallback)
		{
			BindPose.size() == Count ? Instance.set_callback(bctCustom, Actors::BindPoseCallback, &BindPose[Bone], true) : Instance.reset_callback();
		}
	}
	IsRigged = false;
	Solves.clear();
}

u16 ActorRuntime::FindBone(const shared_str& Name) const
{
	return Skeleton && Name.size() ? Skeleton->LL_BoneID(Name) : BI_NONE;
}

u16 ActorRuntime::BoneParent(u16 Bone) const
{
	return Skeleton && Bone < BoneCount() ? Skeleton->GetBoneData(Bone).GetParentID() : BI_NONE;
}

bool ActorRuntime::BoneWorld(u16 Bone, const Fmatrix& World, Fmatrix& Out) const
{
	if (!Skeleton || Bone >= BoneCount())
	{
		return false;
	}
	Out.mul_43(World, Skeleton->LL_GetTransform(Bone));
	return true;
}

bool ActorRuntime::RestPose(xr_vector<Fmatrix>& Model) const
{
	Model.clear();
	if (Skeleton)
	{
		Skeleton->LL_GetBindTransform(Model);
	}
	return Skeleton && Model.size() >= Skeleton->LL_BoneCount();
}

bool ActorRuntime::RestLocal(u16 Bone, Fmatrix& Out) const
{
	if (Bone >= BindLocals.size())
	{
		return false;
	}
	Out = BindLocals[Bone];
	return true;
}

bool ActorRuntime::RestModel(u16 Bone, Fmatrix& Out) const
{
	if (Bone >= BindLocals.size() || Bone >= Solves.size())
	{
		return false;
	}
	Out = BindLocals[Bone];
	for (u16 Parent = Solves[Bone].Parent, Depth = 0; Parent < BindLocals.size() && Depth < 256; Parent = Solves[Parent].Parent, ++Depth)
	{
		Out.mulA_43(BindLocals[Parent]);
	}
	return true;
}

void ActorRuntime::Place(const Fmatrix& Transform)
{
	Placement = Transform;
	if (!IsShown)
	{
		Body.Unbind();
		return;
	}
	Body.Bind(Model.Get(), Transform);
	PlaceItem();
}

bool ActorRuntime::BoneFrame(const shared_str& Bone, Fmatrix& Out) const
{
	Out = Placement;
	const u16 Index = Skeleton && Bone.size() ? Skeleton->LL_BoneID(Bone) : BI_NONE;
	if (Index < BoneCount())
	{
		Skeleton->LL_GetBoneWorldTransform(Index, Placement, Out);
	}
	return !Bone.size() || Index < BoneCount();
}

void ActorRuntime::Bones(const Fmatrix& World, xr_vector<Fvector>& Lines) const
{
	for (u16 Bone = 0; IsShown && Bone < BoneCount(); ++Bone)
	{
		const u16 Parent = Skeleton->GetBoneData(Bone).GetParentID();
		if (Parent < BoneCount())
		{
			Fvector From, To;
			World.transform_tiny(From, Skeleton->LL_GetTransform(Bone).c);
			World.transform_tiny(To, Skeleton->LL_GetTransform(Parent).c);
			Lines.push_back(From);
			Lines.push_back(To);
		}
	}
}

u16 ActorRuntime::BoneCount() const
{
	return Skeleton ? Skeleton->LL_BoneCount() : 0;
}

const char* ActorRuntime::BoneName(u16 Bone) const
{
	return Bone < BoneCount() ? Skeleton->LL_BoneName_dbg(Bone) : nullptr;
}

const char* ActorRuntime::PartName(u16 Part) const
{
	return Animated && Part < MAX_PARTS && Animated->partitions().part(Part).Name.size() ? Animated->partitions().part(Part).Name.c_str() : nullptr;
}

bool ActorRuntime::LoadModel(const shared_str& Visual)
{
	Body.Unbind();
	Body.ItemVisual = nullptr;
	ReleaseRig();
	ClearBindPose();
	Model.Reset();
	Skeleton = nullptr;
	Animated = nullptr;
	ModelSources.clear();
	Adapted.clear();
	IsPoseValid = IsIdleKnown = false;
	NamedParts = 0;
	if (!Visual.size() || !::Render)
	{
		return false;
	}
	xr_string Message;
	if (!Omf::CheckVisual(Visual.c_str(), Message))
	{
		Error = Message.c_str();
		Msg("! Cinema: %s", Message.c_str());
		return false;
	}
	Model = VisualHandle(::Render->model_Create(Visual.c_str()));
	if (!Model.Get())
	{
		Error.printf("Could not create model %s", Visual.c_str());
		return false;
	}
	Skeleton = Model.Get()->dcast_PKinematics();
	Animated = Model.Get()->dcast_PKinematicsAnimated();
	if (Animated)
	{
		Animated->SetUpdateTracksCalback(&Actors::Frozen);
		Animated->LL_SetInertialization(0.0f);
		for (u16 Part = 0; Part < MAX_PARTS; ++Part)
		{
			NamedParts |= PartName(Part) ? static_cast<u8>(1u << Part) : 0u;
		}
		for (u16 Slot = 0, Count = Animated->LL_MotionsSlotCount(); Slot < Count; ++Slot)
		{
			shared_motions Motions = Animated->LL_MotionsSlot(Slot);
			if (Motions.motion_map())
			{
				ModelSources.push_back(Motions.id());
			}
		}
	}
	if (Skeleton)
	{
		Skeleton->CalculateBones(true);
		Skeleton->CalculateBBox(true);
	}
	return true;
}

void ActorRuntime::LoadItem(const shared_str& Section)
{
	Body.ItemVisual = nullptr;
	ItemModel.Reset();
	Item = ItemSetup();
	if (!::Render || !Actors::ItemValid(Section))
	{
		return;
	}
	ItemModel = VisualHandle(::Render->model_Create(pSettings->read_if_exists<str_c>(Section, "visual", "")));
	if (!ItemModel.Get())
	{
		return;
	}
	const char* Name = Section.c_str();
	Item.IsWeaponStyle = pSettings->line_exist(Name, "position") && pSettings->line_exist(Name, "orientation");
	Item.HandDependence = Item.IsWeaponStyle ? pSettings->read_if_exists<s32>(Name, "hand_dependence", 2) : 2;
	Item.BoneName = Item.IsWeaponStyle ? nullptr : pSettings->read_if_exists<str_c>(Name, "attach_bone_name", nullptr);
	Item.Position = pSettings->read_if_exists<Fvector3>(Name, Item.IsWeaponStyle ? "position" : "attach_position_offset", Fvector().set(0.0f, 0.0f, 0.0f));
	Item.Rotation = pSettings->read_if_exists<Fvector3>(Name, Item.IsWeaponStyle ? "orientation" : "attach_angle_offset", Fvector().set(0.0f, 0.0f, 0.0f));
	if (!Skeleton)
	{
		return;
	}
	if (!Item.IsWeaponStyle)
	{
		Item.BoneAttach = Item.BoneName.size() ? Skeleton->LL_BoneID(Item.BoneName) : BI_NONE;
		return;
	}
	const auto FindBone = [this](const char* Preferred, u16 Fallback)
	{
		const u16 Found = Skeleton->LL_BoneID(Preferred);
		return Found == BI_NONE ? Fallback : Found;
	};
	Item.BoneR = FindBone("bip01_r_finger1", Skeleton->LL_BoneID("bip01_r_hand"));
	Item.BoneL = FindBone("bip01_l_finger1", Skeleton->LL_BoneID("bip01_l_hand"));
	Item.BoneR2 = FindBone("bip01_r_finger11", Item.BoneR);
}

void ActorRuntime::BuildSources(const xr_vector<shared_str>& Packs)
{
	Sources.clear();
	Failed.clear();
	IsIdleKnown = false;
	if (!Animated)
	{
		return;
	}
	const auto IsListed = [this](const shared_str& Id) { return std::any_of(Sources.begin(), Sources.end(), [&Id](const AnimationSource& Candidate) { return Candidate.Id == Id; }); };
	const auto Append = [](AnimationSource& Target, accel_map* Map, bool IsFx)
	{
		if (!Map)
		{
			return;
		}
		for (const auto& Entry : *Map)
		{
			Target.Clips.push_back({ Entry.first, IsFx });
		}
	};
	for (u16 Slot = 0, Count = Animated->LL_MotionsSlotCount(); Slot < Count; ++Slot)
	{
		shared_motions Motions = Animated->LL_MotionsSlot(Slot);
		if (!Motions.motion_map() || IsListed(Motions.id()) || std::any_of(Adapted.begin(), Adapted.end(), [&Motions](const AdaptedSource& Candidate) { return Candidate.Loaded == Motions.id(); }))
		{
			continue;
		}
		AnimationSource& Source = Sources.emplace_back();
		Source.Id = Motions.id();
		Append(Source, Motions.cycle(), false);
		Append(Source, Motions.fx(), true);
		if (Source.Clips.empty())
		{
			Sources.pop_back();
		}
	}
	for (const shared_str& Path : Packs)
	{
		if (!Path.size() || IsListed(Path))
		{
			continue;
		}
		AnimationSource Source;
		if (Omf::IndexAnimationFile(Path.c_str(), Source))
		{
			Sources.push_back(std::move(Source));
		}
		else
		{
			Msg("! Cinema: cannot index animation pack %s", Path.c_str());
		}
	}
	std::sort(Sources.begin(), Sources.end(), [](const AnimationSource& Left, const AnimationSource& Right) { return xr_strcmp(Left.Id, Right.Id) < 0; });
}

bool ActorRuntime::EnsureSource(const shared_str& Source, u32& RetargetIndex)
{
	if (!Animated || !Source.size() || std::find(Failed.begin(), Failed.end(), Source) != Failed.end())
	{
		return false;
	}
	const auto Fail = [this, &Source](const char* Text)
	{
		Failed.push_back(Source);
		Msg("! Cinema: %s: %s", Source.c_str(), Text);
		return false;
	};
	const bool IsListed = std::any_of(Sources.begin(), Sources.end(), [&Source](const AnimationSource& Candidate) { return Candidate.Id == Source; });
	if (IsListed && Actors::HasSlot(Animated, Actors::LoadedId(Adapted, Source)))
	{
		return true;
	}
	if (!IsListed || !Omf::IsOmfPath(Source))
	{
		return Fail("animation source is unavailable");
	}
	if (Animated->LL_MotionsSlotCount() >= MAX_ANIM_SLOT)
	{
		return Fail("the model has reached the animation slot limit");
	}
	CMemoryWriter Converted;
	xr_string Message;
	if (!Omf::BuildAdaptedMotion(Skeleton, Animated, Source.c_str(), Converted, Message))
	{
		return Fail(Message.c_str());
	}
	string_path Temporary = {}, File = {}, FullPath = {};
	bool IsReserved = false;
	for (u32 Attempt = 0; Attempt < 100 && !IsReserved; ++Attempt)
	{
		xr_sprintf(Temporary, "cinema_editor_cache\\retarget_%08x_%08x", Device.dwTimeGlobal, ++RetargetIndex);
		xr_strconcat(File, Temporary, ".omf");
		IsReserved = !FS.exist(_game_meshes_, File) && !FS.exist("$level$", File);
	}
	if (!IsReserved)
	{
		return Fail("could not reserve a temporary animation path");
	}
	IWriter* Writer = FS.w_open(_game_meshes_, File);
	if (!Writer || !Writer->valid())
	{
		if (Writer)
		{
			FS.w_close(Writer);
		}
		return Fail("could not write the temporary animation");
	}
	Writer->w(Converted.pointer(), Converted.size());
	FS.w_close(Writer, true);
	const u16 Count = Animated->LL_MotionsSlotCount();
	Animated->append_motion_from_path(AppliedVisual.c_str(), Temporary);
	IsPoseValid = false;
	FS.update_path(FullPath, _game_meshes_, File);
	FS.file_delete(FullPath);
	if (Animated->LL_MotionsSlotCount() <= Count)
	{
		return Fail("could not load the adapted animation");
	}
	Adapted.push_back({ Source, shared_str(File) });
	Msg("* Cinema: retargeted %s to %s", Source.c_str(), AppliedVisual.c_str());
	return true;
}

MotionID ActorRuntime::FindMotion(const shared_str& Source, const shared_str& Name) const
{
	if (!Animated)
	{
		return MotionID();
	}
	const shared_str& Loaded = Actors::LoadedId(Adapted, Source);
	for (u16 Slot = 0, Count = Animated->LL_MotionsSlotCount(); Slot < Count; ++Slot)
	{
		shared_motions Motions = Animated->LL_MotionsSlot(Slot);
		accel_map* Cycles = Motions.cycle();
		if (!Cycles || Motions.id() != Loaded)
		{
			continue;
		}
		const auto Entry = Cycles->find(Name);
		const MotionID Motion = Entry == Cycles->end() ? MotionID() : MotionID(Slot, Entry->second);
		if (Motion.valid() && Animated->LL_GetMotionDef(Motion))
		{
			return Motion;
		}
	}
	return MotionID();
}

float ActorRuntime::MotionLength(const shared_str& Source, const shared_str& Name) const
{
	if (IsClipBlock(Source))
	{
		const s32 Clip = FindClip(AppliedClips, Name);
		return Clip < 0 ? 0.0f : AppliedClips[Clip].Length + END_EPS;
	}
	const MotionID Motion = FindMotion(Source, Name);
	return Motion.valid() ? Animated->get_animation_length(Motion) : 0.0f;
}

float ActorRuntime::BlockDuration(const ActorBlock& Block) const
{
	float In = 0.0f, Out = 0.0f;
	const float Play = Actors::Measure(Block, MotionLength(Block.Source, Block.Name), In, Out);
	return Play >= 0.0f ? Play * std::clamp(Block.Loops, 1.0f, MaxLoops) + std::max(Block.Hold, 0.0f) : 0.0f;
}

bool ActorRuntime::PickSphere(Fsphere& Sphere) const
{
	IRenderVisual* Visual = Body.renderable.visual;
	if (!Visual)
	{
		return false;
	}
	const Fmatrix& Transform = Body.renderable.xform;
	const Fsphere& Bounds = Visual->getVisData().sphere;
	Transform.transform_tiny(Sphere.P, Bounds.P);
	Sphere.R = (Bounds.R < 0.2f ? 0.5f : Bounds.R) * std::max({ Transform.i.magnitude(), Transform.j.magnitude(), Transform.k.magnitude() });
	return true;
}

void ActorRuntime::BuildTracks(const Object& Target, u32& RetargetIndex)
{
	const ActorData& Data = Target.Actor;
	AppliedClips = Data.Clips;
	SequenceLength = 0.0f;
	Unresolved = 0;
	u32 Added = 0;
	for (u8 Layer = 0; Layer <= MaxActorLayers; ++Layer)
	{
		LayerRuntime& Track = Tracks[Layer];
		const ActorLayer* Over = Layer <= Data.Layers.size() ? &RowOf(Data, Layer) : nullptr;
		Track.Spans.clear();
		Track.Starts.clear();
		Track.Ticks.clear();
		Track.Length = 0.0f;
		Track.Mask = !Layer && Over->Parts.empty() ? NamedParts : 0;
		Track.Channel = Over && Over->Mode == RowMode::Add && Added < MaxAddRows ? Actors::AddChannels[Added++] : 0;
		if (!Over)
		{
			continue;
		}
		BuildSpans(Target, BlocksOf(Data, Layer), Track, RetargetIndex);
		for (u16 Part = 0; Over && Part < MAX_PARTS; ++Part)
		{
			const char* Name = PartName(Part);
			Track.Mask |= Name && std::any_of(Over->Parts.begin(), Over->Parts.end(), [Name](const shared_str& Wanted) { return !xr_strcmp(Wanted.c_str(), Name); }) ? static_cast<u8>(1u << Part) : 0u;
		}
		SequenceLength = std::max(SequenceLength, Over->IsMuted ? 0.0f : Track.Length);
	}
}

void ActorRuntime::BuildSpans(const Object& Target, const xr_vector<ActorBlock>& Blocks, LayerRuntime& Track, u32& RetargetIndex)
{
	const ActorData& Data = Target.Actor;
	xr_vector<BlockSpan>& Spans = Track.Spans;
	xr_vector<MarkTick>& Ticks = Track.Ticks;
	float Begin = 0.0f, End = 0.0f;
	for (u32 Index = 0; Index < Blocks.size(); ++Index)
	{
		const ActorBlock& Block = Blocks[Index];
		Begin = Data.IsRelative ? std::max(Begin, End + Block.Start) : static_cast<float>(Curves::LaneArea(Target.Lanes, &TimeScaleArea, Param::TimeScale, 0, std::max(Block.Start, 0.0f), Data.TimeScale));
		End = Begin;
		Track.Starts.push_back(Begin);
		const bool IsClip = IsClipBlock(Block.Source);
		const MotionID Motion = !IsClip && EnsureSource(Block.Source, RetargetIndex) ? FindMotion(Block.Source, Block.Name) : MotionID();
		float In = 0.0f, Out = 0.0f;
		const float Play = Actors::Measure(Block, IsClip ? MotionLength(Block.Source, Block.Name) : Motion.valid() ? Animated->get_animation_length(Motion) : 0.0f, In, Out);
		Unresolved += Play < 0.0f;
		if (!(Play >= 0.0f && (Play > 0.0f || Block.Hold > 0.0f)))
		{
			continue;
		}
		const float Speed = std::fabs(Block.Speed);
		const BlockSpan* Before = Spans.empty() ? nullptr : &Spans.back();
		const float Previous = Before ? Before->Start + Before->Duration : 0.0f, Width = Before ? Before->Duration : 0.0f;
		BlockSpan& Span = Spans.emplace_back();
		Span.Start = Begin;
		Span.Play = Play;
		Span.Loops = std::clamp(Block.Loops, 1.0f, MaxLoops);
		Span.Duration = Play * Span.Loops + std::max(Block.Hold, 0.0f);
		Span.Overlap = std::clamp(Previous - Begin, 0.0f, std::min(Width, Span.Duration));
		Span.From = Block.Speed >= 0.0f ? In : Out;
		Span.To = Block.Speed >= 0.0f ? Out : In;
		CMotionDef* Definition = Motion.valid() ? Animated->LL_GetMotionDef(Motion) : nullptr;
		Span.RawScale = Definition ? Definition->Speed() : 1.0f;
		Span.BlendEase = Block.BlendEase;
		Span.FadeIn = std::max(Block.FadeIn, 0.0f);
		Span.FadeOut = std::max(Block.FadeOut, 0.0f);
		Span.Motion = Motion;
		Span.Block = Index;
		End = Begin + Span.Duration;
		Track.Length = std::max(Track.Length, End);
		if (!Definition)
		{
			continue;
		}
		for (const motion_marks& Mark : Definition->marks)
		{
			for (const motion_marks::interval& Range : Mark.intervals)
			{
				const float Moment = Span.RawScale > EPS_S ? Range.first / Span.RawScale : -1.0f;
				const float Base = std::fabs(Moment - Span.From) / Speed;
				for (u32 Lap = 0; Play > 0.0f && Moment >= In && Moment <= Out && Ticks.size() < MaxTicks && (!Lap || Base + Lap * Play <= Play * Span.Loops); ++Lap)
				{
					Ticks.push_back({ Begin + Base + Lap * Play, Mark.name });
				}
			}
		}
	}
	std::sort(Ticks.begin(), Ticks.end(), [](const MarkTick& A, const MarkTick& B) { return A.Sequence < B.Sequence; });
}

void ActorRuntime::MixParts(const LayerPose* Poses, u32 Count)
{
	if (!Animated)
	{
		return;
	}
	MixPart Next[MAX_PARTS];
	bool HasAny = false;
	for (u8 Channel = 0; Channel < MAX_CHANNELS; ++Channel)
	{
		float Factor = 0.0f;
		for (u16 Part = 0; Part < MAX_PARTS; ++Part)
		{
			MixPart& Now = Next[Part];
			Now.Count = 0;
			float Remain = 1.0f;
			for (u32 Index = Count; Index-- > 0 && Remain > 0.0f;)
			{
				const LayerPose& Pose = Poses[Index];
				if (Pose.Channel != Channel || !(Pose.Mask & (1u << Part)))
				{
					continue;
				}
				const u8 Before = Now.Count;
				Factor = Pose.Weight;
				for (u8 Slot = 0; Slot < Pose.Count; ++Slot)
				{
					const PoseSource& Item = Pose.Items[Slot];
					const u16 Only = Item.Motion.valid() ? Animated->LL_GetMotionDef(Item.Motion)->bone_or_part : BI_NONE;
					const float Weight = Channel ? Item.Alpha : Remain * Pose.Weight * Item.Alpha;
					if (Item.Motion.valid() && (Only == BI_NONE || Only == Part) && Weight > 0.0f && Now.Count < MaxMix)
					{
						Now.Entries[Now.Count++] = { Item.Motion, Pose.Layer, Slot, Item.Raw, Weight };
					}
				}
				Remain *= Now.Count > Before && !Channel ? 1.0f - Pose.Weight : 1.0f;
			}
			HasAny = HasAny || (!Channel && Now.Count);
		}
		const MotionID Fallback = Channel ? MotionID() : IdleMotion().valid() ? IdleMotion() : HasAny ? Poses[0].Items[0].Motion : MotionID();
		for (u16 Part = 0; Fallback.valid() && Part < MAX_PARTS; ++Part)
		{
			MixPart& Now = Next[Part];
			if (!Now.Count && (NamedParts & (1u << Part)))
			{
				Now.Entries[Now.Count++] = { Fallback, static_cast<u8>(MaxActorLayers + 1), 0, 0.0f, 1.0f };
				HasAny = true;
			}
		}
		for (u16 Part = 0; Part < MAX_PARTS; ++Part)
		{
			const MixPart& Now = Next[Part];
			MixPart& Was = Mixed[Channel][Part];
			bool IsSame = IsPoseValid && Now.Count == Was.Count;
			for (u8 Index = 0; IsSame && Index < Now.Count; ++Index)
			{
				IsSame = Now.Entries[Index].Motion == Was.Entries[Index].Motion && Now.Entries[Index].Layer == Was.Entries[Index].Layer && Now.Entries[Index].Slot == Was.Entries[Index].Slot;
			}
			if (!IsSame)
			{
				Animated->LL_CloseCycle(Part, static_cast<u8>(1u << Channel));
			}
			for (u8 Index = 0; Index < Now.Count; ++Index)
			{
				CBlend* Blend = IsSame ? Was.Entries[Index].Blend : Animated->LL_PlayCycle(Part, Now.Entries[Index].Motion, Index > 0, nullptr, nullptr, Channel);
				Was.Entries[Index] = Now.Entries[Index];
				Was.Entries[Index].Blend = Blend;
				if (Blend)
				{
					Blend->SmoothWeight = false;
					Blend->blendPower = 1.0f;
					Blend->blendAmount = Now.Entries[Index].Weight;
					Blend->timeCurrent = Now.Entries[Index].Raw;
					Blend->playing = false;
				}
			}
			Was.Count = Now.Count;
		}
		if (Channel > 1)
		{
			Animated->LL_SetChannelFactor(Channel, Factor);
		}
	}
	IsPoseValid = true;
	if (!HasAny)
	{
		SetBindPose();
		return;
	}
	ClearBindPose();
	Skeleton->CalculateBones_Invalidate();
	Skeleton->CalculateBones(true);
}

MotionID ActorRuntime::IdleMotion()
{
	if (IsIdleKnown)
	{
		return IdleId;
	}
	const auto Find = [this](const auto& IsMatch)
	{
		for (const AnimationSource& Source : Sources)
		{
			if (std::find(ModelSources.begin(), ModelSources.end(), Source.Id) == ModelSources.end())
			{
				continue;
			}
			for (const AnimationClip& Clip : Source.Clips)
			{
				if (!Clip.IsFx && IsMatch(Clip.Name.c_str()))
				{
					return FindMotion(Source.Id, Clip.Name);
				}
			}
		}
		return MotionID();
	};
	MotionID Motion = Find([](const char* Name) { return !_stricmp(Name, "$editor"); });
	for (const char* Wanted : Actors::IdleNames)
	{
		Motion = Motion.valid() ? Motion : Find([Wanted](const char* Name) { return !xr_strcmp(Name, Wanted); });
	}
	Motion = Motion.valid() ? Motion : Find(Actors::ContainsIdle);
	IdleId = Motion.valid() ? Motion : Find([](const char*) { return true; });
	IsIdleKnown = true;
	return IdleId;
}

bool ActorRuntime::SetBindPose()
{
	if (!Skeleton || !BindPose.empty())
	{
		return false;
	}
	const u16 Count = Skeleton->LL_BoneCount();
	Skeleton->LL_GetBindTransform(BindPose);
	if (BindPose.size() < Count)
	{
		BindPose.clear();
		return false;
	}
	for (u16 Bone = 0; Bone < Count; ++Bone)
	{
		Skeleton->LL_GetBoneInstance(Bone).set_callback(bctCustom, Actors::BindPoseCallback, &BindPose[Bone], true);
	}
	Skeleton->CalculateBones_Invalidate();
	Skeleton->CalculateBones(true);
	return true;
}

void ActorRuntime::ClearBindPose()
{
	if (BindPose.empty())
	{
		return;
	}
	for (u16 Bone = 0, Count = Skeleton ? Skeleton->LL_BoneCount() : 0; Bone < Count; ++Bone)
	{
		CBoneInstance& Instance = Skeleton->LL_GetBoneInstance(Bone);
		if (Instance.callback() == Actors::BindPoseCallback || Instance.callback() == Actors::RigCallback)
		{
			Instance.reset_callback();
		}
	}
	BindPose.clear();
}

void ActorRuntime::PlaceItem()
{
	Body.ItemVisual = nullptr;
	if (!ItemModel.Get() || !Skeleton)
	{
		return;
	}
	const Fmatrix& Placement = Body.renderable.xform;
	Fmatrix Offset;
	if (!Item.IsWeaponStyle)
	{
		if (Item.BoneAttach == BI_NONE)
		{
			return;
		}
		Fmatrix Bone;
		Skeleton->Bone_GetAnimPos(Bone, Item.BoneAttach, u8(-1), false);
		Offset.setHPB(Item.Rotation.x, Item.Rotation.y, Item.Rotation.z);
		Offset.translate_over(Item.Position);
		Body.ItemTransform.mul_43(Bone, Offset);
		Body.ItemTransform.mulA_43(Placement);
		Body.ItemVisual = ItemModel.Get();
		return;
	}
	const u16 RightBone = Item.BoneR;
	const u16 LeftBone = Item.HandDependence == 1 ? Item.BoneR2 : Item.BoneL;
	if (RightBone == BI_NONE || LeftBone == BI_NONE)
	{
		return;
	}
	const Fmatrix& Right = Skeleton->LL_GetTransform(RightBone);
	Fvector Direction;
	Direction.sub(Skeleton->LL_GetTransform(LeftBone).c, Right.c);
	Fmatrix Transform = Placement;
	if (fis_zero(Direction.magnitude()))
	{
		Placement.transform_tiny(Transform.c, Right.c);
	}
	else
	{
		Direction.normalize();
		Fvector Side, Normal;
		Side.crossproduct(Right.j, Direction).normalize();
		Normal.crossproduct(Direction, Side).normalize();
		Transform.set(Side, Normal, Direction, Right.c);
		Transform.mulA_43(Placement);
	}
	Offset.setHPB(deg2rad(Item.Rotation.x), deg2rad(Item.Rotation.y), deg2rad(Item.Rotation.z));
	Offset.translate_over(Item.Position);
	Body.ItemTransform.mul(Transform, Offset);
	Body.ItemVisual = ItemModel.Get();
}
}
