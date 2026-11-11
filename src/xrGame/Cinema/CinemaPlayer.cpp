#include "StdAfx.h"
#include "CinemaPlayer.h"
#include "CinemaApi.h"
#include "CinemaSceneIo.h"
#include "../Actor.h"
#include "../GamePersistent.h"
#include "../Level.h"
#include "../ai_space.h"
#include "../alife_simulator.h"
#include "../../xrEngine/CameraManager.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/XR_IOConsole.h"

namespace Cinema::PlayerLocal
{
xr_unique_ptr<Player> g_Player;
u32 g_Generation = 0;
const Curve NoCurve {};

constexpr float LightData::* LightScalars[7] = { &LightData::Intensity, &LightData::Range, &LightData::Cone, &LightData::VirtualSize, &LightData::VolumeIntensity, &LightData::VolumeDistance, &LightData::VolumeQuality };

template <typename T>
bool ById(const xr_unique_ptr<T>& Item, u32 Id)
{
	return Item->Id < Id;
}

template <typename T>
T* RuntimeOf(const xr_vector<xr_unique_ptr<T>>& Items, u32 Id)
{
	const auto Found = std::lower_bound(Items.begin(), Items.end(), Id, ById<T>);
	return Found != Items.end() && (*Found)->Id == Id ? Found->get() : nullptr;
}

template <typename T>
void Reconcile(xr_vector<xr_unique_ptr<T>>& Items, const Scene& Data, ObjectType Type, xr_vector<u32>& Ids)
{
	Ids.clear();
	for (const Object& Item : Data.Objects)
	{
		if (Item.Type == Type)
		{
			Ids.push_back(Item.Id);
		}
	}
	std::sort(Ids.begin(), Ids.end());
	std::erase_if(Items, [&Ids](const xr_unique_ptr<T>& Item) { return !std::binary_search(Ids.begin(), Ids.end(), Item->Id); });
	for (u32 Id : Ids)
	{
		const auto At = std::lower_bound(Items.begin(), Items.end(), Id, ById<T>);
		if (At == Items.end() || (*At)->Id != Id)
		{
			Items.insert(At, xr_make_unique<T>(Id));
		}
	}
}

const Curve& LaneCurve(const xr_vector<Lane>& Lanes, Param Id, u8 Layer)
{
	const Lane* Found = Curves::FindLane(Lanes, Id, Layer);
	return Found ? Found->Curves[0] : NoCurve;
}

template <typename TVisit>
void ForEachAttachment(const Scene& Data, const TVisit& Visit)
{
	for (const Object& Item : Data.Objects)
	{
		Visit(Item.Id, Item.Attach);
		for (const CameraBlock& Shot : Item.Camera.Blocks)
		{
			Visit(Item.Id, Shot.Attach);
		}
	}
}

void Lock(const Attachment& Links, Fmatrix& Parent)
{
	if (!Links.IsRollLocked && !Links.IsPitchLocked)
	{
		return;
	}
	Fvector Angles;
	const Fvector Origin = Parent.c;
	Parent.getHPB(Angles);
	Parent.setHPB(Angles.x, Links.IsPitchLocked ? 0.0f : Angles.y, Links.IsRollLocked ? 0.0f : Angles.z);
	Parent.c = Origin;
}

AttachLink LinkAt(const Attachment& Links, float Local)
{
	AttachLink Out = Links.Parent;
	for (const AttachSwitch& Item : Links.Switches)
	{
		Out.Bone = Item.Time <= Local ? Item.Bone : Out.Bone;
	}
	return Out;
}

Fmatrix PoseMatrix(const Fvector& Position, const Fvector& Rotation)
{
	Fmatrix Pose;
	Pose.setHPB(deg2rad(Rotation.x), deg2rad(Rotation.y), deg2rad(Rotation.z));
	Pose.c = Position;
	return Pose;
}

Fvector KeyValues(const xr_vector<Lane>& Lanes, Param Id, float Local, const Fvector& Static)
{
	const Lane* Found = Curves::FindLane(Lanes, Id, 0);
	return !Found ? Static : Fvector().set(Curves::Value(Found->Curves[0], Local, Static.x), Curves::Value(Found->Curves[1], Local, Static.y), Curves::Value(Found->Curves[2], Local, Static.z));
}

void Unpose(const Fmatrix& Pose, const Fvector& Reference, Fvector& Position, Fvector& Rotation)
{
	const ParamInfo &Place = Info(Param::Position), &Turn = Info(Param::Rotation);
	Fvector Angles;
	Pose.getHPB(Angles);
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		Position[Axis] = Sanitize(Pose.c[Axis], Place.Min, Place.Max, Position[Axis]);
		Rotation[Axis] = Sanitize(Reference[Axis] + std::remainder(rad2deg(Angles[Axis]) - Reference[Axis], 360.0f), Turn.Min, Turn.Max, Rotation[Axis]);
	}
}

void PutKey(Curve& Target, float Time, float NewValue, float HalfFrame)
{
	const u32 At = Curves::KeyAt(Target, Time, HalfFrame);
	if (At != u32(-1))
	{
		Target.Keys[At].Value = NewValue;
		return;
	}
	const auto Next = std::upper_bound(Target.Keys.begin(), Target.Keys.end(), Time, [](float Moment, const Key& Item) { return Moment < Item.Time; });
	Key Item { Time, NewValue };
	Item.Mode = Next == Target.Keys.begin() ? Item.Mode : (Next - 1)->Mode;
	Curves::PlaceKey(Target, Item, HalfFrame);
}
}

namespace Cinema
{
Player::Player()
{
	ResetLevel();
	Late.Owner = this;
	Device.seqFrame.Add(this, REG_PRIORITY_HIGH);
	Device.seqFrame.Add(&Late, REG_PRIORITY_LOW);
}

Player::~Player()
{
	SaveUnsaved();
	Release();
	RestoreDofSwitch();
	Device.seqFrame.Remove(&Late);
	Device.seqFrame.Remove(this);
}

void Player::OnFrame()
{
	if (TrackedLevel != g_pGameLevel || (TrackedLevel && WasLevelReady != TrackedLevel->bReady))
	{
		ResetLevel();
	}
	WasLevelReady = TrackedLevel && TrackedLevel->bReady;
	if (ShouldDropRuntimes)
	{
		DropRuntimes();
		ShouldDropRuntimes = false;
	}
	if (WasLevelReady)
	{
		Advance();
		Sync();
		EvaluateObjects(Time, nullptr);
		OutputCamera();
		OutputWeather();
		OutputPost();
	}
}

void Player::Replace(Scene&& Incoming)
{
	Incoming.LevelName = Data.LevelName;
	EnsureCamera(Incoming);
	Data = std::move(Incoming);
	Time = PlayStart = 0.0f;
	IsPlaying = HasLoopRange = HasUnsavedChanges = false;
	FitRequests.clear();
	AttachRequests.clear();
	Listen = {};
	Soloed.clear();
	ShouldDropRuntimes = true;
	Generation = ++PlayerLocal::g_Generation;
}

void Player::Play()
{
	if (Time >= ContentEnd(Data))
	{
		Time = 0.0f;
	}
	PlayStart = Time;
	IsPlaying = IsLookingThrough = true;
}

void Player::Pause()
{
	IsPlaying = false;
}

void Player::Stop()
{
	IsPlaying = false;
	Time = PlayStart;
}

void Player::Seek(float Target)
{
	Time = Sanitize(Target, 0.0f, MaxTime, Time);
}

void Player::SetEditorView(bool IsOpen, bool IsCamera, const Fvector& Position, const Fmatrix& Orientation, float Fov)
{
	IsEditorOpen = IsOpen;
	IsEditorCamera = IsCamera;
	EditorPose.Position = Position;
	EditorPose.Orientation = Orientation;
	EditorPose.Fov = Fov;
}


void Player::SetAudition(u32 ObjectId, const shared_str& Source, const shared_str& Name, const shared_str& Visual)
{
	if (!Source.size() && !Visual.size())
	{
		Listen = {};
	}
	else if (Listen.ObjectId != ObjectId || Listen.Source != Source || Listen.Name != Name || Listen.Visual != Visual)
	{
		Listen = { ObjectId, Source, Name, Device.fTimeGlobal, Visual };
	}
}

void Player::RequestFit(u32 ObjectId)
{
	if (std::find(FitRequests.begin(), FitRequests.end(), ObjectId) == FitRequests.end())
	{
		FitRequests.push_back(ObjectId);
	}
}

void Player::RequestAttach(u32 ObjectId, s32 Block, const Attachment& NewAttach)
{
	AttachRequests.push_back({ ObjectId, Block, NewAttach });
}

const ObjectState* Player::FindState(u32 ObjectId) const
{
	const s32 Index = Graph.IndexOf(ObjectId);
	const bool IsCurrent = !ShouldDropRuntimes && Index >= 0 && static_cast<u32>(Index) < std::min(States.size(), Data.Objects.size()) && Data.Objects[Index].Id == ObjectId;
	return IsCurrent ? &States[Index] : nullptr;
}

const ActorRuntime* Player::FindActor(u32 ObjectId) const
{
	return ShouldDropRuntimes ? nullptr : PlayerLocal::RuntimeOf(Actors, ObjectId);
}

void Player::PreviewClip(u32 ObjectId, s32 Clip, float Time)
{
	if (ActorRuntime* Found = const_cast<ActorRuntime*>(FindActor(ObjectId)))
	{
		Found->Preview = { Clip, Time, Device.dwFrame };
	}
}

const LightRuntime* Player::FindLight(u32 ObjectId) const
{
	return ShouldDropRuntimes ? nullptr : PlayerLocal::RuntimeOf(Lights, ObjectId);
}

const SoundRuntime* Player::FindSound(u32 ObjectId) const
{
	return ShouldDropRuntimes ? nullptr : PlayerLocal::RuntimeOf(Voices, ObjectId);
}

const DecalRuntime* Player::FindDecal(u32 ObjectId) const
{
	return ShouldDropRuntimes ? nullptr : PlayerLocal::RuntimeOf(Decals, ObjectId);
}

const ParticleRuntime* Player::FindParticle(u32 ObjectId) const
{
	return ShouldDropRuntimes ? nullptr : PlayerLocal::RuntimeOf(Emitters, ObjectId);
}

const BlockCache* Player::FindCameraBlock(u32 Block) const
{
	return !ShouldDropRuntimes && Block < CameraCache.size() ? &CameraCache[Block] : nullptr;
}

void Player::ResetLevel()
{
	SaveUnsaved();
	Release();
	IsEditorOpen = IsEditorCamera = false;
	IsLookingThrough = true;
	TrackedLevel = g_pGameLevel;
	Data.LevelName = TrackedLevel ? TrackedLevel->name() : shared_str();
	Replace(Scene {});
}

void Player::SaveUnsaved()
{
	if (!HasUnsavedChanges)
	{
		return;
	}
	HasUnsavedChanges = false;
	string_path Path;
	xr_string Error;
	Io::UnsavedPath(Path, Data.LevelName);
	if (Io::Save(Data, Path, Error))
	{
		Msg("Cinema editor: unsaved changes written to %s", Path);
		return;
	}
	Msg("! Cinema editor: unsaved changes are lost: %s", Error.c_str());
}

void Player::Advance()
{
	Played = 0.0f;
	if (!IsPlaying || Device.Paused() || !std::isfinite(Device.fTimeDelta))
	{
		return;
	}
	const float Before = Time;
	Time += Device.fTimeDelta * Speed;
	Played = Time - Before;
	const bool IsRange = IsLooping && HasLoopRange;
	const float Begin = IsRange ? LoopStart : 0.0f;
	const float End = IsRange ? std::min(LoopEnd, ContentEnd(Data)) : ContentEnd(Data);
	if (Time < End)
	{
		return;
	}
	if (IsLooping && End > Begin)
	{
		Time = Begin + std::fmod(Time - Begin, End - Begin);
		Played = -1.0f;
		return;
	}
	Time = End;
	Played = End - Before;
	IsPlaying = false;
}

void Player::Sync()
{
	PlayerLocal::Reconcile(Actors, Data, ObjectType::Actor, RuntimeIds);
	PlayerLocal::Reconcile(Lights, Data, ObjectType::Light, RuntimeIds);
	PlayerLocal::Reconcile(Emitters, Data, ObjectType::Particle, RuntimeIds);
	PlayerLocal::Reconcile(Decals, Data, ObjectType::Decal, RuntimeIds);
	PlayerLocal::Reconcile(Voices, Data, ObjectType::Sound, RuntimeIds);
	const float Step = 1.0f / static_cast<float>(Data.Header.FrameRate);
	for (Object& Item : Data.Objects)
	{
		if (Item.Type == ObjectType::Actor)
		{
			ActorRuntime& Runtime = ActorAt(Item.Id);
			const Audition* Ear = Heard(Item.Id);
			Runtime.Apply(Item, Step, RetargetIndex, Ear);
			if (Item.Actor.IsRelative && Runtime.IsComplete() && !(Ear && Ear->Visual.size()))
			{
				Absolutize(Item, Runtime, Step);
				Runtime.Apply(Item, Step, RetargetIndex, Ear);
			}
			if (!Item.Actor.IsRelative && HasClips(Item.Actor) && Runtime.Extent > 0.0f)
			{
				Item.Span.Length = std::clamp(Runtime.Extent, MinLength, MaxTime);
				Item.Span.IsLengthLocked = false;
			}
		}
		else if (Item.Type == ObjectType::Light)
		{
			PlayerLocal::RuntimeOf(Lights, Item.Id)->Apply(Item.Light);
		}
		else if (Item.Type == ObjectType::Particle)
		{
			ParticleRuntime& Runtime = *PlayerLocal::RuntimeOf(Emitters, Item.Id);
			Runtime.Apply(Item.Particle);
			Curves::Refresh(Runtime.SpeedArea, PlayerLocal::LaneCurve(Item.Lanes, Param::Speed, 0), Item.Particle.Speed, Step, Item.Span.Length, Info(Param::Speed));
		}
		else if (Item.Type == ObjectType::Decal)
		{
			PlayerLocal::RuntimeOf(Decals, Item.Id)->Apply(Item.Decal, Item.Decal.Shader.size() ? Shaders() : ShaderNames);
		}
		else if (Item.Type == ObjectType::Sound)
		{
			PlayerLocal::RuntimeOf(Voices, Item.Id)->Apply(Item.Sound);
		}
	}
	for (u32 Id : FitRequests)
	{
		Object* Target = Find(Data, Id);
		if (Target && Target->Type == ObjectType::Actor)
		{
			Fit(*Target, ActorAt(Id), Step);
		}
	}
	FitRequests.clear();
	Object* Rig = FindCamera(Data);
	CameraCache.resize(Rig ? Rig->Camera.Blocks.size() : 0);
	float Length = 0.0f;
	for (u32 Index = 0; Index < CameraCache.size(); ++Index)
	{
		CameraBlock& Block = Rig->Camera.Blocks[Index];
		BlockCache& Cache = CameraCache[Index];
		Length = std::max(Length, Block.Start + Block.Length);
		Cameras::Rebuild(Block);
		Cache.Keys = Block.Keys;
		Curves::Refresh(Cache.TimeScaleArea, PlayerLocal::LaneCurve(Block.Lanes, Param::TimeScale, 0), Block.TimeScale, Step, Block.Length, Info(Param::TimeScale));
	}
	ShakeAreas.resize(Rig ? Rig->Camera.Shakes.size() : 0);
	ShakeOwners.resize(ShakeAreas.size());
	for (u32 Index = 0; Index < ShakeAreas.size(); ++Index)
	{
		const ShakeClip& Clip = Rig->Camera.Shakes[Index];
		Curves::Refresh(ShakeAreas[Index], PlayerLocal::LaneCurve(Clip.Lanes, Param::ShakeSpeed, Clip.Row), Clip.Setup.Speed, Step, Clip.Length, Info(Param::ShakeSpeed));
		ShakeOwners[Index] = static_cast<s16>(Cameras::ShakeOwner(Rig->Camera, Clip));
		Length = std::max(Length, Clip.Start + Clip.Length);
	}
	if (Rig)
	{
		Rig->Span.Start = 0.0f;
		Rig->Span.Length = std::clamp(Rig->Span.IsLengthLocked ? std::max(Length, Rig->Span.Length) : Length, MinLength, MaxTime);
	}
	RefreshLinks();
	for (const AttachRequest& Request : AttachRequests)
	{
		Convert(Request);
		RefreshLinks();
	}
	AttachRequests.clear();
}

void Player::RefreshLinks()
{
	u32 Cursor = 0;
	bool IsSame = true;
	PlayerLocal::ForEachAttachment(Data, [&](u32 Id, const Attachment& Links)
	{
		IsSame = IsSame && Cursor < LinkCache.size() && LinkCache[Cursor].Id == Id && LinkCache[Cursor].Links == Links;
		++Cursor;
	});
	if (IsSame && Cursor == LinkCache.size())
	{
		return;
	}
	LinkCache.clear();
	PlayerLocal::ForEachAttachment(Data, [this](u32 Id, const Attachment& Links) { LinkCache.push_back({ Id, Links }); });
	Graph.Build(Data);
	States.resize(Data.Objects.size());
}

void Player::Convert(const AttachRequest& Request)
{
	Object* Target = Find(Data, Request.ObjectId);
	const bool IsBlock = Request.Block >= 0;
	if (!Target || (IsBlock ? static_cast<u32>(Request.Block) >= CameraCache.size() || Target->Type != ObjectType::Camera : Target->Type == ObjectType::Camera))
	{
		return;
	}
	Attachment& Links = IsBlock ? Target->Camera.Blocks[Request.Block].Attach : Target->Attach;
	const auto Mark = [this](u32 Id)
	{
		const s32 At = Graph.IndexOf(Id);
		if (At >= 0)
		{
			Needed[At] = 1;
		}
	};
	Needed.assign(States.size(), 0);
	for (u32 Id : { Links.Parent.ObjectId, Links.LookAt.ObjectId, Request.Links.Parent.ObjectId, Request.Links.LookAt.ObjectId })
	{
		Mark(Id);
	}
	for (auto Item = Graph.Order.rbegin(); Item != Graph.Order.rend(); ++Item)
	{
		if (Needed[*Item])
		{
			ForEachLink(Data.Objects[*Item], Mark);
		}
	}
	const float HalfFrame = 0.5f / static_cast<float>(Data.Header.FrameRate);
	const float Start = Target->Span.Start + (IsBlock ? Cameras::BlockStart(Target->Camera, static_cast<u32>(Request.Block)) : 0.0f);
	xr_vector<Lane>& Lanes = IsBlock ? Target->Camera.Blocks[Request.Block].Lanes : Target->Lanes;
	const Fvector Zero = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector* const Place = IsBlock ? nullptr : PlacementOf(*Target, 0);
	Fvector* const Turn = IsBlock ? nullptr : PlacementOf(*Target, 1);
	Times.clear();
	for (const Lane& Track : Lanes)
	{
		for (u32 Component = 0; Track.Id <= Param::Rotation && Component < 3; ++Component)
		{
			for (const Key& Item : Track.Curves[Component].Keys)
			{
				Times.push_back(Item.Time);
			}
		}
	}
	std::sort(Times.begin(), Times.end());
	Times.erase(std::unique(Times.begin(), Times.end(), [HalfFrame](float A, float B) { return B - A <= HalfFrame; }), Times.end());
	const bool IsStatic = !IsBlock && Times.empty();
	if (IsStatic)
	{
		Times.push_back(Time - Start);
	}
	Converted.clear();
	Fvector Reference = Fvector().set(0.0f, 0.0f, 0.0f);
	for (u32 Index = 0; Index < Times.size(); ++Index)
	{
		EvaluateObjects(Start + (IsBlock ? Cameras::LocalFromPath(Target->Camera.Blocks[Request.Block], Times[Index]) : Times[Index]), Needed.data());
		LinkFrames Old, New;
		Resolve(Links, Target->Type == ObjectType::Actor, Old, Times[Index]);
		Resolve(Request.Links, Target->Type == ObjectType::Actor, New, Times[Index]);
		Fvector Position = PlayerLocal::KeyValues(Lanes, Param::Position, Times[Index], Place ? *Place : Zero);
		Fvector Rotation = PlayerLocal::KeyValues(Lanes, Param::Rotation, Times[Index], Turn ? *Turn : Zero);
		Reference = Index ? Reference : Rotation;
		Fmatrix Pose = PlayerLocal::PoseMatrix(Position, Rotation);
		Spaces::Compose(Old, Pose);
		Spaces::Decompose(New, Pose);
		PlayerLocal::Unpose(Pose, Reference, Position, Rotation);
		Reference = Rotation;
		Converted.push_back(Position);
		Converted.push_back(Rotation);
	}
	for (u32 Index = 0; Index < Times.size(); ++Index)
	{
		if (IsStatic)
		{
			*Place = Converted[0];
			*Turn = Converted[1];
		}
		for (u32 Kind = 0; !IsStatic && Kind < 2; ++Kind)
		{
			Lane& Track = Curves::EnsureLane(Lanes, static_cast<Param>(Kind), 0);
			for (u32 Component = 0; Component < 3; ++Component)
			{
				PlayerLocal::PutKey(Track.Curves[Component], Times[Index], Converted[2 * Index + Kind][Component], HalfFrame);
			}
		}
	}
	if (IsBlock)
	{
		Cameras::Rebuild(Target->Camera.Blocks[Request.Block]);
		CameraCache[Request.Block].Keys = Target->Camera.Blocks[Request.Block].Keys;
	}
	Links = Request.Links;
}

void Player::EvaluateObjects(float At, const u8* Mask)
{
	EvaluationTime = At;
	Shot = CameraSample {};
	for (u32 Index = 0; Index < States.size(); ++Index)
	{
		if (!Mask || Mask[Index])
		{
			States[Index].IsReady = false;
		}
	}
	for (u32 Index : Graph.Order)
	{
		const ObjectType Type = Data.Objects[Index].Type;
		if (Mask && !Mask[Index])
		{
			continue;
		}
		if (Type == ObjectType::Camera)
		{
			PoseCamera(Index, At);
		}
		else if (Type == ObjectType::Actor)
		{
			PoseActor(Index, At, Mask || Graph.IsLinked[Index]);
			const Object& Item = Data.Objects[Index];
			if (!Mask && States[Index].IsReady)
			{
				ActorAt(Item.Id).Dress(Item, std::clamp(At - Item.Span.Start, 0.0f, Item.Span.Length), Item.Actor.Looks.empty() ? ShaderNames : Shaders());
			}
		}
		else if (Type == ObjectType::Light)
		{
			PoseLight(Index, At, !Mask);
		}
		else if (Type == ObjectType::Particle)
		{
			PoseParticle(Index, At, !Mask);
		}
		else if (Type == ObjectType::Decal)
		{
			PoseDecal(Index, At, !Mask);
		}
		else if (Type == ObjectType::Sound)
		{
			PoseSound(Index, At, !Mask);
		}
	}
}

void Player::EvaluateLinked(u32 Index, float At)
{
	xr_vector<u8> Mask(States.size(), 0);
	Mask[Index] = 1;
	const auto Mark = [this, &Mask](u32 Id)
	{
		const s32 Dependency = Graph.IndexOf(Id);
		if (Dependency >= 0)
		{
			Mask[Dependency] = 1;
		}
	};
	for (auto Item = Graph.Order.rbegin(); Item != Graph.Order.rend(); ++Item)
	{
		if (Mask[*Item])
		{
			ForEachLink(Data.Objects[*Item], Mark);
		}
	}
	const CameraSample PreviousShot = Shot;
	const float PreviousTime = EvaluationTime;
	EvaluateObjects(At, Mask.data());
	Shot = PreviousShot;
	EvaluationTime = PreviousTime;
}

void Player::PoseActor(u32 Index, float At, bool IsNeeded)
{
	const Object& Item = Data.Objects[Index];
	ActorRuntime& Runtime = ActorAt(Item.Id);
	ObjectState& State = States[Index];
	Resolve(Item.Attach, true, State.Frames, std::clamp(At - Item.Span.Start, 0.0f, Item.Span.Length));
	Unhook(Item.Attach, Item.Lanes, Item.Span.Start, std::clamp(At - Item.Span.Start, 0.0f, Item.Span.Length), State.Frames);
	const bool IsAimed = State.Frames.HasLook;
	State.Frames.HasLook = IsAimed && !(Item.Attach.LookFixed & LookLegs);
	State.IsReady = !(State.Frames.Problems & MissingParent) && Runtime.Evaluate(Item, At, Heard(Item.Id), IsNeeded, IsSilent(Item));
	if (!State.IsReady)
	{
		Runtime.Body.Unbind();
		return;
	}
	const float Local = std::clamp(At - Item.Span.Start, 0.0f, Item.Span.Length);
	const ActorData& Setup = Item.Actor;
	const Fvector Scale = Curves::LaneVector(Item.Lanes, Param::Scale, Local, Setup.Scale);
	State.World = PlayerLocal::PoseMatrix(Curves::LaneVector(Item.Lanes, Param::Position, Local, Setup.Position), Curves::LaneVector(Item.Lanes, Param::Rotation, Local, Setup.Rotation));
	Spaces::Compose(State.Frames, State.World);
	State.World.i.mul(Scale.x);
	State.World.j.mul(Scale.y);
	State.World.k.mul(Scale.z);
	if (IsAimed && (Item.Attach.LookFixed & LookLegs))
	{
		Runtime.LookWith(static_cast<u8>(~Item.Attach.LookFixed & (LookTorso | LookHead)), State.Frames.Look, State.Frames.LookWeight, State.World);
	}
	for (u32 Slot = 0; Slot < Setup.Constraints.size(); ++Slot)
	{
		const BoneConstraint& Rule = Setup.Constraints[Slot];
		const float Weight = Rule.IsMuted ? 0.0f : std::clamp(Curves::LaneScalar(Item.Lanes, Param::ConstraintWeight, static_cast<u8>(Slot), Local, Rule.Weight), 0.0f, 1.0f);
		Fmatrix Goal;
		if (Weight > 0.0f && Rule.Target.ObjectId != Item.Id && LinkFrame(Rule.Target, Goal))
		{
			Runtime.Constrain(Rule, Weight, Goal, State.World);
		}
	}
	Runtime.SolveIk(Item, Local, State.World);
	Runtime.Place(State.World);
}

void Player::PoseCamera(u32 Index, float At)
{
	const Object& Rig = Data.Objects[Index];
	ObjectState& State = States[Index];
	for (u32 Block = 0; Block < CameraCache.size(); ++Block)
	{
		BlockCache& Cache = CameraCache[Block];
		const AttachLink& Focus = Rig.Camera.Blocks[Block].FocusTarget;
		Fmatrix Point = Fidentity;
		const CameraBlock& Clip = Rig.Camera.Blocks[Block];
		Resolve(Clip.Attach, false, Cache.Frames, 0.0f);
		Unhook(Clip.Attach, Clip.Lanes, Rig.Span.Start + Clip.Start, std::clamp(At - Rig.Span.Start - Clip.Start, 0.0f, Clip.Length), Cache.Frames);
		Cache.HasFocus = Focus.ObjectId && Frame(Focus, Point, Cache.Frames.Problems, MissingFocusBone);
		Cache.FocusPoint = Point.c;
		Cache.Frames.Problems |= Focus.ObjectId && !Cache.HasFocus ? MissingFocus : 0;
	}
	if (!IsSilent(Rig))
	{
		Cameras::Sample(Rig, CameraCache, ShakeAreas, ShakeOwners, At, Shot);
	}
	const CameraPose Pose = Shot.IsActive ? Cameras::Mix(Shot) : EditorPose;
	State.World = Pose.Orientation;
	State.World.c = Pose.Position;
	if (!Shot.IsActive && !(IsEditorOpen && IsEditorCamera))
	{
		State.World.set(Device.vCameraRight, Device.vCameraTop, Device.vCameraDirection, Device.vCameraPosition);
	}
	State.Frames = LinkFrames {};
	State.IsReady = true;
}

void Player::PoseLight(u32 Index, float At, bool IsDrawn)
{
	const Object& Item = Data.Objects[Index];
	const LightData& Setup = Item.Light;
	const float Tau = PoseTransform(Index, At, Setup.Position, Setup.Rotation);
	if (!IsDrawn)
	{
		return;
	}
	const float Local = std::clamp(Tau, 0.0f, Item.Span.Length);
	LightData Values;
	Values.Color = Curves::LaneVector(Item.Lanes, Param::Color, Local, Setup.Color);
	for (u32 Field = 0; Field < 7; ++Field)
	{
		const auto Scalar = PlayerLocal::LightScalars[Field];
		Values.*Scalar = Curves::LaneScalar(Item.Lanes, static_cast<Param>(static_cast<u32>(Param::Intensity) + Field), 0, Local, Setup.*Scalar);
	}
	PlayerLocal::RuntimeOf(Lights, Item.Id)->Place(States[Index].World, Values, States[Index].IsReady && !IsSilent(Item) && Tau >= 0.0f && Tau < Item.Span.Length);
}

void Player::PoseParticle(u32 Index, float At, bool IsDrawn)
{
	const Object& Item = Data.Objects[Index];
	const ParticleData& Setup = Item.Particle;
	const float Tau = PoseTransform(Index, At, Setup.Position, Setup.Rotation);
	if (!IsDrawn)
	{
		return;
	}
	ParticleRuntime& Runtime = *PlayerLocal::RuntimeOf(Emitters, Item.Id);
	const float Local = std::clamp(Tau, 0.0f, Item.Span.Length);
	const bool IsLive = States[Index].IsReady && !IsSilent(Item);
	const float EffectTime = static_cast<float>(Curves::LaneArea(Item.Lanes, &Runtime.SpeedArea, Param::Speed, 0, Local, Setup.Speed));
	const float Rate = Curves::LaneScalar(Item.Lanes, Param::Speed, 0, Local, Setup.Speed);
	const float Size = Curves::LaneScalar(Item.Lanes, Param::Size, 0, Local, Setup.Size);
	const bool IsInside = Tau >= 0.0f && Tau < Item.Span.Length, IsFree = Setup.IsAlways && IsEditorOpen && !IsPlaying;
	const float Step = IsFree && !IsInside ? 0.0f : Played;
	Runtime.Drive({ .World = States[Index].World, .Tau = IsFree ? Local : Tau, .Played = Step, .Idle = IsFree && Step == 0.0f && !Device.Paused() ? Device.fTimeDelta : 0.0f, .Rate = Rate, .Preroll = std::clamp(EffectTime, 0.0f, MaxPreroll), .Size = Size, .Opacity = Curves::LaneScalar(Item.Lanes, Param::ParticleOpacity, 0, Local, Setup.Opacity), .IsShown = IsLive && (IsFree || IsInside), .IsAfter = IsLive && !IsFree && Tau >= Item.Span.Length, .ShouldRetrigger = Setup.ShouldRetrigger });
}

void Player::PoseDecal(u32 Index, float At, bool IsDrawn)
{
	const Object& Item = Data.Objects[Index];
	const DecalData& Setup = Item.Decal;
	const float Tau = PoseTransform(Index, At, Setup.Position, Setup.Rotation);
	if (!IsDrawn)
	{
		return;
	}
	DecalRuntime& Runtime = *PlayerLocal::RuntimeOf(Decals, Item.Id);
	const float Local = std::clamp(Tau, 0.0f, Item.Span.Length);
	const bool IsLive = States[Index].IsReady && !IsSilent(Item);
	const float Width = Curves::LaneScalar(Item.Lanes, Param::DecalWidth, 0, Local, Setup.Width), Height = Curves::LaneScalar(Item.Lanes, Param::DecalHeight, 0, Local, Setup.Height);
	if (Setup.IsStamp && IsLive && IsPlaying && Runtime.LastTau < 0.0f && Tau >= 0.0f)
	{
		Stamp(Runtime, States[Index].World, std::max(Width, Height));
	}
	Runtime.LastTau = Tau;
	Runtime.Drive(States[Index].World, Width, Height, !Setup.IsStamp && IsLive && Tau >= 0.0f && Tau < Item.Span.Length);
}

void Player::PoseSound(u32 Index, float At, bool IsDrawn)
{
	const Object& Item = Data.Objects[Index];
	const SoundData& Setup = Item.Sound;
	const float Tau = PoseTransform(Index, At, Setup.Position, Setup.Rotation);
	if (!IsDrawn)
	{
		return;
	}
	const float Local = std::clamp(Tau, 0.0f, Item.Span.Length);
	const bool IsHeard = IsPlaying && States[Index].IsReady && !IsSilent(Item) && Tau >= 0.0f && Tau < Item.Span.Length;
	PlayerLocal::RuntimeOf(Voices, Item.Id)->Drive({ States[Index].World.c, Tau, Curves::LaneScalar(Item.Lanes, Param::SoundVolume, 0, Local, Setup.Volume), Curves::LaneScalar(Item.Lanes, Param::SoundPitch, 0, Local, Setup.Pitch), IsHeard });
}

void Player::Stamp(DecalRuntime& Runtime, const Fmatrix& World, float Size)
{
	IWallMarkArray* Marks = Runtime.Marks();
	Fvector Up = World.j, Down, Point;
	Up.normalize_safe();
	Down.invert(Up);
	const Fvector Start = Fvector().mad(World.c, Up, 0.5f);
	u32 Triangle = 0;
	float Best = 0.0f;
	const bool IsStatic = Marks && Runtime.Trace(World, Point, Triangle, Best);
	Best = IsStatic ? Best : 1.0f;
	const ActorRuntime* Hit = nullptr;
	for (const xr_unique_ptr<ActorRuntime>& Actor : Actors)
	{
		Fsphere Sphere;
		if (!Marks || !Actor->PickSphere(Sphere))
		{
			continue;
		}
		const Fvector Gap = Fvector().sub(Sphere.P, Start);
		const float Along = Gap.dotproduct(Down);
		const bool IsNear = Along >= 0.0f && Along < Best && Fvector().mad(Gap, Down, -Along).magnitude() < Sphere.R;
		Hit = IsNear ? Actor.get() : Hit;
		Best = IsNear ? Along : Best;
	}
	IKinematics* Skin = Hit && Hit->Body.renderable.visual ? Hit->Body.renderable.visual->dcast_PKinematics() : nullptr;
	if (Skin)
	{
		::Render->add_SkeletonWallmark(&Hit->Body.renderable.xform, Skin, Marks, Start, Down, Size);
	}
	else if (IsStatic)
	{
		::Render->add_StaticWallmark(Marks, Point, Size, &Level().ObjectSpace.GetStaticTris()[Triangle], Level().ObjectSpace.GetStaticVerts().data());
	}
}

float Player::PoseTransform(u32 Index, float At, const Fvector& Position, const Fvector& Rotation)
{
	const Object& Item = Data.Objects[Index];
	ObjectState& State = States[Index];
	const float Tau = At - Item.Span.Start;
	const float Local = std::clamp(Tau, 0.0f, Item.Span.Length);
	Resolve(Item.Attach, false, State.Frames, Local);
	Unhook(Item.Attach, Item.Lanes, Item.Span.Start, Local, State.Frames);
	State.World = PlayerLocal::PoseMatrix(Curves::LaneVector(Item.Lanes, Param::Position, Local, Position), Curves::LaneVector(Item.Lanes, Param::Rotation, Local, Rotation));
	Spaces::Compose(State.Frames, State.World);
	State.IsReady = !(State.Frames.Problems & MissingParent);
	return Tau;
}

void Player::Resolve(const Attachment& Links, bool IsActor, LinkFrames& Out, float Local) const
{
	Fmatrix Target = Fidentity;
	Out = LinkFrames {};
	Out.IsYawOnly = IsActor || Links.IsYawOnly;
	Out.HasParent = Links.Parent.ObjectId && Frame(PlayerLocal::LinkAt(Links, Local), Out.Parent, Out.Problems, MissingParentBone);
	Out.HasLook = Links.LookAt.ObjectId && Frame(Links.LookAt, Target, Out.Problems, MissingTargetBone);
	Out.Problems |= (Links.Parent.ObjectId && !Out.HasParent ? MissingParent : 0) | (Links.LookAt.ObjectId && !Out.HasLook ? MissingTarget : 0);
	Out.Look = Target.c;
	if (Out.HasParent)
	{
		PlayerLocal::Lock(Links, Out.Parent);
	}
}

void Player::Unhook(const Attachment& Links, const xr_vector<Lane>& Lanes, float Start, float Local, LinkFrames& Out)
{
	const Lane* Track = Curves::FindLane(Lanes, Param::AttachWeight, 0);
	Out.Weight = std::clamp(Track ? Curves::Value(Track->Curves[0], Local, Links.Weight) : Links.Weight, 0.0f, 1.0f);
	Out.LookWeight = std::clamp(Curves::LaneScalar(Lanes, Param::LookWeight, 0, Local, Links.LookWeight), 0.0f, 1.0f);
	const float Smooth = std::clamp(Curves::LaneScalar(Lanes, Param::FollowSmooth, 0, Local, Links.Smooth), 0.0f, 5.0f);
	if (Smooth > 0.0f && (Out.HasLook || Out.HasParent))
	{
		constexpr u32 Samples = 3;
		Fvector Aimed = Out.Look, Placed = Out.Parent.c;
		u32 AimCount = 1, PlaceCount = 1;
		for (u32 Step = 1; Step <= Samples; ++Step)
		{
			const float Past = Start + Local - Smooth * static_cast<float>(Step) / static_cast<float>(Samples);
			Fmatrix Earlier;
			if (Out.HasLook && FrameAt(Links.LookAt, Past, Earlier))
			{
				Aimed.add(Earlier.c);
				++AimCount;
			}
			if (Out.HasParent && FrameAt(PlayerLocal::LinkAt(Links, Local), Past, Earlier))
			{
				Placed.add(Earlier.c);
				++PlaceCount;
			}
		}
		Out.Look.mul(Aimed, 1.0f / static_cast<float>(AimCount));
		Out.Parent.c.mul(Placed, 1.0f / static_cast<float>(PlaceCount));
	}
	if (!Out.HasParent || Out.Weight >= 1.0f)
	{
		return;
	}
	float Freeze = 0.0f;
	bool IsFound = false;
	for (u32 At = 0; Track && At < Track->Curves[0].Keys.size(); ++At)
	{
		const xr_vector<Key>& Keys = Track->Curves[0].Keys;
		if (Keys[At].Value < 1.0f || (IsFound && Keys[At].Time > Local))
		{
			continue;
		}
		Freeze = Keys[At].Mode == Interp::Hold && At + 1 < Keys.size() && Keys[At + 1].Time <= Local ? Keys[At + 1].Time : Keys[At].Time;
		IsFound = true;
	}
	Fmatrix Frozen;
	if (!FrameAt(PlayerLocal::LinkAt(Links, Local), Start + Freeze, Frozen))
	{
		return;
	}
	PlayerLocal::Lock(Links, Frozen);
	Fquaternion From, To, Mixed;
	From.set(Frozen);
	To.set(Out.Parent);
	Mixed.slerp(From, To, Out.Weight);
	const Fvector Origin = Fvector().lerp(Frozen.c, Out.Parent.c, Out.Weight);
	Out.Parent.rotation(Mixed.normalize());
	Out.Parent.c = Origin;
}

bool Player::FrameAt(const AttachLink& Link, float At, Fmatrix& Out)
{
	const s32 Index = Graph.IndexOf(Link.ObjectId);
	if (Index < 0 || static_cast<u32>(Index) >= States.size() || Data.Objects[Index].Type == ObjectType::Camera)
	{
		u8 Problems = 0;
		return Frame(Link, Out, Problems, 0);
	}
	const float PreviousTime = EvaluationTime;
	EvaluateLinked(static_cast<u32>(Index), At);
	u8 Problems = 0;
	const bool IsFramed = Frame(Link, Out, Problems, 0);
	EvaluateLinked(static_cast<u32>(Index), PreviousTime);
	return IsFramed;
}

bool Player::Frame(const AttachLink& Link, Fmatrix& Out, u8& Problems, u8 BoneProblem) const
{
	const s32 Index = Graph.IndexOf(Link.ObjectId);
	if (Index < 0 || static_cast<u32>(Index) >= States.size() || !States[Index].IsReady)
	{
		return false;
	}
	const ActorRuntime* Runtime = Data.Objects[Index].Type == ObjectType::Actor ? FindActor(Link.ObjectId) : nullptr;
	Out = States[Index].World;
	if (Runtime ? !Runtime->BoneFrame(Link.Bone, Out) : Link.Bone.size() != 0)
	{
		Problems |= BoneProblem;
	}
	Spaces::Orthonormalize(Out);
	return true;
}

void Player::Fit(Object& Target, ActorRuntime& Runtime, float Step)
{
	if (!Target.Actor.IsRelative || Target.Span.IsLengthLocked || !Runtime.IsComplete() || !Runtime.HasSpans() || !(Runtime.SequenceLength > 0.0f))
	{
		return;
	}
	const ActorData& Setup = Target.Actor;
	Curves::Refresh(Runtime.TimeScaleArea, PlayerLocal::LaneCurve(Target.Lanes, Param::TimeScale, 0), Setup.TimeScale, Step, MaxTime, Info(Param::TimeScale));
	const double TargetArea = Runtime.SequenceLength * (Setup.End == EndMode::Hold ? 1.0 : 2.0);
	if (Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, MaxTime, Setup.TimeScale) >= TargetArea)
	{
		Target.Span.Length = std::clamp(Curves::LaneInverseArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Setup.TimeScale, TargetArea, MaxTime), MinLength, MaxTime);
	}
}

void Player::Absolutize(Object& Target, ActorRuntime& Runtime, float Step)
{
	ActorData& Setup = Target.Actor;
	const EndMode Mode = Setup.End;
	Setup.IsRelative = false;
	Setup.End = EndMode::Hold;
	Target.Span.IsLengthLocked = false;
	if (!HasClips(Setup))
	{
		return;
	}
	Curves::Refresh(Runtime.TimeScaleArea, PlayerLocal::LaneCurve(Target.Lanes, Param::TimeScale, 0), Setup.TimeScale, Step, MaxTime, Info(Param::TimeScale));
	const float Length = Runtime.SequenceLength, Stop = static_cast<float>(Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Target.Span.Length, Setup.TimeScale));
	const bool IsBack = Mode == EndMode::PingPong;
	u32 Most = 0;
	for (u8 Layer = 0; Layer <= Setup.Layers.size(); ++Layer)
	{
		Most = std::max<u32>(Most, static_cast<u32>(Runtime.Track(Layer).Spans.size()));
	}
	const u32 Wanted = Mode != EndMode::Hold && Length > 0.0f ? std::max(static_cast<u32>(std::ceil(Stop / Length)), 1u) : 1u;
	const u32 Passes = std::min(Wanted, Most ? std::max(MaxBlocks / (Most * (IsBack ? 3 : 1) + 1), 1u) : Wanted);
	bool HasGaps = false;
	for (u8 Layer = 0; Layer <= Setup.Layers.size(); ++Layer)
	{
		const LayerRuntime& Track = Runtime.Track(Layer);
		const xr_vector<ActorBlock>& Old = BlocksOf(Setup, Layer);
		Built.clear();
		BuiltStarts.clear();
		const auto Push = [this](const ActorBlock& Block, float At)
		{
			Built.push_back(Block);
			BuiltStarts.push_back(At);
		};
		const auto Pose = [](ActorBlock Block, float Frame, float Hold)
		{
			Block.In = Block.Out = std::max(Frame, 1e-4f);
			Block.Speed = Block.Loops = 1.0f;
			Block.Hold = Hold;
			return Block;
		};
		HasGaps = HasGaps || (Layer && Passes > 1 && !Track.Spans.empty() && (Track.Spans.front().Start > 0.0f || Track.Length < Length));
		for (u32 Pass = 0; Pass < Passes; ++Pass)
		{
			const float Base = static_cast<float>(Pass) * Length;
			if (!Layer && !Track.Spans.empty() && Track.Spans.front().Start > 1e-4f && (!Pass || !IsBack))
			{
				Push(Pose(Old[Track.Spans.front().Block], Track.Spans.front().From, Track.Spans.front().Start), Base);
			}
			for (u32 Index = 0; !(IsBack && (Pass & 1)) && Index < Track.Spans.size(); ++Index)
			{
				Push(Old[Track.Spans[Index].Block], Base + Track.Spans[Index].Start);
			}
			for (u32 Index = static_cast<u32>(Track.Spans.size()); IsBack && (Pass & 1) && Index-- > 0;)
			{
				const BlockSpan& Span = Track.Spans[Index];
				const ActorBlock& Block = Old[Span.Block];
				const float Whole = std::floor(Span.Loops), Part = Span.Loops - Whole, Hold = std::max(Block.Hold, 0.0f), End = Actors::FrameAt(Span, Span.Play * Span.Loops);
				const size_t Head = Built.size();
				float At = Base + Length - Span.Start - Span.Duration;
				if (Hold > 0.0f)
				{
					Push(Pose(Block, End, Hold), At);
					At += Hold;
				}
				ActorBlock Back = Block;
				Back.Speed = -Block.Speed;
				Back.Hold = 0.0f;
				Back.Loops = 1.0f;
				if (Part > 1e-4f)
				{
					ActorBlock Tail = Back;
					Tail.In = std::min(Span.From, End);
					Tail.Out = std::max(Span.From, End);
					Push(Tail, At);
					At += Part * Span.Play;
				}
				Back.Loops = Whole;
				Push(Back, At);
				const Interp Ease = Index + 1 < Track.Spans.size() ? Old[Track.Spans[Index + 1].Block].BlendEase : Interp::EaseInOut;
				Built[Head].BlendEase = Ease == Interp::EaseIn ? Interp::EaseOut : Ease == Interp::EaseOut ? Interp::EaseIn : Ease;
			}
		}
		u32 Kept = 0;
		for (u32 Index = 0; Index < Built.size(); ++Index)
		{
			if (BuiltStarts[Index] < Stop - 1e-4f)
			{
				Actors::TrimBlock(Built[Index], Runtime.MotionLength(Built[Index].Source, Built[Index].Name), Stop - BuiltStarts[Index]);
				Built[Kept] = Built[Index];
				BuiltStarts[Kept++] = BuiltStarts[Index];
			}
		}
		Built.resize(Kept);
		BuiltStarts.resize(Kept);
		const float Reach = Kept ? BuiltStarts.back() + Runtime.BlockDuration(Built.back()) : Stop;
		if (!Layer && Mode == EndMode::Hold && Reach < Stop)
		{
			Built.back().Hold += Stop - Reach;
		}
		for (u32 Index = 0; Index < Kept; ++Index)
		{
			const float Moment = Curves::LaneInverseArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Setup.TimeScale, BuiltStarts[Index], MaxTime);
			Built[Index].Start = std::clamp(std::round(Moment * 1e5f) * 1e-5f, 0.0f, MaxTime);
		}
		BlocksOf(Setup, Layer) = Built;
	}
	const auto IsValid = [&](const xr_vector<ActorBlock>& Blocks)
	{
		float Ends[2] = { -MaxTime, -MaxTime }, Begin = -MaxTime;
		for (const ActorBlock& Block : Blocks)
		{
			const float From = static_cast<float>(Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Block.Start, Setup.TimeScale)), To = From + Runtime.BlockDuration(Block);
			if (From <= Begin + 1e-4f || To <= Ends[1] + 1e-4f || From < Ends[0] - 1e-4f)
			{
				return false;
			}
			Begin = From;
			Ends[0] = Ends[1];
			Ends[1] = To;
		}
		return true;
	};
	const auto ByStart = [](const ActorBlock& A, const ActorBlock& B) { return A.Start < B.Start; };
	u32 Crowded = 0;
	u8 Origin[MaxActorLayers + 1];
	for (u8 Row = 0; Row <= MaxActorLayers; ++Row)
	{
		Origin[Row] = Row;
	}
	const u8 Created = static_cast<u8>(Setup.Layers.size() + 1);
	for (u8 Row = 0; Row <= Setup.Layers.size(); ++Row)
	{
		for (u32 Index = 1; Index < BlocksOf(Setup, Row).size();)
		{
			Built.assign(BlocksOf(Setup, Row).begin() + (Index >= 2 ? Index - 2 : 0), BlocksOf(Setup, Row).begin() + Index + 1);
			if (IsValid(Built))
			{
				++Index;
				continue;
			}
			ActorBlock Moved = BlocksOf(Setup, Row)[Index];
			const ActorBlock& Previous = BlocksOf(Setup, Row)[Index - 1];
			const float From = static_cast<float>(Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Moved.Start, Setup.TimeScale));
			Moved.FadeIn = std::min(std::max(Moved.FadeIn, static_cast<float>(Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Previous.Start, Setup.TimeScale)) + Runtime.BlockDuration(Previous) - From), Runtime.BlockDuration(Moved));
			const ActorLayer& Source = RowOf(Setup, Row);
			u8 Into = std::max(static_cast<u8>(Row + 1), Created);
			for (; Into <= Setup.Layers.size(); ++Into)
			{
				Built = RowOf(Setup, Into).Blocks;
				Built.insert(std::upper_bound(Built.begin(), Built.end(), Moved, ByStart), Moved);
				if (Origin[Into] == Origin[Row] && IsValid(Built))
				{
					break;
				}
			}
			if (Into > Setup.Layers.size() && Setup.Layers.size() < MaxActorLayers)
			{
				const auto Weight = std::find_if(Target.Lanes.begin(), Target.Lanes.end(), [Row](const Lane& Track) { return Track.Id == Param::LayerWeight && Track.Layer == Row; });
				if (Weight != Target.Lanes.end() && Target.Lanes.size() < MaxLanes)
				{
					Lane Copy = *Weight;
					Copy.Layer = static_cast<u8>(Setup.Layers.size() + 1);
					Target.Lanes.push_back(std::move(Copy));
				}
				Origin[Setup.Layers.size() + 1] = Origin[Row];
				ActorLayer Added { Source.Parts, Source.Mode, Source.IsMuted, Source.Weight };
				for (u16 Part = 0; Source.Parts.empty() && Part < MAX_PARTS; ++Part)
				{
					if (const char* Name = Runtime.PartName(Part))
					{
						Added.Parts.push_back(Name);
					}
				}
				Setup.Layers.push_back(std::move(Added));
			}
			if (Into > Setup.Layers.size())
			{
				++Crowded;
				++Index;
				continue;
			}
			xr_vector<ActorBlock>& Blocks = BlocksOf(Setup, Into);
			Blocks.insert(std::upper_bound(Blocks.begin(), Blocks.end(), Moved, ByStart), Moved);
			BlocksOf(Setup, Row).erase(BlocksOf(Setup, Row).begin() + Index);
		}
	}
	if (Passes < Wanted || HasGaps || Crowded || (Setup.Base.Blocks.empty() && HasClips(Setup)))
	{
		Msg("* Cinema: %s was converted to animation rows%s%s%s%s", Target.Name.c_str(), Passes < Wanted ? "; its repeats were cut at the block limit" : "", HasGaps ? "; its layers now hold between repeats" : "", Crowded ? "; some overlaps were kept on a full row and may play differently" : "", Setup.Base.Blocks.empty() ? "; with an empty first row it shows only while its other rows play" : "");
	}
}

void Player::OutputCamera()
{
	CCameraManager& Manager = g_pGameLevel->Cameras();
	if (Camera && !Camera->IsAttached)
	{
		Camera.reset();
		Effector = nullptr;
	}
	const bool IsGlobal = IsLookingThrough && (IsPlaying || IsEditorOpen) && Shot.IsActive;
	CameraSample Sample = IsGlobal ? Shot : CameraSample {};
	// HACK: parity with the old Cinema Film Maker Editor, which takes cefDemo while its window is open; remove with it (M6).
	const bool IsLegacyOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)];
	const ViewSource Want = IsLegacyOpen ? ViewSource::None : IsGlobal ? ViewSource::Global : IsEditorCamera ? ViewSource::Editor : ViewSource::None;
	if (Want == ViewSource::None)
	{
		if (CurrentView != Want)
		{
			Device.MarkCameraCut();
		}
		HasLastPose = false;
		Release();
		CurrentView = Want;
		if (!IsEditorOpen)
		{
			RestoreDofSwitch();
		}
		return;
	}
	if (!Camera && Manager.GetCamEffector(cefDemo))
	{
		GamePersistent().DofManager().SetCinema(nullptr);
		GamePersistent().DofManager().SetCinemaCamera(nullptr);
		CurrentView = ViewSource::Busy;
		return;
	}
	CameraSample Lens = IsGlobal ? Sample : CameraSample {};
	const Object* Rig = FindCamera(Data);
	if (!IsGlobal && IsDofPreview && Rig && Shot.IsActive && Shot.Block >= 0 && static_cast<u32>(Shot.Block) < CameraCache.size())
	{
		const CameraBlock& Block = Rig->Camera.Blocks[Shot.Block];
		const float Shift = Cameras::FocusDepth(Block, CameraCache[Shot.Block], Time - Rig->Span.Start - Block.Start, EditorPose.Position, EditorPose.Orientation.k) - Shot.B.Dof.y;
		Lens.B = Shot.B;
		Lens.B.Dof.add(Fvector4().set(Shift, Shift, Shift, 0.0f));
		Lens.A = Lens.B;
	}
	OutputDof(Lens);
	if (!Camera)
	{
		Camera = xr_make_shared<CameraState>();
		Effector = Manager.AddCamEffector(new CameraEffector(Camera));
	}
	if (!IsGlobal)
	{
		Sample.A = Sample.B = EditorPose;
	}
	const CameraPose Pose = Cameras::Mix(Sample);
	Camera->Position = Pose.Position;
	Camera->Orientation = Pose.Orientation;
	Camera->FovA = Sample.A.Fov;
	Camera->FovB = Sample.B.Fov;
	Camera->Weight = Sample.Weight;
	const s32 Block = IsGlobal ? Sample.Block : -1;
	if (CurrentView != Want || !HasLastPose || IsCut(LastPose, LastBlock, Pose, Block))
	{
		Device.MarkCameraCut();
	}
	if (IsGlobal && Data.Header.IsWorldFollowing && ai().get_alife())
	{
		ai().alife().set_switch_centre(Pose.Position);
	}
	LastPose = Pose;
	LastBlock = Block;
	HasLastPose = true;
	CurrentView = Want;
}

void Player::OutputWeather()
{
	if (!g_pGamePersistent || !(IsPlaying || IsEditorOpen))
	{
		return;
	}
	EnvOverride Mixed;
	float Share = 0.0f, Open = 1.0f;
	bool IsUsed = false;
	const auto Layer = [this](const Object& Item)
	{
		const auto Group = std::find_if(Data.Header.Folders.begin(), Data.Header.Folders.end(), [&Item](const Folder& Other) { return Other.Id == Item.FolderId; });
		return Item.FolderId && Group != Data.Header.Folders.end() ? std::pair<u32, u32>(Group->Row, Item.Row + 1) : std::pair<u32, u32>(Item.Row, 0u);
	};
	const Object* Order[16];
	u32 Count = 0;
	for (const Object& Item : Data.Objects)
	{
		const float Local = Time - Item.Span.Start;
		if (Item.Type != ObjectType::Weather || IsSilent(Item) || Local < 0.0f || Local > Item.Span.Length || Count >= 16)
		{
			continue;
		}
		u32 At = Count++;
		for (; At > 0 && Layer(*Order[At - 1]) > Layer(Item); --At)
		{
			Order[At] = Order[At - 1];
		}
		Order[At] = &Item;
	}
	for (u32 Index = 0; Index < Count; ++Index)
	{
		const Object& Item = *Order[Index];
		const WeatherData& Setup = Item.Weather;
		const float Local = Time - Item.Span.Start;
		const float In = Setup.FadeIn > 0.0f ? std::clamp(Local / Setup.FadeIn, 0.0f, 1.0f) : 1.0f, Out = Setup.FadeOut > 0.0f ? std::clamp((Item.Span.Length - Local) / Setup.FadeOut, 0.0f, 1.0f) : 1.0f;
		const float Weight = In * Out;
		if (!(Weight > 0.0f))
		{
			continue;
		}
		const auto IsSet = [&](Param Id) { return IsWeatherSet(Setup, Item.Lanes, Id); };
		const auto Scalar = [&](Param Id) { return Curves::LaneScalar(Item.Lanes, Id, 0, Local, *WeatherField(const_cast<WeatherData&>(Setup), Id, 0)); };
		const auto Color = [&](Param Id, const Fvector& Static) { return Curves::LaneVector(Item.Lanes, Id, Local, Static); };
		const auto Blend = [&](u32 Bit, float* Slots, const float* Values, u32 Size)
		{
			float& Amount = Mixed.Amount[Bit];
			for (u32 At = 0; At < Size; ++At)
			{
				Slots[At] = Amount > 0.0f ? Slots[At] + (Values[At] - Slots[At]) * Weight : Values[At];
			}
			Amount += (1.0f - Amount) * Weight;
			Mixed.Mask |= 1u << Bit;
			IsUsed = true;
		};
		const auto One = [&](u32 Bit, float& Slot, Param Id, float Scale)
		{
			const float Value = Scalar(Id) * Scale;
			if (IsSet(Id))
			{
				Blend(Bit, &Slot, &Value, 1);
			}
		};
		const auto Three = [&](u32 Bit, float* Slot, Param Id, const Fvector& Static)
		{
			const Fvector Value = Color(Id, Static);
			if (IsSet(Id))
			{
				Blend(Bit, Slot, &Value.x, 3);
			}
		};
		if (Setup.Cycle.size() || IsSet(Param::WeatherTime))
		{
			Share *= 1.0f - Weight;
			Open *= 1.0f - Weight;
			if (Weight >= Share)
			{
				Share = Weight;
				Mixed.Cycle = Setup.Cycle;
				Mixed.Time = IsSet(Param::WeatherTime) ? std::clamp(std::fmod(Scalar(Param::WeatherTime), 24.0f) * 3600.0f, 0.0f, 86399.0f) : -1.0f;
			}
			IsUsed = true;
		}
		if (IsSet(Param::SunHeading) || IsSet(Param::SunHeight))
		{
			float Angles[2] = { Mixed.SunHeading, Mixed.SunHeight };
			const float Values[2] = { deg2rad(Scalar(Param::SunHeading)), deg2rad(Scalar(Param::SunHeight)) };
			Blend(0, Angles, Values, 2);
			Mixed.SunHeading = Angles[0];
			Mixed.SunHeight = Angles[1];
		}
		Three(1, &Mixed.Look.SunColor.x, Param::SunColor, Setup.SunColor);
		One(2, Mixed.SunBrightness, Param::SunBrightness, 1.0f);
		One(3, Mixed.Look.SunShafts, Param::SunShafts, 1.0f);
		Three(4, &Mixed.Look.Ambient.x, Param::AmbientColor, Setup.AmbientColor);
		Three(5, &Mixed.Look.HemiColor.x, Param::HemiColor, Setup.HemiColor);
		Three(6, &Mixed.Look.FogColor.x, Param::FogColor, Setup.FogColor);
		One(7, Mixed.Look.FogDensity, Param::FogDensity, 1.0f);
		One(8, Mixed.Look.FogDistance, Param::FogDistance, 1.0f);
		One(9, Mixed.Look.FarPlane, Param::FarPlane, 1.0f);
		One(10, Mixed.Look.RainDensity, Param::RainDensity, 1.0f);
		One(11, Mixed.Look.WindVelocity, Param::WindSpeed, 1.0f);
		One(12, Mixed.Look.WindDirection, Param::WindDirection, PI / 180.0f);
		One(13, Mixed.Look.SkyRotation, Param::SkyRotation, PI / 180.0f);
	}
	if (IsUsed)
	{
		Mixed.Weight = 1.0f - Open;
		Mixed.Frame = Device.dwFrame;
		g_pGamePersistent->Environment().Override = Mixed;
	}
}

void Player::OutputPost()
{
	if (!g_pGameLevel || !(IsPlaying || IsEditorOpen))
	{
		return;
	}
	PostData Look;
	decltype(Device.PostGrade) Grade;
	bool IsUsed = false;
	const auto Layer = [this](const Object& Item)
	{
		const auto Group = std::find_if(Data.Header.Folders.begin(), Data.Header.Folders.end(), [&Item](const Folder& Other) { return Other.Id == Item.FolderId; });
		return Item.FolderId && Group != Data.Header.Folders.end() ? std::pair<u32, u32>(Group->Row, Item.Row + 1) : std::pair<u32, u32>(Item.Row, 0u);
	};
	const Object* Order[16];
	u32 Count = 0;
	for (const Object& Item : Data.Objects)
	{
		const float Local = Time - Item.Span.Start;
		if (Item.Type != ObjectType::Post || IsSilent(Item) || Local < 0.0f || Local > Item.Span.Length || Count >= 16)
		{
			continue;
		}
		u32 At = Count++;
		for (; At > 0 && Layer(*Order[At - 1]) > Layer(Item); --At)
		{
			Order[At] = Order[At - 1];
		}
		Order[At] = &Item;
	}
	for (u32 Index = 0; Index < Count; ++Index)
	{
		const Object& Item = *Order[Index];
		PostData& Setup = const_cast<PostData&>(Item.Post);
		const float Local = Time - Item.Span.Start;
		const float In = Setup.FadeIn > 0.0f ? std::clamp(Local / Setup.FadeIn, 0.0f, 1.0f) : 1.0f, Out = Setup.FadeOut > 0.0f ? std::clamp((Item.Span.Length - Local) / Setup.FadeOut, 0.0f, 1.0f) : 1.0f;
		const float Weight = In * Out;
		for (u32 Id = static_cast<u32>(Param::PostBase); Weight > 0.0f && Id <= static_cast<u32>(Param::PostMotionBlur); ++Id)
		{
			const Param Which = static_cast<Param>(Id);
			if (!IsPostSet(Setup, Item.Lanes, Which))
			{
				continue;
			}
			IsUsed = true;
			const u32 Bit = Id >= static_cast<u32>(Param::PostExposure) ? Id - static_cast<u32>(Param::PostExposure) : 5;
			if (Bit < 5)
			{
				const float Value = Curves::LaneScalar(Item.Lanes, Which, 0, Local, *PostField(Setup, Which, 0));
				Grade.Value[Bit] = Grade.Amount[Bit] > 0.0f ? Grade.Value[Bit] + (Value - Grade.Value[Bit]) * Weight : Value;
				Grade.Amount[Bit] += (1.0f - Grade.Amount[Bit]) * Weight;
				Grade.Mask |= 1u << Bit;
				continue;
			}
			for (u8 Component = 0; Component < Info(Which).Components; ++Component)
			{
				const Lane* Track = Curves::FindLane(Item.Lanes, Which, 0);
				const float Static = *PostField(Setup, Which, Component);
				float& Slot = *PostField(Look, Which, Component);
				Slot += ((Track ? Curves::Value(Track->Curves[Component], Local, Static) : Static) - Slot) * Weight;
			}
		}
	}
	if (!IsUsed)
	{
		return;
	}
	const float Keep = 1.0f - std::clamp(Look.Fade, 0.0f, 1.0f);
	SPPInfo Shown;
	Shown.blur = Look.Blur;
	Shown.gray = Look.Gray;
	Shown.duality.set(Look.DualityH, Look.DualityV);
	Shown.noise.set(Look.Noise, Look.Grain, Look.NoiseFps);
	Shown.color_base.set(Look.Base.x * Keep, Look.Base.y * Keep, Look.Base.z * Keep);
	Shown.color_gray.set(Look.GrayColor.x, Look.GrayColor.y, Look.GrayColor.z);
	Shown.color_add.set(Look.Add.x * Keep + Look.FadeColor.x * (1.0f - Keep), Look.Add.y * Keep + Look.FadeColor.y * (1.0f - Keep), Look.Add.z * Keep + Look.FadeColor.z * (1.0f - Keep));
	g_pGameLevel->Cameras().SetPost(Shown);
	Grade.Frame = Device.dwFrame;
	Device.PostGrade = Grade;
}

bool Player::IsCut(const CameraPose& From, s32 FromBlock, const CameraPose& To, s32 ToBlock) const
{
	const float Moved = From.Position.distance_to(To.Position), Turned = rad2deg(std::acos(std::clamp(From.Orientation.k.dotproduct(To.Orientation.k), -1.0f, 1.0f)));
	const SceneHeader& Header = Data.Header;
	return (FromBlock != ToBlock && (Moved > 0.01f || Turned > 0.5f)) || (Header.CutDistance > 0.0f && Moved > Header.CutDistance) || (Header.CutAngle > 0.0f && Turned > Header.CutAngle);
}

bool Player::IsCutBetween(float From, float To) const
{
	const Object* Rig = FindCamera(Data);
	CameraSample Early, Late;
	if (!Rig || IsSilent(*Rig))
	{
		return false;
	}
	Cameras::Sample(*Rig, CameraCache, ShakeAreas, ShakeOwners, From, Early);
	Cameras::Sample(*Rig, CameraCache, ShakeAreas, ShakeOwners, To, Late);
	return Early.IsActive != Late.IsActive || (Late.IsActive && IsCut(Cameras::Mix(Early), Early.Block, Cameras::Mix(Late), Late.Block));
}

void Player::OutputDof(const CameraSample& Lens)
{
	CDofManager& Dof = GamePersistent().DofManager();
	const bool IsEarly = Lens.A.HasDof && Lens.A.IsPhysical, IsLate = Lens.B.HasDof && Lens.B.IsPhysical;
	if (IsEarly || IsLate)
	{
		const float Share = std::clamp(Lens.Weight, 0.0f, 1.0f);
		const CameraPose& Lead = IsLate && (!IsEarly || Share >= 0.5f) ? Lens.B : Lens.A;
		Fvector4 Mixed = Lead.Lens, Shape = Lead.Shape, Timing = Lead.Timing;
		if (IsEarly && IsLate)
		{
			Mixed.lerp(Lens.A.Lens, Lens.B.Lens, Share);
			Shape.lerp(Lens.A.Shape, Lens.B.Shape, Share);
			Timing.lerp(Lens.A.Timing, Lens.B.Timing, Share);
		}
		SDofCameraSettings Settings;
		Settings.Mode = Lead.Lens.w > 0.5f ? EDofMode::Autofocus : EDofMode::Manual;
		Settings.FNumber = Mixed.x;
		Settings.ManualFocalLength = Lead.Lens.y > 0.0f;
		Settings.FocalLength = Settings.ManualFocalLength ? Mixed.y : Settings.FocalLength;
		Settings.FocusDistance = Mixed.z;
		Settings.FocusPoint.set(Shape.x, Shape.y);
		Settings.MaxRadius = Shape.z;
		Settings.Squeeze = Shape.w;
		Settings.FocusTime = Settings.Mode == EDofMode::Autofocus ? Timing.x : 0.0f;
		Settings.TransitionTime = Timing.y;
		Settings.Swirl = Timing.z;
		Dof.SetForced(false);
		Dof.SetCinema(nullptr);
		Dof.SetCinemaCamera(&Settings);
		return;
	}
	Dof.SetCinemaCamera(nullptr);
	const Fvector& Base = Dof.GetBase();
	float Lo = 0.0f, Hi = 0.0f;
	const Fvector4 Rest = Fvector4().set(Base.x, Base.y, Base.z, Console ? Console->GetFloat("r2_dof_kernel", Lo, Hi) : (Lens.B.HasDof ? Lens.B.Dof.w : Lens.A.Dof.w));
	Fvector4 Mixed;
	Mixed.lerp(Lens.A.HasDof ? Lens.A.Dof : Rest, Lens.B.HasDof ? Lens.B.Dof : Rest, std::clamp(Lens.Weight, 0.0f, 1.0f));
	const bool HasDof = Lens.A.HasDof || Lens.B.HasDof;
	IsDofForced = IsDofForced || HasDof;
	Dof.SetForced(IsDofForced);
	Dof.SetCinema(HasDof ? &Mixed : nullptr);
}

void Player::RestoreDofSwitch()
{
	if (g_pGamePersistent)
	{
		GamePersistent().DofManager().SetForced(false);
	}
	IsDofForced = false;
}

void Player::FlushCamera()
{
	if (Camera && Camera->IsAttached && g_pGameLevel && (Device.Paused() || g_pGameLevel->Cameras().GetCamEffector(cefDemo) != Effector) && g_actor && g_actor->cam_Active())
	{
		g_pGameLevel->Cameras().UpdateFromCamera(g_actor->cam_Active());
	}
}

void Player::Release()
{
	if (g_pGamePersistent)
	{
		GamePersistent().DofManager().SetCinema(nullptr);
		GamePersistent().DofManager().SetCinemaCamera(nullptr);
	}
	if (Camera)
	{
		Camera->IsActive = false;
	}
	if (Camera && Camera->IsAttached && g_pGameLevel && g_pGameLevel->Cameras().GetCamEffector(cefDemo) == Effector)
	{
		g_pGameLevel->Cameras().RemoveCamEffector(cefDemo);
	}
	Camera.reset();
	Effector = nullptr;
}

void Player::EndLevel()
{
	Release();
	RestoreDofSwitch();
	DropRuntimes();
}

void Player::DropRuntimes()
{
	Actors.clear();
	Lights.clear();
	Emitters.clear();
	Decals.clear();
	Voices.clear();
	CameraCache.clear();
	ShakeAreas.clear();
	ShakeOwners.clear();
}

void Player::GhostBones(u32 ObjectId, float At, xr_vector<Fvector>& Lines)
{
	const s32 Index = IndexOf(Data, ObjectId);
	if (Index < 0 || static_cast<u32>(Index) >= States.size() || Data.Objects[Index].Type != ObjectType::Actor || !FindActor(ObjectId))
	{
		return;
	}
	EvaluateLinked(static_cast<u32>(Index), At);
	if (States[Index].IsReady)
	{
		ActorAt(ObjectId).Bones(States[Index].World, Lines);
	}
	EvaluateLinked(static_cast<u32>(Index), Time);
}

bool Player::BakeMotion(u32 ObjectId, float From, u32 Frames, bool IsPoseOnly, xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts)
{
	const s32 Index = IndexOf(Data, ObjectId);
	if (!Frames || Index < 0 || static_cast<u32>(Index) >= States.size() || Data.Objects[Index].Type != ObjectType::Actor || !FindActor(ObjectId))
	{
		return false;
	}
	ActorRuntime& Runtime = ActorAt(ObjectId);
	Runtime.IsPoseOnly = IsPoseOnly;
	bool IsDone = true;
	for (u32 Frame = 0; IsDone && Frame < Frames; ++Frame)
	{
		EvaluateLinked(static_cast<u32>(Index), From + static_cast<float>(Frame) / Omf::MotionRate);
		IsDone = States[Index].IsReady;
		if (IsDone)
		{
			Runtime.Locals(Turns, Shifts);
		}
	}
	Runtime.IsPoseOnly = false;
	EvaluateLinked(static_cast<u32>(Index), Time);
	return IsDone;
}

bool Player::BakeClip(u32 ObjectId, s32 Clip, xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts)
{
	const s32 Index = IndexOf(Data, ObjectId);
	if (Index < 0 || static_cast<u32>(Index) >= States.size() || Data.Objects[Index].Type != ObjectType::Actor || !FindActor(ObjectId) || static_cast<u32>(Clip) >= Data.Objects[Index].Actor.Clips.size())
	{
		return false;
	}
	ActorRuntime& Runtime = ActorAt(ObjectId);
	const u32 Frames = std::min(static_cast<u32>(Data.Objects[Index].Actor.Clips[Clip].Length * Omf::MotionRate) + 2, Omf::MaxMotionFrames);
	Runtime.IsPoseOnly = true;
	bool IsDone = true;
	for (u32 Frame = 0; IsDone && Frame < Frames; ++Frame)
	{
		Runtime.Preview = { Clip, static_cast<float>(Frame) / Omf::MotionRate, Device.dwFrame };
		PoseActor(static_cast<u32>(Index), Time, true);
		IsDone = States[Index].IsReady;
		if (IsDone)
		{
			Runtime.Locals(Turns, Shifts);
		}
	}
	Runtime.IsPoseOnly = false;
	Runtime.Preview = {};
	PoseActor(static_cast<u32>(Index), Time, true);
	return IsDone;
}

u32 Player::BakeSource(u32 ObjectId, const shared_str& Source, const shared_str& Name, xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts)
{
	const s32 Index = IndexOf(Data, ObjectId);
	float Length = 0.0f;
	if (Index < 0 || static_cast<u32>(Index) >= States.size() || Data.Objects[Index].Type != ObjectType::Actor || !FindActor(ObjectId) || !ActorAt(ObjectId).SetSolo(Source, Name, Length))
	{
		return 0;
	}
	ActorRuntime& Runtime = ActorAt(ObjectId);
	const u32 Frames = std::min(static_cast<u32>(Length * Omf::MotionRate) + 1, Omf::MaxMotionFrames);
	u32 Done = 0;
	for (; Done < Frames; ++Done)
	{
		Runtime.SoloTime = std::min(static_cast<float>(Done) / Omf::MotionRate, Length);
		PoseActor(static_cast<u32>(Index), Time, true);
		if (!States[Index].IsReady)
		{
			break;
		}
		Runtime.Locals(Turns, Shifts);
	}
	Runtime.SoloMotion = MotionID();
	PoseActor(static_cast<u32>(Index), Time, true);
	return Done == Frames ? Frames : 0;
}

const xr_vector<shared_str>& Player::Shaders()
{
	if (IsShadersRead)
	{
		return ShaderNames;
	}
	IsShadersRead = true;
	IReader* File = FS.r_open(_game_data_, "shaders.xr");
	IReader* Blenders = File ? File->open_chunk(2) : nullptr;
	for (u32 Id = 0; IReader* Chunk = Blenders ? Blenders->open_chunk(Id) : nullptr; ++Id)
	{
		string128 Name = {};
		if (Chunk->elapsed() >= static_cast<int>(sizeof(u64) + sizeof(Name)))
		{
			Chunk->r_u64();
			Chunk->r(Name, sizeof(Name));
			Name[sizeof(Name) - 1] = 0;
			Name[0] ? ShaderNames.push_back(Name) : (void)0;
		}
		Chunk->close();
	}
	Blenders ? Blenders->close() : (void)0;
	File ? FS.r_close(File) : (void)0;
	std::sort(ShaderNames.begin(), ShaderNames.end(), ShaderLess);
	ShaderNames.erase(std::unique(ShaderNames.begin(), ShaderNames.end()), ShaderNames.end());
	return ShaderNames;
}

bool Player::PathPoint(u32 ObjectId, float At, Fvector& Point) const
{
	const s32 Index = IndexOf(Data, ObjectId);
	const Object* Item = Index >= 0 && static_cast<u32>(Index) < States.size() ? &Data.Objects[Index] : nullptr;
	const Fvector* Static = Item && Item->Type != ObjectType::Camera ? PlacementOf(*Item, 0) : nullptr;
	if (!Static || At < Item->Span.Start || At > Item->Span.Start + Item->Span.Length)
	{
		return false;
	}
	Fmatrix World = Fidentity;
	World.c = Curves::LaneVector(Item->Lanes, Param::Position, At - Item->Span.Start, *Static);
	Spaces::Compose(States[Index].Frames, World);
	Point = World.c;
	return true;
}

ActorRuntime& Player::ActorAt(u32 ObjectId)
{
	return *PlayerLocal::RuntimeOf(Actors, ObjectId);
}

Player* FindPlayer()
{
	return PlayerLocal::g_Player.get();
}

Player& EnsurePlayer()
{
	if (!PlayerLocal::g_Player)
	{
		PlayerLocal::g_Player = xr_make_unique<Player>();
	}
	return *PlayerLocal::g_Player;
}

void ShutdownPlayer()
{
	PlayerLocal::g_Player.reset();
}

void EndPlayerLevel()
{
	if (PlayerLocal::g_Player)
	{
		PlayerLocal::g_Player->EndLevel();
	}
}
}
