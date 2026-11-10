#include "StdAfx.h"
#include "CinemaPlayer.h"
#include "CinemaApi.h"
#include "CinemaSceneIo.h"
#include "../Actor.h"
#include "../GamePersistent.h"
#include "../../xrEngine/CameraManager.h"
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

CameraPose Mix(const CameraSample& Sample)
{
	Fquaternion From, To, Turn;
	From.set(Sample.A.Orientation);
	To.set(Sample.B.Orientation);
	Turn.slerp(From, To, std::clamp(Sample.Weight, 0.0f, 1.0f));
	CameraPose Pose;
	Fmatrix Mixed;
	Fvector Moved;
	Mixed.rotation(Turn.normalize());
	Mixed.transform_dir(Moved, Sample.Shift);
	Pose.Orientation.mul(Mixed, Sample.Turn);
	Pose.Position.lerp(Sample.A.Position, Sample.B.Position, Sample.Weight).add(Moved);
	return Pose;
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

const LightRuntime* Player::FindLight(u32 ObjectId) const
{
	return ShouldDropRuntimes ? nullptr : PlayerLocal::RuntimeOf(Lights, ObjectId);
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
		const CameraBlock& Block = Rig->Camera.Blocks[Index];
		BlockCache& Cache = CameraCache[Index];
		Length = std::max(Length, Block.Start + Block.Length);
		if (Cache.Keys != Block.Keys)
		{
			Cache.Keys = Block.Keys;
			Cache.Path.Build(Block);
		}
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
	xr_vector<PoseKey>* Keys = IsBlock ? &Target->Camera.Blocks[Request.Block].Keys : nullptr;
	Fvector* const Place = PlacementOf(*Target, 0);
	Fvector* const Turn = PlacementOf(*Target, 1);
	Times.clear();
	for (u32 Index = 0; Keys && Index < Keys->size(); ++Index)
	{
		Times.push_back(Start + Cameras::LocalFromPath(Target->Camera.Blocks[Request.Block], CameraCache[Request.Block], (*Keys)[Index].Time));
	}
	for (const Lane& Track : Target->Lanes)
	{
		for (u32 Component = 0; !IsBlock && Track.Id <= Param::Rotation && Component < 3; ++Component)
		{
			for (const Key& Item : Track.Curves[Component].Keys)
			{
				Times.push_back(Start + Item.Time);
			}
		}
	}
	if (!IsBlock)
	{
		std::sort(Times.begin(), Times.end());
		Times.erase(std::unique(Times.begin(), Times.end(), [HalfFrame](float A, float B) { return B - A <= HalfFrame; }), Times.end());
	}
	const bool IsStatic = !IsBlock && Times.empty();
	if (IsStatic)
	{
		Times.push_back(Time);
	}
	Converted.clear();
	Fvector Reference = Fvector().set(0.0f, 0.0f, 0.0f);
	for (u32 Index = 0; Index < Times.size(); ++Index)
	{
		EvaluateObjects(Times[Index], Needed.data());
		LinkFrames Old, New;
		Resolve(Links, Target->Type == ObjectType::Actor, Old);
		Resolve(Request.Links, Target->Type == ObjectType::Actor, New);
		Fvector Position = Keys ? (*Keys)[Index].Position : PlayerLocal::KeyValues(Target->Lanes, Param::Position, Times[Index] - Start, *Place);
		Fvector Rotation = Keys ? (*Keys)[Index].Rotation : PlayerLocal::KeyValues(Target->Lanes, Param::Rotation, Times[Index] - Start, *Turn);
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
		if (Keys)
		{
			(*Keys)[Index].Position = Converted[2 * Index];
			(*Keys)[Index].Rotation = Converted[2 * Index + 1];
		}
		else if (IsStatic)
		{
			*Place = Converted[0];
			*Turn = Converted[1];
		}
		for (u32 Kind = 0; !Keys && !IsStatic && Kind < 2; ++Kind)
		{
			Lane& Track = Curves::EnsureLane(Target->Lanes, static_cast<Param>(Kind), 0);
			for (u32 Component = 0; Component < 3; ++Component)
			{
				PlayerLocal::PutKey(Track.Curves[Component], Times[Index] - Start, Converted[2 * Index + Kind][Component], HalfFrame);
			}
		}
	}
	if (Keys)
	{
		CameraCache[Request.Block].Keys = *Keys;
		CameraCache[Request.Block].Path.Build(Target->Camera.Blocks[Request.Block]);
	}
	Links = Request.Links;
}

void Player::EvaluateObjects(float At, const u8* Mask)
{
	Shot = CameraSample {};
	for (u32 Index = 0; !Mask && Index < States.size(); ++Index)
	{
		States[Index].IsReady = false;
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
		}
		else if (Type == ObjectType::Light)
		{
			PoseLight(Index, At, !Mask);
		}
		else
		{
			PoseParticle(Index, At, !Mask);
		}
	}
}

void Player::PoseActor(u32 Index, float At, bool IsNeeded)
{
	const Object& Item = Data.Objects[Index];
	ActorRuntime& Runtime = ActorAt(Item.Id);
	ObjectState& State = States[Index];
	Resolve(Item.Attach, true, State.Frames);
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
		Resolve(Rig.Camera.Blocks[Block].Attach, false, Cache.Frames);
		Cache.HasFocus = Focus.ObjectId && Frame(Focus, Point, Cache.Frames.Problems, MissingFocusBone);
		Cache.FocusPoint = Point.c;
		Cache.Frames.Problems |= Focus.ObjectId && !Cache.HasFocus ? MissingFocus : 0;
	}
	if (!IsSilent(Rig))
	{
		Cameras::Sample(Rig, CameraCache, ShakeAreas, ShakeOwners, At, Shot);
	}
	const CameraPose Pose = Shot.IsActive ? PlayerLocal::Mix(Shot) : EditorPose;
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
	Runtime.Drive({ .World = States[Index].World, .Tau = Tau, .Played = Played, .Rate = Rate, .Preroll = std::clamp(EffectTime, 0.0f, MaxPreroll), .Size = Size, .IsShown = IsLive && Tau >= 0.0f && Tau < Item.Span.Length, .IsAfter = IsLive && Tau >= Item.Span.Length, .ShouldRetrigger = Setup.ShouldRetrigger });
}

float Player::PoseTransform(u32 Index, float At, const Fvector& Position, const Fvector& Rotation)
{
	const Object& Item = Data.Objects[Index];
	ObjectState& State = States[Index];
	const float Tau = At - Item.Span.Start;
	const float Local = std::clamp(Tau, 0.0f, Item.Span.Length);
	Resolve(Item.Attach, false, State.Frames);
	State.World = PlayerLocal::PoseMatrix(Curves::LaneVector(Item.Lanes, Param::Position, Local, Position), Curves::LaneVector(Item.Lanes, Param::Rotation, Local, Rotation));
	Spaces::Compose(State.Frames, State.World);
	State.IsReady = !(State.Frames.Problems & MissingParent);
	return Tau;
}

void Player::Resolve(const Attachment& Links, bool IsActor, LinkFrames& Out) const
{
	Fmatrix Target = Fidentity;
	Out = LinkFrames {};
	Out.IsYawOnly = IsActor || Links.IsYawOnly;
	Out.HasParent = Links.Parent.ObjectId && Frame(Links.Parent, Out.Parent, Out.Problems, MissingParentBone);
	Out.HasLook = Links.LookAt.ObjectId && Frame(Links.LookAt, Target, Out.Problems, MissingTargetBone);
	Out.Problems |= (Links.Parent.ObjectId && !Out.HasParent ? MissingParent : 0) | (Links.LookAt.ObjectId && !Out.HasLook ? MissingTarget : 0);
	Out.Look = Target.c;
	if (Out.HasParent && (Links.IsRollLocked || Links.IsPitchLocked))
	{
		Fvector Angles;
		const Fvector Origin = Out.Parent.c;
		Out.Parent.getHPB(Angles);
		Out.Parent.setHPB(Angles.x, Links.IsPitchLocked ? 0.0f : Angles.y, Links.IsRollLocked ? 0.0f : Angles.z);
		Out.Parent.c = Origin;
	}
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
		float Ends[2] = { -MaxTime, -MaxTime };
		for (const ActorBlock& Block : Blocks)
		{
			const float From = static_cast<float>(Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Block.Start, Setup.TimeScale)), To = From + Runtime.BlockDuration(Block);
			if (To < Ends[1] - 1e-4f || From < Ends[0] - 1e-4f)
			{
				return false;
			}
			Ends[0] = Ends[1];
			Ends[1] = To;
		}
		return true;
	};
	const auto ByStart = [](const ActorBlock& A, const ActorBlock& B) { return A.Start < B.Start; };
	u32 Crowded = 0;
	for (u8 Row = 0; Row <= Setup.Layers.size(); ++Row)
	{
		for (u32 Index = 1; Index < BlocksOf(Setup, Row).size();)
		{
			Built.assign(BlocksOf(Setup, Row).begin(), BlocksOf(Setup, Row).begin() + Index + 1);
			if (IsValid(Built))
			{
				++Index;
				continue;
			}
			ActorBlock Moved = BlocksOf(Setup, Row)[Index];
			const ActorBlock& Previous = BlocksOf(Setup, Row)[Index - 1];
			const float From = static_cast<float>(Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Moved.Start, Setup.TimeScale));
			Moved.FadeIn = std::max(Moved.FadeIn, static_cast<float>(Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Previous.Start, Setup.TimeScale)) + Runtime.BlockDuration(Previous) - From);
			const ActorLayer& Source = RowOf(Setup, Row);
			u8 Into = static_cast<u8>(Row + 1);
			for (; Into <= Setup.Layers.size(); ++Into)
			{
				const ActorLayer& Other = RowOf(Setup, Into);
				Built = Other.Blocks;
				Built.insert(std::upper_bound(Built.begin(), Built.end(), Moved, ByStart), Moved);
				if (Other.Parts == Source.Parts && Other.Mode == Source.Mode && IsValid(Built))
				{
					break;
				}
			}
			if (Into > Setup.Layers.size() && Setup.Layers.size() < MaxActorLayers)
			{
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
	const CameraPose Pose = PlayerLocal::Mix(Sample);
	Camera->Position = Pose.Position;
	Camera->Orientation = Pose.Orientation;
	Camera->FovA = Sample.A.Fov;
	Camera->FovB = Sample.B.Fov;
	Camera->Weight = Sample.Weight;
	CurrentView = Want;
}

void Player::OutputDof(const CameraSample& Lens)
{
	CDofManager& Dof = GamePersistent().DofManager();
	const Fvector& Base = Dof.GetBase();
	const Fvector4 Rest = Fvector4().set(Base.x, Base.y, Base.z, Lens.B.HasDof ? Lens.B.Dof.w : Lens.A.Dof.w);
	Fvector4 Mixed;
	Mixed.lerp(Lens.A.HasDof ? Lens.A.Dof : Rest, Lens.B.HasDof ? Lens.B.Dof : Rest, std::clamp(Lens.Weight, 0.0f, 1.0f));
	const bool HasDof = Lens.A.HasDof || Lens.B.HasDof;
	if (HasDof && !IsDofForced && Console && !Console->GetBool("r2_dof_enable"))
	{
		IsDofForced = true;
		Console->Execute("r2_dof_enable 1");
	}
	Dof.SetCinema(HasDof ? &Mixed : nullptr);
}

void Player::RestoreDofSwitch()
{
	if (IsDofForced && Console)
	{
		Console->Execute("r2_dof_enable 0");
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
	CameraCache.clear();
	ShakeAreas.clear();
	ShakeOwners.clear();
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
