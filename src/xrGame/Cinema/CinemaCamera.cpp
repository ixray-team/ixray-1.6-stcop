#include "StdAfx.h"
#include "CinemaCamera.h"
#include "CinemaShake.h"

namespace Cinema
{
CameraEffector::CameraEffector(xr_shared_ptr<CameraState> Shared) : CEffectorCam(cefDemo, FLT_MAX), State(std::move(Shared))
{
}

CameraEffector::~CameraEffector()
{
	State->IsAttached = false;
}

bool CameraEffector::AbsolutePositioning()
{
	return true;
}

bool CameraEffector::ProcessCam(SCamEffectorInfo& Info)
{
	if (!State->IsActive)
	{
		return false;
	}
	Info.p = State->Position;
	Info.d = State->Orientation.k;
	Info.n = State->Orientation.j;
	Info.fFov = _lerp(State->FovA > 0.0f ? State->FovA : Info.fFov, State->FovB > 0.0f ? State->FovB : Info.fFov, State->Weight);
	return true;
}

void Spline::Build(const CameraBlock& Block)
{
	Points.clear();
	for (u32 Index = 0; Index < Block.Keys.size(); ++Index)
	{
		const PoseKey& Item = Block.Keys[Index];
		if (!Points.empty() && Item.Time <= Points.back().Time)
		{
			continue;
		}
		Fmatrix Rotation;
		Rotation.setHPB(deg2rad(Item.Rotation.x), deg2rad(Item.Rotation.y), deg2rad(Item.Rotation.z));
		Fquaternion Q;
		Q.set(Rotation).normalize();
		if (!Points.empty())
		{
			const double* Last = Points.back().Value;
			if (Last[3] * Q.x + Last[4] * Q.y + Last[5] * Q.z + Last[6] * Q.w < 0.0)
			{
				Q.set(-Q.w, -Q.x, -Q.y, -Q.z);
			}
		}
		Points.push_back({ Item.Time, Item.Mode, Index, { Item.Position.x, Item.Position.y, Item.Position.z, Q.x, Q.y, Q.z, Q.w } });
	}
	for (size_t Index = 0; Index < Points.size(); ++Index)
	{
		Point& Here = Points[Index];
		const PoseKey& Item = Block.Keys[Here.Key];
		const Point& Previous = Points[Index > 0 ? Index - 1 : Index];
		const Point& Next = Points[Index + 1 < Points.size() ? Index + 1 : Index];
		const double Whole = static_cast<double>(Next.Time) - Previous.Time, Before = static_cast<double>(Here.Time) - Previous.Time, After = static_cast<double>(Next.Time) - Here.Time;
		const double T = Item.Tension, C = Item.Continuity, B = Item.Bias, Lead = Whole > 0.0 ? Before / Whole : 0.0;
		const bool IsPlain = !(T != 0.0 || C != 0.0 || B != 0.0);
		for (u32 Component = 0; Component < 7; ++Component)
		{
			const double Central = Whole > 0.0 ? (Next.Value[Component] - Previous.Value[Component]) / Whole : 0.0;
			const double Incoming = Before > 0.0 ? (Here.Value[Component] - Previous.Value[Component]) / Before * Lead : 0.0;
			const double Outgoing = After > 0.0 ? (Next.Value[Component] - Here.Value[Component]) / After * (1.0 - Lead) : 0.0;
			const double Out = IsPlain ? Central : (1.0 - T) * ((1.0 + C) * (1.0 + B) * Incoming + (1.0 - C) * (1.0 - B) * Outgoing);
			const double In = IsPlain ? Central : (1.0 - T) * ((1.0 - C) * (1.0 + B) * Incoming + (1.0 + C) * (1.0 - B) * Outgoing);
			Here.Ahead[Component] = Out * After / 3.0;
			Here.Back[Component] = In * Before / 3.0;
		}
		for (u32 Axis = 0; Item.Tangent != TangentMode::Auto && Axis < 3; ++Axis)
		{
			Here.Ahead[Axis] = Item.OutHandle[Axis];
			Here.Back[Axis] = -Item.InHandle[Axis];
		}
	}
}

bool Spline::Handles(u32 Key, Fvector& In, Fvector& Out) const
{
	const auto Found = std::find_if(Points.begin(), Points.end(), [Key](const Point& Item) { return Item.Key == Key; });
	if (Found == Points.end())
	{
		return false;
	}
	const auto Fit = [](double Value) { return static_cast<float>(std::clamp(Value, -static_cast<double>(MaxHandle), static_cast<double>(MaxHandle))); };
	In.set(Fit(-Found->Back[0]), Fit(-Found->Back[1]), Fit(-Found->Back[2]));
	Out.set(Fit(Found->Ahead[0]), Fit(Found->Ahead[1]), Fit(Found->Ahead[2]));
	return true;
}

void Spline::Evaluate(float Time, Fvector& Position, Fmatrix& Orientation) const
{
	if (Points.empty())
	{
		return;
	}
	const auto Upper = std::upper_bound(Points.begin(), Points.end(), Time, [](float Moment, const Point& Item) { return Moment < Item.Time; });
	const size_t After = Upper == Points.end() ? Points.size() - 1 : static_cast<size_t>(Upper - Points.begin());
	const size_t Before = After > 0 && Time < Points[After].Time ? After - 1 : After;
	const Point& A = Points[Before];
	const Point& B = Points[After];
	const double Span = static_cast<double>(B.Time) - A.Time;
	const double T = Span > 0.0 ? std::clamp((static_cast<double>(Time) - A.Time) / Span, 0.0, 1.0) : 0.0;
	const bool IsSmooth = A.Mode == Interp::Bezier && Span > 0.0;
	const double U = IsSmooth ? T : std::clamp(static_cast<double>(Curves::Ease(A.Mode, static_cast<float>(T))), 0.0, 1.0);
	double Values[7];
	for (u32 Component = 0; Component < 7; ++Component)
	{
		const double From = A.Value[Component], To = B.Value[Component];
		Values[Component] = IsSmooth ? Curves::BezierValue(From, From + A.Ahead[Component], To - B.Back[Component], To, T) : From + (To - From) * U;
	}
	Position.set(static_cast<float>(Values[0]), static_cast<float>(Values[1]), static_cast<float>(Values[2]));
	Fquaternion Q;
	Q.set(static_cast<float>(Values[6]), static_cast<float>(Values[3]), static_cast<float>(Values[4]), static_cast<float>(Values[5]));
	if (!IsSmooth || Points.size() < 3 || Q.magnitude() < EPS_S)
	{
		Fquaternion From, To;
		From.set(static_cast<float>(A.Value[6]), static_cast<float>(A.Value[3]), static_cast<float>(A.Value[4]), static_cast<float>(A.Value[5]));
		To.set(static_cast<float>(B.Value[6]), static_cast<float>(B.Value[3]), static_cast<float>(B.Value[4]), static_cast<float>(B.Value[5]));
		Q.slerp(From, To, static_cast<float>(U));
	}
	Orientation.rotation(Q.normalize());
}
}

namespace Cinema::Cameras
{
static float SampleShake(const ShakePreset& Preset, double Frame, u32 Channel)
{
	const double Frames = static_cast<double>(Preset.Count);
	Frame = std::fmod(Frame, Frames);
	if (Frame < 0.0)
	{
		Frame += Frames;
	}
	const double Whole = std::floor(Frame);
	const u32 Index = static_cast<u32>(Whole) % Preset.Count;
	const float T = static_cast<float>(Frame - Whole);
	const float A = Preset.Samples[(Index + Preset.Count - 1) % Preset.Count].Values[Channel];
	const float B = Preset.Samples[Index].Values[Channel];
	const float C = Preset.Samples[(Index + 1) % Preset.Count].Values[Channel];
	const float D = Preset.Samples[(Index + 2) % Preset.Count].Values[Channel];
	return B + 0.5f * T * (C - A + T * (2.0f * A - 5.0f * B + 4.0f * C - D + T * (3.0f * (B - C) + D - A)));
}

static bool IsPlaying(const ShakeClip& Clip, float Local)
{
	return Clip.Setup.IsEnabled && Local >= Clip.Start && Local < Clip.Start + Clip.Length;
}

static float ShakeWeight(const CameraData& Data, u32 Index, float Local)
{
	const ShakeClip& Clip = Data.Shakes[Index];
	const float Inside = Local - Clip.Start;
	if (!IsPlaying(Clip, Local))
	{
		return 0.0f;
	}
	float Weight = (Clip.FadeIn > 0.0f ? std::min(Inside / Clip.FadeIn, 1.0f) : 1.0f) * (Clip.FadeOut > 0.0f ? std::min((Clip.Length - Inside) / Clip.FadeOut, 1.0f) : 1.0f) * Curves::LaneScalar(Clip.Lanes, Param::ShakeInfluence, Clip.Row, Inside, Clip.Setup.Influence);
	for (u32 Other = 0; Other < Data.Shakes.size(); ++Other)
	{
		const ShakeClip& Item = Data.Shakes[Other];
		if (Other == Index || Item.Row != Clip.Row || !IsPlaying(Item, Local))
		{
			continue;
		}
		const bool IsLater = Item.Start > Clip.Start || (Item.Start == Clip.Start && Other > Index);
		const ShakeClip& Late = IsLater ? Item : Clip;
		const float Span = (IsLater ? Clip : Item).Start + (IsLater ? Clip : Item).Length - Late.Start;
		const float Mix = Span > 0.0f ? Curves::Ease(Late.BlendEase, std::clamp((Local - Late.Start) / Span, 0.0f, 1.0f)) : 1.0f;
		Weight *= IsLater ? 1.0f - Mix : Mix;
	}
	return Weight;
}

static void ApplyShakes(const CameraData& Data, const xr_vector<Curves::AreaTable>& Areas, const xr_vector<s16>& Owners, const s32 (&Sides)[2], float Mix, float Local, Fvector& Shift, Fmatrix& Turn)
{
	const auto Share = [&](u32 Index)
	{
		const s32 Owner = Index < Owners.size() ? Owners[Index] : -1;
		return Owner >= 0 && Owner == Sides[1] ? Mix : Owner >= 0 && Owner == Sides[0] ? 1.0f - Mix : 1.0f;
	};
	float Kept[MaxShakeRows + 1] = {}, Rows[MaxShakeRows] = {};
	for (u32 Index = 0; Data.OverrideRows && Index < Data.Shakes.size(); ++Index)
	{
		Rows[Data.Shakes[Index].Row % MaxShakeRows] += ShakeWeight(Data, Index, Local) * Share(Index);
	}
	Kept[MaxShakeRows] = 1.0f;
	for (u32 Row = MaxShakeRows; Row-- > 0;)
	{
		Kept[Row] = Row + 1 < MaxShakeRows && ((Data.OverrideRows >> (Row + 1)) & 1) ? Kept[Row + 1] * (1.0f - std::clamp(Rows[Row + 1], 0.0f, 1.0f)) : Kept[Row + 1];
	}
	for (u32 Index = 0; Index < Data.Shakes.size(); ++Index)
	{
		const ShakeClip& Clip = Data.Shakes[Index];
		const Shake& Layer = Clip.Setup;
		const float Inside = Local - Clip.Start;
		const float Influence = ShakeWeight(Data, Index, Local) * Share(Index) * Kept[Clip.Row % MaxShakeRows];
		if (!(Influence > 0.0f))
		{
			continue;
		}
		const ShakePreset& Preset = ShakePresets[std::min<u32>(Layer.Preset, ShakePresetCount - 1)];
		const float Scale = Curves::LaneScalar(Clip.Lanes, Param::ShakePositionScale, Clip.Row, Inside, Layer.PositionScale);
		const double Elapsed = Curves::LaneArea(Clip.Lanes, Index < Areas.size() ? &Areas[Index] : nullptr, Param::ShakeSpeed, Clip.Row, Inside, Layer.Speed);
		const double Offset = Layer.Offset;
		const double Frame = Layer.IsStretched ? Elapsed / Clip.Length * Preset.Count + Offset * Preset.FramesPerSecond : (Elapsed + Offset) * Preset.FramesPerSecond;
		float Channels[6];
		for (u32 Channel = 0; Channel < 6; ++Channel)
		{
			Channels[Channel] = SampleShake(Preset, Frame, Channel) * Influence;
		}
		Fvector Moved, World;
		Moved.set(Channels[0], Channels[1], -Channels[2]).mul(Scale);
		Turn.transform_dir(World, Moved);
		Shift.add(World);
		Fmatrix X, Y, Z, XY, Rotation, Turned;
		X.rotateX(-Channels[3]);
		Y.rotateY(-Channels[4]);
		Z.rotateZ(Channels[5]);
		XY.mul(Y, X);
		Rotation.mul(Z, XY);
		Turned.mul(Turn, Rotation);
		Turn = Turned;
	}
}

float BlockStart(const CameraData& Data, u32 Block)
{
	return Block < Data.Blocks.size() ? Data.Blocks[Block].Start : Data.Blocks.empty() ? 0.0f : Data.Blocks.back().Start + Data.Blocks.back().Length;
}

s32 BlockAt(const CameraData& Data, float Local)
{
	const auto Next = std::upper_bound(Data.Blocks.begin(), Data.Blocks.end(), Local, [](float Moment, const CameraBlock& Item) { return Moment < Item.Start; });
	const s32 Last = static_cast<s32>(Next - Data.Blocks.begin()) - 1;
	return Last >= 0 && Local < Data.Blocks[Last].Start + Data.Blocks[Last].Length ? Last : -1;
}

s32 ShakeOwner(const CameraData& Data, const ShakeClip& Clip)
{
	return BlockAt(Data, Clip.Start + ShakeAnchor);
}

bool IsTrackValid(const xr_vector<ClipSpan>& Spans)
{
	for (u32 Index = 0; Index < Spans.size(); ++Index)
	{
		const ClipSpan& Item = Spans[Index];
		const float End = Item.Start + Item.Length;
		const bool IsBad = !(Item.Start >= 0.0f && Item.Length >= MinLength && End <= MaxTime + EPS_L);
		const bool IsCrowded = Index + 1 < Spans.size() && !(Item.Start < Spans[Index + 1].Start && End < Spans[Index + 1].Start + Spans[Index + 1].Length);
		if (IsBad || IsCrowded || (Index + 2 < Spans.size() && End > Spans[Index + 2].Start))
		{
			return false;
		}
	}
	return true;
}

float PathTime(const CameraBlock& Block, const BlockCache& Cache, float Local)
{
	return static_cast<float>(Curves::LaneArea(Block.Lanes, &Cache.TimeScaleArea, Param::TimeScale, 0, Local, Block.TimeScale));
}

float LocalFromPath(const CameraBlock& Block, const BlockCache& Cache, float Path)
{
	return Curves::LaneInverseArea(Block.Lanes, &Cache.TimeScaleArea, Param::TimeScale, 0, Block.TimeScale, Path, MaxTime);
}

float PathTime(const CameraBlock& Block, float Local)
{
	return static_cast<float>(Curves::LaneArea(Block.Lanes, nullptr, Param::TimeScale, 0, Local, Block.TimeScale));
}

float LocalFromPath(const CameraBlock& Block, float Path)
{
	return Curves::LaneInverseArea(Block.Lanes, nullptr, Param::TimeScale, 0, Block.TimeScale, Path, MaxTime);
}

static float NearAngle(float Value, float Reference)
{
	return Value + 360.0f * std::round((Reference - Value) / 360.0f);
}

template <typename TVisit>
static void ForPathCurves(CameraBlock& Block, const TVisit& Visit)
{
	for (Lane& Track : Block.Lanes)
	{
		for (u32 Axis = 0; (Track.Id == Param::Position || Track.Id == Param::Rotation) && Track.Layer == 0 && Axis < 3; ++Axis)
		{
			Visit(Track.Curves[Axis]);
		}
	}
}

void PathPose(const CameraBlock& Block, float Local, Fvector& Position, Fmatrix& Orientation)
{
	const Fvector Turn = Curves::LaneVector(Block.Lanes, Param::Rotation, Local, Block.Rotation);
	Position = Curves::LaneVector(Block.Lanes, Param::Position, Local, Block.Position, 0, Block.Tension);
	Orientation.setHPB(deg2rad(Turn.x), deg2rad(Turn.y), deg2rad(Turn.z));
	const auto Next = std::upper_bound(Block.Keys.begin(), Block.Keys.end(), Local, [](float Moment, const PoseKey& Item) { return Moment < Item.Time; });
	if (Block.IsShortTurn && Next != Block.Keys.begin() && Next != Block.Keys.end())
	{
		const PoseKey& A = *(Next - 1);
		const PoseKey& B = *Next;
		Fmatrix Early, Late;
		Fquaternion From, To, Mixed;
		Early.setHPB(deg2rad(A.Rotation.x), deg2rad(A.Rotation.y), deg2rad(A.Rotation.z));
		Late.setHPB(deg2rad(B.Rotation.x), deg2rad(B.Rotation.y), deg2rad(B.Rotation.z));
		From.set(Early);
		To.set(Late);
		Mixed.slerp(From, To, Curves::Ease(A.Mode == Interp::Bezier ? Interp::Linear : A.Mode, std::clamp((Local - A.Time) / std::max(B.Time - A.Time, 1e-4f), 0.0f, 1.0f)));
		Orientation.rotation(Mixed.normalize());
	}
	Orientation.c.set(0.0f, 0.0f, 0.0f);
}

void Rebuild(CameraBlock& Block)
{
	Block.Keys.clear();
	ForPathCurves(Block, [&Block](Curve& Part)
	{
		for (u32 At = 0; At < Part.Keys.size(); ++At)
		{
			PoseKey& Entry = Block.Keys.emplace_back();
			Entry.Time = Part.Keys[At].Time;
			Entry.Mode = Part.Keys[At].Mode;
		}
	});
	std::stable_sort(Block.Keys.begin(), Block.Keys.end(), [](const PoseKey& A, const PoseKey& B) { return A.Time < B.Time; });
	Block.Keys.erase(std::unique(Block.Keys.begin(), Block.Keys.end(), [](const PoseKey& A, const PoseKey& B) { return B.Time - A.Time <= 1e-4f; }), Block.Keys.end());
	if (Block.Keys.size() > MaxPoseKeys)
	{
		Block.Keys.resize(MaxPoseKeys);
	}
	for (PoseKey& Entry : Block.Keys)
	{
		Entry.Position = Curves::LaneVector(Block.Lanes, Param::Position, Entry.Time, Block.Position, 0, Block.Tension);
		Entry.Rotation = Curves::LaneVector(Block.Lanes, Param::Rotation, Entry.Time, Block.Rotation);
	}
}

void SetPoseAt(CameraBlock& Block, float Time, const Fvector& Position, const Fvector& Rotation, Interp Mode, float HalfFrame)
{
	for (u32 Kind = 0; Kind < 2; ++Kind)
	{
		Lane& Track = Curves::EnsureLane(Block.Lanes, Kind ? Param::Rotation : Param::Position, 0);
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			Curve& Part = Track.Curves[Axis];
			Curves::SetKey(Part, Time, Kind ? NearAngle(Rotation[Axis], Curves::Value(Part, Time, Rotation[Axis])) : Position[Axis], HalfFrame);
			const u32 At = Curves::KeyAt(Part, Time, HalfFrame);
			if (At != u32(-1))
			{
				Part.Keys[At].Mode = Mode;
			}
		}
	}
	Rebuild(Block);
}

void RemovePoseAt(CameraBlock& Block, float Time, float HalfFrame)
{
	ForPathCurves(Block, [Time, HalfFrame](Curve& Part)
	{
		const u32 At = Curves::KeyAt(Part, Time, HalfFrame);
		if (At != u32(-1))
		{
			Part.Keys.erase(Part.Keys.begin() + At);
		}
	});
	Rebuild(Block);
}

void MovePoseAt(CameraBlock& Block, float From, float To, float HalfFrame)
{
	ForPathCurves(Block, [From, To, HalfFrame](Curve& Part)
	{
		const u32 At = Curves::KeyAt(Part, From, HalfFrame);
		if (At != u32(-1))
		{
			Key Item = Part.Keys[At];
			Part.Keys.erase(Part.Keys.begin() + At);
			Item.Time = To;
			Curves::PlaceKey(Part, Item, HalfFrame);
		}
	});
	Rebuild(Block);
}

void ModeAt(CameraBlock& Block, float Time, Interp Mode, float HalfFrame)
{
	ForPathCurves(Block, [Time, Mode, HalfFrame](Curve& Part)
	{
		const u32 At = Curves::KeyAt(Part, Time, HalfFrame);
		if (At != u32(-1))
		{
			Curves::SetMode(Part, At, Mode);
		}
	});
	Rebuild(Block);
}

bool KeysToLanes(CameraBlock& Block)
{
	bool IsApprox = false;
	if (!Block.Keys.empty())
	{
		Spline Path;
		Path.Build(Block);
		xr_vector<float> Times;
		Times.reserve(Block.Keys.size());
		for (const PoseKey& Item : Block.Keys)
		{
			Times.push_back(Item.Time);
		}
		Lane& Place = Curves::EnsureLane(Block.Lanes, Param::Position, 0);
		Lane& Turn = Curves::EnsureLane(Block.Lanes, Param::Rotation, 0);
		Fvector Previous = Block.Keys[0].Rotation;
		const auto Angles = [&Path](float Moment, const Fvector& Near)
		{
			Fvector Position, Raw;
			Fmatrix Orientation = Fidentity;
			Path.Evaluate(Moment, Position, Orientation);
			Orientation.getHPB(Raw);
			return Fvector().set(NearAngle(rad2deg(Raw.x), Near.x), NearAngle(rad2deg(Raw.y), Near.y), NearAngle(rad2deg(Raw.z), Near.z));
		};
		for (u32 Index = 0; Index < Block.Keys.size(); ++Index)
		{
			const PoseKey& Item = Block.Keys[Index];
			const Fvector Rotation = Fvector().set(NearAngle(Item.Rotation.x, Previous.x), NearAngle(Item.Rotation.y, Previous.y), NearAngle(Item.Rotation.z, Previous.z));
			Previous = Rotation;
			Fvector In, Out;
			const bool HasHandles = Path.Handles(Index, In, Out);
			const float Before = Index ? Times[Index] - Times[Index - 1] : 0.0f, After = Index + 1 < Times.size() ? Times[Index + 1] - Times[Index] : 0.0f;
			const float Step = 1.0f / 240.0f;
			const Fvector Slope = HasHandles ? Fvector().sub(Angles(Item.Time + Step, Rotation), Angles(Item.Time - Step, Rotation)).div(2.0f * Step) : Fvector().set(0.0f, 0.0f, 0.0f);
			IsApprox = IsApprox || (Item.Mode == Interp::Bezier && HasHandles) || Item.HasTangent();
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Key Spot { Times[Index], Item.Position[Axis], Item.Mode };
				Spot.InTime = -Before / 3.0f;
				Spot.OutTime = After / 3.0f;
				Spot.InValue = HasHandles ? In[Axis] : 0.0f;
				Spot.OutValue = HasHandles ? Out[Axis] : 0.0f;
				Curves::PlaceKey(Place.Curves[Axis], Spot, 1e-4f);
				Spot.Value = Rotation[Axis];
				Spot.InValue = -Slope[Axis] * Before / 3.0f;
				Spot.OutValue = Slope[Axis] * After / 3.0f;
				Curves::PlaceKey(Turn.Curves[Axis], Spot, 1e-4f);
			}
		}
	}
	Block.Position = Block.Keys.empty() ? Block.Position : Block.Keys[0].Position;
	Block.Rotation = Block.Keys.empty() ? Block.Rotation : Block.Keys[0].Rotation;
	Rebuild(Block);
	return IsApprox;
}

void EvaluateBlock(const CameraBlock& Block, const BlockCache& Cache, float Local, CameraPose& Pose)
{
	PathPose(Block, PathTime(Block, Cache, Local), Pose.Position, Pose.Orientation);
	Pose.Orientation.c = Pose.Position;
	Spaces::Compose(Cache.Frames, Pose.Orientation);
	Pose.Position = Pose.Orientation.c;
	Pose.Orientation.c.set(0.0f, 0.0f, 0.0f);
	const Lane* FovLane = Curves::FindLane(Block.Lanes, Param::Fov, 0);
	Pose.Fov = FovLane && (Block.Fov > 0.0f || !FovLane->Curves[0].Keys.empty()) ? Curves::Result(FovLane->Curves[0], Local, Block.Fov, Info(Param::Fov)) : Block.Fov;
	const float Focus = FocusDepth(Block, Cache, Local, Pose.Position, Pose.Orientation.k);
	Pose.HasDof = Block.Dof != DofMode::Off;
	Pose.IsPhysical = Pose.HasDof && Block.IsPhysical;
	Pose.Lens.set(Curves::LaneScalar(Block.Lanes, Param::DofAperture, 0, Local, Block.Aperture), Curves::LaneScalar(Block.Lanes, Param::DofLength, 0, Local, Block.FocalLength), Focus, Block.IsAutofocus && Block.Dof == DofMode::Manual ? 1.0f : 0.0f);
	Pose.Shape.set(Curves::LaneScalar(Block.Lanes, Param::DofPoint, 0, Local, Block.Point[0]), Curves::LaneScalar(Block.Lanes, Param::DofPoint, 1, Local, Block.Point[1]), Curves::LaneScalar(Block.Lanes, Param::DofLimit, 0, Local, Block.Limit), Curves::LaneScalar(Block.Lanes, Param::DofSqueeze, 0, Local, Block.Squeeze));
	Pose.Timing.set(Curves::LaneScalar(Block.Lanes, Param::DofSpeed, 0, Local, Block.FocusTime), Curves::LaneScalar(Block.Lanes, Param::DofTransition, 0, Local, Block.TransitionTime), Curves::LaneScalar(Block.Lanes, Param::DofSwirl, 0, Local, Block.Swirl), 0.0f);
	Pose.Dof.set(Focus - Curves::LaneScalar(Block.Lanes, Param::DofNear, 0, Local, Block.NearRange), Focus, Focus + Curves::LaneScalar(Block.Lanes, Param::DofFar, 0, Local, Block.FarRange), Curves::LaneScalar(Block.Lanes, Param::DofBlur, 0, Local, Block.Blur));
}

float FocusDepth(const CameraBlock& Block, const BlockCache& Cache, float Local, const Fvector& Eye, const Fvector& Forward)
{
	const bool IsTracked = Block.Dof == DofMode::Object && Cache.HasFocus;
	const float Depth = IsTracked ? Fvector().sub(Cache.FocusPoint, Eye).dotproduct(Forward) + Curves::LaneScalar(Block.Lanes, Param::DofOffset, 0, Local, Block.FocusOffset) : Curves::LaneScalar(Block.Lanes, Param::DofFocus, 0, Local, Block.Focus);
	return std::max(Depth, Info(Param::DofFocus).Min);
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

static bool SampleRaw(const Object& Camera, const xr_vector<BlockCache>& Cache, const xr_vector<Curves::AreaTable>& ShakeAreas, const xr_vector<s16>& ShakeOwners, float Time, CameraSample& Out)
{
	Out = CameraSample {};
	const CameraData& Data = Camera.Camera;
	const float Local = Time - Camera.Span.Start;
	const s32 Late = BlockAt(Data, Local);
	const s32 Early = Late > 0 && Local < Data.Blocks[Late - 1].Start + Data.Blocks[Late - 1].Length ? Late - 1 : -1;
	const auto IsShot = [&](s32 At) { return At >= 0 && static_cast<u32>(At) < Cache.size() && !Data.Blocks[At].Keys.empty() && !(Cache[At].Frames.Problems & MissingParent); };
	const s32 Sides[2] = { IsShot(Early) && IsShot(Late) ? Early : -2, IsShot(Late) ? Late : IsShot(Early) ? Early : -1 };
	if (Camera.IsMuted || Sides[1] < 0)
	{
		return false;
	}
	Out.Block = Sides[1];
	const CameraBlock& Shot = Data.Blocks[Out.Block];
	EvaluateBlock(Shot, Cache[Out.Block], Local - Shot.Start, Out.B);
	Out.A = Out.B;
	if (Sides[0] >= 0)
	{
		const CameraBlock& Previous = Data.Blocks[Early];
		const float Span = Previous.Start + Previous.Length - Shot.Start;
		EvaluateBlock(Previous, Cache[Early], Local - Previous.Start, Out.A);
		Out.Weight = Curves::Ease(Shot.BlendEase, Span > 0.0f ? std::clamp((Local - Shot.Start) / Span, 0.0f, 1.0f) : 1.0f);
	}
	Out.IsActive = true;
	ApplyShakes(Data, ShakeAreas, ShakeOwners, Sides, Out.Weight, Local, Out.Shift, Out.Turn);
	return true;
}

bool Sample(const Object& Camera, const xr_vector<BlockCache>& Cache, const xr_vector<Curves::AreaTable>& ShakeAreas, const xr_vector<s16>& ShakeOwners, float Time, CameraSample& Out)
{
	if (!SampleRaw(Camera, Cache, ShakeAreas, ShakeOwners, Time, Out))
	{
		return false;
	}
	const CameraBlock& Shot = Camera.Camera.Blocks[Out.Block];
	const float Begin = Camera.Span.Start + Shot.Start, Sigma = Curves::LaneScalar(Shot.Lanes, Param::CameraSmooth, 0, Time - Begin, Shot.Smooth);
	if (!(Sigma > 0.0f))
	{
		return true;
	}
	const CameraPose Centre = Mix(Out);
	Fquaternion Facing, Mean;
	Facing.set(Centre.Orientation);
	Mean.set(0.0f, 0.0f, 0.0f, 0.0f);
	Fvector Sum = Fvector().set(0.0f, 0.0f, 0.0f);
	float Total = 0.0f;
	CameraSample Tap;
	for (s32 Step = -4; Step <= 4; ++Step)
	{
		const float Weight = std::exp(-0.125f * static_cast<float>(Step * Step));
		const float At = std::clamp(Time + 0.5f * Sigma * static_cast<float>(Step), Begin, Begin + std::max(Shot.Length - 1e-4f, 0.0f));
		const CameraPose Pose = SampleRaw(Camera, Cache, ShakeAreas, ShakeOwners, At, Tap) ? Mix(Tap) : Centre;
		Fquaternion Part;
		Part.set(Pose.Orientation);
		const float Side = Part.x * Facing.x + Part.y * Facing.y + Part.z * Facing.z + Part.w * Facing.w < 0.0f ? -Weight : Weight;
		Mean.set(Mean.w + Part.w * Side, Mean.x + Part.x * Side, Mean.y + Part.y * Side, Mean.z + Part.z * Side);
		Sum.mad(Pose.Position, Weight);
		Total += Weight;
	}
	Out.A.Orientation.rotation(Mean.normalize());
	Out.A.Position.div(Sum, Total);
	Out.B.Orientation = Out.A.Orientation;
	Out.B.Position = Out.A.Position;
	Out.Shift.set(0.0f, 0.0f, 0.0f);
	Out.Turn = Fidentity;
	return true;
}

static bool IsRowFree(const CameraData& Data, const auto& IsCounted, u8 Row, float Start, float Length, bool IsStrict)
{
	const auto SpanOf = [&](u32 Index) { const ShakeClip& Clip = Data.Shakes[Index]; return RowSpan { IsCounted(Index), Clip.Row, Clip.Start, Clip.Start + Clip.Length }; };
	return IsSpanFree(static_cast<u32>(Data.Shakes.size()), SpanOf, Row, Start, Start + Length, IsStrict);
}

bool IsShakeFree(const CameraData& Data, u32 Skip, u8 Row, float Start, float Length, u32 Until, bool IsStrict)
{
	return IsRowFree(Data, [Skip, Until](u32 Index) { return Index != Skip && Index < Until; }, Row, Start, Length, IsStrict);
}

u32 SettleShakes(CameraData& Data, const xr_vector<u8>& Moved, bool IsStrict)
{
	const auto MovedAt = [&Moved](u32 Index) { return Index < Moved.size() && Moved[Index] ? 1u : 0u; };
	u32 Crowded = 0;
	for (u32 Pass = 0; Pass < 2; ++Pass)
	{
		for (u32 Index = 0; Index < Data.Shakes.size(); ++Index)
		{
			ShakeClip& Clip = Data.Shakes[Index];
			const auto IsSettled = [&](u32 Other) { return MovedAt(Other) < Pass || (MovedAt(Other) == Pass && Other < Index); };
			const auto IsFree = [&](u8 Row) { return IsRowFree(Data, IsSettled, Row, Clip.Start, Clip.Length, IsStrict && Pass); };


			u32 Step = 0;
			while (MovedAt(Index) == Pass && Step < MaxShakeRows && !IsFree(static_cast<u8>((Clip.Row + Step) % MaxShakeRows)))
			{
				++Step;
			}
			Crowded += Step == MaxShakeRows;
			SetShakeRow(Clip, static_cast<u8>((Clip.Row + (Step == MaxShakeRows ? 0 : Step)) % MaxShakeRows));
		}
	}
	return Crowded;
}
}

namespace Cinema::Spaces
{
static bool LookBasis(const LinkFrames& Frames, const Fvector& From, Fmatrix& Basis)
{
	Fvector Direction = Fvector().sub(Frames.Look, From);
	Direction.y = Frames.IsYawOnly ? 0.0f : Direction.y;
	if (!Frames.HasLook || !(Direction.magnitude() >= 1e-4f))
	{
		return false;
	}
	Basis.identity();
	Basis.k.normalize_safe(Direction);
	Basis.i.crossproduct(Fvector().set(0.0f, 1.0f, 0.0f), Basis.k);
	if (Basis.i.square_magnitude() < 1e-6f)
	{
		Fvector::generate_orthonormal_basis(Basis.k, Basis.j, Basis.i);
	}
	Basis.i.normalize_safe();
	Basis.j.crossproduct(Basis.k, Basis.i);
	return true;
}

static void Unrotate(const Fmatrix& Frame, Fvector& Value)
{
	Value.set(Value.dotproduct(Frame.i), Value.dotproduct(Frame.j), Value.dotproduct(Frame.k));
}

void Orthonormalize(Fmatrix& Frame)
{
	Frame.k.normalize_safe();
	Frame.i.crossproduct(Frame.j, Frame.k).normalize_safe();
	Frame.j.crossproduct(Frame.k, Frame.i);
}

void Compose(const LinkFrames& Frames, Fmatrix& Pose)
{
	Fmatrix World = Pose, Basis, Turn;
	if (Frames.HasParent)
	{
		World.mul_43(Frames.Parent, Pose);
	}
	if (Frames.LookWeight > 0.0f && LookBasis(Frames, World.c, Basis))
	{
		Turn.mul_43(Basis, Pose);
		if (Frames.LookWeight < 1.0f)
		{
			const float Sizes[3] = { Turn.i.magnitude(), Turn.j.magnitude(), Turn.k.magnitude() };
			Fmatrix Own = World, Aimed = Turn, Mix;
			for (Fmatrix* Each : { &Own, &Aimed })
			{
				Each->i.normalize_safe();
				Each->j.normalize_safe();
				Each->k.normalize_safe();
			}
			Fquaternion From, To, Mixed;
			From.set(Own);
			To.set(Aimed);
			Mixed.slerp(From, To, Frames.LookWeight);
			Mix.rotation(Mixed.normalize());
			Turn.i.mul(Mix.i, Sizes[0]);
			Turn.j.mul(Mix.j, Sizes[1]);
			Turn.k.mul(Mix.k, Sizes[2]);
		}
		World.i = Turn.i;
		World.j = Turn.j;
		World.k = Turn.k;
	}
	Pose = World;
}

void Decompose(const LinkFrames& Frames, Fmatrix& Pose)
{
	Fmatrix Basis;
	const bool HasBasis = Frames.LookWeight >= 0.5f && LookBasis(Frames, Pose.c, Basis);
	if (Frames.HasParent)
	{
		Pose.c.sub(Frames.Parent.c);
		Unrotate(Frames.Parent, Pose.c);
	}
	for (Fvector* Axis : { &Pose.i, &Pose.j, &Pose.k })
	{
		Unrotate(HasBasis ? Basis : Frames.HasParent ? Frames.Parent : Fidentity, *Axis);
	}
}
}
