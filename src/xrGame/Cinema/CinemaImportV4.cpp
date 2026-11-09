#include "StdAfx.h"
#include "CinemaSceneIo.h"
#include "CinemaCurve.h"
#include "CinemaShake.h"
#include "CinemaCamera.h"

namespace Cinema::V4
{
struct OldKey
{
	float Time = 0.0f;
	float Value = 0.0f;
	float InSlope = 0.0f;
	float OutSlope = 0.0f;
	bool IsSmooth = false;
};

using OldChannel = xr_vector<OldKey>;

struct OldSegment
{
	float Start = 0.0f;
	float End = 0.0f;
};

struct OldTrim
{
	shared_str Source;
	shared_str Name;
	xr_vector<OldSegment> Segments;
	float Speed = 1.0f;
	float Blend = 0.0f;
};

struct OldEntry
{
	shared_str Source;
	shared_str Name;
};

struct OldClip
{
	shared_str Source;
	shared_str Name;
	xr_string Label;
	u32 Color = 0;
	s32 Track = 0;
	bool IsMuted = false;
	float Start = 0.0f;
	float Duration = 0.0f;
	float SourceIn = 0.0f;
	float SourceOut = 0.0f;
	float Speed = 1.0f;
	float Ramp[2] = { 1.0f, 1.0f };
	Fvector Tint[2] = { Fvector().set(1.0f, 1.0f, 1.0f), Fvector().set(1.0f, 1.0f, 1.0f) };
};

struct OldEffect
{
	OldChannel Channels[15];
	Generator Waves[15];
};

struct ChannelTarget
{
	Param Id;
	u32 Component;
	float* Base;
};

struct FreeLight
{
	u32 Index = 0;
	float Period = 10.0f;
	bool IsLoop = true;
};

struct Shot
{
	float Start = 0.0f;
	s32 Track = 0;
	u32 Index = 0;
	bool IsMuted = false;
	bool IsPreview = false;
	bool IsEnabled = true;
	CameraBlock Block;
	xr_vector<ShakeClip> Shakes;
};

struct Tally
{
	u32 Shots = 0;
	u32 Poses = 0;
	u32 Transforms = 0;
	u32 Entries = 0;
	u32 Keys = 0;
	u32 Clips = 0;
	u32 Muted = 0;
	u32 Hidden = 0;
	u32 Overlaps = 0;
	u32 Baked = 0;
	u32 Merged = 0;
	u32 Approximated = 0;
	u32 Unsafe = 0;
	u32 Overflow = 0;
	u32 Effects = 0;
	u32 Rotations = 0;
	u32 Ramps = 0;
	u32 Unrolled = 0;
	u32 EngineFlags = 0;
	u32 Owned = 0;
	u32 Bands = 0;
};

constexpr float EffectBounds[4][2] = { { -3600.0f, 3600.0f }, { 0.0f, 10.0f }, { 0.1f, 5.0f }, { 0.0f, 4.0f } };
constexpr const char* LightFields[7] = { "intensity", "range", "cone", "virtual_size", "volume_intensity", "volume_distance", "volume_quality" };
constexpr float LightLimits[7][2] = { { 0.0f, 100.0f }, { 0.1f, 1000.0f }, { 1.0f, 120.0f }, { 0.0f, 100.0f }, { 0.0f, 100.0f }, { 0.0f, 1.0f }, { 0.01f, 1.0f } };
constexpr float CameraBounds[9][2] = { { -3600.0f, 3600.0f }, { -3600.0f, 3600.0f }, { -3600.0f, 3600.0f }, { -3600.0f, 3600.0f }, { -3600.0f, 3600.0f }, { -3600.0f, 3600.0f }, { 1.0f, 179.0f }, { 0.0f, 1.0f }, { 0.0f, 1.0f } };
constexpr float ShakeBounds[9][2] = { { 0.0f, 10.0f }, { 0.0f, 10.0f }, { 0.0f, 10.0f }, { -3600.0f, 3600.0f }, { 0.0f, 1.0f }, { 0.0f, static_cast<float>(ShakePresetCount - 1) }, { 0.0f, 1.0f }, { 0.0f, 3600.0f }, { 0.0f, 3600.0f } };

float Number(const CInifile& Ini, const char* Section, const char* Line, float Default, Io::Warnings& Notes)
{
	float Value = Default;
	Io::ReadFloats(Ini, Section, Line, &Value, 1, Notes);
	return Value;
}

Fvector ReadVector(const CInifile& Ini, const char* Section, const char* Line, const Fvector& Default, Io::Warnings& Notes)
{
	float Values[3] = { Default.x, Default.y, Default.z };
	Io::ReadFloats(Ini, Section, Line, Values, 3, Notes);
	return Fvector().set(Values[0], Values[1], Values[2]);
}

shared_str SafeText(const CInifile& Ini, const char* Section, const char* Line, Tally& Counts)
{
	const char* Text = Ini.read_if_exists<str_c>(Section, Line, nullptr);
	const bool IsSafe = !Text || Io::IniSafe(Text);
	Counts.Unsafe += !IsSafe;
	return IsSafe ? shared_str(Text) : shared_str();
}

xr_string FitName(const char* Text)
{
	xr_string Name = Text;
	size_t Size = std::min<size_t>(Name.size(), MaxNameBytes);
	while (Size < Name.size() && (static_cast<u8>(Name[Size]) & 0xC0) == 0x80)
	{
		--Size;
	}
	Name.resize(Size);
	return Name;
}

float Step(const OldChannel& Keys, float Time, float Fallback)
{
	if (Keys.empty())
	{
		return Fallback;
	}
	const auto Next = std::upper_bound(Keys.begin(), Keys.end(), Time, [](float Moment, const OldKey& Point) { return Moment < Point.Time; });
	return (Next == Keys.begin() ? Keys.front() : *(Next - 1)).Value;
}

float Sample(const OldChannel& Keys, float Time, float Fallback)
{
	if (Keys.empty())
	{
		return Fallback;
	}
	if (Time <= Keys.front().Time)
	{
		return Keys.front().Value;
	}
	for (size_t Index = 1; Index < Keys.size(); ++Index)
	{
		const OldKey& A = Keys[Index - 1];
		const OldKey& B = Keys[Index];
		if (Time > B.Time)
		{
			continue;
		}
		const float Span = B.Time - A.Time;
		const float U = (Time - A.Time) / Span;
		if (!A.IsSmooth)
		{
			return A.Value + (B.Value - A.Value) * U;
		}
		const float U2 = U * U;
		const float U3 = U2 * U;
		return (2.0f * U3 - 3.0f * U2 + 1.0f) * A.Value + (U3 - 2.0f * U2 + U) * Span * A.OutSlope + (3.0f * U2 - 2.0f * U3) * B.Value + (U3 - U2) * Span * B.InSlope;
	}
	return Keys.back().Value;
}

bool Varies(const OldChannel& Keys)
{
	return std::any_of(Keys.begin(), Keys.end(), [&](const OldKey& Point) { return Point.Value != Keys.front().Value; });
}

void SpeedSlopes(const OldKey& A, const OldKey& B, float& Out, float& In)
{
	const double Delta = (static_cast<double>(B.Value) - A.Value) / (static_cast<double>(B.Time) - A.Time);
	if (std::fabs(Delta) < 1e-8)
	{
		Out = 0.0f;
		In = 0.0f;
		return;
	}
	Out = static_cast<float>(std::clamp(A.OutSlope / Delta, 0.0, 3.0) * Delta);
	In = static_cast<float>(std::clamp(B.InSlope / Delta, 0.0, 3.0) * Delta);
}

Curve Convert(const OldChannel& Keys, float Scale, Param Id, bool IsSpeed)
{
	const ParamInfo& Meta = Info(Id);
	Curve Converted;
	for (const OldKey& Point : Keys)
	{
		Converted.Keys.push_back({ Point.Time, std::clamp(Point.Value * Scale, Meta.Min, Meta.Max), Interp::Linear });
	}
	for (size_t Index = 0; Index + 1 < Keys.size(); ++Index)
	{
		const OldKey& A = Keys[Index];
		const OldKey& B = Keys[Index + 1];
		float Out = A.OutSlope;
		float In = B.InSlope;
		if (!A.IsSmooth)
		{
			continue;
		}
		if (IsSpeed)
		{
			SpeedSlopes(A, B, Out, In);
		}
		const float Third = (B.Time - A.Time) / 3.0f;
		Converted.Keys[Index].Mode = Interp::Bezier;
		Converted.Keys[Index].OutTime = Third;
		Converted.Keys[Index].OutValue = std::clamp(Out * Scale * Third, -100000.0f, 100000.0f);
		Converted.Keys[Index + 1].InTime = -Third;
		Converted.Keys[Index + 1].InValue = std::clamp(-In * Scale * Third, -100000.0f, 100000.0f);
	}
	return Converted;
}

void ReadChannel(const CInifile& Ini, const char* Section, const char* Prefix, u32 Channel, const float (&Bounds)[2], bool IsStep, bool IsEffect, float Default, OldChannel& Out, Tally& Counts, Io::Warnings& Notes)
{
	string64 Line;
	xr_sprintf(Line, "%scurve_%u_count", Prefix, Channel);
	const u32 Count = Io::ReadU32(Ini, Section, Line, 128, Notes);
	const auto Name = [&](u32 Index, const char* Suffix)
	{
		xr_sprintf(Line, IsEffect ? "%scurve_%u_%u_%s" : "%scurve_%u_%03u_%s", Prefix, Channel, Index, Suffix);
		return static_cast<const char*>(Line);
	};
	for (u32 Index = 0; Index < Count; ++Index)
	{
		OldKey Point;
		Point.Time = Number(Ini, Section, Name(Index, "time"), -1.0f, Notes);
		Point.Value = Number(Ini, Section, Name(Index, "value"), Default, Notes);
		Point.InSlope = Number(Ini, Section, Name(Index, IsEffect ? "in_slope" : "in"), 0.0f, Notes);
		Point.OutSlope = Number(Ini, Section, Name(Index, IsEffect ? "out_slope" : "out"), 0.0f, Notes);
		Point.IsSmooth = Io::ReadBool(Ini, Section, Name(Index, "smooth"), false, Notes);
		if (!(Point.Time >= 0.0f && Point.Time <= (IsEffect ? 600.0f : 3600.0f)) || !(Point.Value >= Bounds[0] && Point.Value <= Bounds[1]) || !std::isfinite(Point.InSlope) || !std::isfinite(Point.OutSlope))
		{
			++Counts.Keys;
			continue;
		}
		Point.Value = IsStep ? std::round(Point.Value) : Point.Value;
		Point.InSlope = std::clamp(Point.InSlope, -3600.0f, 3600.0f);
		Point.OutSlope = std::clamp(Point.OutSlope, -3600.0f, 3600.0f);
		Out.push_back(Point);
	}
	std::stable_sort(Out.begin(), Out.end(), [](const OldKey& A, const OldKey& B) { return A.Time < B.Time; });
	const auto Last = std::unique(Out.begin(), Out.end(), [](const OldKey& A, const OldKey& B) { return A.Time == B.Time; });
	Counts.Keys += static_cast<u32>(Out.end() - Last);
	Out.erase(Last, Out.end());
}

void SplitAt(OldChannel& Keys, float At)
{
	const auto Next = std::upper_bound(Keys.begin(), Keys.end(), At, [](float Moment, const OldKey& Point) { return Moment < Point.Time; });
	if (Next != Keys.begin() && (Next - 1)->Time == At)
	{
		return;
	}
	OldKey Point { At, Sample(Keys, At, 0.0f) };
	if (Next != Keys.begin() && Next != Keys.end())
	{
		const OldKey& A = *(Next - 1);
		const OldKey& B = *Next;
		const float Span = B.Time - A.Time;
		const float U = (At - A.Time) / Span;
		Point.InSlope = (6.0f * U * U - 6.0f * U) * (A.Value - B.Value) / Span + (3.0f * U * U - 4.0f * U + 1.0f) * A.OutSlope + (3.0f * U * U - 2.0f * U) * B.InSlope;
		Point.OutSlope = Point.InSlope;
		Point.IsSmooth = A.IsSmooth;
	}
	Keys.insert(Next, Point);
}

void Cut(OldChannel& Keys, float At, bool IsAfter)
{
	if (!Keys.empty() && (IsAfter ? Keys.front().Time < At : Keys.back().Time > At))
	{
		SplitAt(Keys, At);
		std::erase_if(Keys, [&](const OldKey& Point) { return IsAfter ? Point.Time < At : Point.Time > At; });
	}
	for (OldKey& Point : Keys)
	{
		Point.Time -= IsAfter ? At : 0.0f;
	}
}

void FitWave(Generator& Shape, const ParamInfo& Meta, Tally& Counts)
{
	const Generator Wanted = Shape;
	Shape.Amplitude = std::min(Shape.Amplitude, 100000.0f);
	Shape.Phase = std::clamp(Shape.Phase, -100000.0f, 100000.0f);
	Shape.Min = std::clamp(Shape.Min, -100000.0f, 100000.0f);
	Shape.Max = std::clamp(Shape.Max, -100000.0f, 100000.0f);
	if (Shape.Type == Wave::None || Shape.Min > Meta.Max || Shape.Max < Meta.Min)
	{
		Shape = Generator {};
	}
	Counts.Bands += Wanted.Type != Wave::None && !(Shape == Wanted);
}

void PutChannel(xr_vector<Lane>& Lanes, Param Id, u32 Component, const OldChannel& Keys, Generator Shape, float Scale, float Shift, Tally& Counts)
{
	const ParamInfo& Meta = Info(Id);
	Curve Part = Convert(Keys, Scale, Id, false);
	for (u32 Index = 0; Index < Part.Keys.size(); ++Index)
	{
		const float Wanted = Keys[Index].Value * Scale + Shift;
		Part.Keys[Index].Value = std::clamp(Wanted, Meta.Min, Meta.Max);
		Counts.Bands += Part.Keys[Index].Value != Wanted;
	}
	Shape.Amplitude *= Scale;
	Shape.Min = Shape.Min * Scale + Shift;
	Shape.Max = Shape.Max * Scale + Shift;
	FitWave(Shape, Meta, Counts);
	Part.Gen = Shape;
	if (!Part.Keys.empty() || Part.Gen.Type != Wave::None)
	{
		Curves::EnsureLane(Lanes, Id, 0).Curves[Component] = std::move(Part);
	}
}

void BakeRotation(xr_vector<Lane>& Lanes, const Fvector& Rest, const OldChannel* Keys, const Generator* Shapes, Tally& Counts)
{
	const bool HasMotion = std::any_of(Keys, Keys + 3, [](const OldChannel& Part) { return !Part.empty(); }) || std::any_of(Shapes, Shapes + 3, [](const Generator& Shape) { return Shape.Type != Wave::None; });
	OldChannel Sampled[3];
	const OldChannel* Source = Keys;
	if (HasMotion && std::fabs(Rest.y) + std::fabs(Rest.z) >= 1e-3f)
	{
		xr_vector<float> Times;
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			for (const OldKey& Point : Keys[Axis])
			{
				Times.push_back(Point.Time);
			}
		}
		std::sort(Times.begin(), Times.end());
		Times.erase(std::unique(Times.begin(), Times.end()), Times.end());
		Fmatrix Base;
		Base.setHPB(deg2rad(Rest.x), deg2rad(Rest.y), deg2rad(Rest.z));
		Fvector Previous = Rest;
		for (const float Time : Times)
		{
			Fmatrix Offset;
			Fmatrix Turned;
			Fvector Angles;
			Offset.setHPB(deg2rad(Sample(Keys[0], Time, 0.0f)), deg2rad(Sample(Keys[1], Time, 0.0f)), deg2rad(Sample(Keys[2], Time, 0.0f)));
			Turned.mul(Base, Offset);
			Turned.getHPB(Angles);
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Previous[Axis] = std::clamp(Previous[Axis] + std::remainder(rad2deg(Angles[Axis]) - Previous[Axis], 360.0f), -3600.0f, 3600.0f);
				Sampled[Axis].push_back({ Time, Previous[Axis] - Rest[Axis] });
			}
		}
		Source = Sampled;
		++Counts.Rotations;
	}
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		PutChannel(Lanes, Param::Rotation, Axis, Source[Axis], Shapes[Axis], 1.0f, Rest[Axis], Counts);
	}
}

void BuildLanes(Object& Item, const OldEffect& Source, Tally& Counts)
{
	LightData& Light = Item.Light;
	ParticleData& Effect = Item.Particle;
	const ChannelTarget LightTargets[9] = { { Param::Color, 0, &Light.Color.x }, { Param::Color, 1, &Light.Color.y }, { Param::Color, 2, &Light.Color.z }, { Param::Range, 0, &Light.Range }, { Param::Cone, 0, &Light.Cone }, { Param::VirtualSize, 0, &Light.VirtualSize }, { Param::VolumeQuality, 0, &Light.VolumeQuality }, { Param::VolumeIntensity, 0, &Light.VolumeIntensity }, { Param::VolumeDistance, 0, &Light.VolumeDistance } };
	const ChannelTarget ParticleTargets[2] = { { Param::Size, 0, &Effect.Size }, { Param::Speed, 0, &Effect.Speed } };
	const bool IsLight = Item.Type == ObjectType::Light;
	const Fvector& Position = *PlacementOf(Item, 0);
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		PutChannel(Item.Lanes, Param::Position, Axis, Source.Channels[Axis], Source.Waves[Axis], 1.0f, Position[Axis], Counts);
	}
	BakeRotation(Item.Lanes, *PlacementOf(Item, 1), Source.Channels + 3, Source.Waves + 3, Counts);
	for (u32 Channel = 6; Channel < (IsLight ? 15u : 8u); ++Channel)
	{
		const ChannelTarget& Target = (IsLight ? LightTargets : ParticleTargets)[Channel - 6];
		PutChannel(Item.Lanes, Target.Id, Target.Component, Source.Channels[Channel], Source.Waves[Channel], *Target.Base, 0.0f, Counts);
	}
	Light.VolumeDistance = std::max(Light.VolumeDistance, 0.01f);
}

Generator ReadWave(const CInifile& Ini, const char* Section, u32 Channel, const float (&Bounds)[2], Tally& Counts, Io::Warnings& Notes)
{
	string32 Line;
	const auto Field = [&](const char* Suffix, float Fallback)
	{
		xr_sprintf(Line, "wave_%u_%s", Channel, Suffix);
		return Number(Ini, Section, Line, Fallback, Notes);
	};
	const float Values[6] = { Field("type", 0.0f), Field("amplitude", 0.0f), Field("frequency", 1.0f), Field("phase", 0.0f), Field("min", Bounds[0]), Field("max", Bounds[1]) };
	const bool IsGood = Values[0] >= 0.0f && Values[0] <= 10.0f && Values[0] == std::floor(Values[0]) && Values[1] >= 0.0f && Values[1] <= 3600.0f && Values[2] >= 0.0f && Values[2] <= 100.0f && std::fabs(Values[3]) <= 100.0f && Bounds[0] <= Values[4] && Values[4] <= Values[5] && Values[5] <= Bounds[1];
	Counts.Effects += !IsGood;
	Counts.Bands += IsGood && Values[0] == 0.0f && (Values[4] > Bounds[0] || Values[5] < Bounds[1]);
	return IsGood ? Generator { static_cast<Wave>(static_cast<u8>(Values[0])), Values[1], Values[2], Values[3], Values[4], Values[5] } : Generator { Wave::None, 0.0f, 1.0f, 0.0f, Bounds[0], Bounds[1] };
}

void ReadEffect(const CInifile& Ini, const char* Section, bool IsLight, OldEffect& Out, Tally& Counts, Io::Warnings& Notes)
{
	for (u32 Channel = 0; Channel < (IsLight ? 15u : 8u); ++Channel)
	{
		const float (&Bounds)[2] = EffectBounds[Channel < 6 ? 0 : IsLight ? 1 : Channel - 4];
		Out.Waves[Channel] = ReadWave(Ini, Section, Channel, Bounds, Counts, Notes);
		ReadChannel(Ini, Section, "", Channel, Bounds, false, true, Channel < 6 ? 0.0f : 1.0f, Out.Channels[Channel], Counts, Notes);
	}
}

void Unroll(Curve& Part, float Period, float End, Tally& Counts)
{
	if (Part.Keys.size() < 2)
	{
		return;
	}
	xr_vector<Key> Pass = std::move(Part.Keys);
	const float Edge = Period - 0.001f;
	while (Pass.size() > 1 && Pass[Pass.size() - 2].Time >= Edge)
	{
		Pass.pop_back();
	}
	Pass.back().Time = std::min(Pass.back().Time, Edge);
	Pass.back().Mode = Interp::Hold;
	Part.Keys.clear();
	for (u32 Index = 0; static_cast<float>(Index) * Period < End; ++Index)
	{
		const float Base = static_cast<float>(Index) * Period;
		if (Part.Keys.size() + Pass.size() >= MaxLaneKeys)
		{
			++Counts.Overflow;
			return;
		}
		if (Index && Pass.front().Time > 0.0f)
		{
			Part.Keys.push_back({ Base, Pass.front().Value, Interp::Linear });
		}
		for (Key Point : Pass)
		{
			Point.Time += Base;
			if (Point.Time <= MaxTime)
			{
				Part.Keys.push_back(Point);
			}
		}
	}
}

bool ReadClip(const CInifile& Ini, const char* Section, u32 Index, u32 Track, OldClip& Take, Io::Warnings& Notes)
{
	string64 Line;
	const auto Field = [&](const char* Suffix)
	{
		xr_sprintf(Line, "master_clip_%03u%s", Index, Suffix);
		return static_cast<const char*>(Line);
	};
	Take.Start = Number(Ini, Section, Field("_start"), 0.0f, Notes);
	Take.Duration = Number(Ini, Section, Field("_duration"), 0.0f, Notes);
	Take.Track = static_cast<s32>(Sanitize(Number(Ini, Section, Field("_track"), static_cast<float>(Track % 65535), Notes), 0.0f, 65534.0f, 0.0f));
	Take.IsMuted = Io::ReadBool(Ini, Section, Field("_muted"), false, Notes);
	Io::DecodeName(Ini.read_if_exists<str_c>(Section, Field("_label_hex"), nullptr), Take.Label);
	const char* Color = Ini.read_if_exists<str_c>(Section, Field("_display_color"), nullptr);
	Take.Color = Color ? static_cast<u32>(std::strtoll(Color, nullptr, 10)) : 0;
	return Take.Start >= 0.0f && Take.Start <= 3600.0f && Take.Duration >= 0.01f && Take.Duration <= 3600.0f;
}

Object* AddObject(Scene& Out, ObjectType Type, const xr_string& Label, const xr_string& Fallback, u32 Color, Tally& Counts)
{
	if (Out.Objects.size() >= MaxObjects)
	{
		++Counts.Overflow;
		return nullptr;
	}
	Object& Item = Out.Objects.emplace_back();
	Item.Id = Out.NextId++;
	Item.Type = Type;
	Item.Row = static_cast<u32>(Out.Objects.size() - 1);
	Item.Name = Label.empty() ? Fallback : Label;
	Item.IsNameLocked = !Label.empty();
	Item.Color = Color ? (Color | 0xFF000000u) : NextColor(Out.Header);
	return &Item;
}

void ClampPlacement(Fvector& Position, Fvector& Rotation)
{
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		Position[Axis] = std::clamp(Position[Axis], -100000.0f, 100000.0f);
		Rotation[Axis] = std::fabs(Rotation[Axis]) > 3600.0f ? std::remainder(Rotation[Axis], 360.0f) : Rotation[Axis];
	}
}

bool ParseTrim(const char* Text, OldTrim& Trim)
{
	xr_vector<xr_string_view> Parts;
	for (xr_string_view Rest = Text;;)
	{
		const size_t Bar = Rest.find('|');
		Parts.push_back(Rest.substr(0, Bar));
		if (Bar == xr_string_view::npos)
		{
			break;
		}
		Rest.remove_prefix(Bar + 1);
	}
	if (Parts.size() < 4)
	{
		return false;
	}
	const auto Value = [](xr_string_view Part) { return static_cast<float>(std::strtod(Part.data(), nullptr)); };
	const size_t First = Parts[0] == "v2" ? 1 : 0;
	const xr_string Source(Parts[First].data(), static_cast<u32>(Parts[First].size()));
	const xr_string Name(Parts[First + 1].data(), static_cast<u32>(Parts[First + 1].size()));
	if (First)
	{
		Trim.Speed = Value(Parts[3]);
		const size_t Begin = Parts.size() > 4 && Parts[4].find(',') == xr_string_view::npos ? 5 : 4;
		Trim.Blend = Begin == 5 ? Value(Parts[4]) : 0.0f;
		for (size_t Index = Begin; Index < Parts.size(); ++Index)
		{
			const size_t Comma = Parts[Index].find(',');
			const float Start = Value(Parts[Index]);
			const float End = Comma == xr_string_view::npos ? 0.0f : Value(Parts[Index].substr(Comma + 1));
			if (Comma != xr_string_view::npos && std::isfinite(Start) && End > Start && End <= 3600.0f && Trim.Segments.size() < 128)
			{
				Trim.Segments.push_back({ Start, End });
			}
		}
	}
	else
	{
		const float Start = Value(Parts[2]);
		const float End = Value(Parts[3]);
		Trim.Speed = Parts.size() > 4 ? Value(Parts.back()) : Trim.Speed;
		if (std::isfinite(Start) && End > Start && End <= 3600.0f)
		{
			Trim.Segments.push_back({ Start, End });
		}
	}
	if (Source.empty() || Name.empty() || Source.size() >= sizeof(string_path) || Name.size() >= 256 || Source.find(',') != xr_string::npos || Name.find(',') != xr_string::npos)
	{
		return false;
	}
	Trim.Source = Source.c_str();
	Trim.Name = Name.c_str();
	std::sort(Trim.Segments.begin(), Trim.Segments.end(), [](const OldSegment& A, const OldSegment& B) { return A.Start < B.Start; });
	for (OldSegment& Segment : Trim.Segments)
	{
		Segment.Start = std::max(0.0f, Segment.Start);
	}
	Trim.Speed = std::isfinite(Trim.Speed) ? std::clamp(Trim.Speed, -4.0f, 4.0f) : 1.0f;
	Trim.Blend = std::isfinite(Trim.Blend) ? std::clamp(Trim.Blend, 0.0f, 2.0f) : 0.0f;
	return true;
}

bool IsTrackMuted(const CInifile& Ini, s32 Track, s32 Solo, Io::Warnings& Notes)
{
	string64 Line;
	xr_sprintf(Line, "master_track_%02u_muted", static_cast<u32>(Track));
	if (Io::ReadBool(Ini, "scene", Line, false, Notes))
	{
		return true;
	}
	xr_sprintf(Line, "master_track_%02u_parent", static_cast<u32>(Track));
	const float Parent = Number(Ini, "scene", Line, -1.0f, Notes);
	if (!(Parent >= 0.0f && Parent < 65535.0f) || static_cast<s32>(Parent) == Track || Track == Solo)
	{
		return false;
	}
	xr_sprintf(Line, "master_track_%02u_muted", static_cast<u32>(Parent));
	return Io::ReadBool(Ini, "scene", Line, false, Notes);
}

void Bake(CameraBlock& Block, const OldChannel* Offsets, Tally& Counts)
{
	xr_vector<float> Times;
	for (const PoseKey& Pose : Block.Keys)
	{
		Times.push_back(Pose.Time);
	}
	for (u32 Channel = 0; Channel < 6; ++Channel)
	{
		for (const OldKey& Point : Offsets[Channel])
		{
			Times.push_back(Point.Time);
		}
	}
	std::sort(Times.begin(), Times.end());
	Times.erase(std::unique(Times.begin(), Times.end()), Times.end());
	if (Times.size() > MaxPoseKeys)
	{
		++Counts.Overflow;
		Times.resize(MaxPoseKeys);
	}
	Spline Path;
	Path.Build(Block);
	const Interp Mode = Block.Keys.front().Mode;
	Block.Keys.clear();
	for (const float Time : Times)
	{
		Fmatrix Orientation = Fidentity;
		Fmatrix Offset;
		Fmatrix Turned;
		PoseKey& Pose = Block.Keys.emplace_back();
		Path.Evaluate(Time, Pose.Position, Orientation);
		Pose.Position.add(Fvector().set(Sample(Offsets[0], Time, 0.0f), Sample(Offsets[1], Time, 0.0f), Sample(Offsets[2], Time, 0.0f)));
		Offset.setHPB(deg2rad(Sample(Offsets[3], Time, 0.0f)), deg2rad(Sample(Offsets[4], Time, 0.0f)), deg2rad(Sample(Offsets[5], Time, 0.0f)));
		Turned.mul(Orientation, Offset);
		Turned.getHPB(Pose.Rotation);
		Pose.Rotation.mul(rad2deg(1.0f));
		Pose.Time = Time;
		Pose.Mode = Mode;
	}
	++Counts.Baked;
}

void ReadShake(const CInifile& Ini, const char* Section, u32 Index, float Duration, xr_vector<ShakeClip>& Clips, Tally& Counts, Io::Warnings& Notes)
{
	string32 Prefix;
	string64 Line;
	xr_sprintf(Prefix, "shake_%02u_", Index);
	const auto Field = [&](const char* Name, float Fallback)
	{
		xr_sprintf(Line, "%s%s", Prefix, Name);
		return Number(Ini, Section, Line, Fallback, Notes);
	};
	const auto Flag = [&](const char* Name, bool Fallback)
	{
		xr_sprintf(Line, "%s%s", Prefix, Name);
		return Io::ReadBool(Ini, Section, Line, Fallback, Notes);
	};
	const float Preset = Field("preset", 0.0f);
	const float Influence = Field("influence", 1.0f);
	const float PositionScale = Field("position_scale", 1.0f);
	const float Speed = Field("speed", 1.0f);
	const float Offset = Field("offset", 0.0f);
	const float FadeIn = Field("fade_in", 0.0f);
	const float FadeOut = Field("fade_out", 0.0f);
	const bool IsEnabled = Flag("enabled", true);
	const bool IsStretched = Flag("stretch", false);
	if (!(Preset >= 0.0f && Preset < static_cast<float>(ShakePresetCount)) || !(Influence >= 0.0f && Influence <= 4.0f) || !(PositionScale >= 0.0f && PositionScale <= 100.0f) || !(Speed >= 0.0f && Speed <= 100.0f) || !(std::fabs(Offset) <= 3600.0f) || !(FadeIn >= 0.0f && FadeIn <= 3600.0f) || !(FadeOut >= 0.0f && FadeOut <= 3600.0f))
	{
		Notes.Add("[%s] %s has invalid values, shake layer skipped", Section, Prefix);
		return;
	}
	if (Clips.size() >= MaxShakeRows)
	{
		++Counts.Overflow;
		return;
	}
	OldChannel Parts[9];
	for (u32 Channel = 0; Channel < 9; ++Channel)
	{
		ReadChannel(Ini, Section, Prefix, Channel, ShakeBounds[Channel], Channel >= 4 && Channel <= 6, false, Channel == 3 ? 0.0f : 1.0f, Parts[Channel], Counts, Notes);
	}
	const u8 Layer = static_cast<u8>(Clips.size());
	ShakeClip& Clip = Clips.emplace_back();
	Clip.Length = Duration;
	Clip.Row = Layer;
	Shake& Entry = Clip.Setup;
	Entry.Preset = static_cast<u8>(std::clamp(Step(Parts[5], 0.0f, std::floor(Preset)), 0.0f, static_cast<float>(ShakePresetCount - 1)));
	Entry.IsEnabled = IsEnabled || !Parts[4].empty();
	Entry.Influence = Influence;
	Entry.PositionScale = PositionScale;
	Entry.Speed = Speed;
	Entry.Offset = std::clamp(Offset + Sample(Parts[3], 0.0f, 0.0f), -3600.0f, 3600.0f);
	Entry.IsStretched = Step(Parts[6], 0.0f, IsStretched ? 1.0f : 0.0f) >= 0.5f;
	Counts.Approximated += Varies(Parts[3]) || Varies(Parts[5]) || Varies(Parts[6]);
	if (!Parts[1].empty())
	{
		Curves::EnsureLane(Clip.Lanes, Param::ShakePositionScale, Layer).Curves[0] = Convert(Parts[1], PositionScale, Param::ShakePositionScale, false);
	}
	if (!Parts[2].empty())
	{
		Curves::EnsureLane(Clip.Lanes, Param::ShakeSpeed, Layer).Curves[0] = Convert(Parts[2], Speed, Param::ShakeSpeed, true);
	}
	if (Parts[4].empty() && Parts[7].empty() && Parts[8].empty())
	{
		Clip.FadeIn = FadeIn;
		Clip.FadeOut = FadeOut;
		if (!Parts[0].empty())
		{
			Curves::EnsureLane(Clip.Lanes, Param::ShakeInfluence, Layer).Curves[0] = Convert(Parts[0], Influence, Param::ShakeInfluence, false);
		}
		return;
	}
	const auto IsOn = [&](float Time) { return Step(Parts[4], Time, IsEnabled ? 1.0f : 0.0f) >= 0.5f; };
	const auto InfluenceAt = [&](float Time)
	{
		const float In = std::clamp(Sample(Parts[7], Time, FadeIn), 0.0f, 3600.0f);
		const float Out = std::clamp(Sample(Parts[8], Time, FadeOut), 0.0f, 3600.0f);
		const float Fade = (In > 0.0f ? std::clamp(Time / In, 0.0f, 1.0f) : 1.0f) * (Out > 0.0f ? std::clamp((Duration - Time) / Out, 0.0f, 1.0f) : 1.0f);
		return IsOn(Time) ? std::min(Influence * std::clamp(Sample(Parts[0], Time, 1.0f), 0.0f, 10.0f) * Fade, 4.0f) : 0.0f;
	};
	xr_vector<float> Times = { 0.0f, Duration, std::clamp(Sample(Parts[7], 0.0f, FadeIn), 0.0f, Duration), Duration - std::clamp(Sample(Parts[8], Duration, FadeOut), 0.0f, Duration) };
	for (const u32 Channel : { 0u, 4u, 7u, 8u })
	{
		for (const OldKey& Point : Parts[Channel])
		{
			Times.push_back(Point.Time);
			Times.push_back(Channel == 4 ? Point.Time - 0.001f : Point.Time);
		}
	}
	std::sort(Times.begin(), Times.end());
	Times.erase(std::remove_if(Times.begin(), Times.end(), [&](float Time) { return Time < 0.0f || Time > Duration; }), Times.end());
	Times.erase(std::unique(Times.begin(), Times.end(), [](float A, float B) { return B - A < 0.0001f; }), Times.end());
	Curve& Target = Curves::EnsureLane(Clip.Lanes, Param::ShakeInfluence, Layer).Curves[0];
	for (size_t Index = 0; Index < Times.size(); ++Index)
	{
		const bool IsStep = Index + 1 < Times.size() && IsOn(Times[Index]) != IsOn(Times[Index + 1]);
		Target.Keys.push_back({ Times[Index], InfluenceAt(Times[Index]), IsStep ? Interp::Hold : Interp::Linear });
	}
	++Counts.Merged;
}

bool ReadShot(const CInifile& Ini, const char* Section, Shot& Take, Tally& Counts, Io::Warnings& Notes)
{
	string64 Line;
	const float Duration = Number(Ini, Section, "duration", 1.0f, Notes);
	const float SourceDuration = Number(Ini, Section, "source_duration", Duration, Notes);
	const float Track = Number(Ini, Section, "track", 0.0f, Notes);
	Take.Start = Number(Ini, Section, "start", 0.0f, Notes);
	if (!(Take.Start >= 0.0f && Take.Start <= 3600.0f) || !(Duration >= 0.01f && Duration <= 3600.0f) || !(SourceDuration >= 0.01f && SourceDuration <= 3600.0f) || !(Track >= 0.0f && Track < 65535.0f))
	{
		Notes.Add("[%s] has an invalid start, duration or track, camera clip skipped", Section);
		return false;
	}
	OldChannel Channels[9];
	for (u32 Channel = 0; Channel < 9; ++Channel)
	{
		ReadChannel(Ini, Section, "camera_", Channel, CameraBounds[Channel], Channel >= 7, false, 0.0f, Channels[Channel], Counts, Notes);
	}
	const bool IsStretched = Io::ReadBool(Ini, Section, "stretch", true, Notes);
	const bool IsSmooth = Step(Channels[8], 0.0f, Io::ReadBool(Ini, Section, "smooth", true, Notes) ? 1.0f : 0.0f) >= 0.5f;
	const float Fov = Number(Ini, Section, "fov", 0.0f, Notes);
	const auto IsStretchedAt = [&](float Time) { return Step(Channels[7], Time, IsStretched ? 1.0f : 0.0f) >= 0.5f; };
	const float Missing = std::numeric_limits<float>::quiet_NaN();
	const Fvector Absent = Fvector().set(Missing, Missing, Missing);
	Take.Track = static_cast<s32>(Track);
	Take.IsMuted = Io::ReadBool(Ini, Section, "muted", false, Notes);
	Take.IsPreview = Io::ReadBool(Ini, Section, "preview", false, Notes);
	CameraBlock& Block = Take.Block;
	Block.Length = Duration;
	Block.Fov = Fov >= 1.0f && Fov <= 179.0f ? Fov : 0.0f;
	if (!Channels[6].empty())
	{
		Curves::EnsureLane(Block.Lanes, Param::Fov, 0).Curves[0] = Convert(Channels[6], 1.0f, Param::Fov, false);
	}
	Counts.Approximated += Varies(Channels[7]) || Varies(Channels[8]);
	const u32 KeyCount = Io::ReadU32(Ini, Section, "keys", MaxPoseKeys, Notes);
	for (u32 Index = 0; Index < KeyCount; ++Index)
	{
		xr_sprintf(Line, "key_%03u_time", Index);
		const float Time = Number(Ini, Section, Line, -1.0f, Notes);
		xr_sprintf(Line, "key_%03u_position", Index);
		const Fvector Position = ReadVector(Ini, Section, Line, Absent, Notes);
		xr_sprintf(Line, "key_%03u_rotation", Index);
		const Fvector Rotation = ReadVector(Ini, Section, Line, Absent, Notes);
		const float Stretched = Time * Duration / SourceDuration;
		const bool IsOriginal = Time <= Duration && !IsStretchedAt(Time);
		const float Local = IsOriginal ? Time : ((Stretched <= Duration && IsStretchedAt(Stretched)) || IsStretchedAt(Time)) ? Stretched : Time;
		if (!(Time >= 0.0f && Time <= 3600.0f) || !_valid(Position) || !_valid(Rotation) || Local > MaxTime)
		{
			++Counts.Keys;
			continue;
		}
		PoseKey& Pose = Block.Keys.emplace_back();
		Pose.Time = Local;
		Pose.Position = Position;
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			const float Degrees = rad2deg(Rotation[Axis]);
			Pose.Rotation[Axis] = std::fabs(Degrees) > 3600.0f ? std::remainder(Degrees, 360.0f) : Degrees;
		}
		Pose.Mode = IsSmooth ? Interp::Bezier : Interp::Linear;
	}
	std::stable_sort(Block.Keys.begin(), Block.Keys.end(), [](const PoseKey& A, const PoseKey& B) { return A.Time < B.Time; });
	const auto Last = std::unique(Block.Keys.begin(), Block.Keys.end(), [](const PoseKey& A, const PoseKey& B) { return A.Time == B.Time; });
	Counts.Keys += static_cast<u32>(Block.Keys.end() - Last);
	Block.Keys.erase(Last, Block.Keys.end());
	if (!Block.Keys.empty() && std::any_of(Channels, Channels + 6, [](const OldChannel& Offset) { return !Offset.empty(); }))
	{
		Bake(Block, Channels, Counts);
	}
	const u32 ShakeCount = Io::ReadU32(Ini, Section, "shake_count", MaxShakes, Notes);
	for (u32 Index = 0; Index < ShakeCount; ++Index)
	{
		ReadShake(Ini, Section, Index, Duration, Take.Shakes, Counts, Notes);
	}
	return true;
}

void AddCamera(xr_vector<Shot>& Shots, Scene& Out, Tally& Counts)
{
	if (Shots.empty())
	{
		return;
	}
	std::sort(Shots.begin(), Shots.end(), [](const Shot& A, const Shot& B) { return std::make_tuple(!A.IsEnabled, A.Start, A.Track, A.Index) < std::make_tuple(!B.IsEnabled, B.Start, B.Track, B.Index); });
	xr_vector<Shot*> Accepted;
	for (Shot& Take : Shots)
	{
		const float End = Take.Start + Take.Block.Length;
		if (std::any_of(Accepted.begin(), Accepted.end(), [&](const Shot* Other) { return Take.Start < Other->Start + Other->Block.Length && Other->Start < End; }))
		{
			++Counts.Overlaps;
			continue;
		}
		Counts.Muted += !Take.IsEnabled;
		Counts.Hidden += !Take.IsPreview;
		Accepted.push_back(&Take);
	}
	std::sort(Accepted.begin(), Accepted.end(), [](const Shot* A, const Shot* B) { return A->Start < B->Start; });
	Object& Rig = Out.Objects.emplace_back();
	Rig.Id = Out.NextId++;
	Rig.Type = ObjectType::Camera;
	Rig.Name = "Camera";
	Rig.Color = NextColor(Out.Header);
	Rig.Row = static_cast<u32>(Out.Objects.size() - 1);
	xr_vector<CameraBlock>& Blocks = Rig.Camera.Blocks;
	float Length = 0.0f;
	for (Shot* Take : Accepted)
	{
		if (Blocks.size() >= MaxBlocks)
		{
			++Counts.Overflow;
			continue;
		}
		CameraBlock& Block = Blocks.emplace_back(std::move(Take->Block));
		Block.Start = std::clamp(Take->Start, 0.0f, MaxTime - MinLength);
		Block.Length = std::clamp(Block.Length, MinLength, MaxTime - Block.Start);
		Length = std::max(Length, Block.Start + Block.Length);
		for (ShakeClip& Clip : Take->Shakes)
		{
			Clip.Start = Block.Start;
			Counts.Overflow += Rig.Camera.Shakes.size() >= MaxShakeClips;
			if (Rig.Camera.Shakes.size() < MaxShakeClips)
			{
				Rig.Camera.Shakes.push_back(std::move(Clip));
			}
		}
	}
	Rig.Span.Length = std::clamp(Length, MinLength, MaxTime);
}

void BuildSequence(const xr_vector<OldEntry>& Entries, const xr_vector<OldTrim>& Trims, float ActorSpeed, xr_vector<ActorBlock>& Blocks, Tally& Counts)
{
	xr_vector<float> Blends(Blocks.size(), 0.0f);
	for (const OldEntry& Entry : Entries)
	{
		const auto Found = std::find_if(Trims.begin(), Trims.end(), [&](const OldTrim& Trim) { return Trim.Source == Entry.Source && Trim.Name == Entry.Name; });
		const OldTrim* Trim = Found == Trims.end() ? nullptr : &*Found;
		const float Speed = Trim ? Trim->Speed : ActorSpeed;
		if (std::fabs(Speed) <= EPS_S || !Io::IniSafe(Entry.Source.c_str()) || !Io::IniSafe(Entry.Name.c_str()))
		{
			++Counts.Entries;
			continue;
		}
		const bool HasSegments = Trim && !Trim->Segments.empty();
		const u32 Count = HasSegments ? static_cast<u32>(Trim->Segments.size()) : 1;
		for (u32 Index = 0; Index < Count; ++Index)
		{
			if (Blocks.size() >= MaxBlocks)
			{
				++Counts.Overflow;
				Actors::Unpack(Blocks, Blends);
				return;
			}
			const OldSegment Segment = HasSegments ? Trim->Segments[Speed < 0.0f ? Count - 1 - Index : Index] : OldSegment();
			ActorBlock& Block = Blocks.emplace_back();
			Block.Source = Entry.Source;
			Block.Name = Entry.Name;
			Block.In = Segment.Start;
			Block.Out = Segment.End;
			Block.Speed = std::copysign(std::clamp(std::fabs(Speed), 0.05f, 4.0f), Speed);
			Blends.push_back(Index > 0 ? Trim->Blend : 0.0f);
		}
	}
	Actors::Unpack(Blocks, Blends);
}

void AddPack(ActorData& Setup, const shared_str& Pack, Tally& Counts)
{
	if (!Io::IsOmfPath(Pack.c_str()) || !Io::IniSafe(Pack.c_str()) || std::find(Setup.Packs.begin(), Setup.Packs.end(), Pack) != Setup.Packs.end())
	{
		return;
	}
	if (Setup.Packs.size() >= MaxPacks)
	{
		++Counts.Overflow;
		return;
	}
	Setup.Packs.push_back(Pack);
}

void ReadSequence(const CInifile& Ini, const char* Section, const OldClip* Migrated, ActorData& Setup, Tally& Counts, Io::Warnings& Notes)
{
	string64 Line;
	const float Speed = Number(Ini, Section, "speed", 1.0f, Notes);
	Setup.End = Io::ReadBool(Ini, Section, "pingpong", false, Notes) ? EndMode::PingPong : Io::ReadBool(Ini, Section, "loop", false, Notes) ? EndMode::Loop : EndMode::Hold;
	const u32 PackCount = Io::ReadU32(Ini, Section, "animation_packs", 65535, Notes);
	for (u32 Index = 0; Index < PackCount; ++Index)
	{
		xr_sprintf(Line, "animation_pack_%03u", Index);
		AddPack(Setup, Ini.read_if_exists<str_c>(Section, Line, nullptr), Counts);
	}
	xr_vector<OldEntry> Entries;
	u32 Listed = 0;
	const u32 PlaylistCount = Io::ReadU32(Ini, Section, "playlist", 65535, Notes);
	for (u32 Index = 0; Index < PlaylistCount; ++Index)
	{
		xr_sprintf(Line, "playlist_%03u", Index);
		const char* Value = Ini.read_if_exists<str_c>(Section, Line, nullptr);
		const char* First = Value ? strchr(Value, '|') : nullptr;
		const char* Second = First ? strchr(First + 1, '|') : nullptr;
		if (!Second)
		{
			continue;
		}
		++Listed;
		xr_sprintf(Line, "playlist_%03u_muted", Index);
		if (Io::ReadBool(Ini, Section, Line, false, Notes) || !xr_strcmp(Second + 1, "1"))
		{
			++Counts.Entries;
			continue;
		}
		Entries.push_back({ xr_string(Value, static_cast<u32>(First - Value)).c_str(), xr_string(First + 1, static_cast<u32>(Second - First - 1)).c_str() });
	}
	xr_vector<OldTrim> Trims;
	const u32 TrimCount = Io::ReadU32(Ini, Section, "trims", 65535, Notes);
	for (u32 Index = 0; Index < TrimCount; ++Index)
	{
		xr_sprintf(Line, "trim_%03u_shots", Index);
		Counts.Shots += Ini.line_exist(Section, Line);
		xr_sprintf(Line, "trim_%03u", Index);
		const char* Value = Ini.read_if_exists<str_c>(Section, Line, nullptr);
		OldTrim Trim;
		if (Value && std::count(Value, Value + xr_strlen(Value), '|') >= 3 && ParseTrim(Value, Trim))
		{
			Trims.push_back(std::move(Trim));
		}
	}
	if (Migrated && !Listed)
	{
		Entries.push_back({ Migrated->Source, Migrated->Name });
		auto Found = std::find_if(Trims.begin(), Trims.end(), [&](const OldTrim& Trim) { return Trim.Source == Migrated->Source && Trim.Name == Migrated->Name; });
		if (Found == Trims.end())
		{
			Trims.push_back({ Migrated->Source, Migrated->Name, {}, Migrated->Speed });
			Found = Trims.end() - 1;
		}
		if (Found->Segments.empty() && (Migrated->SourceIn > 0.0f || Migrated->SourceOut > 0.0f))
		{
			Found->Segments.push_back({ Migrated->SourceIn, Migrated->SourceOut });
		}
	}
	BuildSequence(Entries, Trims, std::isfinite(Speed) ? std::clamp(Speed, -4.0f, 4.0f) : 1.0f, Setup.Base.Blocks, Counts);
	for (const ActorBlock& Block : Setup.Base.Blocks)
	{
		AddPack(Setup, Block.Source, Counts);
	}
}

void Report(const Tally& Counts, Io::Warnings& Notes)
{
	const std::pair<u32, const char*> Lines[] = {
		{ Counts.Shots, "%u shot lists were dropped." },
		{ Counts.Poses, "%u saved NPC poses were dropped." },
		{ Counts.Transforms, "%u master transform keys were dropped." },
		{ Counts.Entries, "%u playlist entries were dropped (muted, FX, zero speed or unsupported names)." },
		{ Counts.Keys, "%u invalid or repeated curve and camera keys were skipped." },
		{ Counts.Clips, "%u invalid master clips were skipped." },
		{ Counts.Muted, "%u muted camera clips were imported as normal blocks." },
		{ Counts.Hidden, "%u camera clips without Preview were imported as normal blocks." },
		{ Counts.Overlaps, "%u overlapping camera clips were dropped." },
		{ Counts.Baked, "%u camera clips had their offset curves baked into pose keys." },
		{ Counts.Merged, "%u shake layers had fades or enable spans merged into Influence keys." },
		{ Counts.Approximated, "%u camera clips or shake layers had animated stretch, smooth, preset or offset reduced to the value at their start." },
		{ Counts.Unsafe, "%u model, item, pack, animation, effect or texture names with unsupported characters or too long were dropped." },
		{ Counts.Overflow, "%u objects, presets, blocks, packs, shakes or keys above the format limits were dropped." },
		{ Counts.Effects, "%u lights or particles were skipped as invalid, or had a scale, speed, IN/OUT, modulation, duration, colour ramp or wave reset to defaults." },
		{ Counts.Rotations, "%u lights or particles with a tilted rest rotation had their rotation keys sampled (approximate)." },
		{ Counts.Ramps, "%u clip colour ramps over keyed or waved colour channels were applied at the keys only (approximate)." },
		{ Counts.Unrolled, "%u lights or particles changed timing: free loops were unrolled into keys, waves no longer restart or stop at the end of their modulation, or a free particle loop was dropped." },
		{ Counts.EngineFlags, "%u lights had HUD mode on or occlusion queries off; both settings were dropped." },
		{ Counts.Owned, "%u pattern-owned lights or particles without a master clip were dropped." },
		{ Counts.Bands, "%u modulation waves, key values or light colours were clamped or dropped to fit the new value ranges." }
	};
	for (const auto& [Count, Text] : Lines)
	{
		if (Count)
		{
			Notes.Add(Text, Count);
		}
	}
}

void ImportActor(const CInifile& Ini, const char* Section, s32 Version, s32 Solo, u32 Ordinal, Scene& Out, Tally& Counts, Io::Warnings& Notes)
{
	string64 Line;
	ActorData Setup;
	Setup.Visual = SafeText(Ini, Section, "visual", Counts);
	Setup.Item = SafeText(Ini, Section, "item", Counts);
	Setup.Position = ReadVector(Ini, Section, "position", Setup.Position, Notes);
	Setup.Rotation = ReadVector(Ini, Section, "rotation", Setup.Rotation, Notes);
	Setup.Scale = ReadVector(Ini, Section, "scale", Setup.Scale, Notes);
	if (!_valid(Setup.Position) || !_valid(Setup.Rotation) || !_valid(Setup.Scale))
	{
		Notes.Add("[%s] has broken coordinates, NPC skipped", Section);
		return;
	}
	ClampPlacement(Setup.Position, Setup.Rotation);
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		Setup.Scale[Axis] = std::clamp(Setup.Scale[Axis], 0.1f, 5.0f);
	}
	const bool IsMaster = Io::ReadBool(Ini, Section, "master_track", false, Notes);
	const bool IsPattern = Io::ReadBool(Ini, Section, "pattern_owned", false, Notes);
	Counts.Transforms += Io::ReadU32(Ini, Section, "master_transforms", 65535, Notes);
	Counts.Poses += Ini.line_exist(Section, "pose_clip");
	xr_vector<OldClip> Clips;
	const u32 ClipCount = Io::ReadU32(Ini, Section, "master_clips", 65535, Notes);
	for (u32 Index = 0; Index < ClipCount; ++Index)
	{
		const auto Field = [&](const char* Suffix) -> const char*
		{
			xr_sprintf(Line, "master_clip_%03u%s", Index, Suffix);
			return Line;
		};
		OldClip Take;
		const bool IsValid = ReadClip(Ini, Section, Index, Ordinal, Take, Notes);
		Take.Source = Ini.read_if_exists<str_c>(Section, Field("_source"), nullptr);
		Take.Name = Ini.read_if_exists<str_c>(Section, Field("_name"), nullptr);
		Take.SourceIn = Number(Ini, Section, Field("_source_in"), 0.0f, Notes);
		Take.SourceOut = Number(Ini, Section, Field("_source_out"), 0.0f, Notes);
		Take.Speed = Number(Ini, Section, Field("_speed"), 1.0f, Notes);
		Counts.Shots += Ini.line_exist(Section, Field("_shots"));
		if (!(Take.SourceIn >= 0.0f && Take.SourceIn <= 3600.0f) || !(Take.SourceOut >= 0.0f && Take.SourceOut <= 3600.0f) || (Take.SourceOut > 0.0f && Take.SourceOut <= Take.SourceIn) || !(Take.Speed >= 0.05f && Take.Speed <= 4.0f))
		{
			Notes.Add("[%s] master clip %u has an invalid trim or speed, defaults are used", Section, Index);
			Take.SourceIn = 0.0f;
			Take.SourceOut = 0.0f;
			Take.Speed = 1.0f;
		}
		if (!IsValid || (Take.Source.size() != 0) != (Take.Name.size() != 0))
		{
			++Counts.Clips;
			continue;
		}
		Clips.push_back(std::move(Take));
	}
	const bool IsMigrated = IsPattern && Version < 4 && !Clips.empty() && Clips.front().Source.size();
	ReadSequence(Ini, Section, IsMigrated ? &Clips.front() : nullptr, Setup, Counts, Notes);
	for (const OldClip& Take : Clips)
	{
		AddPack(Setup, Take.Source, Counts);
	}
	const char* RawName = Ini.read_if_exists<str_c>(Section, "name", nullptr);
	string32 Generated;
	xr_sprintf(Generated, "FakeNPC_%02u", Ordinal + 1);
	const xr_string BaseName = FitName(RawName ? RawName : Generated);
	const bool IsGenerated = BaseName.size() > 8 && !strncmp(BaseName.c_str(), "FakeNPC_", 8) && std::all_of(BaseName.begin() + 8, BaseName.end(), [](char Symbol) { return Symbol >= '0' && Symbol <= '9'; });
	const auto Emit = [&](const xr_string& Label, u32 Color, const Clip& Timing, bool IsMuted, EndMode Mode, xr_vector<ActorBlock> Blocks)
	{
		Object* Item = AddObject(Out, ObjectType::Actor, Label, BaseName, Color, Counts);
		if (!Item)
		{
			return;
		}
		Item->IsNameLocked = Item->IsNameLocked || !IsGenerated;
		Item->IsMuted = IsMuted;
		Item->Span = Timing;
		Item->Actor = Setup;
		Item->Actor.IsRelative = true;
		Item->Actor.End = Mode;
		Item->Actor.Base.Blocks = std::move(Blocks);
	};
	if (IsMaster && IsPattern && !Clips.empty())
	{
		for (const OldClip& Take : Clips)
		{
			Emit(Take.Label, Take.Color, { Take.Start, Take.Duration, true }, Take.IsMuted || IsTrackMuted(Ini, Take.Track, Solo, Notes), Setup.End, Setup.Base.Blocks);
		}
		return;
	}
	if (IsMaster && !Clips.empty())
	{
		for (const OldClip& Take : Clips)
		{
			xr_vector<ActorBlock> Single;
			if (Take.Source.size() && Io::IniSafe(Take.Source.c_str()) && Io::IniSafe(Take.Name.c_str()))
			{
				Single.push_back({ Take.Source, Take.Name, Take.SourceIn, Take.SourceOut, Take.Speed });
			}
			Counts.Unsafe += Take.Source.size() && Single.empty();
			Emit(Take.Label, Take.Color, { Take.Start, Take.Duration, true }, Take.IsMuted || IsTrackMuted(Ini, Take.Track, Solo, Notes), EndMode::Hold, std::move(Single));
		}
		return;
	}
	Emit({}, 0, { 0.0f, 5.0f, false }, false, Setup.End, std::move(Setup.Base.Blocks));
}

bool HasWave(const OldEffect& Source)
{
	return std::any_of(std::begin(Source.Waves), std::end(Source.Waves), [](const Generator& Shape) { return Shape.Type != Wave::None; });
}

void RampLight(Object& Item, OldEffect& Source, const OldClip& Take, Tally& Counts)
{
	LightData& Setup = Item.Light;
	const float Excess = std::max(Setup.Intensity * std::max(Take.Ramp[0], Take.Ramp[1]) / 100.0f, 1.0f);
	Setup.Intensity /= Excess;
	if (Take.Ramp[0] != Take.Ramp[1])
	{
		PutChannel(Item.Lanes, Param::Intensity, 0, { { 0.0f, Take.Ramp[0] }, { Take.Duration, Take.Ramp[1] } }, Generator {}, Setup.Intensity, 0.0f, Counts);
	}
	Setup.Intensity *= Take.Ramp[0] == Take.Ramp[1] ? Take.Ramp[0] : 1.0f;
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		const float First = Take.Tint[0][Axis];
		const float Final = Take.Tint[1][Axis];
		OldChannel& Keys = Source.Channels[6 + Axis];
		const float Wanted = Setup.Color[Axis] * Excess * (First == Final ? First : 1.0f);
		Setup.Color[Axis] = std::min(Wanted, 100000.0f);
		Counts.Bands += Wanted > 100000.0f;
		if (First == Final)
		{
			continue;
		}
		Counts.Ramps += !Keys.empty() || Source.Waves[6 + Axis].Type != Wave::None;
		if (Keys.empty())
		{
			Keys = { { 0.0f, 1.0f }, { Take.Duration, 1.0f } };
		}
		SplitAt(Keys, 0.0f);
		SplitAt(Keys, Take.Duration);
		for (OldKey& Point : Keys)
		{
			const float Gain = First + (Final - First) * std::clamp(Point.Time / Take.Duration, 0.0f, 1.0f);
			Point.Value *= Gain;
			Point.InSlope *= Gain;
			Point.OutSlope *= Gain;
		}
	}
}

void ImportLight(const CInifile& Ini, u32 Index, s32 Solo, Scene& Out, xr_vector<FreeLight>& Free, Tally& Counts, Io::Warnings& Notes)
{
	constexpr const char* Ends[2] = { "start", "end" };
	string32 Section;
	string64 Line;
	xr_sprintf(Section, "light_%03u", Index);
	if (!Ini.section_exist(Section))
	{
		++Counts.Effects;
		return;
	}
	LightData Setup;
	Setup.Position = ReadVector(Ini, Section, "position", Setup.Position, Notes);
	Setup.Rotation = ReadVector(Ini, Section, "rotation", Setup.Rotation, Notes);
	Setup.Color = ReadVector(Ini, Section, "color", Setup.Color, Notes);
	const float Type = Number(Ini, Section, "type", 0.0f, Notes);
	float* const Fields[7] = { &Setup.Intensity, &Setup.Range, &Setup.Cone, &Setup.VirtualSize, &Setup.VolumeIntensity, &Setup.VolumeDistance, &Setup.VolumeQuality };
	bool IsGood = _valid(Setup.Position) && _valid(Setup.Rotation) && _valid(Setup.Color) && Setup.Color.x >= 0.0f && Setup.Color.y >= 0.0f && Setup.Color.z >= 0.0f && (Type == 0.0f || Type == 1.0f);
	for (u32 Field = 0; Field < 7; ++Field)
	{
		*Fields[Field] = Number(Ini, Section, LightFields[Field], *Fields[Field], Notes);
		IsGood = IsGood && *Fields[Field] >= LightLimits[Field][0] && *Fields[Field] <= LightLimits[Field][1];
	}
	if (!IsGood)
	{
		++Counts.Effects;
		return;
	}
	ClampPlacement(Setup.Position, Setup.Rotation);
	Setup.Kind = Type == 1.0f ? LightKind::Spot : LightKind::Point;
	Setup.HasShadow = Io::ReadBool(Ini, Section, "shadow", false, Notes);
	Setup.IsVolumetric = Io::ReadBool(Ini, Section, "volumetric", false, Notes);
	Setup.Texture = SafeText(Ini, Section, "texture", Counts);
	if (Setup.Texture.size() > MaxTextureBytes)
	{
		++Counts.Unsafe;
		Setup.Texture = shared_str();
	}
	const bool IsActive = Io::ReadBool(Ini, Section, "active", true, Notes);
	const bool IsLoop = Io::ReadBool(Ini, Section, "modulation_loop", true, Notes);
	const bool IsMaster = Io::ReadBool(Ini, Section, "master_track", false, Notes);
	const float Duration = Number(Ini, Section, "modulation_duration", 10.0f, Notes);
	const bool IsPeriod = Duration >= 0.01f && Duration <= 600.0f;
	const float Period = IsPeriod ? Duration : 10.0f;
	Counts.Effects += !IsPeriod;
	Counts.EngineFlags += Io::ReadBool(Ini, Section, "hud_mode", false, Notes) || !Io::ReadBool(Ini, Section, "occlusion", true, Notes);
	Counts.Transforms += Io::ReadU32(Ini, Section, "master_transforms", 65535, Notes);
	OldEffect Source;
	ReadEffect(Ini, Section, true, Source, Counts, Notes);
	xr_vector<OldClip> Clips;
	const u32 ClipCount = IsMaster ? Io::ReadU32(Ini, Section, "master_clips", 65535, Notes) : 0;
	for (u32 ClipIndex = 0; ClipIndex < ClipCount; ++ClipIndex)
	{
		OldClip Take;
		bool IsValid = ReadClip(Ini, Section, ClipIndex, Index + 20, Take, Notes);
		bool IsTinted = true;
		for (u32 Side = 0; Side < 2; ++Side)
		{
			xr_sprintf(Line, "master_clip_%03u_intensity_%s", ClipIndex, Ends[Side]);
			Take.Ramp[Side] = Number(Ini, Section, Line, 1.0f, Notes);
			xr_sprintf(Line, "master_clip_%03u_color_%s", ClipIndex, Ends[Side]);
			Take.Tint[Side] = ReadVector(Ini, Section, Line, Take.Tint[Side], Notes);
			IsValid = IsValid && Take.Ramp[Side] >= 0.0f && Take.Ramp[Side] <= 10.0f;
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				IsTinted = IsTinted && Take.Tint[Side][Axis] >= 0.0f && Take.Tint[Side][Axis] <= 10.0f;
			}
		}
		Counts.Effects += !IsTinted;
		Counts.Clips += !IsValid;
		Take.Tint[0] = IsTinted ? Take.Tint[0] : Fvector().set(1.0f, 1.0f, 1.0f);
		Take.Tint[1] = IsTinted ? Take.Tint[1] : Fvector().set(1.0f, 1.0f, 1.0f);
		if (IsValid)
		{
			Clips.push_back(std::move(Take));
		}
	}
	if (Clips.empty() && IsMaster && Io::ReadBool(Ini, Section, "pattern_owned", false, Notes))
	{
		++Counts.Owned;
		return;
	}
	const char* Name = Setup.Kind == LightKind::Spot ? "Spot light" : "Point light";
	if (Clips.empty())
	{
		const bool HasKeys = std::any_of(std::begin(Source.Channels), std::end(Source.Channels), [](const OldChannel& Keys) { return !Keys.empty(); });
		for (OldChannel& Keys : Source.Channels)
		{
			Cut(Keys, Period, false);
		}
		Object* Item = AddObject(Out, ObjectType::Light, {}, Name, 0, Counts);
		if (!Item)
		{
			return;
		}
		Item->IsMuted = !IsActive;
		Item->Span.Length = MinLength;
		Item->Light = Setup;
		RampLight(*Item, Source, OldClip {}, Counts);
		BuildLanes(*Item, Source, Counts);
		Free.push_back({ static_cast<u32>(Out.Objects.size() - 1), Period, IsLoop });
		Counts.Unrolled += (IsLoop && HasKeys) || HasWave(Source);
		return;
	}
	for (const OldClip& Take : Clips)
	{
		Object* Item = AddObject(Out, ObjectType::Light, Take.Label, Name, Take.Color, Counts);
		if (!Item)
		{
			return;
		}
		OldEffect Tinted = Source;
		Item->IsMuted = Take.IsMuted || IsTrackMuted(Ini, Take.Track, Solo, Notes) || !IsActive;
		Item->Span = { Take.Start, Take.Duration, false };
		Item->Light = Setup;
		RampLight(*Item, Tinted, Take, Counts);
		BuildLanes(*Item, Tinted, Counts);
	}
}

void ImportParticle(const CInifile& Ini, const char* Section, u32 Ordinal, s32 Solo, Scene& Out, Tally& Counts, Io::Warnings& Notes)
{
	ParticleData Setup;
	Setup.Effect = SafeText(Ini, Section, "name", Counts);
	Setup.Position = ReadVector(Ini, Section, "position", Setup.Position, Notes);
	Setup.Rotation = ReadVector(Ini, Section, "rotation", Setup.Rotation, Notes);
	if (!_valid(Setup.Position))
	{
		++Counts.Effects;
		return;
	}
	Setup.Rotation = _valid(Setup.Rotation) ? Setup.Rotation : Fvector().set(0.0f, 0.0f, 0.0f);
	ClampPlacement(Setup.Position, Setup.Rotation);
	const float Scale = Number(Ini, Section, "scale", 1.0f, Notes);
	const float Speed = Number(Ini, Section, "speed", 1.0f, Notes);
	const float In = Number(Ini, Section, "in", 0.0f, Notes);
	const float Last = Number(Ini, Section, "out", 10.0f, Notes);
	const bool IsTimed = In >= 0.0f && Last >= In + 0.01f && Last <= 600.0f;
	const float Begin = IsTimed ? In : 0.0f;
	const float Finish = IsTimed ? Last : 10.0f;
	Setup.Size = Scale >= 0.1f && Scale <= 5.0f ? Scale : 1.0f;
	Setup.Speed = Speed >= 0.0f && Speed <= 4.0f ? Speed : 1.0f;
	Setup.ShouldRetrigger = Io::ReadBool(Ini, Section, "loop", false, Notes);
	Counts.Effects += (Setup.Size != Scale) + (Setup.Speed != Speed) + !IsTimed;
	Counts.Transforms += Io::ReadU32(Ini, Section, "master_transforms", 65535, Notes);
	OldEffect Source;
	if (Io::ReadU32(Ini, Section, "curve_version", ~0u, Notes) == 1)
	{
		ReadEffect(Ini, Section, false, Source, Counts, Notes);
	}
	else if (Io::ReadBool(Ini, Section, "modulation", false, Notes))
	{
		const Fvector Offset = ReadVector(Ini, Section, "modulation_offset", Fvector().set(0.0f, 0.0f, 0.0f), Notes);
		const Fvector Turn = ReadVector(Ini, Section, "modulation_rotation", Fvector().set(0.0f, 0.0f, 0.0f), Notes);
		const float Ends[7] = { Offset.x, Offset.y, Offset.z, Turn.x, Turn.y, Turn.z, Number(Ini, Section, "modulation_scale", 1.0f, Notes) };
		const bool IsGood = _valid(Offset) && _valid(Turn) && Ends[6] >= 0.1f && Ends[6] <= 5.0f;
		Counts.Effects += !IsGood;
		for (u32 Channel = 0; IsGood && Channel < 7; ++Channel)
		{
			Source.Channels[Channel] = { { 0.0f, Channel == 6 ? 1.0f : 0.0f }, { Finish, Ends[Channel] } };
		}
	}
	const bool IsMaster = Io::ReadBool(Ini, Section, "master_track", false, Notes);
	xr_vector<OldClip> Clips;
	const u32 ClipCount = IsMaster ? Io::ReadU32(Ini, Section, "master_clips", 65535, Notes) : 0;
	for (u32 Index = 0; Index < ClipCount; ++Index)
	{
		OldClip Take;
		const bool IsValid = ReadClip(Ini, Section, Index, Ordinal + 10, Take, Notes);
		Counts.Clips += !IsValid;
		if (IsValid)
		{
			Clips.push_back(std::move(Take));
		}
	}
	if (Clips.empty() && IsMaster && Io::ReadBool(Ini, Section, "pattern_owned", false, Notes))
	{
		++Counts.Owned;
		return;
	}
	const bool IsFree = Clips.empty();
	if (IsFree)
	{
		for (OldChannel& Keys : Source.Channels)
		{
			Cut(Keys, Begin, true);
		}
		for (Generator& Shape : Source.Waves)
		{
			Shape.Phase += Begin * Shape.Frequency;
		}
		Counts.Unrolled += Setup.ShouldRetrigger;
		OldClip& Whole = Clips.emplace_back();
		Whole.Start = Begin;
		Whole.Duration = Finish - Begin;
	}
	const xr_string Name = FitName(Platform::ANSI_TO_UTF8(EffectName(Setup.Effect)).c_str());
	for (const OldClip& Take : Clips)
	{
		Object* Item = AddObject(Out, ObjectType::Particle, Take.Label, Name, Take.Color, Counts);
		if (!Item)
		{
			return;
		}
		OldEffect Frozen = Source;
		const bool IsFrozen = !IsFree && Finish < Take.Duration;
		for (u32 Channel = 0; IsFrozen && Channel < 8; ++Channel)
		{
			Cut(Frozen.Channels[Channel], Finish, false);
		}
		Counts.Unrolled += IsFrozen && HasWave(Source);
		Item->IsMuted = !IsFree && (Take.IsMuted || IsTrackMuted(Ini, Take.Track, Solo, Notes));
		Item->Span = { Take.Start, Take.Duration, false };
		Item->Particle = Setup;
		BuildLanes(*Item, Frozen, Counts);
	}
}

bool Import(const CInifile& Ini, s32 Version, Scene& Out, Io::Warnings& Notes)
{
	constexpr const char* Prefixes[4] = { "light_group_", "light_", "particle_group_", "particle_" };
	Tally Counts;
	u32 Skipped[4] = {};
	u32 Ordinal = 0;
	u32 Emitters = 0;
	const float SoloTrack = Number(Ini, "scene", "master_solo_track", -1.0f, Notes);
	const s32 Solo = SoloTrack >= 0.0f && SoloTrack < 65535.0f ? static_cast<s32>(SoloTrack) : -1;
	const CInifile::Sect* Head = Ini.r_section_nullable("scene");
	if (Solo >= 0)
	{
		Notes.Add("A solo track was active when the scene was saved; clips keep the mute state the solo left.");
	}
	if (Head && std::any_of(Head->Data.begin(), Head->Data.end(), [](const CInifile::Item& Entry) { const char* Name = Entry.first.c_str(); return Name && strstr(Name, "master_track_") == Name && (strstr(Name, "_group") || strstr(Name, "_parent")); }))
	{
		Notes.Add("Track groups were dropped; clips keep the mute state of their track and its parent.");
	}
	xr_vector<Shot> Shots;
	const u32 CameraCount = Io::ReadU32(Ini, "scene", "cameras", MaxObjects, Notes);
	for (u32 Index = 0; Index < CameraCount; ++Index)
	{
		string32 Section;
		xr_sprintf(Section, "camera_%03u", Index);
		Shot Take;
		Take.Index = Index;
		if (Ini.section_exist(Section) && ReadShot(Ini, Section, Take, Counts, Notes))
		{
			Take.IsEnabled = !Take.IsMuted && !IsTrackMuted(Ini, Take.Track, Solo, Notes);
			Shots.push_back(std::move(Take));
		}
	}
	AddCamera(Shots, Out, Counts);
	for (const CInifile::Sect& Section : Ini.sections())
	{
		const char* Name = Section.Name.c_str();
		u32 Kind = 0;
		while (Kind < 4 && strncmp(Name, Prefixes[Kind], xr_strlen(Prefixes[Kind])))
		{
			++Kind;
		}
		if (Kind == 3)
		{
			ImportParticle(Ini, Name, Emitters++, Solo, Out, Counts, Notes);
		}
		else if (Kind < 4)
		{
			++Skipped[Kind];
		}
		else if (!strncmp(Name, "actor_", 6))
		{
			ImportActor(Ini, Name, Version, Solo, Ordinal++, Out, Counts, Notes);
		}
	}
	xr_vector<FreeLight> Free;
	const u32 LightCount = std::min(Io::ReadU32(Ini, "scene", "lights", 65535, Notes), static_cast<u32>(Ini.sections().size()));
	for (u32 Index = 0; Index < LightCount; ++Index)
	{
		ImportLight(Ini, Index, Solo, Out, Free, Counts, Notes);
	}
	const float End = std::max(ContentEnd(Out), 5.0f);
	for (const FreeLight& Pending : Free)
	{
		Object& Item = Out.Objects[Pending.Index];
		Item.Span.Length = std::min(std::max(End, Pending.Period), MaxTime);
		for (Lane& Track : Item.Lanes)
		{
			for (u32 Component = 0; Pending.IsLoop && Component < 3; ++Component)
			{
				Unroll(Track.Curves[Component], Pending.Period, Item.Span.Length, Counts);
			}
		}
	}
	Report(Counts, Notes);
	if (Skipped[0] + Skipped[2])
	{
		Notes.Add("%u light groups and %u particle groups were dropped.", Skipped[0], Skipped[2]);
	}
	return !Out.Objects.empty();
}

void ImportPresets(const CInifile& Ini, xr_vector<ActorPreset>& Out, Io::Warnings& Notes)
{
	Tally Counts;
	u32 Skipped = 0;
	for (const CInifile::Sect& Section : Ini.sections())
	{
		const char* Name = Section.Name.c_str();
		if (strncmp(Name, "preset_", 7))
		{
			continue;
		}
		if (Out.size() >= Io::MaxPresets)
		{
			++Counts.Overflow;
			continue;
		}
		const char* Text = Ini.read_if_exists<str_c>(Name, "name", nullptr);
		ActorPreset Item;
		Item.Name = FitName(Platform::ANSI_TO_UTF8(Text ? Text : "").c_str());
		Item.Setup.Visual = SafeText(Ini, Name, "visual", Counts);
		Item.Setup.Item = SafeText(Ini, Name, "item", Counts);
		Item.Setup.Scale = ReadVector(Ini, Name, "scale", Item.Setup.Scale, Notes);
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			Item.Setup.Scale[Axis] = Sanitize(Item.Setup.Scale[Axis], 0.1f, 5.0f, 1.0f);
		}
		ReadSequence(Ini, Name, nullptr, Item.Setup, Counts, Notes);
		Item.Setup.IsRelative = true;
		if (Item.Name.empty() || !Item.Setup.Visual.size() || std::any_of(Out.begin(), Out.end(), [&](const ActorPreset& Other) { return Other.Name == Item.Name; }))
		{
			++Skipped;
			continue;
		}
		Out.push_back(std::move(Item));
	}
	Report(Counts, Notes);
	if (Skipped)
	{
		Notes.Add("%u presets without a name or model, or with a repeated name, were skipped.", Skipped);
	}
}
}
