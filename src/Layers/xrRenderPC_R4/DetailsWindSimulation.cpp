#include "stdafx.h"
#include "DetailsWind.h"
#include "DetailWindMath.h"
#include "../../xrCore/_static_thread.h"
#include <bit>
#include <cmath>

extern bool ps_wind_enabled;

namespace DetailWind
{
using namespace DetailWindMath;
constexpr float Radians = 0.017453292519943295f;
static std::atomic<u32> g_CompletedPlants{0};
static std::atomic<u32> g_CompletedSamples{0};
static std::atomic<u32> g_CompletedGusts{0};
static std::atomic<float> g_PublishedStepMs{16};
static std::atomic<int> g_PendingPreview{-2};
static std::atomic<u32> g_BehaviorCounts[CDetailWind::BehaviorCount]{};

struct Cell
{
	s32 X, Y, Z;
	bool operator==(const Cell& B) const { return X == B.X && Y == B.Y && Z == B.Z; }
};
} // namespace DetailWind
namespace std
{
template <>
struct hash<DetailWind::Cell>
{
	size_t operator()(const DetailWind::Cell& C) const
	{
		return size_t(DetailWindMath::Hash(u64(u32(C.X)) ^ (u64(u32(C.Y)) << 21) ^ (u64(u32(C.Z)) << 42)));
	}
};
} // namespace std
namespace DetailWind
{
struct Gust
{
	Vec3 Position;
	float Heading, Speed, Life, Age, Length, Width, Height, Strength, Vertical, Warp, Wander, Attack, Release;
	int Behavior = 0;
	float Influence = 1, Spin = 1, InitialHeading = 0;
	bool Preview = false;
	u64 Seed;
	float DirectionX = 1, DirectionZ = 0, Envelope = 0, Phase = 0;
};
class Simulation
{
public:
	float Parameters[CDetailWind::ParameterCount] = {};
	float LayerEnabled[CDetailWind::LayerCount] = {};
	double Offsets[CDetailWind::LayerCount][4] = {};
	float MaterialHeading[CDetailWind::LayerCount] = {};
	float LayerBasis[CDetailWind::LayerCount][4] = {};
	float FlowX = 1, FlowZ = 0;
	u32 PlantCount = 0;
	float StepMs = 16;
	SpringMatrix ResponseXY[16];
	SpringMatrix ResponseY[16];
	struct IntervalResponse
	{
		u32 Milliseconds;
		SpringMatrix XY;
		SpringMatrix Y;
	};
	xr_vector<IntervalResponse> IntervalResponses[16];
	double Time = 0;
	float Heading = 0, WanderStart = 0, WanderTarget = 0, WanderAge = 0, WanderDuration = 1;
	float PreviousHeading = 0, PreviousAnchor = .2f;
	float SeedBlend = 1;
	u32 Seed = 12345, PreviousSeed = 12345, Tick = 0, PreviousTick = 0;
	u64 RandomState = 12345;
	bool Initialized = false;
	bool Enabled = false;
	bool PendingGust = false;
	int PendingBehavior = -1;
	float BehaviorHazard[CDetailWind::BehaviorCount] = {};
	float BehaviorThreshold[CDetailWind::BehaviorCount] = {};
	float BehaviorValues[CDetailWind::BehaviorCount][3] = {};
	float BehaviorGain[CDetailWind::BehaviorCount] = {};
	double LastGust = -1000;
	xr_vector<Gust> Gusts;
	xr_hash_map<Cell, Vec3> FieldCache;
	struct PendingSample
	{
		Cell Coordinates;
		Vec3* Value;
	};
	xr_vector<PendingSample> PendingSamples;
	xr_unique_ptr<XRayWorkerThread> FieldWorkers[3];
	u32 PrefetchWorkers = 1;
	float PrefetchSpacing = 1;

	void EvaluatePendingRange(u32 Worker)
	{
		const size_t Begin = PendingSamples.size() * Worker / PrefetchWorkers;
		const size_t End = PendingSamples.size() * (Worker + 1) / PrefetchWorkers;
		for (size_t Index = Begin; Index < End; ++Index)
		{
			const auto& Sample = PendingSamples[Index];
			const Cell& Key = Sample.Coordinates;
			*Sample.Value = Evaluate({Key.X * PrefetchSpacing, Key.Y * PrefetchSpacing, Key.Z * PrefetchSpacing});
		}
	}
	struct CachedCell
	{
		Cell Coordinates{};
		u32 Generation = 0;
		Vec3 Corners[8]{};
	};
	CachedCell NearbyCells[64];
	u32 FieldGeneration = 0;


	const IntervalResponse& ResponseFor(u32 Milliseconds, int Variant)
	{
		auto& Entries = IntervalResponses[Variant];
		if (Entries.empty() || Entries.back().Milliseconds != Milliseconds)
		{
			const auto Found = std::find_if(Entries.begin(), Entries.end(), [Milliseconds](const auto& Entry)
			{
				return Entry.Milliseconds == Milliseconds;
			});
			if (Found != Entries.end())
			{
				std::iter_swap(Found, std::prev(Entries.end()));
			}
			else
			{
				const float Frequency = Parameters[CDetailWind::Frequency] * (1 + (Variant / 15.0f * 2 - 1) * Parameters[CDetailWind::Variation]);
				const float DampingValue = Parameters[CDetailWind::Damping];
				Entries.push_back({Milliseconds, SpringTransition(Frequency, DampingValue, Milliseconds * .001f),
					SpringTransition(Frequency * 1.3f, DampingValue, Milliseconds * .001f)});
			}
		}
		return Entries.back();
	}

	NoiseSample Sample(double X, double Y, double Z, double T, u64 Salt) const
	{
		NoiseSample Value = Noise(X, Y, Z, T, u64(Seed) + Salt);
		if (SeedBlend < 1)
		{
			const NoiseSample Old = Noise(X, Y, Z, T, u64(PreviousSeed) + Salt);
			const float Blend = Smooth(SeedBlend);
			Value.Value = Old.Value + (Value.Value - Old.Value) * Blend;
			Value.Gradient = Old.Gradient + (Value.Gradient - Old.Gradient) * Blend;
		}
		return Value;
	}

	Vec3 Evaluate(Vec3 Position) const
	{
		using W = CDetailWind;
		Vec3 Result;
		float Lull = 1;
		for (int Layer = 0; Layer < W::LayerCount; ++Layer)
		{
			const float* P = Parameters + W::GlobalCount + Layer * W::LayerStride;
			if (LayerEnabled[Layer] < .0001f || (P[W::Xz] + P[W::Y]) < .0001f)
			{
				continue;
			}
			const float Dx = LayerBasis[Layer][0], Dz = LayerBasis[Layer][1];
			const float Bx = LayerBasis[Layer][2], Bz = LayerBasis[Layer][3];
			const double Px = Position.X - Offsets[Layer][0], Py = Position.Y - Offsets[Layer][1], Pz = Position.Z - Offsets[Layer][2];
			double X = (Px * Bx + Pz * Bz) / P[W::Length], Y = Py / P[W::Height], Z = (-Px * Bz + Pz * Bx) / P[W::Width];
			const double T = Offsets[Layer][3];
			const u64 Salt = u64(Layer + 1) * 104729;
			// Domain deformation evolves trajectories as well as the spatial pattern.
			const auto WarpNoise = Sample(X * .43 + 19, Y * .43 - 3, Z * .43 + 7, T * .7, Salt + 1);
			X += P[W::Warp] * WarpNoise.Value;
			Y += P[W::Warp] * WarpNoise.Gradient.Z * .12;
			Z += P[W::Warp] * WarpNoise.Gradient.X * .12;
			const auto A = Sample(X, Y, Z, T, Salt + 11);
			const auto B = Sample(X + 17.3, Y - 9.1, Z + 3.7, T, Salt + 31);
			const auto C = Sample(X - 7.2, Y + 13.4, Z - 11.6, T, Salt + 71);
			const Vec3 Curl = {C.Gradient.Y - B.Gradient.Z, A.Gradient.Z - C.Gradient.X, B.Gradient.X - A.Gradient.Y};
			const float Mask = std::pow(std::max(.001f, .5f + .5f * A.Value), P[W::Shape]) * 2;
			const float Density = std::max(0.0f, 1 + (Mask - 1) * P[W::Contrast]);
			const float CurlScale = P[W::Curl] * .16f, Gain = LayerEnabled[Layer];
			Result.X += Gain * P[W::Xz] * (Dx * Density + (Bx * Curl.X - Bz * Curl.Z) * CurlScale);
			Result.Z += Gain * P[W::Xz] * (Dz * Density + (Bz * Curl.X + Bx * Curl.Z) * CurlScale);
			Result.Y += Gain * P[W::Y] * (B.Value + Curl.Y * CurlScale);
		}
		for (const Gust& G : Gusts)
		{
			const float Dx = G.DirectionX, Dz = G.DirectionZ;
			const Vec3 D = Position - G.Position;
			const float Across = (-D.X * Dz + D.Z * Dx) / G.Width, Height = D.Y / G.Height;
			// Along is squared later, so these two coordinates alone can reject a point exactly.
			if (Across * Across + Height * Height > 9 || G.Influence == 0)
			{
				continue;
			}
			const float Mutate = Noise(D.X / std::max(2.0f, G.Length * .6f), D.Y / std::max(2.0f, G.Height * .5f), D.Z / std::max(2.0f, G.Width * .35f), Time * .12, G.Seed).Value;
			const float Along = (D.X * Dx + D.Z * Dz) / G.Length + Mutate * G.Warp;
			const float Radius = Along * Along + Across * Across + Height * Height;
			if (Radius > 9)
			{
				continue;
			}
			const float Envelope = G.Envelope;
			const float Amplitude = G.Strength * (G.Preview ? 1 : BehaviorGain[G.Behavior]) * Envelope * std::exp(-Radius * 2) * std::max(0.0f, 1 + Mutate * G.Warp);
			float Fx = Dx, Fz = Dz, Fy = G.Vertical * Mutate;
			const float Phase = G.Phase;
			if (G.Behavior == 3)
			{
				// Let circulation replace the background locally instead of hiding beneath it.
				const float Mix = std::clamp(Envelope * std::exp(-Radius) * G.Influence, 0.0f, 1.0f) * (G.Preview ? 1 : BehaviorGain[G.Behavior]);
				Result = Result * (1 - Mix);
				// A localized tangential velocity field, continuous at its centre.
				Fx = Dx * .2f - D.Z / G.Width * G.Spin * 2;
				Fz = Dz * .2f + D.X / G.Length * G.Spin * 2;
			}
			else if (G.Behavior == 5)
			{
				const float Fragment = .35f + .65f * Smooth(.5f + Mutate * 2);
				Fx *= Fragment;
				Fz *= Fragment;
			}
			else if (G.Behavior == 6)
			{
				const float Roll = std::sin(Phase + Along * 2);
				Fx *= .65f;
				Fz *= .65f;
				Fy += Roll;
			}
			if (G.Behavior == 7)
			{
				Lull *= std::max(0.0f, 1 - BehaviorGain[G.Behavior] * Envelope * std::exp(-Radius * 2) * G.Influence);
			}
			else
			{
				Result.X += Fx * Amplitude * G.Influence;
				Result.Z += Fz * Amplitude * G.Influence;
				Result.Y += Fy * Amplitude * G.Influence;
			}
		}
		return Result * (Parameters[W::Force] * Lull);
	}

	void Prefetch(const xr_vector<Fbox>& Bounds)
	{
		PROF_EVENT("Prefetch detail wind field");
		if (!Enabled || Bounds.size() < 32 || CPU::ID().n_cores < 4)
		{
			return;
		}
		const float Spacing = Parameters[CDetailWind::FieldSpacing];
		constexpr size_t MaximumSamples = 65536;
		PendingSamples.clear();
		for (const Fbox& Box : Bounds)
		{
			const s32 MinX = s32(std::floor(double(Box.min.x) / Spacing));
			const s32 MinY = s32(std::floor(double(Box.min.y) / Spacing));
			const s32 MinZ = s32(std::floor(double(Box.min.z) / Spacing));
			const s32 MaxX = s32(std::floor(double(Box.max.x) / Spacing)) + 1;
			const s32 MaxY = s32(std::floor(double(Box.max.y) / Spacing)) + 1;
			const s32 MaxZ = s32(std::floor(double(Box.max.z) / Spacing)) + 1;
			for (s32 Z = MinZ; Z <= MaxZ && PendingSamples.size() < MaximumSamples; ++Z)
			{
				for (s32 Y = MinY; Y <= MaxY && PendingSamples.size() < MaximumSamples; ++Y)
				{
					for (s32 X = MinX; X <= MaxX && PendingSamples.size() < MaximumSamples; ++X)
					{
						const Cell Key{X, Y, Z};
						auto [Entry, Inserted] = FieldCache.try_emplace(Key);
						if (Inserted)
						{
							// unordered_map keeps value addresses stable when buckets grow.
							PendingSamples.push_back({Key, &Entry->second});
						}
					}
				}
			}
			if (PendingSamples.size() >= MaximumSamples)
			{
				break;
			}
		}
		const u32 Cores = CPU::ID().n_cores;
		const u32 Workers = PendingSamples.size() < 256 ? 1 : std::min(4u, Cores > 2 ? Cores - 2 : 1u);
		PrefetchWorkers = Workers;
		PrefetchSpacing = Spacing;
		for (u32 Worker = 1; Worker < Workers; ++Worker)
		{
			auto& Thread = FieldWorkers[Worker - 1];
			if (!Thread)
			{
				Thread.reset(new XRayWorkerThread([this, Worker]() { EvaluatePendingRange(Worker); }, "Vegetation field"));
			}
			Thread->Run();
		}
		EvaluatePendingRange(0);
		for (u32 Worker = 1; Worker < Workers; ++Worker)
		{
			FieldWorkers[Worker - 1]->Wait();
		}
	}

	Vec3 Cached(Vec3 Position)
	{
		const float Spacing = Parameters[CDetailWind::FieldSpacing];
		const double X = Position.X / Spacing, Y = Position.Y / Spacing, Z = Position.Z / Spacing;
		const s32 Ix = s32(std::floor(X)), Iy = s32(std::floor(Y)), Iz = s32(std::floor(Z));
		const float Fx = Smooth(float(X - Ix)), Fy = Smooth(float(Y - Iy)), Fz = Smooth(float(Z - Iz));
		const Cell Coordinates{Ix, Iy, Iz};
		auto& Entry = NearbyCells[(Ix & 3) | ((Iy & 3) << 2) | ((Iz & 3) << 4)];
		if (Entry.Generation != FieldGeneration || !(Entry.Coordinates == Coordinates))
		{
			for (int Dz = 0; Dz < 2; ++Dz)
			{
				for (int Dy = 0; Dy < 2; ++Dy)
				{
					for (int Dx = 0; Dx < 2; ++Dx)
					{
						const Cell Key{Ix + Dx, Iy + Dy, Iz + Dz};
						auto It = FieldCache.find(Key);
						if (It == FieldCache.end())
						{
							It = FieldCache.emplace(Key, Evaluate({Key.X * Spacing, Key.Y * Spacing, Key.Z * Spacing})).first;
						}
						Entry.Corners[Dx | (Dy << 1) | (Dz << 2)] = It->second;
					}
				}
			}
			Entry.Coordinates = Coordinates;
			Entry.Generation = FieldGeneration;
		}
		Vec3 Result;
		for (int Dz = 0; Dz < 2; ++Dz)
		{
			for (int Dy = 0; Dy < 2; ++Dy)
			{
				for (int Dx = 0; Dx < 2; ++Dx)
				{
					const float Weight = (Dx ? Fx : 1 - Fx) * (Dy ? Fy : 1 - Fy) * (Dz ? Fz : 1 - Fz);
					Result = Result + Entry.Corners[Dx | (Dy << 1) | (Dz << 2)] * Weight;
				}
			}
		}
		return Result;
	}

	void Advance(float Dt, const Fvector& Camera, const CDetailWind::Settings& Settings, float Blend, float Direction, bool WindEnabled)
	{
		using W = CDetailWind;
		const u32 Now = RDEVICE.dwTimeGlobal;
		if (Initialized)
		{
			Dt = (Now - Tick) * .001f;
		}
		StepMs = std::max(1.0f, Dt * 1000);
		Dt = std::max(Dt, 0.0f);
		PreviousTick = Tick;
		Tick = Now;
		PlantCount = 0;
		if (++FieldGeneration == 0)
		{
			FieldGeneration = 1;
			for (auto& Entry : NearbyCells)
			{
				Entry.Generation = 0;
			}
		}
		FieldCache.clear();
		Enabled = WindEnabled;
		if (!Enabled)
		{
			return;
		}
		PreviousHeading = Heading;
		PreviousAnchor = Initialized ? Parameters[CDetailWind::Anchor] : .2f;
		Blend = std::clamp(Blend, 0.0f, 1.0f);
		const int Preview = g_PendingPreview.exchange(-2, std::memory_order_relaxed);
		if (Preview >= -1)
		{
			PendingGust = true;
			PendingBehavior = Preview;
		}
		const float Transition = Settings.Values[W::TransitionTime][0] + (Settings.Values[W::TransitionTime][1] - Settings.Values[W::TransitionTime][0]) * Blend;
		const float Alpha = 1 - std::exp(-Dt / Transition);
		if (!Initialized)
		{
			Seed = PreviousSeed = Settings.Seed;
			RandomState = Settings.Seed;
			Heading = Direction * Radians;
			for (int Index = 0; Index < W::BehaviorCount; ++Index)
			{
				BehaviorThreshold[Index] = -std::log(std::max(.000001f, DetailWindMath::Random(RandomState)));
			}
			FieldCache.reserve(8192);
		}
		for (int Index = 0; Index < W::ParameterCount; ++Index)
		{
			const float Target = Settings.Values[Index][0] + (Settings.Values[Index][1] - Settings.Values[Index][0]) * Blend;
			Parameters[Index] = Initialized ? Parameters[Index] + (Target - Parameters[Index]) * Alpha : Target;
		}
		for (int Layer = 0; Layer < W::LayerCount; ++Layer)
		{
			const float Target = Settings.Enabled[Layer] ? 1.0f : 0.0f;
			LayerEnabled[Layer] = Initialized ? LayerEnabled[Layer] + (Target - LayerEnabled[Layer]) * Alpha : Target;
			if (!Initialized)
			{
				// Material coordinates stay fixed: steering must not spin the entire field around the world origin.
				MaterialHeading[Layer] = Heading + Parameters[W::GlobalCount + Layer * W::LayerStride + W::Angle] * Radians;
			}
		}
		for (int Variant = 0; Variant < 16; ++Variant)
		{
			IntervalResponses[Variant].clear();
			const float Frequency = Parameters[W::Frequency] * (1 + (Variant / 15.0f * 2 - 1) * Parameters[W::Variation]);
			// Trampled plants leave Update before spring integration; only free response is used.
			const float Damping = Parameters[W::Damping];
			ResponseXY[Variant] = SpringTransition(Frequency, Damping, StepMs * .001f);
			ResponseY[Variant] = SpringTransition(Frequency * 1.3f, Damping, StepMs * .001f);
		}
		if (Settings.Seed != Seed && SeedBlend >= 1)
		{
			PreviousSeed = Seed;
			Seed = Settings.Seed;
			SeedBlend = 0;
			RandomState = Seed;
		}
		SeedBlend = std::min(1.0f, SeedBlend + Dt / Transition);
		Initialized = true;
		Time += Dt;
		WanderAge += Dt;
		if (WanderAge >= WanderDuration)
		{
			WanderStart = WanderTarget;
			WanderTarget = DetailWindMath::Random(RandomState) * 2 - 1;
			WanderAge = 0;
			WanderDuration = Parameters[W::DirectionTime] * (.65f + DetailWindMath::Random(RandomState) * .7f);
		}
		const float Wander = WanderStart + (WanderTarget - WanderStart) * Smooth(WanderAge / WanderDuration);
		const float TargetHeading = (Direction + Wander * Parameters[W::DirectionWander]) * Radians;
		Heading = std::remainder(Heading + std::remainder(TargetHeading - Heading, 6.2831853f) * Alpha, 6.2831853f);
		FlowX = std::cos(Heading);
		FlowZ = std::sin(Heading);
		for (int Layer = 0; Layer < W::LayerCount; ++Layer)
		{
			const float* P = Parameters + W::GlobalCount + Layer * W::LayerStride;
			LayerBasis[Layer][0] = std::cos(Heading + P[W::Angle] * Radians);
			LayerBasis[Layer][1] = std::sin(Heading + P[W::Angle] * Radians);
			LayerBasis[Layer][2] = std::cos(MaterialHeading[Layer]);
			LayerBasis[Layer][3] = std::sin(MaterialHeading[Layer]);
			const float Drift = Noise(Offsets[Layer][0] * .015, Offsets[Layer][1] * .015, Offsets[Layer][2] * .015, Time * .04, Seed + Layer).Value;
			const float Angle = Heading + (P[W::Angle] + Drift * P[W::Warp] * 35) * Radians;
			Offsets[Layer][0] += std::cos(Angle) * P[W::Speed] * Dt;
			Offsets[Layer][2] += std::sin(Angle) * P[W::Speed] * Dt;
			Offsets[Layer][1] += Drift * P[W::Y] * P[W::Speed] * Dt * .1;
			Offsets[Layer][3] += P[W::Evolution] * Dt;
		}
		for (Gust& G : Gusts)
		{
			G.Age += Dt;
			const float Drift = Noise(G.Position.X * .01, G.Position.Y * .01, G.Position.Z * .01, Time * .08, G.Seed).Value;
			if (G.Behavior == 2)
			{
				// Smooth half-arcs with random handedness and no closed repeating path.
				G.Heading = G.InitialHeading + G.Spin * 3.1415926f * Smooth(G.Age / G.Life);
			}
			else
			{
				G.Heading += Drift * G.Wander * Radians * Dt;
			}
			G.Position.X += std::cos(G.Heading) * G.Speed * Dt;
			G.Position.Z += std::sin(G.Heading) * G.Speed * Dt;
		}
		Gusts.erase(std::remove_if(Gusts.begin(), Gusts.end(), [](const Gust& G)
								   { return G.Age >= G.Life; }),
					Gusts.end());
		int Candidate = -1;
		float Overdue = 1;
		for (int Index = 0; Index < W::BehaviorCount; ++Index)
		{
			const auto& Behavior = Settings.Behaviors[Index];
			const float TargetGain = Behavior.Enabled ? 1.0f : 0.0f;
			BehaviorGain[Index] = Initialized ? BehaviorGain[Index] + (TargetGain - BehaviorGain[Index]) * Alpha : TargetGain;
			const float* Values[] = {Behavior.Influence, Behavior.Duration, Behavior.Interval};
			for (int Parameter = 0; Parameter < 3; ++Parameter)
			{
				const float Target = Values[Parameter][0] + (Values[Parameter][1] - Values[Parameter][0]) * Blend;
				BehaviorValues[Index][Parameter] = Initialized ? BehaviorValues[Index][Parameter] + (Target - BehaviorValues[Index][Parameter]) * Alpha : Target;
			}
			if (Behavior.Enabled)
			{
				BehaviorHazard[Index] += Dt / std::max(.5f, BehaviorValues[Index][2]);
				const float Ratio = BehaviorHazard[Index] / std::max(.00001f, BehaviorThreshold[Index]);
				if (Ratio >= Overdue)
				{
					Candidate = Index;
					Overdue = Ratio;
				}
			}
			else
			{
				BehaviorHazard[Index] = 0;
			}
		}
		if ((PendingGust || (Candidate >= 0 && Time - LastGust >= .25)) && Gusts.size() < 32)
		{
			const bool Preview = PendingGust;
			const bool SpecificPreview = Preview && PendingBehavior >= 0;
			PendingGust = false;
			const int BehaviorIndex = Preview ? (SpecificPreview ? PendingBehavior : 0) : Candidate;
			PendingBehavior = -1;
			if (!Preview)
			{
				BehaviorHazard[BehaviorIndex] = 0;
				BehaviorThreshold[BehaviorIndex] = -std::log(std::max(.000001f, DetailWindMath::Random(RandomState)));
			}
			LastGust = Time;
			const float Lateral = (DetailWindMath::Random(RandomState) * 2 - 1) * Parameters[W::GustWidth];
			Gust G;
			G.Heading = Heading + (DetailWindMath::Random(RandomState) * 2 - 1) * Parameters[W::DirectionWander] * Radians;
			G.Speed = Parameters[W::GustSpeed] * (.7f + DetailWindMath::Random(RandomState) * .6f);
			G.Life = Parameters[W::GustLife] * (.7f + DetailWindMath::Random(RandomState) * .6f);
			G.Length = Parameters[W::GustLength] * (.7f + DetailWindMath::Random(RandomState) * .6f);
			G.Width = Parameters[W::GustWidth] * (.7f + DetailWindMath::Random(RandomState) * .6f);
			G.Height = Parameters[W::GustHeight];
			G.Strength = Parameters[W::GustStrength] * (.5f + DetailWindMath::Random(RandomState));
			G.Vertical = Parameters[W::GustVertical];
			G.Warp = Parameters[W::GustWarp];
			G.Wander = Parameters[W::GustWander];
			G.Attack = Parameters[W::GustAttack];
			G.Release = Parameters[W::GustRelease];
			G.Age = 0;
			G.Seed = Hash(RandomState++);
			G.Behavior = BehaviorIndex;
			if (G.Behavior == 3)
			{
				G.Speed *= .2f;
			}
			G.Preview = Preview && !SpecificPreview;
			G.Influence = Preview && !SpecificPreview ? 1 : BehaviorValues[BehaviorIndex][0];
			G.Life = Preview ? G.Life : BehaviorValues[BehaviorIndex][1];
			G.Spin = DetailWindMath::Random(RandomState) < .5f ? -1.0f : 1.0f;
			if (BehaviorIndex == 1)
			{
				G.Heading += G.Spin * 1.5707963f;
			}
			else if (BehaviorIndex == 4)
			{
				G.Heading += 3.1415926f;
			}

			const float Ahead = G.Speed * G.Life * .35f;
			G.Position = {Camera.x - std::cos(G.Heading) * Ahead - std::sin(G.Heading) * Lateral, Camera.y - 1.5f, Camera.z - std::sin(G.Heading) * Ahead + std::cos(G.Heading) * Lateral};
			if (Preview)
			{
				auto PreviewValue = [&](int Index)
				{
					return Settings.Values[Index][0] + (Settings.Values[Index][1] - Settings.Values[Index][0]) * Blend;
				};
				// A preview must cross the visible foreground, not originate far outside it.
				const Fvector& View = RDEVICE.vCameraDirection;
				const float ViewLength = std::sqrt(View.x * View.x + View.z * View.z);
				const float ViewX = ViewLength > .01f ? View.x / ViewLength : std::cos(Heading);
				const float ViewZ = ViewLength > .01f ? View.z / ViewLength : std::sin(Heading);
				if (!SpecificPreview)
				{
					G.Heading = Heading;
				}
				G.Speed = PreviewValue(W::GustSpeed) * (G.Behavior == 3 ? .2f : 1.0f);
				G.Length = PreviewValue(W::GustLength);
				G.Width = PreviewValue(W::GustWidth);
				G.Life = SpecificPreview ? BehaviorValues[BehaviorIndex][1] : PreviewValue(W::GustLife);
				G.Height = PreviewValue(W::GustHeight);
				G.Strength = PreviewValue(W::GustStrength);
				G.Attack = PreviewValue(W::GustAttack);
				G.Release = PreviewValue(W::GustRelease);
				G.Vertical = PreviewValue(W::GustVertical);
				G.Warp = PreviewValue(W::GustWarp);
				G.Age = G.Life * G.Attack * .5f;
				const float Offset = G.Behavior == 3 ? 0 : G.Length * .35f;
				G.Position = {Camera.x + ViewX * 8 - std::cos(G.Heading) * Offset, Camera.y - 1.5f, Camera.z + ViewZ * 8 - std::sin(G.Heading) * Offset};
			}

			G.InitialHeading = G.Heading;
			Gusts.push_back(G);
		}
		for (Gust& G : Gusts)
		{
			G.DirectionX = std::cos(G.Heading);
			G.DirectionZ = std::sin(G.Heading);
			const float Age = G.Age / G.Life;
			G.Envelope = Smooth(Age / G.Attack) * Smooth((1 - Age) / G.Release);
			G.Phase = Age * 6.2831853f;
		}
		FieldCache.clear();
	}
};

static Simulation& Runtime()
{
	static Simulation Instance;
	return Instance;
}
} // namespace DetailWind

const char* CDetailWind::BehaviorName(int Index)
{
	static const char* Names[] = {"Направленный накат", "Боковой проход", "Плавная полудуга", "Локальный круговой вихрь", "Обратный порыв", "Рваный фронт", "Вертикальное перекатывание", "Затишье и отпускание"};
	return Index >= 0 && Index < BehaviorCount ? Names[Index] : "";
}

const xr_vector<CDetailWind::Definition>& CDetailWind::Definitions()
{
	static const xr_vector<Definition> Result = []()
	{
#include "DetailWindDefinitions.inl"
		xr_vector<Definition> Items(std::begin(GlobalDefinitions), std::end(GlobalDefinitions));
		for (int Layer = 0; Layer < LayerCount; ++Layer)
		{
			for (const auto& Source : LayerDefinitions)
			{
				Definition D = Source;
				D.Key = "layer" + xr_string(std::to_string(Layer + 1).c_str()) + "_" + Source.Key;
				D.Group = Layer + 1;
				Items.push_back(D);
			}
		}
		return Items;
	}();
	return Result;
}

CDetailWind::Settings::Settings()
{
	const auto& Parameters = Definitions();
	for (size_t Index = 0; Index < Parameters.size(); ++Index)
	{
		Values[Index][0] = Parameters[Index].AtZero;
		Values[Index][1] = Parameters[Index].AtOne;
	}
}

CDetailWind::Settings& CDetailWind::Editable()
{
	static Settings Instance;
	return Instance;
}

void CDetailWind::BeginFrame(float Dt, const Fvector& Camera, const Settings& Snapshot, float Blend, float Direction, bool Enabled)
{
	PROF_EVENT("DetailWind::BeginFrame");
	DetailWind::Runtime().Advance(Dt, Camera, Snapshot, Blend, Direction, Enabled);
	DetailWind::g_PublishedStepMs.store(DetailWind::Runtime().StepMs, std::memory_order_relaxed);
}

void CDetailWind::PrefetchField(const xr_vector<Fbox>& Bounds)
{
	DetailWind::Runtime().Prefetch(Bounds);
}

void CDetailWind::Update(const Fvector& Position, float Trample, float Height, Fvector4& Bend, Fvector4& Previous, Fvector4& Velocity, Fvector4& PreviousVelocity)
{
	using namespace DetailWindMath;
	auto& Sim = DetailWind::Runtime();
	if (!Sim.Enabled)
	{
		return;
	}
	// Stop wind forces immediately, retaining only the entry pose for the visual transition.
	if (Trample > .001f)
	{
		if (std::bit_cast<u32>(Velocity.w) != u32(-2))
		{
			Previous = Bend;
			PreviousVelocity.set(Sim.FlowX, Sim.FlowZ, Sim.Parameters[Anchor], 0);
			Velocity.set(0, 0, 0, std::bit_cast<float>(u32(-2)));
		}
		return;
	}
	const float* P = Sim.Parameters;
	u32 LastTick = std::bit_cast<u32>(Velocity.w);
	// The instance seed depends only on its root position. Cache it in the unused
	// previous-velocity w component instead of hashing/flooring every simulation step.
	const u32 Identity = LastTick == u32(-1) || LastTick == u32(-2)
		? u32(Hash(u64(s64(std::floor(Position.x * 127))) ^ (u64(s64(std::floor(Position.z * 127))) << 32)) & 65535)
		: u32(PreviousVelocity.w);
	if (LastTick == u32(-2))
	{
		// A recovered trail starts the inertial wind response from its resting pose.
		Bend.set(0, 0, 0, P[Profile]);
		Previous = Bend;
		LastTick = Sim.PreviousTick;
		Velocity.set(0, 0, 0, std::bit_cast<float>(LastTick));
		PreviousVelocity = Velocity;
	}
	const u32 Elapsed = Sim.Tick - LastTick;
	if (LastTick == Sim.Tick)
	{
		return;
	}
	++Sim.PlantCount;
	Previous = Bend;
	PreviousVelocity = Velocity;
	PreviousVelocity.w = float(Identity);
	const Vec3 ForceVector = Sim.Cached({Position.x, Position.y + .6f * Height, Position.z});
	const float Individual = 1 + (float(Identity & 65535) / 32767.5f - 1) * P[Variation];
	const float Limit = P[MaxAngle] * DetailWind::Radians;
	const Vec3 Target = LimitBendModes({ForceVector.X * P[Compliance] * Individual, ForceVector.Z * P[Compliance] * Individual, ForceVector.Y * P[VerticalResponse] * P[Compliance]}, Limit);
	if (LastTick == u32(-1) || (Elapsed > 2000 && LastTick != Sim.PreviousTick))
	{
		Bend.set(Target.X, Target.Y, Target.Z, P[Profile]);
		Velocity.set(0, 0, 0, std::bit_cast<float>(Sim.Tick));
		Previous = Bend;
		PreviousVelocity = Velocity;
		PreviousVelocity.w = float(Identity);
		return;
	}
	const int Variant = int(Identity & 15);
	SpringMatrix XY = Sim.ResponseXY[Variant], YMode = Sim.ResponseY[Variant];
	if (std::abs(float(Elapsed) - Sim.StepMs) > .5f)
	{
		// Only plants returning to visibility need a different integration interval.
		const auto& Response = Sim.ResponseFor(Elapsed, Variant);
		XY = Response.XY;
		YMode = Response.Y;
	}
	Spring(Bend.x, Velocity.x, Target.X, XY);
	Spring(Bend.y, Velocity.y, Target.Y, XY);
	Spring(Bend.z, Velocity.z, Target.Z, YMode);
	const float Amount = std::sqrt(Bend.x * Bend.x + Bend.y * Bend.y) + std::abs(Bend.z);
	if (Amount > Limit)
	{
		const float Scale = Limit / std::max(Amount, .00001f);
		Bend.x *= Scale;
		Bend.y *= Scale;
		Bend.z *= Scale;
		Velocity.x *= Scale;
		Velocity.y *= Scale;
		Velocity.z *= Scale;
	}
	Bend.w = P[Profile];
	Velocity.w = std::bit_cast<float>(Sim.Tick);
}

void CDetailWind::Clear()
{
	DetailWind::Runtime() = DetailWind::Simulation{};
	DetailWind::g_CompletedPlants.store(0);
	DetailWind::g_CompletedSamples.store(0);
	DetailWind::g_CompletedGusts.store(0);
	DetailWind::g_PendingPreview.store(-2);
	for (auto& Count : DetailWind::g_BehaviorCounts)
	{
		Count.store(0, std::memory_order_relaxed);
	}
}

void CDetailWind::ComputeConstants(Constants& Out)
{
	Out = {};
	const auto& Sim = DetailWind::Runtime();
	Out.global.set(Sim.Enabled ? 1.0f : 0.0f, Sim.Initialized ? Sim.Parameters[Anchor] : .2f, std::cos(Sim.Heading), std::sin(Sim.Heading));
	Out.xz2.set(0, 0, Sim.Initialized ? Sim.Parameters[MaxAngle] * DetailWind::Radians : 0, Sim.Initialized ? Sim.Parameters[TrampleResidual] : 0);
	Out.xz1.set(Sim.PreviousAnchor, std::cos(Sim.PreviousHeading), std::sin(Sim.PreviousHeading), 0);
}

u32 CDetailWind::ActiveGustCount()
{
	return DetailWind::g_CompletedGusts.load(std::memory_order_relaxed);
}

void CDetailWind::RequestGust()
{
	if (!ps_wind_enabled)
	{
		Msg("! Wind preview: enable procedural wind first");
		return;
	}
	DetailWind::g_PendingPreview.store(-1, std::memory_order_relaxed);
}

void CDetailWind::RequestBehavior(int Index)
{
	if (!ps_wind_enabled || Index < 0 || Index >= BehaviorCount)
	{
		return;
	}
	DetailWind::g_PendingPreview.store(Index, std::memory_order_relaxed);
}

u32 CDetailWind::ActiveBehaviorCount(int Index)
{
	return Index >= 0 && Index < BehaviorCount ? DetailWind::g_BehaviorCounts[Index].load(std::memory_order_relaxed) : 0;
}

void CDetailWind::FinishFrame()
{
	const auto& Sim = DetailWind::Runtime();

	DetailWind::g_CompletedPlants.store(Sim.PlantCount, std::memory_order_relaxed);
	DetailWind::g_CompletedGusts.store(u32(Sim.Gusts.size()), std::memory_order_relaxed);
	for (int Index = 0; Index < BehaviorCount; ++Index)
	{
		u32 Count = 0;
		for (const auto& Event : Sim.Gusts)
		{
			Count += Event.Behavior == Index && (Event.Preview || Sim.BehaviorGain[Index] > .01f);
		}
		DetailWind::g_BehaviorCounts[Index].store(Count, std::memory_order_relaxed);
	}
	DetailWind::g_CompletedSamples.store(u32(Sim.FieldCache.size()), std::memory_order_relaxed);

}

u32 CDetailWind::SimulatedPlants()
{
	return DetailWind::g_CompletedPlants.load(std::memory_order_relaxed);
}
u32 CDetailWind::FieldSamples()
{
	return DetailWind::g_CompletedSamples.load(std::memory_order_relaxed);
}

float CDetailWind::StepMilliseconds()
{
	return DetailWind::g_PublishedStepMs.load(std::memory_order_relaxed);
}
