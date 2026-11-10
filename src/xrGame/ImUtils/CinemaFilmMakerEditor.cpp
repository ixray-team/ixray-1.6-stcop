#include "StdAfx.h"

#include "Actor.h"
#include "ImUtils.h"
#include "../ParticlesObject.h"
#include "../GamePersistent.h"
#include "../../Include/xrRender/EnvironmentRender.h"
#include "../../Include/xrRender/particles_systems_library_interface.hpp"
#include "../HUDManager.h"
#include "../HUDTarget.h"
#include "../Level.h"
#include "../Level_Bullet_Manager.h"
#include "../WeaponAmmo.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../xrEngine/pure.h"
#include "../../xrEngine/Render.h"
#include "../../xrRHI/RHI.h"
#include "../../xrEngine/CameraManager.h"
#include "../../xrEngine/Effector.h"
#include "../../xrEngine/ImGuiManager.h"
#include "../../xrEngine/ParticleAnimCurveInterface.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrCore/Kernel/EngineExternal.h"
#include "imgui_internal.h"
#include <filesystem>
#include <type_traits>
#include "../Cinema/CinemaShake.h"

CBlend* PlayMotionByParts(IKinematicsAnimated* sa, MotionID motion_ID, bool bMixIn, PlayCallback Callback, void* CallbackParam);

namespace CinemaFilmMaker
{
constexpr const char* kCinemaClipPayload = "CINEMA_ANIMATION";
constexpr const char* kCinemaClipIndexPayload = "CINEMA_ANIMATION_INDEX";
constexpr const char* kCinemaSceneFolder = "cinema_scenes\\";
constexpr const char* kCinemaSceneExt = ".ltx";
constexpr s32 kCinemaSceneVersion = 4;
constexpr const char* kCinemaSettingsFile = "cinema_editor\\settings.ltx";
constexpr const char* kCinemaPresetsFile = "cinema_editor\\presets.ltx";
constexpr s32 kCinemaPresetVersion = 1;
constexpr s32 kCinemaMaxTracks = 65535;
constexpr u32 kCinemaMaxPresets = 65535;
constexpr u32 kCinemaSceneMaxActors = 65535;
constexpr u32 kCinemaSceneMaxPlaylist = 65535;
constexpr u32 kCinemaMaxShots = 256;
constexpr u32 kCinemaMaxSegments = 128;

bool IsCinemaParticle(const shared_str& name)
{
	const auto& names = GamePersistent().Environment().m_pRender->particles_systems_library().vec_all_particles();
	auto* curves = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	return std::find(names.begin(), names.end(), name) != names.end() && (!curves || !curves->FindIPAC(name.c_str()));
}

enum class EGizmoMode : u8
{
	Translate,
	Rotate
};

enum class EGizmoAxis : u8
{
	None,
	X,
	Y,
	Z
};

struct SAnimationClip
{
	shared_str Name;
	bool Fx = false;
};

struct SAnimationSource
{
	shared_str Id;
	u32 BoneCount = 0;
	xr_vector<shared_str> BoneNames;
	xr_vector<SAnimationClip> Clips;
};

struct SPlaylistEntry
{
	shared_str Source;
	shared_str Name;
	bool Fx = false;
	bool Muted = false;
};

struct STrimSegment
{
	float Start = 0.0f;
	float End = 0.0f;
};

struct STrimRange
{
	shared_str Source;
	shared_str Name;
	xr_vector<STrimSegment> Segments;
	xr_vector<float> Shots;
	float Speed = 1.0f;
	float Blend = 0.0f;
};
struct SHandItem
{
	shared_str Section;
	shared_str Visual;
};

struct SNpcPreset
{
	xr_string Name;
	shared_str Visual;
	shared_str Item;
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f); 
	bool Loop = false;
	float Speed = 1.0f;
	xr_vector<SPlaylistEntry> Playlist;
	xr_vector<STrimRange> Trims;
	bool PingPong = false;
};

struct SMasterClip
{
	shared_str DisplayName;
	u32 DisplayColor = 0;
	s32 Track = 0;
	bool Muted = false;
	shared_str Source;
	shared_str Name;
	float Start = 0.0f;
	float Duration = 1.0f;
	float SourceIn = 0.0f;
	float SourceOut = 0.0f;
	float Speed = 1.0f;
	xr_vector<float> Shots;
};

static constexpr float CinemaMasterSourceTime(float sourceIn, float speed, float elapsed)
{
	return sourceIn + speed * elapsed;
}

static_assert(CinemaMasterSourceTime(1.0f, 2.0f, 0.5f) == 2.0f);
static_assert(CinemaMasterSourceTime(0.5f, 0.5f, 1.0f) == 1.0f);

static constexpr float CinemaMasterClipTime(float sourceIn, float speed, float sourceTime)
{
	return (sourceTime - sourceIn) / speed;
}

static_assert(CinemaMasterClipTime(1.0f, 2.0f, 2.0f) == 0.5f);
static_assert(CinemaMasterClipTime(0.5f, 0.5f, 1.0f) == 1.0f);

struct SCinemaCameraKey
{
	float Time = 0.0f;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation = Fmatrix().identity();
};

using SCinemaShakeSample = Cinema::ShakeSample;
using SCinemaShakePreset = Cinema::ShakePreset;
static constexpr const auto& CinemaShakePresets = Cinema::ShakePresets;

struct SParticleCurveKey
{
	float Time = 0.0f;
	float Value = 0.0f;
	float InSlope = 0.0f;
	float OutSlope = 0.0f;
	bool Smooth = false;

	bool operator==(const SParticleCurveKey&) const = default;
};

static float ParticleCurveValue(const xr_vector<SParticleCurveKey>& keys, float time, float fallback);
static double CinemaShakeIntegratedSpeed(const xr_vector<SParticleCurveKey>& keys, float time);

static float CinemaStepCurveValue(const xr_vector<SParticleCurveKey>& keys, float time, float fallback)
{
	if (keys.empty()) return fallback;
	const auto next = std::upper_bound(keys.begin(), keys.end(), time, [](float value, const SParticleCurveKey& key) { return value < key.Time; });
	return (next == keys.begin() ? keys.front() : *(next - 1)).Value;
}

struct SCinemaCameraShake
{
	bool Enabled = true;
	s32 Preset = 0;
	float Influence = 1.0f;
	float PositionScale = 1.0f;
	float Speed = 1.0f;
	float Offset = 0.0f;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;
	bool Stretch = false;
	xr_vector<SParticleCurveKey> Curves[9];
};

struct SCinemaCameraClip
{
	shared_str DisplayName;
	u32 DisplayColor = 0;
	bool Preview = false;
	float Fov = 0.0f;
	s32 Track = 0;
	bool Muted = false;
	float Start = 0.0f;
	float Duration = 1.0f;
	float SourceDuration = 1.0f;
	bool Stretch = true;
	bool Smooth = true;
	xr_vector<SParticleCurveKey> Curves[9];
	xr_vector<SCinemaCameraKey> Keys;
	xr_vector<SCinemaCameraShake> Shakes;
};

static bool CinemaCameraStretchAt(const SCinemaCameraClip& camera, float time)
{
	return CinemaStepCurveValue(camera.Curves[7], time, camera.Stretch ? 1.0f : 0.0f) >= 0.5f;
}

static float CinemaCameraKeyLocalTime(const SCinemaCameraClip& camera, const SCinemaCameraKey& key, float currentTime)
{
	const float original = key.Time;
	const float stretched = key.Time * camera.Duration / camera.SourceDuration;
	const bool originalValid = original <= camera.Duration && !CinemaCameraStretchAt(camera, original);
	const bool stretchedValid = stretched <= camera.Duration && CinemaCameraStretchAt(camera, stretched);
	if (originalValid && stretchedValid) return fabsf(currentTime - original) <= fabsf(currentTime - stretched) ? original : stretched;
	if (originalValid) return original;
	if (stretchedValid) return stretched;
	return CinemaCameraStretchAt(camera, currentTime) ? stretched : original;
}

template <typename T>
static bool CinemaMasterClipEnabled(const T& clip, const bool* mutedTracks)
{
	return !clip.Muted && clip.Track >= 0 && clip.Track < kCinemaMaxTracks && !mutedTracks[clip.Track];
}

static constexpr double CinemaBezierValue(double a, double b, double c, double d, double t)
{
	const double left = 1.0 - t;
	return left * left * left * a + 3.0 * left * left * t * b + 3.0 * left * t * t * c + t * t * t * d;
}

static_assert(CinemaBezierValue(1.0, 2.0, 3.0, 4.0, 0.0) == 1.0);
static_assert(CinemaBezierValue(1.0, 2.0, 3.0, 4.0, 1.0) == 4.0);
static_assert(CinemaBezierValue(1.0, 2.0, 3.0, 4.0, 0.5) == 2.5);

struct SCinemaCameraSpline
{
	struct SPoint
	{
		float Time = 0.0f;
		double Value[7] = {};
	};

	xr_vector<SPoint> Points;
	bool Smooth = false;

	explicit SCinemaCameraSpline(const SCinemaCameraClip& camera) : Smooth(camera.Smooth)
	{
		xr_vector<const SCinemaCameraKey*> keys;
		keys.reserve(camera.Keys.size());
		for (const auto& key : camera.Keys) keys.push_back(&key);
		std::stable_sort(keys.begin(), keys.end(), [](const auto* a, const auto* b) { return a->Time < b->Time; });
		Points.reserve(keys.size());
		for (const auto* key : keys)
		{
			if (!Points.empty() && key->Time <= Points.back().Time) continue;
			Fquaternion q = Fquaternion().set(key->Orientation).normalize();
			if (!Points.empty())
			{
				const auto& last = Points.back();
				if (last.Value[3] * q.x + last.Value[4] * q.y + last.Value[5] * q.z + last.Value[6] * q.w < 0.0)
				{
					q.x = -q.x; q.y = -q.y; q.z = -q.z; q.w = -q.w;
				}
			}
			SPoint point;
			point.Time = key->Time;
			point.Value[0] = key->Position.x; point.Value[1] = key->Position.y; point.Value[2] = key->Position.z;
			point.Value[3] = q.x; point.Value[4] = q.y; point.Value[5] = q.z; point.Value[6] = q.w;
			Points.push_back(point);
		}
	}

	void Evaluate(float time, Fvector& position, Fmatrix* orientation = nullptr) const
	{
		if (Points.empty()) return;
		auto upper = std::upper_bound(Points.begin(), Points.end(), time, [](float value, const SPoint& point) { return value < point.Time; });
		const size_t after = upper == Points.end() ? Points.size() - 1 : static_cast<size_t>(upper - Points.begin());
		const size_t before = after > 0 && time < Points[after].Time ? after - 1 : after;
		const SPoint& a = Points[before];
		const SPoint& b = Points[after];
		const double span = static_cast<double>(b.Time) - a.Time;
		const double t = span > 0.0 ? std::clamp((static_cast<double>(time) - a.Time) / span, 0.0, 1.0) : 0.0;
		const double left = 1.0 - t;
		double values[7];
		for (u32 component = 0; component < 7; ++component)
		{
			values[component] = left * a.Value[component] + t * b.Value[component];
			if (Smooth && span > 0.0)
			{
				const SPoint& previous = Points[before > 0 ? before - 1 : before];
				const SPoint& next = Points[after + 1 < Points.size() ? after + 1 : after];
				const double inSlope = (b.Value[component] - previous.Value[component]) / (static_cast<double>(b.Time) - previous.Time);
				const double outSlope = (next.Value[component] - a.Value[component]) / (static_cast<double>(next.Time) - a.Time);
				values[component] = CinemaBezierValue(a.Value[component], a.Value[component] + inSlope * span / 3.0,
					b.Value[component] - outSlope * span / 3.0, b.Value[component], t);
			}
		}
		position.set(static_cast<float>(values[0]), static_cast<float>(values[1]), static_cast<float>(values[2]));
		if (!orientation) return;
		Fquaternion q;
		q.set(static_cast<float>(values[6]), static_cast<float>(values[3]), static_cast<float>(values[4]), static_cast<float>(values[5]));
		if (!Smooth || Points.size() < 3 || q.magnitude() < EPS_S)
		{
			Fquaternion qa, qb;
			qa.set(static_cast<float>(a.Value[6]), static_cast<float>(a.Value[3]), static_cast<float>(a.Value[4]), static_cast<float>(a.Value[5]));
			qb.set(static_cast<float>(b.Value[6]), static_cast<float>(b.Value[3]), static_cast<float>(b.Value[4]), static_cast<float>(b.Value[5]));
			q.slerp(qa, qb, static_cast<float>(t));
		}
		orientation->rotation(q.normalize());
	}
};

static float CinemaShakeSample(const SCinemaShakePreset& preset, double frame, u32 channel)
{
	const double count = static_cast<double>(preset.Count);
	frame = std::fmod(frame, count);
	if (frame < 0.0) frame += count;
	const double whole = std::floor(frame);
	const u32 i = static_cast<u32>(whole) % preset.Count;
	const float t = static_cast<float>(frame - whole);
	const float a = preset.Samples[(i + preset.Count - 1) % preset.Count].Values[channel];
	const float b = preset.Samples[i].Values[channel];
	const float c = preset.Samples[(i + 1) % preset.Count].Values[channel];
	const float d = preset.Samples[(i + 2) % preset.Count].Values[channel];
	return b + 0.5f * t * (c - a + t * (2.0f * a - 5.0f * b + 4.0f * c - d + t * (3.0f * (b - c) + d - a)));
}

static void ApplyCinemaCameraShakes(const SCinemaCameraClip& camera, float time, Fvector& position, Fmatrix& orientation)
{
	for (const auto& shake : camera.Shakes)
	{
		const bool enabled = CinemaStepCurveValue(shake.Curves[4], time, shake.Enabled ? 1.0f : 0.0f) >= 0.5f;
		const s32 presetIndex = std::clamp(static_cast<s32>(roundf(CinemaStepCurveValue(shake.Curves[5], time, static_cast<float>(shake.Preset)))), 0, static_cast<s32>(IM_ARRAYSIZE(CinemaShakePresets)) - 1);
		const bool stretch = CinemaStepCurveValue(shake.Curves[6], time, shake.Stretch ? 1.0f : 0.0f) >= 0.5f;
		const float fadeIn = std::clamp(ParticleCurveValue(shake.Curves[7], time, shake.FadeIn), 0.0f, 3600.0f);
		const float fadeOut = std::clamp(ParticleCurveValue(shake.Curves[8], time, shake.FadeOut), 0.0f, 3600.0f);
		if (!enabled || shake.Influence <= 0.0f) continue;
		const auto& preset = CinemaShakePresets[presetIndex];
		float influence = shake.Influence * std::clamp(ParticleCurveValue(shake.Curves[0], time, 1.0f), 0.0f, 10.0f);
		if (fadeIn > 0.0f) influence *= std::clamp(time / fadeIn, 0.0f, 1.0f);
		if (fadeOut > 0.0f) influence *= std::clamp((camera.Duration - time) / fadeOut, 0.0f, 1.0f);
		const double elapsed = CinemaShakeIntegratedSpeed(shake.Curves[2], time) * shake.Speed;
		const double offset = shake.Offset + std::clamp(ParticleCurveValue(shake.Curves[3], time, 0.0f), -3600.0f, 3600.0f);
		const double frame = stretch ? elapsed / camera.Duration * preset.Count + offset * preset.FramesPerSecond :
			(elapsed + offset) * preset.FramesPerSecond;
		float value[6];
		for (u32 channel = 0; channel < 6; ++channel) value[channel] = CinemaShakeSample(preset, frame, channel) * influence;
		// Blender cameras face -Z; the engine camera faces +Z.
		Fvector localOffset, worldOffset;
		localOffset.set(value[0], value[1], -value[2]).mul(shake.PositionScale * std::clamp(ParticleCurveValue(shake.Curves[1], time, 1.0f), 0.0f, 10.0f));
		orientation.transform_dir(worldOffset, localOffset);
		position.add(worldOffset);
		Fmatrix x, y, z, xy, rotation, result;
		x.rotateX(-value[3]);
		y.rotateY(-value[4]);
		z.rotateZ(value[5]);
		xy.mul(y, x);
		rotation.mul(z, xy);
		result.mul(orientation, rotation);
		orientation = result;
	}
}

struct SCinemaCameraState
{
	bool Active = true;
	bool Attached = true;
	bool DemoType = false;
	float Fov = 0.0f;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation = Fmatrix().identity();
};

class CCinemaCameraEffector final : public CEffectorCam
{
	xr_shared_ptr<SCinemaCameraState> State;
public:
	CCinemaCameraEffector(ECamEffectorType type, xr_shared_ptr<SCinemaCameraState> state) : CEffectorCam(type, FLT_MAX), State(std::move(state)) {}
	~CCinemaCameraEffector() override { State->Attached = false; }
	bool AbsolutePositioning() override { return true; }
	bool ProcessCam(SCamEffectorInfo& info) override
	{
		if (!State->Active) return false;
		info.p = State->Position;
		info.d = State->Orientation.k;
		info.n = State->Orientation.j;
		if (State->Fov > 0.0f) info.fFov = State->Fov;
		return true;
	}
};

struct SMasterPlaylistSlice
{
	const SAnimationSource* Source = nullptr;
	const SAnimationClip* Animation = nullptr;
	const STrimRange* Trim = nullptr;
	float Start = 0.0f;
	float Duration = 0.0f;
	float SourceStart = 0.0f;
	float SourceEnd = 0.0f;
	s32 PlaylistIndex = -1;
};

struct SMasterClipSelection
{
	s32 Type = 0;
	s32 Object = -1;
	s32 Clip = -1;

	bool operator==(const SMasterClipSelection&) const = default;
};

struct SMasterClipProperties
{
	float* Start = nullptr;
	float* Duration = nullptr;
	s32* Track = nullptr;
	bool* Muted = nullptr;
	shared_str* DisplayName = nullptr;
};

struct SMasterGizmoSnapshot
{
	SMasterClipSelection Object;
	Fvector Position;
	Fmatrix Orientation;
};

struct SMasterTransformKey
{
	float Time = 0.0f;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);
};

struct SSceneActorData
{
	xr_string Name;
	shared_str Visual;
	xr_vector<shared_str> AnimationPacks;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);
	bool Loop = false;
	float Speed = 1.0f;
	shared_str Item;
	xr_vector<SPlaylistEntry> Playlist;
	xr_vector<STrimRange> Trims;
	bool PingPong = false;
	shared_str PoseSource;
	shared_str PoseClip;
	bool PoseFx = false;
	float PoseTime = 0.0f;
	bool PosePaused = false;
	bool PatternOwned = false;
	bool MasterTrack = false;
	xr_vector<SMasterClip> MasterClips;
	xr_vector<SMasterTransformKey> MasterTransforms;
};

struct SParticleWave
{
	s32 Type = 0;
	float Amplitude = 0.0f;
	float Frequency = 1.0f;
	float Phase = 0.0f;
	float Minimum = -3600.0f;
	float Maximum = 3600.0f;
};

class CCinemaParticleObject final : public CParticlesObject
{
public:
	CCinemaParticleObject(const char* name) : CParticlesObject(name, false, true) {}

	void SetSpeed(float speed) { Speed.store(speed); }
	void Update(u32 dt, CFrustum* viewbase) override
	{
		const double scaled = std::min(static_cast<double>(dt) * Speed.load() + Remainder, static_cast<double>(u32(-1)));
		const u32 elapsed = static_cast<u32>(scaled);
		Remainder = scaled - elapsed;
		CParticlesObject::Update(elapsed, viewbase);
	}

private:
	xr_atomic_float Speed = 1.0f;
	double Remainder = 0.0;
};

static xr_shared_ptr<CParticlesObject> CreateCinemaParticle(const char* name)
{
	auto particle = xr_make_shared<CCinemaParticleObject>(name);
	GamePersistent().ps_active_deffer.push_back(particle);
	return particle;
}

constexpr u32 kCinemaLightChannels = 15;

constexpr float CinemaLightChannelDefault(u32 channel)
{
	return channel < 6 ? 0.0f : 1.0f;
}

static_assert(CinemaLightChannelDefault(0) == 0.0f);
static_assert(CinemaLightChannelDefault(5) == 0.0f);
static_assert(CinemaLightChannelDefault(6) == 1.0f);
static_assert(CinemaLightChannelDefault(14) == 1.0f);

struct SMasterLightClip
{
	shared_str DisplayName;
	u32 DisplayColor = 0;
	s32 Track = 0;
	bool Muted = false;
	float Start = 0.0f;
	float Duration = 1.0f;
	float StartIntensity = 1.0f;
	float EndIntensity = 1.0f;
	Fvector StartColor = Fvector().set(1.0f, 1.0f, 1.0f);
	Fvector EndColor = Fvector().set(1.0f, 1.0f, 1.0f);
};

struct SCinemaLightData
{
	s32 Group = -1;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation = Fmatrix().identity();
	Fvector Color = Fvector().set(1.0f, 1.0f, 1.0f);
	float Intensity = 1.0f;
	float Range = 8.0f;
	float Cone = 60.0f;
	float VirtualSize = 0.1f;
	float VolumeQuality = 1.0f;
	float VolumeIntensity = 1.0f;
	float VolumeDistance = 1.0f;
	s32 Type = 0;
	bool Active = true;
	bool PatternOwned = false;
	bool MasterTrack = false;
	xr_vector<SMasterLightClip> MasterClips;
	xr_vector<SMasterTransformKey> MasterTransforms;
	bool Shadow = false;
	bool Volumetric = false;
	bool Indirect = false;
	bool HudMode = false;
	bool Occlusion = true;
	char Texture[240] = {};
	bool Modulation = true;
	bool Loop = true;
	bool Running = true;
	float Duration = 10.0f;
	float Time = 0.0f;
	xr_vector<SParticleCurveKey> Curves[kCinemaLightChannels];
	SParticleWave Waves[kCinemaLightChannels];
	s32 CurveChannel = 0;
	s32 CurveDrag = -1;

	SCinemaLightData()
	{
		for (u32 i = 6; i < kCinemaLightChannels; ++i)
		{
			Waves[i].Minimum = 0.0f;
			Waves[i].Maximum = 10.0f;
		}
	}
};

struct SCinemaLight : SCinemaLightData
{
	ref_light Light;
	bool MasterOwned = false;
	float MasterSavedTime = 0.0f;
	bool MasterSavedRunning = false;
	float MasterIntensity = 1.0f;
	Fvector MasterColor = Fvector().set(1.0f, 1.0f, 1.0f);

	SCinemaLight() = default;
	SCinemaLight(const SCinemaLight&) = delete;
	SCinemaLight& operator=(const SCinemaLight&) = delete;

	SCinemaLight(SCinemaLight&& other) noexcept : SCinemaLightData(std::move(other))
	{
		Light.swap(other.Light);
		MasterOwned = other.MasterOwned;
		MasterSavedTime = other.MasterSavedTime;
		MasterSavedRunning = other.MasterSavedRunning;
		MasterIntensity = other.MasterIntensity;
		MasterColor = other.MasterColor;
	}

	SCinemaLight& operator=(SCinemaLight&& other) noexcept
	{
		if (this != &other)
		{
			Light.destroy();
			SCinemaLightData::operator=(std::move(other));
			Light.swap(other.Light);
			MasterOwned = other.MasterOwned;
			MasterSavedTime = other.MasterSavedTime;
			MasterSavedRunning = other.MasterSavedRunning;
			MasterIntensity = other.MasterIntensity;
			MasterColor = other.MasterColor;
		}
		return *this;
	}

	~SCinemaLight()
	{
		Light.destroy();
	}
};

static_assert(!std::is_copy_constructible_v<SCinemaLight>);
static_assert(!std::is_copy_assignable_v<SCinemaLight>);
static_assert(std::is_nothrow_move_constructible_v<SCinemaLight>);
static_assert(std::is_nothrow_move_assignable_v<SCinemaLight>);

static void UpdateCinemaLightValues(SCinemaLight& light);

static void ApplyCinemaLight(SCinemaLight& light, bool texture = false)
{
	if (!light.Light)
	{
		return;
	}
	light.Light->set_type(light.Type == 0 ? IRender_Light::POINT : IRender_Light::SPOT);
	light.Light->set_shadow(light.Shadow);
	light.Light->set_volumetric(light.Volumetric);
	light.Light->set_indirect(light.Indirect);
	UpdateCinemaLightValues(light);
	light.Light->set_hud_mode(light.HudMode);
	light.Light->set_occq_mode(light.Occlusion);
	if (texture)
	{
		light.Light->set_texture(light.Texture);
	}
	light.Light->set_active(light.Active);
}

struct SCinemaParticleGroup
{
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation = Fmatrix().identity();
	float Scale = 1.0f;
	bool MasterTrack = false;
	xr_vector<SMasterTransformKey> MasterTransforms;
};

constexpr bool CinemaSelectionInRange(size_t index, size_t first, size_t last)
{
	return index >= std::min(first, last) && index <= std::max(first, last);
}

static_assert(CinemaSelectionInRange(2, 1, 3));
static_assert(CinemaSelectionInRange(2, 3, 1));
static_assert(CinemaSelectionInRange(1, 1, 3));
static_assert(CinemaSelectionInRange(3, 1, 3));
static_assert(!CinemaSelectionInRange(0, 3, 1));

struct SMasterParticleClip
{
	shared_str DisplayName;
	u32 DisplayColor = 0;
	s32 Track = 0;
	bool Muted = false;
	float Start = 0.0f;
	float Duration = 1.0f;
};

struct SCinemaParticle
{
	shared_str Name;
	s32 Group = -1;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation = Fmatrix().identity();
	float Scale = 1.0f;
	float Speed = 1.0f;
	xr_shared_ptr<CParticlesObject> Object;
	bool Timeline = true;
	bool PatternOwned = false;
	bool PatternPreview = false;
	bool PreviewOnSpawn = false;
	bool PreviewSavedPlaying = false;
	float PreviewSavedTime = 0.0f;
	bool MasterTrack = false;
	xr_vector<SMasterParticleClip> MasterClips;
	xr_vector<SMasterTransformKey> MasterTransforms;
	bool MasterOwned = false;
	bool MasterSavedRunning = false;
	bool MasterSavedStarted = false;
	bool MasterSavedPlaying = false;
	float MasterSavedTime = 0.0f;
	float MasterPreviousTime = -1.0f;
	s32 MasterPlayingClip = -1;
	bool Loop = true;
	float In = 0.0f;
	float Out = 10.0f;
	float Time = 0.0f;
	bool Running = false;
	bool Started = false;
	bool Modulation = true;
	Fvector EndOffset = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector EndRotation = Fvector().set(0.0f, 0.0f, 0.0f);
	float EndScale = 1.0f;
	xr_vector<SParticleCurveKey> Curves[8];
	SParticleWave Waves[8];
	s32 CurveChannel = 0;
	s32 CurveDrag = -1;
	SCinemaParticle()
	{
		Waves[6].Minimum = 0.1f;
		Waves[6].Maximum = 5.0f;
		Waves[7].Minimum = 0.0f;
		Waves[7].Maximum = 4.0f;
	}
};

constexpr float ParticleCurveLerp(float a, float b, float factor)
{
	return a + (b - a) * factor;
}

static_assert(ParticleCurveLerp(2.0f, 6.0f, 0.0f) == 2.0f);
static_assert(ParticleCurveLerp(2.0f, 6.0f, 0.5f) == 4.0f);
static_assert(ParticleCurveLerp(2.0f, 6.0f, 1.0f) == 6.0f);

static constexpr float CinemaCurveHermite(float a, float b, float outSlope, float inSlope, float duration, float t)
{
	const float t2 = t * t;
	const float t3 = t2 * t;
	return (2.0f * t3 - 3.0f * t2 + 1.0f) * a + (t3 - 2.0f * t2 + t) * duration * outSlope +
		(-2.0f * t3 + 3.0f * t2) * b + (t3 - t2) * duration * inSlope;
}

static_assert(CinemaCurveHermite(2.0f, 6.0f, 0.0f, 0.0f, 1.0f, 0.0f) == 2.0f);
static_assert(CinemaCurveHermite(2.0f, 6.0f, 0.0f, 0.0f, 1.0f, 1.0f) == 6.0f);

static float ParticleCurveValue(const xr_vector<SParticleCurveKey>& keys, float time, float fallback)
{
	if (keys.empty())
	{
		return fallback;
	}
	if (time <= keys.front().Time)
	{
		return keys.front().Value;
	}
	for (size_t i = 1; i < keys.size(); ++i)
	{
		if (time <= keys[i].Time)
		{
			const float duration = keys[i].Time - keys[i - 1].Time;
			const float factor = duration > 0.0f ? (time - keys[i - 1].Time) / duration : 1.0f;
			if (keys[i - 1].Smooth && duration > 0.0f)
			{
				return CinemaCurveHermite(keys[i - 1].Value, keys[i].Value, keys[i - 1].OutSlope, keys[i].InSlope, duration, factor);
			}
			return ParticleCurveLerp(keys[i - 1].Value, keys[i].Value, factor);
		}
	}
	return keys.back().Value;
}

static void CinemaShakeSpeedSlopes(const SParticleCurveKey& a, const SParticleCurveKey& b, float& outgoing, float& incoming)
{
	const double delta = (static_cast<double>(b.Value) - a.Value) / (static_cast<double>(b.Time) - a.Time);
	if (std::fabs(delta) < 1e-8)
	{
		outgoing = incoming = 0.0f;
		return;
	}
	outgoing = static_cast<float>(std::clamp(a.OutSlope / delta, 0.0, 3.0) * delta);
	incoming = static_cast<float>(std::clamp(b.InSlope / delta, 0.0, 3.0) * delta);
}

static float CinemaShakeSpeedValue(const xr_vector<SParticleCurveKey>& keys, float time)
{
	if (keys.empty()) return 1.0f;
	if (time <= keys.front().Time) return keys.front().Value;
	for (size_t i = 1; i < keys.size(); ++i)
	{
		const auto& a = keys[i - 1];
		const auto& b = keys[i];
		if (time > b.Time) continue;
		const float duration = b.Time - a.Time;
		if (!a.Smooth || duration <= 0.0f) return ParticleCurveValue(keys, time, 1.0f);
		float outgoing, incoming;
		CinemaShakeSpeedSlopes(a, b, outgoing, incoming);
		return std::clamp(CinemaCurveHermite(a.Value, b.Value, outgoing, incoming, duration, (time - a.Time) / duration), 0.0f, 10.0f);
	}
	return keys.back().Value;
}

static double CinemaShakeIntegratedSpeed(const xr_vector<SParticleCurveKey>& keys, float time)
{
	if (time <= 0.0f) return 0.0;
	if (keys.empty()) return time;
	double total = std::min(time, keys.front().Time) * static_cast<double>(keys.front().Value);
	for (size_t i = 1; i < keys.size(); ++i)
	{
		const auto& a = keys[i - 1];
		const auto& b = keys[i];
		const double duration = static_cast<double>(b.Time) - a.Time;
		if (time <= a.Time) return total;
		if (duration <= 0.0) continue;
		const double t = std::clamp((static_cast<double>(time) - a.Time) / duration, 0.0, 1.0);
		if (a.Smooth)
		{
			float outgoing, incoming;
			CinemaShakeSpeedSlopes(a, b, outgoing, incoming);
			const double t2 = t * t, t3 = t2 * t, t4 = t3 * t;
			total += duration * ((0.5 * t4 - t3 + t) * a.Value + (0.25 * t4 - (2.0 / 3.0) * t3 + 0.5 * t2) * duration * outgoing +
				(-0.5 * t4 + t3) * b.Value + (0.25 * t4 - t3 / 3.0) * duration * incoming);
		}
		else total += duration * (a.Value * t + 0.5 * (b.Value - a.Value) * t * t);
		if (time <= b.Time) return total;
	}
	if (time > keys.back().Time) total += (time - keys.back().Time) * static_cast<double>(keys.back().Value);
	return total;
}

constexpr float ParticleWaveShape(s32 type, float fraction)
{
	switch (type)
	{
	case 2: return fraction < 0.5f ? 4.0f * fraction - 1.0f : 3.0f - 4.0f * fraction;
	case 3: return fraction < 0.5f ? 1.0f : -1.0f;
	case 4: return 2.0f * fraction - 1.0f;
	default: return 0.0f;
	}
}

static_assert(ParticleWaveShape(2, 0.0f) == -1.0f);
static_assert(ParticleWaveShape(2, 0.5f) == 1.0f);
static_assert(ParticleWaveShape(3, 0.5f) == -1.0f);
static_assert(ParticleWaveShape(4, 0.5f) == 0.0f);

constexpr u32 CinemaNoiseHash(u32 value)
{
	value ^= value >> 16;
	value *= 0x7feb352du;
	value ^= value >> 15;
	value *= 0x846ca68bu;
	return value ^ (value >> 16);
}

static_assert(CinemaNoiseHash(0) == 0);
static_assert(CinemaNoiseHash(1) != CinemaNoiseHash(2));

static float CinemaNoiseSample(s32 index)
{
	return static_cast<float>(CinemaNoiseHash(static_cast<u32>(index)) >> 8) * (2.0f / 16777215.0f) - 1.0f;
}

static float CinemaSmoothNoise(float cycle)
{
	const float whole = floorf(cycle);
	const float fraction = cycle - whole;
	const float blend = fraction * fraction * (3.0f - 2.0f * fraction);
	const s32 index = static_cast<s32>(whole);
	const float a = CinemaNoiseSample(index);
	return a + (CinemaNoiseSample(index + 1) - a) * blend;
}

static float CinemaWaveSignal(s32 type, float cycle)
{
	if (type == 1) return sinf(cycle * PI_MUL_2);
	if (type == 5) return CinemaNoiseSample(static_cast<s32>(floorf(cycle)));
	if (type == 9)
	{
		float signal = 0.0f;
		for (u32 i = 0; i < 32; ++i)
		{
			const float phase = static_cast<float>(CinemaNoiseHash(i + 1) >> 8) / 16777216.0f;
			const float offset = static_cast<float>(CinemaNoiseHash(i + 33) >> 8) / 16777216.0f;
			const float cycles = cycle * (static_cast<float>(i) + offset) / 32.0f + phase;
			signal += sinf((cycles - floorf(cycles)) * PI_MUL_2);
		}
		return std::clamp(signal * 0.25f, -1.0f, 1.0f);
	}
	if (type == 6) return CinemaSmoothNoise(cycle);
	if (type == 10)
	{
		const float amplitude = 0.6f + 0.4f * CinemaSmoothNoise(cycle * 0.5f + 19.0f);
		const float phase = cycle + 0.3f * CinemaSmoothNoise(cycle * 0.25f + 47.0f);
		return amplitude * sinf((phase - floorf(phase)) * PI_MUL_2);
	}
	if (type == 8)
	{
		const float whole = floorf(cycle);
		const float fraction = cycle - whole;
		const s32 index = static_cast<s32>(whole);
		const float left = (CinemaNoiseHash(static_cast<u32>(index)) & 1) ? fraction : -fraction;
		const float right = (CinemaNoiseHash(static_cast<u32>(index + 1)) & 1) ? fraction - 1.0f : 1.0f - fraction;
		const float blend = fraction * fraction * fraction * (fraction * (fraction * 6.0f - 15.0f) + 10.0f);
		return 2.0f * (left + (right - left) * blend);
	}
	if (type == 7)
	{
		return (CinemaSmoothNoise(cycle) + 0.5f * CinemaSmoothNoise(cycle * 2.0f) +
			0.25f * CinemaSmoothNoise(cycle * 4.0f) + 0.125f * CinemaSmoothNoise(cycle * 8.0f)) / 1.875f;
	}
	return ParticleWaveShape(type, cycle - floorf(cycle));
}

static float ParticleModulatedValue(const SCinemaParticle& particle, u32 channel, float time)
{
	const auto& wave = particle.Waves[channel];
	const float cycle = time * wave.Frequency + wave.Phase;
	const float signal = CinemaWaveSignal(wave.Type, cycle);
	return std::clamp(ParticleCurveValue(particle.Curves[channel], time, channel >= 6 ? 1.0f : 0.0f) + wave.Amplitude * signal, wave.Minimum, wave.Maximum);
}

static float LightModulatedValue(const SCinemaLight& light, u32 channel, float time)
{
	const auto& wave = light.Waves[channel];
	const float cycle = time * wave.Frequency + wave.Phase;
	const float signal = CinemaWaveSignal(wave.Type, cycle);
	return std::clamp(ParticleCurveValue(light.Curves[channel], time, CinemaLightChannelDefault(channel)) + wave.Amplitude * signal, wave.Minimum, wave.Maximum);
}

static void CinemaLightTransform(const SCinemaLight& light, Fvector& position, Fmatrix& orientation)
{
	position = light.Position;
	orientation = light.Orientation;
	if (light.Modulation)
	{
		position.add(Fvector().set(LightModulatedValue(light, 0, light.Time), LightModulatedValue(light, 1, light.Time), LightModulatedValue(light, 2, light.Time)));
		Fmatrix rotation;
		rotation.setHPB(deg2rad(LightModulatedValue(light, 3, light.Time)), deg2rad(LightModulatedValue(light, 4, light.Time)), deg2rad(LightModulatedValue(light, 5, light.Time)));
		orientation.mul(light.Orientation, rotation);
	}
}

static void UpdateCinemaLightValues(SCinemaLight& light)
{
	if (!light.Light)
	{
		return;
	}
	Fvector position;
	Fmatrix orientation;
	CinemaLightTransform(light, position, orientation);
	float values[kCinemaLightChannels] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f };
	if (light.Modulation)
	{
		for (u32 i = 6; i < kCinemaLightChannels; ++i)
		{
			values[i] = LightModulatedValue(light, i, light.Time);
		}
	}
	light.Light->set_position(position);
	light.Light->set_rotation(orientation.k, orientation.i);
	light.Light->set_color(light.Color.x * light.Intensity * values[6] * light.MasterIntensity * light.MasterColor.x, light.Color.y * light.Intensity * values[7] * light.MasterIntensity * light.MasterColor.y, light.Color.z * light.Intensity * values[8] * light.MasterIntensity * light.MasterColor.z);
	light.Light->set_range(std::clamp(light.Range * values[9], 0.1f, 1000.0f));
	light.Light->set_cone(deg2rad(std::clamp(light.Cone * values[10], 1.0f, 120.0f)));
	light.Light->set_virtual_size(std::clamp(light.VirtualSize * values[11], 0.0f, 100.0f));
	light.Light->set_volumetric_quality(std::clamp(light.VolumeQuality * values[12], 0.01f, 1.0f));
	light.Light->set_volumetric_intensity(std::clamp(light.VolumeIntensity * values[13], 0.0f, 100.0f));
	light.Light->set_volumetric_distance(std::clamp(light.VolumeDistance * values[14], 0.01f, 1.0f));
}

constexpr bool ValidParticleTimeline(float in, float out)
{
	return in >= 0.0f && out >= in + 0.01f && out <= 600.0f;
}

static_assert(ValidParticleTimeline(0.0f, 10.0f));
static_assert(!ValidParticleTimeline(1.0f, 1.0f));
static_assert(!ValidParticleTimeline(-1.0f, 10.0f));
static_assert(!ValidParticleTimeline(0.0f, 601.0f));

static Fvector ParticlePosition(const SCinemaParticle& particle)
{
	Fvector position = particle.Position;
	if (particle.Modulation && particle.Out > 0.0f)
	{
		Fvector offset;
		offset.set(ParticleModulatedValue(particle, 0, particle.Time), ParticleModulatedValue(particle, 1, particle.Time), ParticleModulatedValue(particle, 2, particle.Time));
		position.add(offset);
	}
	return position;
}

void UpdateParticleTransform(SCinemaParticle& particle)
{
	Fmatrix transform = particle.Orientation;
	transform.c = ParticlePosition(particle);
	float scale = particle.Scale;
	if (particle.Modulation && particle.Out > 0.0f)
	{
		Fmatrix rotation;
		rotation.setHPB(deg2rad(ParticleModulatedValue(particle, 3, particle.Time)), deg2rad(ParticleModulatedValue(particle, 4, particle.Time)), deg2rad(ParticleModulatedValue(particle, 5, particle.Time)));
		Fmatrix result;
		result.mul(particle.Orientation, rotation);
		result.c = transform.c;
		transform = result;
		scale *= ParticleModulatedValue(particle, 6, particle.Time);
	}
	particle.Object->UpdateParent(transform, zero_vel);
	particle.Object->SetScale(scale);
	static_cast<CCinemaParticleObject*>(particle.Object.get())->SetSpeed(std::clamp(particle.Speed * (particle.Modulation ? ParticleModulatedValue(particle, 7, particle.Time) : 1.0f), 0.0f, 4.0f));
}

void StartParticleTimeline(SCinemaParticle& particle)
{
	particle.Object->Stop(false);
	particle.Time = 0.0f;
	particle.Running = particle.Timeline;
	particle.Started = !particle.Timeline || particle.In <= 0.0f;
	if (particle.Started)
	{
		UpdateParticleTransform(particle);
		particle.Object->Play(false);
	}
}

struct SClipDragPayload
{
	string_path Source = {};
	string_path Name = {};
	bool Fx = false;
};

struct SIndexDragPayload
{
	s32 Index = -1;
};

struct SToolbarLayout
{
	explicit SToolbarLayout(float available)
		: Available(available)
	{
	}

	void Item(float width)
	{
		const float spacing = ImGui::GetStyle().ItemSpacing.x;

		if (!HasAny)
		{
			HasAny = true;
			RowWidth = width;
			return;
		}

		if (RowWidth + spacing + width > Available)
		{
			ImGui::NewLine();
			RowWidth = width;
			return;
		}

		ImGui::SameLine();
		RowWidth += spacing + width;
	}

	float Available = 0.0f;
	float RowWidth = 0.0f;
	bool HasAny = false;
};

static float CinemaButtonWidth(const char* label)
{
	return ImGui::CalcTextSize(label).x + ImGui::GetStyle().FramePadding.x * 2.0f;
}

static void CinemaHint(const char* text)
{
	ImGui::PushStyleColor(ImGuiCol_Text, ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled));
	ImGui::TextWrapped("%s", text);
	ImGui::PopStyleColor(); 
}

static const char* CinemaFileName(const char* path)
{
	if (path == nullptr)
	{
		return "";
	}

	const char* backslash = strrchr(path, '\\');
	const char* slash = strrchr(path, '/');
	const char* last = backslash != nullptr && slash != nullptr ? (backslash > slash ? backslash : slash) : (backslash != nullptr ? backslash : slash);

	return last != nullptr ? last + 1 : path;
}

struct SProjectedPoint
{
	ImVec2 Screen = ImVec2(0.0f, 0.0f);
	float W = 0.0f;
	bool Visible = false;
};

struct SCinemaActor;

struct SGizmoDrag
{
	bool Active = false;
	Fvector* Position = nullptr;
	EGizmoMode Mode = EGizmoMode::Translate;
	EGizmoAxis Axis = EGizmoAxis::None;
	Fvector AxisDir = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Pivot = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector StartPosition = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix StartOrientation;
	float StartParam = 0.0f;
	float LastAngle = 0.0f;
	float AccumulatedAngle = 0.0f;
	bool UseScreenAngle = false;
};

struct SBindBoneParam
{
	Fmatrix Matrix;
};

class CCinemaActorObject final : public CObject
{
public:
	IRenderVisual* ItemVisual = nullptr;
	Fmatrix ItemTransform;

	CCinemaActorObject()
	{
		cName_set("cinema_fake_npc");
		cNameSect_set("cinema_fake_npc");
		SpatialComponent->type = ESPATIAL_TYPE::RENDERABLE;
	}

	~CCinemaActorObject() override
	{
		renderable.visual = nullptr;
	}

	void Bind(IRenderVisual* visual, const Fmatrix& transform)
	{
		if (visual == nullptr)
		{
			Unbind();
			return;
		}

		renderable.visual = visual;
		renderable.xform = transform;

		if (InSpace)
		{
			spatial_move();
			return;
		}

		InSpace = true;
		spatial_register();
	}

	void Unbind()
	{
		renderable.visual = nullptr;

		if (!InSpace)
		{
			return;
		}

		InSpace = false;
		spatial_unregister();
	}

	void renderable_Render(IDSGraphManager* DM) override
	{
		if (renderable.visual == nullptr)
		{
			return;
		}

		DM->add_Dynamic(renderable.visual, &renderable.xform);
		renderable.visual->getVisData().hom_frame = Device.dwFrame;

		if (ItemVisual != nullptr)
		{
			DM->add_Dynamic(ItemVisual, &ItemTransform);
			ItemVisual->getVisData().hom_frame = Device.dwFrame;
		}
	}

	bool renderable_ShadowGenerate() override
	{
		return false;
	}

	bool register_schedule() const override
	{
		return false;
	}

private:
	bool InSpace = false;
};

struct SCinemaActor
{
	xr_string Name;
	shared_str Visual;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation;
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);
	Fbox LocalBox;
	IRenderVisual* Model = nullptr;
	IKinematics* Skeleton = nullptr;
	IKinematicsAnimated* Animated = nullptr;
	CCinemaActorObject Object;
	xr_vector<xr_unique_ptr<SBindBoneParam>> BindPoseBones;
	bool BindPose = false;
	xr_vector<SAnimationSource> Sources;
	xr_vector<shared_str> ModelSources;
	xr_vector<shared_str> AnimationPacks;
	xr_vector<std::pair<shared_str, shared_str>> AdaptedSources;
	xr_vector<SPlaylistEntry> Playlist;
	xr_vector<STrimRange> Trims;
	bool PingPong = false;
	float PlayDirection = 1.0f;
	s32 PlaylistIndex = -1;
	s32 MasterPlaylistIndex = -1;
	s32 PlaylistSelected = -1;
	xr_vector<s32> PlaylistSelection;
	s32 PlaylistAnchor = -1;
	bool Loop = false;
	float Speed = 1.0f;
	bool Paused = false;
	MotionID Motion;
	shared_str MotionName;
	shared_str MotionSource;
	bool MotionFx = false;
	float FxDeadline = 0.0f;
	float ShotPreviousTime = -1.0f;
	bool PatternOwned = false;
	bool PatternPreview = false;
	bool MasterTrack = false;
	bool MasterOwned = false;
	bool MasterBeforeFirst = false;
	xr_vector<SMasterClip> MasterClips;
	xr_vector<SMasterTransformKey> MasterTransforms;
	shared_str ItemSection;
	IRenderVisual* ItemModel = nullptr;
	bool ItemWeaponStyle = false;
	s32 ItemHandDependence = 2;
	u16 ItemBoneR = u16(-1);
	u16 ItemBoneL = u16(-1);
	u16 ItemBoneR2 = u16(-1);
	u16 ItemBoneAttach = u16(-1);
	shared_str ItemBoneName;
	Fvector ItemPosition = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector ItemRotation = Fvector().set(0.0f, 0.0f, 0.0f);

	SCinemaActor()
	{
		Orientation.identity();
		LocalBox.invalidate();
	}
};

struct SEditorTheme
{
	const char* Id;
	const char* Name;

	ImVec4 WindowBg;
	ImVec4 ChildBg;
	ImVec4 PopupBg;
	ImVec4 Text;
	ImVec4 TextDisabled;
	ImVec4 FrameBg;
	ImVec4 FrameBgHovered;
	ImVec4 FrameBgActive;
	ImVec4 Button;
	ImVec4 ButtonHovered;
	ImVec4 ButtonActive;
	ImVec4 Header;
	ImVec4 HeaderHovered;
	ImVec4 HeaderActive;
	ImVec4 Tab;
	ImVec4 TabHovered;
	ImVec4 TabSelected;
	ImVec4 TitleBg;
	ImVec4 MenuBarBg;
	ImVec4 Separator;
	ImVec4 Border;
	ImVec4 CheckMark;
	ImVec4 SliderGrab;
	ImVec4 SliderGrabActive;
	ImVec4 ScrollbarBg;
	ImVec4 ScrollbarGrab;

	ImU32 TimelineBackground;
	ImU32 TimelineElapsed;
	ImU32 TimelineMark;
	ImU32 TimelineTickMinor;
	ImU32 TimelineTickMajor;
	ImU32 TimelineText;
	ImU32 TimelinePlayhead;
	ImU32 TimelineBorder;

	u32 AxisX;
	u32 AxisY;
	u32 AxisZ;
	u32 AxisHighlighted;
	u32 AxisPivot;
};

struct SThemeColorLink
{
	ImGuiCol Color;
	ImVec4 SEditorTheme::* Value;
};

static const SThemeColorLink kCinemaThemeColors[] =
{
	{ ImGuiCol_WindowBg, &SEditorTheme::WindowBg },
	{ ImGuiCol_ChildBg, &SEditorTheme::ChildBg },
	{ ImGuiCol_PopupBg, &SEditorTheme::PopupBg },
	{ ImGuiCol_Text, &SEditorTheme::Text },
	{ ImGuiCol_TextDisabled, &SEditorTheme::TextDisabled },
	{ ImGuiCol_FrameBg, &SEditorTheme::FrameBg },
	{ ImGuiCol_FrameBgHovered, &SEditorTheme::FrameBgHovered },
	{ ImGuiCol_FrameBgActive, &SEditorTheme::FrameBgActive },
	{ ImGuiCol_Button, &SEditorTheme::Button },
	{ ImGuiCol_ButtonHovered, &SEditorTheme::ButtonHovered },
	{ ImGuiCol_ButtonActive, &SEditorTheme::ButtonActive },
	{ ImGuiCol_Header, &SEditorTheme::Header },
	{ ImGuiCol_HeaderHovered, &SEditorTheme::HeaderHovered },
	{ ImGuiCol_HeaderActive, &SEditorTheme::HeaderActive },
	{ ImGuiCol_Tab, &SEditorTheme::Tab },
	{ ImGuiCol_TabHovered, &SEditorTheme::TabHovered },
	{ ImGuiCol_TabSelected, &SEditorTheme::TabSelected },
	{ ImGuiCol_TitleBg, &SEditorTheme::TitleBg },
	{ ImGuiCol_MenuBarBg, &SEditorTheme::MenuBarBg },
	{ ImGuiCol_Separator, &SEditorTheme::Separator },
	{ ImGuiCol_Border, &SEditorTheme::Border },
	{ ImGuiCol_CheckMark, &SEditorTheme::CheckMark },
	{ ImGuiCol_SliderGrab, &SEditorTheme::SliderGrab },
	{ ImGuiCol_SliderGrabActive, &SEditorTheme::SliderGrabActive },
	{ ImGuiCol_ScrollbarBg, &SEditorTheme::ScrollbarBg },
	{ ImGuiCol_ScrollbarGrab, &SEditorTheme::ScrollbarGrab },
};

static const SEditorTheme kCinemaThemes[] =
{
	{
		.Id = "default",
		.Name = "Default",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, kGeneralAlphaLevelForImGuiWindows),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.06f, 0.06f, 0.07f, 0.94f),
		.Text = ImVec4(0.90f, 0.90f, 0.92f, 1.00f),
		.TextDisabled = ImVec4(0.55f, 0.55f, 0.58f, 1.00f),
		.FrameBg = ImVec4(0.16f, 0.16f, 0.18f, 1.00f),
		.FrameBgHovered = ImVec4(0.22f, 0.22f, 0.25f, 1.00f),
		.FrameBgActive = ImVec4(0.28f, 0.28f, 0.32f, 1.00f),
		.Button = ImVec4(0.18f, 0.18f, 0.21f, 1.00f),
		.ButtonHovered = ImVec4(0.25f, 0.25f, 0.29f, 1.00f),
		.ButtonActive = ImVec4(0.32f, 0.32f, 0.37f, 1.00f),
		.Header = ImVec4(0.25f, 0.35f, 0.45f, 1.00f),
		.HeaderHovered = ImVec4(0.30f, 0.42f, 0.54f, 1.00f),
		.HeaderActive = ImVec4(0.36f, 0.50f, 0.64f, 1.00f),
		.Tab = ImVec4(0.12f, 0.12f, 0.14f, 1.00f),
		.TabHovered = ImVec4(0.20f, 0.28f, 0.34f, 1.00f),
		.TabSelected = ImVec4(0.22f, 0.32f, 0.42f, 1.00f),
		.TitleBg = ImVec4(0.05f, 0.05f, 0.06f, 1.00f),
		.MenuBarBg = ImVec4(0.07f, 0.07f, 0.08f, 1.00f),
		.Separator = ImVec4(0.35f, 0.35f, 0.38f, 1.00f),
		.Border = ImVec4(0.45f, 0.45f, 0.50f, 1.00f),
		.CheckMark = ImVec4(0.60f, 0.80f, 0.95f, 1.00f),
		.SliderGrab = ImVec4(0.35f, 0.55f, 0.70f, 1.00f),
		.SliderGrabActive = ImVec4(0.50f, 0.70f, 0.90f, 1.00f),
		.ScrollbarBg = ImVec4(0.08f, 0.08f, 0.09f, 1.00f),
		.ScrollbarGrab = ImVec4(0.28f, 0.28f, 0.32f, 1.00f),
		.TimelineBackground = IM_COL32(22, 22, 24, 255),
		.TimelineElapsed = IM_COL32(52, 78, 52, 140),
		.TimelineMark = IM_COL32(78, 121, 176, 110),
		.TimelineTickMinor = IM_COL32(58, 58, 62, 255),
		.TimelineTickMajor = IM_COL32(105, 105, 112, 255),
		.TimelineText = IM_COL32(180, 180, 186, 255),
		.TimelinePlayhead = IM_COL32(222, 170, 64, 255),
		.TimelineBorder = IM_COL32(80, 80, 86, 255),
		.AxisX = color_rgba(220, 70, 70, 255),
		.AxisY = color_rgba(80, 210, 90, 255),
		.AxisZ = color_rgba(70, 130, 235, 255),
		.AxisHighlighted = color_rgba(255, 220, 70, 255),
		.AxisPivot = color_rgba(240, 240, 240, 220),
	},
	{
		.Id = "oled_black",
		.Name = "OLED Black",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.94f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.03f, 0.03f, 0.04f, 0.98f),
		.Text = ImVec4(0.94f, 0.95f, 0.96f, 1.00f),
		.TextDisabled = ImVec4(0.60f, 0.62f, 0.66f, 1.00f),
		.FrameBg = ImVec4(0.10f, 0.10f, 0.12f, 1.00f),
		.FrameBgHovered = ImVec4(0.16f, 0.17f, 0.20f, 1.00f),
		.FrameBgActive = ImVec4(0.23f, 0.24f, 0.28f, 1.00f),
		.Button = ImVec4(0.12f, 0.13f, 0.16f, 1.00f),
		.ButtonHovered = ImVec4(0.19f, 0.21f, 0.25f, 1.00f),
		.ButtonActive = ImVec4(0.27f, 0.30f, 0.35f, 1.00f),
		.Header = ImVec4(0.10f, 0.27f, 0.35f, 1.00f),
		.HeaderHovered = ImVec4(0.13f, 0.36f, 0.47f, 1.00f),
		.HeaderActive = ImVec4(0.17f, 0.46f, 0.60f, 1.00f),
		.Tab = ImVec4(0.06f, 0.06f, 0.08f, 1.00f),
		.TabHovered = ImVec4(0.14f, 0.21f, 0.26f, 1.00f),
		.TabSelected = ImVec4(0.10f, 0.27f, 0.35f, 1.00f),
		.TitleBg = ImVec4(0.02f, 0.02f, 0.03f, 1.00f),
		.MenuBarBg = ImVec4(0.03f, 0.03f, 0.04f, 1.00f),
		.Separator = ImVec4(0.24f, 0.26f, 0.30f, 1.00f),
		.Border = ImVec4(0.32f, 0.35f, 0.40f, 1.00f),
		.CheckMark = ImVec4(0.35f, 0.90f, 1.00f, 1.00f),
		.SliderGrab = ImVec4(0.30f, 0.72f, 0.88f, 1.00f),
		.SliderGrabActive = ImVec4(0.45f, 0.90f, 1.00f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.02f, 0.03f, 1.00f),
		.ScrollbarGrab = ImVec4(0.22f, 0.25f, 0.30f, 1.00f),
		.TimelineBackground = IM_COL32(4, 4, 6, 255),
		.TimelineElapsed = IM_COL32(26, 72, 92, 190),
		.TimelineMark = IM_COL32(40, 116, 150, 120),
		.TimelineTickMinor = IM_COL32(40, 42, 48, 255),
		.TimelineTickMajor = IM_COL32(112, 122, 134, 255),
		.TimelineText = IM_COL32(205, 214, 224, 255),
		.TimelinePlayhead = IM_COL32(255, 190, 70, 255),
		.TimelineBorder = IM_COL32(58, 64, 72, 255),
		.AxisX = color_rgba(255, 120, 60, 255),
		.AxisY = color_rgba(130, 235, 130, 255),
		.AxisZ = color_rgba(90, 170, 255, 255),
		.AxisHighlighted = color_rgba(255, 240, 150, 255),
		.AxisPivot = color_rgba(240, 240, 240, 220),
	},
	{
		.Id = "oled_dim",
		.Name = "OLED Dim",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.97f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.02f, 0.02f, 0.03f, 0.98f),
		.Text = ImVec4(0.74f, 0.75f, 0.78f, 1.00f),
		.TextDisabled = ImVec4(0.44f, 0.45f, 0.48f, 1.00f),
		.FrameBg = ImVec4(0.07f, 0.07f, 0.08f, 1.00f),
		.FrameBgHovered = ImVec4(0.11f, 0.12f, 0.14f, 1.00f),
		.FrameBgActive = ImVec4(0.16f, 0.17f, 0.20f, 1.00f),
		.Button = ImVec4(0.09f, 0.09f, 0.11f, 1.00f),
		.ButtonHovered = ImVec4(0.14f, 0.15f, 0.18f, 1.00f),
		.ButtonActive = ImVec4(0.20f, 0.22f, 0.26f, 1.00f),
		.Header = ImVec4(0.08f, 0.14f, 0.20f, 1.00f),
		.HeaderHovered = ImVec4(0.11f, 0.20f, 0.28f, 1.00f),
		.HeaderActive = ImVec4(0.15f, 0.27f, 0.38f, 1.00f),
		.Tab = ImVec4(0.05f, 0.05f, 0.06f, 1.00f),
		.TabHovered = ImVec4(0.11f, 0.16f, 0.20f, 1.00f),
		.TabSelected = ImVec4(0.09f, 0.17f, 0.24f, 1.00f),
		.TitleBg = ImVec4(0.01f, 0.01f, 0.02f, 1.00f),
		.MenuBarBg = ImVec4(0.02f, 0.02f, 0.03f, 1.00f),
		.Separator = ImVec4(0.17f, 0.18f, 0.21f, 1.00f),
		.Border = ImVec4(0.24f, 0.26f, 0.30f, 1.00f),
		.CheckMark = ImVec4(0.55f, 0.72f, 0.88f, 1.00f),
		.SliderGrab = ImVec4(0.45f, 0.60f, 0.75f, 1.00f),
		.SliderGrabActive = ImVec4(0.62f, 0.78f, 0.95f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.02f, 0.02f, 1.00f),
		.ScrollbarGrab = ImVec4(0.16f, 0.17f, 0.20f, 1.00f),
		.TimelineBackground = IM_COL32(2, 2, 3, 255),
		.TimelineElapsed = IM_COL32(20, 44, 58, 190),
		.TimelineMark = IM_COL32(28, 74, 96, 110),
		.TimelineTickMinor = IM_COL32(32, 34, 38, 255),
		.TimelineTickMajor = IM_COL32(84, 92, 102, 255),
		.TimelineText = IM_COL32(168, 174, 184, 255),
		.TimelinePlayhead = IM_COL32(210, 158, 66, 255),
		.TimelineBorder = IM_COL32(44, 48, 54, 255),
		.AxisX = color_rgba(230, 110, 60, 255),
		.AxisY = color_rgba(110, 200, 120, 255),
		.AxisZ = color_rgba(80, 150, 230, 255),
		.AxisHighlighted = color_rgba(240, 220, 120, 255),
		.AxisPivot = color_rgba(220, 220, 220, 220),
	},
	{
		.Id = "oled_amber",
		.Name = "OLED Amber",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.95f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.04f, 0.03f, 0.01f, 0.98f),
		.Text = ImVec4(0.98f, 0.80f, 0.38f, 1.00f),
		.TextDisabled = ImVec4(0.62f, 0.48f, 0.20f, 1.00f),
		.FrameBg = ImVec4(0.12f, 0.09f, 0.03f, 1.00f),
		.FrameBgHovered = ImVec4(0.19f, 0.14f, 0.05f, 1.00f),
		.FrameBgActive = ImVec4(0.27f, 0.20f, 0.07f, 1.00f),
		.Button = ImVec4(0.14f, 0.11f, 0.04f, 1.00f),
		.ButtonHovered = ImVec4(0.22f, 0.16f, 0.06f, 1.00f),
		.ButtonActive = ImVec4(0.31f, 0.23f, 0.08f, 1.00f),
		.Header = ImVec4(0.27f, 0.20f, 0.06f, 1.00f),
		.HeaderHovered = ImVec4(0.37f, 0.27f, 0.09f, 1.00f),
		.HeaderActive = ImVec4(0.47f, 0.35f, 0.12f, 1.00f),
		.Tab = ImVec4(0.09f, 0.07f, 0.02f, 1.00f),
		.TabHovered = ImVec4(0.19f, 0.14f, 0.05f, 1.00f),
		.TabSelected = ImVec4(0.27f, 0.20f, 0.06f, 1.00f),
		.TitleBg = ImVec4(0.03f, 0.02f, 0.01f, 1.00f),
		.MenuBarBg = ImVec4(0.04f, 0.03f, 0.01f, 1.00f),
		.Separator = ImVec4(0.38f, 0.29f, 0.11f, 1.00f),
		.Border = ImVec4(0.50f, 0.38f, 0.14f, 1.00f),
		.CheckMark = ImVec4(1.00f, 0.86f, 0.40f, 1.00f),
		.SliderGrab = ImVec4(0.78f, 0.60f, 0.22f, 1.00f),
		.SliderGrabActive = ImVec4(0.98f, 0.78f, 0.34f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.02f, 0.01f, 1.00f),
		.ScrollbarGrab = ImVec4(0.24f, 0.18f, 0.06f, 1.00f),
		.TimelineBackground = IM_COL32(6, 4, 1, 255),
		.TimelineElapsed = IM_COL32(96, 66, 12, 190),
		.TimelineMark = IM_COL32(128, 88, 20, 120),
		.TimelineTickMinor = IM_COL32(46, 35, 10, 255),
		.TimelineTickMajor = IM_COL32(132, 100, 32, 255),
		.TimelineText = IM_COL32(246, 206, 116, 255),
		.TimelinePlayhead = IM_COL32(255, 236, 150, 255),
		.TimelineBorder = IM_COL32(96, 72, 22, 255),
		.AxisX = color_rgba(255, 140, 50, 255),
		.AxisY = color_rgba(200, 220, 90, 255),
		.AxisZ = color_rgba(90, 180, 240, 255),
		.AxisHighlighted = color_rgba(255, 240, 170, 255),
		.AxisPivot = color_rgba(250, 220, 150, 220),
	},
	{
		.Id = "oled_mint",
		.Name = "OLED Mint",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.95f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.01f, 0.03f, 0.02f, 0.98f),
		.Text = ImVec4(0.82f, 0.98f, 0.90f, 1.00f),
		.TextDisabled = ImVec4(0.48f, 0.62f, 0.54f, 1.00f),
		.FrameBg = ImVec4(0.05f, 0.09f, 0.08f, 1.00f),
		.FrameBgHovered = ImVec4(0.08f, 0.15f, 0.12f, 1.00f),
		.FrameBgActive = ImVec4(0.12f, 0.22f, 0.18f, 1.00f),
		.Button = ImVec4(0.06f, 0.11f, 0.09f, 1.00f),
		.ButtonHovered = ImVec4(0.10f, 0.18f, 0.15f, 1.00f),
		.ButtonActive = ImVec4(0.14f, 0.26f, 0.21f, 1.00f),
		.Header = ImVec4(0.06f, 0.24f, 0.18f, 1.00f),
		.HeaderHovered = ImVec4(0.09f, 0.33f, 0.25f, 1.00f),
		.HeaderActive = ImVec4(0.13f, 0.42f, 0.32f, 1.00f),
		.Tab = ImVec4(0.03f, 0.06f, 0.05f, 1.00f),
		.TabHovered = ImVec4(0.07f, 0.16f, 0.13f, 1.00f),
		.TabSelected = ImVec4(0.06f, 0.24f, 0.18f, 1.00f),
		.TitleBg = ImVec4(0.01f, 0.02f, 0.02f, 1.00f),
		.MenuBarBg = ImVec4(0.02f, 0.03f, 0.03f, 1.00f),
		.Separator = ImVec4(0.16f, 0.30f, 0.26f, 1.00f),
		.Border = ImVec4(0.22f, 0.40f, 0.34f, 1.00f),
		.CheckMark = ImVec4(0.40f, 1.00f, 0.78f, 1.00f),
		.SliderGrab = ImVec4(0.30f, 0.80f, 0.62f, 1.00f),
		.SliderGrabActive = ImVec4(0.45f, 0.98f, 0.78f, 1.00f),
		.ScrollbarBg = ImVec4(0.01f, 0.02f, 0.02f, 1.00f),
		.ScrollbarGrab = ImVec4(0.12f, 0.22f, 0.19f, 1.00f),
		.TimelineBackground = IM_COL32(2, 5, 4, 255),
		.TimelineElapsed = IM_COL32(18, 70, 56, 190),
		.TimelineMark = IM_COL32(28, 110, 88, 120),
		.TimelineTickMinor = IM_COL32(30, 40, 36, 255),
		.TimelineTickMajor = IM_COL32(90, 112, 104, 255),
		.TimelineText = IM_COL32(190, 230, 215, 255),
		.TimelinePlayhead = IM_COL32(190, 255, 225, 255),
		.TimelineBorder = IM_COL32(44, 58, 54, 255),
		.AxisX = color_rgba(255, 130, 70, 255),
		.AxisY = color_rgba(120, 240, 140, 255),
		.AxisZ = color_rgba(90, 175, 255, 255),
		.AxisHighlighted = color_rgba(255, 245, 160, 255),
		.AxisPivot = color_rgba(235, 245, 240, 220),
	},
	{
		.Id = "oled_magenta",
		.Name = "OLED Magenta",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.95f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.03f, 0.01f, 0.03f, 0.98f),
		.Text = ImVec4(0.97f, 0.88f, 0.95f, 1.00f),
		.TextDisabled = ImVec4(0.62f, 0.50f, 0.58f, 1.00f),
		.FrameBg = ImVec4(0.10f, 0.05f, 0.09f, 1.00f),
		.FrameBgHovered = ImVec4(0.17f, 0.08f, 0.15f, 1.00f),
		.FrameBgActive = ImVec4(0.24f, 0.12f, 0.21f, 1.00f),
		.Button = ImVec4(0.12f, 0.06f, 0.11f, 1.00f),
		.ButtonHovered = ImVec4(0.20f, 0.10f, 0.18f, 1.00f),
		.ButtonActive = ImVec4(0.29f, 0.14f, 0.26f, 1.00f),
		.Header = ImVec4(0.30f, 0.08f, 0.24f, 1.00f),
		.HeaderHovered = ImVec4(0.42f, 0.11f, 0.34f, 1.00f),
		.HeaderActive = ImVec4(0.55f, 0.15f, 0.44f, 1.00f),
		.Tab = ImVec4(0.08f, 0.04f, 0.07f, 1.00f),
		.TabHovered = ImVec4(0.18f, 0.09f, 0.16f, 1.00f),
		.TabSelected = ImVec4(0.28f, 0.10f, 0.23f, 1.00f),
		.TitleBg = ImVec4(0.02f, 0.01f, 0.02f, 1.00f),
		.MenuBarBg = ImVec4(0.03f, 0.01f, 0.03f, 1.00f),
		.Separator = ImVec4(0.38f, 0.18f, 0.32f, 1.00f),
		.Border = ImVec4(0.50f, 0.24f, 0.42f, 1.00f),
		.CheckMark = ImVec4(1.00f, 0.55f, 0.92f, 1.00f),
		.SliderGrab = ImVec4(0.82f, 0.35f, 0.72f, 1.00f),
		.SliderGrabActive = ImVec4(1.00f, 0.55f, 0.92f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.01f, 0.02f, 1.00f),
		.ScrollbarGrab = ImVec4(0.24f, 0.12f, 0.20f, 1.00f),
		.TimelineBackground = IM_COL32(5, 2, 5, 255),
		.TimelineElapsed = IM_COL32(80, 20, 66, 190),
		.TimelineMark = IM_COL32(110, 30, 90, 120),
		.TimelineTickMinor = IM_COL32(42, 30, 40, 255),
		.TimelineTickMajor = IM_COL32(120, 90, 110, 255),
		.TimelineText = IM_COL32(230, 205, 225, 255),
		.TimelinePlayhead = IM_COL32(255, 180, 120, 255),
		.TimelineBorder = IM_COL32(80, 50, 74, 255),
		.AxisX = color_rgba(255, 120, 90, 255),
		.AxisY = color_rgba(150, 230, 180, 255),
		.AxisZ = color_rgba(130, 150, 255, 255),
		.AxisHighlighted = color_rgba(255, 235, 180, 255),
		.AxisPivot = color_rgba(240, 235, 241, 220),
	},
};

constexpr s32 kCinemaThemeCount = static_cast<s32>(std::size(kCinemaThemes));

static bool CinemaResetRequested()
{
	if (!ImGui::BeginPopupContextItem())
	{
		return false;
	}

	const bool reset = ImGui::MenuItem("Reset to default");

	ImGui::EndPopup();
	return reset;
}

static bool CinemaContainsFilter(const char* text, const char* filter)
{
	if (filter[0] == '\0')
	{
		return true;
	}

	const size_t length = xr_strlen(filter);

	for (const char* position = text; *position; ++position)
	{
		if (_strnicmp(position, filter, length) == 0)
		{
			return true;
		}
	}

	return false;
}

static bool CinemaIsOmfPath(const shared_str& source)
{
	const u32 length = source.size();

	return length > 4 && !_stricmp(source.c_str() + length - 4, ".omf");
}

static void CinemaBindPoseCallback(CBoneInstance* instance)
{
	SBindBoneParam* parameter = static_cast<SBindBoneParam*>(instance->callback_param());

	if (parameter != nullptr)
	{
		instance->mTransform = parameter->Matrix;
	}
}

static void CinemaClearBindPose(SCinemaActor& actor)
{
	if (!actor.BindPose)
	{
		return;
	}

	if (actor.Skeleton != nullptr)
	{
		for (u16 bone = 0, count = actor.Skeleton->LL_BoneCount(); bone < count; ++bone)
		{
			CBoneInstance& instance = actor.Skeleton->LL_GetBoneInstance(bone);

			if (instance.callback() == CinemaBindPoseCallback)
			{
				instance.reset_callback();
			}
		}
	}

	actor.BindPoseBones.clear();
	actor.BindPose = false;
}

static bool CinemaSetBindPose(SCinemaActor& actor)
{
	if (actor.Skeleton == nullptr)
	{
		return false;
	}

	CinemaClearBindPose(actor);

	xr_vector<Fmatrix> bindTransforms;
	actor.Skeleton->LL_GetBindTransform(bindTransforms);

	const u16 boneCount = actor.Skeleton->LL_BoneCount();

	if (bindTransforms.size() < boneCount)
	{
		return false;
	}

	actor.BindPoseBones.reserve(boneCount);

	for (u16 bone = 0; bone < boneCount; ++bone)
	{
		auto parameter = xr_make_unique<SBindBoneParam>();
		parameter->Matrix = bindTransforms[bone];

		SBindBoneParam* parameterPtr = parameter.get();
		actor.BindPoseBones.push_back(std::move(parameter));
		actor.Skeleton->LL_GetBoneInstance(bone).set_callback(bctCustom, CinemaBindPoseCallback, parameterPtr, true);
	}

	actor.BindPose = true;
	actor.Skeleton->CalculateBones_Invalidate();
	actor.Skeleton->CalculateBones(true);
	return true;
}

static bool CinemaSourcePlayable(const SAnimationSource& source, const SCinemaActor& actor);

static const SAnimationClip* CinemaFindEditorClip(const SCinemaActor& actor, const SAnimationSource** source)
{
	for (const SAnimationSource& animationSource : actor.Sources)
	{
		if (std::find(actor.ModelSources.begin(), actor.ModelSources.end(), animationSource.Id) == actor.ModelSources.end() || !CinemaSourcePlayable(animationSource, actor)) continue;
		for (const SAnimationClip& clip : animationSource.Clips)
		{
			if (!clip.Fx && _stricmp(clip.Name.c_str(), "$editor") == 0)
			{
				if (source != nullptr)
				{
					*source = &animationSource;
				}

				return &clip;
			}
		}
	}

	return nullptr;
}

static const SAnimationClip* CinemaFindDefaultIdle(const SCinemaActor& actor, const SAnimationSource** source)
{
	static const char* idleNames[] =
	{
		"idle",
		"stand_idle_0",
		"stand_idle_1",
		"stand_idle_2",
		"norm_idle_0",
		"norm_idle_1",
		"idle_0",
		"idle_1"
	};

	for (const char* idleName : idleNames)
	{
		for (const SAnimationSource& animationSource : actor.Sources)
		{
			if (std::find(actor.ModelSources.begin(), actor.ModelSources.end(), animationSource.Id) == actor.ModelSources.end() || !CinemaSourcePlayable(animationSource, actor)) continue;
			for (const SAnimationClip& clip : animationSource.Clips)
			{
				if (!clip.Fx && xr_strcmp(clip.Name.c_str(), idleName) == 0)
				{
					if (source != nullptr)
					{
						*source = &animationSource;
					}

					return &clip;
				}
			}
		}
	}

	for (const SAnimationSource& animationSource : actor.Sources)
	{
		if (std::find(actor.ModelSources.begin(), actor.ModelSources.end(), animationSource.Id) == actor.ModelSources.end() || !CinemaSourcePlayable(animationSource, actor)) continue;
		for (const SAnimationClip& clip : animationSource.Clips)
		{
			if (!clip.Fx && CinemaContainsFilter(clip.Name.c_str(), "idle"))
			{
				if (source != nullptr)
				{
					*source = &animationSource;
				}

				return &clip;
			}
		}
	}

	for (const SAnimationSource& animationSource : actor.Sources)
	{
		if (std::find(actor.ModelSources.begin(), actor.ModelSources.end(), animationSource.Id) == actor.ModelSources.end() || !CinemaSourcePlayable(animationSource, actor)) continue;
		for (const SAnimationClip& clip : animationSource.Clips)
		{
			if (!clip.Fx)
			{
				if (source != nullptr)
				{
					*source = &animationSource;
				}

				return &clip;
			}
		}
	}

	return nullptr;
}

static bool CinemaSourcePlayable(const SAnimationSource& source, const SCinemaActor& actor)
{
	if (!actor.Skeleton || !actor.Animated) return false;
	const u16 count = actor.Animated->LL_MotionsSlotCount();
	for (const auto& adapted : actor.AdaptedSources)
	{
		if (adapted.first == source.Id)
		{
			for (u16 slot = 0; slot < count; ++slot) if (actor.Animated->LL_MotionsSlot(slot).id() == adapted.second) return true;
		}
	}
	for (u16 slot = 0; slot < count; ++slot)
	{
		if (xr_strcmp(actor.Animated->LL_MotionsSlot(slot).id().c_str(), source.Id.c_str()) == 0) return true;
	}
	return CinemaIsOmfPath(source.Id) && count < MAX_ANIM_SLOT;
}

static void FillSourceBones(SAnimationSource& source, IKinematics* skeleton)
{
	source.BoneCount = skeleton->LL_BoneCount();
	source.BoneNames.reserve(source.BoneCount);

	if (IKinematics::accel* bones = skeleton->LL_Bones())
	{
		for (const auto& bone : *bones)
		{
			source.BoneNames.push_back(bone.first);
		}
	}
}

static MotionID CinemaFindMotion(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip)
{
	IKinematicsAnimated* kinematics = actor.Animated;
	shared_str sourceId = source.Id;
	for (const auto& adapted : actor.AdaptedSources) if (adapted.first == source.Id) { sourceId = adapted.second; break; }
	if (kinematics == nullptr)
	{
		return MotionID();
	}

	for (u16 slot = 0, count = kinematics->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		shared_motions motions = kinematics->LL_MotionsSlot(slot);

		if (motions.motion_map() == nullptr || xr_strcmp(motions.id().c_str(), sourceId.c_str()) != 0)
		{
			continue;
		}

		accel_map* map = clip.Fx ? motions.fx() : motions.cycle();

		if (map == nullptr)
		{
			continue;
		}

		const auto iterator = map->find(clip.Name);

		if (iterator != map->end())
		{
			return MotionID(slot, iterator->second);
		}
	}

	return MotionID();
}

static CBlend* CinemaFindBlend(IKinematicsAnimated* kinematics, MotionID motion)
{
	if (kinematics == nullptr || !motion.valid())
	{
		return nullptr;
	}

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = kinematics->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = kinematics->LL_PartBlend(part, i);

			if (blend && blend->motionID == motion && blend->blend_state() == CBlend::eAccrue)
			{
				return blend;
			}
		}
	}

	return nullptr;
}

template <typename Func>
static void CinemaForEachBlend(IKinematicsAnimated* kinematics, MotionID motion, const Func& func)
{
	if (kinematics == nullptr || !motion.valid())
	{
		return;
	}

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = kinematics->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = kinematics->LL_PartBlend(part, i);

			if (blend && blend->motionID == motion && blend->blend_state() == CBlend::eAccrue)
			{
				func(*blend);
			}
		}
	}
}

static void CinemaSetBlendTime(IKinematicsAnimated* kinematics, MotionID motion, float time)
{
	CinemaForEachBlend(kinematics, motion, [time](CBlend& blend)
	{
		blend.timeCurrent = time;
	});
}

static void CinemaSetBlendPlaying(IKinematicsAnimated* kinematics, MotionID motion, bool playing)
{
	CinemaForEachBlend(kinematics, motion, [playing](CBlend& blend)
	{
		blend.playing = playing;
	});
}

static void CinemaSetBlendLoop(IKinematicsAnimated* kinematics, MotionID motion, bool loop)
{
	CinemaForEachBlend(kinematics, motion, [loop](CBlend& blend)
	{
		blend.stop_at_end = !loop;
	});
}

static void CinemaSetBlendSpeed(IKinematicsAnimated* kinematics, MotionID motion, float speed)
{
	CinemaForEachBlend(kinematics, motion, [speed](CBlend& blend)
	{
		blend.speed = speed;
	});
}

static void AppendClips(shared_motions motions, xr_vector<SAnimationClip>& clips)
{
	if (accel_map* cycles = motions.cycle())
	{
		for (const auto& pair : *cycles)
		{
			clips.push_back({ .Name = pair.first, .Fx = false });
		}
	}

	if (accel_map* effects = motions.fx())
	{
		for (const auto& pair : *effects)
		{
			clips.push_back({ .Name = pair.first, .Fx = true });
		}
	}
}

static bool VisualPathExists(const char* visual)
{
	if (visual == nullptr || visual[0] == '\0' || xr_strlen(visual) >= 256)
	{
		return false;
	}

	string_path fileName = {};
	xr_strcpy(fileName, visual);

	if (strext(fileName) == nullptr)
	{
		xr_strcat(fileName, ".ogf");
	}

	string_path resolved = {};

	if (FS.exist(resolved, _game_meshes_, fileName))
	{
		return true;
	}

	if (FS.exist(resolved, "$level$", fileName))
	{
		return true;
	}

	return false;
}

static bool CinemaReadString(IReader& reader, xr_string& value)
{
	if (reader.elapsed() <= 0 || !memchr(reader.pointer(), 0, static_cast<size_t>(reader.elapsed())))
	{
		return false;
	}
	reader.r_stringZ(value);
	return true;
}

static bool CinemaIndexAnimationFile(const char* path, SAnimationSource& source)
{
	string_path resolved = {};
	if (!FS.exist(resolved, "$level$", path) && !FS.exist(resolved, _game_meshes_, path)) return false;
	CReaderGuarded file(FS.r_open(resolved));
	if (!file) return false;
	CReaderGuarded params(file->open_chunk(OGF_S_SMPARAMS));
	if (!params || !file->find_chunk(OGF_S_MOTIONS) || params->elapsed() < 4) return false;
	const u16 version = params->r_u16();
	const u16 parts = params->r_u16();
	if (version > xrOGF_SMParamsVersion || parts > MAX_PARTS) return false;
	xr_set<xr_string> names;
	xr_set<u32> indices;
	for (u16 part = 0; part < parts; ++part)
	{
		xr_string name;
		if (!CinemaReadString(*params, name) || params->elapsed() < 2) return false;
		const u16 count = params->r_u16();
		for (u16 bone = 0; bone < count; ++bone)
		{
			if (!CinemaReadString(*params, name) || params->elapsed() < 4 || name.empty()) return false;
			xr_strlwr(name.data());
			const u32 index = params->r_u32();
			if (!names.insert(name).second || !indices.insert(index).second) return false;
			source.BoneNames.emplace_back(name.c_str());
		}
	}
	source.BoneCount = static_cast<u32>(source.BoneNames.size());
	if (source.BoneCount == 0 || params->elapsed() < 2) return false;
	for (u32 index : indices) if (index >= source.BoneCount) return false;
	const u16 count = params->r_u16();
	for (u16 i = 0; i < count; ++i)
	{
		xr_string name;
		if (!CinemaReadString(*params, name) || name.empty() || params->elapsed() < 24) return false;
		xr_strlwr(name.data());
		const u32 flags = params->r_u32();
		params->advance(20);
		if (version >= 4)
		{
			if (params->elapsed() < 4) return false;
			const u32 marks = params->r_u32();
			for (u32 mark = 0; mark < marks; ++mark)
			{
				const char* text = static_cast<const char*>(params->pointer());
				const size_t remaining = static_cast<size_t>(params->elapsed());
				size_t length = 0;
				while (length < remaining && text[length] != '\r' && text[length] != '\n') ++length;
				if (length == remaining) return false;
				while (length < remaining && (text[length] == '\r' || text[length] == '\n')) ++length;
				params->advance(static_cast<u32>(length));
				if (params->elapsed() < 4) return false;
				const u32 intervals = params->r_u32();
				if (intervals > static_cast<u32>(params->elapsed()) / (2 * sizeof(float))) return false;
				params->advance(intervals * 2 * sizeof(float));
			}
		}
		source.Clips.push_back({ .Name = name.c_str(), .Fx = (flags & esmFX) != 0 });
	}
	source.Id = path;
	return !source.Clips.empty();
}

static bool CinemaCheckMotionData(IReader& file, const char* path, const xr_vector<xr_string>& bones, xr_string& error)
{
	CReaderGuarded params(file.open_chunk(OGF_S_SMPARAMS));
	if (!params || params->elapsed() < 4)
	{
		error = xr_string("Invalid animation parameters: ") + path;
		return false;
	}
	const u16 version = params->r_u16();
	const u16 parts = params->r_u16();
	if (version > xrOGF_SMParamsVersion || parts > MAX_PARTS)
	{
		error = xr_string("Unsupported animation parameters: ") + path;
		return false;
	}
	u32 count = 0;
	xr_vector<bool> seenIndices(bones.size(), false);
	xr_set<xr_string> seenBones;
	for (u16 i = 0; i < parts; ++i)
	{
		xr_string name;
		if (!CinemaReadString(*params, name) || params->elapsed() < 2)
		{
			error = xr_string("Invalid animation partition: ") + path;
			return false;
		}
		const u16 partBones = params->r_u16();
		for (u16 j = 0; j < partBones; ++j)
		{
			if (!CinemaReadString(*params, name) || params->elapsed() < 4)
			{
				error = xr_string("Invalid animation bone data: ") + path;
				return false;
			}
			const u32 index = params->r_u32();
			const bool found = std::find(bones.begin(), bones.end(), name) != bones.end();
			if (!found || index >= bones.size() || seenIndices[index] || !seenBones.insert(name).second)
			{
				error = xr_string("Animation ") + path + " is incompatible with the model: bone " + name;
				return false;
			}
			seenIndices[index] = true;
			++count;
		}
	}
	if (count != bones.size())
	{
		error = xr_string("Animation bone count differs from the model: ") + path;
		return false;
	}
	return true;
}

static bool CinemaCheckMotion(const char* path, const xr_vector<xr_string>& bones, xr_string& error)
{
	string_path resolved = {};
	if (!FS.exist(resolved, "$level$", path) && !FS.exist(resolved, _game_meshes_, path))
	{
		error = xr_string("Animation file not found: ") + path;
		return false;
	}
	CReaderGuarded file(FS.r_open(resolved));
	if (!file)
	{
		error = xr_string("Could not read animation file: ") + path;
		return false;
	}
	return CinemaCheckMotionData(*file, path, bones, error);
}

static bool CinemaCheckMotionPath(const char* path, const xr_vector<xr_string>& bones, xr_string& error)
{
	if (strstr(path, "\\*.omf"))
	{
		FS_FileSet files;
		FS.file_list(files, _game_meshes_, FS_ListFiles, path);
		FS.file_list(files, "$level$", FS_ListFiles, path);
		if (files.empty())
		{
			error = xr_string("No animation files found: ") + path;
			return false;
		}
		for (const FS_File& file : files)
		{
			if (!CinemaCheckMotion(file.name.c_str(), bones, error))
			{
				return false;
			}
		}
		return true;
	}
	return CinemaCheckMotion((xr_string(path) + ".omf").c_str(), bones, error);
}

static bool CinemaCheckVisual(const char* visual, xr_string& error)
{
	if (visual == nullptr || visual[0] == '\0' || xr_strlen(visual) >= 256)
	{
		error = "Invalid model path.";
		return false;
	}

	string_path fileName = {};
	xr_strcpy(fileName, visual);
	if (!strext(fileName))
	{
		xr_strcat(fileName, ".ogf");
	}
	string_path resolved = {};
	if (!FS.exist(resolved, _game_meshes_, fileName) && !FS.exist(resolved, "$level$", fileName))
	{
		error = xr_string("Model not found: ") + visual;
		return false;
	}
	CReaderGuarded file(FS.r_open(resolved));
	if (!file)
	{
		error = xr_string("Could not read model: ") + visual;
		return false;
	}
	CReaderGuarded boneChunk(file->open_chunk(OGF_S_BONE_NAMES));
	if (!boneChunk)
	{
		return true;
	}
	if (boneChunk->elapsed() < 4)
	{
		error = xr_string("Invalid model bones: ") + visual;
		return false;
	}
	const u32 count = boneChunk->r_u32();
	if (count > 65535)
	{
		error = xr_string("Invalid model bone count: ") + visual;
		return false;
	}
	xr_vector<xr_string> bones;
	for (u32 i = 0; i < count; ++i)
	{
		xr_string name;
		xr_string parent;
		if (!CinemaReadString(*boneChunk, name) || !CinemaReadString(*boneChunk, parent) || boneChunk->elapsed() < sizeof(Fobb))
		{
			error = xr_string("Invalid model bones: ") + visual;
			return false;
		}
		xr_strlwr(name.data());
		bones.push_back(std::move(name));
		boneChunk->advance(sizeof(Fobb));
	}
	CReaderGuarded refs(file->open_chunk(OGF_S_MOTION_REFS));
	if (refs)
	{
		xr_string list;
		if (!CinemaReadString(*refs, list))
		{
			error = xr_string("Invalid model animation references: ") + visual;
			return false;
		}
		for (u32 i = 0, countRefs = _GetItemCount(list.c_str()); i < countRefs; ++i)
		{
			string_path path = {};
			_GetItem(list.c_str(), i, path);
			if (!CinemaCheckMotionPath(path, bones, error))
			{
				return false;
			}
		}
	}
	else
	{
		CReaderGuarded refs2(file->open_chunk(OGF_S_MOTION_REFS2));
		if (refs2)
		{
			if (refs2->elapsed() < 4)
			{
				error = xr_string("Invalid model animation references: ") + visual;
				return false;
			}
			const u32 countRefs = refs2->r_u32();
			for (u32 i = 0; i < countRefs; ++i)
			{
				xr_string path;
				if (!CinemaReadString(*refs2, path) || !CinemaCheckMotionPath(path.c_str(), bones, error))
				{
					if (error.empty())
					{
						error = xr_string("Invalid model animation references: ") + visual;
					}
					return false;
				}
			}
		}
		else if (file->find_chunk(OGF_S_SMPARAMS) && !CinemaCheckMotionData(*file, visual, bones, error))
		{
			return false;
		}
	}
	return true;
}

static bool CinemaSkipMotionMarks(IReader& reader, u32 marks)
{
	for (u32 mark = 0; mark < marks; ++mark)
	{
		const char* text = static_cast<const char*>(reader.pointer());
		const size_t remaining = static_cast<size_t>(reader.elapsed());
		size_t length = 0;
		while (length < remaining && text[length] != '\r' && text[length] != '\n') ++length;
		if (length == remaining) return false;
		while (length < remaining && (text[length] == '\r' || text[length] == '\n')) ++length;
		reader.advance(static_cast<intptr_t>(length));
		if (reader.elapsed() < 4) return false;
		const u32 intervals = reader.r_u32();
		if (intervals > static_cast<u32>(reader.elapsed()) / (2 * sizeof(float))) return false;
		reader.advance(intervals * 2 * sizeof(float));
	}
	return true;
}

static bool CinemaReadMotionChannel(IReader& reader, u32 frames, const u8*& data, u32& size)
{
	if (reader.elapsed() < 1) return false;
	data = static_cast<const u8*>(reader.pointer());
	const u8 flags = reader.r_u8();
	const bool uncompressed = (flags & flTKeyFFT_Bit) != 0;
	const u64 rotationSize = uncompressed ? sizeof(CKeyQR32) : sizeof(CKeyQR);
	const u64 positionSize = uncompressed ? sizeof(CKeyQT32) : (flags & flTKey16IsBit) ? sizeof(CKeyQT16) : sizeof(CKeyQT8);
	u64 bytes = (flags & flRKeyAbsent) ? rotationSize : 4 + rotationSize * frames;
	if (flags & flTKeyPresent) bytes += 4 + positionSize * frames + (uncompressed ? 0 : 2 * sizeof(Fvector));
	else bytes += sizeof(Fvector);
	if (bytes > static_cast<u64>(reader.elapsed()) || bytes >= u64(u32(-1))) return false;
	reader.advance(static_cast<intptr_t>(bytes));
	size = static_cast<u32>(bytes) + 1;
	return true;
}

static bool CinemaBuildAdaptedMotion(const SCinemaActor& actor, const char* sourcePath, CMemoryWriter& output, xr_string& error)
{
	string_path resolved = {};
	if (!actor.Skeleton || !actor.Animated || !actor.Skeleton->LL_Bones() ||
		(!FS.exist(resolved, "$level$", sourcePath) && !FS.exist(resolved, _game_meshes_, sourcePath)))
	{
		error = "Animation or target skeleton is unavailable.";
		return false;
	}
	CReaderGuarded file(FS.r_open(resolved));
	if (!file || file->length() > 256 * 1024 * 1024)
	{
		error = "Unable to read the animation, or the file exceeds 256 MiB.";
		return false;
	}
	CReaderGuarded params(file->open_chunk(OGF_S_SMPARAMS));
	CReaderGuarded motions(file->open_chunk(OGF_S_MOTIONS));
	if (!params || !motions || params->elapsed() < 4) { error = "Invalid animation chunks."; return false; }
	const u16 version = params->r_u16();
	const u16 partCount = params->r_u16();
	if (version > xrOGF_SMParamsVersion || partCount > MAX_PARTS) { error = "Unsupported animation format."; return false; }
	xr_vector<xr_string> sourceNames;
	xr_vector<xr_string> partNames;
	for (u16 part = 0; part < partCount; ++part)
	{
		xr_string name;
		if (!CinemaReadString(*params, name) || name.size() >= 128 || params->elapsed() < 2) { error = "Invalid animation partition."; return false; }
		partNames.push_back(name);
		const u16 count = params->r_u16();
		for (u16 i = 0; i < count; ++i)
		{
			if (!CinemaReadString(*params, name) || name.empty() || name.size() >= 128 || params->elapsed() < 4) { error = "Invalid animation bone."; return false; }
			xr_strlwr(name.data());
			const u32 index = params->r_u32();
			if (index >= 65535) { error = "Invalid animation bone index."; return false; }
			if (sourceNames.size() <= index) sourceNames.resize(index + 1);
			if (!sourceNames[index].empty()) { error = "Duplicate animation bone index."; return false; }
			sourceNames[index] = name;
		}
	}
	if (sourceNames.empty() || params->elapsed() < 2) { error = "Animation has no bones."; return false; }
	for (const auto& name : sourceNames) if (name.empty()) { error = "Incomplete animation bone indices."; return false; }
	const u16 targetCount = actor.Skeleton->LL_BoneCount();
	if (!targetCount) { error = "Target model has no bones."; return false; }
	xr_vector<shared_str> targetNames(targetCount);
	for (const auto& bone : *actor.Skeleton->LL_Bones())
	{
		if (bone.second >= targetCount || !bone.first.size() || bone.first.size() >= 128) { error = "Invalid target skeleton."; return false; }
		targetNames[bone.second] = bone.first;
	}
	xr_vector<s32> targetToSource(targetCount, -1);
	for (u32 i = 0; i < targetCount; ++i)
	{
		if (!targetNames[i].size()) { error = "Incomplete target skeleton."; return false; }
		for (u32 source = 0; source < sourceNames.size(); ++source)
		{
			if (_stricmp(targetNames[i].c_str(), sourceNames[source].c_str()) == 0) { targetToSource[i] = static_cast<s32>(source); break; }
		}
	}
	const u16 definitions = params->r_u16();
	CReaderGuarded countChunk(motions->open_chunk(0));
	if (!countChunk || countChunk->elapsed() < 4) { error = "Invalid animation count."; return false; }
	const u32 motionCount = countChunk->r_u32();
	if (motionCount != definitions || motionCount >= 0x3fff) { error = "Animation definitions do not match the motion data."; return false; }
	output.open_chunk(OGF_S_SMPARAMS);
	output.w_u16(version);
	output.w_u16(1);
	output.w_stringZ("base");
	output.w_u16(targetCount);
	for (u32 i = 0; i < targetCount; ++i) { output.w_stringZ(targetNames[i]); output.w_u32(i); }
	output.w_u16(definitions);
	for (u32 i = 0; i < definitions; ++i)
	{
		xr_string name;
		if (!CinemaReadString(*params, name) || name.empty() || name.size() >= 128 || params->elapsed() < 24) { error = "Invalid animation definition."; return false; }
		const u32 flags = params->r_u32();
		const u16 sourcePart = params->r_u16();
		u16 targetPart = sourcePart == BI_NONE ? BI_NONE : 0;
		if (flags & esmFX)
		{
			targetPart = BI_NONE;
			if (sourcePart < sourceNames.size()) targetPart = actor.Skeleton->LL_BoneID(sourceNames[sourcePart].c_str());
			if (targetPart == BI_NONE || targetPart >= targetCount) targetPart = actor.Skeleton->LL_GetBoneRoot();
		}
		else if (sourcePart < partNames.size())
		{
			const CPartition& partitions = actor.Animated->partitions();
			for (u16 part = 0; part < MAX_PARTS; ++part)
			{
				if (partitions.part(part).Name.size() && _stricmp(partitions.part(part).Name.c_str(), partNames[sourcePart].c_str()) == 0) { targetPart = part; break; }
			}
		}
		output.w_stringZ(name);
		output.w_u32(flags);
		output.w_u16(targetPart);
		output.w(params->pointer(), 18);
		params->advance(18);
		if (version >= 4)
		{
			if (params->elapsed() < 4) { error = "Invalid animation marks."; return false; }
			const u32 marks = params->r_u32();
			const u8* start = static_cast<const u8*>(params->pointer());
			if (!CinemaSkipMotionMarks(*params, marks)) { error = "Invalid animation marks."; return false; }
			output.w_u32(marks);
			output.w(start, static_cast<u32>(static_cast<const u8*>(params->pointer()) - start));
		}
	}
	output.close_chunk();
	output.open_chunk(OGF_S_MOTIONS);
	output.open_chunk(0);
	output.w_u32(motionCount);
	output.close_chunk();
	for (u32 motion = 0; motion < motionCount; ++motion)
	{
		CReaderGuarded input(motions->open_chunk(motion + 1));
		xr_string name;
		if (!input || !CinemaReadString(*input, name) || name.empty() || name.size() >= 128 || input->elapsed() < 4) { error = "Invalid animation frames."; return false; }
		const u32 frames = input->r_u32();
		if (!frames) { error = "Animation has no frames."; return false; }
		xr_vector<const u8*> channels(sourceNames.size(), nullptr);
		xr_vector<u32> sizes(sourceNames.size(), 0);
		for (size_t bone = 0; bone < sourceNames.size(); ++bone)
		{
			if (!CinemaReadMotionChannel(*input, frames, channels[bone], sizes[bone])) { error = "Invalid animation bone frames."; return false; }
		}
		output.open_chunk(motion + 1);
		output.w_stringZ(name);
		output.w_u32(frames);
		for (u32 bone = 0; bone < targetCount; ++bone)
		{
			const s32 source = targetToSource[bone];
			if (source >= 0) output.w(channels[source], sizes[source]);
			else
			{
				const Fmatrix& bind = actor.Skeleton->GetBoneData(static_cast<u16>(bone)).get_bind_transform();
				Fquaternion q = Fquaternion().set(bind).normalize();
				const CKeyQR32 rotation{ q.x, q.y, q.z, q.w };
				output.w_u8(flTKeyFFT_Bit | flRKeyAbsent);
				output.w(&rotation, sizeof(rotation));
				output.w_fvector3(bind.c);
			}
		}
		output.close_chunk();
	}
	for (u32 motion = 0; motion < motionCount; ++motion)
	{
		CReaderGuarded input(motions->open_chunk(motionCount + 2 + motion));
		if (!input) continue;
		if (input->elapsed() < 4) { error = "Invalid animation notifications."; return false; }
		const u32 boneCount = input->r_u32();
		CMemoryWriter notifications;
		u32 mappedBones = 0;
		for (u32 bone = 0; bone < boneCount; ++bone)
		{
			if (input->elapsed() < 6) { error = "Invalid animation notifications."; return false; }
			const u16 sourceBone = input->r_u16();
			const u32 eventCount = input->r_u32();
			const u8* start = static_cast<const u8*>(input->pointer());
			for (u32 event = 0; event < eventCount; ++event)
			{
				if (input->elapsed() < 8) { error = "Invalid animation notifications."; return false; }
				input->advance(4);
				const u32 names = input->r_u32();
				for (u32 i = 0; i < names; ++i)
				{
					xr_string name;
					if (!CinemaReadString(*input, name)) { error = "Invalid animation notifications."; return false; }
				}
			}
			const auto target = std::find(targetToSource.begin(), targetToSource.end(), static_cast<s32>(sourceBone));
			if (target != targetToSource.end())
			{
				notifications.w_u16(static_cast<u16>(target - targetToSource.begin()));
				notifications.w_u32(eventCount);
				notifications.w(start, static_cast<u32>(static_cast<const u8*>(input->pointer()) - start));
				++mappedBones;
			}
		}
		output.open_chunk(motionCount + 2 + motion);
		output.w_u32(mappedBones);
		if (notifications.size()) output.w(notifications.pointer(), notifications.size());
		output.close_chunk();
	}
	output.close_chunk();
	return true;
}

static void ReleaseActorModel(SCinemaActor& actor)
{
	CinemaClearBindPose(actor);
	actor.Object.Unbind();

	if (actor.Model && ::Render)
	{
		::Render->model_Delete(actor.Model);
	}

	actor.Model = nullptr;
	actor.Skeleton = nullptr;
	actor.Animated = nullptr;
	actor.LocalBox.invalidate();
	actor.Sources.clear();
	actor.ModelSources.clear();
	actor.AdaptedSources.clear();
}

static void ReleaseActorItem(SCinemaActor& actor)
{
	actor.Object.ItemVisual = nullptr;
	actor.ItemSection = "";
	actor.ItemBoneName = "";
	actor.ItemWeaponStyle = false;
	actor.ItemHandDependence = 2;
	actor.ItemBoneR = u16(-1);
	actor.ItemBoneL = u16(-1);
	actor.ItemBoneR2 = u16(-1);
	actor.ItemBoneAttach = u16(-1);
	actor.ItemPosition.set(0.0f, 0.0f, 0.0f);
	actor.ItemRotation.set(0.0f, 0.0f, 0.0f);

	if (actor.ItemModel != nullptr && ::Render != nullptr)
	{
		::Render->model_Delete(actor.ItemModel);
	}

	actor.ItemModel = nullptr;
}

static void UpdateActorItemPlacement(SCinemaActor& actor)
{
	actor.Object.ItemVisual = nullptr;

	if (actor.ItemModel == nullptr || actor.Skeleton == nullptr)
	{
		return;
	}

	const Fmatrix& placement = actor.Object.renderable.xform;
	Fmatrix transform;

	if (actor.ItemWeaponStyle)
	{
		const u16 boneR = actor.ItemBoneR;
		const u16 boneL = actor.ItemHandDependence == 1 ? actor.ItemBoneR2 : actor.ItemBoneL;

		if (boneR == u16(-1) || boneL == u16(-1))
		{
			return;
		}

		const Fmatrix& mL = actor.Skeleton->LL_GetTransform(boneL);
		const Fmatrix& mR = actor.Skeleton->LL_GetTransform(boneR);

		Fvector direction = {};
		direction.sub(mL.c, mR.c);

		if (fis_zero(direction.magnitude()))
		{
			transform = placement;
			placement.transform_tiny(transform.c, mR.c);
		}
		else
		{
			direction.normalize();

			Fvector right = {};
			right.crossproduct(mR.j, direction);
			right.normalize();

			Fvector normal = {};
			normal.crossproduct(direction, right);
			normal.normalize();

			transform.set(right, normal, direction, mR.c);
			transform.mulA_43(placement);
		}

		Fmatrix offset;
		offset.setHPB(
			actor.ItemRotation.x * PI / 180.0f,
			actor.ItemRotation.y * PI / 180.0f,
			actor.ItemRotation.z * PI / 180.0f);
		offset.translate_over(actor.ItemPosition);

		actor.Object.ItemTransform.mul(transform, offset);
	}
	else
	{
		if (actor.ItemBoneAttach == u16(-1))
		{
			return;
		}

		Fmatrix boneMatrix;
		actor.Skeleton->Bone_GetAnimPos(boneMatrix, actor.ItemBoneAttach, u8(-1), false);

		Fmatrix offset;
		offset.setHPB(actor.ItemRotation.x, actor.ItemRotation.y, actor.ItemRotation.z);
		offset.translate_over(actor.ItemPosition);

		transform.mul_43(boneMatrix, offset);
		transform.mulA_43(placement);
		actor.Object.ItemTransform = transform;
	}

	actor.Object.ItemVisual = actor.ItemModel;
}

static void UpdateActorPlacement(SCinemaActor& actor)
{
	if (actor.Skeleton != nullptr)
	{
		actor.Skeleton->CalculateBones(true);
	}

	Fmatrix transform = actor.Orientation;
	transform.c = actor.Position;
	transform.i.mul(actor.Scale.x);
	transform.j.mul(actor.Scale.y);
	transform.k.mul(actor.Scale.z);
	actor.Object.Bind(actor.Model, transform);

	UpdateActorItemPlacement(actor);
}

static Fvector AxisVector(EGizmoAxis axis)
{
	switch (axis)
	{
	case EGizmoAxis::X: return Fvector().set(1.0f, 0.0f, 0.0f);
	case EGizmoAxis::Y: return Fvector().set(0.0f, 1.0f, 0.0f);
	case EGizmoAxis::Z: return Fvector().set(0.0f, 0.0f, 1.0f);
	default: return Fvector().set(0.0f, 0.0f, 0.0f);
	}
}

static void RotationBasis(EGizmoAxis axis, Fvector& u, Fvector& v)
{
	switch (axis)
	{
	case EGizmoAxis::X:
	{
		u.set(0.0f, 1.0f, 0.0f);
		v.set(0.0f, 0.0f, 1.0f);
		break;
	}
	case EGizmoAxis::Y:
	{
		u.set(1.0f, 0.0f, 0.0f);
		v.set(0.0f, 0.0f, -1.0f);
		break;
	}
	case EGizmoAxis::Z:
	{
		u.set(1.0f, 0.0f, 0.0f);
		v.set(0.0f, 1.0f, 0.0f);
		break;
	}
	default:
	{
		u.set(1.0f, 0.0f, 0.0f);
		v.set(0.0f, 1.0f, 0.0f);
		break;
	}
	}
}

static SProjectedPoint ProjectWorld(const Fvector& world)
{
	SProjectedPoint result;
	Fvector4 clip = {};
	Device.mFullTransform.transform(clip, world);

	result.W = clip.w;

	if (clip.w <= 0.0001f)
	{
		return result;
	}

	const ImGuiViewport* Viewport = ImGui::GetMainViewport();
	result.Screen.x = Viewport->Pos.x + (clip.x + 1.0f) * 0.5f * Viewport->Size.x;
	result.Screen.y = Viewport->Pos.y + (1.0f - clip.y) * 0.5f * Viewport->Size.y;
	result.Visible = true;
	return result;
}

static bool MouseRay(Fvector& origin, Fvector& direction)
{
	const ImGuiIO& io = ImGui::GetIO();
	const ImGuiViewport* Viewport = ImGui::GetMainViewport();

	if (Viewport->Size.x <= 0.0f || Viewport->Size.y <= 0.0f)
	{
		return false;
	}

	const float ndcX = ((io.MousePos.x - Viewport->Pos.x) / Viewport->Size.x) * 2.0f - 1.0f;
	const float ndcY = 1.0f - ((io.MousePos.y - Viewport->Pos.y) / Viewport->Size.y) * 2.0f;

	Fvector nearPoint = {};
	Fvector farPoint = {};
	Device.mInvFullTransform.transform(nearPoint, Fvector().set(ndcX, ndcY, 0.0f));
	Device.mInvFullTransform.transform(farPoint, Fvector().set(ndcX, ndcY, 1.0f));

	direction.sub(farPoint, nearPoint);

	if (direction.square_magnitude() < EPS)
	{
		return false;
	}

	direction.normalize();
	origin.set(nearPoint);
	return true;
}

static bool RaySphere(const Fvector& origin, const Fvector& direction, const Fvector& center, float radius, float& distance)
{
	Fvector offset = {};
	offset.sub(origin, center);

	const float b = offset.dotproduct(direction);
	const float c = offset.square_magnitude() - radius * radius;
	const float discriminant = b * b - c;

	if (discriminant < 0.0f)
	{
		return false;
	}

	const float root = sqrtf(discriminant);
	float nearDistance = -b - root;

	if (nearDistance < 0.0f)
	{
		nearDistance = -b + root;
	}

	if (nearDistance < 0.0f)
	{
		return false;
	}

	distance = nearDistance;
	return true;
}

static bool RayPlaneIntersect(const Fvector& origin, const Fvector& direction, const Fvector& point, const Fvector& normal, Fvector& intersection)
{
	const float denominator = direction.dotproduct(normal);

	if (fabsf(denominator) < 1e-4f)
	{
		return false;
	}

	Fvector offset = {};
	offset.sub(point, origin);

	const float distance = offset.dotproduct(normal) / denominator;

	if (distance < 0.0f)
	{
		return false;
	}

	intersection.mad(origin, direction, distance);
	return true;
}

static bool ClosestPointOnAxis(const Fvector& point, const Fvector& axis, const Fvector& origin, const Fvector& direction, float& parameter)
{
	Fvector offset = {};
	offset.sub(point, origin);

	const float b = axis.dotproduct(direction);
	const float d = axis.dotproduct(offset);
	const float e = direction.dotproduct(offset);
	const float denominator = 1.0f - b * b;

	if (fabsf(denominator) < 1e-5f)
	{
		return false;
	}

	parameter = (b * e - d) / denominator;
	return true;
}

static float DistanceToSegment(const ImVec2& point, const ImVec2& a, const ImVec2& b)
{
	const float dx = b.x - a.x;
	const float dy = b.y - a.y;
	const float lengthSquared = dx * dx + dy * dy;

	if (lengthSquared <= 1e-6f)
	{
		const float px = point.x - a.x;
		const float py = point.y - a.y;
		return sqrtf(px * px + py * py);
	}

	float t = ((point.x - a.x) * dx + (point.y - a.y) * dy) / lengthSquared;
	t = std::clamp(t, 0.0f, 1.0f);

	const float cx = a.x + dx * t;
	const float cy = a.y + dy * t;
	const float px = point.x - cx;
	const float py = point.y - cy;
	return sqrtf(px * px + py * py);
}

static bool ComputeMouseAngle(const Fvector& pivot, EGizmoAxis axis, float& angle, bool& screenFallback)
{
	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		return false;
	}

	const Fvector normal = AxisVector(axis);
	Fvector intersection = {};

	if (RayPlaneIntersect(origin, direction, pivot, normal, intersection))
	{
		Fvector offset = {};
		offset.sub(intersection, pivot);
		offset.mad(normal, -offset.dotproduct(normal));

		if (offset.square_magnitude() > 1e-6f)
		{
			offset.normalize();

			Fvector u = {};
			Fvector v = {};
			RotationBasis(axis, u, v);
			angle = atan2f(offset.dotproduct(v), offset.dotproduct(u));
			screenFallback = false;
			return true;
		}
	}

	const SProjectedPoint center = ProjectWorld(pivot);

	if (!center.Visible)
	{
		return false;
	}

	const ImGuiIO& io = ImGui::GetIO();
	angle = atan2f(io.MousePos.y - center.Screen.y, io.MousePos.x - center.Screen.x);
	screenFallback = true;
	return true;
}

static void SortSegmentList(xr_vector<STrimSegment>& segments)
{
	std::sort(segments.begin(), segments.end(), [](const STrimSegment& left, const STrimSegment& right)
	{
		return left.Start < right.Start;
	});
}

static bool DrawAnimationTimeline(const char* id, float& time, float total, const CMotionDef* motionDef, const SEditorTheme& theme,
	STrimRange* trim, s32& dragMarker, s32& dragIndex, xr_string& dragKey, const char* trimKey, bool canShoot)
{
	const float width = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
	constexpr float height = 64.0f;
	const ImVec2 origin = ImGui::GetCursorScreenPos();
	const ImVec2 size = ImVec2(width, height);
	const ImVec2 end = ImVec2(origin.x + size.x, origin.y + size.y);

	ImGui::InvisibleButton(id, size);

	bool changed = false;

	const float pixelsPerSecond = total > 0.0f ? width / total : 0.0f;
	const ImVec2 mouse = ImGui::GetIO().MousePos;
	const bool ctrlHeld = ImGui::GetIO().KeyCtrl;
	const bool shiftHeld = ImGui::GetIO().KeyShift;

	const auto timeToX = [&](float value)
	{
		return origin.x + std::clamp(value, 0.0f, total) * pixelsPerSecond;
	};

	const auto xToTime = [&](float x)
	{
		return std::clamp((x - origin.x) / width * total, 0.0f, total);
	};

	if (total > 0.0f && trim != nullptr)
	{
		s32 hoverSegment = -1;
		s32 hoverFlag = 0;
		float hoverDistance = 8.0f;

		for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
		{
			const STrimSegment& segment = trim->Segments[index];
			const float distanceIn = fabsf(mouse.x - timeToX(segment.Start));
			const float distanceOut = fabsf(mouse.x - timeToX(segment.End));

			if (distanceIn < hoverDistance)
			{
				hoverDistance = distanceIn;
				hoverSegment = index;
				hoverFlag = 1;
			}

			if (distanceOut < hoverDistance)
			{
				hoverDistance = distanceOut;
				hoverSegment = index;
				hoverFlag = 2;
			}
		}

		if (ImGui::IsItemHovered())
		{
			if (dragMarker == 0 && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
			{
				if (shiftHeld && !ctrlHeld && canShoot && trim->Shots.size() < kCinemaMaxShots)
				{
					trim->Shots.push_back(std::min(xToTime(mouse.x), std::max(0.0f, total - END_EPS)));
				}
				else if (ctrlHeld)
				{
					STrimSegment segment;
					segment.Start = xToTime(mouse.x);
					segment.End = std::min(segment.Start + 0.2f, total);

					for (const STrimSegment& existing : trim->Segments)
					{
						if (segment.End > existing.Start && segment.Start < existing.End)
						{
							segment.End = existing.Start - 0.01f;
						}
					}

					if (segment.End - segment.Start >= 0.02f && trim->Segments.size() < kCinemaMaxSegments)
					{
						trim->Segments.push_back(segment);
						SortSegmentList(trim->Segments);

						for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
						{
							if (trim->Segments[index].Start == segment.Start && trim->Segments[index].End == segment.End)
							{
								dragIndex = index;
								break;
							}
						}

						dragMarker = 3;
						dragKey = trimKey;
					}
				}
				else if (hoverFlag != 0)
				{
					dragMarker = hoverFlag;
					dragIndex = hoverSegment;
					dragKey = trimKey;
				}
			}

			if (dragMarker == 0 && ImGui::IsMouseClicked(ImGuiMouseButton_Right))
			{
				const float value = xToTime(mouse.x);

				for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
				{
					const STrimSegment& segment = trim->Segments[index];

					if (value >= segment.Start && value <= segment.End)
					{
						trim->Segments.erase(trim->Segments.begin() + index);
						break;
					}
				}
			}
		}

		if (!ImGui::IsMouseDown(ImGuiMouseButton_Left) && dragMarker != 0)
		{
			dragMarker = 0;
			dragIndex = -1;
			dragKey.clear();
		}

		if (dragMarker != 0 && dragKey == trimKey && dragIndex >= 0 && dragIndex < static_cast<s32>(trim->Segments.size()))
		{
			STrimSegment& segment = trim->Segments[dragIndex];
			const float value = xToTime(mouse.x);
			const float lowerBound = dragIndex > 0 ? trim->Segments[dragIndex - 1].End : 0.0f;
			const float upperBound = dragIndex + 1 < static_cast<s32>(trim->Segments.size()) ? trim->Segments[dragIndex + 1].Start : total;

			if (dragMarker == 1)
			{
				segment.Start = std::clamp(value, lowerBound, std::max(lowerBound, segment.End - 0.02f));
			}
			else
			{
				segment.End = std::clamp(value, segment.Start + 0.02f, std::max(segment.Start + 0.02f, upperBound));
			}
		}

		if (dragMarker == 0 && ImGui::IsItemActive() && !shiftHeld)
		{
			time = xToTime(mouse.x);
			changed = true;
		}
	}

	ImDrawList* draw = ImGui::GetWindowDrawList();
	const ImU32 colorBackground = theme.TimelineBackground;
	const ImU32 colorElapsed = theme.TimelineElapsed;
	const ImU32 colorMark = theme.TimelineMark;
	const ImU32 colorTickMinor = theme.TimelineTickMinor;
	const ImU32 colorTickMajor = theme.TimelineTickMajor;
	const ImU32 colorText = theme.TimelineText;
	const ImU32 colorPlayhead = theme.TimelinePlayhead;
	const ImU32 colorBorder = theme.TimelineBorder;
	const ImU32 colorTrimIn = IM_COL32(120, 230, 150, 240);
	const ImU32 colorTrimOut = IM_COL32(255, 150, 80, 240);

	draw->AddRectFilled(origin, end, colorBackground);

	if (total > 0.0f)
	{
		const float playedX = origin.x + std::clamp(time, 0.0f, total) * pixelsPerSecond;

		draw->AddRectFilled(ImVec2(origin.x + 1.0f, origin.y + 1.0f), ImVec2(playedX, origin.y + height - 1.0f), colorElapsed);

		if (motionDef)
		{
			for (const motion_marks& mark : motionDef->marks)
			{
				for (const motion_marks::interval& interval : mark.intervals)
				{
					const float x0 = origin.x + std::max(0.0f, interval.first) * pixelsPerSecond;
					const float x1 = origin.x + std::max(0.0f, interval.second) * pixelsPerSecond;
					draw->AddRectFilled(ImVec2(x0, origin.y + 1.0f), ImVec2(std::max(x1, x0 + 1.0f), origin.y + height - 1.0f), colorMark);
				}
			}
		}

		const float steps[] = { 0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1.0f, 2.0f, 5.0f, 10.0f, 30.0f, 60.0f };
		float step = steps[IM_ARRAYSIZE(steps) - 1];

		for (const float candidate : steps)
		{
			if (candidate * pixelsPerSecond >= 64.0f)
			{
				step = candidate;
				break;
			}
		}

		const int majorCount = std::max(1, iFloor(total / step + 0.5f));
		string32 label = {};

		for (int major = 0; major <= majorCount; ++major)
		{
			const float majorTime = major * step;
			const float majorX = origin.x + majorTime * pixelsPerSecond;

			draw->AddLine(ImVec2(majorX, origin.y + 1.0f), ImVec2(majorX, origin.y + height - 1.0f), colorTickMajor);

			if (major < majorCount)
			{
				xr_sprintf(label, step >= 1.0f ? "%.1f" : "%.2f", majorTime);
				draw->AddText(ImVec2(majorX + 3.0f, origin.y + 3.0f), colorText, label);

				for (int minor = 1; minor < 5; ++minor)
				{
					const float minorTime = majorTime + step * minor / 5.0f;

					if (minorTime > total)
					{
						break;
					}

					const float minorX = origin.x + minorTime * pixelsPerSecond;
					draw->AddLine(ImVec2(minorX, origin.y + height - 10.0f), ImVec2(minorX, origin.y + height - 1.0f), colorTickMinor);
				}
			}
		}

		if (trim != nullptr && !trim->Segments.empty())
		{
			float cursor = 0.0f;

			for (const STrimSegment& segment : trim->Segments)
			{
				if (segment.Start > cursor)
				{
					draw->AddRectFilled(ImVec2(timeToX(cursor), origin.y + 1.0f), ImVec2(timeToX(segment.Start), origin.y + height - 1.0f), IM_COL32(0, 0, 0, 120));
				}

				draw->AddRectFilled(ImVec2(timeToX(segment.Start), origin.y + 1.0f), ImVec2(timeToX(segment.End), origin.y + height - 1.0f), IM_COL32(255, 255, 255, 14));
				cursor = std::max(cursor, segment.End);
			}

			if (cursor < total)
			{
				draw->AddRectFilled(ImVec2(timeToX(cursor), origin.y + 1.0f), ImVec2(end.x - 1.0f, origin.y + height - 1.0f), IM_COL32(0, 0, 0, 120));
			}
		}

		draw->AddLine(ImVec2(playedX, origin.y), ImVec2(playedX, origin.y + height), colorPlayhead, 2.0f);
		draw->AddTriangleFilled(ImVec2(playedX - 5.0f, origin.y), ImVec2(playedX + 5.0f, origin.y), ImVec2(playedX, origin.y + 7.0f), colorPlayhead);

		if (trim != nullptr)
		{
			for (const float shot : trim->Shots)
			{
				const float shotX = timeToX(shot);
				draw->AddLine(ImVec2(shotX, origin.y + 9.0f), ImVec2(shotX, origin.y + height - 9.0f), IM_COL32(255, 75, 75, 255), 2.0f);
				draw->AddCircleFilled(ImVec2(shotX, origin.y + height - 5.0f), 4.0f, IM_COL32(255, 75, 75, 255));
			}
			for (const STrimSegment& segment : trim->Segments)
			{
				const float inX = timeToX(segment.Start);
				const float outX = timeToX(segment.End);

				draw->AddLine(ImVec2(inX, origin.y), ImVec2(inX, origin.y + height), colorTrimIn, 2.0f);
				draw->AddTriangleFilled(ImVec2(inX, origin.y), ImVec2(inX + 7.0f, origin.y), ImVec2(inX, origin.y + 7.0f), colorTrimIn);

				draw->AddLine(ImVec2(outX, origin.y), ImVec2(outX, origin.y + height), colorTrimOut, 2.0f);
				draw->AddTriangleFilled(ImVec2(outX, origin.y + height), ImVec2(outX, origin.y + height - 7.0f), ImVec2(outX - 7.0f, origin.y + height), colorTrimOut);
			}
		}

		if (ImGui::IsItemHovered())
		{
			const float hoverTime = xToTime(mouse.x);
			if (trim)
			{
				ImGui::SetTooltip("Time: %.3f s\nFrame: %d\nCtrl+drag - add segment, drag a flag - move it, right click on a segment - delete%s", hoverTime, iFloor(hoverTime / SAMPLE_SPF + 0.5f), canShoot ? "\nShift+click - add shot" : "");
			}
			else
			{
				ImGui::SetTooltip("Time: %.3f s", hoverTime);
			}
		}
	}
	else
	{
		const char* message = "no animation";
		const ImVec2 messageSize = ImGui::CalcTextSize(message);
		draw->AddText(ImVec2(origin.x + (width - messageSize.x) * 0.5f, origin.y + (height - messageSize.y) * 0.5f), colorText, message);
	}

	draw->AddRect(origin, end, colorBorder);
	return changed;
}


struct SCinemaCurveEditorState
{
	bool Open = false;
	bool InitializedChannels[15] = {};
	s32 Type = 0;
	s32 Object = -1;
	s32 Layer = -1;
	s32 Channel = 0;
	s32 SelectedKey = -1;
	s32 DragPart = 0;
	bool Fit = true;
	bool Snap = false;
	bool ShowResult = true;
	float TimeStep = 0.1f;
	float ValueStep = 0.1f;
	float TimeMin = 0.0f;
	float TimeMax = 1.0f;
	float ValueMin = -1.0f;
	float ValueMax = 1.0f;
};

class CCinemaFilmMakerManager final : public pureFrame
{
public:
	virtual ~CCinemaFilmMakerManager() = default;

	enum class ESceneDialog : u8
	{
		None,
		Info,
		Save,
		Load,
		Confirm,
		Preset
	};

	void OnFrame() override;
	void Render();

	void DrawMasterTimeline(bool& state);
	void DrawCurveEditor();
	float MasterClipStart() const;
	float MasterContentEnd() const;
	s32 FindAvailableMasterTrack(float start, float duration) const;
	void UpdatePatternSettingsSelection(s32 type, size_t previousCount);
	void UpdateMasterCamera();
	void RefreshMasterTrackMute();
	void ResetMasterTimeline();
	void UpdateMasterActors();
	float BuildMasterPlaylist(SCinemaActor& actor, xr_vector<SMasterPlaylistSlice>& slices);
	void UpdateMasterLights();
	SMasterClipProperties MasterClipProperties(const SMasterClipSelection& item);
	void MoveMasterClips(float delta);
	void ResizeMasterClips(float delta);
	void MoveMasterClipTracks(const SMasterClipSelection& anchor, s32 track, const xr_vector<s32>& visibleTracks);
	void UpdateMasterParticles();
	void FireMasterShots(float from, float to, bool includeStart);
	bool AddMasterClip(SCinemaActor& actor, const shared_str& source, const shared_str& name, float start);
	void DrawGizmo();
	xr_vector<SMasterClipSelection> GetMasterGizmoObjects() const;
	Fvector GetMasterGizmoObjectPosition(const SMasterClipSelection& item) const;
	void TransformMasterGizmoObjects(const Fvector& previousPosition, const Fmatrix& previousOrientation);

	void PushThemeStyle();
	void PopThemeStyle();

	void Shutdown();

private:
	void RestoreInspectorDepth();
	LevelInspector* DepthInspector = nullptr;
	bool PreviousInspectorDepth = true;
	bool PreviousInspectorToggleMode = false;
	void ClearActors();
	void ClearParticles();
	void ClearLights();
	void TransformSelectedParticles(const SCinemaParticleGroup& previous, const SCinemaParticleGroup& current);
	bool AddParticle(const shared_str& name, const Fvector& position);
	void DuplicateParticles(const xr_vector<s32>& indices);
	void RemoveParticles(const xr_vector<s32>& indices);
	void DrawParticleTab();
	void DrawLightTab();
	void DrawLightModulation(SCinemaLight& light);
	void DuplicateLights(const xr_vector<s32>& indices);
	void RemoveLights(const xr_vector<s32>& indices);
	void TransformSelectedLights(const Fvector& previousPosition, const Fmatrix& previousOrientation);
	void DrawParticleBrowser();
	void ClearGizmo();
	void ResetSelection();
	void SelectActor(s32 index);
	void LoadSettings();
	void SaveSettings();
	const SEditorTheme& GetTheme() const;

	void BuildModelFiles();
	void BuildHandItems();
	void BuildLibrary();
	void BuildActorSources(SCinemaActor& actor);

	bool AddActor(const Fvector& position, const char* visual);
	bool AddActorInFront(const char* visual);
	bool SetActorVisual(SCinemaActor& actor, const char* visual);
	void SetActorItem(SCinemaActor& actor, const shared_str& section);
	void RefreshActorItemBones(SCinemaActor& actor);
	void DuplicateSelectedActor();
	void DeleteSelectedActor();

	void RequestSaveScene();
	void SaveSceneFile(const char* name);
	void RequestLoadScene();
	void BuildSceneList();
	void LoadSceneFile(const char* name);
	void ApplyLoadedScene();
	void ShowSceneDialog(ESceneDialog mode, const char* title, const xr_vector<xr_string>& lines);
	void DrawSceneDialog();
	bool RestoreActorPose(SCinemaActor& actor, const SSceneActorData& data);


	const STrimRange* FindTrim(const SCinemaActor& actor, const shared_str& source, const shared_str& name) const;
	STrimRange* FindTrimEdit(SCinemaActor& actor, const shared_str& source, const shared_str& name);
	void UpdateActorTrim(SCinemaActor& actor);
	void StartBlendJump(SCinemaActor& actor, float targetTime, float blendTime);
	bool DrawTimelineWidget(float& time, float total, const CMotionDef* motionDef, SCinemaActor& actor);

	void LoadPresets();
	bool SavePresets();
	void RequestSavePreset();
	void SavePresetFromActor(const char* name);
	void SpawnFromPreset(s32 index);
	void ApplyPresetToActor(s32 index);
	void DeletePreset(s32 index);
	s32 FindPreset(const char* name) const;
	void DrawPresetTab();

	void DrawToolbar();
	void DrawSelectedActor();
	void DrawModelTab();
	void DrawItemTab();
	void DrawAnimationTab();
	void DrawPlaylist(SCinemaActor& actor, float listHeight);
	void DrawClipTimelines(SCinemaActor& actor);
	void DrawClipTimeline(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip);
	float GetClipSpeed(const SCinemaActor& actor, const STrimRange* trim) const;
	void RemoveTrim(SCinemaActor& actor, const shared_str& source, const shared_str& name);

	u32 AxisColor(EGizmoAxis axis, bool highlighted) const;
	void AppendTranslateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis);
	void AppendRotateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis);
	void AppendWorldGizmo();
	EGizmoAxis HitTranslateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const;
	EGizmoAxis HitRotateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const;
	void BeginGizmo(Fvector& position, Fmatrix& orientation, const Fvector& pivot, EGizmoAxis axis);
	void UpdateGizmo(Fvector& position, Fmatrix& orientation);
	void PickActorAtMouse();

	void UpdateActorPlayback(SCinemaActor& actor);
	void UpdateActorShots(SCinemaActor& actor, float direction);
	void AdvanceActorPlaylist(SCinemaActor& actor);
	bool TryPlayPlaylistFrom(SCinemaActor& actor, s32 start);
	bool PlayPlaylistEntry(SCinemaActor& actor, s32 index);
	bool PlayClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip, bool playlist);
	bool EnsureSource(SCinemaActor& actor, const shared_str& source);
	bool ApplyDefaultPose(SCinemaActor& actor);
	void PreviewClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip);
	void StopParticlePreview(SCinemaParticle& particle);
	void StartParticleSpawnPreview(SCinemaParticle& particle);
	void PreviewParticle(const shared_str& name);
	void StopPatternPreviews(bool keepSpawnedParticles = false);
	void StartPlaylist(SCinemaActor& actor, s32 start);
	void StartActorPlayback(SCinemaActor& actor);
	void StopActor(SCinemaActor& actor);
	void TogglePause(SCinemaActor& actor);
	void ApplySpeed(SCinemaActor& actor);
	void MovePlaylistEntry(SCinemaActor& actor, s32 from, s32 to);
	void RemovePlaylistEntry(SCinemaActor& actor, s32 index);
	void AddClipToPlaylist(SCinemaActor& actor, const shared_str& source, const shared_str& name, bool fx);

	const SAnimationSource* FindSource(const SCinemaActor& actor, const shared_str& id) const;
	const SAnimationClip* FindClip(const SAnimationSource& source, const shared_str& name, bool fx) const;
	SCinemaActor* GetSelectedActor();
	const SCinemaActor* GetSelectedActor() const;

	xr_vector<xr_unique_ptr<SCinemaActor>> Actors;
	xr_vector<SCinemaLight> Lights;
	xr_vector<SCinemaLight> SceneLights;
	xr_vector<SCinemaParticleGroup> LightGroups;
	xr_vector<SCinemaParticleGroup> SceneLightGroups;
	s32 SelectedLight = -1;
	xr_vector<s32> SelectedLights;
	s32 LightSelectionAnchor = -1;
	xr_vector<SCinemaParticle> Particles;
	xr_vector<SCinemaParticle> SceneParticles;
	xr_vector<SCinemaParticleGroup> ParticleGroups;
	xr_vector<SCinemaParticleGroup> SceneParticleGroups;
	s32 SelectedParticle = -1;
	xr_vector<s32> SelectedParticles;
	char ParticleFilter[128] = {};
	shared_str SelectedParticleName;
	xr_vector<shared_str> SelectedParticleNames;
	shared_str ParticleLibraryAnchor;
	xr_vector<shared_str> ModelFiles;
	xr_vector<shared_str> LightTextureFiles;
	xr_vector<s32> LightTextureFiltered;
	xr_vector<SHandItem> HandItems;
	IGame_Level* Level = nullptr;
	bool LevelWasReady = false;
	s32 Selected = -1;
	s32 SelectedOgf = -1;
	s32 SelectedSource = -1;
	s32 SelectedClip = -1;
	xr_vector<std::pair<s32, s32>> SelectedClips;
	std::pair<s32, s32> ClipAnchor = { -1, -1 };
	s32 NameEditActor = -1;
	shared_str SelectedVisual;
	u32 NextNameIndex = 1;
	bool LibraryBuilt = false;
	bool ModelFilesBuilt = false;
	bool LightTextureFilesBuilt = false;
	bool HandItemsBuilt = false;
	bool Snap = false;
	float TranslateStep = 0.1f;
	float RotateStepDeg = 5.0f;
	char ModelFileFilter[128] = {};
	char LightTextureFilter[128] = {};
	char ItemFilter[128] = {};
	char ClipFilter[128] = {};
	char AnimationBrowseFilter[128] = {};
	xr_string AnimationBrowseFolder;
	xr_string AnimationBrowseStatus;
	xr_vector<xr_string> AnimationBrowseFiles;
	s32 AnimationBrowseOpenGroup = -1;
	s32 AnimationBrowseOpenSource = -1;
	char NameEdit[128] = {};
	EGizmoMode GizmoMode = EGizmoMode::Translate;
	EGizmoAxis HoveredAxis = EGizmoAxis::None;
	SGizmoDrag Gizmo;
	xr_shared_ptr<SCinemaCameraState> CameraState;
	xr_vector<SCinemaCameraClip> Cameras;
	xr_vector<SCinemaCameraClip> SceneCameras;
	s32 SelectedCameraKey = -1;
	s32 SelectedCameraKeyCamera = -1;
	s32 CameraTimelineDragKey = -1;
	s32 CameraTimelineContextKey = -1;
	s32 CameraTimelineContextCamera = -1;
	float CameraTimelineDragRange = 0.0f;
	SCinemaCurveEditorState CurveEditor;
	bool LightColorPicking = false;
	s32 LightColorPickingTarget = -1;
	xr_vector<SSceneActorData> SceneData;
	xr_vector<xr_string> SceneDialogLines;
	xr_vector<xr_string> SceneList;
	xr_string SceneDialogTitle;
	xr_string SceneListSelected;
	xr_string SceneNameLast;
	char SceneNameEdit[128] = {};
	ESceneDialog SceneDialogMode = ESceneDialog::None;
	bool SceneDialogRequested = false;
	bool SceneLoadRequested = false;
	bool PresetsOpen = false;
	bool MasterPatternEditorOpen = false;
	float AnimationLibraryWidth = 0.0f;
	SMasterClipSelection MasterEditingPattern;
	bool MasterPlaying = false;
	bool HideHelpersDuringPlayback = true;
	bool MasterActive = false;
	bool MasterTrackMuted[kCinemaMaxTracks] = {};
	bool MasterEffectiveTrackMuted[kCinemaMaxTracks] = {};
	bool LoadedTrackMuted[kCinemaMaxTracks] = {};
	u16 MasterTrackParent[kCinemaMaxTracks] = {};
	u16 LoadedTrackParent[kCinemaMaxTracks] = {};
	bool MasterTrackGroup[kCinemaMaxTracks] = {};
	bool LoadedTrackGroup[kCinemaMaxTracks] = {};
	bool MasterTrackCollapsed[kCinemaMaxTracks] = {};
	bool LoadedTrackCollapsed[kCinemaMaxTracks] = {};
	bool MasterSoloPreviousMuted[kCinemaMaxTracks] = {};
	bool LoadedSoloPreviousMuted[kCinemaMaxTracks] = {};
	s32 MasterSoloTrack = -1;
	s32 LoadedSoloTrack = -1;
	bool MasterSoloChanged = false;
	bool LoadedSoloChanged = false;
	bool MasterIncludeStart = true;
	bool MasterParticleSeek = false;
	xr_vector<SMasterClipSelection> MasterClipSelection;
	SMasterClipSelection MasterClipAnchor;
	SMasterClipSelection MasterRenameTarget;
	char MasterRenameBuffer[128] = {};
	bool MasterRenameRequested = false;
	bool MasterBoxSelecting = false;
	bool MasterBoxDragged = false;
	float MasterBoxStartTime = 0.0f;
	float MasterBoxStartRow = 0.0f;
	xr_vector<SMasterClipSelection> MasterBoxBaseSelection;
	xr_vector<SMasterClipSelection> MasterGizmoObjects;
	xr_vector<SMasterGizmoSnapshot> MasterGizmoSnapshots;
	Fvector MasterGizmoPosition = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix MasterGizmoOrientation = Fmatrix().identity();
	s32 MasterSelectedActor = -1;
	s32 MasterSelectedClip = -1;
	s32 MasterSelectedLight = -1;
	s32 MasterSelectedLightClip = -1;
	s32 MasterSelectedParticle = -1;
	s32 MasterSelectedParticleClip = -1;
	bool MasterLoop = false;
	bool MasterUseLoopRange = false;
	float MasterLoopStart = 0.0f;
	float MasterLoopEnd = 10.0f;
	bool LoadedMasterUseLoopRange = false;
	float LoadedMasterLoopStart = 0.0f;
	float LoadedMasterLoopEnd = 10.0f;
	float MasterTime = 0.0f;
	float MasterDuration = 20.0f;
	float MasterPreviousContentEnd = 0.0f;
	float MasterZoom = 1.0f;
	float MasterZoomTarget = 1.0f;
	float MasterZoomAnchor = 0.0f;
	float MasterSidebarWidth = 0.0f;
	bool MasterSnap = false;
	bool MasterDragMoved = false;
	bool MasterGroupResize = false;
	SMasterClipSelection MasterGroupResizeAnchor;
	float MasterDragTarget = 0.0f;
	bool MasterDuplicateDrag = false;
	bool MasterDuplicateMoved = false;
	SMasterClipSelection MasterDuplicateTarget;
	float MasterContextStart = 0.0f;
	s32 MasterContextTrack = 0;
	s32 MasterLoopResizeEdge = 0;
	float MasterLoopDragStart = 0.0f;
	bool MasterPanCandidate = false;
	bool MasterPanDragged = false;
	bool MasterMiddlePan = false;
	bool MasterShiftClickPending = false;
	bool MasterSingleClickPending = false;
	SMasterClipSelection MasterSingleClickItem;
	bool MasterShiftClickCtrl = false;
	SMasterClipSelection MasterShiftClickItem;
	SMasterClipSelection MasterShiftPreviousAnchor;
	xr_vector<SMasterClipSelection> MasterShiftPreviousSelection;
	float MasterSnapStep = 1.0f / 30.0f;
	s32 LoadedSceneVersion = 0;
	float LoadedMasterDuration = 20.0f;
	bool LoadedMasterLoop = false;
	s32 ThemeIndex = 0;
	bool SettingsLoaded = false;
	bool SpawnFromCamera = false;
	s32 TrimDragMarker = 0;
	s32 TrimDragIndex = -1;
	xr_string TrimDragKey;
	bool TimelinesOpenByDefault = true;
	bool TimelinesOpenDirty = true;
	xr_vector<SNpcPreset> Presets;
	xr_string ExtraPathStatus;
	s32 PresetSelected = -1;
	xr_vector<s32> PresetSelection;
	s32 PresetAnchor = -1;
	bool PresetsLoaded = false;
	char PresetNameEdit[128] = {};
};

CCinemaFilmMakerManager* g_CinemaFilmMaker = nullptr;

void CCinemaFilmMakerManager::Shutdown()
{
	RestoreInspectorDepth();
	if (CameraState) CameraState->Active = false;
	CameraState.reset();
	ClearGizmo();
	ClearActors();
	ClearParticles();
	ClearLights();
	if (CurveEditor.Type == 4 || CurveEditor.Type == 5) CurveEditor.Open = false;
	Cameras.clear();
	SceneCameras.clear();
	SceneLights.clear();
	SceneLightGroups.clear();
	SceneParticles.clear();
	LibraryBuilt = false;
	ModelFiles.clear();
	ModelFilesBuilt = false;
	LightTextureFiles.clear();
	LightTextureFiltered.clear();
	LightTextureFilesBuilt = false;
	HandItems.clear();
	HandItemsBuilt = false;
	SceneData.clear();
	SceneDialogLines.clear();
	SceneList.clear();
	SceneListSelected.clear();
	SceneNameLast.clear();
	Presets.clear();
	PresetsLoaded = false;
	PresetSelected = -1;
	PresetSelection.clear();
	PresetAnchor = -1;
	SceneDialogMode = ESceneDialog::None;
	SceneDialogRequested = false;
	SceneLoadRequested = false;
	Level = nullptr;
	LevelWasReady = false;
	Selected = -1;
}

const SEditorTheme& CCinemaFilmMakerManager::GetTheme() const
{
	if (ThemeIndex < 0 || ThemeIndex >= kCinemaThemeCount)
	{
		return kCinemaThemes[0];
	}

	return kCinemaThemes[ThemeIndex];
}

void CCinemaFilmMakerManager::PushThemeStyle()
{
	LoadSettings();

	const SEditorTheme& theme = GetTheme();

	for (const SThemeColorLink& link : kCinemaThemeColors)
	{
		ImGui::PushStyleColor(link.Color, theme.*link.Value);
	}

	ImGui::PushStyleColor(ImGuiCol_TreeLines, ImVec4(theme.Text.x, theme.Text.y, theme.Text.z, 0.3f));
}

void CCinemaFilmMakerManager::PopThemeStyle()
{
	ImGui::PopStyleColor(static_cast<int>(std::size(kCinemaThemeColors)) + 1);
}

void CCinemaFilmMakerManager::LoadSettings()
{
	if (SettingsLoaded)
	{
		return;
	}

	SettingsLoaded = true;

	string_path path = {};
	FS.update_path(path, "$app_data_root$", kCinemaSettingsFile);

	std::error_code error;

	if (!std::filesystem::exists(path, error))
	{
		return;
	}

	CInifile ini(path, true, true, false);

	if (!ini.section_exist("editor"))
	{
		return;
	}

	SpawnFromCamera = ini.read_if_exists<bool>("editor", "spawn_from_camera", SpawnFromCamera);
	TimelinesOpenByDefault = ini.read_if_exists<bool>("editor", "timelines_open", TimelinesOpenByDefault);
	MasterSnap = ini.read_if_exists<bool>("editor", "master_snap", MasterSnap);
	HideHelpersDuringPlayback = ini.read_if_exists<bool>("editor", "hide_helpers_during_playback", HideHelpersDuringPlayback);
	const float gridStep = ini.read_if_exists<float>("editor", "master_snap_step", MasterSnapStep);
	if (std::isfinite(gridStep) && gridStep >= 0.001f && gridStep <= 60.0f)
	{
		MasterSnapStep = gridStep;
	}

	const char* themeId = ini.line_exist("editor", "theme") ? ini.r_string("editor", "theme") : nullptr;

	if (themeId == nullptr)
	{
		return;
	}

	for (s32 index = 0; index < kCinemaThemeCount; ++index)
	{
		if (xr_strcmp(kCinemaThemes[index].Id, themeId) == 0)
		{
			ThemeIndex = index;
			return;
		}
	}
}

static bool CinemaSaveIni(const CInifile& ini, const char* path)
{
	IWriter* writer = FS.w_open(path);
	const bool valid = writer != nullptr && writer->valid();

	if (valid)
	{
		ini.save_as(*writer);
	}

	FS.w_close(writer);

	if (valid)
	{
		FS.file_update(path);
	}

	return valid;
}

void CCinemaFilmMakerManager::SaveSettings()
{
	string_path path = {};
	FS.update_path(path, "$app_data_root$", kCinemaSettingsFile);

	CInifile ini(nullptr, false, false, false);
	ini.w_string("editor", "theme", GetTheme().Id);
	ini.w_bool("editor", "spawn_from_camera", SpawnFromCamera);
	ini.w_bool("editor", "timelines_open", TimelinesOpenByDefault);
	ini.w_bool("editor", "master_snap", MasterSnap);
	ini.w_bool("editor", "hide_helpers_during_playback", HideHelpersDuringPlayback);
	ini.w_float("editor", "master_snap_step", MasterSnapStep);

	CinemaSaveIni(ini, path);
}

u32 CCinemaFilmMakerManager::AxisColor(EGizmoAxis axis, bool highlighted) const
{
	const SEditorTheme& theme = GetTheme();

	if (highlighted)
	{
		return theme.AxisHighlighted;
	}

	switch (axis)
	{
	case EGizmoAxis::X:
		return theme.AxisX;
	case EGizmoAxis::Y:
		return theme.AxisY;
	case EGizmoAxis::Z:
		return theme.AxisZ;
	default:
		return theme.AxisPivot;
	}
}


void CCinemaFilmMakerManager::ClearGizmo()
{
	Gizmo = SGizmoDrag();
}

void CCinemaFilmMakerManager::ClearActors()
{
	MasterPatternEditorOpen = false;
	MasterDuplicateDrag = false;
	MasterDuplicateMoved = false;
	MasterGroupResize = false;
	MasterBoxSelecting = false;
	MasterBoxBaseSelection.clear();
	MasterClipSelection.clear();
	MasterClipAnchor = {};
	for (auto& actor : Actors)
	{
		actor->PlaylistIndex = -1;
		actor->Paused = false;
		ReleaseActorItem(*actor);
		ReleaseActorModel(*actor);
	}

	Actors.clear();
	Selected = -1;
	SelectedSource = -1;
	SelectedClip = -1;
	NameEditActor = -1;
}

void CCinemaFilmMakerManager::ClearLights()
{
	if (CurveEditor.Type == 2) CurveEditor.Open = false;
	MasterPatternEditorOpen = false;
	Lights.clear();
	LightGroups.clear();
	SelectedLight = -1;
	SelectedLights.clear();
	LightSelectionAnchor = -1;
}

void CCinemaFilmMakerManager::ClearParticles()
{
	if (CurveEditor.Type == 3) CurveEditor.Open = false;
	MasterPatternEditorOpen = false;
	for (auto& particle : Particles)
	{
		::Particles::Details::Destroy(particle.Object);
	}
	Particles.clear();
	ParticleGroups.clear();
	SelectedParticle = -1;
	SelectedParticles.clear();
}

void CCinemaFilmMakerManager::TransformSelectedParticles(const SCinemaParticleGroup& previous, const SCinemaParticleGroup& current)
{
	Fmatrix inverse, rotation;
	inverse.invert(previous.Orientation);
	rotation.mul(current.Orientation, inverse);
	for (const s32 selected : SelectedParticles)
	{
		if (selected == SelectedParticle || selected < 0 || selected >= static_cast<s32>(Particles.size()))
		{
			continue;
		}
		auto& particle = Particles[selected];
		Fvector offset, rotated;
		offset.sub(particle.Position, previous.Position);
		rotation.transform_dir(rotated, offset);
		particle.Position.add(current.Position, rotated);
		Fmatrix orientation;
		orientation.mul(rotation, particle.Orientation);
		particle.Orientation = orientation;
		UpdateParticleTransform(particle);
	}
}

bool CCinemaFilmMakerManager::AddParticle(const shared_str& name, const Fvector& position)
{
	if (Particles.size() >= kCinemaSceneMaxActors)
	{
		return false;
	}
	if (!IsCinemaParticle(name))
	{
		return false;
	}
	SCinemaParticle particle;
	particle.Name = name;
	particle.Position = position;
	particle.Object = CreateCinemaParticle(name.c_str());
	StartParticleTimeline(particle);
	SelectActor(-1);
	ClearGizmo();
	Particles.push_back(std::move(particle));
	SelectedParticle = static_cast<s32>(Particles.size()) - 1;
	SelectedParticles = { SelectedParticle };
	return true;
}

void CCinemaFilmMakerManager::DuplicateParticles(const xr_vector<s32>& indices)
{
	xr_vector<SCinemaParticle> copies;
	for (const s32 index : indices)
	{
		if (index >= 0 && index < static_cast<s32>(Particles.size()))
		{
			copies.push_back(Particles[index]);
			copies.back().PatternPreview = false;
			copies.back().PreviewOnSpawn = false;
		}
	}
	if (copies.empty() || copies.size() > kCinemaSceneMaxActors - Particles.size())
	{
		return;
	}
	xr_vector<s32> createdIndices;
	for (auto& copy : copies)
	{
		if (!AddParticle(copy.Name, copy.Position))
		{
			continue;
		}
		auto& created = Particles.back();
		const auto object = created.Object;
		created = copy;
		created.Object = object;
		created.MasterOwned = false;
		created.MasterPlayingClip = -1;
		created.MasterPreviousTime = -1.0f;
		created.CurveDrag = -1;
		StartParticleTimeline(created);
		createdIndices.push_back(static_cast<s32>(Particles.size()) - 1);
	}
	if (!createdIndices.empty())
	{
		SelectedParticles = std::move(createdIndices);
		SelectedParticle = SelectedParticles.back();
	}
}

void CCinemaFilmMakerManager::RemoveParticles(const xr_vector<s32>& indices)
{
	if (CurveEditor.Type == 3) CurveEditor.Open = false;
	MasterPatternEditorOpen = false;
	MasterClipSelection.clear();
	MasterClipAnchor = {};
	xr_vector<s32> sorted = indices;
	std::sort(sorted.begin(), sorted.end());
	sorted.erase(std::unique(sorted.begin(), sorted.end()), sorted.end());
	ClearGizmo();
	for (auto it = sorted.rbegin(); it != sorted.rend(); ++it)
	{
		if (*it >= 0 && *it < static_cast<s32>(Particles.size()))
		{
			::Particles::Details::Destroy(Particles[*it].Object);
			Particles.erase(Particles.begin() + *it);
		}
	}
	SelectedParticle = -1;
	SelectedParticles.clear();
	MasterSelectedParticle = -1;
	MasterSelectedParticleClip = -1;
}

void CCinemaFilmMakerManager::ResetSelection()
{
	SelectedClips.clear();
	ClipAnchor = { -1, -1 };
	SelectedSource = -1;
	SelectedClip = -1;
	NameEditActor = -1;
}

void CCinemaFilmMakerManager::SelectActor(s32 index)
{
	SelectedLight = -1;
	SelectedLights.clear();
	LightSelectionAnchor = -1;
	if (index < -1 || index >= static_cast<s32>(Actors.size()))
	{
		index = -1;
	}

	if (index >= 0)
	{
		SelectedParticle = -1;
		SelectedParticles.clear();
	}
	if (Selected == index)
	{
		return;
	}

	ClearGizmo();
	Selected = index;
	TimelinesOpenDirty = true;
	TrimDragMarker = 0;
	TrimDragIndex = -1;
	TrimDragKey.clear();
	ResetSelection();
}

SCinemaActor* CCinemaFilmMakerManager::GetSelectedActor()
{
	if (Selected < 0 || Selected >= static_cast<s32>(Actors.size()))
	{
		return nullptr;
	}

	return Actors[Selected].get();
}

const SCinemaActor* CCinemaFilmMakerManager::GetSelectedActor() const
{
	if (Selected < 0 || Selected >= static_cast<s32>(Actors.size()))
	{
		return nullptr;
	}

	return Actors[Selected].get();
}

float CCinemaFilmMakerManager::MasterContentEnd() const
{
	float end = 0.0f;
	const auto include = [&](const auto& clips)
	{
		for (const auto& clip : clips) end = std::max(end, clip.Start + clip.Duration);
	};
	for (const auto& actor : Actors) if (actor->MasterTrack) include(actor->MasterClips);
	for (const auto& light : Lights) if (light.MasterTrack) include(light.MasterClips);
	for (const auto& particle : Particles) if (particle.MasterTrack) include(particle.MasterClips);
	include(Cameras);
	return end;
}

static constexpr bool CinemaTrackEffectivelyMuted(bool muted, bool parentMuted, bool soloTrack)
{
	return muted || (parentMuted && !soloTrack);
}

static_assert(CinemaTrackEffectivelyMuted(false, true, false));
static_assert(!CinemaTrackEffectivelyMuted(false, true, true));
static_assert(CinemaTrackEffectivelyMuted(true, false, true));

void CCinemaFilmMakerManager::RefreshMasterTrackMute()
{
	for (s32 track = 0; track < kCinemaMaxTracks; ++track)
	{
		const s32 parent = static_cast<s32>(MasterTrackParent[track]) - 1;
		const bool parentMuted = parent >= 0 && parent < kCinemaMaxTracks && MasterTrackMuted[parent];
		MasterEffectiveTrackMuted[track] = CinemaTrackEffectivelyMuted(MasterTrackMuted[track], parentMuted, MasterSoloTrack == track);
	}
}

void CCinemaFilmMakerManager::OnFrame()
{
	if (!g_CinemaFilmMaker)
	{
		return;
	}
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)])
	{
		MasterDuplicateDrag = false;
		MasterDuplicateMoved = false;
		MasterGroupResize = false;
		MasterMiddlePan = false;
		CurveEditor.Open = false;
		LightColorPicking = false;
		MasterBoxSelecting = false;
		MasterBoxBaseSelection.clear();
	}

	if (Level != g_pGameLevel || (Level != nullptr && LevelWasReady && !Level->bReady))
	{
		if (CameraState) CameraState->Active = false;
		CameraState.reset();
		ClearGizmo();
		ClearActors();
		ClearParticles();
		ClearLights();
		if (CurveEditor.Type == 4 || CurveEditor.Type == 5) CurveEditor.Open = false;
		Cameras.clear();
		SceneCameras.clear();
		SelectedCameraKey = -1;
		SelectedCameraKeyCamera = -1;
		SceneLights.clear();
		SceneLightGroups.clear();
		SceneParticles.clear();
			LibraryBuilt = false;
		SceneData.clear();
		SceneDialogLines.clear();
		SceneDialogMode = ESceneDialog::None;
		SceneDialogRequested = false;
		SceneList.clear();
		SceneListSelected.clear();
		SceneLoadRequested = false;
		MasterPlaying = false;
		MasterActive = false;
		MasterIncludeStart = true;
		MasterParticleSeek = false;
		std::fill(std::begin(MasterTrackMuted), std::end(MasterTrackMuted), false);
		std::fill(std::begin(LoadedTrackMuted), std::end(LoadedTrackMuted), false);
		std::fill(std::begin(MasterTrackParent), std::end(MasterTrackParent), 0);
		std::fill(std::begin(LoadedTrackParent), std::end(LoadedTrackParent), 0);
		std::fill(std::begin(MasterTrackGroup), std::end(MasterTrackGroup), false);
		std::fill(std::begin(LoadedTrackGroup), std::end(LoadedTrackGroup), false);
		std::fill(std::begin(MasterTrackCollapsed), std::end(MasterTrackCollapsed), false);
		std::fill(std::begin(LoadedTrackCollapsed), std::end(LoadedTrackCollapsed), false);
		MasterSoloTrack = LoadedSoloTrack = -1;
		MasterSoloChanged = LoadedSoloChanged = false;
		MasterSelectedActor = -1;
		MasterSelectedClip = -1;
		MasterSelectedLight = -1;
		MasterSelectedLightClip = -1;
		MasterSelectedParticle = -1;
		MasterSelectedParticleClip = -1;
		MasterTime = 0.0f;
		MasterDuration = 20.0f;
		MasterPreviousContentEnd = 0.0f;
		MasterLoop = false;
		MasterUseLoopRange = false;
		MasterLoopStart = 0.0f;
		MasterLoopEnd = 10.0f;
		TrimDragMarker = 0;
		TrimDragIndex = -1;
		TrimDragKey.clear();
		Level = g_pGameLevel;
		LevelWasReady = false;
		Selected = -1;
		SelectedSource = -1;
		SelectedClip = -1;
		NameEditActor = -1;
	}

	if (Level == nullptr)
	{
		return;
	}
	LevelWasReady = Level->bReady;
	RefreshMasterTrackMute();
	if (LevelWasReady) MasterActive = true;

	const float contentEnd = MasterContentEnd();
	const bool editingWithMouse = ImGui::GetCurrentContext() && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	if (!editingWithMouse && contentEnd < MasterPreviousContentEnd)
	{
		const float desiredDuration = std::max(20.0f, contentEnd + 5.0f);
		if (desiredDuration < MasterDuration)
		{
			const float previousDuration = MasterDuration;
			MasterDuration = desiredDuration;
			MasterZoom = std::clamp(MasterZoom * MasterDuration / previousDuration, 1.0f, 32.0f);
			MasterZoomTarget = MasterZoom;
			if (MasterTime + 5.0f > MasterDuration)
			{
				MasterTime = std::max(contentEnd, MasterDuration - 5.0f);
				MasterIncludeStart = true;
				MasterParticleSeek = true;
			}
		}
	}
	MasterPreviousContentEnd = editingWithMouse ? std::max(MasterPreviousContentEnd, contentEnd) : contentEnd;
	if (MasterUseLoopRange && contentEnd >= 0.01f)
	{
		MasterLoopEnd = std::clamp(MasterLoopEnd, 0.01f, contentEnd);
		MasterLoopStart = std::clamp(MasterLoopStart, 0.0f, MasterLoopEnd - 0.01f);
	}
	const float requiredWidth = std::max(contentEnd, MasterTime) + 5.0f;
	if (requiredWidth > MasterDuration && MasterDuration < 7210.0f)
	{
		const float previousDuration = MasterDuration;
		MasterDuration = std::min(7210.0f, std::max(requiredWidth, MasterDuration + 10.0f));
		MasterZoom = std::min(32.0f, MasterZoom * MasterDuration / previousDuration);
		MasterZoomTarget = MasterZoom;
	}
	if (MasterPlaying && contentEnd <= 0.0f) MasterPlaying = false;
	if (MasterPlaying && Level->bReady && std::isfinite(Device.fTimeDelta))
	{
		const bool looping = MasterLoop || MasterUseLoopRange;
		const float playbackEnd = MasterUseLoopRange ? std::min(MasterLoopEnd, contentEnd) : contentEnd;
		const float loopStart = MasterUseLoopRange ? std::min(MasterLoopStart, std::max(0.0f, playbackEnd - 0.01f)) : 0.0f;
		if (looping && (MasterTime < loopStart || MasterTime >= playbackEnd))
		{
			MasterTime = loopStart;
			MasterIncludeStart = true;
			MasterParticleSeek = true;
		}
		float remaining = std::max(0.0f, Device.fTimeDelta);
		while (remaining > 0.0f && MasterPlaying)
		{
			const float from = MasterTime;
			const float step = std::min(remaining, playbackEnd - from);
			const float to = from + step;
			if (step > 0.0f)
			{
				FireMasterShots(from, to, MasterIncludeStart);
				MasterIncludeStart = false;
				const float nextRemaining = remaining - step;
				remaining = nextRemaining < remaining ? nextRemaining : 0.0f;
			}
			MasterTime = to;
			if (MasterTime >= playbackEnd)
			{
				if (looping)
				{
					MasterTime = loopStart;
					MasterIncludeStart = true;
					MasterParticleSeek = true;
				}
				else
				{
					MasterPlaying = false;
				}
			}
			else if (step <= 0.0f)
			{
				break;
			}
		}
	}

	if (!LibraryBuilt && Level->bReady)
	{
		BuildLibrary();
	}

	for (auto& actor : Actors)
	{
		if (actor->PatternPreview && (!MasterPatternEditorOpen || MasterEditingPattern.Type != 1 || MasterEditingPattern.Object < 0 || MasterEditingPattern.Object >= static_cast<s32>(Actors.size()) || Actors[MasterEditingPattern.Object].get() != actor.get() || !Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)]))
		{
			actor->PatternPreview = false;
			StopActor(*actor);
		}
	}
	UpdateMasterActors();
	for (auto& actor : Actors)
	{
		if (actor->PatternPreview)
		{
			UpdateActorTrim(*actor);
			UpdateActorPlacement(*actor);
			continue;
		}
		if (actor->PatternOwned && !MasterActive && !(MasterPatternEditorOpen && MasterEditingPattern.Type == 1 && MasterEditingPattern.Object >= 0 && MasterEditingPattern.Object < static_cast<s32>(Actors.size()) && Actors[MasterEditingPattern.Object].get() == actor.get()))
		{
			actor->Object.Unbind();
			continue;
		}
		if (MasterActive && actor->MasterTrack && (!actor->MasterClips.empty() || actor->PatternOwned))
		{
			const bool active = std::any_of(actor->MasterClips.begin(), actor->MasterClips.end(), [&](const SMasterClip& clip)
			{
				return CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) && MasterTime >= clip.Start && MasterTime < clip.Start + clip.Duration;
			});
			if (active)
			{
				UpdateActorPlacement(*actor);
			}
			else
			{
				actor->Object.Unbind();
			}
			continue;
		}
		const STrimRange* trim = FindTrim(*actor, actor->MotionSource, actor->MotionName);
		if (trim != nullptr && !trim->Shots.empty())
		{
			UpdateActorPlacement(*actor);
			UpdateActorShots(*actor, actor->PlayDirection);
		}
		const float shotDirection = actor->PlayDirection;
		UpdateActorTrim(*actor);
		UpdateActorPlacement(*actor);
		UpdateActorShots(*actor, shotDirection);
		UpdateActorPlayback(*actor);
	}
	UpdateMasterLights();
	for (auto& light : Lights)
	{
		if (light.PatternOwned && !MasterActive)
		{
			const bool editing = MasterPatternEditorOpen && MasterEditingPattern.Type == 2 && MasterEditingPattern.Object >= 0 && MasterEditingPattern.Object < static_cast<s32>(Lights.size()) && &Lights[MasterEditingPattern.Object] == &light;
			if (light.Light) light.Light->set_active(editing && light.Active);
			if (!editing) continue;
		}
		if (light.MasterOwned || !light.Modulation || !light.Running)
		{
			continue;
		}
		light.Time = std::min(light.Time + Device.fTimeDelta, light.Duration);
		if (light.Time >= light.Duration && light.Loop)
		{
			light.Time = 0.0f;
		}
		else if (light.Time >= light.Duration)
		{
			light.Running = false;
		}
		UpdateCinemaLightValues(light);
	}
	UpdateMasterParticles();
	for (auto& particle : Particles)
	{
		if (particle.PatternOwned && !MasterActive)
		{
			const bool editing = MasterPatternEditorOpen && MasterEditingPattern.Type == 3 && MasterEditingPattern.Object >= 0 && MasterEditingPattern.Object < static_cast<s32>(Particles.size()) && &Particles[MasterEditingPattern.Object] == &particle;
			if (!editing)
			{
				particle.Object->Stop(false);
				continue;
			}
		}
		if (particle.PatternPreview || particle.MasterOwned || !particle.Running)
		{
			continue;
		}
		particle.Time = std::min(particle.Time + Device.fTimeDelta, particle.Out);
		if (particle.Modulation)
		{
			UpdateParticleTransform(particle);
		}
		if (particle.Time >= particle.Out)
		{
			if (particle.Loop)
			{
				StartParticleTimeline(particle);
			}
			else
			{
				particle.Object->Stop(false);
				particle.Running = false;
			}
		}
		else if (!particle.Started && particle.Time >= particle.In)
		{
			UpdateParticleTransform(particle);
			particle.Object->Play(false);
			particle.Started = true;
		}
		else if (particle.Loop && particle.Started && particle.Time > particle.In + 0.05f && !particle.Object->IsPlaying())
		{
			particle.Object->Stop(false);
			UpdateParticleTransform(particle);
			particle.Object->Play(false);
		}
	}
	UpdateMasterCamera();
}

static xr_vector<bool> CinemaCameraConflicts(const xr_vector<SCinemaCameraClip>& cameras, const bool* mutedTracks)
{
	xr_vector<s32> order;
	order.reserve(cameras.size());
	for (s32 i = 0; i < static_cast<s32>(cameras.size()); ++i)
	{
		if (CinemaMasterClipEnabled(cameras[i], mutedTracks)) order.push_back(i);
	}
	std::sort(order.begin(), order.end(), [&](s32 a, s32 b)
	{
		if (cameras[a].Start != cameras[b].Start) return cameras[a].Start < cameras[b].Start;
		if (cameras[a].Track != cameras[b].Track) return cameras[a].Track < cameras[b].Track;
		return a < b;
	});
	xr_vector<bool> conflicts(cameras.size(), false);
	float acceptedEnd = 0.0f;
	for (s32 index : order)
	{
		if (cameras[index].Start < acceptedEnd) conflicts[index] = true;
		else acceptedEnd = cameras[index].Start + cameras[index].Duration;
	}
	return conflicts;
}

void CCinemaFilmMakerManager::UpdateMasterCamera()
{
	const SCinemaCameraClip* active = nullptr;
	const bool editorOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)];
	if (MasterActive && Level && Level->bReady && editorOpen)
	{
		const xr_vector<bool> conflicts = CinemaCameraConflicts(Cameras, MasterEffectiveTrackMuted);
		for (s32 i = 0; i < static_cast<s32>(Cameras.size()); ++i)
		{
			const auto& camera = Cameras[i];
			if (camera.Preview && CinemaMasterClipEnabled(camera, MasterEffectiveTrackMuted) && !camera.Keys.empty() && !conflicts[i] && MasterTime >= camera.Start && MasterTime < camera.Start + camera.Duration)
			{
				active = &camera;
				break;
			}
		}
	}
	if (!active)
	{
		if (CameraState) CameraState->Active = false;
		CameraState.reset();
		return;
	}
	const float localTime = MasterTime - active->Start;
	float time = localTime;
	if (CinemaCameraStretchAt(*active, localTime)) time *= active->SourceDuration / active->Duration;
	if (CameraState && (!CameraState->Attached || (!CameraState->DemoType && !g_pGameLevel->Cameras().GetCamEffector(cefDemo))))
	{
		CameraState->Active = false;
		CameraState.reset();
	}
	bool initializeCamera = false;
	if (!CameraState)
	{
		CameraState = xr_make_shared<SCinemaCameraState>();
		CameraState->DemoType = g_pGameLevel->Cameras().GetCamEffector(cefDemo) == nullptr;
		const ECamEffectorType type = CameraState->DemoType ? cefDemo : g_pGameLevel->Cameras().RequestCamEffectorId();
		g_pGameLevel->Cameras().AddCamEffector(new CCinemaCameraEffector(type, CameraState));
		initializeCamera = CameraState->DemoType;
	}
	SCinemaCameraSpline spline(*active);
	spline.Smooth = CinemaStepCurveValue(active->Curves[8], localTime, active->Smooth ? 1.0f : 0.0f) >= 0.5f;
	spline.Evaluate(time, CameraState->Position, &CameraState->Orientation);
	CameraState->Position.x += ParticleCurveValue(active->Curves[0], localTime, 0.0f);
	CameraState->Position.y += ParticleCurveValue(active->Curves[1], localTime, 0.0f);
	CameraState->Position.z += ParticleCurveValue(active->Curves[2], localTime, 0.0f);
	Fmatrix rotation, orientation;
	rotation.setHPB(deg2rad(ParticleCurveValue(active->Curves[3], localTime, 0.0f)), deg2rad(ParticleCurveValue(active->Curves[4], localTime, 0.0f)), deg2rad(ParticleCurveValue(active->Curves[5], localTime, 0.0f)));
	orientation.mul(CameraState->Orientation, rotation);
	CameraState->Orientation = orientation;
	CameraState->Fov = active->Curves[6].empty() ? active->Fov : std::clamp(ParticleCurveValue(active->Curves[6], localTime, 90.0f), 1.0f, 179.0f);
	ApplyCinemaCameraShakes(*active, localTime, CameraState->Position, CameraState->Orientation);
	if (initializeCamera && g_actor && g_actor->cam_Active())
	{
		g_pGameLevel->Cameras().UpdateFromCamera(g_actor->cam_Active());
	}
}

void CCinemaFilmMakerManager::Render()
{
	if (g_hud == nullptr || g_pGameLevel == nullptr ||
		!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)] ||
		(MasterPlaying && HideHelpersDuringPlayback))
	{
		RestoreInspectorDepth();
		return;
	}

	LevelInspector& inspector = HUD().world_prims;
	if (DepthInspector != &inspector)
	{
		DepthInspector = &inspector;
		PreviousInspectorDepth = inspector.zbuffer_enable;
		PreviousInspectorToggleMode = inspector.key_toggle_mode;
	}
	inspector.key_toggle_mode = true;
	inspector.zbuffer_enable = false;
	AppendWorldGizmo();
}

void CCinemaFilmMakerManager::RestoreInspectorDepth()
{
	if (DepthInspector && g_hud && DepthInspector == &HUD().world_prims)
	{
		DepthInspector->zbuffer_enable = PreviousInspectorDepth;
		DepthInspector->key_toggle_mode = PreviousInspectorToggleMode;
	}
	DepthInspector = nullptr;
}

void CCinemaFilmMakerManager::BuildModelFiles()
{
	if (ModelFilesBuilt)
	{
		return;
	}

	ModelFilesBuilt = true;
	ModelFiles.clear();

	FS_FileSet files;
	FS.file_list(files, _game_meshes_, FS_ListFiles, "*.ogf");

	ModelFiles.reserve(files.size());

	for (const FS_File& file : files)
	{
		ModelFiles.emplace_back(file.name.c_str());
	}

	std::sort(ModelFiles.begin(), ModelFiles.end(), [](const shared_str& left, const shared_str& right)
	{
		return xr_strcmp(left.c_str(), right.c_str()) < 0;
	});

}

void CCinemaFilmMakerManager::BuildHandItems()
{
	if (HandItemsBuilt)
	{
		return;
	}

	HandItemsBuilt = true;
	HandItems.clear();

	if (pSettings == nullptr)
	{
		return;
	}

	for (CInifile::Sect& section : pSettings->sections())
	{
		if (!section.line_exist("visual"))
		{
			continue;
		}

		const bool weaponStyle = section.line_exist("position") && section.line_exist("orientation");
		bool attachStyle = section.line_exist("attach_bone_name") && section.line_exist("attach_angle_offset") && section.line_exist("attach_position_offset");

		if (attachStyle)
		{
			const shared_str boneName = pSettings->r_string(section.Name.c_str(), "attach_bone_name");
			attachStyle = boneName.size() > 0;
		}

		if (!weaponStyle && !attachStyle)
		{
			continue;
		}

		const shared_str visual = pSettings->r_string(section.Name.c_str(), "visual");

		if (!VisualPathExists(visual.c_str()))
		{
			continue;
		}

		HandItems.push_back({ .Section = section.Name, .Visual = visual });
	}

	std::sort(HandItems.begin(), HandItems.end(), [](const SHandItem& left, const SHandItem& right)
	{
		return xr_strcmp(left.Section.c_str(), right.Section.c_str()) < 0;
	});
}

void CCinemaFilmMakerManager::BuildLibrary()
{
	LibraryBuilt = g_pGameLevel != nullptr && g_pGameLevel->bReady;
	if (!LibraryBuilt) return;
	for (auto& actor : Actors) BuildActorSources(*actor);
}

void CCinemaFilmMakerManager::BuildActorSources(SCinemaActor& actor)
{
	if (GetSelectedActor() == &actor)
	{
		SelectedClips.clear();
		ClipAnchor = { -1, -1 };
		SelectedSource = -1;
		SelectedClip = -1;
	}
	actor.Sources.clear();

	if (actor.Animated == nullptr || actor.Skeleton == nullptr)
	{
		return;
	}

	xr_set<shared_str> added;

	for (u16 slot = 0, count = actor.Animated->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		shared_motions motions = actor.Animated->LL_MotionsSlot(slot);

		if (motions.motion_map() == nullptr)
		{
			continue;
		}

		const shared_str& id = motions.id();
		if (std::any_of(actor.AdaptedSources.begin(), actor.AdaptedSources.end(), [&](const auto& adapted) { return adapted.second == id; })) continue;

		if (added.contains(id))
		{
			continue;
		}

		added.insert(id);

		SAnimationSource source;
		source.Id = id;
		FillSourceBones(source, actor.Skeleton);
		AppendClips(motions, source.Clips);

		if (!source.Clips.empty())
		{
			actor.Sources.push_back(std::move(source));
		}
	}

	for (const shared_str& path : actor.AnimationPacks)
	{
		if (added.contains(path)) continue;
		SAnimationSource source;
		if (CinemaIndexAnimationFile(path.c_str(), source))
		{
			added.insert(path);
			actor.Sources.push_back(std::move(source));
		}
	}

	std::sort(actor.Sources.begin(), actor.Sources.end(), [](const SAnimationSource& left, const SAnimationSource& right)
	{
		return xr_strcmp(left.Id.c_str(), right.Id.c_str()) < 0;
	});
}

const SAnimationSource* CCinemaFilmMakerManager::FindSource(const SCinemaActor& actor, const shared_str& id) const
{
	if (!id.size()) return nullptr;
	for (const SAnimationSource& source : actor.Sources)
	{
		if (xr_strcmp(source.Id.c_str(), id.c_str()) == 0)
		{
			return &source;
		}
	}

	return nullptr;
}

const SAnimationClip* CCinemaFilmMakerManager::FindClip(const SAnimationSource& source, const shared_str& name, bool fx) const
{
	for (const SAnimationClip& clip : source.Clips)
	{
		if (clip.Fx == fx && clip.Name == name)
		{
			return &clip;
		}
	}

	return nullptr;
}

bool CCinemaFilmMakerManager::EnsureSource(SCinemaActor& actor, const shared_str& source)
{
	if (actor.Animated == nullptr)
	{
		return false;
	}

	shared_str loadedSource = source;
	for (const auto& adapted : actor.AdaptedSources) if (adapted.first == source) { loadedSource = adapted.second; break; }
	for (u16 slot = 0, count = actor.Animated->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		if (xr_strcmp(actor.Animated->LL_MotionsSlot(slot).id().c_str(), loadedSource.c_str()) == 0)
		{
			return true;
		}
	}

	if (!CinemaIsOmfPath(source))
	{
		return false;
	}

	if (actor.Animated->LL_MotionsSlotCount() >= MAX_ANIM_SLOT)
	{
		ExtraPathStatus = "The model has reached the animation slot limit.";
		Msg("! Cinema editor: %s", ExtraPathStatus.c_str());
		return false;
	}
	xr_string error;
	if (FindSource(actor, source))
	{
		CMemoryWriter adapted;
		if (!CinemaBuildAdaptedMotion(actor, source.c_str(), adapted, error))
		{
			ExtraPathStatus = error;
			Msg("! Cinema editor: %s", error.c_str());
			return false;
		}
		static u32 temporaryIndex = 0;
		string_path temporary = {}, temporaryFile = {}, fullPath = {};
		bool available = false;
		for (u32 attempt = 0; attempt < 100; ++attempt)
		{
			xr_sprintf(temporary, "cinema_editor_cache\\retarget_%08x_%08x", Device.dwTimeGlobal, ++temporaryIndex);
			xr_strconcat(temporaryFile, temporary, ".omf");
			FS.update_path(fullPath, _game_meshes_, temporaryFile);
			std::error_code ec;
			const bool exists = std::filesystem::exists(std::filesystem::u8path(fullPath), ec);
			if (!ec && !exists && !FS.exist(_game_meshes_, temporaryFile) && !FS.exist("$level$", temporaryFile)) { available = true; break; }
		}
		if (!available) { ExtraPathStatus = "Could not reserve a temporary animation path."; return false; }
		IWriter* writer = FS.w_open(_game_meshes_, temporaryFile);
		if (!writer || !writer->valid())
		{
			if (writer) FS.w_close(writer);
			ExtraPathStatus = "Could not write temporary animation in game meshes.";
			return false;
		}
		writer->w(adapted.pointer(), adapted.size());
		FS.w_close(writer, true);
		const u16 oldCount = actor.Animated->LL_MotionsSlotCount();
		actor.Animated->append_motion_from_path(actor.Visual.c_str(), temporary);
		FS.file_delete(fullPath);
		if (actor.Animated->LL_MotionsSlotCount() <= oldCount)
		{
			ExtraPathStatus = "Could not load the adapted animation.";
			return false;
		}
		actor.AdaptedSources.emplace_back(source, shared_str(temporaryFile));
		ExtraPathStatus.clear();
		return true;
	}
	ExtraPathStatus = "Animation source is unavailable.";
	return false;
}

bool CCinemaFilmMakerManager::ApplyDefaultPose(SCinemaActor& actor)
{
	if (actor.Animated == nullptr)
	{
		return false;
	}

	const SAnimationSource* source = nullptr;
	const SAnimationClip* clip = CinemaFindEditorClip(actor, &source);

	if (clip == nullptr)
	{
		clip = CinemaFindDefaultIdle(actor, &source);
	}

	if (source != nullptr && clip != nullptr && PlayClip(actor, *source, *clip, false))
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, 0.0f);
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
		actor.PlaylistIndex = -1;
		actor.Paused = true;
		return true;
	}

	return CinemaSetBindPose(actor);
}

bool CCinemaFilmMakerManager::PlayClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip, bool playlist)
{
	if (!CinemaSourcePlayable(source, actor))
	{
		return false;
	}

	CinemaClearBindPose(actor);

	if (!EnsureSource(actor, source.Id))
	{
		return false;
	}

	const MotionID motion = CinemaFindMotion(actor, source, clip);

	if (!motion.valid())
	{
		return false;
	}

	if (clip.Fx)
	{
		actor.Animated->PlayFX(motion, 1.0f);
		actor.FxDeadline = Device.fTimeGlobal + actor.Animated->get_animation_length(motion);
	}
	else
	{
		const STrimRange* clipSetup = FindTrim(actor, source.Id, clip.Name);
		const float clipSpeed = GetClipSpeed(actor, clipSetup);

		PlayMotionByParts(actor.Animated, motion, true, nullptr, nullptr);
		CinemaSetBlendLoop(actor.Animated, motion, playlist ? false : actor.Loop);
		CinemaSetBlendSpeed(actor.Animated, motion, clipSpeed);
		actor.PlayDirection = clipSpeed >= 0.0f ? 1.0f : -1.0f;
	}

	actor.Motion = motion;
	actor.ShotPreviousTime = -1.0f;
	actor.MotionName = clip.Name;
	actor.MotionSource = source.Id;
	actor.MotionFx = clip.Fx;
	actor.Paused = false;

	if (const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName))
	{
		if (!trim->Segments.empty())
		{
			const float total = actor.Animated->get_animation_length(actor.Motion);
			const float start = std::clamp(trim->Segments.front().Start, 0.0f, std::max(0.0f, total - 0.02f));
			CinemaSetBlendTime(actor.Animated, actor.Motion, start);
		}
	}

	return true;
}

void CCinemaFilmMakerManager::StopParticlePreview(SCinemaParticle& particle)
{
	if (!particle.PatternPreview)
	{
		return;
	}
	particle.PatternPreview = false;
	particle.PreviewOnSpawn = false;
	particle.Object->Stop(false);
	particle.Time = particle.PreviewSavedTime;
	particle.MasterPlayingClip = -1;
	particle.MasterPreviousTime = -1.0f;
	UpdateParticleTransform(particle);
	if (!particle.MasterOwned && !particle.PatternOwned && particle.PreviewSavedPlaying)
	{
		particle.Object->Play(false);
	}
}

void CCinemaFilmMakerManager::StartParticleSpawnPreview(SCinemaParticle& particle)
{
	if (MasterPlaying || particle.PatternPreview || !std::any_of(particle.MasterClips.begin(), particle.MasterClips.end(), [&](const SMasterParticleClip& clip)
	{
		return CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) && MasterTime >= clip.Start && MasterTime < clip.Start + clip.Duration;
	}))
	{
		return;
	}
	particle.PreviewSavedTime = particle.Time;
	particle.PreviewSavedPlaying = particle.Object->IsPlaying();
	particle.PatternPreview = true;
	particle.PreviewOnSpawn = true;
	particle.Time = 0.0f;
	particle.Object->Stop(false);
	UpdateParticleTransform(particle);
	particle.Object->Play(false);
}

void CCinemaFilmMakerManager::ResetMasterTimeline()
{
	StopPatternPreviews();
	MasterPlaying = false;
	MasterActive = true;
	MasterIncludeStart = true;
	MasterParticleSeek = true;
	MasterTime = 0.0f;
	for (SCinemaParticle& particle : Particles)
	{
		if (particle.PatternOwned && particle.MasterTrack)
		{
			StartParticleSpawnPreview(particle);
		}
	}
}

void CCinemaFilmMakerManager::PreviewParticle(const shared_str& name)
{
	if (!name.size() || !IsCinemaParticle(name) || SelectedParticle < 0 || SelectedParticle >= static_cast<s32>(Particles.size()))
	{
		return;
	}
	const xr_vector<s32> targets = std::find(SelectedParticles.begin(), SelectedParticles.end(), SelectedParticle) != SelectedParticles.end() ? SelectedParticles : xr_vector<s32>{ SelectedParticle };
	for (const s32 index : targets)
	{
		if (index < 0 || index >= static_cast<s32>(Particles.size()))
		{
			continue;
		}
		SCinemaParticle& particle = Particles[index];
		StopParticlePreview(particle);
		const bool wasPlaying = particle.Object && particle.Object->IsPlaying();
		if (particle.Name != name)
		{
			auto replacement = CreateCinemaParticle(name.c_str());
			if (!replacement)
			{
				continue;
			}
			::Particles::Details::Destroy(particle.Object);
			particle.Object = std::move(replacement);
			particle.Name = name;
		}
		const bool activeClip = MasterActive && particle.MasterTrack && std::any_of(particle.MasterClips.begin(), particle.MasterClips.end(), [&](const SMasterParticleClip& clip)
		{
			return CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) && MasterTime >= clip.Start && MasterTime < clip.Start + clip.Duration;
		});
		if (MasterPlaying && activeClip)
		{
			particle.MasterPlayingClip = -1;
			particle.MasterPreviousTime = -1.0f;
			continue;
		}
		particle.PreviewSavedTime = particle.Time;
		particle.PreviewSavedPlaying = wasPlaying;
		particle.PatternPreview = true;
		particle.Time = 0.0f;
		particle.Object->Stop(false);
		UpdateParticleTransform(particle);
		particle.Object->Play(false);
	}
}

void CCinemaFilmMakerManager::StopPatternPreviews(bool keepSpawnedParticles)
{
	for (auto& particle : Particles)
	{
		if (!keepSpawnedParticles || !particle.PreviewOnSpawn)
		{
			StopParticlePreview(particle);
		}
	}
	for (auto& actor : Actors)
	{
		if (actor->PatternPreview)
		{
			actor->PatternPreview = false;
			StopActor(*actor);
		}
	}
}

void CCinemaFilmMakerManager::PreviewClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip)
{
	if (!CinemaSourcePlayable(source, actor)) return;
	if (actor.PatternOwned)
	{
		StopPatternPreviews();
		MasterPlaying = false;
		MasterActive = true;
		MasterIncludeStart = true;
		MasterParticleSeek = true;
		actor.PatternPreview = PlayClip(actor, source, clip, false);
		return;
	}
	actor.PlaylistIndex = -1;
	actor.Paused = false;
	PlayClip(actor, source, clip, false);
}

bool CCinemaFilmMakerManager::PlayPlaylistEntry(SCinemaActor& actor, s32 index)
{
	if (index < 0 || index >= static_cast<s32>(actor.Playlist.size()))
	{
		return false;
	}

	const SPlaylistEntry& entry = actor.Playlist[index];
	if (entry.Muted)
	{
		return false;
	}

	const SAnimationSource* source = FindSource(actor, entry.Source);

	if (source == nullptr)
	{
		return false;
	}

	const SAnimationClip* clip = FindClip(*source, entry.Name, entry.Fx);

	if (clip == nullptr)
	{
		return false;
	}

	if (!PlayClip(actor, *source, *clip, true))
	{
		return false;
	}

	actor.PlaylistIndex = index;
	return true;
}

bool CCinemaFilmMakerManager::TryPlayPlaylistFrom(SCinemaActor& actor, s32 start)
{
	const s32 count = static_cast<s32>(actor.Playlist.size());

	if (count == 0)
	{
		return false;
	}

	if (start < 0 || start >= count)
	{
		start = 0;
	}

	if (actor.Loop)
	{
		for (s32 i = 0; i < count; ++i)
		{
			const s32 index = (start + i) % count;

			if (PlayPlaylistEntry(actor, index))
			{
				return true;
			}
		}
	}
	else
	{
		for (s32 index = start; index < count; ++index)
		{
			if (PlayPlaylistEntry(actor, index))
			{
				return true;
			}
		}
	}

	actor.PlaylistIndex = -1;
	return false;
}

void CCinemaFilmMakerManager::AdvanceActorPlaylist(SCinemaActor& actor)
{
	const s32 count = static_cast<s32>(actor.Playlist.size());

	if (count == 0)
	{
		actor.PlaylistIndex = -1;
		return;
	}

	s32 start = actor.PlaylistIndex + 1;

	if (start >= count)
	{
		if (!actor.Loop)
		{
			actor.PlaylistIndex = -1;
			return;
		}

		start = 0;
	}

	if (!TryPlayPlaylistFrom(actor, start))
	{
		actor.PlaylistIndex = -1;
	}
}

static bool CinemaSameText(const shared_str& left, const shared_str& right)
{
	if (left.size() == 0 || right.size() == 0)
	{
		return left.size() == right.size();
	}

	return xr_strcmp(left.c_str(), right.c_str()) == 0;
}

const STrimRange* CCinemaFilmMakerManager::FindTrim(const SCinemaActor& actor, const shared_str& source, const shared_str& name) const
{
	for (const STrimRange& trim : actor.Trims)
	{
		if (CinemaSameText(trim.Source, source) && CinemaSameText(trim.Name, name))
		{
			return &trim;
		}
	}

	return nullptr;
}

STrimRange* CCinemaFilmMakerManager::FindTrimEdit(SCinemaActor& actor, const shared_str& source, const shared_str& name)
{
	for (STrimRange& trim : actor.Trims)
	{
		if (CinemaSameText(trim.Source, source) && CinemaSameText(trim.Name, name))
		{
			return &trim;
		}
	}

	return nullptr;
}

float CCinemaFilmMakerManager::GetClipSpeed(const SCinemaActor& actor, const STrimRange* trim) const
{
	return trim != nullptr ? trim->Speed : actor.Speed;
}

void CCinemaFilmMakerManager::RemoveTrim(SCinemaActor& actor, const shared_str& source, const shared_str& name)
{
	for (size_t index = 0; index < actor.Trims.size();)
	{
		if (CinemaSameText(actor.Trims[index].Source, source) && CinemaSameText(actor.Trims[index].Name, name))
		{
			actor.Trims.erase(actor.Trims.begin() + index);
			continue;
		}

		++index;
	}
}

void CCinemaFilmMakerManager::StartBlendJump(SCinemaActor& actor, float targetTime, float blendTime)
{
	if (actor.Animated == nullptr || !actor.Motion.valid())
	{
		return;
	}

	const float total = actor.Animated->get_animation_length(actor.Motion);
	const float time = std::clamp(targetTime, 0.0f, std::max(0.0f, total - END_EPS));

	if (blendTime <= 0.0f)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
		return;
	}

	CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);

	if (PlayMotionByParts(actor.Animated, actor.Motion, true, nullptr, nullptr) == nullptr)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
		return;
	}

	CinemaSetBlendLoop(actor.Animated, actor.Motion, false);
	CinemaSetBlendSpeed(actor.Animated, actor.Motion, 0.0f);
	CinemaSetBlendTime(actor.Animated, actor.Motion, time);

	if (CBlend* fresh = CinemaFindBlend(actor.Animated, actor.Motion))
	{
		fresh->blendAccrue = 1.0f / blendTime;
	}
}

void CCinemaFilmMakerManager::UpdateActorTrim(SCinemaActor& actor)
{
	if (actor.Animated == nullptr || !actor.Motion.valid() || actor.MotionFx || actor.Paused)
	{
		return;
	}

	CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion);

	if (blend == nullptr || !blend->playing || blend->timeTotal <= 0.0f)
	{
		return;
	}

	const float total = blend->timeTotal;
	const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName);
	const bool pingPong = actor.PingPong;
	const float speed = GetClipSpeed(actor, trim);
	const bool hasSegments = trim != nullptr && !trim->Segments.empty();
	const float blendTime = trim != nullptr ? trim->Blend : 0.0f;

	if (!hasSegments && !pingPong)
	{
		CinemaSetBlendSpeed(actor.Animated, actor.Motion, speed);
		actor.PlayDirection = speed >= 0.0f ? 1.0f : -1.0f;
		return;
	}

	CinemaSetBlendSpeed(actor.Animated, actor.Motion, 0.0f);

	const float delta = Device.fTimeDelta * fabsf(speed);

	if (delta <= 0.0f)
	{
		return;
	}

	STrimSegment chain;
	chain.Start = 0.0f;
	chain.End = total - END_EPS;

	if (hasSegments)
	{
		chain.Start = trim->Segments.front().Start;
		chain.End = trim->Segments.back().End;
	}

	const bool forward = actor.PlayDirection > 0.0f;
	float time = blend->timeCurrent + actor.PlayDirection * delta;

	if (!hasSegments && time >= chain.Start && time <= chain.End)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
		return;
	}

	if (hasSegments)
	{
		s32 currentIndex = -1;

		for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
		{
			const STrimSegment& segment = trim->Segments[index];

			if (blend->timeCurrent >= segment.Start && blend->timeCurrent <= segment.End)
			{
				currentIndex = index;
				break;
			}
		}

		s32 targetIndex = -1;

		for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
		{
			const STrimSegment& segment = trim->Segments[index];

			if (time >= segment.Start && time <= segment.End)
			{
				targetIndex = index;
				break;
			}
		}

		if (targetIndex < 0)
		{
			for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
			{
				const STrimSegment& segment = trim->Segments[index];

				if (forward && segment.Start > time)
				{
					targetIndex = index;
					break;
				}

				if (!forward && segment.End < time)
				{
					targetIndex = index;
				}
			}
		}

		if (targetIndex >= 0)
		{
			const STrimSegment& target = trim->Segments[targetIndex];
			const bool jumped = currentIndex != targetIndex;
			const float targetTime = jumped ? (forward ? target.Start : target.End) : time;

			if (jumped && blendTime > 0.0f)
			{
				StartBlendJump(actor, targetTime, blendTime);
				return;
			}

			CinemaSetBlendTime(actor.Animated, actor.Motion, std::clamp(targetTime, target.Start, target.End));
			return;
		}
	}

	if (forward)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, chain.End);

		if (pingPong)
		{
			actor.PlayDirection = -1.0f;
		}
		else if (actor.PlaylistIndex >= 0)
		{
			AdvanceActorPlaylist(actor);
		}
		else if (actor.Loop)
		{
			if (blendTime > 0.0f)
			{
				StartBlendJump(actor, chain.Start, blendTime);
			}
			else
			{
				CinemaSetBlendTime(actor.Animated, actor.Motion, chain.Start);
			}
		}
		else
		{
			CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
		}

		return;
	}

	CinemaSetBlendTime(actor.Animated, actor.Motion, chain.Start);

	if (pingPong)
	{
		actor.PlayDirection = 1.0f;
	}
	else if (actor.Loop)
	{
		if (blendTime > 0.0f)
		{
			StartBlendJump(actor, chain.End, blendTime);
		}
		else
		{
			CinemaSetBlendTime(actor.Animated, actor.Motion, chain.End);
		}
	}
	else
	{
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
	}
}


static void CinemaFireShot(SCinemaActor& actor)
{
	if (g_actor == nullptr || g_pGameLevel == nullptr || pSettings == nullptr || actor.ItemSection.size() == 0 || actor.Object.ItemVisual == nullptr)
	{
		return;
	}
	const char* section = actor.ItemSection.c_str();
	if (!pSettings->line_exist(section, "fire_point") || !pSettings->line_exist(section, "ammo_class"))
	{
		return;
	}
	string128 ammo = {};
	_GetItem(pSettings->r_string(section, "ammo_class"), 0, ammo);
	if (!ammo[0] || !pSettings->section_exist(ammo))
	{
		return;
	}
	const char* required[] = { "k_dist", "k_disp", "k_hit", "k_impulse", "tracer", "buck_shot", "impair", "wm_size", "inv_name_short" };
	for (const char* key : required)
	{
		if (!pSettings->line_exist(ammo, key))
		{
			return;
		}
	}
	if ((!pSettings->line_exist(ammo, "k_ap") && !pSettings->line_exist(ammo, "k_pierce")) || (!pSettings->line_exist(ammo, "k_air_resistance") && !pSettings->line_exist("bullet_manager", "air_resistance_k")) || !std::isfinite(pSettings->r_float(ammo, "k_dist")) || pSettings->r_float(ammo, "k_dist") <= 0.0f || pSettings->r_float(ammo, "wm_size") <= 0.0f)
	{
		return;
	}
	const float speed = pSettings->read_if_exists<float>(section, "bullet_speed", 0.0f) * pSettings->read_if_exists<float>(section, "bullet_speed_mult", 1.0f);
	const float distance = pSettings->read_if_exists<float>(section, "fire_distance", 0.0f) * pSettings->read_if_exists<float>(section, "fire_distance_mult", 1.0f);
	if (!std::isfinite(speed) || !std::isfinite(distance) || speed <= 0.0f || distance <= 0.0f)
	{
		return;
	}
	float power = 0.0f;
	if (pSettings->line_exist(section, "hit_power"))
	{
		string32 value = {};
		_GetItem(pSettings->r_string(section, "hit_power"), 0, value);
		power = static_cast<float>(atof(value));
	}
	const float impulse = pSettings->read_if_exists<float>(section, "hit_impulse", 0.0f) * pSettings->read_if_exists<float>(section, "hit_impulse_mult", 1.0f);
	const Fmatrix& transform = actor.Object.ItemTransform;
	Fvector position = {};
	transform.transform_tiny(position, pSettings->r_fvector3(section, "fire_point"));
	Fvector direction = transform.k;
	if (direction.square_magnitude() <= EPS_S)
	{
		return;
	}
	direction.normalize();
	CCartridge cartridge;
	cartridge.Load(ammo, 0);
	Level().BulletManager().AddBullet(position, direction, speed, power, impulse, g_actor->ID(), g_actor->ID(), ALife::eHitTypeFireWound, distance, cartridge, pSettings->read_if_exists<float>(section, "air_resistance_factor", 1.0f), true);
}

static constexpr bool CinemaCrossedShot(float shot, float from, float to, bool forward)
{
	return forward ? shot > from && shot <= to : shot < from && shot >= to;
}

static_assert(CinemaCrossedShot(0.5f, 0.4f, 0.5f, true));
static_assert(!CinemaCrossedShot(0.5f, 0.5f, 0.6f, true));
static_assert(CinemaCrossedShot(0.5f, 0.6f, 0.5f, false));
static_assert(!CinemaCrossedShot(0.5f, 0.5f, 0.4f, false));

void CCinemaFilmMakerManager::UpdateActorShots(SCinemaActor& actor, float direction)
{
	if (actor.Animated == nullptr || !actor.Motion.valid() || actor.MotionFx)
	{
		actor.ShotPreviousTime = -1.0f;
		return;
	}
	CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion);
	if (blend == nullptr)
	{
		actor.ShotPreviousTime = -1.0f;
		return;
	}
	const float now = blend->timeCurrent;
	const float previous = actor.ShotPreviousTime < 0.0f ? (direction >= 0.0f ? -EPS_S : now + EPS_S) : actor.ShotPreviousTime;
	actor.ShotPreviousTime = now;
	if (actor.Paused || (!blend->playing && now == previous))
	{
		return;
	}
	const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName);
	if (trim == nullptr || trim->Shots.empty())
	{
		return;
	}
	const bool forward = direction >= 0.0f;
	for (const float shot : trim->Shots)
	{
		if (shot > blend->timeTotal)
		{
			continue;
		}
		if (!trim->Segments.empty() && std::none_of(trim->Segments.begin(), trim->Segments.end(), [shot](const STrimSegment& segment) { return shot >= segment.Start && shot <= segment.End; }))
		{
			continue;
		}
		const bool wrapped = forward ? now < previous : now > previous;
		const bool hit = wrapped ? (CinemaCrossedShot(shot, previous, forward ? blend->timeTotal : 0.0f, forward) || CinemaCrossedShot(shot, forward ? -EPS_S : blend->timeTotal + EPS_S, now, forward)) : CinemaCrossedShot(shot, previous, now, forward);
		if (hit)
		{
			CinemaFireShot(actor);
		}
	}
}

void CCinemaFilmMakerManager::UpdateActorPlayback(SCinemaActor& actor)
{
	if (actor.PlaylistIndex < 0 || actor.Paused || actor.PlaylistIndex >= static_cast<s32>(actor.Playlist.size()))
	{
		return;
	}

	const SPlaylistEntry& entry = actor.Playlist[actor.PlaylistIndex];
	const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName);
	if (!entry.Fx && (actor.PingPong || (trim != nullptr && !trim->Segments.empty())))
	{
		return;
	}

	if (entry.Fx)
	{
		if (Device.fTimeGlobal >= actor.FxDeadline)
		{
			AdvanceActorPlaylist(actor);
		}

		return;
	}

	if (!actor.Motion.valid())
	{
		AdvanceActorPlaylist(actor);
		return;
	}

	CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion);

	if (blend == nullptr)
	{
		AdvanceActorPlaylist(actor);
		return;
	}

	if (blend->speed < 0.0f)
	{
		if (blend->timeCurrent <= END_EPS)
		{
			AdvanceActorPlaylist(actor);
		}
	}
	else if (blend->timeTotal > 0.0f && blend->timeCurrent >= blend->timeTotal - END_EPS)
	{
		AdvanceActorPlaylist(actor);
	}
}

void CCinemaFilmMakerManager::StartPlaylist(SCinemaActor& actor, s32 start)
{
	if (actor.PatternOwned) return;
	if (actor.Playlist.empty())
	{
		return;
	}

	actor.Paused = false;
	actor.PlaylistIndex = -1;
	if (!TryPlayPlaylistFrom(actor, start))
	{
		StopActor(actor);
	}
}

void CCinemaFilmMakerManager::StartActorPlayback(SCinemaActor& actor)
{
	if (!actor.Playlist.empty())
	{
		StartPlaylist(actor, actor.PlaylistSelected >= 0 ? actor.PlaylistSelected : 0);
		return;
	}

	if (actor.Motion.valid())
	{
		const SAnimationSource* source = FindSource(actor, actor.MotionSource);

		if (source != nullptr)
		{
			const SAnimationClip* clip = FindClip(*source, actor.MotionName, actor.MotionFx);

			if (clip != nullptr)
			{
				actor.Paused = false;
				PlayClip(actor, *source, *clip, false);
			}
		}
	}
}

void CCinemaFilmMakerManager::StopActor(SCinemaActor& actor)
{
	if (actor.Animated != nullptr && actor.Motion.valid())
	{
		CinemaForEachBlend(actor.Animated, actor.Motion, [](CBlend& blend)
		{
			blend.playing = false;
			blend.timeCurrent = 0.0f;
		});
	}

	actor.PlaylistIndex = -1;
	actor.Paused = false;
	actor.FxDeadline = 0.0f;
	actor.Motion.invalidate();
	actor.MotionName = "";
	actor.MotionSource = "";
	actor.MotionFx = false;
	actor.PlayDirection = 1.0f;

	ApplyDefaultPose(actor);
}

void CCinemaFilmMakerManager::TogglePause(SCinemaActor& actor)
{
	if (!actor.Motion.valid() || actor.MotionFx)
	{
		return;
	}

	actor.Paused = !actor.Paused;
	CinemaSetBlendPlaying(actor.Animated, actor.Motion, !actor.Paused);
}

void CCinemaFilmMakerManager::ApplySpeed(SCinemaActor& actor)
{
	if (actor.Animated != nullptr && actor.Motion.valid() && !actor.MotionFx)
	{
		const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName);
		const float speed = GetClipSpeed(actor, trim);

		CinemaSetBlendSpeed(actor.Animated, actor.Motion, speed);
		actor.PlayDirection = speed >= 0.0f ? 1.0f : -1.0f;
	}
}

void CCinemaFilmMakerManager::AddClipToPlaylist(SCinemaActor& actor, const shared_str& source, const shared_str& name, bool fx)
{
	if (actor.PatternOwned && fx) return;
	const SAnimationSource* animationSource = FindSource(actor, source);

	if (animationSource == nullptr || !CinemaSourcePlayable(*animationSource, actor))
	{
		return;
	}

	const SAnimationClip* clip = FindClip(*animationSource, name, fx);

	if (clip == nullptr)
	{
		return;
	}

	actor.Playlist.push_back({ .Source = source, .Name = name, .Fx = fx });
	actor.PlaylistSelected = static_cast<s32>(actor.Playlist.size()) - 1;
	actor.PlaylistSelection = { actor.PlaylistSelected };
	actor.PlaylistAnchor = actor.PlaylistSelected;
}

void CCinemaFilmMakerManager::MovePlaylistEntry(SCinemaActor& actor, s32 from, s32 to)
{
	const s32 count = static_cast<s32>(actor.Playlist.size());

	if (from == to || from < 0 || from >= count || to < 0 || to > count)
	{
		return;
	}

	s32 insertAt = to;

	if (insertAt > from)
	{
		--insertAt;
	}

	actor.PlaylistSelection.clear();
	actor.PlaylistAnchor = -1;
	const SPlaylistEntry entry = actor.Playlist[from];
	actor.Playlist.erase(actor.Playlist.begin() + from);
	actor.Playlist.insert(actor.Playlist.begin() + insertAt, entry);

	if (actor.PlaylistIndex == from)
	{
		actor.PlaylistIndex = insertAt;
	}
	else
	{
		if (from < actor.PlaylistIndex)
		{
			--actor.PlaylistIndex;
		}

		if (insertAt <= actor.PlaylistIndex)
		{
			++actor.PlaylistIndex;
		}
	}

	if (actor.PlaylistSelected == from)
	{
		actor.PlaylistSelected = insertAt;
	}
	else if (actor.PlaylistSelected > from && actor.PlaylistSelected <= insertAt)
	{
		--actor.PlaylistSelected;
	}
	else if (actor.PlaylistSelected >= insertAt && actor.PlaylistSelected < from)
	{
		++actor.PlaylistSelected;
	}
}

void CCinemaFilmMakerManager::RemovePlaylistEntry(SCinemaActor& actor, s32 index)
{
	if (index < 0 || index >= static_cast<s32>(actor.Playlist.size()))
	{
		return;
	}

	const bool wasPlaying = actor.PlaylistIndex == index;

	actor.Playlist.erase(actor.Playlist.begin() + index);

	if (wasPlaying)
	{
		StopActor(actor);
	}
	else if (actor.PlaylistIndex > index)
	{
		--actor.PlaylistIndex;
	}

	if (actor.PlaylistSelected == index)
	{
		actor.PlaylistSelected = -1;
	}
	else if (actor.PlaylistSelected > index)
	{
		--actor.PlaylistSelected;
	}
}

bool CCinemaFilmMakerManager::AddActor(const Fvector& position, const char* visual)
{
	if (visual == nullptr || visual[0] == '\0' || !VisualPathExists(visual))
	{
		return false;
	}

	auto actor = xr_make_unique<SCinemaActor>();
	string64 name = {};
	xr_sprintf(name, "FakeNPC_%02u", NextNameIndex++);
	actor->Name = name;
	actor->Position = position;

	Fvector direction = {};
	direction.sub(Device.vCameraPosition, position);
	const float yaw = direction.square_magnitude() > EPS ? -atan2f(direction.x, direction.z) : 0.0f;
	actor->Orientation.setHPB(yaw, 0.0f, 0.0f);

	if (!SetActorVisual(*actor, visual))
	{
		return false;
	}

	Actors.push_back(std::move(actor));
	Selected = static_cast<s32>(Actors.size()) - 1;
	SelectedParticle = -1;
	ResetSelection();
	return true;
}

bool CCinemaFilmMakerManager::AddActorInFront(const char* visual)
{
	if (g_pGameLevel == nullptr || visual == nullptr || visual[0] == '\0')
	{
		return false;
	}

	Fvector position = Device.vCameraPosition;
	Fvector direction = Device.vCameraDirection;

	if (!SpawnFromCamera && g_actor != nullptr)
	{
		position = g_actor->Position();
		direction = g_actor->Direction();
	}

	direction.normalize_safe();
	position.mad(direction, 3.0f);
	return AddActor(position, visual);
}

bool CCinemaFilmMakerManager::SetActorVisual(SCinemaActor& actor, const char* visual)
{
	if (visual == nullptr || visual[0] == '\0')
	{
		return false;
	}

	if (actor.Model != nullptr && xr_strcmp(actor.Visual.c_str(), visual) == 0)
	{
		return true;
	}

	if (!VisualPathExists(visual))
	{
		return false;
	}

	xr_string error;
	if (!CinemaCheckVisual(visual, error))
	{
		ShowSceneDialog(ESceneDialog::Info, "Cannot load model", { error });
		return false;
	}

	IRenderVisual* model = ::Render->model_Create(visual);

	if (model == nullptr)
	{
		return false;
	}

	vis_data& visData = model->getVisData();
	visData.marker = 0;
	visData.accept_frame = 0;
	visData.hom_frame = 0;

	IKinematics* skeleton = model->dcast_PKinematics();
	IKinematicsAnimated* animated = model->dcast_PKinematicsAnimated();

	if (skeleton != nullptr)
	{
		skeleton->CalculateBones(true);
		skeleton->CalculateBBox(true);
	}

	if (actor.Model != nullptr)
	{
		CinemaClearBindPose(actor);
		actor.Object.Unbind();
		::Render->model_Delete(actor.Model);
	}

	actor.Model = model;
	actor.Skeleton = skeleton;
	actor.Animated = animated;
	actor.ModelSources.clear();
	actor.AdaptedSources.clear();
	if (animated)
	{
		for (u16 slot = 0, count = animated->LL_MotionsSlotCount(); slot < count; ++slot)
			actor.ModelSources.push_back(animated->LL_MotionsSlot(slot).id());
	}
	actor.Visual = visual;
	actor.LocalBox = model->getVisData().box;
	actor.PlaylistIndex = -1;
	actor.Paused = false;

	if (actor.PlaylistSelected >= static_cast<s32>(actor.Playlist.size()))
	{
		actor.PlaylistSelected = static_cast<s32>(actor.Playlist.size()) - 1;
	}
	actor.Motion.invalidate();
	actor.MotionName = "";
	actor.MotionSource = "";
	actor.MotionFx = false;
	actor.FxDeadline = 0.0f;

	BuildActorSources(actor);
	ApplyDefaultPose(actor);
	RefreshActorItemBones(actor);
	SelectedSource = -1;
	SelectedClip = -1;
	return true;
}


static u16 CinemaFindBone(IKinematics* skeleton, const char* name)
{
	if (skeleton == nullptr || name == nullptr || name[0] == '\0')
	{
		return u16(-1);
	}

	return skeleton->LL_BoneID(name);
}

void CCinemaFilmMakerManager::RefreshActorItemBones(SCinemaActor& actor)
{
	actor.ItemBoneR = u16(-1);
	actor.ItemBoneL = u16(-1);
	actor.ItemBoneR2 = u16(-1);
	actor.ItemBoneAttach = u16(-1);

	if (actor.Skeleton == nullptr)
	{
		return;
	}

	if (!actor.ItemWeaponStyle)
	{
		actor.ItemBoneAttach = CinemaFindBone(actor.Skeleton, actor.ItemBoneName.c_str());
		return;
	}

	actor.ItemBoneR = CinemaFindBone(actor.Skeleton, "bip01_r_finger1");

	if (actor.ItemBoneR == u16(-1))
	{
		actor.ItemBoneR = CinemaFindBone(actor.Skeleton, "bip01_r_hand");
	}

	actor.ItemBoneL = CinemaFindBone(actor.Skeleton, "bip01_l_finger1");

	if (actor.ItemBoneL == u16(-1))
	{
		actor.ItemBoneL = CinemaFindBone(actor.Skeleton, "bip01_l_hand");
	}

	actor.ItemBoneR2 = CinemaFindBone(actor.Skeleton, "bip01_r_finger11");

	if (actor.ItemBoneR2 == u16(-1))
	{
		actor.ItemBoneR2 = actor.ItemBoneR;
	}
}

void CCinemaFilmMakerManager::SetActorItem(SCinemaActor& actor, const shared_str& section)
{
	ReleaseActorItem(actor);

	if (section.size() == 0 || pSettings == nullptr || ::Render == nullptr)
	{
		return;
	}

	BuildHandItems();
	const auto item = std::find_if(HandItems.begin(), HandItems.end(), [&](const SHandItem& entry) { return entry.Section == section; });

	if (item == HandItems.end())
	{
		return;
	}

	IRenderVisual* model = ::Render->model_Create(item->Visual.c_str());

	if (model == nullptr)
	{
		return;
	}

	vis_data& visData = model->getVisData();
	visData.marker = 0;
	visData.accept_frame = 0;
	visData.hom_frame = 0;

	actor.ItemSection = section;
	actor.ItemModel = model;
	actor.ItemWeaponStyle = pSettings->line_exist(section.c_str(), "position") && pSettings->line_exist(section.c_str(), "orientation");

	if (actor.ItemWeaponStyle)
	{
		actor.ItemHandDependence = pSettings->line_exist(section.c_str(), "hand_dependence") ? pSettings->r_s32(section.c_str(), "hand_dependence") : 2;
		actor.ItemPosition = pSettings->r_fvector3(section.c_str(), "position");
		actor.ItemRotation = pSettings->r_fvector3(section.c_str(), "orientation");
	}
	else
	{
		actor.ItemHandDependence = 2;
		actor.ItemBoneName = pSettings->r_string(section.c_str(), "attach_bone_name");
		actor.ItemPosition = pSettings->line_exist(section.c_str(), "attach_position_offset") ? pSettings->r_fvector3(section.c_str(), "attach_position_offset") : Fvector().set(0.0f, 0.0f, 0.0f);
		actor.ItemRotation = pSettings->line_exist(section.c_str(), "attach_angle_offset") ? pSettings->r_fvector3(section.c_str(), "attach_angle_offset") : Fvector().set(0.0f, 0.0f, 0.0f);
	}

	RefreshActorItemBones(actor);
}

void CCinemaFilmMakerManager::DuplicateSelectedActor()
{
	const SCinemaActor* source = GetSelectedActor();

	if (source == nullptr)
	{
		return;
	}

	auto actor = xr_make_unique<SCinemaActor>();
	string256 name = {};
	xr_sprintf(name, "%s_copy", source->Name.c_str());
	actor->Name = name;
	actor->Position = source->Position;
	actor->Position.mad(Device.vCameraRight, 1.0f);
	actor->Orientation = source->Orientation;
	actor->Scale = source->Scale;

	actor->Loop = source->Loop;
	actor->Speed = source->Speed;
	actor->AnimationPacks = source->AnimationPacks;

	if (!SetActorVisual(*actor, source->Visual.c_str()))
	{
		return;
	}

	if (source->ItemSection.size() > 0)
	{
		SetActorItem(*actor, source->ItemSection);
	}

	actor->Playlist = source->Playlist;
	actor->PlaylistSelected = source->PlaylistSelected;

	Actors.push_back(std::move(actor));
	Selected = static_cast<s32>(Actors.size()) - 1;
	ResetSelection();

}

void CCinemaFilmMakerManager::DeleteSelectedActor()
{
	MasterPatternEditorOpen = false;
	MasterClipSelection.clear();
	MasterClipAnchor = {};
	if (Selected < 0 || Selected >= static_cast<s32>(Actors.size()))
	{
		return;
	}

	ClearGizmo();
	Actors[Selected]->PlaylistIndex = -1;
	Actors[Selected]->Paused = false;
	ReleaseActorItem(*Actors[Selected]);
	ReleaseActorModel(*Actors[Selected]);
	Actors.erase(Actors.begin() + Selected);
	if (MasterSelectedActor == Selected)
	{
		MasterSelectedActor = -1;
		MasterSelectedClip = -1;
	}
	else if (MasterSelectedActor > Selected)
	{
		--MasterSelectedActor;
	}

	if (Selected >= static_cast<s32>(Actors.size()))
	{
		Selected = static_cast<s32>(Actors.size()) - 1;
	}

	ResetSelection();
}


static void CinemaSceneFolder(string_path& path, const shared_str& levelName)
{
	string_path relative = {};
	xr_sprintf(relative, "%s%s\\", kCinemaSceneFolder, levelName.c_str());
	FS.update_path(path, "$app_data_root$", relative);
}

static void CinemaPresetsPath(string_path& path)
{
	FS.update_path(path, "$app_data_root$", kCinemaPresetsFile);
}

static void CinemaScenePath(string_path& path, const shared_str& levelName, const char* sceneName)
{
	CinemaSceneFolder(path, levelName);
	xr_strcat(path, sceneName);
	xr_strcat(path, kCinemaSceneExt);
}

static void CinemaSceneCleanName(const char* source, xr_string& result)
{
	result.clear();

	if (source == nullptr)
	{
		return;
	}

	for (const char* symbol = source; *symbol != '\0'; ++symbol)
	{
		const char value = *symbol;

		if (value < 32 || value == '\\' || value == '/' || value == ':' || value == '*' || value == '?' || value == '"' || value == '<' || value == '>' || value == '|')
		{
			continue;
		}

		result.push_back(value);
	}

	while (!result.empty() && (result.back() == ' ' || result.back() == '.'))
	{
		result.pop_back();
	}

	while (!result.empty() && result.front() == ' ')
	{
		result.erase(result.begin());
	}
}

static void SplitTrimValue(const xr_string& text, xr_vector<xr_string>& parts)
{
	parts.clear();
	size_t cursor = 0;

	while (true)
	{
		const size_t found = text.find('|', cursor);

		if (found == xr_string::npos)
		{
			parts.push_back(text.substr(cursor));
			return;
		}

		parts.push_back(text.substr(cursor, found - cursor));
		cursor = found + 1;
	}
}

static void CinemaSaveShots(CInifile& ini, const char* section, const char* trimKey, const xr_vector<float>& shots)
{
	if (shots.empty())
	{
		return;
	}
	string64 key = {};
	xr_sprintf(key, "%s_shots", trimKey);
	xr_string value;
	for (float shot : shots)
	{
		string32 number = {};
		xr_sprintf(number, "%.5f|", shot);
		value += number;
	}
	ini.w_string(section, key, value.c_str());
}

static void CinemaLoadShots(CInifile& ini, const char* section, const char* trimKey, xr_vector<float>& shots)
{
	string64 key = {};
	xr_sprintf(key, "%s_shots", trimKey);
	if (!ini.line_exist(section, key))
	{
		return;
	}
	xr_vector<xr_string> values;
	SplitTrimValue(ini.r_string(section, key), values);
	for (const xr_string& value : values)
	{
		if (value.empty() || shots.size() >= kCinemaMaxShots)
		{
			continue;
		}
		const float time = static_cast<float>(atof(value.c_str()));
		if (time >= 0.0f && time <= 3600.0f)
		{
			shots.push_back(time);
		}
	}
}

static bool CinemaParseTrim(const shared_str& value, STrimRange& trim)
{
	if (value.size() == 0)
	{
		return false;
	}

	const xr_string text = value.c_str();
	xr_vector<xr_string> parts;
	SplitTrimValue(text, parts);

	if (parts.empty())
	{
		return false;
	}

	if (parts[0] == "v2")
	{
		if (parts.size() < 4)
		{
			return false;
		}

		trim.Source = parts[1].c_str();
		trim.Name = parts[2].c_str();
		trim.Speed = static_cast<float>(atof(parts[3].c_str()));

		size_t firstSegment = 4;

		if (parts.size() > 4 && parts[4].find(',') == xr_string::npos)
		{
			trim.Blend = static_cast<float>(atof(parts[4].c_str()));
			firstSegment = 5;
		}

		for (size_t index = firstSegment; index < parts.size(); ++index)
		{
			const size_t comma = parts[index].find(',');

			if (comma == xr_string::npos)
			{
				continue;
			}

			STrimSegment segment;
			segment.Start = static_cast<float>(atof(parts[index].substr(0, comma).c_str()));
			segment.End = static_cast<float>(atof(parts[index].c_str() + comma + 1));

			if (!std::isfinite(segment.Start) || !(segment.End > segment.Start && segment.End <= 3600.0f) || trim.Segments.size() >= kCinemaMaxSegments)
			{
				continue;
			}

			trim.Segments.push_back(segment);
		}
	}
	else
	{
		if (parts.size() < 4)
		{
			return false;
		}

		trim.Source = parts[0].c_str();
		trim.Name = parts[1].c_str();

		STrimSegment segment;
		segment.Start = static_cast<float>(atof(parts[2].c_str()));
		segment.End = static_cast<float>(atof(parts[3].c_str()));

		if (parts.size() > 4)
		{
			trim.Speed = static_cast<float>(atof(parts[parts.size() - 1].c_str()));
		}

		if (std::isfinite(segment.Start) && segment.End > segment.Start && segment.End <= 3600.0f)
		{
			trim.Segments.push_back(segment);
		}
	}

	if (trim.Source.size() == 0 || trim.Name.size() == 0 || trim.Source.size() >= sizeof(string_path) || trim.Name.size() >= 256 || strchr(trim.Source.c_str(), ',') || strchr(trim.Name.c_str(), ','))
	{
		return false;
	}

	SortSegmentList(trim.Segments);
	trim.Speed = std::isfinite(trim.Speed) ? std::clamp(trim.Speed, -4.0f, 4.0f) : 1.0f;
	trim.Blend = std::isfinite(trim.Blend) ? std::clamp(trim.Blend, 0.0f, 2.0f) : 0.0f;

	for (STrimSegment& segment : trim.Segments)
	{
		segment.Start = std::max(0.0f, segment.Start);
	}

	return true;
}

static shared_str CinemaReadString(CInifile& ini, const char* section, const char* line)
{
	if (!ini.line_exist(section, line))
	{
		return shared_str();
	}

	const char* value = ini.r_string(section, line);

	return value != nullptr ? shared_str(value) : shared_str();
}

bool CCinemaFilmMakerManager::RestoreActorPose(SCinemaActor& actor, const SSceneActorData& data)
{
	if (data.PoseClip.size() == 0)
	{
		return false;
	}

	const SAnimationSource* source = FindSource(actor, data.PoseSource);

	if (source == nullptr)
	{
		return false;
	}

	const SAnimationClip* clip = FindClip(*source, data.PoseClip, data.PoseFx);

	if (clip == nullptr)
	{
		return false;
	}

	if (!PlayClip(actor, *source, *clip, false))
	{
		return false;
	}

	CinemaSetBlendLoop(actor.Animated, actor.Motion, !data.PosePaused);

	if (!data.PoseFx && data.PoseTime > 0.0f && data.PoseTime <= 3600.0f)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, data.PoseTime);
		actor.ShotPreviousTime = data.PoseTime;
	}

	if (data.PosePaused)
	{
		actor.Paused = true;
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
	}

	return true;
}

void CCinemaFilmMakerManager::ShowSceneDialog(ESceneDialog mode, const char* title, const xr_vector<xr_string>& lines)
{
	SceneDialogMode = mode;
	SceneDialogTitle = title != nullptr ? title : "";
	SceneDialogLines = lines;
	SceneDialogRequested = true;
}

void CCinemaFilmMakerManager::DrawSceneDialog()
{
	if (SceneDialogRequested)
	{
		SceneDialogRequested = false;
		ImGui::OpenPopup("Scene##cinema_scene_dialog");
	}

	if (!ImGui::BeginPopupModal("Scene##cinema_scene_dialog", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		return;
	}

	ImGui::TextUnformatted(SceneDialogTitle.c_str());
	ImGui::Separator();

	const auto drawLines = [this]()
	{
		for (const xr_string& line : SceneDialogLines)
		{
			ImGui::Bullet();
			ImGui::TextWrapped("%s", line.c_str());
		}
	};

	switch (SceneDialogMode)
	{
	case ESceneDialog::Save:
	{
		drawLines();

		ImGui::SetNextItemWidth(420.0f);

		if (ImGui::IsWindowAppearing())
		{
			ImGui::SetKeyboardFocusHere();
		}

		ImGui::InputTextWithHint("##cinema_scene_name", "Scene name", SceneNameEdit, sizeof(SceneNameEdit));

		xr_string cleanName;
		CinemaSceneCleanName(SceneNameEdit, cleanName);

		bool exists = false;
		string_path path = {};

		if (!cleanName.empty() && Level != nullptr)
		{
			CinemaScenePath(path, Level->name(), cleanName.c_str());

			std::error_code error;
			exists = std::filesystem::exists(path, error);

			ImGui::TextDisabled("%s", path);
		}

		if (exists)
		{
			ImGui::TextDisabled("This scene already exists and will be overwritten.");
		}

		ImGui::Separator();

		ImGui::BeginDisabled(cleanName.empty());

		if (ImGui::Button("Save", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
			SaveSceneFile(cleanName.c_str());
		}

		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::Preset:
	{
		drawLines();

		ImGui::SetNextItemWidth(420.0f);

		if (ImGui::IsWindowAppearing())
		{
			ImGui::SetKeyboardFocusHere();
		}

		ImGui::InputTextWithHint("##cinema_preset_name", "Preset name", PresetNameEdit, sizeof(PresetNameEdit));

		xr_string cleanName;
		CinemaSceneCleanName(PresetNameEdit, cleanName);

		bool exists = false;
		string_path path = {};

		if (!cleanName.empty())
		{
			CinemaPresetsPath(path);

			exists = FindPreset(cleanName.c_str()) >= 0;

			ImGui::TextDisabled("%s", path);
		}

		if (exists)
		{
			ImGui::TextDisabled("This preset already exists and will be replaced.");
		}

		ImGui::Separator();

		ImGui::BeginDisabled(cleanName.empty());

		if (ImGui::Button("Save", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
			SavePresetFromActor(cleanName.c_str());
		}

		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::Load:
	{
		ImGui::TextDisabled("Saved scenes for level \"%s\":", Level != nullptr ? Level->name().c_str() : "");

		if (SceneList.empty())
		{
			ImGui::TextDisabled("Nothing saved yet.");
		}
		else
		{
			ImGui::BeginChild("##cinema_scene_list", ImVec2(420.0f, 260.0f), true);

			for (const xr_string& sceneName : SceneList)
			{
				if (ImGui::Selectable(sceneName.c_str(), SceneListSelected == sceneName))
				{
					SceneListSelected = sceneName;
				}

				if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
				{
					SceneListSelected = sceneName;
					SceneLoadRequested = true;
				}
			}

			ImGui::EndChild();
		}

		ImGui::TextDisabled("Loading replaces the current scene (%u NPC, %u particles, %u lights).", static_cast<u32>(Actors.size()), static_cast<u32>(Particles.size()), static_cast<u32>(Lights.size()));
		ImGui::Separator();

		ImGui::BeginDisabled(SceneListSelected.empty());

		if (ImGui::Button("Load", ImVec2(120.0f, 0.0f)))
		{
			SceneLoadRequested = true;
		}

		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			SceneListSelected.clear();
			ImGui::CloseCurrentPopup();
		}

		if (SceneLoadRequested)
		{
			SceneLoadRequested = false;

			const xr_string sceneName = SceneListSelected;
			ImGui::CloseCurrentPopup();
			LoadSceneFile(sceneName.c_str());
		}

		break;
	}

	case ESceneDialog::Confirm:
	{
		drawLines();
		ImGui::Separator();

		if (ImGui::Button("Load", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
			ApplyLoadedScene();
		}

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			SceneData.clear();
			SceneParticles.clear();
			SceneLights.clear();
			SceneLightGroups.clear();
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::Info:
	{
		drawLines();
		ImGui::Separator();

		if (ImGui::Button("OK", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::None:
		ImGui::CloseCurrentPopup();
		break;
	default:
		break;
	}

	ImGui::EndPopup();
}

void CCinemaFilmMakerManager::RequestSaveScene()
{
	if (Level == nullptr)
	{
		return;
	}

	xr_vector<xr_string> lines;

	if (Actors.empty() && Particles.empty() && Lights.empty() && LightGroups.empty() && Cameras.empty())
	{
		lines.push_back("The scene is empty: there is nothing to save.");
	}

	xr_string defaultName = SceneNameLast;

	if (defaultName.empty())
	{
		defaultName = "scene";

		std::error_code error;

		for (s32 index = 2; index < 1000; ++index)
		{
			string_path path = {};
			CinemaScenePath(path, Level->name(), defaultName.c_str());

			if (!std::filesystem::exists(path, error))
			{
				break;
			}

			string64 candidate = {};
			xr_sprintf(candidate, "scene_%d", index);
			defaultName = candidate;
		}
	}

	xr_sprintf(SceneNameEdit, "%s", defaultName.c_str());

	ShowSceneDialog(ESceneDialog::Save, "Save scene", lines);
}

static void CinemaSaveMasterGroup(CInifile& ini, const char* section, const SCinemaParticleGroup& group)
{
	ini.w_bool(section, "master_track", group.MasterTrack);
	ini.w_u32(section, "master_transforms", static_cast<u32>(group.MasterTransforms.size()));
	for (u32 i = 0; i < group.MasterTransforms.size(); ++i)
	{
		const SMasterTransformKey& transform = group.MasterTransforms[i];
		string64 key = {};
		xr_sprintf(key, "master_transform_%03u_time", i);
		ini.w_float(section, key, transform.Time);
		xr_sprintf(key, "master_transform_%03u_position", i);
		ini.w_fvector3(section, key, transform.Position);
		xr_sprintf(key, "master_transform_%03u_rotation", i);
		ini.w_fvector3(section, key, transform.Rotation);
		xr_sprintf(key, "master_transform_%03u_scale", i);
		ini.w_float(section, key, transform.Scale.x);
	}
}

static void CinemaLoadMasterGroup(CInifile& ini, const char* section, SCinemaParticleGroup& group, xr_vector<xr_string>& warnings)
{
	group.MasterTrack = ini.read_if_exists<bool>(section, "master_track", false);
	const u32 count = ini.read_if_exists<u32>(section, "master_transforms", 0);
	for (u32 i = 0; i < std::min(count, kCinemaSceneMaxPlaylist); ++i)
	{
		SMasterTransformKey transform;
		string64 key = {};
		xr_sprintf(key, "master_transform_%03u_time", i);
		transform.Time = ini.read_if_exists<float>(section, key, -1.0f);
		xr_sprintf(key, "master_transform_%03u_position", i);
		transform.Position = ini.read_if_exists<Fvector>(section, key, transform.Position);
		xr_sprintf(key, "master_transform_%03u_rotation", i);
		transform.Rotation = ini.read_if_exists<Fvector>(section, key, transform.Rotation);
		xr_sprintf(key, "master_transform_%03u_scale", i);
		const float scale = ini.read_if_exists<float>(section, key, 1.0f);
		if (std::isfinite(transform.Time) && transform.Time >= 0.0f && transform.Time <= 3600.0f && _valid(transform.Position) && _valid(transform.Rotation) && std::isfinite(scale) && scale >= 0.1f && scale <= 5.0f)
		{
			transform.Scale.set(scale, scale, scale);
			group.MasterTransforms.push_back(transform);
		}
		else
		{
			warnings.push_back("Invalid master group transform key; it was skipped.");
		}
	}
}

template <typename T>
static void CinemaSaveClipAppearance(CInifile& ini, const char* section, const char* prefix, const T& clip)
{
	string128 key = {};
	xr_sprintf(key, "%s_label_hex", prefix);
	if (clip.DisplayName.size())
	{
		static constexpr char digits[] = "0123456789abcdef";
		xr_string encoded;
		for (const char* p = clip.DisplayName.c_str(); *p; ++p)
		{
			const u8 byte = static_cast<u8>(*p);
			encoded += digits[byte >> 4];
			encoded += digits[byte & 15];
		}
		ini.w_string(section, key, encoded.c_str());
	}
	xr_sprintf(key, "%s_display_color", prefix);
	if (clip.DisplayColor) ini.w_u32(section, key, clip.DisplayColor);
}

template <typename T>
static void CinemaLoadClipAppearance(CInifile& ini, const char* section, const char* prefix, T& clip)
{
	string128 key = {};
	xr_sprintf(key, "%s_label_hex", prefix);
	if (ini.line_exist(section, key))
	{
		const char* encoded = ini.r_string(section, key);
		xr_string decoded;
		const auto digit = [](char c) -> int { return c >= '0' && c <= '9' ? c - '0' : c >= 'a' && c <= 'f' ? c - 'a' + 10 : -1; };
		const size_t length = encoded != nullptr ? xr_strlen(encoded) : 0;
		if (length <= 254 && length % 2 == 0)
		{
			for (size_t i = 0; i < length; i += 2)
			{
				const int high = digit(encoded[i]);
				const int low = digit(encoded[i + 1]);
				if (high < 0 || low < 0 || (high == 0 && low == 0)) { decoded.clear(); break; }
				decoded += static_cast<char>((high << 4) | low);
			}
			clip.DisplayName = decoded.c_str();
		}
	}
	xr_sprintf(key, "%s_display_color", prefix);
	clip.DisplayColor = ini.read_if_exists<u32>(section, key, 0);
}

void CCinemaFilmMakerManager::SaveSceneFile(const char* name)
{
	if (Level == nullptr)
	{
		return;
	}

	xr_vector<xr_string> dialog;

	xr_string cleanName;
	CinemaSceneCleanName(name, cleanName);

	if (cleanName.empty())
	{
		dialog.push_back("Enter a scene name.");
		ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
		return;
	}

	if (Actors.empty() && Particles.empty() && Lights.empty() && LightGroups.empty() && Cameras.empty())
	{
		dialog.push_back("The scene is empty: there is nothing to save.");
		ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
		return;
	}

	string_path path = {};
	CinemaScenePath(path, Level->name(), cleanName.c_str());

	CInifile ini(nullptr, false, false, false);

	ini.w_s32("scene", "version", kCinemaSceneVersion);
	ini.w_string("scene", "name", cleanName.c_str());
	ini.w_string("scene", "level", Level->name().c_str());
	ini.w_string("scene", "platform", EngineExternal().GetCurrentPlatformName());
	ini.w_float("scene", "master_duration", MasterDuration);
	ini.w_bool("scene", "master_loop", MasterLoop);
	ini.w_bool("scene", "master_loop_range", MasterUseLoopRange);
	ini.w_float("scene", "master_loop_start", MasterLoopStart);
	ini.w_float("scene", "master_loop_end", MasterLoopEnd);
	ini.w_s32("scene", "master_solo_track", MasterSoloTrack);
	ini.w_bool("scene", "master_solo_changed", MasterSoloChanged);
	for (u32 track = 0; track < kCinemaMaxTracks; ++track)
	{
		string32 key = {};
		if (MasterTrackMuted[track])
		{
			xr_sprintf(key, "master_track_%02u_muted", track);
			ini.w_bool("scene", key, true);
		}
		if (MasterTrackParent[track] != 0)
		{
			xr_sprintf(key, "master_track_%02u_parent", track);
			ini.w_s32("scene", key, static_cast<s32>(MasterTrackParent[track]) - 1);
		}
		if (MasterTrackGroup[track])
		{
			xr_sprintf(key, "master_track_%02u_group", track);
			ini.w_bool("scene", key, true);
		}
		if (MasterTrackCollapsed[track])
		{
			xr_sprintf(key, "master_track_%02u_collapsed", track);
			ini.w_bool("scene", key, true);
		}
		if (MasterSoloTrack >= 0 && MasterSoloPreviousMuted[track])
		{
			xr_sprintf(key, "master_solo_%02u_muted", track);
			ini.w_bool("scene", key, true);
		}
	}
	ini.w_u32("scene", "cameras", static_cast<u32>(Cameras.size()));
	for (u32 i = 0; i < Cameras.size(); ++i)
	{
		const SCinemaCameraClip& camera = Cameras[i];
		string32 section = {};
		xr_sprintf(section, "camera_%03u", i);
		ini.w_float(section, "start", camera.Start);
		ini.w_float(section, "duration", camera.Duration);
		ini.w_float(section, "source_duration", camera.SourceDuration);
		ini.w_s32(section, "track", camera.Track);
		ini.w_bool(section, "muted", camera.Muted);
		ini.w_bool(section, "preview", camera.Preview);
		ini.w_float(section, "fov", camera.Fov);
		CinemaSaveClipAppearance(ini, section, "clip", camera);
		ini.w_bool(section, "stretch", camera.Stretch);
		ini.w_bool(section, "smooth", camera.Smooth);
		for (u32 channel = 0; channel < 9; ++channel)
		{
			string64 key = {};
			xr_sprintf(key, "camera_curve_%u_count", channel); ini.w_u32(section, key, static_cast<u32>(camera.Curves[channel].size()));
			for (u32 k = 0; k < camera.Curves[channel].size(); ++k)
			{
				const auto& point = camera.Curves[channel][k];
				xr_sprintf(key, "camera_curve_%u_%03u_time", channel, k); ini.w_float(section, key, point.Time);
				xr_sprintf(key, "camera_curve_%u_%03u_value", channel, k); ini.w_float(section, key, point.Value);
				xr_sprintf(key, "camera_curve_%u_%03u_smooth", channel, k); ini.w_bool(section, key, point.Smooth);
				xr_sprintf(key, "camera_curve_%u_%03u_in", channel, k); ini.w_float(section, key, point.InSlope);
				xr_sprintf(key, "camera_curve_%u_%03u_out", channel, k); ini.w_float(section, key, point.OutSlope);
			}
		}
		ini.w_u32(section, "shake_count", static_cast<u32>(camera.Shakes.size()));
		for (u32 j = 0; j < camera.Shakes.size(); ++j)
		{
			const auto& shake = camera.Shakes[j];
			string64 key = {};
			xr_sprintf(key, "shake_%02u_enabled", j); ini.w_bool(section, key, shake.Enabled);
			xr_sprintf(key, "shake_%02u_preset", j); ini.w_s32(section, key, shake.Preset);
			xr_sprintf(key, "shake_%02u_influence", j); ini.w_float(section, key, shake.Influence);
			xr_sprintf(key, "shake_%02u_position_scale", j); ini.w_float(section, key, shake.PositionScale);
			xr_sprintf(key, "shake_%02u_speed", j); ini.w_float(section, key, shake.Speed);
			xr_sprintf(key, "shake_%02u_offset", j); ini.w_float(section, key, shake.Offset);
			xr_sprintf(key, "shake_%02u_fade_in", j); ini.w_float(section, key, shake.FadeIn);
			xr_sprintf(key, "shake_%02u_fade_out", j); ini.w_float(section, key, shake.FadeOut);
			xr_sprintf(key, "shake_%02u_stretch", j); ini.w_bool(section, key, shake.Stretch);
			for (u32 channel = 0; channel < 9; ++channel)
			{
				xr_sprintf(key, "shake_%02u_curve_%u_count", j, channel); ini.w_u32(section, key, static_cast<u32>(shake.Curves[channel].size()));
				for (u32 k = 0; k < shake.Curves[channel].size(); ++k)
				{
					const auto& point = shake.Curves[channel][k];
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_time", j, channel, k); ini.w_float(section, key, point.Time);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_value", j, channel, k); ini.w_float(section, key, point.Value);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_smooth", j, channel, k); ini.w_bool(section, key, point.Smooth);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_in", j, channel, k); ini.w_float(section, key, point.InSlope);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_out", j, channel, k); ini.w_float(section, key, point.OutSlope);
				}
			}
		}
		ini.w_u32(section, "keys", static_cast<u32>(camera.Keys.size()));
		for (u32 j = 0; j < camera.Keys.size(); ++j)
		{
			string64 key = {};
			xr_sprintf(key, "key_%03u_time", j);
			ini.w_float(section, key, camera.Keys[j].Time);
			xr_sprintf(key, "key_%03u_position", j);
			ini.w_fvector3(section, key, camera.Keys[j].Position);
			Fvector rotation;
			camera.Keys[j].Orientation.getHPB(rotation);
			xr_sprintf(key, "key_%03u_rotation", j);
			ini.w_fvector3(section, key, rotation);
		}
	}
	ini.w_u32("scene", "actors", static_cast<u32>(Actors.size()));
	ini.w_u32("scene", "particles", static_cast<u32>(Particles.size()));
	ini.w_u32("scene", "light_groups", static_cast<u32>(LightGroups.size()));
	for (u32 i = 0; i < LightGroups.size(); ++i)
	{
		string32 section = {};
		xr_sprintf(section, "light_group_%03u", i);
		ini.w_fvector3(section, "position", LightGroups[i].Position);
		Fvector rotation = {};
		LightGroups[i].Orientation.getHPB(rotation);
		rotation.mul(rad2deg(1.0f));
		ini.w_fvector3(section, "rotation", rotation);
		ini.w_float(section, "scale", LightGroups[i].Scale);
		CinemaSaveMasterGroup(ini, section, LightGroups[i]);
	}
	ini.w_u32("scene", "lights", static_cast<u32>(Lights.size()));
	for (u32 i = 0; i < Lights.size(); ++i)
	{
		const auto& light = Lights[i];
		string32 section = {};
		xr_sprintf(section, "light_%03u", i);
		Fvector rotation = {};
		light.Orientation.getHPB(rotation);
		rotation.mul(rad2deg(1.0f));
		ini.w_s32(section, "group", light.Group);
		ini.w_u32(section, "master_transforms", static_cast<u32>(light.MasterTransforms.size()));
		for (u32 j = 0; j < light.MasterTransforms.size(); ++j)
		{
			const SMasterTransformKey& transform = light.MasterTransforms[j];
			string64 key = {};
			xr_sprintf(key, "master_transform_%03u_time", j);
			ini.w_float(section, key, transform.Time);
			xr_sprintf(key, "master_transform_%03u_position", j);
			ini.w_fvector3(section, key, transform.Position);
			xr_sprintf(key, "master_transform_%03u_rotation", j);
			ini.w_fvector3(section, key, transform.Rotation);
		}
		ini.w_bool(section, "master_track", light.MasterTrack);
		ini.w_bool(section, "pattern_owned", light.PatternOwned);
		ini.w_u32(section, "master_clips", static_cast<u32>(light.MasterClips.size()));
		for (u32 j = 0; j < light.MasterClips.size(); ++j)
		{
			const SMasterLightClip& clip = light.MasterClips[j];
			string64 key = {};
			xr_sprintf(key, "master_clip_%03u_start", j);
			ini.w_float(section, key, clip.Start);
			xr_sprintf(key, "master_clip_%03u_duration", j);
			ini.w_float(section, key, clip.Duration);
			xr_sprintf(key, "master_clip_%03u_track", j);
			ini.w_s32(section, key, clip.Track);
			xr_sprintf(key, "master_clip_%03u_muted", j);
			ini.w_bool(section, key, clip.Muted);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaSaveClipAppearance(ini, section, key, clip);
			xr_sprintf(key, "master_clip_%03u_intensity_start", j);
			ini.w_float(section, key, clip.StartIntensity);
			xr_sprintf(key, "master_clip_%03u_intensity_end", j);
			ini.w_float(section, key, clip.EndIntensity);
			xr_sprintf(key, "master_clip_%03u_color_start", j);
			ini.w_fvector3(section, key, clip.StartColor);
			xr_sprintf(key, "master_clip_%03u_color_end", j);
			ini.w_fvector3(section, key, clip.EndColor);
		}
		ini.w_fvector3(section, "position", light.Position);
		ini.w_fvector3(section, "rotation", rotation);
		ini.w_fvector3(section, "color", light.Color);
		ini.w_float(section, "intensity", light.Intensity);
		ini.w_float(section, "range", light.Range);
		ini.w_float(section, "cone", light.Cone);
		ini.w_float(section, "virtual_size", light.VirtualSize);
		ini.w_float(section, "volume_quality", light.VolumeQuality);
		ini.w_float(section, "volume_intensity", light.VolumeIntensity);
		ini.w_float(section, "volume_distance", light.VolumeDistance);
		ini.w_s32(section, "type", light.Type);
		ini.w_bool(section, "active", light.Active);
		ini.w_bool(section, "shadow", light.Shadow);
		ini.w_bool(section, "volumetric", light.Volumetric);
		ini.w_bool(section, "indirect", light.Indirect);
		ini.w_bool(section, "hud_mode", light.HudMode);
		ini.w_bool(section, "occlusion", light.Occlusion);
		ini.w_bool(section, "modulation", light.Modulation);
		ini.w_bool(section, "modulation_loop", light.Loop);
		ini.w_float(section, "modulation_duration", light.Duration);
		for (u32 channel = 0; channel < kCinemaLightChannels; ++channel)
		{
			string32 key = {};
			const auto& wave = light.Waves[channel];
			xr_sprintf(key, "wave_%u_type", channel);
			ini.w_s32(section, key, wave.Type);
			xr_sprintf(key, "wave_%u_amplitude", channel);
			ini.w_float(section, key, wave.Amplitude);
			xr_sprintf(key, "wave_%u_frequency", channel);
			ini.w_float(section, key, wave.Frequency);
			xr_sprintf(key, "wave_%u_phase", channel);
			ini.w_float(section, key, wave.Phase);
			xr_sprintf(key, "wave_%u_min", channel);
			ini.w_float(section, key, wave.Minimum);
			xr_sprintf(key, "wave_%u_max", channel);
			ini.w_float(section, key, wave.Maximum);
			xr_sprintf(key, "curve_%u_count", channel);
			ini.w_u32(section, key, static_cast<u32>(light.Curves[channel].size()));
			for (u32 k = 0; k < light.Curves[channel].size(); ++k)
			{
				xr_sprintf(key, "curve_%u_%u_time", channel, k);
				ini.w_float(section, key, light.Curves[channel][k].Time);
				xr_sprintf(key, "curve_%u_%u_value", channel, k);
				ini.w_float(section, key, light.Curves[channel][k].Value);
				xr_sprintf(key, "curve_%u_%u_smooth", channel, k);
				ini.w_bool(section, key, light.Curves[channel][k].Smooth);
				xr_sprintf(key, "curve_%u_%u_in_slope", channel, k);
				ini.w_float(section, key, light.Curves[channel][k].InSlope);
				xr_sprintf(key, "curve_%u_%u_out_slope", channel, k);
				ini.w_float(section, key, light.Curves[channel][k].OutSlope);
			}
		}
		if (light.Texture[0])
		{
			ini.w_string(section, "texture", light.Texture);
		}
	}
	ini.w_u32("scene", "particle_groups", static_cast<u32>(ParticleGroups.size()));
	for (u32 i = 0; i < ParticleGroups.size(); ++i)
	{
		string32 section = {};
		xr_sprintf(section, "particle_group_%03u", i);
		ini.w_fvector3(section, "position", ParticleGroups[i].Position);
		Fvector rotation = {};
		ParticleGroups[i].Orientation.getHPB(rotation);
		rotation.mul(rad2deg(1.0f));
		ini.w_fvector3(section, "rotation", rotation);
		ini.w_float(section, "scale", ParticleGroups[i].Scale);
		CinemaSaveMasterGroup(ini, section, ParticleGroups[i]);
	}
	for (u32 i = 0; i < Particles.size(); ++i)
	{
		string32 section = {};
		xr_sprintf(section, "particle_%03u", i);
		ini.w_string(section, "name", Particles[i].Name.c_str());
		ini.w_bool(section, "master_track", Particles[i].MasterTrack);
		ini.w_bool(section, "pattern_owned", Particles[i].PatternOwned);
		ini.w_u32(section, "master_transforms", static_cast<u32>(Particles[i].MasterTransforms.size()));
		for (u32 j = 0; j < Particles[i].MasterTransforms.size(); ++j)
		{
			const SMasterTransformKey& transform = Particles[i].MasterTransforms[j];
			string64 key = {};
			xr_sprintf(key, "master_transform_%03u_time", j);
			ini.w_float(section, key, transform.Time);
			xr_sprintf(key, "master_transform_%03u_position", j);
			ini.w_fvector3(section, key, transform.Position);
			xr_sprintf(key, "master_transform_%03u_rotation", j);
			ini.w_fvector3(section, key, transform.Rotation);
			xr_sprintf(key, "master_transform_%03u_scale", j);
			ini.w_float(section, key, transform.Scale.x);
		}
		ini.w_u32(section, "master_clips", static_cast<u32>(Particles[i].MasterClips.size()));
		for (u32 j = 0; j < Particles[i].MasterClips.size(); ++j)
		{
			string64 key = {};
			xr_sprintf(key, "master_clip_%03u_start", j);
			ini.w_float(section, key, Particles[i].MasterClips[j].Start);
			xr_sprintf(key, "master_clip_%03u_duration", j);
			ini.w_float(section, key, Particles[i].MasterClips[j].Duration);
			xr_sprintf(key, "master_clip_%03u_track", j);
			ini.w_s32(section, key, Particles[i].MasterClips[j].Track);
			xr_sprintf(key, "master_clip_%03u_muted", j);
			ini.w_bool(section, key, Particles[i].MasterClips[j].Muted);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaSaveClipAppearance(ini, section, key, Particles[i].MasterClips[j]);
		}
		ini.w_s32(section, "group", Particles[i].Group);
		ini.w_fvector3(section, "position", Particles[i].Position);
		Fvector rotation = {};
		Particles[i].Orientation.getHPB(rotation);
		rotation.mul(rad2deg(1.0f));
		ini.w_fvector3(section, "rotation", rotation);
		ini.w_float(section, "scale", Particles[i].Scale);
		ini.w_bool(section, "timeline", Particles[i].Timeline);
		ini.w_bool(section, "loop", Particles[i].Loop);
		ini.w_bool(section, "modulation", Particles[i].Modulation);
		ini.w_fvector3(section, "modulation_offset", Particles[i].EndOffset);
		ini.w_fvector3(section, "modulation_rotation", Particles[i].EndRotation);
		ini.w_float(section, "modulation_scale", Particles[i].EndScale);
		ini.w_u32(section, "curve_version", 1);
		ini.w_float(section, "speed", Particles[i].Speed);
		for (u32 channel = 0; channel < 8; ++channel)
		{
			string32 key = {};
			const auto& wave = Particles[i].Waves[channel];
			xr_sprintf(key, "wave_%u_type", channel);
			ini.w_s32(section, key, wave.Type);
			xr_sprintf(key, "wave_%u_amplitude", channel);
			ini.w_float(section, key, wave.Amplitude);
			xr_sprintf(key, "wave_%u_frequency", channel);
			ini.w_float(section, key, wave.Frequency);
			xr_sprintf(key, "wave_%u_phase", channel);
			ini.w_float(section, key, wave.Phase);
			xr_sprintf(key, "wave_%u_min", channel);
			ini.w_float(section, key, wave.Minimum);
			xr_sprintf(key, "wave_%u_max", channel);
			ini.w_float(section, key, wave.Maximum);
			xr_sprintf(key, "curve_%u_count", channel);
			ini.w_u32(section, key, static_cast<u32>(Particles[i].Curves[channel].size()));
			for (u32 k = 0; k < Particles[i].Curves[channel].size(); ++k)
			{
				xr_sprintf(key, "curve_%u_%u_time", channel, k);
				ini.w_float(section, key, Particles[i].Curves[channel][k].Time);
				xr_sprintf(key, "curve_%u_%u_value", channel, k);
				ini.w_float(section, key, Particles[i].Curves[channel][k].Value);
				xr_sprintf(key, "curve_%u_%u_smooth", channel, k);
				ini.w_bool(section, key, Particles[i].Curves[channel][k].Smooth);
				xr_sprintf(key, "curve_%u_%u_in_slope", channel, k);
				ini.w_float(section, key, Particles[i].Curves[channel][k].InSlope);
				xr_sprintf(key, "curve_%u_%u_out_slope", channel, k);
				ini.w_float(section, key, Particles[i].Curves[channel][k].OutSlope);
			}
		}
		ini.w_float(section, "in", Particles[i].In);
		ini.w_float(section, "out", Particles[i].Out);
	}

	for (u32 i = 0; i < static_cast<u32>(Actors.size()); ++i)
	{
		const SCinemaActor& actor = *Actors[i];

		string32 section = {};
		xr_sprintf(section, "actor_%03u", i);

		Fvector rotation = {};
		actor.Orientation.getHPB(rotation);
		rotation.mul(rad2deg(1.0f));

		ini.w_bool(section, "master_track", actor.MasterTrack);
		ini.w_bool(section, "pattern_owned", actor.PatternOwned);
		ini.w_u32(section, "master_transforms", static_cast<u32>(actor.MasterTransforms.size()));
		for (u32 j = 0; j < actor.MasterTransforms.size(); ++j)
		{
			const SMasterTransformKey& transform = actor.MasterTransforms[j];
			string64 key = {};
			xr_sprintf(key, "master_transform_%03u_time", j);
			ini.w_float(section, key, transform.Time);
			xr_sprintf(key, "master_transform_%03u_position", j);
			ini.w_fvector3(section, key, transform.Position);
			xr_sprintf(key, "master_transform_%03u_rotation", j);
			ini.w_fvector3(section, key, transform.Rotation);
			xr_sprintf(key, "master_transform_%03u_scale", j);
			ini.w_fvector3(section, key, transform.Scale);
		}
		ini.w_u32(section, "master_clips", static_cast<u32>(actor.MasterClips.size()));
		for (u32 j = 0; j < actor.MasterClips.size(); ++j)
		{
			const SMasterClip& clip = actor.MasterClips[j];
			string64 key = {};
			xr_sprintf(key, "master_clip_%03u_source", j);
			if (clip.Source.size()) ini.w_string(section, key, clip.Source.c_str());
			xr_sprintf(key, "master_clip_%03u_name", j);
			if (clip.Name.size()) ini.w_string(section, key, clip.Name.c_str());
			xr_sprintf(key, "master_clip_%03u_start", j);
			ini.w_float(section, key, clip.Start);
			xr_sprintf(key, "master_clip_%03u_duration", j);
			ini.w_float(section, key, clip.Duration);
			xr_sprintf(key, "master_clip_%03u_track", j);
			ini.w_s32(section, key, clip.Track);
			xr_sprintf(key, "master_clip_%03u_muted", j);
			ini.w_bool(section, key, clip.Muted);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaSaveClipAppearance(ini, section, key, clip);
			xr_sprintf(key, "master_clip_%03u_source_in", j);
			ini.w_float(section, key, clip.SourceIn);
			xr_sprintf(key, "master_clip_%03u_source_out", j);
			ini.w_float(section, key, clip.SourceOut);
			xr_sprintf(key, "master_clip_%03u_speed", j);
			ini.w_float(section, key, clip.Speed);
			xr_sprintf(key, "master_clip_%03u_shots_source_time", j);
			ini.w_bool(section, key, true);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaSaveShots(ini, section, key, clip.Shots);
		}
		ini.w_string(section, "name", actor.Name.c_str());
		ini.w_string(section, "visual", actor.Visual.c_str());
		ini.w_u32(section, "animation_packs", static_cast<u32>(actor.AnimationPacks.size()));
		for (u32 j = 0; j < static_cast<u32>(actor.AnimationPacks.size()); ++j)
		{
			string32 key = {};
			xr_sprintf(key, "animation_pack_%03u", j);
			ini.w_string(section, key, actor.AnimationPacks[j].c_str());
		}
		ini.w_fvector3(section, "position", actor.Position);
		ini.w_fvector3(section, "rotation", rotation);
		ini.w_fvector3(section, "scale", actor.Scale);
		ini.w_bool(section, "loop", actor.Loop);
		ini.w_float(section, "speed", actor.Speed);

		if (actor.ItemSection.size() > 0)
		{
			ini.w_string(section, "item", actor.ItemSection.c_str());
		}

		if (actor.Motion.valid())
		{
			ini.w_string(section, "pose_source", actor.MotionSource.c_str());
			ini.w_string(section, "pose_clip", actor.MotionName.c_str());
			ini.w_bool(section, "pose_fx", actor.MotionFx);
			ini.w_bool(section, "pose_paused", actor.Paused);

			if (actor.Animated != nullptr && !actor.MotionFx)
			{
				if (CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion))
				{
					ini.w_float(section, "pose_time", blend->timeCurrent);
				}
			}
		}

		ini.w_u32(section, "playlist", static_cast<u32>(actor.Playlist.size()));

		for (u32 j = 0; j < static_cast<u32>(actor.Playlist.size()); ++j)
		{
			const SPlaylistEntry& entry = actor.Playlist[j];

			string32 line = {};
			xr_sprintf(line, "playlist_%03u", j);

			string1024 value = {};
			xr_sprintf(value, "%s|%s|%u", entry.Source.c_str(), entry.Name.c_str(), entry.Fx ? 1u : 0u);
			ini.w_string(section, line, value);
			xr_sprintf(line, "playlist_%03u_muted", j);
			ini.w_bool(section, line, entry.Muted);
		}

		ini.w_bool(section, "pingpong", actor.PingPong);
		ini.w_u32(section, "trims", static_cast<u32>(actor.Trims.size()));

		for (u32 j = 0; j < static_cast<u32>(actor.Trims.size()); ++j)
		{
			const STrimRange& trim = actor.Trims[j];

			if (trim.Source.size() == 0 || trim.Name.size() == 0)
			{
				continue;
			}

			string32 line = {};
			xr_sprintf(line, "trim_%03u", j);

			xr_string value = "v2|";
			value += trim.Source.c_str();
			value += "|";
			value += trim.Name.c_str();

			string64 speedText = {};
			xr_sprintf(speedText, "|%.4f|%.4f|", trim.Speed, trim.Blend);
			value += speedText;

			for (size_t segmentIndex = 0; segmentIndex < trim.Segments.size(); ++segmentIndex)
			{
				string64 segmentText = {};
				xr_sprintf(segmentText, segmentIndex > 0 ? "%.4f,%.4f" : "%.4f,%.4f", trim.Segments[segmentIndex].Start, trim.Segments[segmentIndex].End);
				value += segmentText;

				if (segmentIndex + 1 < trim.Segments.size())
				{
					value += "|";
				}
			}

			ini.w_string(section, line, value.c_str());
			CinemaSaveShots(ini, section, line, trim.Shots);
		}
	}

	if (!CinemaSaveIni(ini, path))
	{
		dialog.push_back("Could not write the scene file:");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
		return;
	}

	SceneNameLast = cleanName;

	string256 summary = {};
	xr_sprintf(summary, "Saved %u NPC and %u particles to \"%s\".", static_cast<u32>(Actors.size()), static_cast<u32>(Particles.size()), cleanName.c_str());

	dialog.push_back(summary);
	dialog.push_back(path);
	ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
}

void CCinemaFilmMakerManager::BuildSceneList()
{
	SceneList.clear();

	if (Level == nullptr)
	{
		return;
	}

	string_path folder = {};
	CinemaSceneFolder(folder, Level->name());

	std::error_code error;
	std::filesystem::directory_iterator iterator(folder, error);

	if (error)
	{
		return;
	}

	const size_t extLength = xr_strlen(kCinemaSceneExt);

	for (const std::filesystem::directory_entry& entry : iterator)
	{
		if (!entry.is_regular_file(error))
		{
			continue;
		}

		xr_string fileName = entry.path().filename().string().c_str();

		if (fileName.size() <= extLength)
		{
			continue;
		}

		if (_stricmp(fileName.c_str() + fileName.size() - extLength, kCinemaSceneExt) != 0)
		{
			continue;
		}

		fileName.erase(fileName.size() - extLength);
		SceneList.push_back(fileName);
	}

	std::sort(SceneList.begin(), SceneList.end(), [](const xr_string& left, const xr_string& right)
	{
		return xr_strcmp(left.c_str(), right.c_str()) < 0;
	});
}

void CCinemaFilmMakerManager::RequestLoadScene()
{
	if (Level == nullptr)
	{
		return;
	}

	BuildSceneList();

	SceneListSelected.clear();
	SceneData.clear();

	ShowSceneDialog(ESceneDialog::Load, "Load scene", {});
}

void CCinemaFilmMakerManager::LoadSceneFile(const char* name)
{
	if (Level == nullptr)
	{
		return;
	}

	xr_vector<xr_string> dialog;
	xr_vector<xr_string> warnings;

	SceneData.clear();
	SceneCameras.clear();
	SceneParticles.clear();
	SceneLights.clear();
	SceneLightGroups.clear();
	SceneParticleGroups.clear();

	string_path path = {};
	CinemaScenePath(path, Level->name(), name);

	std::error_code error;

	if (!std::filesystem::exists(path, error))
	{
		dialog.push_back("Scene file not found:");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	CInifile ini(path, true, true, false);

	if (!ini.section_exist("scene"))
	{
		dialog.push_back("The file is not a Cinema Film Maker scene:");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	const s32 version = ini.line_exist("scene", "version") ? ini.r_s32("scene", "version") : 0;
	LoadedSceneVersion = version;

	if (version <= 0 || version > kCinemaSceneVersion)
	{
		dialog.push_back("Unsupported scene version: the file was saved by another editor build.");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	LoadedMasterDuration = ini.line_exist("scene", "master_duration") ? ini.r_float("scene", "master_duration") : 20.0f;
	if (!std::isfinite(LoadedMasterDuration) || LoadedMasterDuration < 0.1f || LoadedMasterDuration > 7210.0f)
	{
		LoadedMasterDuration = 20.0f;
		warnings.push_back("Invalid master timeline duration; using 20 seconds.");
	}
	LoadedMasterLoop = ini.line_exist("scene", "master_loop") && ini.r_bool("scene", "master_loop");
	LoadedMasterUseLoopRange = ini.read_if_exists<bool>("scene", "master_loop_range", false);
	LoadedMasterLoopStart = ini.read_if_exists<float>("scene", "master_loop_start", 0.0f);
	LoadedMasterLoopEnd = ini.read_if_exists<float>("scene", "master_loop_end", LoadedMasterDuration);
	if (!std::isfinite(LoadedMasterLoopStart) || !std::isfinite(LoadedMasterLoopEnd) || LoadedMasterLoopStart < 0.0f || LoadedMasterLoopEnd > LoadedMasterDuration || LoadedMasterLoopEnd - LoadedMasterLoopStart < 0.01f)
	{
		LoadedMasterUseLoopRange = false;
		LoadedMasterLoopStart = 0.0f;
		LoadedMasterLoopEnd = LoadedMasterDuration;
		warnings.push_back("Invalid master loop range; using the full scene.");
	}

	LoadedSoloTrack = ini.read_if_exists<s32>("scene", "master_solo_track", -1);
	if (LoadedSoloTrack < 0 || LoadedSoloTrack >= kCinemaMaxTracks) LoadedSoloTrack = -1;
	LoadedSoloChanged = LoadedSoloTrack >= 0 && ini.read_if_exists<bool>("scene", "master_solo_changed", false);
	for (u32 track = 0; track < kCinemaMaxTracks; ++track)
	{
		string32 key = {};
		xr_sprintf(key, "master_track_%02u_muted", track);
		LoadedTrackMuted[track] = ini.read_if_exists<bool>("scene", key, false);
		xr_sprintf(key, "master_track_%02u_parent", track);
		const s32 parent = ini.read_if_exists<s32>("scene", key, -1);
		LoadedTrackParent[track] = parent >= 0 && parent < kCinemaMaxTracks && parent != static_cast<s32>(track) ? static_cast<u16>(parent + 1) : 0;
		xr_sprintf(key, "master_track_%02u_group", track);
		LoadedTrackGroup[track] = ini.read_if_exists<bool>("scene", key, false);
		xr_sprintf(key, "master_track_%02u_collapsed", track);
		LoadedTrackCollapsed[track] = ini.read_if_exists<bool>("scene", key, false);
		xr_sprintf(key, "master_solo_%02u_muted", track);
		LoadedSoloPreviousMuted[track] = ini.read_if_exists<bool>("scene", key, false);
	}
	const u32 cameraCount = std::min(ini.read_if_exists<u32>("scene", "cameras", 0), kCinemaSceneMaxActors);
	for (u32 i = 0; i < cameraCount; ++i)
	{
		string32 section = {};
		xr_sprintf(section, "camera_%03u", i);
		if (!ini.section_exist(section)) continue;
		SCinemaCameraClip camera;
		camera.Start = ini.read_if_exists<float>(section, "start", 0.0f);
		camera.Duration = ini.read_if_exists<float>(section, "duration", 1.0f);
		camera.SourceDuration = ini.read_if_exists<float>(section, "source_duration", camera.Duration);
		camera.Track = ini.read_if_exists<s32>(section, "track", 0);
		camera.Muted = ini.read_if_exists<bool>(section, "muted", false);
		camera.Preview = ini.read_if_exists<bool>(section, "preview", false);
		camera.Fov = ini.read_if_exists<float>(section, "fov", 0.0f);
		if (!std::isfinite(camera.Fov) || (camera.Fov != 0.0f && (camera.Fov < 1.0f || camera.Fov > 179.0f))) camera.Fov = 0.0f;
		CinemaLoadClipAppearance(ini, section, "clip", camera);
		camera.Stretch = ini.read_if_exists<bool>(section, "stretch", true);
		camera.Smooth = ini.read_if_exists<bool>(section, "smooth", true);
		for (u32 channel = 0; channel < 9; ++channel)
		{
			string64 key = {};
			xr_sprintf(key, "camera_curve_%u_count", channel);
			const u32 count = std::min(ini.read_if_exists<u32>(section, key, 0), 128u);
			for (u32 k = 0; k < count; ++k)
			{
				SParticleCurveKey point;
				xr_sprintf(key, "camera_curve_%u_%03u_time", channel, k); point.Time = ini.read_if_exists<float>(section, key, -1.0f);
				xr_sprintf(key, "camera_curve_%u_%03u_value", channel, k); point.Value = ini.read_if_exists<float>(section, key, 0.0f);
				xr_sprintf(key, "camera_curve_%u_%03u_smooth", channel, k); point.Smooth = ini.read_if_exists<bool>(section, key, false);
				xr_sprintf(key, "camera_curve_%u_%03u_in", channel, k); point.InSlope = ini.read_if_exists<float>(section, key, 0.0f);
				xr_sprintf(key, "camera_curve_%u_%03u_out", channel, k); point.OutSlope = ini.read_if_exists<float>(section, key, 0.0f);
				const float lower = channel < 6 ? -3600.0f : channel == 6 ? 1.0f : 0.0f;
				const float upper = channel < 6 ? 3600.0f : channel == 6 ? 179.0f : 1.0f;
				if (!std::isfinite(point.Time) || point.Time < 0.0f || point.Time > 3600.0f || !std::isfinite(point.Value) || point.Value < lower || point.Value > upper || !std::isfinite(point.InSlope) || !std::isfinite(point.OutSlope)) continue;
				if (channel >= 7) point.Value = roundf(point.Value);
				point.InSlope = std::clamp(point.InSlope, -3600.0f, 3600.0f);
				point.OutSlope = std::clamp(point.OutSlope, -3600.0f, 3600.0f);
				camera.Curves[channel].push_back(point);
			}
			std::stable_sort(camera.Curves[channel].begin(), camera.Curves[channel].end(), [](const auto& a, const auto& b) { return a.Time < b.Time; });
		}
		if (!std::isfinite(camera.Start) || camera.Start < 0.0f || camera.Start > 3600.0f || !std::isfinite(camera.Duration) || camera.Duration < 0.01f || camera.Duration > 3600.0f || !std::isfinite(camera.SourceDuration) || camera.SourceDuration < 0.01f || camera.SourceDuration > 3600.0f || camera.Track < 0 || camera.Track >= kCinemaMaxTracks)
		{
			warnings.push_back("Invalid camera clip: skipped.");
			continue;
		}
		const u32 shakeCount = std::min(ini.read_if_exists<u32>(section, "shake_count", 0), 32u);
		for (u32 j = 0; j < shakeCount; ++j)
		{
			SCinemaCameraShake shake;
			string64 key = {};
			xr_sprintf(key, "shake_%02u_enabled", j); shake.Enabled = ini.read_if_exists<bool>(section, key, true);
			xr_sprintf(key, "shake_%02u_preset", j); shake.Preset = ini.read_if_exists<s32>(section, key, 0);
			xr_sprintf(key, "shake_%02u_influence", j); shake.Influence = ini.read_if_exists<float>(section, key, 1.0f);
			xr_sprintf(key, "shake_%02u_position_scale", j); shake.PositionScale = ini.read_if_exists<float>(section, key, 1.0f);
			xr_sprintf(key, "shake_%02u_speed", j); shake.Speed = ini.read_if_exists<float>(section, key, 1.0f);
			xr_sprintf(key, "shake_%02u_offset", j); shake.Offset = ini.read_if_exists<float>(section, key, 0.0f);
			xr_sprintf(key, "shake_%02u_fade_in", j); shake.FadeIn = ini.read_if_exists<float>(section, key, 0.0f);
			xr_sprintf(key, "shake_%02u_fade_out", j); shake.FadeOut = ini.read_if_exists<float>(section, key, 0.0f);
			xr_sprintf(key, "shake_%02u_stretch", j); shake.Stretch = ini.read_if_exists<bool>(section, key, false);
			if (shake.Preset < 0 || shake.Preset >= static_cast<s32>(IM_ARRAYSIZE(CinemaShakePresets)) ||
				!std::isfinite(shake.Influence) || shake.Influence < 0.0f || shake.Influence > 4.0f ||
				!std::isfinite(shake.PositionScale) || shake.PositionScale < 0.0f || shake.PositionScale > 100.0f ||
				!std::isfinite(shake.Speed) || shake.Speed < 0.0f || shake.Speed > 100.0f ||
				!std::isfinite(shake.Offset) || fabsf(shake.Offset) > 3600.0f ||
				!std::isfinite(shake.FadeIn) || shake.FadeIn < 0.0f || shake.FadeIn > 3600.0f ||
				!std::isfinite(shake.FadeOut) || shake.FadeOut < 0.0f || shake.FadeOut > 3600.0f)
			{
				warnings.push_back("Invalid camera shake layer: skipped.");
				continue;
			}
			for (u32 channel = 0; channel < 9; ++channel)
			{
				xr_sprintf(key, "shake_%02u_curve_%u_count", j, channel);
				const u32 count = std::min(ini.read_if_exists<u32>(section, key, 0), 128u);
				for (u32 k = 0; k < count; ++k)
				{
					SParticleCurveKey point;
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_time", j, channel, k); point.Time = ini.read_if_exists<float>(section, key, -1.0f);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_value", j, channel, k); point.Value = ini.read_if_exists<float>(section, key, channel == 3 ? 0.0f : 1.0f);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_smooth", j, channel, k); point.Smooth = ini.read_if_exists<bool>(section, key, false);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_in", j, channel, k); point.InSlope = ini.read_if_exists<float>(section, key, 0.0f);
					xr_sprintf(key, "shake_%02u_curve_%u_%03u_out", j, channel, k); point.OutSlope = ini.read_if_exists<float>(section, key, 0.0f);
					const float lower = channel == 3 ? -3600.0f : 0.0f;
					const float upper = channel == 3 || channel >= 7 ? 3600.0f : channel == 5 ? static_cast<float>(IM_ARRAYSIZE(CinemaShakePresets) - 1) : channel == 4 || channel == 6 ? 1.0f : 10.0f;
					if (!std::isfinite(point.Time) || point.Time < 0.0f || point.Time > 3600.0f || !std::isfinite(point.Value) || point.Value < lower || point.Value > upper || !std::isfinite(point.InSlope) || !std::isfinite(point.OutSlope)) continue;
					if (channel >= 4 && channel <= 6) point.Value = roundf(point.Value);
					point.InSlope = std::clamp(point.InSlope, -3600.0f, 3600.0f);
					point.OutSlope = std::clamp(point.OutSlope, -3600.0f, 3600.0f);
					shake.Curves[channel].push_back(point);
				}
				std::stable_sort(shake.Curves[channel].begin(), shake.Curves[channel].end(), [](const auto& a, const auto& b) { return a.Time < b.Time; });
			}
			camera.Shakes.push_back(std::move(shake));
		}
		const u32 keyCount = std::min(ini.read_if_exists<u32>(section, "keys", 0), 1024u);
		for (u32 j = 0; j < keyCount; ++j)
		{
			string64 timeKey = {}, positionKey = {}, rotationKey = {};
			xr_sprintf(timeKey, "key_%03u_time", j);
			xr_sprintf(positionKey, "key_%03u_position", j);
			xr_sprintf(rotationKey, "key_%03u_rotation", j);
			if (!ini.line_exist(section, timeKey) || !ini.line_exist(section, positionKey) || !ini.line_exist(section, rotationKey)) continue;
			SCinemaCameraKey key;
			key.Time = ini.r_float(section, timeKey);
			key.Position = ini.r_fvector3(section, positionKey);
			const Fvector rotation = ini.r_fvector3(section, rotationKey);
			if (!std::isfinite(key.Time) || key.Time < 0.0f || key.Time > 3600.0f || !_valid(key.Position) || !_valid(rotation)) continue;
			key.Orientation.setHPB(rotation.x, rotation.y, rotation.z);
			camera.Keys.push_back(key);
		}
		std::stable_sort(camera.Keys.begin(), camera.Keys.end(), [](const auto& a, const auto& b) { return a.Time < b.Time; });
		SceneCameras.push_back(std::move(camera));
	}
	const shared_str fileLevel = CinemaReadString(ini, "scene", "level");
	const shared_str filePlatform = CinemaReadString(ini, "scene", "platform");
	const shared_str currentLevel = Level->name();
	const shared_str currentPlatform = EngineExternal().GetCurrentPlatformName();

	if (fileLevel.size() > 0 && xr_strcmp(fileLevel.c_str(), currentLevel.c_str()) != 0)
	{
		warnings.push_back(xr_string("Scene was saved on level \"") + fileLevel.c_str() + "\", current level is \"" + currentLevel.c_str() + "\": NPC positions may not match this level.");
	}

	if (filePlatform.size() > 0 && xr_strcmp(filePlatform.c_str(), currentPlatform.c_str()) != 0)
	{
		warnings.push_back(xr_string("Scene was saved for platform \"") + filePlatform.c_str() + "\", current platform is \"" + currentPlatform.c_str() + "\": models, animations and items may be missing.");
	}

	for (CInifile::Sect& section : ini.sections())
	{
		if (section.Name.size() == 0 || _strnicmp(section.Name.c_str(), "actor_", 6) != 0)
		{
			continue;
		}

		if (SceneData.size() >= kCinemaSceneMaxActors)
		{
			warnings.push_back("The scene contains too many NPC: the rest was ignored.");
			break;
		}

		const char* sect = section.Name.c_str();

		SSceneActorData data;
		data.MasterTrack = ini.line_exist(sect, "master_track") && ini.r_bool(sect, "master_track");
		data.PatternOwned = ini.read_if_exists<bool>(sect, "pattern_owned", false);
		const u32 transformCount = ini.read_if_exists<u32>(sect, "master_transforms", 0);
		for (u32 j = 0; j < std::min(transformCount, kCinemaSceneMaxPlaylist); ++j)
		{
			SMasterTransformKey transform;
			string64 key = {};
			xr_sprintf(key, "master_transform_%03u_time", j);
			transform.Time = ini.read_if_exists<float>(sect, key, -1.0f);
			xr_sprintf(key, "master_transform_%03u_position", j);
			transform.Position = ini.read_if_exists<Fvector>(sect, key, transform.Position);
			xr_sprintf(key, "master_transform_%03u_rotation", j);
			transform.Rotation = ini.read_if_exists<Fvector>(sect, key, transform.Rotation);
			xr_sprintf(key, "master_transform_%03u_scale", j);
			transform.Scale = ini.read_if_exists<Fvector>(sect, key, transform.Scale);
			if (std::isfinite(transform.Time) && transform.Time >= 0.0f && transform.Time <= 3600.0f && _valid(transform.Position) && _valid(transform.Rotation) && _valid(transform.Scale) && transform.Scale.x >= 0.1f && transform.Scale.x <= 5.0f && transform.Scale.y >= 0.1f && transform.Scale.y <= 5.0f && transform.Scale.z >= 0.1f && transform.Scale.z <= 5.0f)
			{
				data.MasterTransforms.push_back(transform);
			}
			else
			{
				warnings.push_back("Invalid master actor transform key; it was skipped.");
			}
		}
		const u32 masterCount = ini.line_exist(sect, "master_clips") ? ini.r_u32(sect, "master_clips") : 0;
		for (u32 j = 0; j < std::min(masterCount, kCinemaSceneMaxPlaylist); ++j)
		{
			string64 key = {};
			SMasterClip clip;
			xr_sprintf(key, "master_clip_%03u_source", j);
			clip.Source = CinemaReadString(ini, sect, key);
			xr_sprintf(key, "master_clip_%03u_name", j);
			clip.Name = CinemaReadString(ini, sect, key);
			xr_sprintf(key, "master_clip_%03u_start", j);
			clip.Start = ini.line_exist(sect, key) ? ini.r_float(sect, key) : 0.0f;
			xr_sprintf(key, "master_clip_%03u_duration", j);
			clip.Duration = ini.line_exist(sect, key) ? ini.r_float(sect, key) : 0.0f;
			xr_sprintf(key, "master_clip_%03u_track", j);
			clip.Track = std::clamp(ini.read_if_exists<s32>(sect, key, static_cast<s32>(SceneData.size() % kCinemaMaxTracks)), 0, kCinemaMaxTracks - 1);
			xr_sprintf(key, "master_clip_%03u_muted", j);
			clip.Muted = ini.read_if_exists<bool>(sect, key, false);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaLoadClipAppearance(ini, sect, key, clip);
			xr_sprintf(key, "master_clip_%03u_source_in", j);
			clip.SourceIn = ini.read_if_exists<float>(sect, key, 0.0f);
			xr_sprintf(key, "master_clip_%03u_source_out", j);
			clip.SourceOut = ini.read_if_exists<float>(sect, key, 0.0f);
			xr_sprintf(key, "master_clip_%03u_speed", j);
			clip.Speed = ini.read_if_exists<float>(sect, key, 1.0f);
			if (!std::isfinite(clip.SourceIn) || clip.SourceIn < 0.0f || clip.SourceIn > 3600.0f || !std::isfinite(clip.SourceOut) || clip.SourceOut < 0.0f || clip.SourceOut > 3600.0f || (clip.SourceOut > 0.0f && clip.SourceOut <= clip.SourceIn) || !std::isfinite(clip.Speed) || clip.Speed < 0.05f || clip.Speed > 4.0f)
			{
				clip.SourceIn = 0.0f;
				clip.SourceOut = 0.0f;
				clip.Speed = 1.0f;
				warnings.push_back("Invalid master animation trim or speed; using defaults.");
			}
			xr_sprintf(key, "master_clip_%03u_shots_source_time", j);
			const bool shotsUseSourceTime = ini.read_if_exists<bool>(sect, key, false);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaLoadShots(ini, sect, key, clip.Shots);
			if (!shotsUseSourceTime)
			{
				for (float& shot : clip.Shots)
				{
					shot = CinemaMasterSourceTime(clip.SourceIn, clip.Speed, shot);
				}
			}
			if (((clip.Source.size() && clip.Name.size()) || (!clip.Source.size() && !clip.Name.size())) && std::isfinite(clip.Start) && std::isfinite(clip.Duration) && clip.Start >= 0.0f && clip.Start <= 3600.0f && clip.Duration >= 0.01f && clip.Duration <= 3600.0f)
			{
				data.MasterClips.push_back(std::move(clip));
			}
			else
			{
				warnings.push_back("Invalid master animation clip; it was skipped.");
			}
		}

		const shared_str actorName = CinemaReadString(ini, sect, "name");

		if (actorName.size() > 0)
		{
			data.Name = actorName.c_str();
		}

		if (data.Name.empty())
		{
			string32 generated = {};
			xr_sprintf(generated, "FakeNPC_%02u", static_cast<u32>(SceneData.size()) + 1);
			data.Name = generated;
		}

		data.Visual = CinemaReadString(ini, sect, "visual");

		if (data.Visual.size() == 0 || !VisualPathExists(data.Visual.c_str()))
		{
			warnings.push_back(xr_string("Model not found for \"") + data.Name + "\": " + (data.Visual.size() > 0 ? data.Visual.c_str() : "<empty>") + " - NPC skipped.");
			continue;
		}

		if (ini.line_exist(sect, "position"))
		{
			data.Position = ini.r_fvector3(sect, "position");
		}

		if (ini.line_exist(sect, "rotation"))
		{
			data.Rotation = ini.r_fvector3(sect, "rotation");
		}

		if (ini.line_exist(sect, "scale"))
		{
			data.Scale = ini.r_fvector3(sect, "scale");
		}

		if (!_valid(data.Position) || !_valid(data.Rotation) || !_valid(data.Scale))
		{
			warnings.push_back(xr_string("Broken coordinates for \"") + data.Name + "\" - NPC skipped.");
			continue;
		}

		data.Scale.x = std::clamp(data.Scale.x, 0.1f, 5.0f);
		data.Scale.y = std::clamp(data.Scale.y, 0.1f, 5.0f);
		data.Scale.z = std::clamp(data.Scale.z, 0.1f, 5.0f);

		data.Loop = ini.line_exist(sect, "loop") && ini.r_bool(sect, "loop");
		data.Speed = ini.line_exist(sect, "speed") ? ini.r_float(sect, "speed") : 1.0f;

		data.Speed = std::isfinite(data.Speed) ? std::clamp(data.Speed, -4.0f, 4.0f) : 1.0f;

		if (ini.line_exist(sect, "item"))
		{
			const shared_str item = CinemaReadString(ini, sect, "item");

			if (item.size() > 0)
			{
				BuildHandItems();
				const bool available = std::any_of(HandItems.begin(), HandItems.end(), [&](const SHandItem& entry) { return entry.Section == item; });

				if (available)
				{
					data.Item = item;
				}
				else
				{
					warnings.push_back(xr_string("Item \"") + item.c_str() + "\" is not available in this build - removed from \"" + data.Name + "\".");
				}
			}
		}

		if (ini.line_exist(sect, "pose_clip"))
		{
			data.PoseClip = CinemaReadString(ini, sect, "pose_clip");
			data.PoseSource = CinemaReadString(ini, sect, "pose_source");
			data.PoseFx = ini.line_exist(sect, "pose_fx") && ini.r_bool(sect, "pose_fx");
			data.PoseTime = ini.line_exist(sect, "pose_time") ? ini.r_float(sect, "pose_time") : 0.0f;
			data.PosePaused = ini.line_exist(sect, "pose_paused") && ini.r_bool(sect, "pose_paused");
		}

		const u32 packCount = ini.line_exist(sect, "animation_packs") ? ini.r_u32(sect, "animation_packs") : 0;
		for (u32 j = 0; j < std::min(packCount, kCinemaSceneMaxPlaylist); ++j)
		{
			string32 line = {};
			xr_sprintf(line, "animation_pack_%03u", j);
			const shared_str pack = CinemaReadString(ini, sect, line);
			if (CinemaIsOmfPath(pack) && std::find(data.AnimationPacks.begin(), data.AnimationPacks.end(), pack) == data.AnimationPacks.end()) data.AnimationPacks.push_back(pack);
		}
		const u32 playlistCount = ini.line_exist(sect, "playlist") ? ini.r_u32(sect, "playlist") : 0;
		const u32 playlistLimit = std::min(playlistCount, kCinemaSceneMaxPlaylist);

		for (u32 j = 0; j < playlistLimit; ++j)
		{
			string32 line = {};
			xr_sprintf(line, "playlist_%03u", j);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			SPlaylistEntry entry;
			entry.Source = text.substr(0, first).c_str();
			entry.Name = text.substr(first + 1, second - first - 1).c_str();
			entry.Fx = text.compare(second + 1, xr_string::npos, "1") == 0;
			xr_sprintf(line, "playlist_%03u_muted", j);
			entry.Muted = ini.line_exist(sect, line) && ini.r_bool(sect, line);
			data.Playlist.push_back(entry);
		}

		data.PingPong = ini.line_exist(sect, "pingpong") && ini.r_bool(sect, "pingpong");

		const u32 trimCount = ini.line_exist(sect, "trims") ? ini.r_u32(sect, "trims") : 0;
		const u32 trimLimit = std::min(trimCount, kCinemaSceneMaxPlaylist);

		for (u32 j = 0; j < trimLimit; ++j)
		{
			string32 line = {};
			xr_sprintf(line, "trim_%03u", j);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			const size_t third = text.find('|', second + 1);

			if (third == xr_string::npos)
			{
				continue;
			}

			STrimRange trim;

			if (!CinemaParseTrim(value, trim))
			{
				continue;
			}

			CinemaLoadShots(ini, sect, line, trim.Shots);
			data.Trims.push_back(trim);
		}

		SceneData.push_back(std::move(data));
	}

	const u32 groupCount = ini.read_if_exists<u32>("scene", "particle_groups", 0);
	for (u32 i = 0; i < std::min(groupCount, kCinemaSceneMaxActors); ++i)
	{
		string32 section = {};
		xr_sprintf(section, "particle_group_%03u", i);
		SCinemaParticleGroup group;
		if (ini.section_exist(section))
		{
			group.Position = ini.read_if_exists<Fvector>(section, "position", group.Position);
			const Fvector rotation = ini.read_if_exists<Fvector>(section, "rotation", Fvector().set(0.0f, 0.0f, 0.0f));
			group.Scale = ini.read_if_exists<float>(section, "scale", 1.0f);
			if (std::isfinite(group.Position.x) && std::isfinite(group.Position.y) && std::isfinite(group.Position.z) &&
				std::isfinite(rotation.x) && std::isfinite(rotation.y) && std::isfinite(rotation.z) && std::isfinite(group.Scale) && group.Scale >= 0.1f && group.Scale <= 5.0f)
			{
				group.Orientation.setHPB(deg2rad(rotation.x), deg2rad(rotation.y), deg2rad(rotation.z));
			}
			else
			{
				group = SCinemaParticleGroup();
				warnings.push_back("Invalid particle group transform: using defaults.");
			}
		}
		if (ini.section_exist(section))
		{
			CinemaLoadMasterGroup(ini, section, group, warnings);
		}
		SceneParticleGroups.push_back(group);
	}
	if (groupCount > kCinemaSceneMaxActors)
	{
		warnings.push_back("Too many particle groups: the rest were ignored.");
	}

	for (CInifile::Sect& section : ini.sections())
	{
		if (section.Name.size() == 0 || _strnicmp(section.Name.c_str(), "particle_", 9) != 0 || _strnicmp(section.Name.c_str(), "particle_group_", 15) == 0)
		{
			continue;
		}
		if (SceneParticles.size() >= kCinemaSceneMaxActors)
		{
			warnings.push_back("The scene contains too many particles: the rest was ignored.");
			break;
		}
		SCinemaParticle particle;
		particle.MasterTrack = ini.read_if_exists<bool>(section.Name.c_str(), "master_track", false);
		particle.PatternOwned = ini.read_if_exists<bool>(section.Name.c_str(), "pattern_owned", false);
		const u32 transformCount = ini.read_if_exists<u32>(section.Name.c_str(), "master_transforms", 0);
		for (u32 j = 0; j < std::min(transformCount, kCinemaSceneMaxPlaylist); ++j)
		{
			SMasterTransformKey transform;
			string64 key = {};
			xr_sprintf(key, "master_transform_%03u_time", j);
			transform.Time = ini.read_if_exists<float>(section.Name.c_str(), key, -1.0f);
			xr_sprintf(key, "master_transform_%03u_position", j);
			transform.Position = ini.read_if_exists<Fvector>(section.Name.c_str(), key, transform.Position);
			xr_sprintf(key, "master_transform_%03u_rotation", j);
			transform.Rotation = ini.read_if_exists<Fvector>(section.Name.c_str(), key, transform.Rotation);
			xr_sprintf(key, "master_transform_%03u_scale", j);
			const float scale = ini.read_if_exists<float>(section.Name.c_str(), key, 1.0f);
			if (std::isfinite(transform.Time) && transform.Time >= 0.0f && transform.Time <= 3600.0f && _valid(transform.Position) && _valid(transform.Rotation) && std::isfinite(scale) && scale >= 0.1f && scale <= 5.0f)
			{
				transform.Scale.set(scale, scale, scale);
				particle.MasterTransforms.push_back(transform);
			}
			else
			{
				warnings.push_back("Invalid master particle transform key; it was skipped.");
			}
		}
		const u32 masterCount = ini.read_if_exists<u32>(section.Name.c_str(), "master_clips", 0);
		for (u32 j = 0; j < std::min(masterCount, kCinemaSceneMaxPlaylist); ++j)
		{
			SMasterParticleClip clip;
			string64 key = {};
			xr_sprintf(key, "master_clip_%03u_start", j);
			clip.Start = ini.read_if_exists<float>(section.Name.c_str(), key, 0.0f);
			xr_sprintf(key, "master_clip_%03u_duration", j);
			clip.Duration = ini.read_if_exists<float>(section.Name.c_str(), key, 1.0f);
			xr_sprintf(key, "master_clip_%03u_track", j);
			clip.Track = std::clamp(ini.read_if_exists<s32>(section.Name.c_str(), key, static_cast<s32>((SceneParticles.size() + 10) % kCinemaMaxTracks)), 0, kCinemaMaxTracks - 1);
			xr_sprintf(key, "master_clip_%03u_muted", j);
			clip.Muted = ini.read_if_exists<bool>(section.Name.c_str(), key, false);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaLoadClipAppearance(ini, section.Name.c_str(), key, clip);
			if (std::isfinite(clip.Start) && clip.Start >= 0.0f && clip.Start <= 3600.0f && std::isfinite(clip.Duration) && clip.Duration >= 0.01f && clip.Duration <= 3600.0f)
			{
				particle.MasterClips.push_back(clip);
			}
			else
			{
				warnings.push_back("Invalid master particle clip; it was skipped.");
			}
		}
		particle.Group = ini.read_if_exists<s32>(section.Name.c_str(), "group", -1);
		if (particle.Group < -1 || particle.Group >= static_cast<s32>(SceneParticleGroups.size()))
		{
			particle.Group = -1;
			warnings.push_back("Invalid particle group: particle left ungrouped.");
		}
		particle.Name = CinemaReadString(ini, section.Name.c_str(), "name");
		if (!IsCinemaParticle(particle.Name))
		{
			warnings.push_back(xr_string("Particle not found: ") + (particle.Name.size() ? particle.Name.c_str() : "<empty>"));
			continue;
		}
		if (ini.line_exist(section.Name.c_str(), "position"))
		{
			particle.Position = ini.r_fvector3(section.Name.c_str(), "position");
			if (!_valid(particle.Position))
			{
				warnings.push_back(xr_string("Broken coordinates for particle \"") + particle.Name.c_str() + "\" - skipped.");
				continue;
			}
		}
		if (ini.line_exist(section.Name.c_str(), "rotation"))
		{
			const Fvector rotation = ini.r_fvector3(section.Name.c_str(), "rotation");
			if (std::isfinite(rotation.x) && std::isfinite(rotation.y) && std::isfinite(rotation.z))
			{
				particle.Orientation.setHPB(deg2rad(rotation.x), deg2rad(rotation.y), deg2rad(rotation.z));
			}
		}
		particle.Scale = ini.read_if_exists<float>(section.Name.c_str(), "scale", 1.0f);
		if (!std::isfinite(particle.Scale) || particle.Scale < 0.1f || particle.Scale > 5.0f)
		{
			particle.Scale = 1.0f;
			warnings.push_back(xr_string("Invalid particle scale: ") + particle.Name.c_str() + " - using default.");
		}
		particle.Timeline = true;
		particle.Loop = ini.read_if_exists<bool>(section.Name.c_str(), "loop", false);
		particle.Speed = ini.read_if_exists<float>(section.Name.c_str(), "speed", 1.0f);
		if (!std::isfinite(particle.Speed) || particle.Speed < 0.0f || particle.Speed > 4.0f)
		{
			particle.Speed = 1.0f;
			warnings.push_back(xr_string("Invalid particle speed: ") + particle.Name.c_str());
		}
		particle.Modulation = ini.read_if_exists<bool>(section.Name.c_str(), "modulation", false);
		particle.EndOffset = ini.read_if_exists<Fvector>(section.Name.c_str(), "modulation_offset", particle.EndOffset);
		particle.EndRotation = ini.read_if_exists<Fvector>(section.Name.c_str(), "modulation_rotation", particle.EndRotation);
		particle.EndScale = ini.read_if_exists<float>(section.Name.c_str(), "modulation_scale", 1.0f);
		if (!std::isfinite(particle.EndOffset.x) || !std::isfinite(particle.EndOffset.y) || !std::isfinite(particle.EndOffset.z) ||
			!std::isfinite(particle.EndRotation.x) || !std::isfinite(particle.EndRotation.y) || !std::isfinite(particle.EndRotation.z) ||
			!std::isfinite(particle.EndScale) || particle.EndScale < 0.1f || particle.EndScale > 5.0f)
		{
			particle.Modulation = false;
			particle.EndOffset.set(0.0f, 0.0f, 0.0f);
			particle.EndRotation.set(0.0f, 0.0f, 0.0f);
			particle.EndScale = 1.0f;
			warnings.push_back(xr_string("Invalid particle modulation: ") + particle.Name.c_str());
		}
		particle.In = ini.read_if_exists<float>(section.Name.c_str(), "in", 0.0f);
		particle.Out = ini.read_if_exists<float>(section.Name.c_str(), "out", 10.0f);
		if (!std::isfinite(particle.In) || !std::isfinite(particle.Out) || !ValidParticleTimeline(particle.In, particle.Out))
		{
			warnings.push_back(xr_string("Invalid particle timeline: ") + particle.Name.c_str() + " - using defaults.");
			particle.Timeline = true;
			particle.In = 0.0f;
			particle.Out = 10.0f;
		}
		if (ini.read_if_exists<u32>(section.Name.c_str(), "curve_version", 0) == 1)
		{
			for (u32 channel = 0; channel < 8; ++channel)
			{
				string32 key = {};
				auto& wave = particle.Waves[channel];
				xr_sprintf(key, "wave_%u_type", channel);
				wave.Type = ini.read_if_exists<s32>(section.Name.c_str(), key, 0);
				xr_sprintf(key, "wave_%u_amplitude", channel);
				wave.Amplitude = ini.read_if_exists<float>(section.Name.c_str(), key, 0.0f);
				xr_sprintf(key, "wave_%u_frequency", channel);
				wave.Frequency = ini.read_if_exists<float>(section.Name.c_str(), key, 1.0f);
				xr_sprintf(key, "wave_%u_phase", channel);
				wave.Phase = ini.read_if_exists<float>(section.Name.c_str(), key, 0.0f);
				xr_sprintf(key, "wave_%u_min", channel);
				wave.Minimum = ini.read_if_exists<float>(section.Name.c_str(), key, wave.Minimum);
				xr_sprintf(key, "wave_%u_max", channel);
				wave.Maximum = ini.read_if_exists<float>(section.Name.c_str(), key, wave.Maximum);
				if (wave.Type < 0 || wave.Type > 10 || !std::isfinite(wave.Amplitude) || wave.Amplitude < 0.0f || wave.Amplitude > 3600.0f || !std::isfinite(wave.Frequency) || wave.Frequency < 0.0f || wave.Frequency > 100.0f || !std::isfinite(wave.Phase) || fabsf(wave.Phase) > 100.0f || !std::isfinite(wave.Minimum) || !std::isfinite(wave.Maximum) || wave.Minimum < (channel == 6 ? 0.1f : channel == 7 ? 0.0f : -3600.0f) || wave.Maximum > (channel == 6 ? 5.0f : channel == 7 ? 4.0f : 3600.0f) || wave.Minimum > wave.Maximum)
				{
					wave = SParticleWave();
					if (channel == 6)
					{
						wave.Minimum = 0.1f;
						wave.Maximum = 5.0f;
					}
					else if (channel == 7)
					{
						wave.Minimum = 0.0f;
						wave.Maximum = 4.0f;
					}
					warnings.push_back(xr_string("Invalid particle wave: ") + particle.Name.c_str());
				}
				xr_sprintf(key, "curve_%u_count", channel);
				const u32 count = std::min(ini.read_if_exists<u32>(section.Name.c_str(), key, 0), 128u);
				for (u32 k = 0; k < count; ++k)
				{
					xr_sprintf(key, "curve_%u_%u_time", channel, k);
					const float time = ini.read_if_exists<float>(section.Name.c_str(), key, -1.0f);
					xr_sprintf(key, "curve_%u_%u_value", channel, k);
					const float value = ini.read_if_exists<float>(section.Name.c_str(), key, channel >= 6 ? 1.0f : 0.0f);
					if (std::isfinite(time) && time >= 0.0f && time <= 600.0f && std::isfinite(value) && value >= (channel == 6 ? 0.1f : channel == 7 ? 0.0f : -3600.0f) && value <= (channel == 6 ? 5.0f : channel == 7 ? 4.0f : 3600.0f))
					{
						SParticleCurveKey curveKey{ time, value };
						xr_sprintf(key, "curve_%u_%u_smooth", channel, k);
						curveKey.Smooth = ini.read_if_exists<bool>(section.Name.c_str(), key, false);
						xr_sprintf(key, "curve_%u_%u_in_slope", channel, k);
						curveKey.InSlope = ini.read_if_exists<float>(section.Name.c_str(), key, 0.0f);
						xr_sprintf(key, "curve_%u_%u_out_slope", channel, k);
						curveKey.OutSlope = ini.read_if_exists<float>(section.Name.c_str(), key, 0.0f);
						curveKey.InSlope = std::isfinite(curveKey.InSlope) ? std::clamp(curveKey.InSlope, -3600.0f, 3600.0f) : 0.0f;
						curveKey.OutSlope = std::isfinite(curveKey.OutSlope) ? std::clamp(curveKey.OutSlope, -3600.0f, 3600.0f) : 0.0f;
						particle.Curves[channel].push_back(curveKey);
					}
				}
				std::stable_sort(particle.Curves[channel].begin(), particle.Curves[channel].end(), [](const auto& a, const auto& b) { return a.Time < b.Time; });
			}
		}
		else if (particle.Modulation)
		{
			const float values[] = { particle.EndOffset.x, particle.EndOffset.y, particle.EndOffset.z, particle.EndRotation.x, particle.EndRotation.y, particle.EndRotation.z, particle.EndScale };
			for (u32 channel = 0; channel < 7; ++channel)
			{
				particle.Curves[channel].push_back({ 0.0f, channel == 6 ? 1.0f : 0.0f });
				particle.Curves[channel].push_back({ particle.Out, values[channel] });
			}
		}
		particle.Modulation = true;
		SceneParticles.push_back(std::move(particle));
	}

	const u32 lightGroupCount = ini.read_if_exists<u32>("scene", "light_groups", 0);
	for (u32 i = 0; i < std::min(lightGroupCount, kCinemaSceneMaxActors); ++i)
	{
		string32 section = {};
		xr_sprintf(section, "light_group_%03u", i);
		SCinemaParticleGroup group;
		if (ini.section_exist(section))
		{
			group.Position = ini.read_if_exists<Fvector>(section, "position", group.Position);
			const Fvector rotation = ini.read_if_exists<Fvector>(section, "rotation", Fvector().set(0.0f, 0.0f, 0.0f));
			group.Scale = ini.read_if_exists<float>(section, "scale", 1.0f);
			if (std::isfinite(group.Position.x) && std::isfinite(group.Position.y) && std::isfinite(group.Position.z) &&
				std::isfinite(rotation.x) && std::isfinite(rotation.y) && std::isfinite(rotation.z) && std::isfinite(group.Scale) && group.Scale >= 0.1f && group.Scale <= 5.0f)
			{
				group.Orientation.setHPB(deg2rad(rotation.x), deg2rad(rotation.y), deg2rad(rotation.z));
			}
			else
			{
				group = SCinemaParticleGroup();
				warnings.push_back("Invalid light group transform: using defaults.");
			}
		}
		if (ini.section_exist(section))
		{
			CinemaLoadMasterGroup(ini, section, group, warnings);
		}
		SceneLightGroups.push_back(group);
	}
	if (lightGroupCount > kCinemaSceneMaxActors)
	{
		warnings.push_back("Too many light groups: the rest were ignored.");
	}
	const u32 lightCount = ini.read_if_exists<u32>("scene", "lights", 0);
	for (u32 i = 0; i < std::min(lightCount, static_cast<u32>(ini.sections().size())); ++i)
	{
		string32 section = {};
		xr_sprintf(section, "light_%03u", i);
		if (!ini.section_exist(section))
		{
			warnings.push_back("A light section is missing.");
			continue;
		}
		SCinemaLight light;
		light.MasterTrack = ini.read_if_exists<bool>(section, "master_track", false);
		light.PatternOwned = ini.read_if_exists<bool>(section, "pattern_owned", false);
		const u32 transformCount = ini.read_if_exists<u32>(section, "master_transforms", 0);
		for (u32 j = 0; j < std::min(transformCount, kCinemaSceneMaxPlaylist); ++j)
		{
			SMasterTransformKey transform;
			string64 key = {};
			xr_sprintf(key, "master_transform_%03u_time", j);
			transform.Time = ini.read_if_exists<float>(section, key, -1.0f);
			xr_sprintf(key, "master_transform_%03u_position", j);
			transform.Position = ini.read_if_exists<Fvector>(section, key, transform.Position);
			xr_sprintf(key, "master_transform_%03u_rotation", j);
			transform.Rotation = ini.read_if_exists<Fvector>(section, key, transform.Rotation);
			if (std::isfinite(transform.Time) && transform.Time >= 0.0f && transform.Time <= 3600.0f && _valid(transform.Position) && _valid(transform.Rotation))
			{
				light.MasterTransforms.push_back(transform);
			}
			else
			{
				warnings.push_back("Invalid master light transform key; it was skipped.");
			}
		}
		const u32 masterCount = ini.read_if_exists<u32>(section, "master_clips", 0);
		for (u32 j = 0; j < std::min(masterCount, kCinemaSceneMaxPlaylist); ++j)
		{
			SMasterLightClip clip;
			string64 key = {};
			xr_sprintf(key, "master_clip_%03u_start", j);
			clip.Start = ini.read_if_exists<float>(section, key, 0.0f);
			xr_sprintf(key, "master_clip_%03u_duration", j);
			clip.Duration = ini.read_if_exists<float>(section, key, 1.0f);
			xr_sprintf(key, "master_clip_%03u_track", j);
			clip.Track = std::clamp(ini.read_if_exists<s32>(section, key, static_cast<s32>((SceneLights.size() + 20) % kCinemaMaxTracks)), 0, kCinemaMaxTracks - 1);
			xr_sprintf(key, "master_clip_%03u_muted", j);
			clip.Muted = ini.read_if_exists<bool>(section, key, false);
			xr_sprintf(key, "master_clip_%03u", j);
			CinemaLoadClipAppearance(ini, section, key, clip);
			xr_sprintf(key, "master_clip_%03u_intensity_start", j);
			clip.StartIntensity = ini.read_if_exists<float>(section, key, 1.0f);
			xr_sprintf(key, "master_clip_%03u_intensity_end", j);
			clip.EndIntensity = ini.read_if_exists<float>(section, key, 1.0f);
			xr_sprintf(key, "master_clip_%03u_color_start", j);
			clip.StartColor = ini.read_if_exists<Fvector>(section, key, clip.StartColor);
			xr_sprintf(key, "master_clip_%03u_color_end", j);
			clip.EndColor = ini.read_if_exists<Fvector>(section, key, clip.EndColor);
			if (!_valid(clip.StartColor) || !_valid(clip.EndColor) || clip.StartColor.x < 0.0f || clip.StartColor.y < 0.0f || clip.StartColor.z < 0.0f || clip.EndColor.x < 0.0f || clip.EndColor.y < 0.0f || clip.EndColor.z < 0.0f || clip.StartColor.x > 10.0f || clip.StartColor.y > 10.0f || clip.StartColor.z > 10.0f || clip.EndColor.x > 10.0f || clip.EndColor.y > 10.0f || clip.EndColor.z > 10.0f)
			{
				clip.StartColor.set(1.0f, 1.0f, 1.0f);
				clip.EndColor.set(1.0f, 1.0f, 1.0f);
				warnings.push_back("Invalid master light colors; using defaults.");
			}
			if (std::isfinite(clip.Start) && clip.Start >= 0.0f && clip.Start <= 3600.0f && std::isfinite(clip.Duration) && clip.Duration >= 0.01f && clip.Duration <= 3600.0f && std::isfinite(clip.StartIntensity) && clip.StartIntensity >= 0.0f && clip.StartIntensity <= 10.0f && std::isfinite(clip.EndIntensity) && clip.EndIntensity >= 0.0f && clip.EndIntensity <= 10.0f)
			{
				light.MasterClips.push_back(clip);
			}
			else
			{
				warnings.push_back("Invalid master light clip; it was skipped.");
			}
		}
		light.Group = ini.read_if_exists<s32>(section, "group", -1);
		if (light.Group < -1 || light.Group >= static_cast<s32>(SceneLightGroups.size()))
		{
			light.Group = -1;
			warnings.push_back("Invalid light group: light left ungrouped.");
		}
		light.Position = ini.read_if_exists<Fvector>(section, "position", light.Position);
		const Fvector rotation = ini.read_if_exists<Fvector>(section, "rotation", Fvector().set(0.0f, 0.0f, 0.0f));
		light.Color = ini.read_if_exists<Fvector>(section, "color", light.Color);
		light.Intensity = ini.read_if_exists<float>(section, "intensity", light.Intensity);
		light.Range = ini.read_if_exists<float>(section, "range", light.Range);
		light.Cone = ini.read_if_exists<float>(section, "cone", light.Cone);
		light.VirtualSize = ini.read_if_exists<float>(section, "virtual_size", light.VirtualSize);
		light.VolumeQuality = ini.read_if_exists<float>(section, "volume_quality", light.VolumeQuality);
		light.VolumeIntensity = ini.read_if_exists<float>(section, "volume_intensity", light.VolumeIntensity);
		light.VolumeDistance = ini.read_if_exists<float>(section, "volume_distance", light.VolumeDistance);
		light.Type = ini.read_if_exists<s32>(section, "type", light.Type);
		light.Active = ini.read_if_exists<bool>(section, "active", light.Active);
		light.Shadow = ini.read_if_exists<bool>(section, "shadow", light.Shadow);
		light.Volumetric = ini.read_if_exists<bool>(section, "volumetric", light.Volumetric);
		light.Indirect = ini.read_if_exists<bool>(section, "indirect", light.Indirect);
		light.HudMode = ini.read_if_exists<bool>(section, "hud_mode", light.HudMode);
		light.Occlusion = ini.read_if_exists<bool>(section, "occlusion", light.Occlusion);
		light.Modulation = true;
		light.Loop = ini.read_if_exists<bool>(section, "modulation_loop", true);
		light.Duration = ini.read_if_exists<float>(section, "modulation_duration", 10.0f);
		if (!std::isfinite(light.Duration) || light.Duration < 0.01f || light.Duration > 600.0f)
		{
			light.Duration = 10.0f;
			warnings.push_back("Invalid light modulation duration: using default.");
		}
		for (u32 channel = 0; channel < kCinemaLightChannels; ++channel)
		{
			string32 key = {};
			auto& wave = light.Waves[channel];
			const float lower = channel < 6 ? -3600.0f : 0.0f;
			const float upper = channel < 6 ? 3600.0f : 10.0f;
			xr_sprintf(key, "wave_%u_type", channel);
			wave.Type = ini.read_if_exists<s32>(section, key, 0);
			xr_sprintf(key, "wave_%u_amplitude", channel);
			wave.Amplitude = ini.read_if_exists<float>(section, key, 0.0f);
			xr_sprintf(key, "wave_%u_frequency", channel);
			wave.Frequency = ini.read_if_exists<float>(section, key, 1.0f);
			xr_sprintf(key, "wave_%u_phase", channel);
			wave.Phase = ini.read_if_exists<float>(section, key, 0.0f);
			xr_sprintf(key, "wave_%u_min", channel);
			wave.Minimum = ini.read_if_exists<float>(section, key, lower);
			xr_sprintf(key, "wave_%u_max", channel);
			wave.Maximum = ini.read_if_exists<float>(section, key, upper);
			if (wave.Type < 0 || wave.Type > 10 || !std::isfinite(wave.Amplitude) || wave.Amplitude < 0.0f || wave.Amplitude > 3600.0f ||
				!std::isfinite(wave.Frequency) || wave.Frequency < 0.0f || wave.Frequency > 100.0f || !std::isfinite(wave.Phase) || fabsf(wave.Phase) > 100.0f ||
				!std::isfinite(wave.Minimum) || !std::isfinite(wave.Maximum) || wave.Minimum < lower || wave.Maximum > upper || wave.Minimum > wave.Maximum)
			{
				wave = SParticleWave();
				wave.Minimum = lower;
				wave.Maximum = upper;
				warnings.push_back("Invalid light modulation wave: using default.");
			}
			xr_sprintf(key, "curve_%u_count", channel);
			const u32 count = std::min(ini.read_if_exists<u32>(section, key, 0), 128u);
			for (u32 k = 0; k < count; ++k)
			{
				xr_sprintf(key, "curve_%u_%u_time", channel, k);
				const float time = ini.read_if_exists<float>(section, key, -1.0f);
				xr_sprintf(key, "curve_%u_%u_value", channel, k);
				const float value = ini.read_if_exists<float>(section, key, channel < 6 ? 0.0f : 1.0f);
				if (std::isfinite(time) && time >= 0.0f && time <= 600.0f && std::isfinite(value) && value >= lower && value <= upper)
				{
					SParticleCurveKey curveKey{ time, value };
					xr_sprintf(key, "curve_%u_%u_smooth", channel, k);
					curveKey.Smooth = ini.read_if_exists<bool>(section, key, false);
					xr_sprintf(key, "curve_%u_%u_in_slope", channel, k);
					curveKey.InSlope = ini.read_if_exists<float>(section, key, 0.0f);
					xr_sprintf(key, "curve_%u_%u_out_slope", channel, k);
					curveKey.OutSlope = ini.read_if_exists<float>(section, key, 0.0f);
					curveKey.InSlope = std::isfinite(curveKey.InSlope) ? std::clamp(curveKey.InSlope, -3600.0f, 3600.0f) : 0.0f;
					curveKey.OutSlope = std::isfinite(curveKey.OutSlope) ? std::clamp(curveKey.OutSlope, -3600.0f, 3600.0f) : 0.0f;
					light.Curves[channel].push_back(curveKey);
				}
			}
			std::stable_sort(light.Curves[channel].begin(), light.Curves[channel].end(), [](const auto& a, const auto& b) { return a.Time < b.Time; });
		}
		const shared_str texture = CinemaReadString(ini, section, "texture");
		if (texture.size() && texture.size() < sizeof(light.Texture))
		{
			xr_strcpy(light.Texture, texture.c_str());
		}
		const auto validVector = [](const Fvector& v) { return std::isfinite(v.x) && std::isfinite(v.y) && std::isfinite(v.z); };
		if (!validVector(light.Position) || !validVector(rotation) || !validVector(light.Color) ||
			light.Color.x < 0.0f || light.Color.y < 0.0f || light.Color.z < 0.0f ||
			!std::isfinite(light.Intensity) || light.Intensity < 0.0f || light.Intensity > 100.0f ||
			!std::isfinite(light.Range) || light.Range < 0.1f || light.Range > 1000.0f ||
			!std::isfinite(light.Cone) || light.Cone < 1.0f || light.Cone > 120.0f ||
			!std::isfinite(light.VirtualSize) || light.VirtualSize < 0.0f || light.VirtualSize > 100.0f ||
			!std::isfinite(light.VolumeQuality) || light.VolumeQuality < 0.01f || light.VolumeQuality > 1.0f ||
			!std::isfinite(light.VolumeIntensity) || light.VolumeIntensity < 0.0f || light.VolumeIntensity > 100.0f ||
			!std::isfinite(light.VolumeDistance) || light.VolumeDistance < 0.0f || light.VolumeDistance > 1.0f || light.Type < 0 || light.Type > 1)
		{
			warnings.push_back("Invalid light settings: light skipped.");
			continue;
		}
		light.Orientation.setHPB(deg2rad(rotation.x), deg2rad(rotation.y), deg2rad(rotation.z));
		SceneLights.push_back(std::move(light));
	}
	if (SceneData.empty() && SceneParticles.empty() && SceneLights.empty() && SceneLightGroups.empty() && SceneCameras.empty())
	{
		dialog = warnings;
		dialog.push_back("There is nothing to load.");
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	string256 summary = {};
	xr_sprintf(summary, "Load scene \"%s\"? Replace %u NPC and %u particles with %u NPC and %u particles?", name, static_cast<u32>(Actors.size()), static_cast<u32>(Particles.size()), static_cast<u32>(SceneData.size()), static_cast<u32>(SceneParticles.size()));

	dialog.push_back(summary);
	string128 lightSummary = {};
	xr_sprintf(lightSummary, "Replace %u lights with %u lights?", static_cast<u32>(Lights.size()), static_cast<u32>(SceneLights.size()));
	dialog.push_back(lightSummary);
	dialog.insert(dialog.end(), warnings.begin(), warnings.end());

	ShowSceneDialog(ESceneDialog::Confirm, "Load scene", dialog);
}


void CCinemaFilmMakerManager::ApplyLoadedScene()
{
	if (SceneData.empty() && SceneParticles.empty() && SceneLights.empty() && SceneLightGroups.empty() && SceneCameras.empty())
	{
		return;
	}

	ClearGizmo();
	ClearActors();
	ClearParticles();
	ClearLights();
	MasterPlaying = false;
	MasterActive = true;
	MasterIncludeStart = true;
	MasterSelectedActor = -1;
	MasterSelectedClip = -1;
	MasterSelectedLight = -1;
	MasterSelectedLightClip = -1;
	MasterSelectedParticle = -1;
	MasterSelectedParticleClip = -1;
	MasterTime = 0.0f;
	MasterDuration = LoadedMasterDuration;
	MasterPreviousContentEnd = FLT_MAX;
	MasterLoop = LoadedMasterLoop;
	MasterUseLoopRange = LoadedMasterUseLoopRange;
	MasterLoopStart = LoadedMasterLoopStart;
	MasterLoopEnd = LoadedMasterLoopEnd;
	std::copy(std::begin(LoadedTrackMuted), std::end(LoadedTrackMuted), std::begin(MasterTrackMuted));
	std::copy(std::begin(LoadedTrackParent), std::end(LoadedTrackParent), std::begin(MasterTrackParent));
	std::copy(std::begin(LoadedTrackGroup), std::end(LoadedTrackGroup), std::begin(MasterTrackGroup));
	std::copy(std::begin(LoadedTrackCollapsed), std::end(LoadedTrackCollapsed), std::begin(MasterTrackCollapsed));
	for (s32 track = 0; track < kCinemaMaxTracks; ++track)
	{
		const s32 parent = static_cast<s32>(MasterTrackParent[track]) - 1;
		if (parent >= 0 && (MasterTrackParent[parent] != 0 || MasterTrackGroup[track])) MasterTrackParent[track] = 0;
	}
	for (s32 track = 0; track < kCinemaMaxTracks; ++track) if (MasterTrackParent[track]) MasterTrackGroup[MasterTrackParent[track] - 1] = true;
	MasterSoloTrack = LoadedSoloTrack;
	MasterSoloChanged = LoadedSoloChanged;
	std::copy(std::begin(LoadedSoloPreviousMuted), std::end(LoadedSoloPreviousMuted), std::begin(MasterSoloPreviousMuted));
	RefreshMasterTrackMute();
	if (CurveEditor.Type == 4 || CurveEditor.Type == 5) CurveEditor.Open = false;
	Cameras = std::move(SceneCameras);
	SelectedCameraKey = -1;
	SelectedCameraKeyCamera = -1;
	LightGroups = std::move(SceneLightGroups);
	xr_vector<xr_string> issues;
	for (auto& light : SceneLights)
	{
		light.Light = ::Render->light_create();
		if (light.Light)
		{
			ApplyCinemaLight(light, true);
			Lights.push_back(std::move(light));
		}
		else
		{
			issues.push_back("A light could not be created by the renderer.");
		}
	}
	SceneLights.clear();
	ParticleGroups = SceneParticleGroups;
	SceneParticleGroups.clear();

	for (const auto& particle : SceneParticles)
	{
		if (!AddParticle(particle.Name, particle.Position))
		{
			issues.push_back(xr_string("Particle not found: ") + particle.Name.c_str());
		}
		else
		{
			auto& loaded = Particles.back();
			loaded.Group = particle.Group;
			loaded.MasterTrack = particle.MasterTrack;
			loaded.PatternOwned = particle.PatternOwned;
			loaded.MasterClips = particle.MasterClips;
			loaded.MasterTransforms = particle.MasterTransforms;
			loaded.Orientation = particle.Orientation;
			loaded.Scale = particle.Scale;
			UpdateParticleTransform(loaded);
			loaded.Timeline = particle.Timeline;
			loaded.Loop = particle.Loop;
			loaded.Speed = particle.Speed;
			loaded.Modulation = particle.Modulation;
			loaded.EndOffset = particle.EndOffset;
			loaded.EndRotation = particle.EndRotation;
			loaded.EndScale = particle.EndScale;
			for (u32 channel = 0; channel < 8; ++channel)
			{
				loaded.Curves[channel] = particle.Curves[channel];
				loaded.Waves[channel] = particle.Waves[channel];
			}
			loaded.In = particle.In;
			loaded.Out = particle.Out;
			if (loaded.Timeline)
			{
				StartParticleTimeline(loaded);
			}
		}
	}
	SceneParticles.clear();
	u32 droppedAnimations = 0;

	for (const SSceneActorData& data : SceneData)
	{
		auto actor = xr_make_unique<SCinemaActor>();
		actor->Name = data.Name;
		actor->MasterTrack = data.MasterTrack;
		actor->PatternOwned = data.PatternOwned;
		actor->MasterClips = data.MasterClips;
		actor->MasterTransforms = data.MasterTransforms;
		actor->Position = data.Position;
		actor->Orientation.setHPB(deg2rad(data.Rotation.x), deg2rad(data.Rotation.y), deg2rad(data.Rotation.z));
		actor->Scale = data.Scale;
		actor->Loop = data.Loop;
		actor->Speed = data.Speed;
		actor->AnimationPacks = data.AnimationPacks;
		const auto rememberPack = [&](const shared_str& path)
		{
			if (CinemaIsOmfPath(path) && std::find(actor->AnimationPacks.begin(), actor->AnimationPacks.end(), path) == actor->AnimationPacks.end()) actor->AnimationPacks.push_back(path);
		};
		for (const SPlaylistEntry& entry : data.Playlist) rememberPack(entry.Source);
		for (const SMasterClip& entry : data.MasterClips) rememberPack(entry.Source);
		rememberPack(data.PoseSource);

		if (!SetActorVisual(*actor, data.Visual.c_str()))
		{
			issues.push_back(xr_string("Skipped \"") + data.Name + "\": the model could not be loaded.");
			continue;
		}

		for (const SMasterClip& entry : actor->MasterClips)
		{
			if (!entry.Source.size() && !entry.Name.size()) continue;
			const SAnimationSource* source = FindSource(*actor, entry.Source);
			if (source == nullptr || FindClip(*source, entry.Name, false) == nullptr)
			{
				issues.push_back(xr_string("Master timeline animation unavailable for \"") + data.Name + "\": " + entry.Name.c_str());
			}
		}

		for (const SPlaylistEntry& entry : data.Playlist)
		{
			const SAnimationSource* source = FindSource(*actor, entry.Source);

			if (source == nullptr || FindClip(*source, entry.Name, entry.Fx) == nullptr)
			{
				++droppedAnimations;
			}

			actor->Playlist.push_back(entry);
		}

		actor->PlaylistSelected = actor->Playlist.empty() ? -1 : 0;
		actor->Trims = data.Trims;
		if (actor->PatternOwned && LoadedSceneVersion < 4 && !actor->MasterClips.empty())
		{
			const SMasterClip& old = actor->MasterClips.front();
			const SAnimationSource* source = FindSource(*actor, old.Source);
			const SAnimationClip* animation = source ? FindClip(*source, old.Name, false) : nullptr;
			if (animation != nullptr && EnsureSource(*actor, old.Source))
			{
				const bool migrateAnimation = actor->Playlist.empty();
				if (migrateAnimation)
				{
					actor->Playlist.push_back({ old.Source, old.Name, false, false });
					actor->PlaylistSelected = 0;
				}
				if (migrateAnimation || !old.Shots.empty())
				{
					STrimRange* trim = FindTrimEdit(*actor, old.Source, old.Name);
					if (trim == nullptr)
					{
						actor->Trims.emplace_back();
						trim = &actor->Trims.back();
						trim->Source = old.Source;
						trim->Name = old.Name;
						trim->Speed = migrateAnimation ? old.Speed : actor->Speed;
					}
					if (migrateAnimation && trim->Segments.empty() && (old.SourceIn > 0.0f || old.SourceOut > 0.0f))
					{
						const MotionID motion = CinemaFindMotion(*actor, *source, *animation);
						const float length = motion.valid() ? actor->Animated->get_animation_length(motion) : 0.0f;
						const float last = old.SourceOut > 0.0f ? std::min(old.SourceOut, length) : length;
						if (last > old.SourceIn) trim->Segments.push_back({ old.SourceIn, last });
					}
					for (const float shot : old.Shots)
					{
						if (trim->Shots.size() < kCinemaMaxShots && std::find(trim->Shots.begin(), trim->Shots.end(), shot) == trim->Shots.end()) trim->Shots.push_back(shot);
					}
				}
			}
		}
		actor->PingPong = data.PingPong;

		if (data.Item.size() > 0)
		{
			SetActorItem(*actor, data.Item);

			if (actor->ItemModel == nullptr)
			{
				issues.push_back(xr_string("Item \"") + data.Item.c_str() + "\" was not applied to \"" + data.Name + "\".");
			}
		}

		RestoreActorPose(*actor, data);

		Actors.push_back(std::move(actor));
	}

	SceneData.clear();
	ResetMasterTimeline();

	Selected = Actors.empty() ? -1 : 0;
	if (Selected >= 0)
	{
		SelectedParticle = -1;
	}
	ResetSelection();

	if (droppedAnimations > 0)
	{
		string256 line = {};
		xr_sprintf(line, "%u playlist entries are unavailable; their references were kept.", droppedAnimations);
		issues.push_back(line);
	}

	string256 summary = {};
	xr_sprintf(summary, "Loaded %u NPC and %u particles.", static_cast<u32>(Actors.size()), static_cast<u32>(Particles.size()));
	issues.insert(issues.begin(), summary);

	ShowSceneDialog(ESceneDialog::Info, "Load scene", issues);
}

s32 CCinemaFilmMakerManager::FindPreset(const char* name) const
{
	if (name == nullptr || name[0] == '\0')
	{
		return -1;
	}

	for (s32 index = 0; index < static_cast<s32>(Presets.size()); ++index)
	{
		if (xr_strcmp(Presets[index].Name.c_str(), name) == 0)
		{
			return index;
		}
	}

	return -1;
}

void CCinemaFilmMakerManager::LoadPresets()
{
	if (PresetsLoaded)
	{
		return;
	}

	PresetsLoaded = true;
	Presets.clear();

	string_path path = {};
	CinemaPresetsPath(path);

	std::error_code error;

	if (!std::filesystem::exists(path, error))
	{
		return;
	}

	CInifile ini(path, true, true, false);

	for (CInifile::Sect& section : ini.sections())
	{
		if (section.Name.size() == 0 || _strnicmp(section.Name.c_str(), "preset_", 7) != 0)
		{
			continue;
		}

		if (Presets.size() >= kCinemaMaxPresets)
		{
			break;
		}

		const char* sect = section.Name.c_str();
		const shared_str presetName = CinemaReadString(ini, sect, "name");
		const shared_str visual = CinemaReadString(ini, sect, "visual");

		if (presetName.size() == 0 || visual.size() == 0)
		{
			continue;
		}

		SNpcPreset preset;
		preset.Name = presetName.c_str();
		preset.Visual = visual;
		preset.Item = CinemaReadString(ini, sect, "item");

		if (ini.line_exist(sect, "scale"))
		{
			preset.Scale = ini.r_fvector3(sect, "scale");
		}

		if (!_valid(preset.Scale))
		{
			preset.Scale.set(1.0f, 1.0f, 1.0f);
		}

		preset.Scale.x = std::clamp(preset.Scale.x, 0.1f, 5.0f);
		preset.Scale.y = std::clamp(preset.Scale.y, 0.1f, 5.0f);
		preset.Scale.z = std::clamp(preset.Scale.z, 0.1f, 5.0f);
		preset.Loop = ini.line_exist(sect, "loop") && ini.r_bool(sect, "loop");
		preset.Speed = ini.line_exist(sect, "speed") ? ini.r_float(sect, "speed") : 1.0f;
		preset.Speed = std::isfinite(preset.Speed) ? std::clamp(preset.Speed, -4.0f, 4.0f) : 1.0f;

		const u32 playlistCount = ini.line_exist(sect, "playlist") ? ini.r_u32(sect, "playlist") : 0;
		const u32 playlistLimit = std::min(playlistCount, kCinemaSceneMaxPlaylist);

		for (u32 index = 0; index < playlistLimit; ++index)
		{
			string32 line = {};
			xr_sprintf(line, "playlist_%03u", index);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			SPlaylistEntry entry;
			entry.Source = text.substr(0, first).c_str();
			entry.Name = text.substr(first + 1, second - first - 1).c_str();
			entry.Fx = text.compare(second + 1, xr_string::npos, "1") == 0;
			xr_sprintf(line, "playlist_%03u_muted", index);
			entry.Muted = ini.line_exist(sect, line) && ini.r_bool(sect, line);
			preset.Playlist.push_back(entry);
		}

		preset.PingPong = ini.line_exist(sect, "pingpong") && ini.r_bool(sect, "pingpong");

		const u32 trimCount = ini.line_exist(sect, "trims") ? ini.r_u32(sect, "trims") : 0;
		const u32 trimLimit = std::min(trimCount, kCinemaSceneMaxPlaylist);

		for (u32 trimIndex = 0; trimIndex < trimLimit; ++trimIndex)
		{
			string32 line = {};
			xr_sprintf(line, "trim_%03u", trimIndex);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			const size_t third = text.find('|', second + 1);

			if (third == xr_string::npos)
			{
				continue;
			}

			STrimRange trim;

			if (!CinemaParseTrim(value, trim))
			{
				continue;
			}

			CinemaLoadShots(ini, sect, line, trim.Shots);
			preset.Trims.push_back(trim);
		}

		Presets.push_back(std::move(preset));
	}

	if (PresetSelected >= static_cast<s32>(Presets.size()))
	{
		PresetSelected = static_cast<s32>(Presets.size()) - 1;
	}
}

bool CCinemaFilmMakerManager::SavePresets()
{
	string_path path = {};
	CinemaPresetsPath(path);

	CInifile ini(nullptr, false, false, false);

	ini.w_s32("preset", "version", kCinemaPresetVersion);

	for (u32 index = 0; index < static_cast<u32>(Presets.size()); ++index)
	{
		const SNpcPreset& preset = Presets[index];

		string32 section = {};
		xr_sprintf(section, "preset_%03u", index);

		ini.w_string(section, "name", preset.Name.c_str());
		ini.w_string(section, "visual", preset.Visual.c_str());

		if (preset.Item.size() > 0)
		{
			ini.w_string(section, "item", preset.Item.c_str());
		}

		ini.w_fvector3(section, "scale", preset.Scale);
		ini.w_bool(section, "loop", preset.Loop);
		ini.w_float(section, "speed", preset.Speed);
		ini.w_u32(section, "playlist", static_cast<u32>(preset.Playlist.size()));

		for (u32 entryIndex = 0; entryIndex < static_cast<u32>(preset.Playlist.size()); ++entryIndex)
		{
			const SPlaylistEntry& entry = preset.Playlist[entryIndex];

			string32 line = {};
			xr_sprintf(line, "playlist_%03u", entryIndex);

			string1024 value = {};
			xr_sprintf(value, "%s|%s|%u", entry.Source.c_str(), entry.Name.c_str(), entry.Fx ? 1u : 0u);
			ini.w_string(section, line, value);
			xr_sprintf(line, "playlist_%03u_muted", entryIndex);
			ini.w_bool(section, line, entry.Muted);
		}

		ini.w_bool(section, "pingpong", preset.PingPong);
		ini.w_u32(section, "trims", static_cast<u32>(preset.Trims.size()));

		for (u32 trimIndex = 0; trimIndex < static_cast<u32>(preset.Trims.size()); ++trimIndex)
		{
			const STrimRange& trim = preset.Trims[trimIndex];

			if (trim.Source.size() == 0 || trim.Name.size() == 0)
			{
				continue;
			}

			string32 line = {};
			xr_sprintf(line, "trim_%03u", trimIndex);

			xr_string value = "v2|";
			value += trim.Source.c_str();
			value += "|";
			value += trim.Name.c_str();

			string64 speedText = {};
			xr_sprintf(speedText, "|%.4f|%.4f|", trim.Speed, trim.Blend);
			value += speedText;

			for (size_t segmentIndex = 0; segmentIndex < trim.Segments.size(); ++segmentIndex)
			{
				string64 segmentText = {};
				xr_sprintf(segmentText, "%.4f,%.4f", trim.Segments[segmentIndex].Start, trim.Segments[segmentIndex].End);
				value += segmentText;

				if (segmentIndex + 1 < trim.Segments.size())
				{
					value += "|";
				}
			}

			ini.w_string(section, line, value.c_str());
			CinemaSaveShots(ini, section, line, trim.Shots);
		}
	}

	if (!CinemaSaveIni(ini, path))
	{
		ShowSceneDialog(ESceneDialog::Info, "Presets", { "Could not write the presets file:", path });
		return false;
	}

	return true;
}

void CCinemaFilmMakerManager::RequestSavePreset()
{
	xr_vector<xr_string> lines;

	const SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		lines.push_back("Select an NPC first: a preset is taken from the configured NPC.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	LoadPresets();

	xr_string defaultName = CinemaFileName(actor->Visual.c_str());
	const size_t dot = defaultName.rfind('.');

	if (dot != xr_string::npos)
	{
		defaultName.erase(dot);
	}

	if (defaultName.empty())
	{
		defaultName = "preset";
	}

	xr_sprintf(PresetNameEdit, "%s", defaultName.c_str());

	ShowSceneDialog(ESceneDialog::Preset, "Save preset", lines);
}

void CCinemaFilmMakerManager::SavePresetFromActor(const char* name)
{
	PresetSelection.clear();
	PresetAnchor = -1;
	xr_vector<xr_string> lines;

	const SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		lines.push_back("Select an NPC first: a preset is taken from the configured NPC.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	xr_string cleanName;
	CinemaSceneCleanName(name, cleanName);

	if (cleanName.empty())
	{
		lines.push_back("Enter a preset name.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	const s32 existing = FindPreset(cleanName.c_str());

	if (existing < 0 && Presets.size() >= kCinemaMaxPresets)
	{
		lines.push_back("There are too many presets: delete one first.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	SNpcPreset preset;
	preset.Name = cleanName;
	preset.Visual = actor->Visual;
	preset.Item = actor->ItemSection;
	preset.Scale = actor->Scale;
	preset.Loop = actor->Loop;
	preset.Speed = actor->Speed;
	preset.Playlist = actor->Playlist;
	preset.Trims = actor->Trims;
	preset.PingPong = actor->PingPong;

	if (existing >= 0)
	{
		Presets[existing] = preset;
		PresetSelected = existing;
	}
	else
	{
		Presets.push_back(preset);
		PresetSelected = static_cast<s32>(Presets.size()) - 1;
	}

	if (!SavePresets())
	{
		return;
	}

	string256 summary = {};
	xr_sprintf(summary, "Preset \"%s\" saved (%u animations).", cleanName.c_str(), static_cast<u32>(preset.Playlist.size()));
	lines.push_back(summary);
	ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
}

void CCinemaFilmMakerManager::SpawnFromPreset(s32 index)
{
	if (index < 0 || index >= static_cast<s32>(Presets.size()))
	{
		return;
	}

	const SNpcPreset& preset = Presets[index];
	xr_vector<xr_string> lines;

	if (!AddActorInFront(preset.Visual.c_str()))
	{
		lines.push_back(xr_string("Could not create an NPC from preset \"") + preset.Name + "\": model is missing.");
		ShowSceneDialog(ESceneDialog::Info, "Presets", lines);
		return;
	}

	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		return;
	}

	actor->Scale = preset.Scale;
	actor->Loop = preset.Loop;
	actor->Speed = preset.Speed;
	actor->Trims = preset.Trims;
	actor->PingPong = preset.PingPong;

	if (preset.Item.size() > 0)
	{
		SetActorItem(*actor, preset.Item);
	}

	for (const SPlaylistEntry& entry : preset.Playlist)
	{
		if (CinemaIsOmfPath(entry.Source) && std::find(actor->AnimationPacks.begin(), actor->AnimationPacks.end(), entry.Source) == actor->AnimationPacks.end()) actor->AnimationPacks.push_back(entry.Source);
	}
	BuildActorSources(*actor);
	for (const SPlaylistEntry& entry : preset.Playlist)
	{
		const SAnimationSource* source = FindSource(*actor, entry.Source);

		if (source == nullptr || FindClip(*source, entry.Name, entry.Fx) == nullptr)
		{
			continue;
		}

		actor->Playlist.push_back(entry);
	}

	actor->PlaylistSelected = actor->Playlist.empty() ? -1 : 0;
}

void CCinemaFilmMakerManager::ApplyPresetToActor(s32 index)
{
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr || index < 0 || index >= static_cast<s32>(Presets.size()))
	{
		return;
	}

	const SNpcPreset& preset = Presets[index];

	StopActor(*actor);

	if (!SetActorVisual(*actor, preset.Visual.c_str()))
	{
		xr_vector<xr_string> lines;
		lines.push_back(xr_string("Could not apply preset \"") + preset.Name + "\": model is missing.");
		ShowSceneDialog(ESceneDialog::Info, "Presets", lines);
		return;
	}

	actor->Scale = preset.Scale;
	actor->Loop = preset.Loop;
	actor->Speed = preset.Speed;
	actor->Trims = preset.Trims;
	actor->PingPong = preset.PingPong;
	actor->Playlist.clear();
	actor->PlaylistSelection.clear();
	actor->PlaylistAnchor = -1;

	for (const SPlaylistEntry& entry : preset.Playlist)
	{
		if (CinemaIsOmfPath(entry.Source) && std::find(actor->AnimationPacks.begin(), actor->AnimationPacks.end(), entry.Source) == actor->AnimationPacks.end()) actor->AnimationPacks.push_back(entry.Source);
	}
	BuildActorSources(*actor);
	for (const SPlaylistEntry& entry : preset.Playlist)
	{
		const SAnimationSource* source = FindSource(*actor, entry.Source);

		if (source == nullptr || FindClip(*source, entry.Name, entry.Fx) == nullptr)
		{
			continue;
		}

		actor->Playlist.push_back(entry);
	}

	actor->PlaylistSelected = actor->Playlist.empty() ? -1 : 0;

	ReleaseActorItem(*actor);

	if (preset.Item.size() > 0)
	{
		SetActorItem(*actor, preset.Item);
	}
}

void CCinemaFilmMakerManager::DeletePreset(s32 index)
{
	if (index < 0 || index >= static_cast<s32>(Presets.size()))
	{
		return;
	}

	Presets.erase(Presets.begin() + index);

	if (PresetSelected >= static_cast<s32>(Presets.size()))
	{
		PresetSelected = static_cast<s32>(Presets.size()) - 1;
	}

	SavePresets();
}

void CCinemaFilmMakerManager::DrawPresetTab()
{
	LoadPresets();

	SCinemaActor* actor = GetSelectedActor();

	ImGui::TextDisabled("A preset stores the model, held item, loop and speed, and the animation playlist.");

	ImGui::BeginDisabled(actor == nullptr);

	if (ImGui::Button("Save selected NPC as preset"))
	{
		RequestSavePreset();
	}

	ImGui::EndDisabled();

	ImGui::SeparatorText("Saved presets");

	if (Presets.empty())
	{
		ImGui::TextDisabled("No presets yet. Select an actor and save it as a preset.");
		return;
	}

	CinemaHint("Shift + click: select range | Ctrl + click: toggle | Double-click: spawn");
	const float listHeight = std::clamp(ImGui::GetContentRegionAvail().y - 90.0f, 120.0f, 420.0f);

	ImGui::BeginChild("##cinema_preset_list", ImVec2(0.0f, listHeight), true);

	s32 spawnRequest = -1;

	for (s32 index = 0; index < static_cast<s32>(Presets.size()); ++index)
	{
		const SNpcPreset& preset = Presets[index];

		ImGui::PushID(index);

		string256 label = {};
		xr_sprintf(label, "%s   (%u animations)", preset.Name.c_str(), static_cast<u32>(preset.Playlist.size()));

		const bool selected = PresetSelection.empty() ? PresetSelected == index : std::find(PresetSelection.begin(), PresetSelection.end(), index) != PresetSelection.end();
		if (ImGui::Selectable(label, selected))
		{
			if (ImGui::GetIO().KeyShift && PresetAnchor >= 0 && PresetAnchor < static_cast<s32>(Presets.size()))
			{
				PresetSelection.clear();
				for (s32 i = std::min(PresetAnchor, index); i <= std::max(PresetAnchor, index); ++i)
				{
					PresetSelection.push_back(i);
				}
			}
			else if (ImGui::GetIO().KeyCtrl)
			{
				if (PresetSelection.empty() && PresetSelected >= 0)
				{
					PresetSelection.push_back(PresetSelected);
				}
				const auto it = std::find(PresetSelection.begin(), PresetSelection.end(), index);
				if (it == PresetSelection.end())
				{
					PresetSelection.push_back(index);
				}
				else
				{
					PresetSelection.erase(it);
				}
				PresetAnchor = index;
			}
			else
			{
				PresetSelection = { index };
				PresetAnchor = index;
			}
			PresetSelected = std::find(PresetSelection.begin(), PresetSelection.end(), index) != PresetSelection.end() ? index : (PresetSelection.empty() ? -1 : PresetSelection.back());
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s\nitem: %s", preset.Visual.c_str(), preset.Item.size() > 0 ? preset.Item.c_str() : "<none>");

			if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
			{
				PresetSelected = index;
				spawnRequest = index;
			}
		}

		ImGui::PopID();
	}

	ImGui::EndChild();

	if (spawnRequest >= 0)
	{
		SpawnFromPreset(spawnRequest);
	}

	SToolbarLayout actions(ImGui::GetContentRegionAvail().x);

	ImGui::BeginDisabled(PresetSelected < 0);

	actions.Item(CinemaButtonWidth("Spawn") + 10.0f);

	if (ImGui::Button("Spawn##preset"))
	{
		const auto indices = PresetSelection.empty() ? xr_vector<s32>{ PresetSelected } : PresetSelection;
		for (const s32 index : indices)
		{
			SpawnFromPreset(index);
		}
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Create a new NPC in front of you from this preset");
	}

	ImGui::BeginDisabled(actor == nullptr || PresetSelection.size() > 1);

	actions.Item(CinemaButtonWidth("Apply to selected") + 10.0f);

	if (ImGui::Button("Apply to selected##preset"))
	{
		ApplyPresetToActor(PresetSelected);
	}

	ImGui::EndDisabled();

	actions.Item(CinemaButtonWidth("Delete") + 10.0f);

	if (ImGui::Button("Delete##preset"))
	{
		auto indices = PresetSelection.empty() ? xr_vector<s32>{ PresetSelected } : PresetSelection;
		std::sort(indices.begin(), indices.end(), std::greater<s32>());
		for (const s32 index : indices)
		{
			DeletePreset(index);
		}
		PresetSelection.clear();
		PresetAnchor = -1;
	}

	ImGui::EndDisabled();
}

void CCinemaFilmMakerManager::DrawToolbar()
{
	SToolbarLayout layout(ImGui::GetContentRegionAvail().x);

	layout.Item(CinemaButtonWidth("Translate") + 20.0f);
	if (ImGui::RadioButton("Translate##gizmo_mode", GizmoMode == EGizmoMode::Translate))
	{
		GizmoMode = EGizmoMode::Translate;
	}

	layout.Item(CinemaButtonWidth("Rotate") + 20.0f);
	if (ImGui::RadioButton("Rotate##gizmo_mode", GizmoMode == EGizmoMode::Rotate))
	{
		GizmoMode = EGizmoMode::Rotate;
	}

	layout.Item(CinemaButtonWidth("Snap") + 20.0f);
	ImGui::Checkbox("Snap##gizmo", &Snap);

	if (CinemaResetRequested() && Snap)
	{
		Snap = false;
	}

	if (Snap)
	{
		layout.Item(110.0f);
		ImGui::SetNextItemWidth(100.0f);
		ImGui::DragFloat("Move##snap_step", &TranslateStep, 0.01f, 0.01f, 10.0f, "%.2f");

		if (CinemaResetRequested())
		{
			TranslateStep = 0.1f;
		}

		layout.Item(140.0f);
		ImGui::SetNextItemWidth(100.0f);
		ImGui::DragFloat("Rotate step##snap_step", &RotateStepDeg, 0.5f, 0.5f, 180.0f, "%.1f");

		if (CinemaResetRequested())
		{
			RotateStepDeg = 5.0f;
		}
	}
	ImGui::SameLine();
	const float moreWidth = CinemaButtonWidth("More");
	if (ImGui::GetContentRegionAvail().x < moreWidth)
	{
		ImGui::NewLine();
	}
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + std::max(0.0f, ImGui::GetContentRegionAvail().x - moreWidth));
	if (ImGui::Button("More##toolbar"))
	{
		ImGui::OpenPopup("##cinema_more_popup");
	}

	if (ImGui::BeginPopup("##cinema_more_popup"))
	{
		if (ImGui::MenuItem("Reload library"))
		{
			BuildLibrary();
			LightTextureFilesBuilt = false;
		}
		if (ImGui::MenuItem("Presets"))
		{
			PresetsOpen = true;
		}
		if (ImGui::MenuItem("Camera spawn", nullptr, &SpawnFromCamera))
		{
			SaveSettings();
		}

		ImGui::Separator();

		if (ImGui::MenuItem("Save scene"))
		{
			RequestSaveScene();
		}

		if (ImGui::MenuItem("Load scene"))
		{
			RequestLoadScene();
		}

		ImGui::Separator();

		if (ImGui::BeginMenu("Theme"))
		{
			for (s32 index = 0; index < kCinemaThemeCount; ++index)
			{
				if (ImGui::MenuItem(kCinemaThemes[index].Name, nullptr, ThemeIndex == index))
				{
					ThemeIndex = index;
					SaveSettings();
				}
			}

			ImGui::EndMenu();
		}

		ImGui::Separator();

		if (ImGui::MenuItem("Reset editor settings"))
		{
			SpawnFromCamera = false;
			ThemeIndex = 0;
			TimelinesOpenByDefault = true;
			TimelinesOpenDirty = true;
			HideHelpersDuringPlayback = true;
			Snap = false;
			TranslateStep = 0.1f;
			RotateStepDeg = 5.0f;
			SaveSettings();
		}

		ImGui::Separator();

		if (Level != nullptr)
		{
			string_path sceneFolder = {};
			CinemaSceneFolder(sceneFolder, Level->name());
			ImGui::TextDisabled("%s", sceneFolder);
		}

		ImGui::EndPopup();
	}

}

void CCinemaFilmMakerManager::DrawSelectedActor()
{
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		ImGui::TextDisabled("No actor selected");
		ImGui::TextDisabled("Choose a model in the Model section and press Add.");
		return;
	}

	ImGui::SeparatorText("Actor");
	if (NameEditActor != Selected)
	{
		xr_sprintf(NameEdit, "%s", actor->Name.c_str());
		NameEditActor = Selected;
	}

	ImGui::SetNextItemWidth(-1.0f);

	if (ImGui::InputTextWithHint("##cinema_name", "Actor name", NameEdit, sizeof(NameEdit)))
	{
		actor->Name = NameEdit;
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Actor name");
	}

	ImGui::TextDisabled("Model");
	ImGui::SameLine();
	ImGui::TextUnformatted(CinemaFileName(actor->Visual.c_str()));

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("%s", actor->Visual.c_str());
	}

	ImGui::SeparatorText("Playback");
	const float available = ImGui::GetContentRegionAvail().x;
	SToolbarLayout controls(available);

	ImGui::BeginDisabled(actor->PatternOwned);
	controls.Item(CinemaButtonWidth("Play"));
	if (ImGui::Button("Play##actor"))
	{
		StartActorPlayback(*actor);
	}

	controls.Item(CinemaButtonWidth(actor->Paused ? "Resume" : "Pause"));
	if (ImGui::Button(actor->Paused ? "Resume##actor" : "Pause##actor"))
	{
		TogglePause(*actor);
	}

	controls.Item(CinemaButtonWidth("Stop"));
	if (ImGui::Button("Stop##actor"))
	{
		StopActor(*actor);
	}

	ImGui::EndDisabled();

	bool loop = actor->Loop;

	controls.Item(CinemaButtonWidth("Loop") + 20.0f);

	const auto applyLoop = [actor](bool value)
	{
		actor->Loop = value;

		if (actor->Animated != nullptr && actor->Motion.valid() && !actor->MotionFx && actor->PlaylistIndex < 0)
		{
			CinemaSetBlendLoop(actor->Animated, actor->Motion, actor->Loop);
		}
	};

	if (ImGui::Checkbox("Loop##actor", &loop))
	{
		applyLoop(loop);
	}

	if (CinemaResetRequested() && actor->Loop)
	{
		applyLoop(false);
	}

	bool pingPong = actor->PingPong;

	controls.Item(CinemaButtonWidth("Ping-pong") + 22.0f);

	if (ImGui::Checkbox("Ping-pong##actor", &pingPong))
	{
		actor->PingPong = pingPong;
		actor->PlayDirection = actor->Speed >= 0.0f ? 1.0f : -1.0f;
	}

	if (CinemaResetRequested() && actor->PingPong)
	{
		actor->PingPong = false;
		actor->PlayDirection = actor->Speed >= 0.0f ? 1.0f : -1.0f;
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Play the clip (or its trim range) back and forth");
	}

	float speed = actor->Speed;

	controls.Item(170.0f);
	ImGui::SetNextItemWidth(110.0f);

	if (ImGui::DragFloat("Speed##actor", &speed, 0.01f, -4.0f, 4.0f, "%.2f"))
	{
		actor->Speed = speed;
		ApplySpeed(*actor);
	}

	if (CinemaResetRequested() && actor->Speed != 1.0f)
	{
		actor->Speed = 1.0f;
		ApplySpeed(*actor);
	}

	if (actor->PlaylistIndex >= 0)
	{
		ImGui::TextDisabled("Playlist: %d / %u", actor->PlaylistIndex + 1, static_cast<u32>(actor->Playlist.size()));
	}
	else if (actor->Motion.valid())
	{
		ImGui::TextDisabled("Clip: %s%s", actor->MotionName.c_str(), actor->MotionFx ? " [fx]" : "");
	}
	else
	{
		ImGui::TextDisabled("Not playing");
	}

	if (ImGui::TreeNodeEx("Timeline", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_DrawLinesToNodes | ImGuiTreeNodeFlags_SpanAvailWidth))
	{
		DrawClipTimelines(*actor);
		ImGui::TreePop();
	}

	if (ImGui::TreeNodeEx("Transform", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_DrawLinesToNodes | ImGuiTreeNodeFlags_SpanAvailWidth))
	{
		Fvector position = actor->Position;

		if (ImGui::DragFloat3("Position", &position.x, 0.01f, -100000.0f, 100000.0f, "%.2f"))
		{
			actor->Position = position;
		}

		if (CinemaResetRequested())
		{
			actor->Position.set(0.0f, 0.0f, 0.0f);
		}

		Fvector hpb = {};
		actor->Orientation.getHPB(hpb);
		Fvector degrees = hpb;
		degrees.mul(rad2deg(1.0f));

		if (ImGui::DragFloat3("Rotation", &degrees.x, 0.5f, -3600.0f, 3600.0f, "%.1f"))
		{
			actor->Orientation.setHPB(deg2rad(degrees.x), deg2rad(degrees.y), deg2rad(degrees.z));
		}

		if (CinemaResetRequested())
		{
			actor->Orientation.setHPB(0.0f, 0.0f, 0.0f);
		}

		ImGui::TextDisabled("H / P / B, degrees");

		if (ImGui::Button("Reset tilt##actor"))
		{
			const Fvector forward = actor->Orientation.k;
			const float heading = -atan2f(forward.x, forward.z);
			actor->Orientation.setHPB(heading, 0.0f, 0.0f);
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Straighten the NPC: keep the heading and drop the slope tilt"); 
		}

		Fvector scale = actor->Scale;

		if (ImGui::DragFloat3("Scale", &scale.x, 0.01f, 0.1f, 5.0f, "%.2f"))
		{
			actor->Scale = scale;
		}

		if (CinemaResetRequested())
		{
			actor->Scale.set(1.0f, 1.0f, 1.0f);
		}

		ImGui::SameLine();

		if (ImGui::SmallButton("Reset##scale"))
		{
			actor->Scale.set(1.0f, 1.0f, 1.0f);
		}

		ImGui::TextDisabled("XYZ scale of the model");
		ImGui::TreePop();
	}
}

void CCinemaFilmMakerManager::DrawClipTimelines(SCinemaActor& actor)
{
	if (actor.Animated == nullptr)
	{
		ImGui::TextDisabled("The selected model has no animations");
		return;
	}

	if (ImGui::Checkbox("Open timelines by default##cinema", &TimelinesOpenByDefault))
	{
		TimelinesOpenDirty = true;
		SaveSettings();
	}

	if (CinemaResetRequested() && !TimelinesOpenByDefault)
	{
		TimelinesOpenByDefault = true;
		TimelinesOpenDirty = true;
		SaveSettings();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Whether every animation timeline starts expanded");
	}

	struct SClipRef
	{
		const SAnimationSource* Source = nullptr;
		const SAnimationClip* Clip = nullptr;
	};

	xr_vector<SClipRef> clips;
	xr_set<shared_str> seen;

	const auto addClip = [&clips, &seen](const SAnimationSource& source, const SAnimationClip& clip)
	{
		string512 key = {};
		xr_sprintf(key, "%s|%s|%u", source.Id.c_str(), clip.Name.c_str(), clip.Fx ? 1u : 0u);
		shared_str id = key;

		if (seen.contains(id))
		{
			return;
		}

		seen.insert(id);
		clips.push_back({ &source, &clip });
	};

	for (const SPlaylistEntry& entry : actor.Playlist)
	{
		const SAnimationSource* source = FindSource(actor, entry.Source);

		if (source == nullptr)
		{
			continue;
		}

		const SAnimationClip* clip = FindClip(*source, entry.Name, entry.Fx);

		if (clip != nullptr && !clip->Fx)
		{
			addClip(*source, *clip);
		}
	}

	if (actor.Motion.valid() && !actor.MotionFx)
	{
		if (const SAnimationSource* source = FindSource(actor, actor.MotionSource))
		{
			if (const SAnimationClip* clip = FindClip(*source, actor.MotionName, false))
			{
				addClip(*source, *clip);
			}
		}
	}

	if (clips.empty())
	{
		ImGui::TextDisabled("Add animations to the playlist to get a timeline for each animation");
		return;
	}

	ImGuiTreeNodeFlags headerFlags = ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DrawLinesToNodes | ImGuiTreeNodeFlags_SpanAvailWidth;

	if (TimelinesOpenByDefault)
	{
		headerFlags |= ImGuiTreeNodeFlags_DefaultOpen;
	}

	for (s32 index = 0; index < static_cast<s32>(clips.size()); ++index)
	{
		const SClipRef& reference = clips[index];

		ImGui::PushID(index);

		if (TimelinesOpenDirty)
		{
			ImGui::SetNextItemOpen(TimelinesOpenByDefault, ImGuiCond_Always);
		}

		string256 label = {};
		xr_sprintf(label, "%s — %s", reference.Clip->Name.c_str(), CinemaFileName(reference.Source->Id.c_str()));

		if (ImGui::TreeNodeEx(label, headerFlags))
		{
			DrawClipTimeline(actor, *reference.Source, *reference.Clip);
			ImGui::TreePop();
		}

		ImGui::PopID();
	}

	TimelinesOpenDirty = false;
}

void CCinemaFilmMakerManager::DrawClipTimeline(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip)
{
	const MotionID motion = CinemaFindMotion(actor, source, clip);

	if (!motion.valid())
	{
		ImGui::TextDisabled("This animation is not loaded for the model");
		return;
	}

	const float total = actor.Animated->get_animation_length(motion);
	const bool playing = actor.Motion.valid() && actor.Motion == motion;
	CBlend* blend = playing ? CinemaFindBlend(actor.Animated, motion) : nullptr;
	float time = blend != nullptr ? blend->timeCurrent : 0.0f;

	STrimRange* trim = FindTrimEdit(actor, source.Id, clip.Name);

	if (trim == nullptr && total > 0.0f)
	{
		STrimRange entry;
		entry.Source = source.Id;
		entry.Name = clip.Name;
		entry.Speed = actor.Speed;
		actor.Trims.push_back(entry);
		trim = &actor.Trims.back();
	}

	if (!playing)
	{
		time = trim != nullptr && !trim->Segments.empty() ? trim->Segments.front().Start : 0.0f;
	}

	string512 trimKey = {};
	xr_sprintf(trimKey, "%s|%s", source.Id.c_str(), clip.Name.c_str());

	const bool canShoot = actor.ItemSection.size() > 0 && pSettings != nullptr && pSettings->line_exist(actor.ItemSection.c_str(), "fire_point");
	if (DrawAnimationTimeline("##cinema_clip_timeline", time, total, actor.Animated->LL_GetMotionDef(motion), GetTheme(), trim, TrimDragMarker, TrimDragIndex, TrimDragKey, trimKey, canShoot))
	{
		if (!playing)
		{
			PreviewClip(actor, source, clip);
		}

		actor.Paused = true;
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
		actor.ShotPreviousTime = time;
	}

	if (ImGui::IsItemHovered() || ImGui::IsItemActive())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}

	if (blend != nullptr)
	{
		const u32 frame = static_cast<u32>(iFloor(time / SAMPLE_SPF + 0.5f));
		const u32 frames = static_cast<u32>(iFloor(total / SAMPLE_SPF + 0.5f));
		ImGui::Text("Frame: %u / %u    Time: %.3f / %.3f s", frame, frames, time, total);
	}
	else
	{
		ImGui::TextDisabled("Clip length: %.3f s", total);
	}

	if (trim != nullptr && canShoot)
	{
		if (ImGui::TreeNodeEx("Shots###clip_shots", ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
		{
			ImGui::BeginDisabled(trim->Shots.size() >= kCinemaMaxShots);
			if (ImGui::SmallButton("Add shot at playhead"))
			{
				trim->Shots.push_back(std::clamp(time, 0.0f, std::max(0.0f, total - END_EPS)));
			}
			ImGui::EndDisabled();
			ImGui::SameLine();
			ImGui::TextDisabled("Shift+click on timeline");
			if (!pSettings->line_exist(actor.ItemSection.c_str(), "ammo_class"))
			{
				ImGui::TextDisabled("This item has no ammo_class for bullet shots.");
			}
			for (s32 index = 0; index < static_cast<s32>(trim->Shots.size()); ++index)
			{
				ImGui::PushID(index);
				ImGui::SetNextItemWidth(110.0f);
				ImGui::DragFloat("Time##shot", &trim->Shots[index], 0.01f, 0.0f, std::max(0.0f, total - END_EPS), "%.3f s", ImGuiSliderFlags_AlwaysClamp);
				ImGui::SameLine();
				if (ImGui::SmallButton("Remove##shot"))
				{
					trim->Shots.erase(trim->Shots.begin() + index);
					ImGui::PopID();
					break;
				}
				ImGui::PopID();
			}
			ImGui::TreePop();
		}
	}

	if (ImGui::TreeNodeEx("Segments###clip_segments", ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		const s32 segmentCount = trim != nullptr ? static_cast<s32>(trim->Segments.size()) : 0;

		if (segmentCount > 0)
		{
			for (s32 index = 0; index < segmentCount; ++index)
			{
				const STrimSegment& segment = trim->Segments[index];

				ImGui::PushID(index);
				ImGui::Text("#%d  %.3f - %.3f s  (%.3f s)", index + 1, segment.Start, segment.End, segment.End - segment.Start);
				ImGui::SameLine();

				if (ImGui::SmallButton("Remove##segment"))
				{
					trim->Segments.erase(trim->Segments.begin() + index);
					ImGui::PopID();
					ImGui::TreePop();
					return;
				}

				ImGui::PopID();
			}

			if (ImGui::SmallButton("Clear segments##clip"))
			{
				trim->Segments.clear();
			}
		}
		else
		{
			ImGui::TextDisabled("Whole clip is played. Ctrl+drag on the timeline to add an IN/OUT segment chain.");
		}

		ImGui::TreePop();
	}

	if (ImGui::TreeNodeEx("Playback###clip_playback", ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		float speed = trim != nullptr ? trim->Speed : actor.Speed;

		ImGui::SetNextItemWidth(90.0f);

		if (trim != nullptr && ImGui::DragFloat("Speed##clip", &speed, 0.01f, -4.0f, 4.0f, "%.2f"))
		{
			trim->Speed = speed;
			ApplySpeed(actor);
		}

		if (trim != nullptr && CinemaResetRequested())
		{
			trim->Speed = actor.Speed;
			ApplySpeed(actor);
		}

		ImGui::SameLine();
		ImGui::TextDisabled("base: %.2f", actor.Speed);

		float blendTime = trim != nullptr ? trim->Blend : 0.0f;

		ImGui::SetNextItemWidth(90.0f);

		if (trim != nullptr && ImGui::DragFloat("Blend##clip", &blendTime, 0.005f, 0.0f, 1.0f, "%.2f s"))
		{
			trim->Blend = blendTime;
		}

		if (trim != nullptr && CinemaResetRequested())
		{
			trim->Blend = 0.0f;
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Smooth transition between segments, in seconds (0 - hard cut)");
		}

		ImGui::SameLine();
		ImGui::TextDisabled("0 - hard cut");

		ImGui::TreePop();
	}

	if (trim != nullptr && trim->Segments.empty() && trim->Shots.empty() && fabsf(trim->Speed - actor.Speed) <= 0.001f && trim->Blend <= 0.0f)
	{
		RemoveTrim(actor, source.Id, clip.Name);
	}
}

void CCinemaFilmMakerManager::DrawModelTab()
{
	if (!ModelFilesBuilt)
	{
		BuildModelFiles();
	}

	const float availableHeight = ImGui::GetContentRegionAvail().y;
	const float listHeight = std::clamp(availableHeight * 0.4f, 150.0f, 240.0f);
	const bool hasModel = SelectedVisual.size() > 0 && VisualPathExists(SelectedVisual.c_str());

	ImGui::SeparatorText("Model library");
	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##cinema_ogf_filter", "Search models", ModelFileFilter, sizeof(ModelFileFilter));

	if (CinemaResetRequested())
	{
		ModelFileFilter[0] = '\0';
	}

	xr_vector<s32> filteredFiles;
	filteredFiles.reserve(ModelFiles.size());

	for (s32 i = 0; i < static_cast<s32>(ModelFiles.size()); ++i)
	{
		if (CinemaContainsFilter(ModelFiles[i].c_str(), ModelFileFilter))
		{
			filteredFiles.push_back(i);
		}
	}

	ImGui::BeginChild("##cinema_ogf_list", ImVec2(0.0f, listHeight), true);

	ImGuiListClipper fileClipper;
	fileClipper.Begin(static_cast<int>(filteredFiles.size()));

	while (fileClipper.Step())
	{
		for (int row = fileClipper.DisplayStart; row < fileClipper.DisplayEnd; ++row)
		{
			const s32 index = filteredFiles[row];
			const shared_str& file = ModelFiles[index];

			ImGui::PushID(index);

			if (ImGui::Selectable(file.c_str(), SelectedOgf == index))
			{
				SelectedOgf = index;
				SelectedVisual = file;

				if (SCinemaActor* actor = GetSelectedActor())
				{
					SetActorVisual(*actor, SelectedVisual.c_str());
				}
			}

			if (ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("%s", file.c_str());
			}

			ImGui::PopID();
		}
	}

	if (filteredFiles.empty())
	{
		ImGui::TextDisabled("Nothing found");
	}

	ImGui::EndChild();

	ImGui::Separator();
	ImGui::TextDisabled("Model");
	ImGui::SameLine();
	ImGui::TextUnformatted(SelectedVisual.size() > 0 ? CinemaFileName(SelectedVisual.c_str()) : "<none>");

	if (ImGui::IsItemHovered() && SelectedVisual.size() > 0)
	{
		ImGui::SetTooltip("%s", SelectedVisual.c_str());
	}

	SToolbarLayout actions(ImGui::GetContentRegionAvail().x);

	actions.Item(CinemaButtonWidth("Apply to selected"));
	ImGui::BeginDisabled(!hasModel || GetSelectedActor() == nullptr);

	if (ImGui::Button("Apply to selected"))
	{
		if (SCinemaActor* actor = GetSelectedActor())
		{
			SetActorVisual(*actor, SelectedVisual.c_str());
		}
	}

	ImGui::EndDisabled();

	actions.Item(CinemaButtonWidth("Add"));
	ImGui::BeginDisabled(!hasModel);

	if (ImGui::Button("Add##model"))
	{
		AddActorInFront(SelectedVisual.c_str());
	}

	ImGui::EndDisabled();
}

void CCinemaFilmMakerManager::DrawItemTab()
{
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		ImGui::TextDisabled("Select a fake NPC to give it a held item");
		return;
	}

	BuildHandItems();

	ImGui::SeparatorText("Item library");
	const float availableHeight = ImGui::GetContentRegionAvail().y;
	const float listHeight = std::max(150.0f, availableHeight - 105.0f);

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##cinema_item_filter", "Search items", ItemFilter, sizeof(ItemFilter));

	if (CinemaResetRequested())
	{
		ItemFilter[0] = '\0';
	}

	xr_vector<s32> filteredItems;
	filteredItems.reserve(HandItems.size());

	for (s32 i = 0; i < static_cast<s32>(HandItems.size()); ++i)
	{
		if (CinemaContainsFilter(HandItems[i].Section.c_str(), ItemFilter) || CinemaContainsFilter(HandItems[i].Visual.c_str(), ItemFilter))
		{
			filteredItems.push_back(i);
		}
	}

	ImGui::BeginChild("##cinema_items_list", ImVec2(0.0f, listHeight), true);

	ImGuiListClipper itemClipper;
	itemClipper.Begin(static_cast<int>(filteredItems.size()));

	while (itemClipper.Step())
	{
		for (int row = itemClipper.DisplayStart; row < itemClipper.DisplayEnd; ++row)
		{
			const s32 index = filteredItems[row];
			const SHandItem& item = HandItems[index];
			const bool selected = actor->ItemSection.size() > 0 && xr_strcmp(actor->ItemSection.c_str(), item.Section.c_str()) == 0;

			ImGui::PushID(index);

			if (ImGui::Selectable(item.Section.c_str(), selected))
			{
				SetActorItem(*actor, item.Section);
			}

			if (ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("%s\n%s", item.Section.c_str(), item.Visual.c_str());
			}

			ImGui::PopID();
		}
	}

	if (filteredItems.empty())
	{
		ImGui::TextDisabled("Nothing found");
	}

	ImGui::EndChild();

	ImGui::SeparatorText("Selected item");
	ImGui::TextDisabled("Item");
	ImGui::SameLine();
	ImGui::TextUnformatted(actor->ItemSection.size() > 0 ? actor->ItemSection.c_str() : "<none>");

	if (actor->ItemSection.size() > 0 && actor->Skeleton != nullptr)
	{
		const u16 boneL = actor->ItemHandDependence == 1 ? actor->ItemBoneR2 : actor->ItemBoneL;
		const bool resolvable = actor->ItemWeaponStyle ? (actor->ItemBoneR != u16(-1) && boneL != u16(-1)) : (actor->ItemBoneAttach != u16(-1));

		if (!resolvable)
		{
			ImGui::TextDisabled("This model has no bones for this item");
		}
	}

	SToolbarLayout actions(ImGui::GetContentRegionAvail().x);

	actions.Item(CinemaButtonWidth("Remove"));
	ImGui::BeginDisabled(actor->ItemSection.size() == 0);

	if (ImGui::Button("Remove##item"))
	{
		SetActorItem(*actor, shared_str());
	}

	ImGui::EndDisabled();
}

void CCinemaFilmMakerManager::DrawAnimationTab()
{
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		ImGui::TextDisabled("Select a fake NPC to edit its animation playlist");
		return;
	}

	if (actor->Animated == nullptr)
	{
		ImGui::TextDisabled("The selected model has no animations");
		return;
	}

	const float totalWidth = ImGui::GetContentRegionAvail().x;
	const float splitterWidth = 6.0f;
	const float minWidth = 180.0f;
	const float maxWidth = std::max(minWidth, totalWidth - splitterWidth - minWidth - ImGui::GetStyle().ItemSpacing.x * 2.0f);
	if (AnimationLibraryWidth <= 0.0f) AnimationLibraryWidth = totalWidth * 0.5f;
	AnimationLibraryWidth = std::clamp(AnimationLibraryWidth, minWidth, maxWidth);
	ImGui::BeginChild("##animation_library_panel", ImVec2(AnimationLibraryWidth, 0.0f), true);
	bool hasSelectedClip = false;

	if (SelectedSource >= 0 && SelectedSource < static_cast<s32>(actor->Sources.size()))
	{
		hasSelectedClip = CinemaSourcePlayable(actor->Sources[SelectedSource], *actor) && SelectedClip >= 0 && SelectedClip < static_cast<s32>(actor->Sources[SelectedSource].Clips.size());
	}

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##cinema_clip_filter", "Search animations and sources", ClipFilter, sizeof(ClipFilter));

	if (CinemaResetRequested())
	{
		ClipFilter[0] = '\0';
	}
	if (ImGui::Button("Browse##cinema_omf"))
	{
		FS_FileSet files;
		FS.file_list(files, _game_meshes_, FS_ListFiles, "*.omf");
		FS.file_list(files, "$level$", FS_ListFiles, "*.omf");
		AnimationBrowseFiles.clear();
		for (const FS_File& file : files)
		{
			xr_string path = file.name;
			std::replace(path.begin(), path.end(), '/', '\\');
			AnimationBrowseFiles.push_back(std::move(path));
		}
		AnimationBrowseFolder.clear();
		AnimationBrowseFilter[0] = '\0';
		AnimationBrowseStatus.clear();
		ImGui::OpenPopup("##cinema_omf_browser");
	}
	ImGui::SetNextWindowSize(ImVec2(650.0f, 450.0f), ImGuiCond_Appearing);
	if (ImGui::BeginPopup("##cinema_omf_browser"))
	{
		ImGui::TextWrapped("Game resources / %s", AnimationBrowseFolder.c_str());
		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputTextWithHint("##omf_browser_filter", "Filter files in this folder", AnimationBrowseFilter, sizeof(AnimationBrowseFilter));
		if (!AnimationBrowseFolder.empty() && ImGui::Button("Up##omf_browser"))
		{
			const size_t slash = AnimationBrowseFolder.find_last_of('\\', AnimationBrowseFolder.size() - 2);
			AnimationBrowseFolder = slash == xr_string::npos ? "" : AnimationBrowseFolder.substr(0, slash + 1);
			AnimationBrowseStatus.clear();
		}
		if (!AnimationBrowseStatus.empty()) ImGui::TextWrapped("%s", AnimationBrowseStatus.c_str());
		ImGui::BeginChild("##omf_browser_files", ImVec2(0.0f, -ImGui::GetFrameHeightWithSpacing()), true);
		xr_set<xr_string> folders;
		xr_vector<const xr_string*> files;
		for (const auto& path : AnimationBrowseFiles)
		{
			if (path.size() <= AnimationBrowseFolder.size() || _strnicmp(path.c_str(), AnimationBrowseFolder.c_str(), AnimationBrowseFolder.size()) != 0) continue;
			const xr_string tail = path.substr(AnimationBrowseFolder.size());
			const size_t slash = tail.find('\\');
			if (slash != xr_string::npos) folders.insert(tail.substr(0, slash));
			else if (CinemaContainsFilter(tail.c_str(), AnimationBrowseFilter)) files.push_back(&path);
		}
		for (const auto& folder : folders)
		{
			ImGui::PushID(folder.c_str());
			const xr_string label = xr_string("[Folder] ") + folder;
			if (ImGui::Selectable(label.c_str()))
			{
				AnimationBrowseFolder += folder;
				AnimationBrowseFolder += '\\';
				AnimationBrowseStatus.clear();
			}
			ImGui::PopID();
		}
		for (const auto* path : files)
		{
			ImGui::PushID(path->c_str());
			if (ImGui::Selectable(CinemaFileName(path->c_str())))
			{
				SAnimationSource indexed;
				if (!CinemaIndexAnimationFile(path->c_str(), indexed))
				{
					AnimationBrowseStatus = "This file could not be indexed as an animation source.";
					ImGui::PopID();
					continue;
				}
				if (std::find(actor->AnimationPacks.begin(), actor->AnimationPacks.end(), indexed.Id) == actor->AnimationPacks.end())
				{
					if (actor->AnimationPacks.size() >= kCinemaSceneMaxPlaylist)
					{
						AnimationBrowseStatus = "The actor has reached the animation pack limit.";
						ImGui::PopID();
						continue;
					}
					actor->AnimationPacks.push_back(indexed.Id);
					BuildActorSources(*actor);
				}
				const auto found = std::find_if(actor->Sources.begin(), actor->Sources.end(), [&](const SAnimationSource& source)
				{
					xr_string sourcePath = source.Id.c_str();
					std::replace(sourcePath.begin(), sourcePath.end(), '/', '\\');
					return _stricmp(sourcePath.c_str(), path->c_str()) == 0;
				});
				if (found == actor->Sources.end()) AnimationBrowseStatus = "This file could not be indexed as an animation source.";
				else
				{
					SelectedSource = static_cast<s32>(found - actor->Sources.begin());
					SelectedClip = -1;
					SelectedClips.clear();
					ClipAnchor = { -1, -1 };
					const bool modelSource = std::find(actor->ModelSources.begin(), actor->ModelSources.end(), found->Id) != actor->ModelSources.end();
					AnimationBrowseOpenGroup = modelSource ? 0 : 1;
					AnimationBrowseOpenSource = SelectedSource;
					ClipFilter[0] = '\0';
					EnsureSource(*actor, found->Id);
					ImGui::CloseCurrentPopup();
				}
			}
			ImGui::PopID();
		}
		ImGui::EndChild();
		if (ImGui::Button("Close##omf_browser")) ImGui::CloseCurrentPopup();
		ImGui::EndPopup();
	}
	ImGui::SameLine();
	ImGui::BeginDisabled(!hasSelectedClip);

	if (ImGui::Button("Add##clip"))
	{
		const auto clips = SelectedClips.empty() ? xr_vector<std::pair<s32, s32>>{ { SelectedSource, SelectedClip } } : SelectedClips;
		for (const auto& [sourceIndex, clipIndex] : clips)
		{
			if (sourceIndex >= 0 && sourceIndex < static_cast<s32>(actor->Sources.size()) && clipIndex >= 0 && clipIndex < static_cast<s32>(actor->Sources[sourceIndex].Clips.size()))
			{
				const auto& source = actor->Sources[sourceIndex];
				const auto& clip = source.Clips[clipIndex];
				AddClipToPlaylist(*actor, source.Id, clip.Name, clip.Fx);
			}
		}
	}

	ImGui::EndDisabled();

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Add the selected animation to the playlist");
	}

	CinemaHint("Shift + click: select range | Ctrl + click: toggle | Double-click: add");
	if (!ExtraPathStatus.empty()) ImGui::TextWrapped("%s", ExtraPathStatus.c_str());
	ImGui::BeginChild("##cinema_animation_library", ImVec2(0.0f, 0.0f), true);

	shared_str packToRemove;
	xr_vector<std::pair<s32, s32>> visibleClips;
	std::pair<s32, s32> clickedClip = { -1, -1 };

	const char* sourceGroups[] = { "Model animations", "Selected OMF packs" };
	for (s32 group = 0; group < 2; ++group)
	{
		if (AnimationBrowseOpenGroup == group)
		{
			ImGui::SetNextItemOpen(true, ImGuiCond_Always);
			AnimationBrowseOpenGroup = -1;
		}
		if (!ImGui::TreeNodeEx(sourceGroups[group], ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_SpanAvailWidth | (group == 0 ? ImGuiTreeNodeFlags_DefaultOpen : 0))) continue;
		for (s32 sourceIndex = 0; sourceIndex < static_cast<s32>(actor->Sources.size()); ++sourceIndex)
		{
		const SAnimationSource& source = actor->Sources[sourceIndex];
		const bool modelSource = std::find(actor->ModelSources.begin(), actor->ModelSources.end(), source.Id) != actor->ModelSources.end();
		const s32 sourceGroup = modelSource ? 0 : 1;
		if (sourceGroup != group) continue;
		const bool sourceMatches = CinemaContainsFilter(source.Id.c_str(), ClipFilter);
		bool hasMatchingClip = false;

		for (const SAnimationClip& clip : source.Clips)
		{
			if (sourceMatches || CinemaContainsFilter(clip.Name.c_str(), ClipFilter))
			{
				hasMatchingClip = true;
				break;
			}
		}

		if (!sourceMatches && !hasMatchingClip)
		{
			continue;
		}

		ImGui::PushID(sourceIndex);

		const bool playable = CinemaSourcePlayable(source, *actor);
		const char* sourceStatus = !playable ? " [slot limit reached]" : "";
		string256 sourceLabel = {};
		xr_sprintf(sourceLabel, "%s (%u)%s", CinemaFileName(source.Id.c_str()), static_cast<u32>(source.Clips.size()), sourceStatus);

		ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_SpanAvailWidth;

		if (SelectedSource == sourceIndex)
		{
			flags |= ImGuiTreeNodeFlags_Selected;
		}

		if (SelectedSource == sourceIndex || (SelectedSource < 0 && sourceIndex == 0) || (ClipFilter[0] != '\0' && hasMatchingClip))
		{
			flags |= ImGuiTreeNodeFlags_DefaultOpen;
		}

		if (AnimationBrowseOpenSource == sourceIndex)
		{
			ImGui::SetNextItemOpen(true, ImGuiCond_Always);
			AnimationBrowseOpenSource = -1;
		}
		const bool open = ImGui::TreeNodeEx(sourceLabel, flags);
		if (!modelSource && ImGui::BeginPopupContextItem("##animation_pack_context"))
		{
			const bool inUse = std::any_of(actor->Playlist.begin(), actor->Playlist.end(), [&](const SPlaylistEntry& entry) { return entry.Source == source.Id; }) ||
				std::any_of(actor->MasterClips.begin(), actor->MasterClips.end(), [&](const SMasterClip& clip) { return clip.Source == source.Id; }) || (actor->Motion.valid() && actor->MotionSource == source.Id);
			ImGui::BeginDisabled(inUse);
			if (ImGui::MenuItem("Remove pack")) packToRemove = source.Id;
			ImGui::EndDisabled();
			if (inUse) ImGui::TextDisabled("This pack is used by the actor.");
			ImGui::EndPopup();
		}

		if (ImGui::IsItemClicked())
		{
			SelectedSource = sourceIndex;
			SelectedClip = -1;
			SelectedClips.clear();
			ClipAnchor = { -1, -1 };
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s", source.Id.c_str());
		}

		if (open)
		{
			for (s32 clipIndex = 0; clipIndex < static_cast<s32>(source.Clips.size()); ++clipIndex)
			{
				const SAnimationClip& clip = source.Clips[clipIndex];

				if (!sourceMatches && !CinemaContainsFilter(clip.Name.c_str(), ClipFilter))
				{
					continue;
				}

				ImGui::PushID(clipIndex);

				string256 clipLabel = {};
				xr_sprintf(clipLabel, "%s%s", clip.Name.c_str(), clip.Fx ? " [fx]" : "");

				const std::pair<s32, s32> clipId = { sourceIndex, clipIndex };
				visibleClips.push_back(clipId);
				const bool selected = SelectedClips.empty() ? SelectedSource == sourceIndex && SelectedClip == clipIndex : std::find(SelectedClips.begin(), SelectedClips.end(), clipId) != SelectedClips.end();

				if (ImGui::Selectable(clipLabel, selected))
				{
					clickedClip = clipId;
				}

				if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
				{
					SelectedSource = sourceIndex;
					SelectedClip = clipIndex;
					AddClipToPlaylist(*actor, source.Id, clip.Name, clip.Fx);
				}

				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip("%s\n%s%s", clip.Name.c_str(), source.Id.c_str(), !playable ? "\nAnimation slot limit reached." : "");
				}

				if (ImGui::BeginPopupContextItem("##cinema_clip_context"))
				{
					if (ImGui::MenuItem("Add to playlist", nullptr, false, playable))
					{
						const auto clips = selected && !SelectedClips.empty() ? SelectedClips : xr_vector<std::pair<s32, s32>>{ clipId };
						for (const auto& [selectedSource, selectedClip] : clips)
						{
							if (selectedSource >= 0 && selectedSource < static_cast<s32>(actor->Sources.size()) && selectedClip >= 0 && selectedClip < static_cast<s32>(actor->Sources[selectedSource].Clips.size()))
							{
								const auto& itemSource = actor->Sources[selectedSource];
								const auto& itemClip = itemSource.Clips[selectedClip];
								AddClipToPlaylist(*actor, itemSource.Id, itemClip.Name, itemClip.Fx);
							}
						}
					}

					if (ImGui::MenuItem("Play preview", nullptr, false, playable))
					{
						if (!selected)
						{
							SelectedClips = { clipId };
							ClipAnchor = clipId;
						}
						SelectedSource = sourceIndex;
						SelectedClip = clipIndex;
						PreviewClip(*actor, source, clip);
					}

					ImGui::EndPopup();
				}

				if (playable && ImGui::BeginDragDropSource())
				{
					SClipDragPayload payload = {};
					xr_strcpy(payload.Source, source.Id.c_str());
					xr_strcpy(payload.Name, clip.Name.c_str());
					payload.Fx = clip.Fx;

					ImGui::SetDragDropPayload(kCinemaClipPayload, &payload, sizeof(payload));
					ImGui::TextUnformatted(clipLabel);
					ImGui::EndDragDropSource();
				}

				ImGui::PopID();
			}

			ImGui::TreePop();
		}

		ImGui::PopID();
		}
		if (group == 1)
		{
			for (const shared_str& pack : actor->AnimationPacks)
			{
				if (FindSource(*actor, pack) == nullptr) ImGui::TextWrapped("Unavailable: %s", pack.c_str());
			}
		}
		ImGui::TreePop();
	}

	if (packToRemove.size())
	{
		auto& packs = actor->AnimationPacks;
		packs.erase(std::remove(packs.begin(), packs.end(), packToRemove), packs.end());
		BuildActorSources(*actor);
		clickedClip = { -1, -1 };
	}
	if (clickedClip.first >= 0)
	{
		const auto first = std::find(visibleClips.begin(), visibleClips.end(), ClipAnchor);
		const auto last = std::find(visibleClips.begin(), visibleClips.end(), clickedClip);
		if (ImGui::GetIO().KeyShift && first != visibleClips.end() && last != visibleClips.end())
		{
			SelectedClips.assign(std::min(first, last), std::max(first, last) + 1);
		}
		else if (ImGui::GetIO().KeyCtrl)
		{
			const auto it = std::find(SelectedClips.begin(), SelectedClips.end(), clickedClip);
			if (it == SelectedClips.end())
			{
				SelectedClips.push_back(clickedClip);
			}
			else
			{
				SelectedClips.erase(it);
			}
			ClipAnchor = clickedClip;
		}
		else
		{
			SelectedClips = { clickedClip };
			ClipAnchor = clickedClip;
		}
		if (SelectedClips.empty())
		{
			SelectedClip = -1;
		}
		else
		{
			const auto active = std::find(SelectedClips.begin(), SelectedClips.end(), clickedClip) != SelectedClips.end() ? clickedClip : SelectedClips.back();
			SelectedSource = active.first;
			SelectedClip = active.second;
			PreviewClip(*actor, actor->Sources[active.first], actor->Sources[active.first].Clips[active.second]);
		}
	}
	ImGui::EndChild();
	ImGui::EndChild();
	ImGui::SameLine();
	ImGui::InvisibleButton("##animation_library_splitter", ImVec2(splitterWidth, std::max(1.0f, ImGui::GetContentRegionAvail().y)));
	if (ImGui::IsItemHovered() || ImGui::IsItemActive())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
		ImGui::GetWindowDrawList()->AddRectFilled(ImGui::GetItemRectMin(), ImGui::GetItemRectMax(), ImGui::GetColorU32(ImGuiCol_SeparatorHovered));
	}
	if (ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		AnimationLibraryWidth = std::clamp(AnimationLibraryWidth + ImGui::GetIO().MouseDelta.x, minWidth, maxWidth);
	}
	ImGui::SameLine();
	ImGui::BeginChild("##animation_playlist_panel", ImVec2(0.0f, 0.0f), true);
	DrawPlaylist(*actor, std::max(100.0f, ImGui::GetContentRegionAvail().y * 0.35f));
	ImGui::SeparatorText("Animation timelines");
	ImGui::BeginChild("##animation_timelines", ImVec2(0.0f, 0.0f), true);
	DrawClipTimelines(*actor);
	ImGui::EndChild();
	ImGui::EndChild();
}

void CCinemaFilmMakerManager::DrawPlaylist(SCinemaActor& actor, float listHeight)
{
	ImGui::SeparatorText("Playlist");
	CinemaHint("Shift + click: select range | Ctrl + click: toggle | Right-click: actions");
	if (actor.PatternOwned)
	{
		ImGui::TextWrapped("The master timeline controls playback. FX animations and blend transitions are not supported in patterns.");
	}

	SToolbarLayout playlistControls(ImGui::GetContentRegionAvail().x);

	ImGui::BeginDisabled(actor.PatternOwned);
	playlistControls.Item(CinemaButtonWidth("Play"));
	if (ImGui::Button("Play##playlist"))
	{
		StartPlaylist(actor, actor.PlaylistSelected >= 0 ? actor.PlaylistSelected : 0);
	}

	playlistControls.Item(CinemaButtonWidth("Stop"));
	if (ImGui::Button("Stop##playlist"))
	{
		StopActor(actor);
	}

	ImGui::EndDisabled();

	playlistControls.Item(CinemaButtonWidth("Clear"));
	ImGui::BeginDisabled(actor.Playlist.empty());

	if (ImGui::Button("Clear##playlist"))
	{
		StopActor(actor);
		actor.Playlist.clear();
		actor.PlaylistSelected = -1;
		actor.PlaylistSelection.clear();
		actor.PlaylistAnchor = -1;
	}

	ImGui::EndDisabled();

	playlistControls.Item(CinemaButtonWidth("Entries: 00"));
	ImGui::TextDisabled("Entries: %u", static_cast<u32>(actor.Playlist.size()));

	bool loop = actor.Loop;

	playlistControls.Item(CinemaButtonWidth("Loop") + 20.0f);
	if (ImGui::Checkbox("Loop##animations", &loop))
	{
		actor.Loop = loop;

		if (actor.Animated != nullptr && actor.Motion.valid() && !actor.MotionFx && actor.PlaylistIndex < 0)
		{
			CinemaSetBlendLoop(actor.Animated, actor.Motion, actor.Loop);
		}
	}

	if (CinemaResetRequested() && actor.Loop)
	{
		actor.Loop = false;

		if (actor.Animated != nullptr && actor.Motion.valid() && !actor.MotionFx && actor.PlaylistIndex < 0)
		{
			CinemaSetBlendLoop(actor.Animated, actor.Motion, false);
		}
	}

	ImGui::BeginChild("##cinema_playlist_items", ImVec2(0.0f, listHeight), true);

	if (ImGui::BeginDragDropTarget())
	{
		if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipPayload))
		{
			const SClipDragPayload* clip = static_cast<const SClipDragPayload*>(payload->Data);
			AddClipToPlaylist(actor, clip->Source, clip->Name, clip->Fx);
		}
		else if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipIndexPayload))
		{
			const SIndexDragPayload* data = static_cast<const SIndexDragPayload*>(payload->Data);
			MovePlaylistEntry(actor, data->Index, static_cast<s32>(actor.Playlist.size()));
		}

		ImGui::EndDragDropTarget();
	}

	if (actor.Playlist.empty())
	{
		ImGui::TextDisabled("Drag animations here or use the context menu");
	}

	bool muteChanged = false;
	for (s32 i = 0; i < static_cast<s32>(actor.Playlist.size()); ++i)
	{
		SPlaylistEntry& entry = actor.Playlist[i];
		const bool selected = actor.PlaylistSelection.empty() ? actor.PlaylistSelected == i : std::find(actor.PlaylistSelection.begin(), actor.PlaylistSelection.end(), i) != actor.PlaylistSelection.end();
		ImGui::PushID(i);

		bool enabled = !entry.Muted;
		if (ImGui::Checkbox("##playlist_enabled", &enabled))
		{
			const auto indices = selected && !actor.PlaylistSelection.empty() ? actor.PlaylistSelection : xr_vector<s32>{ i };
			for (const s32 index : indices)
			{
				if (index >= 0 && index < static_cast<s32>(actor.Playlist.size()))
				{
					actor.Playlist[index].Muted = !enabled;
				}
			}
			muteChanged = true;
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Enable or mute selected playlist entries");
		}
		ImGui::SameLine();

		string256 label = {};
		const bool playing = actor.MasterOwned ? MasterActive && MasterPlaying && actor.MasterPlaylistIndex == i : !actor.Paused && actor.PlaylistIndex == i;
		xr_sprintf(label, "%s%d. %s%s%s###playlist_row", playing ? "[PLAYING] " : "", i + 1, entry.Name.c_str(), entry.Fx ? " [fx]" : "", entry.Muted ? " [muted]" : "");
		if (ImGui::Selectable(label, selected))
		{
			if (ImGui::GetIO().KeyShift && actor.PlaylistAnchor >= 0 && actor.PlaylistAnchor < static_cast<s32>(actor.Playlist.size()))
			{
				actor.PlaylistSelection.clear();
				for (s32 index = std::min(actor.PlaylistAnchor, i); index <= std::max(actor.PlaylistAnchor, i); ++index)
				{
					actor.PlaylistSelection.push_back(index);
				}
			}
			else if (ImGui::GetIO().KeyCtrl)
			{
				if (actor.PlaylistSelection.empty() && actor.PlaylistSelected >= 0)
				{
					actor.PlaylistSelection.push_back(actor.PlaylistSelected);
				}
				const auto it = std::find(actor.PlaylistSelection.begin(), actor.PlaylistSelection.end(), i);
				if (it == actor.PlaylistSelection.end())
				{
					actor.PlaylistSelection.push_back(i);
				}
				else
				{
					actor.PlaylistSelection.erase(it);
				}
				actor.PlaylistAnchor = i;
			}
			else
			{
				actor.PlaylistSelection = { i };
				actor.PlaylistAnchor = i;
			}
			actor.PlaylistSelected = std::find(actor.PlaylistSelection.begin(), actor.PlaylistSelection.end(), i) != actor.PlaylistSelection.end() ? i : (actor.PlaylistSelection.empty() ? -1 : actor.PlaylistSelection.back());
		}

		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		{
			StartPlaylist(actor, i);
		}

		if (ImGui::BeginDragDropSource())
		{
			SIndexDragPayload payload = {};
			payload.Index = i;

			ImGui::SetDragDropPayload(kCinemaClipIndexPayload, &payload, sizeof(payload));
			ImGui::TextUnformatted(label, ImGui::FindRenderedTextEnd(label));
			ImGui::EndDragDropSource();
		}

		if (ImGui::BeginDragDropTarget())
		{
			if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipIndexPayload))
			{
				const SIndexDragPayload* data = static_cast<const SIndexDragPayload*>(payload->Data);
				MovePlaylistEntry(actor, data->Index, i);
			}
			else if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipPayload))
			{
				const SClipDragPayload* clip = static_cast<const SClipDragPayload*>(payload->Data);
				AddClipToPlaylist(actor, clip->Source, clip->Name, clip->Fx);
			}

			ImGui::EndDragDropTarget();
		}

		if (ImGui::BeginPopupContextItem("##cinema_playlist_context"))
		{
			if (ImGui::MenuItem("Play from here"))
			{
				StartPlaylist(actor, i);
			}

			if (ImGui::MenuItem("Remove"))
			{
				auto indices = selected && !actor.PlaylistSelection.empty() ? actor.PlaylistSelection : xr_vector<s32>{ i };
				std::sort(indices.begin(), indices.end(), std::greater<s32>());
				for (const s32 index : indices)
				{
					RemovePlaylistEntry(actor, index);
				}
				actor.PlaylistSelection.clear();
				actor.PlaylistAnchor = -1;
				ImGui::EndPopup();
				ImGui::PopID();
				break;
			}

			ImGui::EndPopup();
		}

		ImGui::PopID();
	}

	ImGui::EndChild();

	if (muteChanged && actor.PlaylistIndex >= 0 && actor.PlaylistIndex < static_cast<s32>(actor.Playlist.size()) && actor.Playlist[actor.PlaylistIndex].Muted)
	{
		if (actor.MotionFx && actor.Animated != nullptr && actor.Motion.valid())
		{
			struct StopFx : IterateBlendsCallback
			{
				MotionID Motion;
				void operator()(CBlend& blend) override
				{
					if (blend.motionID == Motion)
					{
						blend.blendAmount = 0.0f;
						blend.set_falloff_state();
					}
				}
			} stopFx;
			stopFx.Motion = actor.Motion;
			actor.Animated->LL_IterateBlends(stopFx);
		}
		AdvanceActorPlaylist(actor);
		if (actor.PlaylistIndex < 0)
		{
			StopActor(actor);
		}
	}
}

void CCinemaFilmMakerManager::DrawParticleBrowser()
{
	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##particle_filter", "Search particles", ParticleFilter, sizeof(ParticleFilter));
	if (CinemaResetRequested())
	{
		ParticleFilter[0] = '\0';
	}
	const auto& names = GamePersistent().Environment().m_pRender->particles_systems_library().vec_all_particles();
	auto* curves = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	xr_vector<s32> filtered;
	for (s32 i = 0; i < static_cast<s32>(names.size()); ++i)
	{
		if ((!curves || !curves->FindIPAC(names[i].c_str())) && CinemaContainsFilter(names[i].c_str(), ParticleFilter))
		{
			filtered.push_back(i);
		}
	}
	CinemaHint("Click an effect to apply and preview | Shift + click: select range | Ctrl + click: toggle");
	if (ImGui::BeginChild("##particle_library", ImVec2(0.0f, 160.0f), true))
	{
		ImGuiListClipper clipper;
		clipper.Begin(static_cast<int>(filtered.size()));
		while (clipper.Step())
		{
			for (int row = clipper.DisplayStart; row < clipper.DisplayEnd; ++row)
			{
				const auto& name = names[filtered[row]];
				ImGui::PushID(filtered[row]);
				const bool selected = std::find(SelectedParticleNames.begin(), SelectedParticleNames.end(), name) != SelectedParticleNames.end();
				if (ImGui::Selectable(name.c_str(), selected))
				{
					const auto anchor = std::find_if(filtered.begin(), filtered.end(), [&](s32 index) { return names[index] == ParticleLibraryAnchor; });
					if (ImGui::GetIO().KeyShift && anchor != filtered.end())
					{
						const s32 first = static_cast<s32>(anchor - filtered.begin());
						SelectedParticleNames.clear();
						for (s32 i = std::min(first, row); i <= std::max(first, row); ++i)
						{
							SelectedParticleNames.push_back(names[filtered[i]]);
						}
					}
					else if (ImGui::GetIO().KeyCtrl)
					{
						const auto it = std::find(SelectedParticleNames.begin(), SelectedParticleNames.end(), name);
						if (it == SelectedParticleNames.end())
						{
							SelectedParticleNames.push_back(name);
						}
						else
						{
							SelectedParticleNames.erase(it);
						}
						ParticleLibraryAnchor = name;
					}
					else
					{
						SelectedParticleNames = { name };
						ParticleLibraryAnchor = name;
					}
					SelectedParticleName = std::find(SelectedParticleNames.begin(), SelectedParticleNames.end(), name) != SelectedParticleNames.end() ? name : (SelectedParticleNames.empty() ? shared_str() : SelectedParticleNames.back());
					if (!ImGui::GetIO().KeyCtrl && !ImGui::GetIO().KeyShift)
					{
						PreviewParticle(name);
					}
				}
				ImGui::PopID();
			}
		}
		if (filtered.empty())
		{
			ImGui::TextDisabled("Nothing found");
		}
	}
	ImGui::EndChild();
	ImGui::TextDisabled("Selected effects: %u", static_cast<u32>(SelectedParticleNames.size()));
	SToolbarLayout actions(ImGui::GetContentRegionAvail().x);
	actions.Item(CinemaButtonWidth("Add to scene"));
	ImGui::BeginDisabled(SelectedParticleNames.empty() || SelectedParticleNames.size() > kCinemaSceneMaxActors - Particles.size());
	if (ImGui::Button("Add to scene##particle"))
	{
		Fvector position = Device.vCameraPosition;
		Fvector direction = Device.vCameraDirection;
		if (!SpawnFromCamera && g_actor != nullptr)
		{
			position = g_actor->Position();
			direction = g_actor->Direction();
		}
		direction.normalize_safe();
		position.mad(direction, 3.0f);
		for (const auto& name : SelectedParticleNames)
		{
			AddParticle(name, position);
		}
	}
	ImGui::EndDisabled();
	if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
	{
		ImGui::SetTooltip("Add the selected library effects to the scene");
	}

}


static void DrawCinemaCurvePreviewGrid(ImDrawList* draw, const ImVec2& start, float width, float height, float duration, float minimum, float maximum, const SEditorTheme& theme)
{
	for (u32 i = 0; i <= 4; ++i)
	{
		const float fraction = i / 4.0f;
		const float x = start.x + width * fraction;
		const float y = start.y + height * fraction;
		draw->AddLine(ImVec2(x, start.y), ImVec2(x, start.y + height), theme.TimelineTickMinor);
		draw->AddLine(ImVec2(start.x, y), ImVec2(start.x + width, y), theme.TimelineTickMinor);
		char label[32];
		xr_sprintf(label, "%.2f", duration * fraction);
		const ImVec2 textSize = ImGui::CalcTextSize(label);
		draw->AddText(ImVec2(std::clamp(x + 2.0f, start.x, std::max(start.x, start.x + width - textSize.x)), start.y + height - ImGui::GetTextLineHeight()), theme.TimelineText, label);
	}
	char label[32];
	xr_sprintf(label, "%.2f", maximum);
	draw->AddText(ImVec2(start.x + 3.0f, start.y + 2.0f), theme.TimelineText, label);
	xr_sprintf(label, "%.2f", minimum);
	draw->AddText(ImVec2(start.x + 3.0f, start.y + height - 2.0f * ImGui::GetTextLineHeight()), theme.TimelineText, label);
}

void CCinemaFilmMakerManager::DrawParticleTab()
{
	if (ImGui::TreeNodeEx("Particle library##cinema", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		DrawParticleBrowser();
		ImGui::TreePop();
	}
	if (SelectedParticle < 0 || SelectedParticle >= static_cast<s32>(Particles.size()))
	{
		SelectedParticles.clear();
	}
	else if (std::find(SelectedParticles.begin(), SelectedParticles.end(), SelectedParticle) == SelectedParticles.end())
	{
		SelectedParticles = { SelectedParticle };
	}
	if (SelectedParticle < 0 || SelectedParticle >= static_cast<s32>(Particles.size()))
	{
		return;
	}
	ImGui::SeparatorText("Selected particle");
	const s32 index = SelectedParticle;
	{
		ImGui::PushID(index);
		auto& particle = Particles[index];
		const SCinemaParticle before = particle;
		bool speedChanged = false;
		bool loopChanged = false;
		bool positionReset = false;
		bool rotationReset = false;
		bool scaleChanged = false;
		bool resetTilt = false;
		bool resetChannel = false;
		bool resetCurveKey = false;
		const char* resetWaveField = nullptr;
		const bool open = ImGui::TreeNodeEx(particle.Name.c_str(), ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes | (SelectedParticle == index ? ImGuiTreeNodeFlags_Selected | ImGuiTreeNodeFlags_DefaultOpen : 0));
		if (ImGui::IsItemClicked())
		{
			SelectActor(-1);
			ClearGizmo();
			SelectedParticle = index;
		}
		if (!open)
		{
			ImGui::PopID();
			return;
		}
		SToolbarLayout controls(ImGui::GetContentRegionAvail().x);
		controls.Item(CinemaButtonWidth("Play"));
		if (ImGui::Button("Play##particle"))
		{
			for (const s32 selected : SelectedParticles)
			{
				StartParticleTimeline(Particles[selected]);
			}
		}
		controls.Item(CinemaButtonWidth("Stop"));
		if (ImGui::Button("Stop##particle"))
		{
			for (const s32 selected : SelectedParticles)
			{
				Particles[selected].Object->Stop(false);
				Particles[selected].Running = false;
			}
		}
		controls.Item(CinemaButtonWidth("Loop") + 20.0f);
		loopChanged = ImGui::Checkbox("Loop##particle", &particle.Loop);
		controls.Item(170.0f);
		ImGui::SetNextItemWidth(100.0f);
		if (ImGui::DragFloat("Speed##particle", &particle.Speed, 0.01f, 0.0f, 4.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp))
		{
			UpdateParticleTransform(particle);
			speedChanged = true;
		}
		if (CinemaResetRequested())
		{
			particle.Speed = 1.0f;
			UpdateParticleTransform(particle);
			speedChanged = true;
		}
		controls.Item(CinemaButtonWidth("Duplicate"));
		ImGui::BeginDisabled(SelectedParticles.size() > kCinemaSceneMaxActors - Particles.size());
		const bool duplicate = ImGui::Button("Duplicate##particle");
		ImGui::EndDisabled();
		if (duplicate)
		{
			const xr_vector<s32> indices = SelectedParticles;
			ImGui::TreePop();
			ImGui::PopID();
			DuplicateParticles(indices);
			return;
		}
		controls.Item(CinemaButtonWidth("Delete"));
		if (ImGui::Button("Delete##particle"))
		{
			const xr_vector<s32> indices = SelectedParticles;
			ImGui::TreePop();
			ImGui::PopID();
			RemoveParticles(indices);
			return;
		}

		if (particle.Running && !particle.Started)
		{
			ImGui::TextDisabled("Waiting to start");
		}
		else
		{
			ImGui::TextDisabled(particle.Object->IsPlaying() ? "Playing" : "Not playing");
		}
		if (ImGui::TreeNodeEx("Timeline##particle", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_DrawLinesToNodes | ImGuiTreeNodeFlags_SpanAvailWidth))
		{
			const ImVec2 origin = ImGui::GetCursorScreenPos();
			const float width = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
			float time = particle.Time;
			DrawAnimationTimeline("##particle_timeline", time, particle.Out, nullptr, GetTheme(), nullptr, TrimDragMarker, TrimDragIndex, TrimDragKey, "", false);
			const float inX = origin.x + width * particle.In / particle.Out;
			ImGui::GetWindowDrawList()->AddLine(ImVec2(inX, origin.y), ImVec2(inX, origin.y + 64.0f), IM_COL32(120, 230, 150, 240), 2.0f);
			ImGui::TextDisabled("%.2f / %.2f s", particle.Time, particle.Out);
			if (ImGui::TreeNodeEx("Playback##particle", ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth))
			{
				bool changed = ImGui::DragFloat("IN (start delay)##particle", &particle.In, 0.05f, 0.0f, particle.Out - 0.01f, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
				changed |= ImGui::DragFloat("OUT (stop time)##particle", &particle.Out, 0.05f, particle.In + 0.01f, 600.0f, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
				if (changed)
				{
					StartParticleTimeline(particle);
				}
				ImGui::TextDisabled("IN starts the effect; OUT stops it. Play starts from zero.");
				ImGui::TextDisabled("Seeking and reverse playback are unavailable.");
				ImGui::TreePop();
			}
			if (ImGui::TreeNodeEx("Modulation##particle", ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth))
			{
				bool changed = false;
				if (ImGui::Button("Open curve editor##particle"))
				{
					CurveEditor = {};
					CurveEditor.Open = true;
					CurveEditor.Type = 3;
					CurveEditor.Object = SelectedParticle;
					CurveEditor.Channel = particle.CurveChannel;
				}
				const char* channels[] = { "Offset X", "Offset Y", "Offset Z", "Rotation H", "Rotation P", "Rotation B", "Size multiplier", "Speed multiplier" };
				if (ImGui::Combo("Parameter##particle_curve", &particle.CurveChannel, channels, IM_ARRAYSIZE(channels)))
				{
					particle.CurveDrag = -1;
				}
				ImGui::SameLine();
				if (ImGui::SmallButton("Reset##particle_curve_parameter"))
				{
					particle.CurveChannel = 0;
					particle.CurveDrag = -1;
				}
				auto& keys = particle.Curves[particle.CurveChannel];
				const float lower = particle.CurveChannel == 6 ? 0.1f : particle.CurveChannel == 7 ? 0.0f : -3600.0f;
				const float upper = particle.CurveChannel == 6 ? 5.0f : particle.CurveChannel == 7 ? 4.0f : 3600.0f;
				const float fallback = particle.CurveChannel >= 6 ? 1.0f : 0.0f;
				auto& wave = particle.Waves[particle.CurveChannel];
				const auto resetValue = [&](const char* id, auto& value, auto defaultValue)
				{
					ImGui::SameLine();
					ImGui::PushID(id);
					const bool reset = ImGui::SmallButton("Reset");
					ImGui::PopID();
					if (reset)
					{
						value = defaultValue;
						if (xr_strcmp(id, "time") == 0 || xr_strcmp(id, "value") == 0)
						{
							resetCurveKey = true;
						}
						else
						{
							resetWaveField = id;
						}
						changed = true;
					}
				};
				if (ImGui::Button("Reset channel##particle_modulation"))
				{
					keys.clear();
					resetChannel = true;
					wave = SParticleWave();
					wave.Minimum = lower;
					wave.Maximum = upper;
					particle.CurveDrag = -1;
					changed = true;
				}
				const char* waveTypes[] = { "None", "Sine", "Triangle", "Square", "Sawtooth", "White noise (stepped)", "Smooth noise", "Fractal noise", "Perlin noise", "White noise (continuous)", "Random oscillator" };
				changed |= ImGui::Combo("Wave##particle", &wave.Type, waveTypes, IM_ARRAYSIZE(waveTypes));
				if (wave.Type == 9 && ImGui::IsItemHovered()) ImGui::SetTooltip("Continuous band-limited approximation; Frequency sets the upper frequency in Hz.");
				resetValue("wave", wave.Type, 0);
				if (wave.Type != 0)
				{
					changed |= ImGui::DragFloat("Amplitude##particle_wave", &wave.Amplitude, 0.01f, 0.0f, 3600.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
					resetValue("amplitude", wave.Amplitude, 0.0f);
					changed |= ImGui::DragFloat("Frequency##particle_wave", &wave.Frequency, 0.01f, 0.0f, 100.0f, "%.2f Hz", ImGuiSliderFlags_AlwaysClamp);
					resetValue("frequency", wave.Frequency, 1.0f);
					changed |= ImGui::DragFloat("Phase##particle_wave", &wave.Phase, 0.01f, -100.0f, 100.0f, "%.2f cycles", ImGuiSliderFlags_AlwaysClamp);
					resetValue("phase", wave.Phase, 0.0f);
				}
				changed |= ImGui::DragFloat("Minimum##particle_wave", &wave.Minimum, 0.01f, lower, wave.Maximum, "%.2f", ImGuiSliderFlags_AlwaysClamp);
				resetValue("minimum", wave.Minimum, lower);
				wave.Minimum = std::isfinite(wave.Minimum) ? std::clamp(wave.Minimum, lower, wave.Maximum) : lower;
				changed |= ImGui::DragFloat("Maximum##particle_wave", &wave.Maximum, 0.01f, wave.Minimum, upper, "%.2f", ImGuiSliderFlags_AlwaysClamp);
				resetValue("maximum", wave.Maximum, upper);
				wave.Maximum = std::isfinite(wave.Maximum) ? std::clamp(wave.Maximum, wave.Minimum, upper) : upper;
				float minValue = particle.CurveChannel == 6 ? 0.1f : particle.CurveChannel == 7 ? 0.0f : -1.0f;
				float maxValue = particle.CurveChannel >= 6 ? 2.0f : 1.0f;
				for (const auto& key : keys)
				{
					minValue = std::min(minValue, key.Value);
					maxValue = std::max(maxValue, key.Value);
				}
				for (u32 sample = 0; sample <= 512; ++sample)
				{
					const float time = particle.Out * sample / 512.0f;
					const float value = ParticleModulatedValue(particle, particle.CurveChannel, time);
					const float base = ParticleCurveValue(keys, time, fallback);
					minValue = std::min(minValue, std::min(value, base));
					maxValue = std::max(maxValue, std::max(value, base));
				}
				const ImVec2 start = ImGui::GetCursorScreenPos();
				const float width = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
				const float height = 150.0f;
				ImGui::InvisibleButton("##particle_curve", ImVec2(width, height));
				ImDrawList* draw = ImGui::GetWindowDrawList();
				draw->AddRectFilled(start, ImVec2(start.x + width, start.y + height), GetTheme().TimelineBackground);
				draw->PushClipRect(start, ImVec2(start.x + width, start.y + height), true);
				DrawCinemaCurvePreviewGrid(draw, start, width, height, particle.Out, minValue, maxValue, GetTheme());
				const auto point = [&](float time, float value)
				{
					return ImVec2(start.x + std::clamp(time / particle.Out, 0.0f, 1.0f) * width, start.y + height - (value - minValue) / (maxValue - minValue) * height);
				};
				ImVec2 previous = point(0.0f, ParticleCurveValue(keys, 0.0f, fallback));
				for (u32 sample = 1; sample <= 512; ++sample)
				{
					const float time = particle.Out * sample / 512.0f;
					const ImVec2 current = point(time, ParticleCurveValue(keys, time, fallback));
					draw->AddLine(previous, current, GetTheme().TimelinePlayhead, 2.0f);
					previous = current;
				}
				for (const auto& key : keys)
				{
					if (key.Time <= particle.Out) draw->AddCircleFilled(point(key.Time, key.Value), 4.0f, GetTheme().TimelinePlayhead);
				}
				previous = point(0.0f, ParticleModulatedValue(particle, particle.CurveChannel, 0.0f));
				for (u32 sample = 1; sample <= 512; ++sample)
				{
					const float time = particle.Out * sample / 512.0f;
					const ImVec2 current = point(time, ParticleModulatedValue(particle, particle.CurveChannel, time));
					draw->AddLine(previous, current, IM_COL32(120, 230, 150, 240), 1.5f);
					previous = current;
				}
				draw->AddLine(point(particle.Time, minValue), point(particle.Time, maxValue), GetTheme().TimelineText);
				draw->PopClipRect();
				draw->AddRect(start, ImVec2(start.x + width, start.y + height), GetTheme().TimelineBorder);
				if (ImGui::IsItemHovered() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
				{
					const ImVec2 mouse = ImGui::GetIO().MousePos;
					float nearest = 64.0f;
					for (s32 k = 0; k < static_cast<s32>(keys.size()); ++k)
					{
						if (keys[k].Time > particle.Out)
						{
							continue;
						}
						const ImVec2 p = point(keys[k].Time, keys[k].Value);
						const float distance = (p.x - mouse.x) * (p.x - mouse.x) + (p.y - mouse.y) * (p.y - mouse.y);
						if (distance < nearest)
						{
							nearest = distance;
							particle.CurveDrag = k;
						}
					}
				}
				if (particle.CurveDrag >= 0 && particle.CurveDrag < static_cast<s32>(keys.size()) && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
				{
					const ImVec2 mouse = ImGui::GetIO().MousePos;
					auto& key = keys[particle.CurveDrag];
					const float left = particle.CurveDrag > 0 ? keys[particle.CurveDrag - 1].Time : 0.0f;
					const float right = particle.CurveDrag + 1 < static_cast<s32>(keys.size()) ? std::min(keys[particle.CurveDrag + 1].Time, particle.Out) : particle.Out;
					key.Time = std::clamp((mouse.x - start.x) / width * particle.Out, std::min(left, right), right);
					key.Value = std::clamp(maxValue - (mouse.y - start.y) / height * (maxValue - minValue), lower, upper);
					changed = true;
				}
				if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
				{
					particle.CurveDrag = -1;
				}
				if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) && keys.size() < 128)
				{
					const ImVec2 mouse = ImGui::GetIO().MousePos;
					keys.push_back({ std::clamp((mouse.x - start.x) / width * particle.Out, 0.0f, particle.Out), std::clamp(maxValue - (mouse.y - start.y) / height * (maxValue - minValue), lower, upper) });
					particle.CurveDrag = -1;
					changed = true;
				}
				ImGui::TextDisabled("Orange: keys. Green: result with wave and limits.");
				ImGui::TextDisabled("Double-click to add a key; drag keys or edit their values below.");
				for (s32 keyIndex = 0; keyIndex < static_cast<s32>(keys.size()); ++keyIndex)
				{
					ImGui::PushID(keyIndex);
					ImGui::SetNextItemWidth(100.0f);
					changed |= ImGui::DragFloat("Time##key", &keys[keyIndex].Time, 0.01f, 0.0f, 600.0f, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
					resetValue("time", keys[keyIndex].Time, 0.0f);
					ImGui::SameLine();
					ImGui::SetNextItemWidth(100.0f);
					changed |= ImGui::DragFloat("Value##key", &keys[keyIndex].Value, 0.01f, lower, upper, "%.2f", ImGuiSliderFlags_AlwaysClamp);
					resetValue("value", keys[keyIndex].Value, fallback);
					ImGui::SameLine();
					if (ImGui::SmallButton("Remove##key"))
					{
						keys.erase(keys.begin() + keyIndex);
						changed = true;
						ImGui::PopID();
						break;
					}
					ImGui::PopID();
				}
				if (changed)
				{
					std::stable_sort(keys.begin(), keys.end(), [](const auto& a, const auto& b) { return a.Time < b.Time; });
				}
				if (changed)
				{
					UpdateParticleTransform(particle);
				}
				ImGui::TreePop();
			}
			ImGui::TreePop();
		}
		if (ImGui::TreeNodeEx("Transform##particle", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
		{
			bool changed = ImGui::DragFloat3("Position##particle", &particle.Position.x, 0.01f, -100000.0f, 100000.0f, "%.2f");
			if (CinemaResetRequested())
			{
				particle.Position.set(0.0f, 0.0f, 0.0f);
				positionReset = true;
				changed = true;
			}
			Fvector degrees = {};
			particle.Orientation.getHPB(degrees);
			degrees.mul(rad2deg(1.0f));
			if (ImGui::DragFloat3("Rotation##particle", &degrees.x, 0.5f, -3600.0f, 3600.0f, "%.1f"))
			{
				particle.Orientation.setHPB(deg2rad(degrees.x), deg2rad(degrees.y), deg2rad(degrees.z));
				changed = true;
			}
			if (CinemaResetRequested())
			{
				particle.Orientation.setHPB(0.0f, 0.0f, 0.0f);
				rotationReset = true;
				changed = true;
			}
			ImGui::TextDisabled("H / P / B, degrees");
			if (ImGui::Button("Reset tilt##particle"))
			{
				const Fvector forward = particle.Orientation.k;
				resetTilt = true;
				particle.Orientation.setHPB(-atan2f(forward.x, forward.z), 0.0f, 0.0f);
				changed = true;
			}
			scaleChanged = ImGui::DragFloat("Scale##particle", &particle.Scale, 0.01f, 0.1f, 5.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
			changed |= scaleChanged;
			if (CinemaResetRequested())
			{
				particle.Scale = 1.0f;
				scaleChanged = true;
				changed = true;
			}
			ImGui::SameLine();
			if (ImGui::SmallButton("Reset##particle_scale"))
			{
				particle.Scale = 1.0f;
				scaleChanged = true;
				changed = true;
			}
			ImGui::TextDisabled("Uniform particle size; emitter shape is unchanged");
			if (changed)
			{
				UpdateParticleTransform(particle);
			}
			ImGui::TreePop();
		}
		Fvector oldRotation, newRotation;
		before.Orientation.getHPB(oldRotation);
		particle.Orientation.getHPB(newRotation);
		for (const s32 selected : SelectedParticles)
		{
			if (selected == index || selected < 0 || selected >= static_cast<s32>(Particles.size()))
			{
				continue;
			}
			auto& other = Particles[selected];
			bool refresh = speedChanged || positionReset || rotationReset || resetTilt || scaleChanged ||
				before.Position.x != particle.Position.x || before.Position.y != particle.Position.y || before.Position.z != particle.Position.z ||
				oldRotation.x != newRotation.x || oldRotation.y != newRotation.y || oldRotation.z != newRotation.z;
			if (loopChanged)
			{
				other.Loop = particle.Loop;
			}
			if (speedChanged)
			{
				other.Speed = particle.Speed;
			}
			if (positionReset || before.Position.x != particle.Position.x)
			{
				other.Position.x = particle.Position.x;
			}
			if (positionReset || before.Position.y != particle.Position.y)
			{
				other.Position.y = particle.Position.y;
			}
			if (positionReset || before.Position.z != particle.Position.z)
			{
				other.Position.z = particle.Position.z;
			}
			Fvector rotation;
			other.Orientation.getHPB(rotation);
			if (rotationReset || oldRotation.x != newRotation.x)
			{
				rotation.x = newRotation.x;
			}
			if (rotationReset || oldRotation.y != newRotation.y)
			{
				rotation.y = newRotation.y;
			}
			if (rotationReset || oldRotation.z != newRotation.z)
			{
				rotation.z = newRotation.z;
			}
			if (resetTilt)
			{
				const Fvector forward = other.Orientation.k;
				other.Orientation.setHPB(-atan2f(forward.x, forward.z), 0.0f, 0.0f);
			}
			else if (rotationReset || oldRotation.x != newRotation.x || oldRotation.y != newRotation.y || oldRotation.z != newRotation.z)
			{
				other.Orientation.setHPB(rotation.x, rotation.y, rotation.z);
			}
			if (scaleChanged)
			{
				other.Scale = particle.Scale;
			}
			const bool timeChanged = before.In != particle.In || before.Out != particle.Out;
			if (timeChanged)
			{
				if (before.In != particle.In)
				{
					other.In = particle.In;
				}
				if (before.Out != particle.Out)
				{
					other.Out = particle.Out;
				}
				if (other.In >= other.Out)
				{
					if (before.In != particle.In && before.Out == particle.Out)
					{
						other.Out = std::min(600.0f, other.In + 0.01f);
					}
					else
					{
						other.In = std::max(0.0f, other.Out - 0.01f);
					}
				}
			}
			for (u32 channel = 0; channel < 8; ++channel)
			{
				const auto& oldWave = before.Waves[channel];
				const auto& newWave = particle.Waves[channel];
				auto& wave = other.Waves[channel];
				refresh |= oldWave.Type != newWave.Type || oldWave.Amplitude != newWave.Amplitude || oldWave.Frequency != newWave.Frequency ||
					oldWave.Phase != newWave.Phase || oldWave.Minimum != newWave.Minimum || oldWave.Maximum != newWave.Maximum ||
					(channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || resetWaveField != nullptr));
				if (oldWave.Type != newWave.Type || (channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || (resetWaveField != nullptr && xr_strcmp(resetWaveField, "wave") == 0))))
				{
					wave.Type = newWave.Type;
				}
				if (oldWave.Amplitude != newWave.Amplitude || (channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || (resetWaveField != nullptr && xr_strcmp(resetWaveField, "amplitude") == 0))))
				{
					wave.Amplitude = newWave.Amplitude;
				}
				if (oldWave.Frequency != newWave.Frequency || (channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || (resetWaveField != nullptr && xr_strcmp(resetWaveField, "frequency") == 0))))
				{
					wave.Frequency = newWave.Frequency;
				}
				if (oldWave.Phase != newWave.Phase || (channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || (resetWaveField != nullptr && xr_strcmp(resetWaveField, "phase") == 0))))
				{
					wave.Phase = newWave.Phase;
				}
				if (oldWave.Minimum != newWave.Minimum || (channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || (resetWaveField != nullptr && xr_strcmp(resetWaveField, "minimum") == 0))))
				{
					wave.Minimum = newWave.Minimum;
					wave.Maximum = std::max(wave.Maximum, wave.Minimum);
				}
				if (oldWave.Maximum != newWave.Maximum || (channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || (resetWaveField != nullptr && xr_strcmp(resetWaveField, "maximum") == 0))))
				{
					wave.Maximum = newWave.Maximum;
					wave.Minimum = std::min(wave.Minimum, wave.Maximum);
				}
				const auto& oldKeys = before.Curves[channel];
				const auto& newKeys = particle.Curves[channel];
				const bool keysChanged = oldKeys.size() != newKeys.size() || !std::equal(oldKeys.begin(), oldKeys.end(), newKeys.begin(), newKeys.end(),
					[](const SParticleCurveKey& a, const SParticleCurveKey& b) { return a == b; });
				if (keysChanged || (channel == static_cast<u32>(particle.CurveChannel) && (resetChannel || resetCurveKey)))
				{
					other.Curves[channel] = newKeys;
					refresh = true;
				}
			}
			if (timeChanged)
			{
				StartParticleTimeline(other);
			}
			else if (refresh)
			{
				UpdateParticleTransform(other);
			}
		}
		ImGui::TreePop();
		ImGui::PopID();
	}
}

void CCinemaFilmMakerManager::TransformSelectedLights(const Fvector& previousPosition, const Fmatrix& previousOrientation)
{
	const auto& current = Lights[SelectedLight];
	Fmatrix inverse, rotation;
	inverse.invert(previousOrientation);
	rotation.mul(current.Orientation, inverse);
	for (const s32 index : SelectedLights)
	{
		if (index == SelectedLight)
		{
			continue;
		}
		auto& light = Lights[index];
		Fvector offset, rotated;
		offset.sub(light.Position, previousPosition);
		rotation.transform_dir(rotated, offset);
		light.Position.add(current.Position, rotated);
		Fmatrix orientation;
		orientation.mul(rotation, light.Orientation);
		light.Orientation = orientation;
		ApplyCinemaLight(light);
	}
}

void CCinemaFilmMakerManager::DuplicateLights(const xr_vector<s32>& indices)
{
	xr_vector<SCinemaLightData> copies;
	for (const s32 index : indices)
	{
		if (index >= 0 && index < static_cast<s32>(Lights.size()))
		{
			copies.push_back(Lights[index]);
		}
	}
	if (copies.empty())
	{
		return;
	}
	ClearGizmo();
	xr_vector<s32> created;
	bool failed = false;
	for (auto& data : copies)
	{
		SCinemaLight light;
		static_cast<SCinemaLightData&>(light) = std::move(data);
		light.Position.x += 1.0f;
		light.CurveDrag = -1;
		light.Light = ::Render->light_create();
		if (!light.Light)
		{
			failed = true;
			continue;
		}
		ApplyCinemaLight(light, true);
		Lights.push_back(std::move(light));
		created.push_back(static_cast<s32>(Lights.size()) - 1);
	}
	if (!created.empty())
	{
		SelectActor(-1);
		SelectedParticle = -1;
		SelectedParticles.clear();
		SelectedLights = std::move(created);
		SelectedLight = SelectedLights.back();
		LightSelectionAnchor = SelectedLight;
	}
	if (failed)
	{
		ShowSceneDialog(ESceneDialog::Info, "Duplicate lights", { "The renderer could not create one or more lights." });
	}
}

void CCinemaFilmMakerManager::RemoveLights(const xr_vector<s32>& indices)
{
	if (CurveEditor.Type == 2) CurveEditor.Open = false;
	MasterPatternEditorOpen = false;
	MasterClipSelection.clear();
	MasterClipAnchor = {};
	ClearGizmo();
	auto sorted = indices;
	std::sort(sorted.begin(), sorted.end());
	sorted.erase(std::unique(sorted.begin(), sorted.end()), sorted.end());
	for (auto it = sorted.rbegin(); it != sorted.rend(); ++it)
	{
		if (*it >= 0 && *it < static_cast<s32>(Lights.size()))
		{
			Lights.erase(Lights.begin() + *it);
		}
	}
	SelectedLight = -1;
	SelectedLights.clear();
	LightSelectionAnchor = -1;
	MasterSelectedLight = -1;
	MasterSelectedLightClip = -1;
}

template <typename Clip>
static s32 CinemaCurveClipIndex(const xr_vector<Clip>& clips, float time, s32 preferred, const bool* mutedTracks)
{
	if (preferred >= 0 && preferred < static_cast<s32>(clips.size())) return preferred;
	s32 selected = -1;
	for (s32 i = 0; i < static_cast<s32>(clips.size()); ++i)
	{
		if (CinemaMasterClipEnabled(clips[i], mutedTracks) && time >= clips[i].Start && time < clips[i].Start + clips[i].Duration &&
			(selected < 0 || clips[i].Start > clips[selected].Start)) selected = i;
	}
	return selected >= 0 ? selected : clips.empty() ? -1 : 0;
}

void CCinemaFilmMakerManager::DrawLightModulation(SCinemaLight& light)
{
	if (!ImGui::TreeNodeEx("Modulation##light", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		return;
	}
	const bool patternTimeline = light.MasterTrack && !light.MasterClips.empty();
	float curveDuration = light.Duration;
	bool loopChanged = false;
	bool durationChanged = false;
	bool playbackChanged = false;
	if (patternTimeline)
	{
		const s32 preferred = MasterPatternEditorOpen && MasterEditingPattern.Type == 2 && MasterEditingPattern.Object == SelectedLight ? MasterEditingPattern.Clip : -1;
		const s32 clipIndex = CinemaCurveClipIndex(light.MasterClips, MasterTime, preferred, MasterEffectiveTrackMuted);
		curveDuration = light.MasterClips[clipIndex].Duration;
		ImGui::TextDisabled("Duration and playback follow the timeline clip.");
	}
	else
	{
		loopChanged = ImGui::Checkbox("Loop##light_modulation", &light.Loop);
		durationChanged = ImGui::DragFloat("Duration##light", &light.Duration, 0.05f, 0.01f, 600.0f, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
		curveDuration = light.Duration;
		if (ImGui::Button(light.Running ? "Pause##light_modulation" : "Play##light_modulation"))
		{
			if (!light.Running && light.Time >= light.Duration) light.Time = 0.0f;
			light.Running = !light.Running;
			playbackChanged = true;
		}
		ImGui::SameLine();
		if (ImGui::Button("Restart##light_modulation"))
		{
			light.Time = 0.0f;
			light.Running = true;
			playbackChanged = true;
		}
		if (durationChanged) light.Time = std::min(light.Time, light.Duration);
	}
	ImGui::TextDisabled("%.2f / %.2f s", light.Time, curveDuration);
	ImGui::TextDisabled("Offsets are added; other channels multiply the base value.");
	if (ImGui::Button("Open curve editor##light"))
	{
		CurveEditor = {};
		CurveEditor.Open = true;
		CurveEditor.Type = 2;
		CurveEditor.Object = SelectedLight;
		CurveEditor.Channel = light.CurveChannel;
	}
	const char* channels[] = { "Offset X", "Offset Y", "Offset Z", "Rotation H", "Rotation P", "Rotation B", "Red multiplier", "Green multiplier", "Blue multiplier", "Range multiplier", "Cone multiplier", "Virtual size multiplier", "Volume quality multiplier", "Volume intensity multiplier", "Volume distance multiplier" };
	if (ImGui::Combo("Parameter##light_curve", &light.CurveChannel, channels, IM_ARRAYSIZE(channels)))
	{
		light.CurveDrag = -1;
	}
	const u32 channel = static_cast<u32>(light.CurveChannel);
	const float lower = channel < 6 ? -3600.0f : 0.0f;
	const float upper = channel < 6 ? 3600.0f : 10.0f;
	const float fallback = CinemaLightChannelDefault(channel);
	auto& wave = light.Waves[channel];
	auto& keys = light.Curves[channel];
	const SParticleWave previousWave = wave;
	const auto previousKeys = keys;
	bool changed = false;
	bool resetChannel = false;
	if (ImGui::Button("Reset channel##light_modulation"))
	{
		keys.clear();
		wave = SParticleWave();
		wave.Minimum = lower;
		wave.Maximum = upper;
		light.CurveDrag = -1;
		resetChannel = true;
		changed = true;
	}
	const char* waveTypes[] = { "None", "Sine", "Triangle", "Square", "Sawtooth", "White noise (stepped)", "Smooth noise", "Fractal noise", "Perlin noise", "White noise (continuous)", "Random oscillator" };
	changed |= ImGui::Combo("Wave##light", &wave.Type, waveTypes, IM_ARRAYSIZE(waveTypes));
	if (wave.Type == 9 && ImGui::IsItemHovered()) ImGui::SetTooltip("Continuous band-limited approximation; Frequency sets the upper frequency in Hz.");
	if (wave.Type != 0)
	{
		changed |= ImGui::DragFloat("Amplitude##light_wave", &wave.Amplitude, 0.01f, 0.0f, 3600.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
		changed |= ImGui::DragFloat("Frequency##light_wave", &wave.Frequency, 0.01f, 0.0f, 100.0f, "%.2f Hz", ImGuiSliderFlags_AlwaysClamp);
		changed |= ImGui::DragFloat("Phase##light_wave", &wave.Phase, 0.01f, -100.0f, 100.0f, "%.2f cycles", ImGuiSliderFlags_AlwaysClamp);
	}
	changed |= ImGui::DragFloat("Minimum##light_wave", &wave.Minimum, 0.01f, lower, wave.Maximum, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	wave.Minimum = std::isfinite(wave.Minimum) ? std::clamp(wave.Minimum, lower, wave.Maximum) : lower;
	changed |= ImGui::DragFloat("Maximum##light_wave", &wave.Maximum, 0.01f, wave.Minimum, upper, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	wave.Maximum = std::isfinite(wave.Maximum) ? std::clamp(wave.Maximum, wave.Minimum, upper) : upper;
	float minValue = channel < 6 ? -1.0f : 0.0f;
	float maxValue = channel < 6 ? 1.0f : 2.0f;
	for (const auto& key : keys)
	{
		minValue = std::min(minValue, key.Value);
		maxValue = std::max(maxValue, key.Value);
	}
	for (u32 i = 0; i <= 512; ++i)
	{
		const float time = curveDuration * i / 512.0f;
		const float value = LightModulatedValue(light, channel, time);
		const float base = ParticleCurveValue(keys, time, fallback);
		minValue = std::min(minValue, std::min(value, base));
		maxValue = std::max(maxValue, std::max(value, base));
	}
	const ImVec2 start = ImGui::GetCursorScreenPos();
	const float width = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
	const float height = 150.0f;
	ImGui::InvisibleButton("##light_curve", ImVec2(width, height));
	ImDrawList* draw = ImGui::GetWindowDrawList();
	draw->AddRectFilled(start, ImVec2(start.x + width, start.y + height), GetTheme().TimelineBackground);
	draw->PushClipRect(start, ImVec2(start.x + width, start.y + height), true);
	DrawCinemaCurvePreviewGrid(draw, start, width, height, curveDuration, minValue, maxValue, GetTheme());
	const auto point = [&](float time, float value)
	{
		return ImVec2(start.x + std::clamp(time / curveDuration, 0.0f, 1.0f) * width, start.y + height - (value - minValue) / (maxValue - minValue) * height);
	};
	ImVec2 last = point(0.0f, ParticleCurveValue(keys, 0.0f, fallback));
	for (u32 i = 1; i <= 512; ++i)
	{
		const float time = curveDuration * i / 512.0f;
		const ImVec2 current = point(time, ParticleCurveValue(keys, time, fallback));
		draw->AddLine(last, current, GetTheme().TimelinePlayhead, 2.0f);
		last = current;
	}
	for (const auto& key : keys)
	{
		if (key.Time <= curveDuration) draw->AddCircleFilled(point(key.Time, key.Value), 4.0f, GetTheme().TimelinePlayhead);
	}
	last = point(0.0f, LightModulatedValue(light, channel, 0.0f));
	for (u32 i = 1; i <= 512; ++i)
	{
		const float time = curveDuration * i / 512.0f;
		const ImVec2 current = point(time, LightModulatedValue(light, channel, time));
		draw->AddLine(last, current, IM_COL32(120, 230, 150, 240), 1.5f);
		last = current;
	}
	draw->AddLine(point(light.Time, minValue), point(light.Time, maxValue), GetTheme().TimelineText);
	draw->PopClipRect();
	draw->AddRect(start, ImVec2(start.x + width, start.y + height), GetTheme().TimelineBorder);
	if (ImGui::IsItemHovered() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		const ImVec2 mouse = ImGui::GetIO().MousePos;
		float nearest = 64.0f;
		for (s32 i = 0; i < static_cast<s32>(keys.size()); ++i)
		{
			if (keys[i].Time > curveDuration)
			{
				continue;
			}
			const ImVec2 p = point(keys[i].Time, keys[i].Value);
			const float distance = (p.x - mouse.x) * (p.x - mouse.x) + (p.y - mouse.y) * (p.y - mouse.y);
			if (distance < nearest)
			{
				nearest = distance;
				light.CurveDrag = i;
			}
		}
	}
	if (light.CurveDrag >= 0 && light.CurveDrag < static_cast<s32>(keys.size()) && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		const ImVec2 mouse = ImGui::GetIO().MousePos;
		auto& key = keys[light.CurveDrag];
		const float left = light.CurveDrag > 0 ? keys[light.CurveDrag - 1].Time : 0.0f;
		const float right = light.CurveDrag + 1 < static_cast<s32>(keys.size()) ? std::min(keys[light.CurveDrag + 1].Time, std::min(curveDuration, 600.0f)) : std::min(curveDuration, 600.0f);
		key.Time = std::clamp((mouse.x - start.x) / width * curveDuration, std::min(left, right), right);
		key.Value = std::clamp(maxValue - (mouse.y - start.y) / height * (maxValue - minValue), lower, upper);
		changed = true;
	}
	if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		light.CurveDrag = -1;
	}
	if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) && keys.size() < 128)
	{
		const ImVec2 mouse = ImGui::GetIO().MousePos;
		keys.push_back({ std::clamp((mouse.x - start.x) / width * curveDuration, 0.0f, std::min(curveDuration, 600.0f)), std::clamp(maxValue - (mouse.y - start.y) / height * (maxValue - minValue), lower, upper) });
		light.CurveDrag = -1;
		changed = true;
	}
	ImGui::TextDisabled("Orange: keys. Green: result with wave and limits.");
	ImGui::TextDisabled("Double-click to add a key; drag it or edit below.");
	for (s32 i = 0; i < static_cast<s32>(keys.size()); ++i)
	{
		ImGui::PushID(i);
		ImGui::SetNextItemWidth(100.0f);
		changed |= ImGui::DragFloat("Time##light_key", &keys[i].Time, 0.01f, 0.0f, 600.0f, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
		ImGui::SameLine();
		ImGui::SetNextItemWidth(100.0f);
		changed |= ImGui::DragFloat("Value##light_key", &keys[i].Value, 0.01f, lower, upper, "%.2f", ImGuiSliderFlags_AlwaysClamp);
		ImGui::SameLine();
		if (ImGui::SmallButton("Remove##light_key"))
		{
			keys.erase(keys.begin() + i);
			light.CurveDrag = -1;
			changed = true;
			ImGui::PopID();
			break;
		}
		ImGui::PopID();
	}
	if (changed)
	{
		std::stable_sort(keys.begin(), keys.end(), [](const auto& a, const auto& b) { return a.Time < b.Time; });
	}
	if (loopChanged || durationChanged || playbackChanged || changed)
	{
		for (const s32 index : SelectedLights)
		{
			auto& target = Lights[index];
			if (index != SelectedLight)
			{
				if (loopChanged)
				{
					target.Loop = light.Loop;
				}
				if (durationChanged)
				{
					target.Duration = light.Duration;
					target.Time = std::min(target.Time, target.Duration);
				}
				if (playbackChanged)
				{
					target.Running = light.Running;
					target.Time = std::min(light.Time, target.Duration);
				}
				if (changed)
				{
					auto& otherWave = target.Waves[channel];
					if (resetChannel || previousWave.Type != wave.Type)
					{
						otherWave.Type = wave.Type;
					}
					if (resetChannel || previousWave.Amplitude != wave.Amplitude)
					{
						otherWave.Amplitude = wave.Amplitude;
					}
					if (resetChannel || previousWave.Frequency != wave.Frequency)
					{
						otherWave.Frequency = wave.Frequency;
					}
					if (resetChannel || previousWave.Phase != wave.Phase)
					{
						otherWave.Phase = wave.Phase;
					}
					if (resetChannel || previousWave.Minimum != wave.Minimum)
					{
						otherWave.Minimum = wave.Minimum;
						otherWave.Maximum = std::max(otherWave.Maximum, otherWave.Minimum);
					}
					if (resetChannel || previousWave.Maximum != wave.Maximum)
					{
						otherWave.Maximum = wave.Maximum;
						otherWave.Minimum = std::min(otherWave.Minimum, otherWave.Maximum);
					}
					if (resetChannel || previousKeys.size() != keys.size() || !std::equal(previousKeys.begin(), previousKeys.end(), keys.begin(), keys.end(),
						[](const SParticleCurveKey& a, const SParticleCurveKey& b) { return a == b; }))
					{
						target.Curves[channel] = keys;
					}
				}
			}
			UpdateCinemaLightValues(target);
		}
	}
	ImGui::TreePop();
}

static float CinemaCurveGridStep(float span, float pixels)
{
	const float target = std::max(0.0001f, span * 80.0f / std::max(pixels, 1.0f));
	const float power = powf(10.0f, floorf(log10f(target)));
	const float scaled = target / power;
	return power * (scaled <= 1.0f ? 1.0f : scaled <= 2.0f ? 2.0f : scaled <= 5.0f ? 5.0f : 10.0f);
}

template <typename Evaluate>
static bool DrawCinemaCurveCanvas(SCinemaCurveEditorState& state, xr_vector<SParticleCurveKey>& keys, float duration, float currentTime, float seekLimit, float& seekTime, bool& seekChanged, float fallback, float lower, float upper, float maxKeyTime, const SEditorTheme& theme, bool discrete, Evaluate evaluate)
{
	const auto baseValue = [&](float time) { return discrete ? CinemaStepCurveValue(keys, time, fallback) : ParticleCurveValue(keys, time, fallback); };
	if (state.SelectedKey < 0 || state.SelectedKey >= static_cast<s32>(keys.size())) state.SelectedKey = -1;
	if (state.Fit)
	{
		state.TimeMin = 0.0f;
		state.TimeMax = std::max(0.01f, duration);
		state.ValueMin = std::min(0.0f, fallback);
		state.ValueMax = std::max(1.0f, fallback);
		for (const auto& key : keys)
		{
			state.TimeMax = std::max(state.TimeMax, key.Time);
			state.ValueMin = std::min(state.ValueMin, key.Value);
			state.ValueMax = std::max(state.ValueMax, key.Value);
		}
		for (u32 i = 0; i <= 128; ++i)
		{
			const float time = state.TimeMax * static_cast<float>(i) / 128.0f;
			const float value = evaluate(time);
			const float base = baseValue(time);
			if (std::isfinite(value) && std::isfinite(base))
			{
				state.ValueMin = std::min(state.ValueMin, std::min(value, base));
				state.ValueMax = std::max(state.ValueMax, std::max(value, base));
			}
		}
		const float padding = std::max(0.1f, (state.ValueMax - state.ValueMin) * 0.15f);
		state.ValueMin -= padding;
		state.ValueMax += padding;
		state.Fit = false;
	}
	const ImVec2 origin = ImGui::GetCursorScreenPos();
	const ImVec2 size(std::max(100.0f, ImGui::GetContentRegionAvail().x), std::max(140.0f, ImGui::GetContentRegionAvail().y));
	ImGui::InvisibleButton("##curve_canvas", size, ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonMiddle | ImGuiButtonFlags_MouseButtonRight);
	const bool hovered = ImGui::IsItemHovered();
	const bool active = ImGui::IsItemActive();
	const ImGuiIO& io = ImGui::GetIO();
	const float graphHeight = size.y - 20.0f;
	const float fx = std::clamp((io.MousePos.x - origin.x) / size.x, 0.0f, 1.0f);
	const float fy = std::clamp((origin.y + size.y - io.MousePos.y) / graphHeight, 0.0f, 1.0f);
	if (hovered && io.MouseWheel != 0.0f)
	{
		const float factor = powf(1.2f, -io.MouseWheel);
		const float minTimeSpan = std::min(2400.0f, std::max(0.001f, std::max(fabsf(state.TimeMin), fabsf(state.TimeMax)) * 0.000001f));
		const float minValueSpan = std::min(20000.0f, std::max(0.001f, std::max(fabsf(state.ValueMin), fabsf(state.ValueMax)) * 0.000001f));
		const float timeSpan = std::clamp((state.TimeMax - state.TimeMin) * factor, minTimeSpan, 2400.0f);
		const float valueSpan = std::clamp((state.ValueMax - state.ValueMin) * factor, minValueSpan, 20000.0f);
		const float anchorTime = state.TimeMin + fx * (state.TimeMax - state.TimeMin);
		const float anchorValue = state.ValueMin + fy * (state.ValueMax - state.ValueMin);
		state.TimeMin = std::max(0.0f, anchorTime - fx * timeSpan);
		state.TimeMax = state.TimeMin + timeSpan;
		state.ValueMin = anchorValue - fy * valueSpan;
		state.ValueMax = state.ValueMin + valueSpan;
	}
	if (active && ImGui::IsMouseDragging(ImGuiMouseButton_Middle))
	{
		const float dx = io.MouseDelta.x * (state.TimeMax - state.TimeMin) / size.x;
		const float dy = io.MouseDelta.y * (state.ValueMax - state.ValueMin) / graphHeight;
		const float timeSpan = state.TimeMax - state.TimeMin;
		const float valueSpan = state.ValueMax - state.ValueMin;
		state.TimeMin = std::clamp(state.TimeMin - dx, 0.0f, 10000.0f);
		state.TimeMax = state.TimeMin + timeSpan;
		state.ValueMin = std::clamp(state.ValueMin + dy, -10000.0f, 10000.0f);
		state.ValueMax = state.ValueMin + valueSpan;
	}
	const auto screen = [&](float time, float value)
	{
		return ImVec2(origin.x + (time - state.TimeMin) / (state.TimeMax - state.TimeMin) * size.x,
			origin.y + size.y - (value - state.ValueMin) / (state.ValueMax - state.ValueMin) * graphHeight);
	};
	const auto timeAt = [&](float x) { return state.TimeMin + (x - origin.x) / size.x * (state.TimeMax - state.TimeMin); };
	const auto valueAt = [&](float y) { return state.ValueMin + (origin.y + size.y - y) / graphHeight * (state.ValueMax - state.ValueMin); };
	const auto snap = [&](float value, float step) { return state.Snap && !io.KeyShift ? roundf(value / step) * step : value; };
	const auto handleTime = [&](s32 key, bool incoming)
	{
		const s32 neighbor = key + (incoming ? -1 : 1);
		if (neighbor < 0 || neighbor >= static_cast<s32>(keys.size())) return 0.0f;
		const float gap = fabsf(keys[neighbor].Time - keys[key].Time);
		return std::min(gap / 3.0f, (state.TimeMax - state.TimeMin) * 0.1f);
	};
	ImDrawList* draw = ImGui::GetWindowDrawList();
	draw->AddRectFilled(origin, ImVec2(origin.x + size.x, origin.y + size.y), theme.TimelineBackground);
	draw->AddRectFilled(origin, ImVec2(origin.x + size.x, origin.y + 20.0f), ImGui::GetColorU32(ImGuiCol_FrameBg));
	draw->PushClipRect(origin, ImVec2(origin.x + size.x, origin.y + size.y), true);
	const float timeStep = CinemaCurveGridStep(state.TimeMax - state.TimeMin, size.x);
	const float valueStep = CinemaCurveGridStep(state.ValueMax - state.ValueMin, graphHeight);
	const double firstTimeTick = ceil(static_cast<double>(state.TimeMin) / timeStep);
	for (u32 i = 0; i < 512; ++i)
	{
		const float t = static_cast<float>((firstTimeTick + i) * timeStep);
		if (t > state.TimeMax) break;
		const ImVec2 p = screen(t, 0.0f);
		draw->AddLine(ImVec2(p.x, origin.y + 20.0f), ImVec2(p.x, origin.y + size.y), theme.TimelineTickMinor);
		char label[32]; xr_sprintf(label, timeStep < 0.01f ? "%.4f" : timeStep < 1.0f ? "%.3f" : "%.2f", t);
		draw->AddText(ImVec2(p.x + 3.0f, origin.y + 2.0f), theme.TimelineText, label);
	}
	draw->PushClipRect(ImVec2(origin.x, origin.y + 20.0f), ImVec2(origin.x + size.x, origin.y + size.y), true);
	const double firstValueTick = ceil(static_cast<double>(state.ValueMin) / valueStep);
	for (u32 i = 0; i < 512; ++i)
	{
		const float value = static_cast<float>((firstValueTick + i) * valueStep);
		if (value > state.ValueMax) break;
		const ImVec2 p = screen(0.0f, value);
		draw->AddLine(ImVec2(origin.x, p.y), ImVec2(origin.x + size.x, p.y), theme.TimelineTickMinor);
		char label[32]; xr_sprintf(label, valueStep < 0.01f ? "%.4f" : valueStep < 1.0f ? "%.3f" : "%.2f", value);
		draw->AddText(ImVec2(origin.x + 3.0f, p.y + 2.0f), theme.TimelineText, label);
	}
	const ImVec2 zero = screen(0.0f, 0.0f);
	draw->AddLine(ImVec2(origin.x, zero.y), ImVec2(origin.x + size.x, zero.y), theme.TimelineBorder);
	draw->AddLine(ImVec2(zero.x, origin.y), ImVec2(zero.x, origin.y + size.y), theme.TimelineBorder);
	ImVec2 previous = screen(state.TimeMin, baseValue(state.TimeMin));
	ImVec2 result = screen(state.TimeMin, evaluate(state.TimeMin));
	for (u32 i = 1; i <= 512; ++i)
	{
		const float time = state.TimeMin + (state.TimeMax - state.TimeMin) * static_cast<float>(i) / 512.0f;
		const ImVec2 point = screen(time, baseValue(time));
		if (discrete) { draw->AddLine(previous, ImVec2(point.x, previous.y), theme.TimelinePlayhead, 2.0f); draw->AddLine(ImVec2(point.x, previous.y), point, theme.TimelinePlayhead, 2.0f); }
		else draw->AddLine(previous, point, theme.TimelinePlayhead, 2.0f);
		previous = point;
		if (state.ShowResult)
		{
			const ImVec2 next = screen(time, evaluate(time));
			if (discrete) { draw->AddLine(result, ImVec2(next.x, result.y), IM_COL32(120, 230, 150, 255), 1.5f); draw->AddLine(ImVec2(next.x, result.y), next, IM_COL32(120, 230, 150, 255), 1.5f); }
			else draw->AddLine(result, next, IM_COL32(120, 230, 150, 255), 1.5f);
			result = next;
		}
	}
	const float playhead = screen(currentTime, 0.0f).x;
	draw->AddLine(ImVec2(playhead, origin.y), ImVec2(playhead, origin.y + size.y), theme.TimelineText);
	s32 hoveredKey = -1;
	float nearest = 100.0f;
	for (s32 i = 0; i < static_cast<s32>(keys.size()); ++i)
	{
		const ImVec2 p = screen(keys[i].Time, keys[i].Value);
		const u32 highlighted = theme.AxisHighlighted;
		draw->AddCircleFilled(p, i == state.SelectedKey ? 6.0f : 4.0f, i == state.SelectedKey ? IM_COL32(color_get_R(highlighted), color_get_G(highlighted), color_get_B(highlighted), color_get_A(highlighted)) : theme.TimelinePlayhead);
		const float distance = (p.x - io.MousePos.x) * (p.x - io.MousePos.x) + (p.y - io.MousePos.y) * (p.y - io.MousePos.y);
		if (hovered && distance < nearest) { nearest = distance; hoveredKey = i; }
	}
	s32 hoveredHandle = 0;
	if (!discrete && state.SelectedKey >= 0)
	{
		const s32 k = state.SelectedKey;
		const ImVec2 point = screen(keys[k].Time, keys[k].Value);
		for (s32 part = 2; part <= 3; ++part)
		{
			const bool incoming = part == 2;
			if ((incoming && (k == 0 || !keys[k - 1].Smooth)) || (!incoming && (k + 1 >= static_cast<s32>(keys.size()) || !keys[k].Smooth))) continue;
			const float dt = handleTime(k, incoming);
			if (dt <= EPS_S) continue;
			const float sign = incoming ? -1.0f : 1.0f;
			const float slope = incoming ? keys[k].InSlope : keys[k].OutSlope;
			const ImVec2 handle = screen(keys[k].Time + sign * dt, keys[k].Value + sign * dt * slope);
			draw->AddLine(point, handle, theme.TimelineText);
			draw->AddCircleFilled(handle, 4.0f, theme.TimelineText);
			const float distance = (handle.x - io.MousePos.x) * (handle.x - io.MousePos.x) + (handle.y - io.MousePos.y) * (handle.y - io.MousePos.y);
			if (hovered && distance < 100.0f) hoveredHandle = part;
		}
	}
	draw->PopClipRect();
	if (playhead >= origin.x && playhead <= origin.x + size.x)
	{
		draw->AddTriangleFilled(ImVec2(playhead - 6.0f, origin.y), ImVec2(playhead + 6.0f, origin.y), ImVec2(playhead, origin.y + 11.0f), theme.TimelineText);
	}
	draw->PopClipRect();
	draw->AddRect(origin, ImVec2(origin.x + size.x, origin.y + size.y), theme.TimelineBorder);
	bool changed = false;
	if (hovered && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		if (io.MousePos.y < origin.y + 20.0f) state.DragPart = 4;
		else if (hoveredHandle) state.DragPart = hoveredHandle;
		else if (hoveredKey >= 0) { state.SelectedKey = hoveredKey; state.DragPart = 1; }
		else { state.SelectedKey = -1; state.DragPart = 0; }
	}
	if (hovered && io.MousePos.y >= origin.y + 20.0f && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) && hoveredKey < 0 && !hoveredHandle && keys.size() < 128)
	{
		SParticleCurveKey key;
		key.Time = std::clamp(snap(timeAt(io.MousePos.x), state.TimeStep), 0.0f, maxKeyTime);
		key.Value = std::clamp(snap(valueAt(io.MousePos.y), state.ValueStep), lower, upper);
		if (discrete) key.Value = roundf(key.Value);
		const auto at = std::upper_bound(keys.begin(), keys.end(), key.Time, [](float t, const SParticleCurveKey& k) { return t < k.Time; });
		state.SelectedKey = static_cast<s32>(at - keys.begin());
		keys.insert(at, key);
		state.DragPart = 0;
		changed = true;
	}
	if (active && state.DragPart == 4 && ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		seekTime = std::clamp(timeAt(io.MousePos.x), 0.0f, seekLimit);
		seekChanged = true;
	}
	if (active && state.DragPart > 0 && state.DragPart < 4 && state.SelectedKey >= 0 && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		auto& key = keys[state.SelectedKey];
		if (state.DragPart == 1)
		{
			const float minTime = state.SelectedKey > 0 ? keys[state.SelectedKey - 1].Time : 0.0f;
			const float maxTime = state.SelectedKey + 1 < static_cast<s32>(keys.size()) ? keys[state.SelectedKey + 1].Time : maxKeyTime;
			key.Time = std::clamp(snap(timeAt(io.MousePos.x), state.TimeStep), minTime, maxTime);
			key.Value = std::clamp(snap(valueAt(io.MousePos.y), state.ValueStep), lower, upper);
			if (discrete) key.Value = roundf(key.Value);
			changed = true;
		}
		else
		{
			const bool incoming = state.DragPart == 2;
			const float dt = handleTime(state.SelectedKey, incoming);
			if (dt > EPS_S)
			{
				const float slope = std::clamp((valueAt(io.MousePos.y) - key.Value) / (incoming ? -dt : dt), -3600.0f, 3600.0f);
				if (incoming) key.InSlope = slope; else key.OutSlope = slope;
				changed = true;
			}
		}
	}
	if (!ImGui::IsMouseDown(ImGuiMouseButton_Left)) state.DragPart = 0;
	if (hovered && hoveredKey >= 0 && ImGui::IsMouseReleased(ImGuiMouseButton_Right))
	{
		state.SelectedKey = hoveredKey;
		ImGui::OpenPopup("##curve_key_menu");
	}
	if (ImGui::BeginPopup("##curve_key_menu"))
	{
		if (ImGui::MenuItem("Remove key") && state.SelectedKey >= 0 && state.SelectedKey < static_cast<s32>(keys.size()))
		{
			keys.erase(keys.begin() + state.SelectedKey);
			state.SelectedKey = -1;
			state.DragPart = 0;
			changed = true;
		}
		ImGui::EndPopup();
	}
	return changed;
}

void CCinemaFilmMakerManager::DrawCurveEditor()
{
	if (!CurveEditor.Open) return;
	const bool light = CurveEditor.Type == 2;
	const bool particle = CurveEditor.Type == 3;
	const bool cameraShake = CurveEditor.Type == 4;
	const bool camera = CurveEditor.Type == 5;
	const s32 index = CurveEditor.Object;
	if ((light && (index < 0 || index >= static_cast<s32>(Lights.size()))) ||
		(particle && (index < 0 || index >= static_cast<s32>(Particles.size()))) ||
		(cameraShake && (index < 0 || index >= static_cast<s32>(Cameras.size()) || CurveEditor.Layer < 0 || CurveEditor.Layer >= static_cast<s32>(Cameras[index].Shakes.size()))) ||
		(camera && (index < 0 || index >= static_cast<s32>(Cameras.size()))) ||
		(!light && !particle && !cameraShake && !camera))
	{
		CurveEditor.Open = false;
		return;
	}
	const char* lightChannels[] = { "Offset X", "Offset Y", "Offset Z", "Rotation H", "Rotation P", "Rotation B", "Red multiplier", "Green multiplier", "Blue multiplier", "Range multiplier", "Cone multiplier", "Virtual size multiplier", "Volume quality multiplier", "Volume intensity multiplier", "Volume distance multiplier" };
	const char* particleChannels[] = { "Offset X", "Offset Y", "Offset Z", "Rotation H", "Rotation P", "Rotation B", "Size multiplier", "Speed multiplier" };
	const char* shakeChannels[] = { "Influence multiplier", "Position scale multiplier", "Speed multiplier", "Time offset (s)", "Enabled (0/1)", "Preset index", "Stretch (0/1)", "Fade in (s)", "Fade out (s)" };
	const char* cameraChannels[] = { "Position offset X", "Position offset Y", "Position offset Z", "Rotation offset H", "Rotation offset P", "Rotation offset B", "FOV (degrees)", "Stretch (0/1)", "Bezier (0/1)" };
	const s32 channelCount = light ? IM_ARRAYSIZE(lightChannels) : particle ? IM_ARRAYSIZE(particleChannels) : cameraShake ? IM_ARRAYSIZE(shakeChannels) : IM_ARRAYSIZE(cameraChannels);
	CurveEditor.Channel = std::clamp(CurveEditor.Channel, 0, channelCount - 1);
	ImGui::SetNextWindowSize(ImVec2(1050.0f, 650.0f), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints(ImVec2(520.0f, 320.0f), ImVec2(FLT_MAX, FLT_MAX));
	if (ImGui::Begin("Curve editor##cinema", &CurveEditor.Open))
	{
		if (ImGui::Button("Fit")) CurveEditor.Fit = true;
		ImGui::SameLine();
		ImGui::Checkbox("Snap", &CurveEditor.Snap);
		if (CurveEditor.Snap)
		{
			ImGui::SameLine(); ImGui::SetNextItemWidth(80.0f);
			ImGui::DragFloat("Time step", &CurveEditor.TimeStep, 0.01f, 0.001f, 600.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp);
			ImGui::SameLine(); ImGui::SetNextItemWidth(80.0f);
			ImGui::DragFloat("Value step", &CurveEditor.ValueStep, 0.01f, 0.001f, 3600.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp);
			CurveEditor.TimeStep = std::isfinite(CurveEditor.TimeStep) ? std::clamp(CurveEditor.TimeStep, 0.001f, 600.0f) : 0.1f;
			CurveEditor.ValueStep = std::isfinite(CurveEditor.ValueStep) ? std::clamp(CurveEditor.ValueStep, 0.001f, 3600.0f) : 0.1f;
		}
		ImGui::SameLine(); ImGui::Checkbox("Show result", &CurveEditor.ShowResult);
		CinemaHint("Drag the top ruler: seek | Double-click: add key | Drag: move key or tangent | Right-click: remove | Middle-drag: pan | Wheel: zoom | Shift: disable snap");
		ImGui::BeginChild("##curve_channels", ImVec2(190.0f, 0.0f), true);
		if (cameraShake) ImGui::Text("Camera %d, layer %d", index + 1, CurveEditor.Layer + 1);
		else if (camera) ImGui::Text("Camera %d", index + 1);
		else ImGui::Text(light ? "Light %d" : "Particle %d", index + 1);
		ImGui::TextUnformatted("Channels");
		for (s32 channel = 0; channel < channelCount; ++channel)
		{
			const char* name = light ? lightChannels[channel] : particle ? particleChannels[channel] : cameraShake ? shakeChannels[channel] : cameraChannels[channel];
			if (ImGui::Selectable(name, CurveEditor.Channel == channel))
			{
				CurveEditor.Channel = channel;
				CurveEditor.SelectedKey = -1;
				CurveEditor.DragPart = 0;
				CurveEditor.Fit = true;
			}
		}
		ImGui::EndChild();
		ImGui::SameLine();
		ImGui::BeginChild("##curve_content", ImVec2(0.0f, 0.0f), true);
		const s32 channel = CurveEditor.Channel;
		auto& keys = light ? Lights[index].Curves[channel] : particle ? Particles[index].Curves[channel] : cameraShake ? Cameras[index].Shakes[CurveEditor.Layer].Curves[channel] : Cameras[index].Curves[channel];
		float duration = light ? Lights[index].Duration : particle ? Particles[index].Out : Cameras[index].Duration;
		float currentTime = light ? Lights[index].Time : particle ? Particles[index].Time : std::clamp(MasterTime - Cameras[index].Start, 0.0f, duration);
		bool hasMasterClip = false;
		float clipStart = 0.0f;
		float clipDuration = 0.0f;
		const auto chooseClip = [&](const auto& clips)
		{
			const s32 preferred = MasterPatternEditorOpen && MasterEditingPattern.Type == CurveEditor.Type && MasterEditingPattern.Object == index ? MasterEditingPattern.Clip : -1;
			const s32 selectedClip = CinemaCurveClipIndex(clips, MasterTime, preferred, MasterEffectiveTrackMuted);
			if (selectedClip < 0) return;
			clipStart = clips[selectedClip].Start;
			clipDuration = clips[selectedClip].Duration;
			hasMasterClip = true;
		};
		if (light && Lights[index].MasterTrack) chooseClip(Lights[index].MasterClips);
		else if (particle && Particles[index].MasterTrack) chooseClip(Particles[index].MasterClips);
		else if (cameraShake || camera)
		{
			clipStart = Cameras[index].Start;
			clipDuration = Cameras[index].Duration;
			hasMasterClip = true;
		}
		if (hasMasterClip)
		{
			const float local = std::clamp(MasterTime - clipStart, 0.0f, clipDuration);
			if (light || cameraShake || camera)
			{
				duration = clipDuration;
				currentTime = local;
			}
			else currentTime = std::min(local, duration);
		}
		const bool discrete = (camera && channel >= 7) || (cameraShake && channel >= 4 && channel <= 6);
		const float maxKeyTime = cameraShake || camera ? 3600.0f : 600.0f;
		const float cameraFallback = camera ? (channel < 6 ? 0.0f : channel == 6 ? (Cameras[index].Fov > 0.0f ? Cameras[index].Fov : std::isfinite(Device.fFOV) ? std::clamp(Device.fFOV, 1.0f, 179.0f) : 90.0f) : channel == 7 ? (Cameras[index].Stretch ? 1.0f : 0.0f) : (Cameras[index].Smooth ? 1.0f : 0.0f)) : 0.0f;
		const float shakeFallback = cameraShake ? (channel < 3 ? 1.0f : channel == 3 ? 0.0f : channel == 4 ? (Cameras[index].Shakes[CurveEditor.Layer].Enabled ? 1.0f : 0.0f) : channel == 5 ? static_cast<float>(Cameras[index].Shakes[CurveEditor.Layer].Preset) : channel == 6 ? (Cameras[index].Shakes[CurveEditor.Layer].Stretch ? 1.0f : 0.0f) : channel == 7 ? Cameras[index].Shakes[CurveEditor.Layer].FadeIn : Cameras[index].Shakes[CurveEditor.Layer].FadeOut) : 0.0f;
		const float fallback = light ? CinemaLightChannelDefault(channel) : particle ? (channel >= 6 ? 1.0f : 0.0f) : camera ? cameraFallback : shakeFallback;
		const float lower = light ? (channel < 6 ? -3600.0f : 0.0f) : particle ? (channel == 6 ? 0.1f : channel == 7 ? 0.0f : -3600.0f) : camera ? (channel < 6 ? -3600.0f : channel == 6 ? 1.0f : 0.0f) : (channel == 3 ? -3600.0f : 0.0f);
		const float upper = light ? (channel < 6 ? 3600.0f : 10.0f) : particle ? (channel == 6 ? 5.0f : channel == 7 ? 4.0f : 3600.0f) : camera ? (channel < 6 ? 3600.0f : channel == 6 ? 179.0f : 1.0f) : channel == 3 || channel >= 7 ? 3600.0f : channel == 5 ? static_cast<float>(IM_ARRAYSIZE(CinemaShakePresets) - 1) : channel == 4 || channel == 6 ? 1.0f : 10.0f;
		if (!CurveEditor.InitializedChannels[channel])
		{
			CurveEditor.InitializedChannels[channel] = true;
			if (keys.empty())
			{
				keys.push_back({ 0.0f, std::clamp(discrete ? roundf(fallback) : fallback, lower, upper) });
				CurveEditor.SelectedKey = 0;
				CurveEditor.Fit = true;
			}
		}
		auto evaluate = [&](float time)
		{
			if (light) return LightModulatedValue(Lights[index], channel, time);
			if (particle) return ParticleModulatedValue(Particles[index], channel, time);
			if (camera) return discrete ? CinemaStepCurveValue(keys, time, fallback) : ParticleCurveValue(keys, time, fallback);
			const auto& shake = Cameras[index].Shakes[CurveEditor.Layer];
			const float value = discrete ? CinemaStepCurveValue(keys, time, fallback) : ParticleCurveValue(keys, time, fallback);
			if (channel == 0)
			{
				float result = shake.Influence * std::clamp(value, 0.0f, 10.0f);
				const float fadeIn = std::clamp(ParticleCurveValue(shake.Curves[7], time, shake.FadeIn), 0.0f, 3600.0f);
				const float fadeOut = std::clamp(ParticleCurveValue(shake.Curves[8], time, shake.FadeOut), 0.0f, 3600.0f);
				if (fadeIn > 0.0f) result *= std::clamp(time / fadeIn, 0.0f, 1.0f);
				if (fadeOut > 0.0f) result *= std::clamp((duration - time) / fadeOut, 0.0f, 1.0f);
				return result;
			}
			if (channel == 1) return shake.PositionScale * std::clamp(value, 0.0f, 10.0f);
			if (channel == 2) return shake.Speed * CinemaShakeSpeedValue(keys, time);
			if (channel == 3) return shake.Offset + std::clamp(value, -3600.0f, 3600.0f);
			return value;
		};
		bool changed = false;
		ImGui::Text("%s | %u / 128 keys", light ? lightChannels[channel] : particle ? particleChannels[channel] : cameraShake ? shakeChannels[channel] : cameraChannels[channel], static_cast<u32>(keys.size()));
		if (discrete) ImGui::TextDisabled("Step values; changes occur at key times.");
		if (cameraShake && channel == 5)
		{
			for (s32 i = 0; i < static_cast<s32>(IM_ARRAYSIZE(CinemaShakePresets)); ++i) ImGui::TextDisabled("%d: %s", i, CinemaShakePresets[i].Name);
		}
		if (ImGui::Button("Add key") && keys.size() < 128)
		{
			const float time = std::clamp(currentTime, 0.0f, maxKeyTime);
			const float value = std::clamp(discrete ? roundf(CinemaStepCurveValue(keys, time, fallback)) : ParticleCurveValue(keys, time, fallback), lower, upper);
			const auto at = std::upper_bound(keys.begin(), keys.end(), time, [](float t, const SParticleCurveKey& k) { return t < k.Time; });
			CurveEditor.SelectedKey = static_cast<s32>(at - keys.begin());
			keys.insert(at, { time, value });
			changed = true;
		}
		ImGui::SameLine();
		ImGui::BeginDisabled(CurveEditor.SelectedKey < 0 || CurveEditor.SelectedKey >= static_cast<s32>(keys.size()));
		if (ImGui::Button("Remove key"))
		{
			keys.erase(keys.begin() + CurveEditor.SelectedKey);
			CurveEditor.SelectedKey = -1;
			changed = true;
		}
		ImGui::EndDisabled();
		if (CurveEditor.SelectedKey >= 0 && CurveEditor.SelectedKey < static_cast<s32>(keys.size()))
		{
			const s32 k = CurveEditor.SelectedKey;
			auto& key = keys[k];
			ImGui::SetNextItemWidth(90.0f);
			if (ImGui::DragFloat("Time##curve", &key.Time, 0.01f, k > 0 ? keys[k - 1].Time : 0.0f, k + 1 < static_cast<s32>(keys.size()) ? keys[k + 1].Time : maxKeyTime, "%.3f", ImGuiSliderFlags_AlwaysClamp))
			{
				key.Time = std::isfinite(key.Time) ? std::clamp(key.Time, k > 0 ? keys[k - 1].Time : 0.0f, k + 1 < static_cast<s32>(keys.size()) ? keys[k + 1].Time : maxKeyTime) : (k > 0 ? keys[k - 1].Time : 0.0f);
				changed = true;
			}
			ImGui::SameLine(); ImGui::SetNextItemWidth(90.0f);
			if (ImGui::DragFloat("Value##curve", &key.Value, discrete ? 1.0f : 0.01f, lower, upper, "%.3f", ImGuiSliderFlags_AlwaysClamp))
			{
				key.Value = std::isfinite(key.Value) ? std::clamp(key.Value, lower, upper) : fallback;
				if (discrete) key.Value = roundf(key.Value);
				changed = true;
			}
			ImGui::SameLine();
			ImGui::BeginDisabled(discrete);
			if (ImGui::Checkbox("Smooth outgoing", &key.Smooth))
			{
				if (key.Smooth && k + 1 < static_cast<s32>(keys.size()) && keys[k + 1].Time > key.Time)
				{
					key.OutSlope = std::clamp((keys[k + 1].Value - key.Value) / (keys[k + 1].Time - key.Time), -3600.0f, 3600.0f);
					keys[k + 1].InSlope = key.OutSlope;
				}
				changed = true;
			}
			ImGui::SetNextItemWidth(100.0f);
			if (ImGui::DragFloat("In slope", &key.InSlope, 0.01f, -3600.0f, 3600.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp))
			{
				key.InSlope = std::isfinite(key.InSlope) ? std::clamp(key.InSlope, -3600.0f, 3600.0f) : 0.0f;
				changed = true;
			}
			ImGui::SameLine(); ImGui::SetNextItemWidth(100.0f);
			if (ImGui::DragFloat("Out slope", &key.OutSlope, 0.01f, -3600.0f, 3600.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp))
			{
				key.OutSlope = std::isfinite(key.OutSlope) ? std::clamp(key.OutSlope, -3600.0f, 3600.0f) : 0.0f;
				changed = true;
			}
			ImGui::EndDisabled();
		}
		else ImGui::TextDisabled("Select a key to edit its time, value and tangents.");
		float seekTime = currentTime;
		bool seekChanged = false;
		const float seekLimit = hasMasterClip ? std::max(0.0f, std::min(duration, std::min(clipDuration, MasterDuration - clipStart))) : duration;
		changed |= DrawCinemaCurveCanvas(CurveEditor, keys, std::max(0.01f, duration), currentTime, seekLimit, seekTime, seekChanged, fallback, lower, upper, maxKeyTime, GetTheme(), discrete, evaluate);
		if (seekChanged)
		{
			if (hasMasterClip)
			{
				StopPatternPreviews();
				MasterTime = std::clamp(clipStart + seekTime, 0.0f, MasterDuration);
				const float clipEnd = clipStart + clipDuration;
				if (seekTime >= clipDuration && clipEnd > clipStart && clipEnd <= MasterDuration && MasterTime >= clipEnd)
				{
					MasterTime = std::nextafter(clipEnd, clipStart);
				}
				MasterActive = true;
				MasterIncludeStart = true;
				MasterParticleSeek = true;
			}
			else if (light)
			{
				Lights[index].Time = seekTime;
				UpdateCinemaLightValues(Lights[index]);
			}
			else
			{
				Particles[index].Time = seekTime;
				UpdateParticleTransform(Particles[index]);
			}
		}
		if (changed)
		{
			if (light)
			{
				for (const s32 selected : SelectedLights)
				{
					if (selected >= 0 && selected < static_cast<s32>(Lights.size()) && selected != index && SelectedLight == index) Lights[selected].Curves[channel] = keys;
				}
				UpdateCinemaLightValues(Lights[index]);
				if (SelectedLight == index) for (const s32 selected : SelectedLights) if (selected >= 0 && selected < static_cast<s32>(Lights.size()) && selected != index) UpdateCinemaLightValues(Lights[selected]);
			}
			else if (particle)
			{
				for (const s32 selected : SelectedParticles)
				{
					if (selected >= 0 && selected < static_cast<s32>(Particles.size()) && selected != index && SelectedParticle == index) Particles[selected].Curves[channel] = keys;
				}
				UpdateParticleTransform(Particles[index]);
				if (SelectedParticle == index) for (const s32 selected : SelectedParticles) if (selected >= 0 && selected < static_cast<s32>(Particles.size()) && selected != index) UpdateParticleTransform(Particles[selected]);
			}
		}
		ImGui::EndChild();
	}
	ImGui::End();
}

void CCinemaFilmMakerManager::DrawLightTab()
{
	ImGui::SeparatorText("Scene lights");
	ImGui::TextDisabled("%u lights", static_cast<u32>(Lights.size()));
	if (ImGui::Button("Add light"))
	{
		SCinemaLight light;
		light.Position = Device.vCameraPosition;
		Fvector direction = Device.vCameraDirection;
		if (!SpawnFromCamera && g_actor != nullptr)
		{
			light.Position = g_actor->Position();
			direction = g_actor->Direction();
		}
		direction.normalize_safe();
		light.Position.mad(direction, 3.0f);
		light.Light = ::Render->light_create();
		if (light.Light)
		{
			ApplyCinemaLight(light, true);
			SelectActor(-1);
			SelectedParticle = -1;
			SelectedParticles.clear();
			ClearGizmo();
			Lights.push_back(std::move(light));
			SelectedLight = static_cast<s32>(Lights.size()) - 1;
			SelectedLights = { SelectedLight };
			LightSelectionAnchor = SelectedLight;
		}
		else
		{
			ShowSceneDialog(ESceneDialog::Info, "Add light", { "The renderer could not create a light." });
		}
	}
	if (Lights.empty())
	{
		CinemaHint("No lights yet. Choose Add light to place one in the scene.");
		return;
	}
	CinemaHint("Shift + click: select range | Ctrl + click: toggle");
	s32 clicked = -1;
	s32 remove = -1;
	s32 duplicate = -1;
	xr_vector<s32> visibleLights;
	const auto drawLight = [&](s32 i)
	{
		visibleLights.push_back(i);
		string64 label = {};
		xr_sprintf(label, "Light %d (%s)##light%d", i + 1, Lights[i].Type == 0 ? "Point" : "Spot", i);
		const bool selected = std::find(SelectedLights.begin(), SelectedLights.end(), i) != SelectedLights.end();
		if (ImGui::Selectable(label, selected))
		{
			clicked = i;
		}
		if (ImGui::BeginPopupContextItem())
		{
			if (ImGui::MenuItem("Duplicate"))
			{
				duplicate = i;
			}
			if (ImGui::MenuItem("Remove"))
			{
				remove = i;
			}
			ImGui::EndPopup();
		}
	};
	for (s32 i = 0; i < static_cast<s32>(Lights.size()); ++i) drawLight(i);
	if (duplicate >= 0)
	{
		const bool selected = std::find(SelectedLights.begin(), SelectedLights.end(), duplicate) != SelectedLights.end();
		DuplicateLights(selected ? SelectedLights : xr_vector<s32>{ duplicate });
		return;
	}
	if (remove >= 0)
	{
		const bool selected = std::find(SelectedLights.begin(), SelectedLights.end(), remove) != SelectedLights.end();
		RemoveLights(selected ? SelectedLights : xr_vector<s32>{ remove });
		return;
	}
	if (clicked >= 0)
	{
		if (SelectedLight < 0)
		{
			SelectActor(-1);
			SelectedParticle = -1;
			SelectedParticles.clear();
		}
		ClearGizmo();
		const auto& io = ImGui::GetIO();
		const auto first = std::find(visibleLights.begin(), visibleLights.end(), LightSelectionAnchor);
		const auto last = std::find(visibleLights.begin(), visibleLights.end(), clicked);
		if (io.KeyShift && first != visibleLights.end() && last != visibleLights.end())
		{
			SelectedLights.assign(std::min(first, last), std::max(first, last) + 1);
		}
		else if (io.KeyCtrl)
		{
			const auto it = std::find(SelectedLights.begin(), SelectedLights.end(), clicked);
			if (it == SelectedLights.end())
			{
				SelectedLights.push_back(clicked);
			}
			else
			{
				SelectedLights.erase(it);
			}
			LightSelectionAnchor = clicked;
		}
		else
		{
			SelectedLights = { clicked };
			LightSelectionAnchor = clicked;
		}
		SelectedLight = std::find(SelectedLights.begin(), SelectedLights.end(), clicked) != SelectedLights.end()
			? clicked : (SelectedLights.empty() ? -1 : SelectedLights.back());
	}
	if (SelectedLight < 0 || SelectedLight >= static_cast<s32>(Lights.size()))
	{
		ImGui::TextDisabled("Select a light to edit it.");
		return;
	}
	if (SelectedLights.size() > 1)
	{
		ImGui::TextDisabled("%u selected. Edits apply to all selected lights.", static_cast<u32>(SelectedLights.size()));
	}
	ImGui::SeparatorText("Selected light");
	SToolbarLayout lightActions(ImGui::GetContentRegionAvail().x);
	lightActions.Item(CinemaButtonWidth("Duplicate"));
	if (ImGui::Button("Duplicate##light"))
	{
		DuplicateLights(SelectedLights);
		return;
	}
	lightActions.Item(CinemaButtonWidth("Remove"));
	if (ImGui::Button("Remove##light"))
	{
		RemoveLights(SelectedLights);
		return;
	}
	auto& light = Lights[SelectedLight];
	const SCinemaLightData previous = light;
	bool textureChanged = false;
	bool changed = false;
	bool colorChanged = false;
	bool positionChanged = false;
	bool rotationChanged = false;
	if (ImGui::TreeNodeEx("Light settings##light", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		changed |= ImGui::Combo("Type", &light.Type, "Point\0Spot\0");
		changed |= ImGui::Checkbox("Enabled", &light.Active);
		changed |= ImGui::Checkbox("Shadows", &light.Shadow);
		const float colorWidth = ImGui::GetContentRegionAvail().x - ImGui::CalcTextSize("Color").x - CinemaButtonWidth("Pick") - ImGui::GetStyle().ItemInnerSpacing.x - ImGui::GetStyle().ItemSpacing.x * 2.0f;
		ImGui::SetNextItemWidth(std::max(40.0f, colorWidth));
		colorChanged = ImGui::ColorEdit3("Color", &light.Color.x, ImGuiColorEditFlags_Float | ImGuiColorEditFlags_HDR);
		ImGui::SameLine();
		if (ImGui::Button("Pick##light_color"))
		{
			LightColorPicking = true;
			LightColorPickingTarget = SelectedLight;
			ClearGizmo();
		}
		if (ImGui::IsItemHovered()) ImGui::SetTooltip("Pick a color from the game scene.");
		changed |= colorChanged;
		changed |= ImGui::SliderFloat("Intensity", &light.Intensity, 0.0f, 100.0f, "%.2f", ImGuiSliderFlags_Logarithmic | ImGuiSliderFlags_AlwaysClamp);
		changed |= ImGui::DragFloat("Range", &light.Range, 0.1f, 0.1f, 1000.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
		changed |= ImGui::DragFloat("Virtual size", &light.VirtualSize, 0.01f, 0.0f, 100.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
		if (light.Type == 1)
		{
			changed |= ImGui::DragFloat("Cone angle", &light.Cone, 0.5f, 1.0f, 120.0f, "%.1f deg", ImGuiSliderFlags_AlwaysClamp);
			if (ImGui::InputText("Projection texture", light.Texture, sizeof(light.Texture), ImGuiInputTextFlags_EnterReturnsTrue))
			{
				textureChanged = true;
			}
			const auto filterTextures = [&]()
			{
				LightTextureFiltered.clear();
				for (s32 i = 0; i < static_cast<s32>(LightTextureFiles.size()); ++i)
				{
					if (CinemaContainsFilter(LightTextureFiles[i].c_str(), LightTextureFilter)) LightTextureFiltered.push_back(i);
				}
			};
			if (ImGui::Button("Browse textures##light"))
			{
				if (!LightTextureFilesBuilt)
				{
					FS_FileSet files;
					FS.file_list(files, "$textures$", FS_ListFiles | FS_ClampExt, "*.dds");
					LightTextureFiles.clear();
					LightTextureFiles.reserve(files.size());
					for (const FS_File& file : files) LightTextureFiles.emplace_back(file.name.c_str());
					LightTextureFilesBuilt = true;
				}
				filterTextures();
				ImGui::OpenPopup("##light_texture_browser");
			}
			if (ImGui::IsPopupOpen("##light_texture_browser")) ImGui::SetNextWindowSize(ImVec2(500.0f, 400.0f), ImGuiCond_FirstUseEver);
			if (ImGui::BeginPopup("##light_texture_browser", ImGuiWindowFlags_AlwaysAutoResize))
			{
				ImGui::TextUnformatted("Projection textures");
				ImGui::SetNextItemWidth(-1.0f);
				if (ImGui::InputTextWithHint("##light_texture_filter", "Search textures", LightTextureFilter, sizeof(LightTextureFilter))) filterTextures();
				if (ImGui::Selectable("None##light_texture", light.Texture[0] == '\0'))
				{
					light.Texture[0] = '\0';
					textureChanged = true;
					ImGui::CloseCurrentPopup();
				}
				ImGui::TextDisabled("%u textures", static_cast<u32>(LightTextureFiltered.size()));
				ImGui::BeginChild("##light_texture_list", ImVec2(460.0f, 300.0f), true, ImGuiWindowFlags_HorizontalScrollbar);
				ImGuiListClipper clipper;
				clipper.Begin(static_cast<int>(LightTextureFiltered.size()));
				while (clipper.Step())
				{
					for (int row = clipper.DisplayStart; row < clipper.DisplayEnd; ++row)
					{
						const s32 textureIndex = LightTextureFiltered[row];
						const char* name = LightTextureFiles[textureIndex].c_str();
						const bool fits = xr_strlen(name) < sizeof(light.Texture);
						ImGui::PushID(textureIndex);
						ImGui::BeginDisabled(!fits);
						if (ImGui::Selectable(name, xr_strcmp(light.Texture, name) == 0))
						{
							xr_strcpy(light.Texture, name);
							textureChanged = true;
							ImGui::CloseCurrentPopup();
						}
						ImGui::EndDisabled();
						if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled)) ImGui::SetTooltip("%s%s", name, fits ? "" : "\nPath is too long for a light texture.");
						ImGui::PopID();
					}
				}
				ImGui::EndChild();
				ImGui::EndPopup();
			}
			ImGui::TextDisabled("Press Enter to apply a typed texture. Shadowed spots only.");
		}
		changed |= ImGui::Checkbox("Volumetric", &light.Volumetric);
		if (light.Volumetric)
		{
			changed |= ImGui::DragFloat("Volume quality", &light.VolumeQuality, 0.01f, 0.01f, 1.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
			changed |= ImGui::DragFloat("Volume intensity", &light.VolumeIntensity, 0.01f, 0.0f, 100.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
			changed |= ImGui::DragFloat("Volume distance", &light.VolumeDistance, 0.01f, 0.01f, 1.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
			if (ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("The renderer uses a minimum distance of 0.01.");
			}
		}
		changed |= ImGui::Checkbox("Indirect", &light.Indirect);
		changed |= ImGui::Checkbox("HUD mode", &light.HudMode);
		changed |= ImGui::Checkbox("Occlusion queries", &light.Occlusion);
		ImGui::TreePop();
	}
	if (LightColorPicking && LightColorPickingTarget == SelectedLight)
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		const ImVec2 mouse = ImGui::GetIO().MousePos;
		ImGui::GetForegroundDrawList(ImGui::GetMainViewport())->AddText(ImVec2(mouse.x + 16.0f, mouse.y + 16.0f), ImGui::GetColorU32(ImGuiCol_Text), "Pick scene color | Esc / right-click: cancel");
		if (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right))
		{
			LightColorPicking = false;
		}
		else if (ImGui::IsMouseClicked(ImGuiMouseButton_Left) && !ImGui::IsWindowHovered(ImGuiHoveredFlags_AnyWindow))
		{
			LightColorPicking = false;
			const ImGuiViewport* viewport = ImGui::GetMainViewport();
			const float x = (mouse.x - viewport->Pos.x) / viewport->Size.x;
			const float y = (mouse.y - viewport->Pos.y) / viewport->Size.y;
			bool sampled = false;
			if (x >= 0.0f && x < 1.0f && y >= 0.0f && y < 1.0f && GRHI && GRHI->DevicePtr && GRHI->DevicePtr->SwapChainRTV)
			{
				const size_t capacity = (static_cast<size_t>(Device.TargetWidth) + 64) * (static_cast<size_t>(Device.TargetHeight) + 64) * 4;
				if (capacity <= static_cast<size_t>(u32(-1)))
				{
					xr_vector<u8> pixels(capacity);
					u32 width = 0, height = 0, pitch = 0;
					if (GRHI->DevicePtr->ReadRenderTargetPixels(GRHI->DevicePtr->SwapChainRTV, pixels.data(), static_cast<u32>(pixels.size()), width, height, pitch) && width > 0 && height > 0)
					{
						const u32 px = std::min(static_cast<u32>(x * width), width - 1);
						const u32 py = std::min(static_cast<u32>(y * height), height - 1);
						const size_t offset = static_cast<size_t>(py) * pitch + static_cast<size_t>(px) * 4;
						if (offset + 3 < pixels.size())
						{
							light.Color.set(pixels[offset + 2] / 255.0f, pixels[offset + 1] / 255.0f, pixels[offset] / 255.0f);
							colorChanged = changed = sampled = true;
						}
					}
				}
			}
			if (!sampled) ShowSceneDialog(ESceneDialog::Info, "Pick light color", { "Could not read the scene color at the selected position." });
		}
	}
	if (ImGui::TreeNodeEx("Transform##light", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		positionChanged = ImGui::DragFloat3("Position##light", &light.Position.x, 0.05f);
		changed |= positionChanged;
		Fvector rotation = {};
		light.Orientation.getHPB(rotation);
		rotation.mul(rad2deg(1.0f));
		rotationChanged = ImGui::DragFloat3("Rotation##light", &rotation.x, 0.5f);
		if (rotationChanged)
		{
			light.Orientation.setHPB(deg2rad(rotation.x), deg2rad(rotation.y), deg2rad(rotation.z));
			changed = true;
		}
		ImGui::TreePop();
	}
	if (changed)
	{
		light.Color.x = std::max(0.0f, light.Color.x);
		light.Color.y = std::max(0.0f, light.Color.y);
		light.Color.z = std::max(0.0f, light.Color.z);
	}
	if (changed || textureChanged)
	{
		for (const s32 index : SelectedLights)
		{
			auto& target = Lights[index];
			if (index != SelectedLight)
			{
				if (light.Type != previous.Type)
				{
					target.Type = light.Type;
				}
				if (light.Active != previous.Active)
				{
					target.Active = light.Active;
				}
				if (light.Shadow != previous.Shadow)
				{
					target.Shadow = light.Shadow;
				}
				if (colorChanged)
				{
					target.Color = light.Color;
				}
				if (light.Intensity != previous.Intensity)
				{
					target.Intensity = light.Intensity;
				}
				if (light.Range != previous.Range)
				{
					target.Range = light.Range;
				}
				if (light.VirtualSize != previous.VirtualSize)
				{
					target.VirtualSize = light.VirtualSize;
				}
				if (light.Cone != previous.Cone)
				{
					target.Cone = light.Cone;
				}
				if (light.Volumetric != previous.Volumetric)
				{
					target.Volumetric = light.Volumetric;
				}
				if (light.VolumeQuality != previous.VolumeQuality)
				{
					target.VolumeQuality = light.VolumeQuality;
				}
				if (light.VolumeIntensity != previous.VolumeIntensity)
				{
					target.VolumeIntensity = light.VolumeIntensity;
				}
				if (light.VolumeDistance != previous.VolumeDistance)
				{
					target.VolumeDistance = light.VolumeDistance;
				}
				if (light.Indirect != previous.Indirect)
				{
					target.Indirect = light.Indirect;
				}
				if (light.HudMode != previous.HudMode)
				{
					target.HudMode = light.HudMode;
				}
				if (light.Occlusion != previous.Occlusion)
				{
					target.Occlusion = light.Occlusion;
				}
				if (positionChanged)
				{
					target.Position = light.Position;
				}
				if (rotationChanged)
				{
					target.Orientation = light.Orientation;
				}
				if (textureChanged)
				{
					xr_strcpy(target.Texture, light.Texture);
				}
			}
			ApplyCinemaLight(target, textureChanged);
		}
	}
	DrawLightModulation(light);
}

bool CCinemaFilmMakerManager::AddMasterClip(SCinemaActor& actor, const shared_str& sourceId, const shared_str& name, float start)
{
	if (actor.Animated == nullptr || actor.MasterClips.size() >= kCinemaSceneMaxPlaylist)
	{
		return false;
	}
	const SAnimationSource* source = FindSource(actor, sourceId);
	const SAnimationClip* clip = source ? FindClip(*source, name, false) : nullptr;
	if (clip == nullptr || !EnsureSource(actor, sourceId))
	{
		return false;
	}
	const MotionID motion = CinemaFindMotion(actor, *source, *clip);
	if (!motion.valid())
	{
		return false;
	}
	const float length = actor.Animated->get_animation_length(motion);
	if (!std::isfinite(length) || length < 0.01f)
	{
		return false;
	}
	SMasterClip entry;
	entry.Source = sourceId;
	entry.Name = name;
	entry.Start = std::clamp(start, 0.0f, std::min(3600.0f, MasterDuration));
	entry.Duration = std::min(length, 3600.0f);
	actor.MasterClips.push_back(std::move(entry));
	return true;
}

float CCinemaFilmMakerManager::BuildMasterPlaylist(SCinemaActor& actor, xr_vector<SMasterPlaylistSlice>& slices)
{
	float total = 0.0f;
	if (actor.Animated == nullptr)
	{
		return total;
	}
	for (const SPlaylistEntry& entry : actor.Playlist)
	{
		if (entry.Muted || entry.Fx)
		{
			continue;
		}
		const SAnimationSource* source = FindSource(actor, entry.Source);
		const SAnimationClip* animation = source ? FindClip(*source, entry.Name, false) : nullptr;
		if (animation == nullptr || !EnsureSource(actor, entry.Source))
		{
			continue;
		}
		const MotionID motion = CinemaFindMotion(actor, *source, *animation);
		if (!motion.valid())
		{
			continue;
		}
		const float length = actor.Animated->get_animation_length(motion);
		const STrimRange* trim = FindTrim(actor, entry.Source, entry.Name);
		const float speed = GetClipSpeed(actor, trim);
		if (!std::isfinite(length) || length <= END_EPS || !std::isfinite(speed) || fabsf(speed) <= EPS_S)
		{
			continue;
		}
		const auto add = [&](float first, float last)
		{
			first = std::clamp(first, 0.0f, length - END_EPS);
			last = std::clamp(last, 0.0f, length - END_EPS);
			if (last <= first)
			{
				return;
			}
			const float duration = (last - first) / fabsf(speed);
			slices.push_back({ source, animation, trim, total, duration, speed >= 0.0f ? first : last, speed >= 0.0f ? last : first, static_cast<s32>(&entry - actor.Playlist.data()) });
			total += duration;
		};
		if (trim == nullptr || trim->Segments.empty())
		{
			add(0.0f, length - END_EPS);
		}
		else if (speed >= 0.0f)
		{
			for (const STrimSegment& segment : trim->Segments) add(segment.Start, segment.End);
		}
		else
		{
			for (auto it = trim->Segments.rbegin(); it != trim->Segments.rend(); ++it) add(it->Start, it->End);
		}
	}
	return total;
}

void CCinemaFilmMakerManager::UpdateMasterActors()
{
	if (!MasterActive)
	{
		return;
	}
	for (auto& actorPtr : Actors)
	{
		SCinemaActor& actor = *actorPtr;
		actor.MasterPlaylistIndex = -1;
		if (actor.PatternPreview)
		{
			continue;
		}
		if (!actor.MasterTrack || (actor.MasterClips.empty() && !actor.PatternOwned))
		{
			if (actor.MasterOwned)
			{
				StopActor(actor);
				actor.MasterOwned = false;
			}
			continue;
		}
		const bool wasOwned = actor.MasterOwned;
		actor.MasterOwned = true;
		const SMasterClip* selected = nullptr;
		for (const SMasterClip& entry : actor.MasterClips)
		{
			if (CinemaMasterClipEnabled(entry, MasterEffectiveTrackMuted) && entry.Start <= MasterTime && MasterTime < entry.Start + entry.Duration && (selected == nullptr || entry.Start > selected->Start))
			{
				selected = &entry;
			}
		}
		if (selected == nullptr)
		{
			if (!wasOwned || !actor.MasterBeforeFirst || !actor.Paused || actor.PlaylistIndex >= 0)
			{
				StopActor(actor);
			}
			actor.MasterBeforeFirst = true;
			continue;
		}
		if (actor.PatternOwned)
		{
			xr_vector<SMasterPlaylistSlice> slices;
			const float total = BuildMasterPlaylist(actor, slices);
			if (slices.empty() || total <= EPS_S)
			{
				if (!wasOwned || !actor.MasterBeforeFirst)
				{
					StopActor(actor);
				}
				actor.MasterBeforeFirst = true;
				continue;
			}
			actor.MasterBeforeFirst = false;
			float elapsed = std::max(0.0f, MasterTime - selected->Start);
			bool reverse = false;
			if (actor.PingPong)
			{
				elapsed = fmodf(elapsed, total * 2.0f);
				reverse = elapsed >= total;
				if (reverse) elapsed = total * 2.0f - elapsed;
			}
			else if (actor.Loop)
			{
				elapsed = fmodf(elapsed, total);
			}
			else
			{
				elapsed = std::min(elapsed, total);
			}
			const SMasterPlaylistSlice* slice = &slices.back();
			for (const SMasterPlaylistSlice& candidate : slices)
			{
				if (elapsed < candidate.Start + candidate.Duration)
				{
					slice = &candidate;
					break;
				}
			}
			if (!actor.Motion.valid() || actor.MotionFx || actor.MotionSource != slice->Source->Id || actor.MotionName != slice->Animation->Name)
			{
				if (!PlayClip(actor, *slice->Source, *slice->Animation, false)) continue;
			}
			actor.PlaylistIndex = -1;
			actor.MasterPlaylistIndex = actor.PingPong || actor.Loop || elapsed < total ? slice->PlaylistIndex : -1;
			actor.Paused = true;
			CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
			const float fraction = std::clamp((elapsed - slice->Start) / slice->Duration, 0.0f, 1.0f);
			const float localTime = slice->SourceStart + (slice->SourceEnd - slice->SourceStart) * fraction;
			CinemaSetBlendTime(actor.Animated, actor.Motion, localTime);
			actor.ShotPreviousTime = localTime;
			continue;
		}
		actor.MasterBeforeFirst = false;
		const SAnimationSource* source = FindSource(actor, selected->Source);
		const SAnimationClip* clip = source ? FindClip(*source, selected->Name, false) : nullptr;
		if (clip == nullptr)
		{
			if (!wasOwned || !actor.Paused)
			{
				StopActor(actor);
			}
			continue;
		}
		if (!actor.Motion.valid() || actor.MotionFx || actor.MotionSource != selected->Source || actor.MotionName != selected->Name)
		{
			if (!PlayClip(actor, *source, *clip, false))
			{
				continue;
			}
		}
		actor.PlaylistIndex = -1;
		actor.Paused = true;
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
		const float length = actor.Animated->get_animation_length(actor.Motion);
		const float sourceOut = selected->SourceOut > 0.0f ? std::min(selected->SourceOut, length) : length;
		const float sourceIn = std::clamp(selected->SourceIn, 0.0f, std::max(0.0f, sourceOut - END_EPS));
		const float elapsed = std::clamp(MasterTime - selected->Start, 0.0f, selected->Duration);
		const float localTime = std::clamp(CinemaMasterSourceTime(sourceIn, selected->Speed, elapsed), sourceIn, std::max(sourceIn, sourceOut - END_EPS));
		CinemaSetBlendTime(actor.Animated, actor.Motion, localTime);
		actor.ShotPreviousTime = localTime;
	}
}

static constexpr float CinemaMasterLightIntensity(float start, float end, float fraction)
{
	return start + (end - start) * fraction;
}

static_assert(CinemaMasterLightIntensity(0.0f, 1.0f, 0.0f) == 0.0f);
static_assert(CinemaMasterLightIntensity(0.0f, 1.0f, 1.0f) == 1.0f);
static_assert(CinemaMasterLightIntensity(0.0f, 1.0f, 0.5f) == 0.5f);

void CCinemaFilmMakerManager::UpdateMasterLights()
{
	for (SCinemaLight& light : Lights)
	{
		if (!MasterActive || !light.MasterTrack || (light.MasterClips.empty() && !light.PatternOwned))
		{
			if (light.MasterOwned)
			{
				light.Time = light.MasterSavedTime;
				light.Running = light.MasterSavedRunning;
				light.MasterIntensity = 1.0f;
				light.MasterColor.set(1.0f, 1.0f, 1.0f);
				light.MasterOwned = false;
				UpdateCinemaLightValues(light);
				if (light.Light)
				{
					light.Light->set_active(light.Active);
				}
			}
			continue;
		}
		if (!light.MasterOwned)
		{
			light.MasterSavedTime = light.Time;
			light.MasterSavedRunning = light.Running;
			light.MasterOwned = true;
		}
		const SMasterLightClip* active = nullptr;
		for (const SMasterLightClip& clip : light.MasterClips)
		{
			if (CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) && MasterTime >= clip.Start && MasterTime < clip.Start + clip.Duration && (active == nullptr || clip.Start > active->Start))
			{
				active = &clip;
			}
		}
		if (active != nullptr)
		{
			const float local = MasterTime - active->Start;
			const float fraction = std::clamp(local / active->Duration, 0.0f, 1.0f);
			light.MasterIntensity = CinemaMasterLightIntensity(active->StartIntensity, active->EndIntensity, fraction);
			light.MasterColor.lerp(active->StartColor, active->EndColor, fraction);
			light.Time = local;
			UpdateCinemaLightValues(light);
		}
		if (light.Light)
		{
			light.Light->set_active(active != nullptr && light.Active);
		}
	}
}

static constexpr bool CinemaMasterParticleActive(float time, float start, float duration)
{
	return time >= start && time < start + duration;
}

static_assert(CinemaMasterParticleActive(1.0f, 1.0f, 2.0f));
static_assert(CinemaMasterParticleActive(2.0f, 1.0f, 2.0f));
static_assert(!CinemaMasterParticleActive(3.0f, 1.0f, 2.0f));

void CCinemaFilmMakerManager::UpdateMasterParticles()
{
	for (SCinemaParticle& particle : Particles)
	{
		if (particle.PatternPreview)
		{
			const s32 index = static_cast<s32>(&particle - Particles.data());
			const bool editorOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)];
			const bool withinClip = MasterActive && particle.MasterTrack && std::any_of(particle.MasterClips.begin(), particle.MasterClips.end(), [&](const SMasterParticleClip& clip)
			{
				return CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) && MasterTime >= clip.Start && MasterTime < clip.Start + clip.Duration;
			});
			const bool editing = editorOpen && (particle.PreviewOnSpawn ? withinClip : (MasterPatternEditorOpen && MasterEditingPattern.Type == 3 && std::find(SelectedParticles.begin(), SelectedParticles.end(), index) != SelectedParticles.end()));
			const bool activeClip = MasterPlaying && withinClip;
			if (!editing || activeClip)
			{
				StopParticlePreview(particle);
			}
			else
			{
				if (std::isfinite(Device.fTimeDelta))
				{
					particle.Time = fmodf(particle.Time + std::max(0.0f, Device.fTimeDelta), std::max(0.01f, particle.Out));
				}
				UpdateParticleTransform(particle);
				if (!particle.Object->IsPlaying())
				{
					particle.Object->Play(false);
				}
				continue;
			}
		}
		if (!MasterActive || !particle.MasterTrack || (particle.MasterClips.empty() && !particle.PatternOwned))
		{
			if (particle.MasterOwned)
			{
				particle.Object->Stop(false);
				particle.Time = particle.MasterSavedTime;
				particle.Running = particle.MasterSavedRunning;
				particle.Started = particle.MasterSavedStarted;
				particle.MasterOwned = false;
				particle.MasterPlayingClip = -1;
				particle.MasterPreviousTime = -1.0f;
				UpdateParticleTransform(particle);
				if (particle.MasterSavedPlaying)
				{
					particle.Object->Play(false);
				}
			}
			continue;
		}
		if (!particle.MasterOwned)
		{
			if (particle.PatternOwned && !MasterPlaying && !MasterParticleSeek && Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)])
			{
				StartParticleSpawnPreview(particle);
				if (particle.PatternPreview) continue;
			}
			particle.MasterSavedTime = particle.Time;
			particle.MasterSavedRunning = particle.Running;
			particle.MasterSavedStarted = particle.Started;
			particle.MasterSavedPlaying = particle.Object->IsPlaying();
			particle.MasterOwned = true;
			particle.MasterPlayingClip = -1;
			particle.MasterPreviousTime = -1.0f;
			particle.Object->Stop(false);
		}
		s32 active = -1;
		for (s32 i = 0; i < static_cast<s32>(particle.MasterClips.size()); ++i)
		{
			const SMasterParticleClip& clip = particle.MasterClips[i];
			if (CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) && CinemaMasterParticleActive(MasterTime, clip.Start, clip.Duration) && (active < 0 || clip.Start > particle.MasterClips[active].Start))
			{
				active = i;
			}
		}
		if (active >= 0 && particle.PatternOwned && !MasterPlaying && (MasterParticleSeek || active != particle.MasterPlayingClip) &&
			fabsf(MasterTime - particle.MasterClips[active].Start) <= EPS_S && Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)])
		{
			StartParticleSpawnPreview(particle);
			if (particle.PatternPreview) continue;
		}
		const bool rewind = particle.MasterPreviousTime >= 0.0f && MasterTime < particle.MasterPreviousTime;
		if (active != particle.MasterPlayingClip || (active >= 0 && (rewind || MasterParticleSeek)))
		{
			particle.Object->Stop(false);
			particle.MasterPlayingClip = active;
			if (active >= 0)
			{
				particle.Time = std::min(MasterTime - particle.MasterClips[active].Start, particle.Out);
				UpdateParticleTransform(particle);
				particle.Object->Play(false);
			}
		}
		if (active >= 0)
		{
			particle.Time = std::min(MasterTime - particle.MasterClips[active].Start, particle.Out);
			UpdateParticleTransform(particle);
			if (MasterPlaying && particle.Loop && particle.Time > 0.05f && !particle.Object->IsPlaying())
			{
				particle.Object->Play(false);
			}
			if (!MasterPlaying)
			{
				static_cast<CCinemaParticleObject*>(particle.Object.get())->SetSpeed(0.0f);
			}
		}
		particle.MasterPreviousTime = MasterTime;
	}
	MasterParticleSeek = false;
}

static constexpr bool CinemaMasterShotInRange(float shot, float from, float to, bool includeStart)
{
	return (shot > from || (includeStart && shot == from)) && shot <= to;
}

static_assert(CinemaMasterShotInRange(0.0f, 0.0f, 0.1f, true));
static_assert(!CinemaMasterShotInRange(0.0f, 0.0f, 0.1f, false));
static_assert(CinemaMasterShotInRange(1.0f, 0.9f, 1.0f, false));
static_assert(!CinemaMasterShotInRange(1.0f, 1.0f, 1.1f, false));

void CCinemaFilmMakerManager::FireMasterShots(float from, float to, bool includeStart)
{
	struct SShotEvent
	{
		SCinemaActor* Actor;
		const SMasterClip* Clip;
		float Time;
		float LocalTime;
		bool Pattern;
	};
	xr_vector<SShotEvent> events;
	for (auto& actor : Actors)
	{
		if (!actor->MasterTrack || actor->Animated == nullptr)
		{
			continue;
		}
		for (const SMasterClip& clip : actor->MasterClips)
		{
			if (!CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted)) continue;
			if (actor->PatternOwned)
			{
				xr_vector<SMasterPlaylistSlice> slices;
				const float total = BuildMasterPlaylist(*actor, slices);
				if (total <= EPS_S) continue;
				const bool repeating = actor->Loop || actor->PingPong;
				const float period = actor->PingPong ? total * 2.0f : total;
				for (const SMasterPlaylistSlice& slice : slices)
				{
					if (slice.Trim == nullptr) continue;
					for (const float shot : slice.Trim->Shots)
					{
						const float fraction = (shot - slice.SourceStart) / (slice.SourceEnd - slice.SourceStart);
						if (fraction < 0.0f || fraction >= 1.0f) continue;
						const float offset = slice.Start + slice.Duration * fraction;
						const auto schedule = [&](float phase)
						{
							const float first = clip.Start + phase;
							float cycle = repeating ? std::max(0.0f, floorf((from - first) / period)) : 0.0f;
							for (;;)
							{
								const float time = first + cycle * period;
								if (time > to || time >= clip.Start + clip.Duration) break;
								if (CinemaMasterShotInRange(time, from, to, includeStart)) events.push_back({ actor.get(), &clip, time, phase, true });
								if (!repeating || cycle + 1.0f <= cycle) break;
								cycle += 1.0f;
							}
						};
						schedule(offset);
						if (actor->PingPong && offset > EPS_S) schedule(total * 2.0f - offset);
					}
				}
				continue;
			}
			for (const float shot : clip.Shots)
			{
				const float localTime = CinemaMasterClipTime(clip.SourceIn, clip.Speed, shot);
				const float time = clip.Start + localTime;
				if (localTime >= 0.0f && localTime < clip.Duration && CinemaMasterShotInRange(time, from, to, includeStart))
				{
					events.push_back({ actor.get(), &clip, time, localTime, false });
				}
			}
		}
	}
	std::stable_sort(events.begin(), events.end(), [](const SShotEvent& left, const SShotEvent& right) { return left.Time < right.Time; });
	for (const SShotEvent& event : events)
	{
		SCinemaActor& actor = *event.Actor;
		const SMasterClip* active = nullptr;
		for (const SMasterClip& clip : actor.MasterClips)
		{
			if (CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) && clip.Start <= event.Time && event.Time < clip.Start + clip.Duration && (active == nullptr || clip.Start > active->Start))
			{
				active = &clip;
			}
		}
		if (active != event.Clip)
		{
			continue;
		}
		if (event.Pattern)
		{
			MasterTime = event.Time;
			UpdateMasterActors();
			if (actor.Motion.valid())
			{
				UpdateActorPlacement(actor);
				CinemaFireShot(actor);
			}
			continue;
		}
		const SAnimationSource* source = FindSource(actor, active->Source);
		const SAnimationClip* animation = source ? FindClip(*source, active->Name, false) : nullptr;
		if (animation == nullptr || !EnsureSource(actor, active->Source))
		{
			continue;
		}
		const MotionID motion = CinemaFindMotion(actor, *source, *animation);
		if (!motion.valid())
		{
			continue;
		}
		const float length = actor.Animated->get_animation_length(motion);
		const float sourceOut = active->SourceOut > 0.0f ? std::min(active->SourceOut, length) : length;
		const float sourceIn = std::clamp(active->SourceIn, 0.0f, std::max(0.0f, sourceOut - END_EPS));
		if (CinemaMasterSourceTime(sourceIn, active->Speed, event.LocalTime) >= sourceOut)
		{
			continue;
		}
		MasterTime = event.Time;
		UpdateMasterActors();
		if (actor.Motion != motion)
		{
			continue;
		}
		UpdateActorPlacement(actor);
		CinemaFireShot(actor);
	}
}

static constexpr bool CinemaMasterBoxIntersects(float firstStart, float firstEnd, float secondStart, float secondEnd)
{
	return firstStart <= secondEnd && secondStart <= firstEnd;
}

static_assert(CinemaMasterBoxIntersects(1.0f, 2.0f, 2.0f, 3.0f));
static_assert(!CinemaMasterBoxIntersects(1.0f, 2.0f, 3.0f, 4.0f));

static void CinemaSelectMasterClip(xr_vector<SMasterClipSelection>& selected, SMasterClipSelection& anchor, const xr_vector<SMasterClipSelection>& visible, SMasterClipSelection clicked, bool control, bool shift)
{
	const auto item = std::find(selected.begin(), selected.end(), clicked);
	if (shift)
	{
		const auto first = std::find(visible.begin(), visible.end(), anchor);
		const auto last = std::find(visible.begin(), visible.end(), clicked);
		if (first != visible.end() && last != visible.end())
		{
			if (!control)
			{
				selected.clear();
			}
			const auto begin = std::min(first, last);
			const auto end = std::max(first, last);
			for (auto it = begin; it <= end; ++it)
			{
				if (std::find(selected.begin(), selected.end(), *it) == selected.end())
				{
					selected.push_back(*it);
				}
			}
			return;
		}
	}
	if (control && item != selected.end())
	{
		selected.erase(item);
	}
	else if (control)
	{
		selected.push_back(clicked);
	}
	else if (item == selected.end() || selected.size() <= 1)
	{
		selected = { clicked };
	}
	anchor = clicked;
}

SMasterClipProperties CCinemaFilmMakerManager::MasterClipProperties(const SMasterClipSelection& item)
{
	if (item.Type == 1 && item.Object >= 0 && item.Object < static_cast<s32>(Actors.size()) && item.Clip >= 0 && item.Clip < static_cast<s32>(Actors[item.Object]->MasterClips.size()))
	{
		auto& clip = Actors[item.Object]->MasterClips[item.Clip];
		return { &clip.Start, &clip.Duration, &clip.Track, &clip.Muted, &clip.DisplayName };
	}
	if (item.Type == 2 && item.Object >= 0 && item.Object < static_cast<s32>(Lights.size()) && item.Clip >= 0 && item.Clip < static_cast<s32>(Lights[item.Object].MasterClips.size()))
	{
		auto& clip = Lights[item.Object].MasterClips[item.Clip];
		return { &clip.Start, &clip.Duration, &clip.Track, &clip.Muted, &clip.DisplayName };
	}
	if (item.Type == 3 && item.Object >= 0 && item.Object < static_cast<s32>(Particles.size()) && item.Clip >= 0 && item.Clip < static_cast<s32>(Particles[item.Object].MasterClips.size()))
	{
		auto& clip = Particles[item.Object].MasterClips[item.Clip];
		return { &clip.Start, &clip.Duration, &clip.Track, &clip.Muted, &clip.DisplayName };
	}
	if (item.Type == 4 && item.Object >= 0 && item.Object < static_cast<s32>(Cameras.size()) && item.Clip == 0)
	{
		auto& clip = Cameras[item.Object];
		return { &clip.Start, &clip.Duration, &clip.Track, &clip.Muted, &clip.DisplayName };
	}
	return {};
}

void CCinemaFilmMakerManager::ResizeMasterClips(float delta)
{
	float lower = -FLT_MAX;
	float upper = FLT_MAX;
	for (const auto& item : MasterClipSelection)
	{
		const auto clip = MasterClipProperties(item);
		if (!clip.Duration) continue;
		lower = std::max(lower, 0.01f - *clip.Duration);
		upper = std::min(upper, std::max(0.0f, std::min(3600.0f, std::max(0.01f, MasterDuration - *clip.Start)) - *clip.Duration));
	}
	if (lower > upper) return;
	delta = std::clamp(delta, lower, upper);
	for (const auto& item : MasterClipSelection)
	{
		const auto clip = MasterClipProperties(item);
		if (clip.Duration) *clip.Duration += delta;
	}
}

void CCinemaFilmMakerManager::MoveMasterClipTracks(const SMasterClipSelection& anchor, s32 track, const xr_vector<s32>& visibleTracks)
{
	const auto anchorClip = MasterClipProperties(anchor);
	if (!anchorClip.Track) return;
	const auto anchorRow = std::find(visibleTracks.begin(), visibleTracks.end(), *anchorClip.Track);
	const auto targetRow = std::find(visibleTracks.begin(), visibleTracks.end(), track);
	if (anchorRow == visibleTracks.end() || targetRow == visibleTracks.end()) return;
	s32 lower = -static_cast<s32>(visibleTracks.size());
	s32 upper = static_cast<s32>(visibleTracks.size());
	for (const auto& item : MasterClipSelection)
	{
		const auto clip = MasterClipProperties(item);
		if (!clip.Track) continue;
		const auto row = std::find(visibleTracks.begin(), visibleTracks.end(), *clip.Track);
		if (row == visibleTracks.end()) return;
		const s32 index = static_cast<s32>(row - visibleTracks.begin());
		lower = std::max(lower, -index);
		upper = std::min(upper, static_cast<s32>(visibleTracks.size()) - 1 - index);
	}
	const s32 delta = std::clamp(static_cast<s32>(targetRow - anchorRow), lower, upper);
	for (const auto& item : MasterClipSelection)
	{
		const auto clip = MasterClipProperties(item);
		if (clip.Track)
		{
			const auto row = std::find(visibleTracks.begin(), visibleTracks.end(), *clip.Track);
			*clip.Track = visibleTracks[static_cast<size_t>(row - visibleTracks.begin() + delta)];
		}
	}
}

void CCinemaFilmMakerManager::MoveMasterClips(float delta)
{
	float lower = -MasterDuration;
	float upper = MasterDuration;
	for (const auto& item : MasterClipSelection)
	{
		const auto clip = MasterClipProperties(item);
		if (!clip.Start) continue;
		lower = std::max(lower, -*clip.Start);
		upper = std::min(upper, std::min(3600.0f, MasterDuration) - *clip.Start);
	}
	if (lower > upper) return;
	delta = std::clamp(delta, lower, upper);
	for (const auto& item : MasterClipSelection)
	{
		const auto clip = MasterClipProperties(item);
		if (clip.Start) *clip.Start += delta;
	}
}

static constexpr float CinemaMasterMoveKey(float time, float delta, float duration, float width)
{
	return std::clamp(time + delta * duration / width, 0.0f, duration);
}

static_assert(CinemaMasterMoveKey(1.0f, 10.0f, 10.0f, 100.0f) == 2.0f);
static_assert(CinemaMasterMoveKey(1.0f, -20.0f, 10.0f, 100.0f) == 0.0f);
static_assert(CinemaMasterMoveKey(9.0f, 20.0f, 10.0f, 100.0f) == 10.0f);

static bool CinemaDrawMasterClipResize(float& clipDuration, float clipStart, ImDrawList* draw, float x, float end, float y, float width, float rowHeight, float rulerWidth, float sceneDuration, bool snap, float snapStep, bool& dragged, float& dragTarget)
{
	if (clipStart + clipDuration > sceneDuration)
	{
		return false;
	}
	const float handleX = width < 8.0f ? x + width : end - 5.0f;
	ImGui::SetCursorScreenPos(ImVec2(handleX, y + 2.0f));
	ImGui::InvisibleButton("##clip_resize", ImVec2(10.0f, rowHeight - 4.0f));
	if (ImGui::IsItemActivated())
	{
		dragTarget = clipDuration;
	}
	if (ImGui::IsItemHovered() || ImGui::IsItemActive())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
		const float markerX = width < 8.0f ? x + width - 1.0f : end - 2.0f;
		draw->AddLine(ImVec2(markerX, y + 4.0f), ImVec2(markerX, y + rowHeight - 4.0f), IM_COL32(255, 255, 255, 255), 2.0f);
	}
	if (ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		dragTarget += ImGui::GetIO().MouseDelta.x * sceneDuration / rulerWidth;
		const float targetEnd = clipStart + dragTarget;
		clipDuration = std::clamp((snap ? roundf(targetEnd / snapStep) * snapStep : targetEnd) - clipStart, 0.01f, std::min(3600.0f, std::max(0.01f, sceneDuration - clipStart)));
		dragged = true;
	}
	if (ImGui::IsItemDeactivated() && dragged)
	{
		if (snap)
		{
			clipDuration = std::clamp(roundf((clipStart + clipDuration) / snapStep) * snapStep - clipStart, 0.01f, std::min(3600.0f, std::max(0.01f, sceneDuration - clipStart)));
		}
		dragged = false;
	}
	return true;
}

static constexpr float CinemaMasterZoomScroll(float scroll, float width, float zoomRatio, float fraction)
{
	return scroll + width * (zoomRatio - 1.0f) * fraction;
}

static_assert(CinemaMasterZoomScroll(0.0f, 100.0f, 2.0f, 0.5f) == 50.0f);
static_assert(CinemaMasterZoomScroll(50.0f, 200.0f, 0.5f, 0.5f) == 0.0f);

static constexpr float CinemaMasterTickMultiple(float normalized)
{
	return normalized <= 1.0f ? 1.0f : normalized <= 2.0f ? 2.0f : normalized <= 5.0f ? 5.0f : 10.0f;
}

static_assert(CinemaMasterTickMultiple(1.0f) == 1.0f);
static_assert(CinemaMasterTickMultiple(1.5f) == 2.0f);
static_assert(CinemaMasterTickMultiple(4.0f) == 5.0f);
static_assert(CinemaMasterTickMultiple(6.0f) == 10.0f);

float CCinemaFilmMakerManager::MasterClipStart() const
{
	const float last = std::min(3600.0f, std::max(0.0f, MasterDuration - 0.01f));
	const float time = std::clamp(MasterTime, 0.0f, last);
	if (!MasterSnap)
	{
		return time;
	}
	return std::clamp(roundf(time / MasterSnapStep) * MasterSnapStep, 0.0f, floorf(last / MasterSnapStep) * MasterSnapStep);
}

s32 CCinemaFilmMakerManager::FindAvailableMasterTrack(float start, float duration) const
{
	const float end = start + duration;
	for (s32 track = 0; track < kCinemaMaxTracks; ++track)
	{
		if (MasterTrackParent[track] && MasterTrackCollapsed[MasterTrackParent[track] - 1]) continue;
		bool occupied = false;
		const auto check = [&](const auto& clips)
		{
			for (const auto& clip : clips)
			{
				if (clip.Track == track && clip.Start < end && start < clip.Start + clip.Duration)
				{
					occupied = true;
				}
			}
		};
		for (const auto& actor : Actors)
		{
			if (actor->MasterTrack)
			{
				check(actor->MasterClips);
			}
		}
		for (const auto& light : Lights)
		{
			if (light.MasterTrack)
			{
				check(light.MasterClips);
			}
		}
		for (const auto& particle : Particles)
		{
			if (particle.MasterTrack)
			{
				check(particle.MasterClips);
			}
		}
		check(Cameras);
		if (!occupied)
		{
			return track;
		}
	}
	for (s32 track = 0; track < kCinemaMaxTracks; ++track) if (!MasterTrackParent[track]) return track;
	return 0;
}

void CCinemaFilmMakerManager::UpdatePatternSettingsSelection(s32 type, size_t previousCount)
{
	const float start = MasterClipStart();
	const float duration = std::min(1.0f, MasterDuration - start);
	const auto prepare = [&](auto& object)
	{
		const s32 track = FindAvailableMasterTrack(start, duration);
		object.MasterTrack = true;
		object.PatternOwned = true;
		MasterActive = true;
		if (object.MasterClips.empty())
		{
			object.MasterClips.emplace_back();
			object.MasterClips.back().Start = start;
			object.MasterClips.back().Duration = duration;
		}
		for (auto& clip : object.MasterClips)
		{
			clip.Track = track;
		}
	};
	if (type == 1)
	{
		for (size_t i = previousCount; i < Actors.size(); ++i)
		{
			prepare(*Actors[i]);
		}
	}
	else if (type == 2)
	{
		for (size_t i = previousCount; i < Lights.size(); ++i)
		{
			prepare(Lights[i]);
		}
	}
	else if (type == 3)
	{
		for (size_t i = previousCount; i < Particles.size(); ++i)
		{
			prepare(Particles[i]);
			StartParticleSpawnPreview(Particles[i]);
		}
	}
	if (!MasterPatternEditorOpen)
	{
		return;
	}
	const s32 selected = type == 1 ? Selected : type == 2 ? SelectedLight : SelectedParticle;
	const bool hasClips = (type == 1 && selected >= 0 && selected < static_cast<s32>(Actors.size()) && Actors[selected]->MasterTrack && !Actors[selected]->MasterClips.empty()) ||
		(type == 2 && selected >= 0 && selected < static_cast<s32>(Lights.size()) && Lights[selected].MasterTrack && !Lights[selected].MasterClips.empty()) ||
		(type == 3 && selected >= 0 && selected < static_cast<s32>(Particles.size()) && Particles[selected].MasterTrack && !Particles[selected].MasterClips.empty());
	if (!hasClips || (MasterEditingPattern.Type == type && MasterEditingPattern.Object == selected))
	{
		return;
	}
	MasterEditingPattern = { type, selected, 0 };
	MasterSelectedActor = type == 1 ? selected : -1;
	MasterSelectedClip = type == 1 ? 0 : -1;
	MasterSelectedLight = type == 2 ? selected : -1;
	MasterSelectedLightClip = type == 2 ? 0 : -1;
	MasterSelectedParticle = type == 3 ? selected : -1;
	MasterSelectedParticleClip = type == 3 ? 0 : -1;
	MasterClipSelection = { MasterEditingPattern };
	MasterClipAnchor = MasterEditingPattern;
}

void CCinemaFilmMakerManager::DrawMasterTimeline(bool& state)
{

	const ImVec2 display = ImGui::GetIO().DisplaySize;
	ImGui::SetNextWindowSize(ImVec2(display.x * 0.9f, display.y * 0.85f), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints(ImVec2(600.0f, 350.0f), ImVec2(FLT_MAX, FLT_MAX));
	if (ImGui::Begin("Cinema Film Maker Editor###cinema_master", &state))
	{
		if (!g_pGameLevel)
		{
			ImGui::TextDisabled("Load a level to use the cinema editor");
			ImGui::End();
			return;
		}
		DrawToolbar();
		ImGui::Separator();
		const bool spacePlayback = ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows) &&
			!ImGui::GetIO().WantTextInput && !ImGui::IsAnyItemActive() && !MasterBoxSelecting && !Gizmo.Active &&
			!ImGui::GetIO().KeyCtrl && !ImGui::GetIO().KeyAlt && ImGui::IsKeyPressed(ImGuiKey_Space, false);
		if (ImGui::Button(MasterPlaying ? "Pause" : "Play") || spacePlayback)
		{
			StopPatternPreviews();
			if (!MasterPlaying && MasterTime >= MasterContentEnd())
			{
				MasterTime = 0.0f;
				MasterIncludeStart = true;
			}
			MasterPlaying = !MasterPlaying;
			MasterActive = true;
		}
		ImGui::SameLine();
		if (ImGui::Button("Stop"))
		{
			ResetMasterTimeline();
		}
		ImGui::SameLine();
		ImGui::Checkbox("Loop", &MasterLoop);
		ImGui::SameLine();
		ImGui::Text("End: %.2f s", MasterContentEnd());
		if (ImGui::Checkbox("Hide editor helpers during playback", &HideHelpersDuringPlayback))
		{
			SaveSettings();
		}
		ImGui::SetNextItemWidth(120.0f);
		if (ImGui::DragFloat("Zoom##master", &MasterZoom, 0.05f, 1.0f, 32.0f, "%.1fx"))
		{
			MasterZoom = std::isfinite(MasterZoom) ? std::clamp(MasterZoom, 1.0f, 32.0f) : 1.0f;
			MasterZoomTarget = MasterZoom;
		}
		ImGui::SameLine();
		if (ImGui::Button("Fit##master_zoom"))
		{
			MasterZoom = 1.0f;
			MasterZoomTarget = MasterZoom;
		}
		ImGui::SameLine();
		CinemaHint("Wheel: scroll tracks | Ctrl + wheel: zoom | Middle-drag: pan | Right-drag: horizontal pan | Shift-drag: temporarily disable snap.");
		if (ImGui::Checkbox("Snap to grid##master", &MasterSnap))
		{
			SaveSettings();
		}
		if (MasterSnap)
		{
			ImGui::SameLine();
			ImGui::SetNextItemWidth(100.0f);
			if (ImGui::DragFloat("Grid step (s)##master_snap", &MasterSnapStep, 0.001f, 0.001f, 60.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp))
			{
				MasterSnapStep = std::isfinite(MasterSnapStep) ? std::clamp(MasterSnapStep, 0.001f, 60.0f) : 1.0f / 30.0f;
				SaveSettings();
			}
		}
		ImGui::TextDisabled("Clips and the playhead snap to the grid. Hold Shift to temporarily disable snap.");
		ImGui::TextDisabled("Patterns play only inside their clip. Shot markers fire real bullets during playback.");
		const auto snapPlayhead = [&](float time)
		{
			if (MasterSnap && !ImGui::GetIO().KeyShift) time = roundf(time / MasterSnapStep) * MasterSnapStep;
			return std::clamp(time, 0.0f, MasterDuration);
		};
		ImGui::SetNextItemWidth(-1.0f);
		if (ImGui::SliderFloat("##master_time", &MasterTime, 0.0f, MasterDuration, "Time: %.2f s"))
		{
			StopPatternPreviews();
			MasterTime = snapPlayhead(MasterTime);
			MasterActive = true;
			MasterIncludeStart = true;
			MasterParticleSeek = true;
		}
		ImGui::Separator();
		CinemaHint("Click: select | Double-click: edit | Ctrl + drag: duplicate | Drag: move selection | Drag edge: resize selection | Shift: disable snap | Drag empty space: select area");
		const float availableWidth = ImGui::GetContentRegionAvail().x;
		const float splitterWidth = 6.0f;
		const ImGuiStyle& style = ImGui::GetStyle();
		const float checkboxWidth = ImGui::GetFrameHeight() + style.ItemInnerSpacing.x + ImGui::CalcTextSize("Stretch to clip").x;
		const float minSidebarWidth = std::max(140.0f, std::max(checkboxWidth, CinemaButtonWidth("Add shot at playhead")) + 2.0f * style.WindowPadding.x + style.ScrollbarSize + 2.0f);
		const float maxSidebarWidth = std::max(minSidebarWidth, availableWidth - splitterWidth - 180.0f);
		if (MasterSidebarWidth <= 0.0f) MasterSidebarWidth = std::clamp(availableWidth * 0.25f, 200.0f, 280.0f);
		MasterSidebarWidth = std::clamp(MasterSidebarWidth, minSidebarWidth, maxSidebarWidth);
		const float objectsWidth = MasterSidebarWidth;
		const float patternsHeight = ImGui::GetTextLineHeightWithSpacing() + ImGui::GetFrameHeightWithSpacing() * 4.0f + ImGui::GetStyle().WindowPadding.y * 2.0f;
		ImGui::BeginChild("##master_patterns", ImVec2(objectsWidth, patternsHeight), true, ImGuiWindowFlags_NoMove);
		ImGui::TextUnformatted("Add pattern");
		const auto addActorPattern = [&](float start, s32 targetTrack)
		{
			const s32 track = targetTrack >= 0 ? targetTrack : FindAvailableMasterTrack(start, std::min(1.0f, MasterDuration - start));
			if (!ModelFilesBuilt) BuildModelFiles();
			const shared_str visual = SelectedVisual.size() && VisualPathExists(SelectedVisual.c_str()) ? SelectedVisual : (ModelFiles.empty() ? shared_str() : ModelFiles.front());
			if (visual.size() && Actors.size() < kCinemaSceneMaxActors && AddActorInFront(visual.c_str()))
			{
				SCinemaActor& actor = *Actors.back();
				actor.MasterTrack = true;
				actor.PatternOwned = true;
				SMasterClip clip;
				clip.Track = track;
				clip.Start = start;
				clip.Duration = std::min(1.0f, MasterDuration - clip.Start);
				actor.MasterClips.push_back(clip);
				MasterActive = true;
				MasterSelectedActor = static_cast<s32>(Actors.size()) - 1;
				MasterSelectedClip = 0;
				MasterSelectedLight = MasterSelectedParticle = -1;
				MasterClipSelection = { { 1, MasterSelectedActor, 0 } };
				MasterClipAnchor = MasterClipSelection.front();
			}
			else
			{
				ShowSceneDialog(ESceneDialog::Info, "Add actor pattern", { "Select an available model in the Actor tab first." });
			}
		};
		const auto addParticlePattern = [&](float start, s32 targetTrack)
		{
			const s32 track = targetTrack >= 0 ? targetTrack : FindAvailableMasterTrack(start, std::min(1.0f, MasterDuration - start));
			const auto& names = GamePersistent().Environment().m_pRender->particles_systems_library().vec_all_particles();
			shared_str name = SelectedParticleName;
			if (!name.size() || !IsCinemaParticle(name))
			{
				for (const shared_str& candidate : names)
				{
					if (IsCinemaParticle(candidate)) { name = candidate; break; }
				}
			}
			Fvector position = Device.vCameraPosition;
			position.mad(Device.vCameraDirection, 3.0f);
			if (name.size() && AddParticle(name, position))
			{
				SCinemaParticle& particle = Particles.back();
				particle.MasterTrack = true;
				particle.PatternOwned = true;
				SMasterParticleClip clip;
				clip.Track = track;
				clip.Start = start;
				clip.Duration = std::min(1.0f, MasterDuration - clip.Start);
				particle.MasterClips.push_back(clip);
				MasterActive = true;
				StartParticleSpawnPreview(particle);
				MasterSelectedParticle = static_cast<s32>(Particles.size()) - 1;
				MasterSelectedParticleClip = 0;
				MasterSelectedActor = MasterSelectedLight = -1;
				MasterClipSelection = { { 3, MasterSelectedParticle, 0 } };
				MasterClipAnchor = MasterClipSelection.front();
			}
			else
			{
				ShowSceneDialog(ESceneDialog::Info, "Add particle pattern", { "No usable particle effect is available." });
			}
		};
		const auto addLightPattern = [&](float start, s32 targetTrack)
		{
			const s32 track = targetTrack >= 0 ? targetTrack : FindAvailableMasterTrack(start, std::min(1.0f, MasterDuration - start));
			SCinemaLight light;
			light.Position = Device.vCameraPosition;
			light.Position.mad(Device.vCameraDirection, 3.0f);
			light.Light = ::Render->light_create();
			if (light.Light)
			{
				light.MasterTrack = true;
				light.PatternOwned = true;
				SMasterLightClip clip;
				clip.Track = track;
				clip.Start = start;
				clip.Duration = std::min(1.0f, MasterDuration - clip.Start);
				light.MasterClips.push_back(clip);
				MasterActive = true;
				ApplyCinemaLight(light, true);
				Lights.push_back(std::move(light));
				MasterSelectedLight = static_cast<s32>(Lights.size()) - 1;
				MasterSelectedLightClip = 0;
				MasterSelectedActor = MasterSelectedParticle = -1;
				MasterClipSelection = { { 2, MasterSelectedLight, 0 } };
				MasterClipAnchor = MasterClipSelection.front();
			}
			else ShowSceneDialog(ESceneDialog::Info, "Add light pattern", { "The renderer could not create a light." });
		};
		const auto addCameraPattern = [&](float start, s32 targetTrack)
		{
			SCinemaCameraClip camera;
			camera.Fov = std::isfinite(Device.fFOV) ? std::clamp(Device.fFOV, 1.0f, 179.0f) : 90.0f;
			if (Cameras.size() >= kCinemaSceneMaxActors) return;
			camera.Start = start;
			camera.Duration = std::min(1.0f, MasterDuration - camera.Start);
			camera.SourceDuration = camera.Duration;
			camera.Track = targetTrack >= 0 ? targetTrack : FindAvailableMasterTrack(camera.Start, camera.Duration);
			ClearGizmo();
			Cameras.push_back(std::move(camera));
			MasterSelectedActor = MasterSelectedLight = MasterSelectedParticle = -1;
			MasterClipSelection = { { 4, static_cast<s32>(Cameras.size()) - 1, 0 } };
			MasterClipAnchor = MasterClipSelection.front();
			MasterActive = true;
		};
		if (ImGui::Button("Actor##pattern", ImVec2(-1.0f, 0.0f))) addActorPattern(MasterClipStart(), -1);
		if (ImGui::Button("Particle##pattern", ImVec2(-1.0f, 0.0f))) addParticlePattern(MasterClipStart(), -1);
		if (ImGui::Button("Light##pattern", ImVec2(-1.0f, 0.0f))) addLightPattern(MasterClipStart(), -1);
		if (ImGui::Button("Camera##pattern", ImVec2(-1.0f, 0.0f))) addCameraPattern(MasterClipStart(), -1);
		ImGui::EndChild();
		const ImVec2 inspectorPosition = ImGui::GetCursorScreenPos();
		ImGui::SameLine(0.0f, 0.0f);
		ImGui::InvisibleButton("##master_sidebar_splitter", ImVec2(splitterWidth, std::max(1.0f, ImGui::GetContentRegionAvail().y)));
		if (ImGui::IsItemHovered() || ImGui::IsItemActive())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
			ImGui::GetWindowDrawList()->AddRectFilled(ImGui::GetItemRectMin(), ImGui::GetItemRectMax(), ImGui::GetColorU32(ImGuiCol_SeparatorHovered));
		}
		if (ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
		{
			MasterSidebarWidth = std::clamp(MasterSidebarWidth + ImGui::GetIO().MouseDelta.x, minSidebarWidth, maxSidebarWidth);
		}
		ImGui::SameLine(0.0f, 0.0f);
		ImGui::BeginChild("##master_tracks", ImVec2(0.0f, 0.0f), true, ImGuiWindowFlags_HorizontalScrollbar | ImGuiWindowFlags_NoScrollWithMouse | ImGuiWindowFlags_NoMove);
		xr_vector<s32> visibleTracks;
		s32 displayedTracks = 32;
		const auto includeTrack = [&](s32 track) { displayedTracks = std::min(kCinemaMaxTracks, std::max(displayedTracks, track + 2)); };
		for (const auto& actor : Actors) for (const auto& clip : actor->MasterClips) includeTrack(clip.Track);
		for (const auto& light : Lights) for (const auto& clip : light.MasterClips) includeTrack(clip.Track);
		for (const auto& particle : Particles) for (const auto& clip : particle.MasterClips) includeTrack(clip.Track);
		for (const auto& camera : Cameras) includeTrack(camera.Track);
		for (s32 track = 0; track < kCinemaMaxTracks; ++track) if (MasterTrackGroup[track] || MasterTrackParent[track]) { includeTrack(track); includeTrack(static_cast<s32>(MasterTrackParent[track]) - 1); }
		s32 trackRows[kCinemaMaxTracks];
		std::fill(std::begin(trackRows), std::end(trackRows), -1);
		for (s32 track = 0; track < displayedTracks; ++track)
		{
			if (MasterTrackParent[track]) continue;
			trackRows[track] = static_cast<s32>(visibleTracks.size());
			visibleTracks.push_back(track);
			if (MasterTrackGroup[track] && !MasterTrackCollapsed[track])
			{
				for (s32 child = 0; child < displayedTracks; ++child)
				{
					if (MasterTrackParent[child] != track + 1) continue;
					trackRows[child] = static_cast<s32>(visibleTracks.size());
					visibleTracks.push_back(child);
				}
			}
		}
		xr_vector<SMasterClipSelection> visibleMasterClips;
		for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
		{
			if (Actors[i]->MasterTrack)
			{
				for (s32 j = 0; j < static_cast<s32>(Actors[i]->MasterClips.size()); ++j)
				{
					if (Actors[i]->MasterClips[j].Start <= MasterDuration) visibleMasterClips.push_back({ 1, i, j });
				}
			}
		}
		for (s32 i = 0; i < static_cast<s32>(Lights.size()); ++i)
		{
			if (Lights[i].MasterTrack)
			{
				for (s32 j = 0; j < static_cast<s32>(Lights[i].MasterClips.size()); ++j)
				{
					if (Lights[i].MasterClips[j].Start <= MasterDuration) visibleMasterClips.push_back({ 2, i, j });
				}
			}
		}
		for (s32 i = 0; i < static_cast<s32>(Particles.size()); ++i)
		{
			if (Particles[i].MasterTrack)
			{
				for (s32 j = 0; j < static_cast<s32>(Particles[i].MasterClips.size()); ++j)
				{
					if (Particles[i].MasterClips[j].Start <= MasterDuration) visibleMasterClips.push_back({ 3, i, j });
				}
			}
		}
		for (s32 i = 0; i < static_cast<s32>(Cameras.size()); ++i)
		{
			if (Cameras[i].Start <= MasterDuration) visibleMasterClips.push_back({ 4, i, 0 });
		}
		visibleMasterClips.erase(std::remove_if(visibleMasterClips.begin(), visibleMasterClips.end(), [&](const SMasterClipSelection& item)
		{
			const auto clip = MasterClipProperties(item);
			return !clip.Track || *clip.Track < 0 || *clip.Track >= kCinemaMaxTracks || trackRows[*clip.Track] < 0;
		}), visibleMasterClips.end());
		std::stable_sort(visibleMasterClips.begin(), visibleMasterClips.end(), [&](const SMasterClipSelection& a, const SMasterClipSelection& b)
		{
			const auto left = MasterClipProperties(a);
			const auto right = MasterClipProperties(b);
			if (trackRows[*left.Track] != trackRows[*right.Track]) return trackRows[*left.Track] < trackRows[*right.Track];
			return *left.Start < *right.Start;
		});
		const size_t selectionCount = MasterClipSelection.size();
		MasterClipSelection.erase(std::remove_if(MasterClipSelection.begin(), MasterClipSelection.end(), [&](const SMasterClipSelection& item)
		{
			return std::find(visibleMasterClips.begin(), visibleMasterClips.end(), item) == visibleMasterClips.end();
		}), MasterClipSelection.end());
		if (MasterClipSelection.size() != selectionCount && std::find(MasterClipSelection.begin(), MasterClipSelection.end(), MasterClipAnchor) == MasterClipSelection.end())
		{
			MasterClipAnchor = MasterClipSelection.empty() ? SMasterClipSelection{} : MasterClipSelection.back();
			MasterSelectedActor = MasterClipAnchor.Type == 1 ? MasterClipAnchor.Object : -1;
			MasterSelectedClip = MasterClipAnchor.Type == 1 ? MasterClipAnchor.Clip : -1;
			MasterSelectedLight = MasterClipAnchor.Type == 2 ? MasterClipAnchor.Object : -1;
			MasterSelectedLightClip = MasterClipAnchor.Type == 2 ? MasterClipAnchor.Clip : -1;
			MasterSelectedParticle = MasterClipAnchor.Type == 3 ? MasterClipAnchor.Object : -1;
			MasterSelectedParticleClip = MasterClipAnchor.Type == 3 ? MasterClipAnchor.Clip : -1;
		}
		s32 clipActionType = 0;
		s32 clipActionObject = -1;
		s32 clipActionIndex = -1;
		bool clipActionDuplicate = false;
		bool shortcutDuplicate = false;
		ImGui::TextUnformatted("Tracks");
		const float minLabelWidth = ImGui::CalcTextSize("Track 65535").x + 44.0f;
		const float labelWidth = std::clamp(ImGui::GetContentRegionAvail().x * 0.20f, minLabelWidth, std::max(minLabelWidth, 160.0f));
		float rulerWidth = std::max(100.0f, ImGui::GetContentRegionAvail().x - labelWidth) * MasterZoom;
		const float rowHeight = ImGui::GetTextLineHeightWithSpacing() + 8.0f;
		const ImVec2 origin = ImGui::GetCursorScreenPos();
		ImDrawList* draw = ImGui::GetWindowDrawList();
		const ImU32 lineColor = ImGui::GetColorU32(ImGuiCol_Border);
		const ImU32 textColor = ImGui::GetColorU32(ImGuiCol_Text);
		const float rulerStart = origin.x + labelWidth;
		const ImGuiIO& io = ImGui::GetIO();
		if (!ImGui::IsMouseDown(ImGuiMouseButton_Middle)) MasterMiddlePan = false;
		if (ImGui::IsWindowHovered() && ImGui::IsMouseClicked(ImGuiMouseButton_Middle) &&
			!ImGui::IsMouseDown(ImGuiMouseButton_Left) && !ImGui::IsMouseDown(ImGuiMouseButton_Right) && !ImGui::IsAnyItemActive())
		{
			MasterMiddlePan = true;
		}
		if (MasterMiddlePan && ImGui::IsMouseDragging(ImGuiMouseButton_Middle))
		{
			ImGui::SetScrollX(std::clamp(ImGui::GetScrollX() - io.MouseDelta.x, 0.0f, ImGui::GetScrollMaxX()));
			ImGui::SetScrollY(std::clamp(ImGui::GetScrollY() - io.MouseDelta.y, 0.0f, ImGui::GetScrollMaxY()));
			ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeAll);
		}
		if (ImGui::IsWindowHovered() && io.MousePos.x >= rulerStart && io.MousePos.y >= origin.y + rowHeight && ImGui::IsMouseClicked(ImGuiMouseButton_Right))
		{
			MasterPanCandidate = true;
			MasterPanDragged = false;
		}
		if (MasterPanCandidate && ImGui::IsMouseDragging(ImGuiMouseButton_Right))
		{
			MasterPanDragged = true;
			ImGui::SetScrollX(std::clamp(ImGui::GetScrollX() - io.MouseDelta.x, 0.0f, ImGui::GetScrollMaxX()));
			ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
		}
		if (ImGui::IsWindowHovered() && io.MouseWheel != 0.0f && !MasterMiddlePan && !ImGui::IsMouseDown(ImGuiMouseButton_Left))
		{
			if (io.KeyCtrl)
			{
				MasterZoomTarget = std::clamp(MasterZoomTarget * powf(1.2f, io.MouseWheel), 1.0f, 32.0f);
				MasterZoomAnchor = std::clamp((io.MousePos.x - rulerStart) / rulerWidth, 0.0f, 1.0f);
			}
			else
			{
				ImGui::SetScrollY(std::clamp(ImGui::GetScrollY() - io.MouseWheel * rowHeight * 3.0f, 0.0f, ImGui::GetScrollMaxY()));
			}
		}
		if (MasterMiddlePan || MasterPanCandidate || (ImGui::IsWindowHovered() && ImGui::IsMouseDown(ImGuiMouseButton_Left)))
		{
			MasterZoomTarget = MasterZoom;
		}
		if (MasterZoom != MasterZoomTarget)
		{
			const float factor = 1.0f - expf(-14.0f * std::clamp(io.DeltaTime, 0.0f, 0.1f));
			float newZoom = MasterZoom + (MasterZoomTarget - MasterZoom) * factor;
			if (fabsf(newZoom - MasterZoomTarget) < 0.001f) newZoom = MasterZoomTarget;
			const float ratio = newZoom / MasterZoom;
			const float scroll = CinemaMasterZoomScroll(ImGui::GetScrollX(), rulerWidth, ratio, MasterZoomAnchor);
			ImGui::SetScrollX(std::max(0.0f, scroll));
			rulerWidth *= ratio;
			MasterZoom = newZoom;
		}
		const float desiredStep = MasterDuration * 90.0f / rulerWidth;
		const float magnitude = powf(10.0f, floorf(log10f(desiredStep)));
		const float normalizedStep = desiredStep / magnitude;
		const float tickStep = MasterSnap ? MasterSnapStep * std::max(1.0f, ceilf(desiredStep / MasterSnapStep)) : magnitude * CinemaMasterTickMultiple(normalizedStep);
		const float gridPixels = rulerWidth * MasterSnapStep / MasterDuration;
		const float visibleGridStep = MasterSnapStep * std::max(1.0f, ceilf(8.0f / gridPixels));
		const float visibleLeft = std::max(rulerStart, ImGui::GetWindowPos().x);
		const float visibleRight = std::min(rulerStart + rulerWidth, ImGui::GetWindowPos().x + ImGui::GetWindowWidth());
		const u32 firstGridLine = static_cast<u32>(std::max(0.0f, ceilf((visibleLeft - rulerStart) * MasterDuration / (rulerWidth * visibleGridStep))));
		const u32 lastGridLine = static_cast<u32>(std::max(0.0f, floorf((visibleRight - rulerStart) * MasterDuration / (rulerWidth * visibleGridStep))));
		const u32 tickCount = static_cast<u32>(floorf(MasterDuration / tickStep));
		for (u32 tick = 0; tick <= tickCount; ++tick)
		{
			const float tickTime = static_cast<float>(tick) * tickStep;
			const float x = rulerStart + rulerWidth * tickTime / MasterDuration;
			char label[32] = {};
			const char* tickFormat = tickStep < 0.01f ? "%.4f" : tickStep < 1.0f || fabsf(tickStep - roundf(tickStep)) > 0.0001f ? "%.3f" : "%.0f";
			xr_sprintf(label, tickFormat, tickTime);
			draw->AddLine(ImVec2(x, origin.y), ImVec2(x, origin.y + rowHeight), lineColor);
			draw->AddText(ImVec2(x + 2.0f, origin.y), textColor, label);
		}
		ImGui::SetCursorScreenPos(ImVec2(rulerStart, origin.y));
		ImGui::InvisibleButton("##master_ruler", ImVec2(rulerWidth, rowHeight), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
		if (MasterUseLoopRange && ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) &&
			io.MousePos.x >= rulerStart + rulerWidth * MasterLoopStart / MasterDuration &&
			io.MousePos.x <= rulerStart + rulerWidth * MasterLoopEnd / MasterDuration)
		{
			MasterUseLoopRange = false;
			MasterLoopResizeEdge = 0;
		}
		const float loopLeftDistance = fabsf(io.MousePos.x - (rulerStart + rulerWidth * MasterLoopStart / MasterDuration));
		const float loopRightDistance = fabsf(io.MousePos.x - (rulerStart + rulerWidth * MasterLoopEnd / MasterDuration));
		const s32 hoveredLoopEdge = MasterUseLoopRange && std::min(loopLeftDistance, loopRightDistance) <= 8.0f ? (loopLeftDistance <= loopRightDistance ? -1 : 1) : 0;
		if ((ImGui::IsItemHovered() && hoveredLoopEdge) || (ImGui::IsItemActive() && MasterLoopResizeEdge)) ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
		if (ImGui::IsItemActivated())
		{
			MasterLoopResizeEdge = hoveredLoopEdge;
			if (MasterLoopResizeEdge) MasterLoopDragStart = MasterLoopResizeEdge < 0 ? MasterLoopEnd : MasterLoopStart;
			if (!MasterLoopResizeEdge && ImGui::IsMouseDown(ImGuiMouseButton_Right))
			{
				MasterLoopDragStart = snapPlayhead(std::clamp((io.MousePos.x - rulerStart) / rulerWidth, 0.0f, 1.0f) * MasterDuration);
				MasterUseLoopRange = false;
			}
		}
		if (ImGui::IsItemActive() && (MasterLoopResizeEdge || ImGui::IsMouseDragging(ImGuiMouseButton_Right)))
		{
			const float end = snapPlayhead(std::clamp((io.MousePos.x - rulerStart) / rulerWidth, 0.0f, 1.0f) * MasterDuration);
			MasterLoopStart = std::min(MasterLoopDragStart, end);
			MasterLoopEnd = std::max(MasterLoopDragStart, end);
			if (MasterLoopResizeEdge) MasterLoopResizeEdge = end < MasterLoopDragStart ? -1 : 1;
			MasterUseLoopRange = MasterLoopEnd - MasterLoopStart >= 0.009f;
		}
		if (ImGui::IsItemActive() && !MasterLoopResizeEdge && ImGui::IsMouseDown(ImGuiMouseButton_Left))
		{
			StopPatternPreviews();
			MasterTime = snapPlayhead(std::clamp((ImGui::GetIO().MousePos.x - rulerStart) / rulerWidth, 0.0f, 1.0f) * MasterDuration);
			MasterActive = true;
			MasterIncludeStart = true;
			MasterParticleSeek = true;
		}
		float y = origin.y + rowHeight;
		s32 pendingTrack = -1;
		SMasterClipSelection pendingTrackClip;
		const s32 trackCount = kCinemaMaxTracks;
		const auto groupChanged = [&]()
		{
			if (MasterSoloTrack >= 0 && !MasterSoloChanged)
			{
				for (s32 i = 0; i < trackCount; ++i) MasterTrackMuted[i] = i != MasterSoloTrack && MasterTrackParent[i] != MasterSoloTrack + 1;
			}
			RefreshMasterTrackMute();
			StopPatternPreviews();
			MasterParticleSeek = true;
		};
		bool contextOverClip = false;
		const xr_vector<bool> cameraConflicts = CinemaCameraConflicts(Cameras, MasterEffectiveTrackMuted);
		const auto duplicateStart = [&]() -> float*
		{
			return MasterClipProperties(MasterDuplicateTarget).Start;
		};
		if (MasterDuplicateDrag && ImGui::IsMouseDown(ImGuiMouseButton_Left) && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
		{
			if (float* start = duplicateStart())
			{
				MasterDuplicateMoved = true;
				MasterDragTarget += io.MouseDelta.x * MasterDuration / rulerWidth;
				const float target = MasterSnap && !io.KeyShift ? roundf(MasterDragTarget / MasterSnapStep) * MasterSnapStep : MasterDragTarget;
				MoveMasterClips(target - *start);
				pendingTrack = visibleTracks[std::clamp(static_cast<s32>((io.MousePos.y - origin.y - rowHeight) / rowHeight), 0, static_cast<s32>(visibleTracks.size()) - 1)];
				pendingTrackClip = MasterDuplicateTarget;
			}
			else
			{
				MasterDuplicateDrag = false;
			}
		}
		for (s32 track : visibleTracks)
		{
			char trackName[32] = {};
			xr_sprintf(trackName, "Track %d", track + 1);
			ImGui::PushID(track);
			const float indent = MasterTrackParent[track] ? 12.0f : 0.0f;
			if (MasterTrackGroup[track])
			{
				ImGui::SetCursorScreenPos(ImVec2(origin.x, y));
				if (ImGui::InvisibleButton("##track_expand", ImVec2(16.0f, rowHeight))) MasterTrackCollapsed[track] = !MasterTrackCollapsed[track];
				const ImVec2 center(origin.x + 8.0f, y + rowHeight * 0.5f);
				if (MasterTrackCollapsed[track]) draw->AddTriangleFilled(ImVec2(center.x - 2.0f, center.y - 4.0f), ImVec2(center.x - 2.0f, center.y + 4.0f), ImVec2(center.x + 3.0f, center.y), textColor);
				else draw->AddTriangleFilled(ImVec2(center.x - 4.0f, center.y - 2.0f), ImVec2(center.x + 4.0f, center.y - 2.0f), ImVec2(center.x, center.y + 3.0f), textColor);
			}
			const float labelOffset = MasterTrackGroup[track] ? 16.0f : indent;
			ImGui::SetCursorScreenPos(ImVec2(origin.x + labelOffset, y));
			const float buttonWidth = std::min(20.0f, labelWidth);
			ImGui::InvisibleButton("##track_label", ImVec2(std::max(1.0f, labelWidth - buttonWidth - labelOffset), rowHeight), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
			const bool trackHovered = ImGui::IsItemHovered();
			if (ImGui::BeginDragDropSource())
			{
				ImGui::SetDragDropPayload("CINEMA_TRACK", &track, sizeof(track));
				if (MasterTrackGroup[track]) ImGui::TextUnformatted("Ungroup this track before moving it into another group.");
				else ImGui::Text("Move %s to a group", trackName);
				ImGui::EndDragDropSource();
			}
			if (ImGui::BeginDragDropTarget())
			{
				if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload("CINEMA_TRACK"))
				{
					if (payload->DataSize == sizeof(s32))
					{
						const s32 source = *static_cast<const s32*>(payload->Data);
						const s32 target = MasterTrackParent[track] ? MasterTrackParent[track] - 1 : track;
						if (source >= 0 && source < trackCount && source != target && !MasterTrackGroup[source])
						{
							MasterTrackGroup[target] = true;
							MasterTrackCollapsed[target] = false;
							MasterTrackParent[source] = static_cast<u16>(target + 1);
							groupChanged();
						}
					}
				}
				ImGui::EndDragDropTarget();
			}
			if (!MasterPanDragged && ImGui::BeginPopupContextItem("##track_context"))
			{
				if (!MasterTrackParent[track] && !MasterTrackGroup[track] && ImGui::MenuItem("Create group")) MasterTrackGroup[track] = true;
				if (MasterTrackParent[track] && ImGui::MenuItem("Remove from group"))
				{
					MasterTrackParent[track] = 0;
					groupChanged();
				}
				if (MasterTrackGroup[track] && ImGui::MenuItem("Ungroup tracks"))
				{
					for (s32 i = 0; i < trackCount; ++i) if (MasterTrackParent[i] == track + 1) MasterTrackParent[i] = 0;
					MasterTrackGroup[track] = MasterTrackCollapsed[track] = false;
					groupChanged();
				}
				if (ImGui::MenuItem(MasterTrackMuted[track] ? "Unmute track" : "Mute track"))
				{
					if (MasterSoloTrack >= 0) MasterSoloChanged = true;
					MasterTrackMuted[track] = !MasterTrackMuted[track];
					RefreshMasterTrackMute();
					StopPatternPreviews();
					MasterParticleSeek = true;
				}
				ImGui::EndPopup();
			}
			if (trackHovered) ImGui::SetTooltip("Drag onto a track to group. Right-click for track actions.");
			ImGui::SetCursorScreenPos(ImVec2(rulerStart - buttonWidth, y));
			ImGui::InvisibleButton("##track_toggle", ImVec2(buttonWidth, rowHeight), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
			if (ImGui::IsItemClicked(ImGuiMouseButton_Left))
			{
				if (MasterSoloTrack >= 0) MasterSoloChanged = true;
				MasterTrackMuted[track] = !MasterTrackMuted[track];
				RefreshMasterTrackMute();
				StopPatternPreviews();
				MasterParticleSeek = true;
			}
			if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
			{
				if (MasterSoloTrack >= 0 && (MasterSoloTrack == track || MasterSoloChanged))
				{
					std::copy(std::begin(MasterSoloPreviousMuted), std::end(MasterSoloPreviousMuted), std::begin(MasterTrackMuted));
					MasterSoloTrack = -1;
					MasterSoloChanged = false;
				}
				else
				{
					if (MasterSoloTrack < 0) std::copy(std::begin(MasterTrackMuted), std::end(MasterTrackMuted), std::begin(MasterSoloPreviousMuted));
					MasterSoloTrack = track;
					for (s32 i = 0; i < trackCount; ++i) MasterTrackMuted[i] = i != track && MasterTrackParent[i] != track + 1;
				}
				RefreshMasterTrackMute();
				StopPatternPreviews();
				MasterParticleSeek = true;
			}
			const bool toggleHovered = ImGui::IsItemHovered();
			if (toggleHovered) ImGui::SetTooltip("Left-click: mute/unmute track\nRight-click: %s", MasterSoloTrack >= 0 && (MasterSoloTrack == track || MasterSoloChanged) ? "restore tracks" : "solo track");
			ImGui::PopID();
			draw->PushClipRect(ImVec2(origin.x, y), ImVec2(rulerStart - buttonWidth, y + rowHeight), true);
			draw->AddText(ImVec2(origin.x + labelOffset + 4.0f, y + 4.0f), MasterEffectiveTrackMuted[track] ? ImGui::GetColorU32(ImGuiCol_TextDisabled) : textColor, trackName);
			draw->PopClipRect();
			const ImVec2 toggleCenter(rulerStart - buttonWidth * 0.5f, y + rowHeight * 0.5f);
			const ImU32 toggleColor = MasterEffectiveTrackMuted[track] ? IM_COL32(85, 85, 85, 255) : MasterSoloTrack == track ? IM_COL32(245, 190, 70, 255) : IM_COL32(110, 200, 110, 255);
			draw->AddCircleFilled(toggleCenter, 5.0f, toggleColor);
			draw->AddCircle(toggleCenter, 5.0f, toggleHovered ? IM_COL32(255, 255, 255, 255) : IM_COL32(20, 20, 20, 255));
			draw->AddRectFilled(ImVec2(rulerStart, y), ImVec2(rulerStart + rulerWidth, y + rowHeight), ImGui::GetColorU32((track & 1) ? ImGuiCol_ChildBg : ImGuiCol_FrameBg));
			if (MasterSnap && visibleRight >= visibleLeft)
			{
				for (u32 grid = firstGridLine; grid <= lastGridLine; ++grid)
				{
					const float x = rulerStart + rulerWidth * (static_cast<float>(grid) * visibleGridStep) / MasterDuration;
					draw->AddLine(ImVec2(x, y), ImVec2(x, y + rowHeight), ImGui::GetColorU32(ImGuiCol_Separator));
				}
			}
			for (u32 tick = 0; tick <= tickCount; ++tick)
			{
				const float x = rulerStart + rulerWidth * (static_cast<float>(tick) * tickStep) / MasterDuration;
				draw->AddLine(ImVec2(x, y), ImVec2(x, y + rowHeight), lineColor);
			}
			draw->AddLine(ImVec2(rulerStart, y + rowHeight), ImVec2(rulerStart + rulerWidth, y + rowHeight), lineColor);
			const auto drawClip = [&](auto& clip, s32 type, s32 objectIndex, s32 clipIndex, const char* label)
			{
				if (clip.Track != track || clip.Start > MasterDuration) return;
				const SMasterClipSelection item{ type, objectIndex, clipIndex };
				const float x = rulerStart + rulerWidth * clip.Start / MasterDuration;
				const float end = rulerStart + rulerWidth * std::min(MasterDuration, clip.Start + clip.Duration) / MasterDuration;
				const float width = std::max(4.0f, end - x);
				if (io.MousePos.x >= x && io.MousePos.x <= x + width + 5.0f && io.MousePos.y >= y && io.MousePos.y < y + rowHeight) contextOverClip = true;
				ImGui::PushID(type);
				ImGui::PushID(objectIndex);
				ImGui::PushID(clipIndex);
				ImGui::SetCursorScreenPos(ImVec2(x, y + 2.0f));
				ImGui::InvisibleButton("##pattern", ImVec2(clip.Start + clip.Duration <= MasterDuration && width >= 8.0f ? width - 6.0f : width, rowHeight - 4.0f));
				if (ImGui::IsItemActivated())
				{
					MasterDragTarget = clip.Start;
				}
				if (ImGui::IsItemClicked())
				{
					SelectedCameraKey = SelectedCameraKeyCamera = -1;
					if (io.KeyCtrl && !io.KeyShift)
					{
						clipActionType = type;
						clipActionObject = objectIndex;
						clipActionIndex = clipIndex;
						clipActionDuplicate = true;
						shortcutDuplicate = true;
						MasterShiftClickPending = false;
						MasterSingleClickPending = false;
					}
					else
					{
						MasterSingleClickPending = !io.KeyCtrl && !io.KeyShift && MasterClipSelection.size() > 1 && std::find(MasterClipSelection.begin(), MasterClipSelection.end(), item) != MasterClipSelection.end();
						if (MasterSingleClickPending)
						{
							MasterSingleClickItem = item;
						}
						if (io.KeyShift)
						{
							MasterShiftClickPending = true;
							MasterShiftClickCtrl = io.KeyCtrl;
							MasterShiftClickItem = item;
							MasterShiftPreviousAnchor = MasterClipAnchor;
							MasterShiftPreviousSelection = MasterClipSelection;
							if (std::find(MasterClipSelection.begin(), MasterClipSelection.end(), item) == MasterClipSelection.end())
							{
								MasterClipSelection = { item };
							}
						}
						else
						{
							MasterShiftClickPending = false;
							CinemaSelectMasterClip(MasterClipSelection, MasterClipAnchor, visibleMasterClips, item, io.KeyCtrl, false);
						}
						MasterSelectedActor = type == 1 ? objectIndex : -1;
						MasterSelectedClip = type == 1 ? clipIndex : -1;
						MasterSelectedLight = type == 2 ? objectIndex : -1;
						MasterSelectedLightClip = type == 2 ? clipIndex : -1;
						MasterSelectedParticle = type == 3 ? objectIndex : -1;
						MasterSelectedParticleClip = type == 3 ? clipIndex : -1;
						if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
						{
							StopPatternPreviews(true);
							MasterEditingPattern = item;
							MasterPatternEditorOpen = true;
							if (type == 4)
							{
								MasterPlaying = false;
								MasterTime = clip.Start;
								SelectedCameraKey = -1;
								SelectedCameraKeyCamera = -1;
								CameraTimelineDragKey = -1;
								MasterActive = true;
							}
							if (type == 1)
							{
								SelectActor(objectIndex);
								SelectedVisual = Actors[objectIndex]->Visual;
							}
							else if (type == 2)
							{
								SelectActor(-1);
								SelectedLight = objectIndex;
								SelectedLights = { objectIndex };
							}
							else if (type == 3)
							{
								SelectActor(-1);
								SelectedParticle = objectIndex;
								SelectedParticles = { objectIndex };
							}
						}
					}
				}
				if (!MasterDuplicateDrag && !shortcutDuplicate && ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
				{
					if (std::find(MasterClipSelection.begin(), MasterClipSelection.end(), item) == MasterClipSelection.end())
					{
						MasterClipSelection = { item };
						MasterClipAnchor = item;
					}
					if (MasterShiftClickPending && MasterShiftClickItem == item && !MasterDragMoved)
					{
						MasterClipAnchor = item;
					}
					MasterDragTarget += io.MouseDelta.x * MasterDuration / rulerWidth;
					const float target = MasterSnap && !io.KeyShift ? roundf(MasterDragTarget / MasterSnapStep) * MasterSnapStep : MasterDragTarget;
					MoveMasterClips(target - clip.Start);
					pendingTrack = visibleTracks[std::clamp(static_cast<s32>((io.MousePos.y - origin.y - rowHeight) / rowHeight), 0, static_cast<s32>(visibleTracks.size()) - 1)];
					pendingTrackClip = item;
					MasterDragMoved = true;
				}
				if (!MasterDuplicateDrag && ImGui::IsItemDeactivated())
				{
					if (MasterSingleClickPending && MasterSingleClickItem == item)
					{
						if (!MasterDragMoved)
						{
							MasterClipSelection = { item };
							MasterClipAnchor = item;
						}
						MasterSingleClickPending = false;
					}
					if (MasterShiftClickPending && MasterShiftClickItem == item)
					{
						if (!MasterDragMoved)
						{
							MasterClipSelection = MasterShiftPreviousSelection;
							MasterClipAnchor = MasterShiftPreviousAnchor;
							CinemaSelectMasterClip(MasterClipSelection, MasterClipAnchor, visibleMasterClips, item, MasterShiftClickCtrl, true);
						}
						MasterShiftClickPending = false;
						MasterShiftPreviousSelection.clear();
					}
					if (MasterDragMoved && MasterSnap && !io.KeyShift)
					{
						MoveMasterClips(roundf(clip.Start / MasterSnapStep) * MasterSnapStep - clip.Start);
					}
					MasterDragMoved = false;
				}
				const bool selected = std::find(MasterClipSelection.begin(), MasterClipSelection.end(), item) != MasterClipSelection.end();
				const char* displayName = clip.DisplayName.size() ? clip.DisplayName.c_str() : label;
				const auto requestRename = [&]()
				{
					MasterRenameTarget = item;
					xr_sprintf(MasterRenameBuffer, "%.*s", static_cast<int>(sizeof(MasterRenameBuffer) - 1), displayName);
					MasterRenameRequested = true;
				};
				if (selected && MasterClipSelection.size() == 1 && !io.WantTextInput && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::IsKeyPressed(ImGuiKey_F2, false)) requestRename();
				const bool conflict = type == 4 && cameraConflicts[objectIndex];
				const bool muted = !CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted);
				const bool clipHovered = ImGui::IsItemHovered();
				const ImU32 clipColor = clip.DisplayColor ? clip.DisplayColor : ImGui::GetColorU32(selected ? ImGuiCol_HeaderActive : ImGuiCol_Header);
				draw->AddRectFilled(ImVec2(x, y + 2.0f), ImVec2(x + width, y + rowHeight - 2.0f), muted ? IM_COL32(65, 65, 65, 255) : conflict ? IM_COL32(90, 90, 90, 255) : clipColor);
				if (selected && (muted || clip.DisplayColor)) draw->AddRect(ImVec2(x, y + 2.0f), ImVec2(x + width, y + rowHeight - 2.0f), ImGui::GetColorU32(ImGuiCol_HeaderActive), 0.0f, 0, 2.0f);
				draw->PushClipRect(ImVec2(x, y + 2.0f), ImVec2(x + width, y + rowHeight - 2.0f), true);
				const ImVec4 colorForText = ImGui::ColorConvertU32ToFloat4(clipColor);
				const bool brightColor = clip.DisplayColor && !muted && !conflict && colorForText.x * 0.299f + colorForText.y * 0.587f + colorForText.z * 0.114f > 0.65f;
				draw->AddText(ImVec2(x + 3.0f, y + 4.0f), brightColor ? IM_COL32(0, 0, 0, 255) : textColor, displayName);
				draw->PopClipRect();
				if (!MasterPanDragged && ImGui::BeginPopupContextItem("##pattern_context"))
				{
					if (ImGui::MenuItem("Rename", "F2")) requestRename();
					ImVec4 color = ImGui::ColorConvertU32ToFloat4(clip.DisplayColor ? clip.DisplayColor : ImGui::GetColorU32(ImGuiCol_Header));
					if (ImGui::ColorEdit3("Color##clip", &color.x, ImGuiColorEditFlags_NoInputs)) clip.DisplayColor = ImGui::ColorConvertFloat4ToU32(color);
					if (clip.DisplayColor && ImGui::MenuItem("Reset color")) clip.DisplayColor = 0;
					const bool group = selected && MasterClipSelection.size() > 1;
					if (ImGui::MenuItem(group ? (clip.Muted ? "Unmute selected" : "Mute selected") : (clip.Muted ? "Unmute clip" : "Mute clip")))
					{
						const bool mute = !clip.Muted;
						if (group)
						{
							for (const auto& selectedItem : MasterClipSelection)
							{
								const auto properties = MasterClipProperties(selectedItem);
								if (properties.Muted) *properties.Muted = mute;
							}
						}
						else clip.Muted = mute;
						StopPatternPreviews();
					}
					if (ImGui::MenuItem("Duplicate")) { clipActionType = type; clipActionObject = objectIndex; clipActionIndex = clipIndex; clipActionDuplicate = true; }
					if (ImGui::MenuItem("Remove")) { clipActionType = type; clipActionObject = objectIndex; clipActionIndex = clipIndex; clipActionDuplicate = false; }
					ImGui::EndPopup();
				}
				if (clipHovered)
				{
					if (clip.Muted) ImGui::SetTooltip("Clip muted");
					else if (MasterEffectiveTrackMuted[clip.Track]) ImGui::SetTooltip("Track muted");
					else if (conflict) ImGui::SetTooltip("Two camera clips cannot be active at the same time. This clip is disabled due to overlap.");
				}
				const float previousDuration = clip.Duration;
				const bool resizeHandle = CinemaDrawMasterClipResize(clip.Duration, clip.Start, draw, x, end, y, width, rowHeight, rulerWidth, MasterDuration, MasterSnap && !io.KeyShift, MasterSnapStep, MasterDragMoved, MasterDragTarget);
				if (resizeHandle && ImGui::IsItemActivated())
				{
					MasterGroupResize = selected && MasterClipSelection.size() > 1;
					MasterGroupResizeAnchor = item;
					if (!selected)
					{
						MasterClipSelection = { item };
						MasterClipAnchor = item;
						MasterSelectedActor = type == 1 ? objectIndex : -1;
						MasterSelectedClip = type == 1 ? clipIndex : -1;
						MasterSelectedLight = type == 2 ? objectIndex : -1;
						MasterSelectedLightClip = type == 2 ? clipIndex : -1;
						MasterSelectedParticle = type == 3 ? objectIndex : -1;
						MasterSelectedParticleClip = type == 3 ? clipIndex : -1;
					}
				}
				if (resizeHandle && MasterGroupResize && MasterGroupResizeAnchor == item && clip.Duration != previousDuration)
				{
					const float delta = clip.Duration - previousDuration;
					clip.Duration = previousDuration;
					ResizeMasterClips(delta);
				}
				if (resizeHandle && MasterGroupResizeAnchor == item && ImGui::IsItemDeactivated()) MasterGroupResize = false;
				ImGui::PopID();
				ImGui::PopID();
				ImGui::PopID();
			};
			for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
			{
				if (!Actors[i]->MasterTrack) continue;
				for (s32 j = 0; j < static_cast<s32>(Actors[i]->MasterClips.size()); ++j)
				{
					const SMasterClip& clip = Actors[i]->MasterClips[j];
					drawClip(Actors[i]->MasterClips[j], 1, i, j, !Actors[i]->PatternOwned && clip.Name.size() ? clip.Name.c_str() : Actors[i]->Name.c_str());
				}
			}
			for (s32 i = 0; i < static_cast<s32>(Lights.size()); ++i)
			{
				if (!Lights[i].MasterTrack) continue;
				for (s32 j = 0; j < static_cast<s32>(Lights[i].MasterClips.size()); ++j) drawClip(Lights[i].MasterClips[j], 2, i, j, "Light");
			}
			for (s32 i = 0; i < static_cast<s32>(Particles.size()); ++i)
			{
				if (!Particles[i].MasterTrack) continue;
				for (s32 j = 0; j < static_cast<s32>(Particles[i].MasterClips.size()); ++j) drawClip(Particles[i].MasterClips[j], 3, i, j, Particles[i].Name.c_str());
			}
			for (s32 i = 0; i < static_cast<s32>(Cameras.size()); ++i) drawClip(Cameras[i], 4, i, 0, "Camera");
			if (MasterTrackGroup[track] && MasterTrackCollapsed[track])
			{
				draw->PushClipRect(ImVec2(rulerStart, y + 2.0f), ImVec2(rulerStart + rulerWidth, y + rowHeight - 1.0f), true);
				const auto drawOverview = [&](const auto& clip)
				{
					if (clip.Track < 0 || clip.Track >= trackCount || MasterTrackParent[clip.Track] != track + 1 || clip.Start > MasterDuration) return;
					const float first = rulerStart + rulerWidth * clip.Start / MasterDuration;
					const float last = rulerStart + rulerWidth * std::min(MasterDuration, clip.Start + clip.Duration) / MasterDuration;
					const float lane = static_cast<float>(clip.Track % 3) * 2.0f;
					const ImU32 color = !CinemaMasterClipEnabled(clip, MasterEffectiveTrackMuted) ? IM_COL32(110, 110, 110, 255) : clip.DisplayColor ? clip.DisplayColor : ImGui::GetColorU32(ImGuiCol_HeaderActive);
					draw->AddLine(ImVec2(first, y + rowHeight - 3.0f - lane), ImVec2(std::max(first + 2.0f, last), y + rowHeight - 3.0f - lane), color, 2.0f);
				};
				for (const auto& actor : Actors) if (actor->MasterTrack) for (const auto& clip : actor->MasterClips) drawOverview(clip);
				for (const auto& light : Lights) if (light.MasterTrack) for (const auto& clip : light.MasterClips) drawOverview(clip);
				for (const auto& particle : Particles) if (particle.MasterTrack) for (const auto& clip : particle.MasterClips) drawOverview(clip);
				for (const auto& camera : Cameras) drawOverview(camera);
				draw->PopClipRect();
			}
			y += rowHeight;
		}
		const float canvasTop = origin.y + rowHeight;
		const bool insideCanvas = io.MousePos.x >= rulerStart && io.MousePos.x <= rulerStart + rulerWidth && io.MousePos.y >= canvasTop && io.MousePos.y <= y;
		bool boxActivated = false;
		if (MasterBoxSelecting || (insideCanvas && ImGui::IsWindowHovered() && !ImGui::IsAnyItemHovered()))
		{
			ImGui::SetCursorScreenPos(ImVec2(rulerStart, canvasTop));
			ImGui::InvisibleButton("##master_box_selection", ImVec2(rulerWidth, y - canvasTop));
			boxActivated = ImGui::IsItemActivated();
		}
		if (boxActivated)
		{
			SelectedCameraKey = SelectedCameraKeyCamera = -1;
			MasterBoxSelecting = true;
			MasterBoxDragged = false;
			MasterBoxStartTime = std::clamp((io.MousePos.x - rulerStart) / rulerWidth, 0.0f, 1.0f) * MasterDuration;
			MasterBoxStartRow = std::clamp((io.MousePos.y - canvasTop) / rowHeight, 0.0f, static_cast<float>(visibleTracks.size()));
			MasterBoxBaseSelection = io.KeyCtrl ? MasterClipSelection : xr_vector<SMasterClipSelection>();
			if (!io.KeyCtrl)
			{
				MasterClipSelection.clear();
				MasterClipAnchor = {};
				MasterSelectedActor = MasterSelectedLight = MasterSelectedParticle = -1;
				MasterSelectedClip = MasterSelectedLightClip = MasterSelectedParticleClip = -1;
			}
		}
		if (MasterBoxSelecting)
		{
			MasterBoxDragged |= ImGui::IsMouseDragging(ImGuiMouseButton_Left);
			if (MasterBoxDragged)
			{
				const float endTime = std::clamp((io.MousePos.x - rulerStart) / rulerWidth, 0.0f, 1.0f) * MasterDuration;
				const float endRow = std::clamp((io.MousePos.y - canvasTop) / rowHeight, 0.0f, static_cast<float>(visibleTracks.size()));
				const float minTime = std::min(MasterBoxStartTime, endTime);
				const float maxTime = std::max(MasterBoxStartTime, endTime);
				const float minRow = std::min(MasterBoxStartRow, endRow);
				const float maxRow = std::max(MasterBoxStartRow, endRow);
				MasterClipSelection = MasterBoxBaseSelection;
				const auto selectInBox = [&](const auto& clip, s32 type, s32 object, s32 index)
				{
					const float visibleEnd = std::min(MasterDuration, std::max(clip.Start + clip.Duration, clip.Start + 4.0f * MasterDuration / rulerWidth));
					if (clip.Track >= 0 && clip.Track < kCinemaMaxTracks && trackRows[clip.Track] >= 0 && clip.Start <= MasterDuration && CinemaMasterBoxIntersects(minTime, maxTime, clip.Start, visibleEnd) && CinemaMasterBoxIntersects(minRow, maxRow, static_cast<float>(trackRows[clip.Track]), static_cast<float>(trackRows[clip.Track] + 1)))
					{
						const SMasterClipSelection item{ type, object, index };
						if (std::find(MasterClipSelection.begin(), MasterClipSelection.end(), item) == MasterClipSelection.end())
						{
							MasterClipSelection.push_back(item);
						}
					}
				};
				for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
				{
					if (!Actors[i]->MasterTrack) continue;
					for (s32 j = 0; j < static_cast<s32>(Actors[i]->MasterClips.size()); ++j) selectInBox(Actors[i]->MasterClips[j], 1, i, j);
				}
				for (s32 i = 0; i < static_cast<s32>(Lights.size()); ++i)
				{
					if (!Lights[i].MasterTrack) continue;
					for (s32 j = 0; j < static_cast<s32>(Lights[i].MasterClips.size()); ++j) selectInBox(Lights[i].MasterClips[j], 2, i, j);
				}
				for (s32 i = 0; i < static_cast<s32>(Particles.size()); ++i)
				{
					if (!Particles[i].MasterTrack) continue;
					for (s32 j = 0; j < static_cast<s32>(Particles[i].MasterClips.size()); ++j) selectInBox(Particles[i].MasterClips[j], 3, i, j);
				}
				for (s32 i = 0; i < static_cast<s32>(Cameras.size()); ++i) selectInBox(Cameras[i], 4, i, 0);
				const SMasterClipSelection focus = MasterClipSelection.empty() ? SMasterClipSelection{} : MasterClipSelection.back();
				MasterClipAnchor = focus;
				MasterSelectedActor = focus.Type == 1 ? focus.Object : -1;
				MasterSelectedClip = focus.Type == 1 ? focus.Clip : -1;
				MasterSelectedLight = focus.Type == 2 ? focus.Object : -1;
				MasterSelectedLightClip = focus.Type == 2 ? focus.Clip : -1;
				MasterSelectedParticle = focus.Type == 3 ? focus.Object : -1;
				MasterSelectedParticleClip = focus.Type == 3 ? focus.Clip : -1;
				const ImVec2 boxMin(rulerStart + rulerWidth * minTime / MasterDuration, canvasTop + minRow * rowHeight);
				const ImVec2 boxMax(rulerStart + rulerWidth * maxTime / MasterDuration, canvasTop + maxRow * rowHeight);
				draw->PushClipRect(ImVec2(rulerStart, canvasTop), ImVec2(rulerStart + rulerWidth, y), true);
				draw->AddRectFilled(boxMin, boxMax, ImGui::GetColorU32(ImGuiCol_Header, 0.35f));
				draw->AddRect(boxMin, boxMax, ImGui::GetColorU32(ImGuiCol_HeaderActive));
				draw->PopClipRect();
			}
			if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
			{
				MasterBoxSelecting = false;
				MasterBoxBaseSelection.clear();
			}
		}
		if (ImGui::IsWindowHovered() && MasterPanCandidate && !MasterPanDragged && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && !contextOverClip &&
			io.MousePos.x >= rulerStart && io.MousePos.x <= rulerStart + rulerWidth && io.MousePos.y >= origin.y + rowHeight && io.MousePos.y < y)
		{
			float start = (io.MousePos.x - rulerStart) * MasterDuration / rulerWidth;
			if (MasterSnap && !io.KeyShift) start = roundf(start / MasterSnapStep) * MasterSnapStep;
			MasterContextStart = std::clamp(start, 0.0f, std::min(3600.0f, std::max(0.0f, MasterDuration - 0.01f)));
			MasterContextTrack = visibleTracks[std::clamp(static_cast<s32>((io.MousePos.y - origin.y - rowHeight) / rowHeight), 0, static_cast<s32>(visibleTracks.size()) - 1)];
			ImGui::OpenPopup("##master_add_pattern_context");
		}
		if (ImGui::BeginPopup("##master_add_pattern_context"))
		{
			if (ImGui::BeginMenu("Add pattern"))
			{
				if (ImGui::MenuItem("Actor")) addActorPattern(MasterContextStart, MasterContextTrack);
				if (ImGui::MenuItem("Particle")) addParticlePattern(MasterContextStart, MasterContextTrack);
				if (ImGui::MenuItem("Light")) addLightPattern(MasterContextStart, MasterContextTrack);
				if (ImGui::MenuItem("Camera")) addCameraPattern(MasterContextStart, MasterContextTrack);
				ImGui::EndMenu();
			}
			ImGui::EndPopup();
		}
		if (pendingTrack >= 0)
		{
			MoveMasterClipTracks(pendingTrackClip, pendingTrack, visibleTracks);
		}
		if (MasterUseLoopRange)
		{
			const float first = rulerStart + rulerWidth * MasterLoopStart / MasterDuration;
			const float last = rulerStart + rulerWidth * MasterLoopEnd / MasterDuration;
			draw->AddRectFilled(ImVec2(first, origin.y), ImVec2(last, origin.y + rowHeight), IM_COL32(190, 75, 75, 100));
			draw->AddLine(ImVec2(first, origin.y), ImVec2(first, y), IM_COL32(105, 200, 130, 180));
			draw->AddLine(ImVec2(last, origin.y), ImVec2(last, y), IM_COL32(105, 200, 130, 180));
		}
		const float contentEndX = rulerStart + rulerWidth * MasterContentEnd() / MasterDuration;
		draw->AddLine(ImVec2(contentEndX, origin.y), ImVec2(contentEndX, y), IM_COL32(220, 130, 90, 200));
		const float playhead = rulerStart + rulerWidth * MasterTime / MasterDuration;
		draw->AddLine(ImVec2(playhead, origin.y), ImVec2(playhead, y), ImGui::GetColorU32(ImGuiCol_SliderGrabActive), 2.0f);
		ImGui::SetCursorScreenPos(origin);
		ImGui::Dummy(ImVec2(labelWidth + rulerWidth, y - origin.y));
		ImGui::EndChild();
		if (ImGui::IsMouseReleased(ImGuiMouseButton_Right) || !ImGui::IsMouseDown(ImGuiMouseButton_Right))
		{
			MasterPanCandidate = false;
			MasterPanDragged = false;
		}
		if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
		{
			if (MasterDuplicateDrag && MasterDuplicateMoved && MasterSnap && !io.KeyShift)
			{
				if (const float* start = duplicateStart())
				{
					MoveMasterClips(roundf(*start / MasterSnapStep) * MasterSnapStep - *start);
				}
			}
			MasterDuplicateDrag = false;
			MasterDuplicateMoved = false;
			MasterGroupResize = false;
			MasterDragMoved = false;
			MasterShiftClickPending = false;
			MasterSingleClickPending = false;
			MasterShiftPreviousSelection.clear();
		}
		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows) && !ImGui::GetIO().WantTextInput &&
			!ImGui::IsAnyItemActive() && !MasterBoxSelecting && !Gizmo.Active)
		{
			if (ImGui::GetIO().KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_A, false))
			{
				MasterClipSelection = visibleMasterClips;
				MasterClipAnchor = MasterClipSelection.empty() ? SMasterClipSelection{} : MasterClipSelection.back();
				MasterSelectedActor = MasterClipAnchor.Type == 1 ? MasterClipAnchor.Object : -1;
				MasterSelectedClip = MasterClipAnchor.Type == 1 ? MasterClipAnchor.Clip : -1;
				MasterSelectedLight = MasterClipAnchor.Type == 2 ? MasterClipAnchor.Object : -1;
				MasterSelectedLightClip = MasterClipAnchor.Type == 2 ? MasterClipAnchor.Clip : -1;
				MasterSelectedParticle = MasterClipAnchor.Type == 3 ? MasterClipAnchor.Object : -1;
				MasterSelectedParticleClip = MasterClipAnchor.Type == 3 ? MasterClipAnchor.Clip : -1;
			}
			if (!MasterClipSelection.empty() && ImGui::IsKeyPressed(ImGuiKey_Delete, false))
			{
				clipActionType = MasterClipSelection.front().Type;
				clipActionObject = MasterClipSelection.front().Object;
				clipActionIndex = MasterClipSelection.front().Clip;
				clipActionDuplicate = false;
			}
		}
		if (clipActionType != 0)
		{
			MasterPatternEditorOpen = false;
			const SMasterClipSelection clicked{ clipActionType, clipActionObject, clipActionIndex };
			xr_vector<SMasterClipSelection> targets = std::find(MasterClipSelection.begin(), MasterClipSelection.end(), clicked) != MasterClipSelection.end() ? MasterClipSelection : xr_vector<SMasterClipSelection>{ clicked };
			const size_t originalActors = Actors.size();
			const size_t originalLights = Lights.size();
			const size_t originalParticles = Particles.size();
			const size_t originalCameras = Cameras.size();
			xr_vector<SMasterClipSelection> createdItems;
			SMasterClipSelection created;
			std::sort(targets.begin(), targets.end(), [](const SMasterClipSelection& a, const SMasterClipSelection& b)
			{
				if (a.Type != b.Type) return a.Type > b.Type;
				if (a.Object != b.Object) return a.Object > b.Object;
				return a.Clip > b.Clip;
			});
			for (const SMasterClipSelection& item : targets)
			{
				if (item.Type == 1 && item.Object >= 0 && item.Object < static_cast<s32>(Actors.size()))
				{
					SCinemaActor& source = *Actors[item.Object];
					if (item.Clip < 0 || item.Clip >= static_cast<s32>(source.MasterClips.size())) continue;
					if (clipActionDuplicate && Actors.size() < kCinemaSceneMaxActors)
					{
						SMasterClip copy = source.MasterClips[item.Clip];
						if (!shortcutDuplicate) copy.Start = std::min(std::min(3600.0f, MasterDuration), copy.Start + copy.Duration);
						const size_t count = Actors.size();
						SelectActor(item.Object);
						DuplicateSelectedActor();
						if (Actors.size() > count)
						{
							SCinemaActor& result = *Actors.back();
							result.Position = source.Position;
							result.Trims = source.Trims;
							result.PingPong = source.PingPong;
							result.ItemPosition = source.ItemPosition;
							result.ItemRotation = source.ItemRotation;
							result.ItemBoneName = source.ItemBoneName;
							RefreshActorItemBones(result);
							result.MasterTrack = result.PatternOwned = true;
							result.MasterClips = { copy };
							const SMasterClipSelection copyItem{ 1, static_cast<s32>(Actors.size()) - 1, 0 };
							createdItems.push_back(copyItem);
							if (item == clicked) created = copyItem;
						}
					}
					else if (!clipActionDuplicate)
					{
						source.MasterClips.erase(source.MasterClips.begin() + item.Clip);
						if (source.PatternOwned && source.MasterClips.empty()) { SelectActor(item.Object); DeleteSelectedActor(); }
					}
				}
				else if (item.Type == 2 && item.Object >= 0 && item.Object < static_cast<s32>(Lights.size()))
				{
					if (item.Clip < 0 || item.Clip >= static_cast<s32>(Lights[item.Object].MasterClips.size())) continue;
					if (clipActionDuplicate)
					{
						SMasterLightClip copy = Lights[item.Object].MasterClips[item.Clip];
						const Fvector position = Lights[item.Object].Position;
						if (!shortcutDuplicate) copy.Start = std::min(std::min(3600.0f, MasterDuration), copy.Start + copy.Duration);
						const size_t count = Lights.size();
						DuplicateLights({ item.Object });
						if (Lights.size() > count)
						{
							SCinemaLight& result = Lights.back();
							result.Position = position;
							result.MasterTrack = result.PatternOwned = true;
							result.MasterClips = { copy };
							const SMasterClipSelection copyItem{ 2, static_cast<s32>(Lights.size()) - 1, 0 };
							createdItems.push_back(copyItem);
							if (item == clicked) created = copyItem;
						}
					}
					else
					{
						Lights[item.Object].MasterClips.erase(Lights[item.Object].MasterClips.begin() + item.Clip);
						if (Lights[item.Object].PatternOwned && Lights[item.Object].MasterClips.empty()) RemoveLights({ item.Object });
					}
				}
				else if (item.Type == 3 && item.Object >= 0 && item.Object < static_cast<s32>(Particles.size()))
				{
					if (item.Clip < 0 || item.Clip >= static_cast<s32>(Particles[item.Object].MasterClips.size())) continue;
					if (clipActionDuplicate)
					{
						SMasterParticleClip copy = Particles[item.Object].MasterClips[item.Clip];
						if (!shortcutDuplicate) copy.Start = std::min(std::min(3600.0f, MasterDuration), copy.Start + copy.Duration);
						const size_t count = Particles.size();
						DuplicateParticles({ item.Object });
						if (Particles.size() > count)
						{
							SCinemaParticle& result = Particles.back();
							result.MasterTrack = result.PatternOwned = true;
							result.MasterClips = { copy };
							const SMasterClipSelection copyItem{ 3, static_cast<s32>(Particles.size()) - 1, 0 };
							createdItems.push_back(copyItem);
							if (item == clicked) created = copyItem;
						}
					}
					else
					{
						Particles[item.Object].MasterClips.erase(Particles[item.Object].MasterClips.begin() + item.Clip);
						Particles[item.Object].MasterPlayingClip = -2;
						if (Particles[item.Object].PatternOwned && Particles[item.Object].MasterClips.empty()) RemoveParticles({ item.Object });
					}
				}
				else if (item.Type == 4 && item.Object >= 0 && item.Object < static_cast<s32>(Cameras.size()))
				{
					ClearGizmo();
					if (clipActionDuplicate && Cameras.size() < kCinemaSceneMaxActors)
					{
						SCinemaCameraClip copy = Cameras[item.Object];
						if (!shortcutDuplicate) copy.Start = std::min(std::min(3600.0f, MasterDuration), copy.Start + copy.Duration);
						Cameras.push_back(std::move(copy));
						const SMasterClipSelection copyItem{ 4, static_cast<s32>(Cameras.size()) - 1, 0 };
						createdItems.push_back(copyItem);
						if (item == clicked) created = copyItem;
					}
					else if (!clipActionDuplicate)
					{
						if (CurveEditor.Type == 4 || CurveEditor.Type == 5) CurveEditor.Open = false;
						Cameras.erase(Cameras.begin() + item.Object);
						SelectedCameraKey = SelectedCameraKeyCamera = -1;
					}
				}
			}
			if (clipActionDuplicate && createdItems.size() != targets.size())
			{
				while (Actors.size() > originalActors)
				{
					SelectActor(static_cast<s32>(Actors.size()) - 1);
					DeleteSelectedActor();
				}
				while (Lights.size() > originalLights) RemoveLights({ static_cast<s32>(Lights.size()) - 1 });
				while (Particles.size() > originalParticles) RemoveParticles({ static_cast<s32>(Particles.size()) - 1 });
				Cameras.resize(originalCameras);
				MasterClipSelection = targets;
				MasterClipAnchor = clicked;
				MasterSelectedActor = clicked.Type == 1 ? clicked.Object : -1;
				MasterSelectedClip = clicked.Type == 1 ? clicked.Clip : -1;
				MasterSelectedLight = clicked.Type == 2 ? clicked.Object : -1;
				MasterSelectedLightClip = clicked.Type == 2 ? clicked.Clip : -1;
				MasterSelectedParticle = clicked.Type == 3 ? clicked.Object : -1;
				MasterSelectedParticleClip = clicked.Type == 3 ? clicked.Clip : -1;
				ShowSceneDialog(ESceneDialog::Info, "Duplicate clips", { "Could not duplicate all selected clips. Check object limits and available resources." });
			}
			else if (shortcutDuplicate && created.Type != 0)
			{
				MasterClipSelection = std::move(createdItems);
				MasterClipAnchor = created;
				MasterSelectedActor = created.Type == 1 ? created.Object : -1;
				MasterSelectedClip = created.Type == 1 ? created.Clip : -1;
				MasterSelectedLight = created.Type == 2 ? created.Object : -1;
				MasterSelectedLightClip = created.Type == 2 ? created.Clip : -1;
				MasterSelectedParticle = created.Type == 3 ? created.Object : -1;
				MasterSelectedParticleClip = created.Type == 3 ? created.Clip : -1;
				MasterDuplicateTarget = created;
				MasterDuplicateDrag = ImGui::IsMouseDown(ImGuiMouseButton_Left);
				MasterDuplicateMoved = false;
				if (const float* start = duplicateStart()) MasterDragTarget = *start;
			}
			else
			{
				MasterClipSelection.clear();
				MasterClipAnchor = {};
				MasterSelectedActor = MasterSelectedLight = MasterSelectedParticle = -1;
				MasterSelectedClip = MasterSelectedLightClip = MasterSelectedParticleClip = -1;
			}
		}
		ImGui::SetCursorScreenPos(inspectorPosition);
		ImGui::BeginChild("##master_inspector", ImVec2(objectsWidth, 0.0f), true, ImGuiWindowFlags_NoMove);
		ImGui::PushTextWrapPos(0.0f);
		ImGui::TextUnformatted("Clip settings");
		if (MasterSelectedActor >= 0 && MasterSelectedActor < static_cast<s32>(Actors.size()))
		{
			SCinemaActor& actor = *Actors[MasterSelectedActor];
			ImGui::TextWrapped("Actor: %s", actor.Name.c_str());
			ImGui::BeginDisabled(!actor.MasterTrack || actor.PatternOwned);
			if (ImGui::Button("Add clip"))
			{
				ImGui::OpenPopup("##master_add_clip");
			}
			ImGui::EndDisabled();
			if (ImGui::BeginPopup("##master_add_clip"))
			{
				for (const SAnimationSource& source : actor.Sources)
				{
					if (ImGui::BeginMenu(CinemaFileName(source.Id.c_str())))
					{
						for (const SAnimationClip& clip : source.Clips)
						{
							if (!clip.Fx && ImGui::MenuItem(clip.Name.c_str()) && AddMasterClip(actor, source.Id, clip.Name, MasterTime))
							{
								MasterSelectedClip = static_cast<s32>(actor.MasterClips.size()) - 1;
								MasterClipSelection = { { 1, MasterSelectedActor, MasterSelectedClip } };
								MasterClipAnchor = MasterClipSelection.front();
							}
						}
						ImGui::EndMenu();
					}
				}
				ImGui::EndPopup();
			}
			if (MasterSelectedClip >= 0 && MasterSelectedClip < static_cast<s32>(actor.MasterClips.size()))
			{
				SMasterClip& clip = actor.MasterClips[MasterSelectedClip];
				ImGui::Separator();
				if (!actor.PatternOwned)
				{
					if (ImGui::Button("Choose animation##pattern")) ImGui::OpenPopup("##pattern_animation");
					if (ImGui::BeginPopup("##pattern_animation"))
					{
						for (const SAnimationSource& source : actor.Sources)
						{
							if (ImGui::BeginMenu(CinemaFileName(source.Id.c_str())))
							{
								for (const SAnimationClip& animation : source.Clips)
								{
									if (!animation.Fx && ImGui::MenuItem(animation.Name.c_str()))
									{
										clip.Source = source.Id;
										clip.Name = animation.Name;
										clip.SourceIn = clip.SourceOut = 0.0f;
										clip.Speed = 1.0f;
										clip.Shots.clear();
									}
								}
								ImGui::EndMenu();
							}
						}
						ImGui::EndPopup();
					}
					ImGui::TextWrapped("%s", clip.Name.size() ? clip.Name.c_str() : "<choose animation>");
					ImGui::TextDisabled("%s", clip.Source.size() ? CinemaFileName(clip.Source.c_str()) : "<none>");
				}
				ImGui::TextUnformatted("Start (s)");
				ImGui::SetNextItemWidth(-1.0f);
				if (ImGui::DragFloat("##master_clip_start", &clip.Start, 0.05f, 0.0f, 3600.0f, "%.2f"))
				{
					clip.Start = std::isfinite(clip.Start) ? std::clamp(clip.Start, 0.0f, 3600.0f) : 0.0f;
				}
				ImGui::TextUnformatted("Duration (s)");
				ImGui::SetNextItemWidth(-1.0f);
				if (ImGui::DragFloat("##master_clip_duration", &clip.Duration, 0.05f, 0.01f, 3600.0f, "%.2f"))
				{
					clip.Duration = std::isfinite(clip.Duration) ? std::clamp(clip.Duration, 0.01f, 3600.0f) : 1.0f;
				}
				if (!actor.PatternOwned)
				{
					const SAnimationSource* clipSource = FindSource(actor, clip.Source);
					const SAnimationClip* animation = clipSource ? FindClip(*clipSource, clip.Name, false) : nullptr;
					if (actor.Animated != nullptr && animation != nullptr && EnsureSource(actor, clip.Source))
					{
						const MotionID motion = CinemaFindMotion(actor, *clipSource, *animation);
						const float length = motion.valid() ? actor.Animated->get_animation_length(motion) : 0.0f;
						if (std::isfinite(length) && length >= 0.01f)
						{
							float sourceOut = clip.SourceOut > 0.0f ? std::min(clip.SourceOut, length) : length;
							float sourceIn = std::clamp(clip.SourceIn, 0.0f, std::max(0.0f, sourceOut - 0.01f));
							float speed = clip.Speed;
							bool changed = false;
							ImGui::TextUnformatted("Animation IN (s)");
							ImGui::SetNextItemWidth(-1.0f);
							changed |= ImGui::DragFloat("##master_source_in", &sourceIn, 0.01f, 0.0f, length, "%.3f");
							ImGui::TextUnformatted("Animation OUT (s)");
							ImGui::SetNextItemWidth(-1.0f);
							changed |= ImGui::DragFloat("##master_source_out", &sourceOut, 0.01f, 0.0f, length, "%.3f");
							ImGui::TextUnformatted("Animation speed");
							ImGui::SetNextItemWidth(-1.0f);
							changed |= ImGui::DragFloat("##master_source_speed", &speed, 0.01f, 0.05f, 4.0f, "%.2fx");
							if (changed)
							{
								sourceIn = std::isfinite(sourceIn) ? std::clamp(sourceIn, 0.0f, std::max(0.0f, length - 0.01f)) : 0.0f;
								sourceOut = std::isfinite(sourceOut) ? std::clamp(sourceOut, sourceIn + 0.01f, length) : length;
								speed = std::isfinite(speed) ? std::clamp(speed, 0.05f, 4.0f) : 1.0f;
								clip.SourceIn = sourceIn;
								clip.SourceOut = sourceOut;
								clip.Speed = speed;
								clip.Duration = std::clamp((sourceOut - sourceIn) / speed, 0.01f, 3600.0f);
							}
						}
					}
					else
					{
						ImGui::TextWrapped("Animation source is unavailable; trim and speed cannot be edited.");
					}
					ImGui::TextWrapped("Changing IN, OUT or speed sets the clip duration. A longer duration holds the last pose.");
					ImGui::Separator();
					ImGui::TextUnformatted("Shot markers");
					const bool canShoot = actor.ItemSection.size() > 0 && pSettings != nullptr && pSettings->line_exist(actor.ItemSection.c_str(), "fire_point") && pSettings->line_exist(actor.ItemSection.c_str(), "ammo_class");
					const bool insideClip = MasterTime >= clip.Start && MasterTime < clip.Start + clip.Duration;
					ImGui::BeginDisabled(!canShoot || !insideClip || clip.Shots.size() >= kCinemaMaxShots);
					if (ImGui::Button("Add shot at playhead"))
					{
						clip.Shots.push_back(CinemaMasterSourceTime(clip.SourceIn, clip.Speed, MasterTime - clip.Start));
					}
					ImGui::EndDisabled();
					if (!canShoot)
					{
						ImGui::TextWrapped("Attach a weapon with fire_point and ammo_class to add shots.");
					}
					else if (!insideClip)
					{
						ImGui::TextWrapped("Move the playhead inside the clip to add a shot.");
					}
					for (s32 shotIndex = 0; shotIndex < static_cast<s32>(clip.Shots.size()); ++shotIndex)
					{
						ImGui::PushID(shotIndex);
						const float removeWidth = ImGui::CalcTextSize("Remove").x + 2.0f * ImGui::GetStyle().FramePadding.x;
						ImGui::SetNextItemWidth(std::max(1.0f, ImGui::GetContentRegionAvail().x - removeWidth - ImGui::GetStyle().ItemSpacing.x));
						if (ImGui::DragFloat("##shot_time", &clip.Shots[shotIndex], 0.01f, 0.0f, 3600.0f, "%.2f s"))
						{
							clip.Shots[shotIndex] = std::isfinite(clip.Shots[shotIndex]) ? std::clamp(clip.Shots[shotIndex], 0.0f, 3600.0f) : 0.0f;
						}
						ImGui::SameLine();
						const bool remove = ImGui::SmallButton("Remove");
						ImGui::PopID();
						if (remove)
						{
							clip.Shots.erase(clip.Shots.begin() + shotIndex);
							--shotIndex;
						}
					}
					ImGui::TextWrapped("Shot times refer to the source animation and follow trim and speed. Shots outside the clip do not fire.");
				}
				else
				{
					ImGui::TextWrapped("Edit the animation playlist and shot markers in Pattern settings.");
				}
				if (ImGui::Button("Remove clip"))
				{
					actor.MasterClips.erase(actor.MasterClips.begin() + MasterSelectedClip);
					if (actor.PatternOwned && actor.MasterClips.empty())
					{
						SelectActor(MasterSelectedActor);
						DeleteSelectedActor();
						MasterSelectedActor = -1;
					}
					MasterClipSelection.clear();
					MasterClipAnchor = {};
					MasterSelectedClip = -1;
				}
			}
		}
		else if (MasterSelectedLight >= 0 && MasterSelectedLight < static_cast<s32>(Lights.size()))
		{
			SCinemaLight& light = Lights[MasterSelectedLight];
			ImGui::Text("%s light %d", light.Type == 0 ? "Point" : "Spot", MasterSelectedLight + 1);
			ImGui::BeginDisabled(!light.MasterTrack || light.PatternOwned || light.MasterClips.size() >= kCinemaSceneMaxPlaylist);
			if (ImGui::Button("Add clip"))
			{
				SMasterLightClip clip;
				clip.Start = MasterClipStart();
				clip.Duration = std::max(0.01f, std::min(5.0f, MasterDuration - clip.Start));
				light.MasterClips.push_back(clip);
				MasterSelectedLightClip = static_cast<s32>(light.MasterClips.size()) - 1;
				MasterClipSelection = { { 2, MasterSelectedLight, MasterSelectedLightClip } };
				MasterClipAnchor = MasterClipSelection.front();
			}
			ImGui::EndDisabled();
			if (MasterSelectedLightClip >= 0 && MasterSelectedLightClip < static_cast<s32>(light.MasterClips.size()))
			{
				SMasterLightClip& clip = light.MasterClips[MasterSelectedLightClip];
				ImGui::TextUnformatted("Start (s)");
				ImGui::SetNextItemWidth(-1.0f);
				if (ImGui::DragFloat("##master_light_start", &clip.Start, 0.05f, 0.0f, 3600.0f, "%.2f"))
				{
					clip.Start = std::isfinite(clip.Start) ? std::clamp(clip.Start, 0.0f, 3600.0f) : 0.0f;
				}
				ImGui::TextUnformatted("Duration (s)");
				ImGui::SetNextItemWidth(-1.0f);
				if (ImGui::DragFloat("##master_light_duration", &clip.Duration, 0.05f, 0.01f, 3600.0f, "%.2f"))
				{
					clip.Duration = std::isfinite(clip.Duration) ? std::clamp(clip.Duration, 0.01f, 3600.0f) : 1.0f;
				}
				if (!light.PatternOwned)
				{
					if (ImGui::TreeNodeEx("Intensity and color##clip_light", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
					{
						ImGui::TextUnformatted("Intensity at start");
						ImGui::SetNextItemWidth(-1.0f);
						if (ImGui::DragFloat("##master_light_intensity_start", &clip.StartIntensity, 0.01f, 0.0f, 10.0f, "%.2f"))
						{
							clip.StartIntensity = std::isfinite(clip.StartIntensity) ? std::clamp(clip.StartIntensity, 0.0f, 10.0f) : 1.0f;
						}
						ImGui::TextUnformatted("Intensity at end");
						ImGui::SetNextItemWidth(-1.0f);
						if (ImGui::DragFloat("##master_light_intensity_end", &clip.EndIntensity, 0.01f, 0.0f, 10.0f, "%.2f"))
						{
							clip.EndIntensity = std::isfinite(clip.EndIntensity) ? std::clamp(clip.EndIntensity, 0.0f, 10.0f) : 1.0f;
						}
						ImGui::TextUnformatted("Color multiplier at start");
						ImGui::SetNextItemWidth(-1.0f);
						const Fvector oldStartColor = clip.StartColor;
						if (ImGui::DragFloat3("##master_light_color_start", &clip.StartColor.x, 0.01f, 0.0f, 10.0f))
						{
							if (!_valid(clip.StartColor))
							{
								clip.StartColor = oldStartColor;
							}
							clip.StartColor.x = std::clamp(clip.StartColor.x, 0.0f, 10.0f);
							clip.StartColor.y = std::clamp(clip.StartColor.y, 0.0f, 10.0f);
							clip.StartColor.z = std::clamp(clip.StartColor.z, 0.0f, 10.0f);
						}
						ImGui::TextUnformatted("Color multiplier at end");
						ImGui::SetNextItemWidth(-1.0f);
						const Fvector oldEndColor = clip.EndColor;
						if (ImGui::DragFloat3("##master_light_color_end", &clip.EndColor.x, 0.01f, 0.0f, 10.0f))
						{
							if (!_valid(clip.EndColor))
							{
								clip.EndColor = oldEndColor;
							}
							clip.EndColor.x = std::clamp(clip.EndColor.x, 0.0f, 10.0f);
							clip.EndColor.y = std::clamp(clip.EndColor.y, 0.0f, 10.0f);
							clip.EndColor.z = std::clamp(clip.EndColor.z, 0.0f, 10.0f);
						}
						ImGui::TextWrapped("Intensity and color multiply the base light and its modulation.");
						ImGui::TreePop();
					}
				}
				else
				{
					ImGui::TextWrapped("Edit light parameters in Pattern settings.");
				}
				if (ImGui::Button("Remove clip"))
				{
					light.MasterClips.erase(light.MasterClips.begin() + MasterSelectedLightClip);
					if (light.PatternOwned && light.MasterClips.empty())
					{
						RemoveLights({ MasterSelectedLight });
						MasterSelectedLight = -1;
					}
					MasterClipSelection.clear();
					MasterClipAnchor = {};
					MasterSelectedLightClip = -1;
				}
			}
		}
		else if (MasterSelectedParticle >= 0 && MasterSelectedParticle < static_cast<s32>(Particles.size()))
		{
			SCinemaParticle& particle = Particles[MasterSelectedParticle];
			ImGui::TextWrapped("%s", particle.Name.c_str());
			ImGui::BeginDisabled(!particle.MasterTrack || particle.PatternOwned || particle.MasterClips.size() >= kCinemaSceneMaxPlaylist);
			if (ImGui::Button("Add clip"))
			{
				SMasterParticleClip clip;
				clip.Start = MasterClipStart();
				clip.Duration = std::max(0.01f, std::min(5.0f, MasterDuration - clip.Start));
				particle.MasterClips.push_back(clip);
				MasterSelectedParticleClip = static_cast<s32>(particle.MasterClips.size()) - 1;
				MasterClipSelection = { { 3, MasterSelectedParticle, MasterSelectedParticleClip } };
				MasterClipAnchor = MasterClipSelection.front();
			}
			ImGui::EndDisabled();
			if (MasterSelectedParticleClip >= 0 && MasterSelectedParticleClip < static_cast<s32>(particle.MasterClips.size()))
			{
				SMasterParticleClip& clip = particle.MasterClips[MasterSelectedParticleClip];
				ImGui::TextUnformatted("Start (s)");
				ImGui::SetNextItemWidth(-1.0f);
				if (ImGui::DragFloat("##master_particle_start", &clip.Start, 0.05f, 0.0f, 3600.0f, "%.2f"))
				{
					clip.Start = std::isfinite(clip.Start) ? std::clamp(clip.Start, 0.0f, 3600.0f) : 0.0f;
				}
				ImGui::TextUnformatted("Duration (s)");
				ImGui::SetNextItemWidth(-1.0f);
				if (ImGui::DragFloat("##master_particle_duration", &clip.Duration, 0.05f, 0.01f, 3600.0f, "%.2f"))
				{
					clip.Duration = std::isfinite(clip.Duration) ? std::clamp(clip.Duration, 0.01f, 3600.0f) : 1.0f;
				}
				ImGui::TextWrapped("Seeking restarts the effect; earlier particles cannot be reconstructed.");
				if (ImGui::Button("Remove clip"))
				{
					particle.MasterPlayingClip = -2;
					particle.MasterClips.erase(particle.MasterClips.begin() + MasterSelectedParticleClip);
					if (particle.PatternOwned && particle.MasterClips.empty())
					{
						RemoveParticles({ MasterSelectedParticle });
						MasterSelectedParticle = -1;
					}
					MasterClipSelection.clear();
					MasterClipAnchor = {};
					MasterSelectedParticleClip = -1;
				}
			}
		}
		else if (MasterClipAnchor.Type == 4 && MasterClipAnchor.Object >= 0 && MasterClipAnchor.Object < static_cast<s32>(Cameras.size()))
		{
			SCinemaCameraClip& camera = Cameras[MasterClipAnchor.Object];
			ImGui::TextUnformatted("Camera");
			ImGui::TextUnformatted("Start (s)");
			ImGui::SetNextItemWidth(-1.0f);
			if (ImGui::DragFloat("##camera_start", &camera.Start, 0.05f, 0.0f, 3600.0f, "%.2f")) camera.Start = std::isfinite(camera.Start) ? std::clamp(camera.Start, 0.0f, 3600.0f) : 0.0f;
			ImGui::TextUnformatted("Duration (s)");
			ImGui::SetNextItemWidth(-1.0f);
			if (ImGui::DragFloat("##camera_duration", &camera.Duration, 0.05f, 0.01f, 3600.0f, "%.2f")) camera.Duration = std::isfinite(camera.Duration) ? std::clamp(camera.Duration, 0.01f, 3600.0f) : 1.0f;
			ImGui::Checkbox("Stretch to clip", &camera.Stretch);
			ImGui::Checkbox("Bezier", &camera.Smooth);
			if (ImGui::Button("Remove clip##camera"))
			{
				ClearGizmo();
				if (CurveEditor.Type == 4 || CurveEditor.Type == 5) CurveEditor.Open = false;
				Cameras.erase(Cameras.begin() + MasterClipAnchor.Object);
				SelectedCameraKey = SelectedCameraKeyCamera = -1;
				MasterClipSelection.clear();
				MasterClipAnchor = {};
				MasterPatternEditorOpen = false;
			}
		}
		else
		{
			ImGui::TextDisabled("Select a pattern on the timeline.");
		}
		ImGui::PopTextWrapPos();
		ImGui::EndChild();
		if (MasterRenameRequested)
		{
			ImGui::OpenPopup("Rename clip##cinema");
			MasterRenameRequested = false;
			ImGui::SetNextWindowFocus();
		}
		if (ImGui::BeginPopupModal("Rename clip##cinema", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
		{
			if (ImGui::IsWindowAppearing()) ImGui::SetKeyboardFocusHere();
			const bool enter = ImGui::InputText("Name", MasterRenameBuffer, sizeof(MasterRenameBuffer), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_AutoSelectAll);
			if (enter || ImGui::Button("Apply"))
			{
				const auto properties = MasterClipProperties(MasterRenameTarget);
				if (properties.DisplayName) *properties.DisplayName = MasterRenameBuffer;
				ImGui::CloseCurrentPopup();
			}
			ImGui::SameLine();
			if (ImGui::Button("Cancel")) ImGui::CloseCurrentPopup();
			ImGui::EndPopup();
		}
		DrawSceneDialog();
	}
	ImGui::End();
	if (PresetsOpen)
	{
		ImGui::SetNextWindowSize(ImVec2(600.0f, 600.0f), ImGuiCond_FirstUseEver);
		if (ImGui::Begin("Presets##cinema_presets", &PresetsOpen))
		{
			DrawPresetTab();
		}
		ImGui::End();
	}
	if (MasterPatternEditorOpen)
	{
		const s32 type = MasterEditingPattern.Type;
		const s32 index = MasterEditingPattern.Object;
		const s32 clip = MasterEditingPattern.Clip;
		const bool valid = (type == 1 && index >= 0 && index < static_cast<s32>(Actors.size()) && clip >= 0 && clip < static_cast<s32>(Actors[index]->MasterClips.size())) ||
			(type == 2 && index >= 0 && index < static_cast<s32>(Lights.size()) && clip >= 0 && clip < static_cast<s32>(Lights[index].MasterClips.size())) ||
			(type == 3 && index >= 0 && index < static_cast<s32>(Particles.size()) && clip >= 0 && clip < static_cast<s32>(Particles[index].MasterClips.size())) ||
			(type == 4 && index >= 0 && index < static_cast<s32>(Cameras.size()) && clip == 0);
		if (!valid)
		{
			MasterPatternEditorOpen = false;
		}
		else
		{
			ImGui::SetNextWindowSize(type == 1 ? ImVec2(1050.0f, 700.0f) : ImVec2(650.0f, 700.0f), ImGuiCond_FirstUseEver);
			if (type == 1) ImGui::SetNextWindowSizeConstraints(ImVec2(500.0f, 350.0f), ImVec2(FLT_MAX, FLT_MAX));
			if (ImGui::Begin(type == 1 ? "Actor settings###cinema_actor_pattern" : "Pattern settings###cinema_pattern", &MasterPatternEditorOpen))
			{
				const size_t previousCount = type == 1 ? Actors.size() : type == 2 ? Lights.size() : Particles.size();
				if (type == 1 && Selected == index)
				{
					if (ImGui::BeginTabBar("##cinema_actor_tabs"))
					{
						if (ImGui::BeginTabItem("Model")) { DrawModelTab(); ImGui::EndTabItem(); }
						if (ImGui::BeginTabItem("Actor settings")) { DrawSelectedActor(); ImGui::EndTabItem(); }
						if (ImGui::BeginTabItem("Item")) { DrawItemTab(); ImGui::EndTabItem(); }
						if (ImGui::BeginTabItem("Animations")) { DrawAnimationTab(); ImGui::EndTabItem(); }
						ImGui::EndTabBar();
					}
				}
				else if (type == 2) DrawLightTab();
				else if (type == 3) DrawParticleTab();
				else if (type == 4)
				{
					SCinemaCameraClip& camera = Cameras[index];
					const float cameraLocalTime = std::clamp(MasterTime - camera.Start, 0.0f, camera.Duration);
					const bool cameraStretch = CinemaCameraStretchAt(camera, cameraLocalTime);
					const auto selectKey = [&](s32 keyIndex)
					{
						ClearGizmo();
						SelectedCameraKey = keyIndex;
						SelectedCameraKeyCamera = index;
						MasterClipSelection = { { 4, index, 0 } };
						MasterClipAnchor = MasterClipSelection.front();
						MasterSelectedActor = MasterSelectedLight = MasterSelectedParticle = -1;
					};
					const auto seekToKey = [&](s32 keyIndex)
					{
						if (!camera.Preview || keyIndex < 0 || keyIndex >= static_cast<s32>(camera.Keys.size())) return;
						const float localTime = CinemaCameraKeyLocalTime(camera, camera.Keys[keyIndex], cameraLocalTime);
						const float clipEnd = camera.Start + camera.Duration;
						MasterTime = std::clamp(camera.Start + localTime, 0.0f, MasterDuration);
						if (MasterTime >= clipEnd && clipEnd > camera.Start) MasterTime = std::nextafter(clipEnd, camera.Start);
						StopPatternPreviews();
						MasterActive = true;
						MasterIncludeStart = true;
						MasterParticleSeek = true;
					};
					const auto removeKey = [&](s32 keyIndex)
					{
						if (keyIndex < 0 || keyIndex >= static_cast<s32>(camera.Keys.size())) return;
						ClearGizmo();
						camera.Keys.erase(camera.Keys.begin() + keyIndex);
						if (SelectedCameraKey == keyIndex) SelectedCameraKey = -1;
						else if (SelectedCameraKey > keyIndex) --SelectedCameraKey;
						CameraTimelineDragKey = -1;
						CameraTimelineContextKey = -1;
					};
					ImGui::TextWrapped("Camera keys use time relative to the start of the clip.");
					ImGui::Checkbox("Preview camera", &camera.Preview);
					float fov = camera.Fov > 0.0f ? camera.Fov : std::isfinite(Device.fFOV) ? std::clamp(Device.fFOV, 1.0f, 179.0f) : 90.0f;
					if (ImGui::DragFloat("FOV (degrees)", &fov, 0.1f, 1.0f, 179.0f, "%.1f", ImGuiSliderFlags_AlwaysClamp))
					{
						camera.Fov = std::isfinite(fov) ? std::clamp(fov, 1.0f, 179.0f) : 90.0f;
					}
					if (ImGui::Button("Open camera automation curves"))
					{
						CurveEditor = {};
						CurveEditor.Open = true;
						CurveEditor.Type = 5;
						CurveEditor.Object = index;
					}
					ImGui::TextWrapped("Position and rotation curves add offsets to camera keys. FOV curves override the base FOV; discrete channels use step values.");
					ImGui::TextDisabled("Disable preview to position the view before adding a key.");
					if (ImGui::TreeNodeEx("Camera shake##camera", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
					{
						ImGui::BeginDisabled(camera.Shakes.size() >= 32);
						if (ImGui::Button("Add shake layer")) camera.Shakes.emplace_back();
						ImGui::EndDisabled();
						if (camera.Keys.empty()) ImGui::TextDisabled("Add a camera key to preview shake.");
						for (s32 layer = 0; layer < static_cast<s32>(camera.Shakes.size()); ++layer)
						{
							ImGui::PushID(layer);
							auto& shake = camera.Shakes[layer];
							char label[128];
							xr_sprintf(label, "Layer %d: %s", layer + 1, CinemaShakePresets[shake.Preset].Name);
							const bool open = ImGui::TreeNodeEx(label, ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth);
							bool remove = false;
							if (ImGui::BeginPopupContextItem("##shake_layer_context"))
							{
								remove = ImGui::MenuItem("Remove layer");
								ImGui::EndPopup();
							}
							if (open)
							{
								ImGui::Checkbox("Enabled", &shake.Enabled);
								if (ImGui::Button("Open modulation curves"))
								{
									CurveEditor = {};
									CurveEditor.Open = true;
									CurveEditor.Type = 4;
									CurveEditor.Object = index;
									CurveEditor.Layer = layer;
								}
								if (ImGui::BeginCombo("Preset", CinemaShakePresets[shake.Preset].Name))
								{
									for (s32 i = 0; i < static_cast<s32>(IM_ARRAYSIZE(CinemaShakePresets)); ++i)
									{
										if (ImGui::Selectable(CinemaShakePresets[i].Name, shake.Preset == i)) shake.Preset = i;
									}
									ImGui::EndCombo();
								}
								ImGui::DragFloat("Influence", &shake.Influence, 0.01f, 0.0f, 4.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
								ImGui::DragFloat("Position scale", &shake.PositionScale, 0.01f, 0.0f, 100.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
								ImGui::DragFloat("Speed", &shake.Speed, 0.01f, 0.0f, 100.0f, "%.2fx", ImGuiSliderFlags_AlwaysClamp);
								ImGui::DragFloat("Time offset (s)", &shake.Offset, 0.01f, -3600.0f, 3600.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
								ImGui::Checkbox("Stretch to clip", &shake.Stretch);
								ImGui::DragFloat("Fade in (s)", &shake.FadeIn, 0.01f, 0.0f, 3600.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
								ImGui::DragFloat("Fade out (s)", &shake.FadeOut, 0.01f, 0.0f, 3600.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
								shake.Influence = std::isfinite(shake.Influence) ? std::clamp(shake.Influence, 0.0f, 4.0f) : 1.0f;
								shake.PositionScale = std::isfinite(shake.PositionScale) ? std::clamp(shake.PositionScale, 0.0f, 100.0f) : 1.0f;
								shake.Speed = std::isfinite(shake.Speed) ? std::clamp(shake.Speed, 0.0f, 100.0f) : 1.0f;
								shake.Offset = std::isfinite(shake.Offset) ? std::clamp(shake.Offset, -3600.0f, 3600.0f) : 0.0f;
								shake.FadeIn = std::isfinite(shake.FadeIn) ? std::clamp(shake.FadeIn, 0.0f, 3600.0f) : 0.0f;
								shake.FadeOut = std::isfinite(shake.FadeOut) ? std::clamp(shake.FadeOut, 0.0f, 3600.0f) : 0.0f;
								remove |= ImGui::Button("Remove layer");
								ImGui::TreePop();
							}
							if (remove)
							{
								if (CurveEditor.Type == 4 && CurveEditor.Object == index) CurveEditor.Open = false;
								camera.Shakes.erase(camera.Shakes.begin() + layer);
								ImGui::PopID();
								break;
							}
							ImGui::PopID();
						}
						ImGui::TreePop();
					}
					if (ImGui::Button("Fit clip to keys") && !camera.Keys.empty())
					{
						float last = 0.0f;
						for (const auto& key : camera.Keys) last = std::max(last, key.Time);
						const float lastInClip = cameraStretch ? last * camera.Duration / camera.SourceDuration : last;
						camera.SourceDuration = std::max(0.01f, last);
						camera.Duration = std::max(0.01f, lastInClip);
					}
					if (ImGui::Button("Add key from view") && camera.Keys.size() < 1024)
					{
						SCinemaCameraKey key;
						if (camera.Keys.empty()) camera.SourceDuration = camera.Duration;
						key.Time = std::clamp(MasterTime - camera.Start, 0.0f, camera.Duration);
						if (cameraStretch) key.Time *= camera.SourceDuration / camera.Duration;
						key.Position = Device.vCameraPosition;
						key.Orientation.i = Device.vCameraRight;
						key.Orientation.j = Device.vCameraTop;
						key.Orientation.k = Device.vCameraDirection;
						ClearGizmo();
						camera.Keys.push_back(key);
						selectKey(static_cast<s32>(camera.Keys.size()) - 1);
						MasterPlaying = false;
					}
					ImGui::SeparatorText("Key timeline");
					ImGui::BeginDisabled(camera.Keys.size() < 2);
					if (ImGui::Button("Distribute keys evenly"))
					{
						xr_vector<size_t> order;
						order.reserve(camera.Keys.size());
						for (size_t i = 0; i < camera.Keys.size(); ++i) order.push_back(i);
						std::stable_sort(order.begin(), order.end(), [&](size_t a, size_t b) { return camera.Keys[a].Time < camera.Keys[b].Time; });
						const float duration = cameraStretch ? camera.SourceDuration : camera.Duration;
						for (size_t i = 0; i < order.size(); ++i)
						{
							camera.Keys[order[i]].Time = duration * static_cast<float>(i) / static_cast<float>(order.size() - 1);
						}
						CameraTimelineDragKey = -1;
					}
					ImGui::EndDisabled();
					CinemaHint("Shift + click: add key from view | Drag marker: move key | Right-click marker: actions");
					if (!ImGui::IsMouseDown(ImGuiMouseButton_Left)) CameraTimelineDragKey = -1;
					float keyRange = camera.Duration;
					for (const auto& key : camera.Keys)
					{
						const float localTime = CinemaCameraKeyLocalTime(camera, key, cameraLocalTime);
						keyRange = std::max(keyRange, localTime);
					}
					if (CameraTimelineDragKey >= 0 && ImGui::IsMouseDown(ImGuiMouseButton_Left)) keyRange = CameraTimelineDragRange;
					const ImVec2 timelineOrigin = ImGui::GetCursorScreenPos();
					const float timelineWidth = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
					float localTime = std::clamp(MasterTime - camera.Start, 0.0f, keyRange);
					DrawAnimationTimeline("##camera_key_timeline", localTime, keyRange, nullptr, GetTheme(), nullptr, TrimDragMarker, TrimDragIndex, TrimDragKey, "", false);
					const ImVec2 mouse = ImGui::GetIO().MousePos;
					const auto keyLocalTime = [&](const SCinemaCameraKey& key)
					{
						return CinemaCameraKeyLocalTime(camera, key, cameraLocalTime);
					};
					const auto timeToX = [&](float time)
					{
						return timelineOrigin.x + std::clamp(time / keyRange, 0.0f, 1.0f) * timelineWidth;
					};
					s32 hoveredKey = -1;
					float hoverDistance = 7.0f;
					if (ImGui::IsItemHovered())
					{
						for (s32 k = 0; k < static_cast<s32>(camera.Keys.size()); ++k)
						{
							const float distance = fabsf(mouse.x - timeToX(keyLocalTime(camera.Keys[k])));
							if (distance < hoverDistance || (hoveredKey >= 0 && SelectedCameraKeyCamera == index && k == SelectedCameraKey && distance == hoverDistance))
							{
								hoverDistance = distance;
								hoveredKey = k;
							}
						}
					}
					if (ImGui::IsItemActivated())
					{
						CameraTimelineDragKey = hoveredKey;
						CameraTimelineDragRange = keyRange;
						if (hoveredKey < 0 && ImGui::GetIO().KeyShift && camera.Keys.size() < 1024)
						{
							if (camera.Keys.empty()) camera.SourceDuration = camera.Duration;
							const float time = std::clamp((mouse.x - timelineOrigin.x) / timelineWidth, 0.0f, 1.0f) * keyRange;
							SCinemaCameraKey key;
							key.Time = std::min(3600.0f, CinemaCameraStretchAt(camera, time) ? time * camera.SourceDuration / camera.Duration : time);
							key.Position = Device.vCameraPosition;
							key.Orientation.i = Device.vCameraRight;
							key.Orientation.j = Device.vCameraTop;
							key.Orientation.k = Device.vCameraDirection;
							ClearGizmo();
							camera.Keys.push_back(key);
							CameraTimelineDragKey = static_cast<s32>(camera.Keys.size()) - 1;
							hoveredKey = CameraTimelineDragKey;
						}
						if (hoveredKey >= 0)
						{
							selectKey(hoveredKey);
							if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left)) seekToKey(hoveredKey);
						}
					}
					if (ImGui::IsItemActive())
					{
						const float time = std::clamp((mouse.x - timelineOrigin.x) / timelineWidth, 0.0f, 1.0f) * keyRange;
						if (CameraTimelineDragKey >= 0 && CameraTimelineDragKey < static_cast<s32>(camera.Keys.size()))
						{
							if (ImGui::IsMouseDragging(ImGuiMouseButton_Left)) camera.Keys[CameraTimelineDragKey].Time = std::min(3600.0f, CinemaCameraStretchAt(camera, time) ? time * camera.SourceDuration / camera.Duration : time);
						}
						else
						{
							MasterTime = std::clamp(camera.Start + std::min(time, camera.Duration), 0.0f, MasterDuration);
							MasterActive = true;
							MasterIncludeStart = true;
							MasterParticleSeek = true;
						}
					}
					if (ImGui::IsItemDeactivated()) CameraTimelineDragKey = -1;
					ImDrawList* draw = ImGui::GetWindowDrawList();
					const ImU32 keyColor = IM_COL32(95, 190, 255, 255);
					const ImU32 selectedColor = IM_COL32(255, 200, 85, 255);
					if (camera.Duration < keyRange)
					{
						const float clipEnd = timeToX(camera.Duration);
						draw->AddRectFilled(ImVec2(clipEnd, timelineOrigin.y + 1.0f), ImVec2(timelineOrigin.x + timelineWidth - 1.0f, timelineOrigin.y + 63.0f), IM_COL32(0, 0, 0, 120));
						draw->AddLine(ImVec2(clipEnd, timelineOrigin.y), ImVec2(clipEnd, timelineOrigin.y + 64.0f), GetTheme().TimelineBorder);
					}
					for (s32 k = 0; k < static_cast<s32>(camera.Keys.size()); ++k)
					{
						const float x = timeToX(keyLocalTime(camera.Keys[k]));
						const ImU32 color = SelectedCameraKeyCamera == index && k == SelectedCameraKey ? selectedColor : keyColor;
						draw->AddLine(ImVec2(x, timelineOrigin.y + 9.0f), ImVec2(x, timelineOrigin.y + 55.0f), color, 2.0f);
						draw->AddCircleFilled(ImVec2(x, timelineOrigin.y + 59.0f), 4.0f, color);
					}
					if (hoveredKey >= 0 && ImGui::IsMouseReleased(ImGuiMouseButton_Right))
					{
						selectKey(hoveredKey);
						CameraTimelineContextKey = hoveredKey;
						CameraTimelineContextCamera = index;
						ImGui::OpenPopup("##camera_key_context");
					}
					if (ImGui::BeginPopup("##camera_key_context"))
					{
						if (CameraTimelineContextCamera != index || CameraTimelineContextKey < 0 || CameraTimelineContextKey >= static_cast<s32>(camera.Keys.size()))
						{
							ImGui::CloseCurrentPopup();
						}
						else if (ImGui::MenuItem("Remove key"))
						{
							removeKey(CameraTimelineContextKey);
							hoveredKey = -1;
						}
						ImGui::EndPopup();
					}
					if (hoveredKey >= 0)
					{
						ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
						ImGui::SetTooltip("Key %d: %.3f s\nDrag to move; right-click for actions", hoveredKey + 1, keyLocalTime(camera.Keys[hoveredKey]));
					}
					ImGui::TextDisabled("%.2f / %.2f s", std::clamp(MasterTime - camera.Start, 0.0f, camera.Duration), camera.Duration);
					for (s32 k = 0; k < static_cast<s32>(camera.Keys.size()); ++k)
					{
						ImGui::PushID(k);
						char label[64] = {};
						float keyTime = CinemaCameraKeyLocalTime(camera, camera.Keys[k], cameraLocalTime);
						xr_sprintf(label, "Key %d (%.2f s)", k + 1, keyTime);
						if (ImGui::Selectable(label, SelectedCameraKeyCamera == index && SelectedCameraKey == k))
						{
							selectKey(k);
						}
						if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left)) { selectKey(k); seekToKey(k); }
						ImGui::PopID();
					}
					if (SelectedCameraKeyCamera == index && SelectedCameraKey >= 0 && SelectedCameraKey < static_cast<s32>(camera.Keys.size()))
					{
						SCinemaCameraKey& key = camera.Keys[SelectedCameraKey];
						float keyTime = CinemaCameraKeyLocalTime(camera, key, cameraLocalTime);
						if (ImGui::DragFloat("Key time (s)", &keyTime, 0.01f, 0.0f, 3600.0f))
						{
							keyTime = std::isfinite(keyTime) ? std::clamp(keyTime, 0.0f, 3600.0f) : 0.0f;
							key.Time = std::min(3600.0f, CinemaCameraStretchAt(camera, keyTime) ? keyTime * camera.SourceDuration / camera.Duration : keyTime);
						}
						const Fvector oldPosition = key.Position;
						if (ImGui::DragFloat3("Position", &key.Position.x, 0.01f) && !_valid(key.Position)) key.Position = oldPosition;
						Fvector rotation; key.Orientation.getHPB(rotation); rotation.mul(rad2deg(1.0f));
						if (ImGui::DragFloat3("Rotation", &rotation.x, 0.1f) && _valid(rotation)) key.Orientation.setHPB(deg2rad(rotation.x), deg2rad(rotation.y), deg2rad(rotation.z));
						if (ImGui::Button("Remove key")) removeKey(SelectedCameraKey);
					}
				}
				else ImGui::TextDisabled("Select an actor with a timeline pattern to edit it.");
				if (type != 4) UpdatePatternSettingsSelection(type, previousCount);
				if (MasterPatternEditorOpen && type == 2 && SelectedLight == MasterEditingPattern.Object && MasterEditingPattern.Object >= 0 && MasterEditingPattern.Object < static_cast<s32>(Lights.size()))
				{
					SCinemaLight& light = Lights[MasterEditingPattern.Object];
					if (light.PatternOwned && MasterEditingPattern.Clip >= 0 && MasterEditingPattern.Clip < static_cast<s32>(light.MasterClips.size()))
					{
						SMasterLightClip& clip = light.MasterClips[MasterEditingPattern.Clip];
						if (ImGui::TreeNodeEx("Intensity and color##clip_light", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
						{
							ImGui::TextUnformatted("Intensity at start");
							ImGui::SetNextItemWidth(-1.0f);
							if (ImGui::DragFloat("##master_light_intensity_start", &clip.StartIntensity, 0.01f, 0.0f, 10.0f, "%.2f"))
							{
								clip.StartIntensity = std::isfinite(clip.StartIntensity) ? std::clamp(clip.StartIntensity, 0.0f, 10.0f) : 1.0f;
							}
							ImGui::TextUnformatted("Intensity at end");
							ImGui::SetNextItemWidth(-1.0f);
							if (ImGui::DragFloat("##master_light_intensity_end", &clip.EndIntensity, 0.01f, 0.0f, 10.0f, "%.2f"))
							{
								clip.EndIntensity = std::isfinite(clip.EndIntensity) ? std::clamp(clip.EndIntensity, 0.0f, 10.0f) : 1.0f;
							}
							ImGui::TextUnformatted("Color multiplier at start");
							ImGui::SetNextItemWidth(-1.0f);
							const Fvector oldStartColor = clip.StartColor;
							if (ImGui::DragFloat3("##master_light_color_start", &clip.StartColor.x, 0.01f, 0.0f, 10.0f))
							{
								if (!_valid(clip.StartColor))
								{
									clip.StartColor = oldStartColor;
								}
								clip.StartColor.x = std::clamp(clip.StartColor.x, 0.0f, 10.0f);
								clip.StartColor.y = std::clamp(clip.StartColor.y, 0.0f, 10.0f);
								clip.StartColor.z = std::clamp(clip.StartColor.z, 0.0f, 10.0f);
							}
							ImGui::TextUnformatted("Color multiplier at end");
							ImGui::SetNextItemWidth(-1.0f);
							const Fvector oldEndColor = clip.EndColor;
							if (ImGui::DragFloat3("##master_light_color_end", &clip.EndColor.x, 0.01f, 0.0f, 10.0f))
							{
								if (!_valid(clip.EndColor))
								{
									clip.EndColor = oldEndColor;
								}
								clip.EndColor.x = std::clamp(clip.EndColor.x, 0.0f, 10.0f);
								clip.EndColor.y = std::clamp(clip.EndColor.y, 0.0f, 10.0f);
								clip.EndColor.z = std::clamp(clip.EndColor.z, 0.0f, 10.0f);
							}
							ImGui::TextWrapped("Intensity and color multiply the base light and its modulation.");
							ImGui::TreePop();
						}
					}
				}
			}
			ImGui::End();
		}
	}
}

EGizmoAxis CCinemaFilmMakerManager::HitTranslateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const
{
	const SProjectedPoint origin = ProjectWorld(pivot);

	if (!origin.Visible)
	{
		return EGizmoAxis::None;
	}

	EGizmoAxis bestAxis = EGizmoAxis::None;
	float bestDistance = 12.0f;

	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		const Fvector endWorld = Fvector().mad(pivot, AxisVector(axis), size);
		const SProjectedPoint end = ProjectWorld(endWorld);

		if (!end.Visible)
		{
			continue;
		}

		const float distance = DistanceToSegment(mouse, origin.Screen, end.Screen);

		if (distance < bestDistance)
		{
			bestDistance = distance;
			bestAxis = axis;
		}
	}

	return bestAxis;
}

EGizmoAxis CCinemaFilmMakerManager::HitRotateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const
{
	constexpr int segments = 64;

	EGizmoAxis bestAxis = EGizmoAxis::None;
	float bestDistance = 12.0f;

	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		Fvector u = {};
		Fvector v = {};
		RotationBasis(axis, u, v);

		ImVec2 previous = ImVec2(0.0f, 0.0f);
		bool hasPrevious = false;

		for (int segment = 0; segment <= segments; ++segment)
		{
			const float angle = PI_MUL_2 * static_cast<float>(segment) / static_cast<float>(segments);
			const Fvector point = Fvector().mad(pivot, u, cosf(angle) * size).mad(v, sinf(angle) * size);
			const SProjectedPoint projected = ProjectWorld(point);

			if (projected.Visible)
			{
				if (hasPrevious)
				{
					const float distance = DistanceToSegment(mouse, previous, projected.Screen);

					if (distance < bestDistance)
					{
						bestDistance = distance;
						bestAxis = axis;
					}
				}

				previous = projected.Screen;
				hasPrevious = true;
			}
			else
			{
				hasPrevious = false;
			}
		}
	}

	return bestAxis;
}

void CCinemaFilmMakerManager::AppendTranslateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis)
{
	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		const bool highlighted = axis == hoveredAxis || (Gizmo.Active && Gizmo.Axis == axis);
		const u32 color = AxisColor(axis, highlighted);
		const Fvector direction = AxisVector(axis);
		const Fvector end = Fvector().mad(pivot, direction, size);

		inspector.append_lines_arrow(pivot, direction, size, color);

		const char* label = axis == EGizmoAxis::X ? "X" : axis == EGizmoAxis::Y ? "Y" : "Z";
		inspector.append_text3d(end, label, color);
	}

	inspector.append_sphere(pivot, size * 0.05f, GetTheme().AxisPivot);
}

void CCinemaFilmMakerManager::AppendRotateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis)
{
	constexpr int segments = 64;

	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		const bool highlighted = axis == hoveredAxis || (Gizmo.Active && Gizmo.Axis == axis);
		const u32 color = AxisColor(axis, highlighted);
		Fvector u = {};
		Fvector v = {};
		RotationBasis(axis, u, v);

		Fvector previous = {};
		bool hasPrevious = false;

		for (int segment = 0; segment <= segments; ++segment)
		{
			const float angle = PI_MUL_2 * static_cast<float>(segment) / static_cast<float>(segments);
			const Fvector point = Fvector().mad(pivot, u, cosf(angle) * size).mad(v, sinf(angle) * size);

			if (hasPrevious)
			{
				inspector.append_line(previous, point, color);
			}

			previous = point;
			hasPrevious = true;
		}
	}

	inspector.append_sphere(pivot, size * 0.035f, GetTheme().AxisPivot);
}

xr_vector<SMasterClipSelection> CCinemaFilmMakerManager::GetMasterGizmoObjects() const
{
	xr_vector<SMasterClipSelection> objects;
	if (MasterClipSelection.empty())
	{
		return objects;
	}
	for (const SMasterClipSelection& item : MasterClipSelection)
	{
		const bool valid = (item.Type == 1 && item.Object >= 0 && item.Object < static_cast<s32>(Actors.size()) && item.Clip >= 0 && item.Clip < static_cast<s32>(Actors[item.Object]->MasterClips.size())) ||
			(item.Type == 2 && item.Object >= 0 && item.Object < static_cast<s32>(Lights.size()) && item.Clip >= 0 && item.Clip < static_cast<s32>(Lights[item.Object].MasterClips.size())) ||
			(item.Type == 3 && item.Object >= 0 && item.Object < static_cast<s32>(Particles.size()) && item.Clip >= 0 && item.Clip < static_cast<s32>(Particles[item.Object].MasterClips.size())) ||
			(item.Type == 4 && item.Object >= 0 && item.Object < static_cast<s32>(Cameras.size()) && item.Clip == 0 && !Cameras[item.Object].Keys.empty());
		if (!valid)
		{
			continue;
		}
		const SMasterClipSelection object{ item.Type, item.Object, -1 };
		if (std::find(objects.begin(), objects.end(), object) == objects.end())
		{
			objects.push_back(object);
		}
	}
	return objects;
}

Fvector CCinemaFilmMakerManager::GetMasterGizmoObjectPosition(const SMasterClipSelection& item) const
{
	if (item.Type == 1)
	{
		return Actors[item.Object]->Position;
	}
	if (item.Type == 2)
	{
		Fvector position;
		Fmatrix orientation;
		CinemaLightTransform(Lights[item.Object], position, orientation);
		return position;
	}
	if (item.Type == 4)
	{
		Fvector position = Fvector().set(0.0f, 0.0f, 0.0f);
		const auto& keys = Cameras[item.Object].Keys;
		for (const auto& key : keys) position.add(key.Position);
		return position.mul(1.0f / static_cast<float>(keys.size()));
	}
	return ParticlePosition(Particles[item.Object]);
}

void CCinemaFilmMakerManager::TransformMasterGizmoObjects(const Fvector& previousPosition, const Fmatrix& previousOrientation)
{
	Fmatrix inverse, rotation;
	inverse.invert(previousOrientation);
	rotation.mul(MasterGizmoOrientation, inverse);
	for (const SMasterClipSelection& item : MasterGizmoObjects)
	{
		if (item.Type == 4 && item.Object >= 0 && item.Object < static_cast<s32>(Cameras.size()))
		{
			for (auto& key : Cameras[item.Object].Keys)
			{
				Fvector relative, rotated;
				relative.sub(key.Position, previousPosition);
				rotation.transform_dir(rotated, relative);
				key.Position.add(MasterGizmoPosition, rotated);
				Fmatrix transformed;
				transformed.mul(rotation, key.Orientation);
				key.Orientation = transformed;
			}
			continue;
		}
		Fvector* position = nullptr;
		Fmatrix* orientation = nullptr;
		if (item.Type == 1 && item.Object >= 0 && item.Object < static_cast<s32>(Actors.size()))
		{
			position = &Actors[item.Object]->Position;
			orientation = &Actors[item.Object]->Orientation;
		}
		else if (item.Type == 2 && item.Object >= 0 && item.Object < static_cast<s32>(Lights.size()))
		{
			position = &Lights[item.Object].Position;
			orientation = &Lights[item.Object].Orientation;
		}
		else if (item.Type == 3 && item.Object >= 0 && item.Object < static_cast<s32>(Particles.size()))
		{
			position = &Particles[item.Object].Position;
			orientation = &Particles[item.Object].Orientation;
		}
		if (position == nullptr || orientation == nullptr)
		{
			continue;
		}
		const Fvector visible = GetMasterGizmoObjectPosition(item);
		Fvector modulationOffset, relative, rotated;
		modulationOffset.sub(visible, *position);
		relative.sub(visible, previousPosition);
		rotation.transform_dir(rotated, relative);
		position->add(MasterGizmoPosition, rotated);
		position->sub(modulationOffset);
		Fmatrix transformed;
		transformed.mul(rotation, *orientation);
		*orientation = transformed;
		if (item.Type == 2)
		{
			UpdateCinemaLightValues(Lights[item.Object]);
		}
		else if (item.Type == 3)
		{
			SCinemaParticle& particle = Particles[item.Object];
			UpdateParticleTransform(particle);
			if (particle.MasterOwned && !MasterPlaying)
			{
				static_cast<CCinemaParticleObject*>(particle.Object.get())->SetSpeed(0.0f);
			}
		}
	}
}

void CCinemaFilmMakerManager::AppendWorldGizmo()
{
	if ((MasterPlaying && HideHelpersDuringPlayback) || g_pGameLevel == nullptr || g_hud == nullptr)
	{
		return;
	}

	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)])
	{
		return;
	}

	LevelInspector& inspector = HUD().world_prims;
	const bool cameraFocused = MasterClipAnchor.Type == 4 && MasterClipAnchor.Object >= 0 && MasterClipAnchor.Object < static_cast<s32>(Cameras.size()) &&
		std::find(MasterClipSelection.begin(), MasterClipSelection.end(), MasterClipAnchor) != MasterClipSelection.end();
	const bool editingFocusedCamera = cameraFocused && SelectedCameraKeyCamera == MasterClipAnchor.Object;
	if (cameraFocused)
	{
		const auto& camera = Cameras[MasterClipAnchor.Object];
		SCinemaCameraSpline spline(camera);
		spline.Smooth = CinemaStepCurveValue(camera.Curves[8], std::clamp(MasterTime - camera.Start, 0.0f, camera.Duration), camera.Smooth ? 1.0f : 0.0f) >= 0.5f;
		xr_vector<s32> order;
		for (s32 i = 0; i < static_cast<s32>(camera.Keys.size()); ++i) order.push_back(i);
		std::stable_sort(order.begin(), order.end(), [&](s32 a, s32 b) { return camera.Keys[a].Time < camera.Keys[b].Time; });
		for (size_t i = 0; i < order.size(); ++i)
		{
			const auto& key = camera.Keys[order[i]];
			const u32 color = editingFocusedCamera && order[i] == SelectedCameraKey ? GetTheme().AxisHighlighted : color_rgba(100, 180, 255, 255);
			inspector.append_sphere(key.Position, 0.12f, color, color);
			inspector.append_lines_arrow(key.Position, key.Orientation.k, 0.6f, color);
			if (i && camera.Keys[order[i - 1]].Time < key.Time)
			{
				size_t previousIndex = i - 1;
				while (previousIndex > 0 && camera.Keys[order[previousIndex - 1]].Time == camera.Keys[order[previousIndex]].Time) --previousIndex;
				const auto& before = camera.Keys[order[previousIndex]];
				Fvector point = before.Position;
				const s32 segments = spline.Smooth ? 16 : 1;
				for (s32 segment = 1; segment <= segments; ++segment)
				{
					Fvector next;
					spline.Evaluate(before.Time + (key.Time - before.Time) * static_cast<float>(segment) / static_cast<float>(segments), next);
					inspector.append_line(point, next, color);
					point = next;
				}
			}
		}
		if (editingFocusedCamera && MasterClipSelection.size() == 1 && SelectedCameraKey >= 0 && SelectedCameraKey < static_cast<s32>(camera.Keys.size()))
		{
			const Fvector& position = camera.Keys[SelectedCameraKey].Position;
			const float size = std::clamp(Device.vCameraPosition.distance_to(position) * 0.15f, 0.5f, 8.0f);
			const EGizmoAxis axis = Gizmo.Active ? Gizmo.Axis : HoveredAxis;
			if (GizmoMode == EGizmoMode::Translate) AppendTranslateGizmo(inspector, position, size, axis);
			else AppendRotateGizmo(inspector, position, size, axis);
			return;
		}
	}
	for (s32 index = 0; index < static_cast<s32>(Lights.size()); ++index)
	{
		const bool selected = std::find(SelectedLights.begin(), SelectedLights.end(), index) != SelectedLights.end() ||
			std::find(MasterGizmoObjects.begin(), MasterGizmoObjects.end(), SMasterClipSelection{ 2, index, -1 }) != MasterGizmoObjects.end();
		if (!selected || Lights[index].Type != 1)
		{
			continue;
		}
		Fvector lightPosition;
		Fmatrix lightOrientation;
		CinemaLightTransform(Lights[index], lightPosition, lightOrientation);
		const float arrowSize = std::clamp(Device.vCameraPosition.distance_to(lightPosition) * 0.2f, 0.75f, 10.0f);
		const u32 arrowColor = GetTheme().AxisHighlighted;
		inspector.append_lines_arrow(lightPosition, lightOrientation.k, arrowSize, arrowColor);
		inspector.append_text3d(Fvector().mad(lightPosition, lightOrientation.k, arrowSize), "Spot", arrowColor);
	}
	if (!MasterGizmoObjects.empty() && MasterGizmoObjects == GetMasterGizmoObjects())
	{
		const float distance = Device.vCameraPosition.distance_to(MasterGizmoPosition);
		const float size = std::clamp(distance * 0.15f, 0.5f, 8.0f);
		const EGizmoAxis hoveredAxis = Gizmo.Active ? Gizmo.Axis : HoveredAxis;
		if (GizmoMode == EGizmoMode::Translate)
		{
			AppendTranslateGizmo(inspector, MasterGizmoPosition, size, hoveredAxis);
		}
		else
		{
			AppendRotateGizmo(inspector, MasterGizmoPosition, size, hoveredAxis);
		}
		return;
	}
	SCinemaActor* actor = GetSelectedActor();
	SCinemaParticle* particle = SelectedParticle >= 0 && SelectedParticle < static_cast<s32>(Particles.size()) ? &Particles[SelectedParticle] : nullptr;
	SCinemaLight* light = SelectedLight >= 0 && SelectedLight < static_cast<s32>(Lights.size()) ? &Lights[SelectedLight] : nullptr;
	if ((actor == nullptr || actor->Model == nullptr) && particle == nullptr && light == nullptr)
	{
		return;
	}
	const Fvector position = light ? light->Position : particle ? ParticlePosition(*particle) : actor->Position;
	const float distance = Device.vCameraPosition.distance_to(position);
	const float size = std::clamp(distance * 0.15f, 0.5f, 8.0f);
	const EGizmoAxis hoveredAxis = Gizmo.Active ? Gizmo.Axis : HoveredAxis;
	if (GizmoMode == EGizmoMode::Translate)
	{
		AppendTranslateGizmo(inspector, position, size, hoveredAxis);
	}
	else
	{
		AppendRotateGizmo(inspector, position, size, hoveredAxis);
	}
}

void CCinemaFilmMakerManager::BeginGizmo(Fvector& position, Fmatrix& orientation, const Fvector& pivot, EGizmoAxis axis)
{
	if (axis == EGizmoAxis::None)
	{
		return;
	}

	Gizmo = SGizmoDrag();
	Gizmo.Active = true;
	Gizmo.Position = &position;
	Gizmo.Mode = GizmoMode;
	Gizmo.Axis = axis;
	Gizmo.AxisDir = AxisVector(axis);
	Gizmo.Pivot = pivot;
	Gizmo.StartPosition = position;
	Gizmo.StartOrientation = orientation;

	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		ClearGizmo();
		return;
	}

	if (Gizmo.Mode == EGizmoMode::Translate)
	{
		if (!ClosestPointOnAxis(Gizmo.Pivot, Gizmo.AxisDir, origin, direction, Gizmo.StartParam))
		{
			ClearGizmo();
		}

		return;
	}

	float angle = 0.0f;
	bool screenFallback = false;

	if (!ComputeMouseAngle(Gizmo.Pivot, Gizmo.Axis, angle, screenFallback))
	{
		ClearGizmo();
		return;
	}

	Gizmo.LastAngle = angle;
	Gizmo.AccumulatedAngle = 0.0f;
	Gizmo.UseScreenAngle = screenFallback;
}

void CCinemaFilmMakerManager::UpdateGizmo(Fvector& position, Fmatrix& orientation)
{
	if (!Gizmo.Active || Gizmo.Position != &position)
	{
		return;
	}

	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		return;
	}

	if (Gizmo.Mode == EGizmoMode::Translate)
	{
		float parameter = 0.0f;

		if (!ClosestPointOnAxis(Gizmo.Pivot, Gizmo.AxisDir, origin, direction, parameter))
		{
			return;
		}

		float delta = parameter - Gizmo.StartParam;

		if (Snap)
		{
			delta = roundf(delta / TranslateStep) * TranslateStep;
		}

		position.mad(Gizmo.StartPosition, Gizmo.AxisDir, delta);
		return;
	}

	float angle = 0.0f;
	bool screenFallback = false;

	if (!ComputeMouseAngle(Gizmo.Pivot, Gizmo.Axis, angle, screenFallback))
	{
		return;
	}

	const float delta = angle_difference_signed(angle, Gizmo.LastAngle);

	Gizmo.LastAngle = angle;
	Gizmo.AccumulatedAngle += delta;

	float accumulated = Gizmo.AccumulatedAngle;

	if (Snap)
	{
		const float degrees = rad2deg(accumulated);
		accumulated = deg2rad(roundf(degrees / RotateStepDeg) * RotateStepDeg);
	}

	Fmatrix rotation;
	rotation.rotation(Gizmo.AxisDir, accumulated);

	orientation.mul(Gizmo.StartOrientation, rotation);

	position = Gizmo.StartPosition;
}

void CCinemaFilmMakerManager::PickActorAtMouse()
{
	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		return;
	}

	s32 bestIndex = -1;
	s32 bestParticle = -1;
	float bestDistance = flt_max;

	for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
	{
		const SCinemaActor& actor = *Actors[i];

		if (actor.Model == nullptr)
		{
			continue;
		}

		Fmatrix transform = actor.Orientation;
		transform.c = actor.Position;

		Fvector center = {};
		transform.transform_tiny(center, actor.Model->getVisData().sphere.P);

		float radius = actor.Model->getVisData().sphere.R;

		if (radius < 0.2f)
		{
			radius = 0.5f;
		}

		radius *= std::max(actor.Scale.x, std::max(actor.Scale.y, actor.Scale.z));

		float distance = 0.0f;

		if (RaySphere(origin, direction, center, radius, distance) && distance < bestDistance)
		{
			bestDistance = distance;
			bestIndex = i;
		}
	}
	for (s32 i = 0; i < static_cast<s32>(Particles.size()); ++i)
	{
		float distance = 0.0f;
		if (RaySphere(origin, direction, ParticlePosition(Particles[i]), 0.3f, distance) && distance < bestDistance)
		{
			bestDistance = distance;
			bestParticle = i;
			bestIndex = -1;
		}
	}

	if (bestIndex < 0 && bestParticle < 0)
	{
		const ImVec2 mouse = ImGui::GetIO().MousePos;
		float bestScreenDistance = 32.0f;

		for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
		{
			const SCinemaActor& actor = *Actors[i];

			if (actor.Model == nullptr)
			{
				continue;
			}

			Fmatrix transform = actor.Orientation;
			transform.c = actor.Position;

			Fvector center = {};
			transform.transform_tiny(center, actor.Model->getVisData().sphere.P);
			const SProjectedPoint projected = ProjectWorld(center);

			if (!projected.Visible)
			{
				continue;
			}

			const float dx = projected.Screen.x - mouse.x;
			const float dy = projected.Screen.y - mouse.y;
			const float screenDistance = sqrtf(dx * dx + dy * dy);

			if (screenDistance < bestScreenDistance)
			{
				bestScreenDistance = screenDistance;
				bestIndex = i;
			}
		}
	}

	SelectActor(bestIndex);
	if (bestParticle >= 0)
	{
		ClearGizmo();
		SelectedParticle = bestParticle;
		SelectedParticles = { bestParticle };
	}
	else if (bestIndex < 0)
	{
		ClearGizmo();
		SelectedParticle = -1;
		SelectedParticles.clear();
	}
}

void CCinemaFilmMakerManager::DrawGizmo()
{
	if (MasterPlaying && HideHelpersDuringPlayback)
	{
		ClearGizmo();
		HoveredAxis = EGizmoAxis::None;
		return;
	}
	const bool capturing = CImGuiManager::Instance().IsCapturingInputs();
	if (LightColorPicking)
	{
		if (!capturing || !MasterPatternEditorOpen || MasterEditingPattern.Type != 2 || SelectedLight != LightColorPickingTarget)
		{
			LightColorPicking = false;
		}
		else
		{
			ClearGizmo();
			HoveredAxis = EGizmoAxis::None;
			return;
		}
	}
	const bool mouseOverUi = ImGui::IsWindowHovered(ImGuiHoveredFlags_AnyWindow);
	const ImGuiViewport* Viewport = ImGui::GetMainViewport();
	const bool IsOverGame = ImGui::IsMouseHoveringRect(Viewport->Pos, ImVec2(Viewport->Pos.x + Viewport->Size.x, Viewport->Pos.y + Viewport->Size.y), false);
	const bool canInteract = capturing && IsOverGame && !mouseOverUi && !ImGui::IsAnyItemActive();

	if (MasterClipSelection.size() == 1 && MasterClipAnchor.Type == 4 && MasterClipAnchor.Object >= 0 && MasterClipAnchor.Object < static_cast<s32>(Cameras.size()) &&
		MasterClipSelection.front() == MasterClipAnchor)
	{
		auto& keys = Cameras[MasterClipAnchor.Object].Keys;
		if (SelectedCameraKeyCamera == MasterClipAnchor.Object && SelectedCameraKey >= 0 && SelectedCameraKey < static_cast<s32>(keys.size()))
		{
			auto& key = keys[SelectedCameraKey];
			if (Gizmo.Active && Gizmo.Position != &key.Position) ClearGizmo();
			const float size = std::clamp(Device.vCameraPosition.distance_to(key.Position) * 0.15f, 0.5f, 8.0f);
			EGizmoAxis axis = Gizmo.Active ? Gizmo.Axis : EGizmoAxis::None;
			if (!Gizmo.Active && canInteract) axis = GizmoMode == EGizmoMode::Translate ? HitTranslateAxis(key.Position, size, ImGui::GetIO().MousePos) : HitRotateAxis(key.Position, size, ImGui::GetIO().MousePos);
			HoveredAxis = axis;
			if (Gizmo.Active)
			{
				if (ImGui::IsMouseReleased(ImGuiMouseButton_Left)) ClearGizmo();
				else if (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right))
				{
					key.Position = Gizmo.StartPosition;
					key.Orientation = Gizmo.StartOrientation;
					ClearGizmo();
				}
				else UpdateGizmo(key.Position, key.Orientation);
			}
			else if (canInteract && axis != EGizmoAxis::None && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
			{
				BeginGizmo(key.Position, key.Orientation, key.Position, axis);
				return;
			}
			if (Gizmo.Active) return;
		}
		if (canInteract && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		{
			Fvector origin, direction;
			if (MouseRay(origin, direction))
			{
				s32 nearest = -1;
				float nearestDistance = FLT_MAX;
				for (s32 i = 0; i < static_cast<s32>(keys.size()); ++i)
				{
					float distance = 0.0f;
					if (RaySphere(origin, direction, keys[i].Position, 0.12f, distance) && distance < nearestDistance)
					{
						nearest = i;
						nearestDistance = distance;
					}
				}
				ClearGizmo();
				SelectedCameraKey = nearest;
				SelectedCameraKeyCamera = nearest >= 0 ? MasterClipAnchor.Object : -1;
				if (nearest < 0)
				{
					MasterClipSelection.clear();
					MasterClipAnchor = {};
					PickActorAtMouse();
				}
			}
		}
		return;
	}

	const auto masterObjects = GetMasterGizmoObjects();
	if (masterObjects.empty() && MasterClipAnchor.Type == 4 && canInteract && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		SelectedCameraKey = -1;
		SelectedCameraKeyCamera = -1;
		MasterClipSelection.clear();
		MasterClipAnchor = {};
	}
	if (masterObjects != MasterGizmoObjects)
	{
		if (Gizmo.Active && Gizmo.Position == &MasterGizmoPosition)
		{
			ClearGizmo();
		}
		MasterGizmoObjects = masterObjects;
	}
	if (!MasterGizmoObjects.empty() && g_pGameLevel != nullptr)
	{
		if (!Gizmo.Active || Gizmo.Position != &MasterGizmoPosition)
		{
			if (Gizmo.Active)
			{
				ClearGizmo();
			}
			MasterGizmoPosition.set(0.0f, 0.0f, 0.0f);
			for (const SMasterClipSelection& item : MasterGizmoObjects)
			{
				MasterGizmoPosition.add(GetMasterGizmoObjectPosition(item));
			}
			MasterGizmoPosition.mul(1.0f / static_cast<float>(MasterGizmoObjects.size()));
			MasterGizmoOrientation.identity();
		}
		const Fvector pivot = MasterGizmoPosition;
		const float distance = Device.vCameraPosition.distance_to(pivot);
		const float size = std::clamp(distance * 0.15f, 0.5f, 8.0f);
		EGizmoAxis hoveredAxis = EGizmoAxis::None;
		if (!Gizmo.Active && canInteract)
		{
			hoveredAxis = GizmoMode == EGizmoMode::Translate ? HitTranslateAxis(pivot, size, ImGui::GetIO().MousePos) : HitRotateAxis(pivot, size, ImGui::GetIO().MousePos);
		}
		if (Gizmo.Active)
		{
			hoveredAxis = Gizmo.Axis;
		}
		HoveredAxis = hoveredAxis;
		if (Gizmo.Active)
		{
			if (ImGui::IsMouseReleased(ImGuiMouseButton_Left))
			{
				MasterGizmoSnapshots.clear();
				ClearGizmo();
				return;
			}
			const Fvector previousPosition = MasterGizmoPosition;
			const Fmatrix previousOrientation = MasterGizmoOrientation;
			if (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right))
			{
				for (const SMasterGizmoSnapshot& snapshot : MasterGizmoSnapshots)
				{
					const s32 index = snapshot.Object.Object;
					if (snapshot.Object.Type == 1 && index >= 0 && index < static_cast<s32>(Actors.size()))
					{
						Actors[index]->Position = snapshot.Position;
						Actors[index]->Orientation = snapshot.Orientation;
					}
					else if (snapshot.Object.Type == 2 && index >= 0 && index < static_cast<s32>(Lights.size()))
					{
						Lights[index].Position = snapshot.Position;
						Lights[index].Orientation = snapshot.Orientation;
						UpdateCinemaLightValues(Lights[index]);
					}
					else if (snapshot.Object.Type == 4 && index >= 0 && index < static_cast<s32>(Cameras.size()) && snapshot.Object.Clip >= 0 && snapshot.Object.Clip < static_cast<s32>(Cameras[index].Keys.size()))
					{
						auto& key = Cameras[index].Keys[snapshot.Object.Clip];
						key.Position = snapshot.Position;
						key.Orientation = snapshot.Orientation;
					}
					else if (snapshot.Object.Type == 3 && index >= 0 && index < static_cast<s32>(Particles.size()))
					{
						SCinemaParticle& particle = Particles[index];
						particle.Position = snapshot.Position;
						particle.Orientation = snapshot.Orientation;
						UpdateParticleTransform(particle);
						if (particle.MasterOwned && !MasterPlaying)
						{
							static_cast<CCinemaParticleObject*>(particle.Object.get())->SetSpeed(0.0f);
						}
					}
				}
				MasterGizmoPosition = Gizmo.StartPosition;
				MasterGizmoOrientation = Gizmo.StartOrientation;
				MasterGizmoSnapshots.clear();
				ClearGizmo();
				return;
			}
			UpdateGizmo(MasterGizmoPosition, MasterGizmoOrientation);
			TransformMasterGizmoObjects(previousPosition, previousOrientation);
			return;
		}
		if (canInteract && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		{
			if (hoveredAxis != EGizmoAxis::None)
			{
				BeginGizmo(MasterGizmoPosition, MasterGizmoOrientation, pivot, hoveredAxis);
				MasterGizmoSnapshots.clear();
				if (Gizmo.Active)
				{
					for (const SMasterClipSelection& item : MasterGizmoObjects)
					{
						if (item.Type == 1)
						{
							MasterGizmoSnapshots.push_back({ item, Actors[item.Object]->Position, Actors[item.Object]->Orientation });
						}
						else if (item.Type == 2)
						{
							MasterGizmoSnapshots.push_back({ item, Lights[item.Object].Position, Lights[item.Object].Orientation });
						}
						else if (item.Type == 3)
						{
							MasterGizmoSnapshots.push_back({ item, Particles[item.Object].Position, Particles[item.Object].Orientation });
						}
						else if (item.Type == 4)
						{
							const auto& keys = Cameras[item.Object].Keys;
							for (s32 i = 0; i < static_cast<s32>(keys.size()); ++i)
							{
								MasterGizmoSnapshots.push_back({ { 4, item.Object, i }, keys[i].Position, keys[i].Orientation });
							}
						}
					}
				}
			}
			else
			{
				MasterClipSelection.clear();
				MasterClipAnchor = {};
				MasterGizmoObjects.clear();
				PickActorAtMouse();
			}
		}
		if (hoveredAxis != EGizmoAxis::None)
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
		return;
	}

	SCinemaActor* actor = GetSelectedActor();
	SCinemaParticle* particle = SelectedParticle >= 0 && SelectedParticle < static_cast<s32>(Particles.size()) ? &Particles[SelectedParticle] : nullptr;
	SCinemaLight* light = SelectedLight >= 0 && SelectedLight < static_cast<s32>(Lights.size()) ? &Lights[SelectedLight] : nullptr;

	if (((actor == nullptr || actor->Model == nullptr) && particle == nullptr && light == nullptr) || g_pGameLevel == nullptr)
	{
		ClearGizmo();
		HoveredAxis = EGizmoAxis::None;

		if (canInteract && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		{
			PickActorAtMouse();
		}

		return;
	}

	Fvector& position = light ? light->Position : particle ? particle->Position : actor->Position;
	Fmatrix& orientation = light ? light->Orientation : particle ? particle->Orientation : actor->Orientation;
	const Fvector pivot = !light && particle ? ParticlePosition(*particle) : position;
	if (Gizmo.Active && Gizmo.Position != &position)
	{
		ClearGizmo();
	}

	const ImVec2 mouse = ImGui::GetIO().MousePos;
	const float distance = Device.vCameraPosition.distance_to(pivot);
	const float size = std::clamp(distance * 0.15f, 0.5f, 8.0f);

	EGizmoAxis hoveredAxis = EGizmoAxis::None;

	if (!Gizmo.Active && canInteract)
	{
		hoveredAxis = GizmoMode == EGizmoMode::Translate ? HitTranslateAxis(pivot, size, mouse) : HitRotateAxis(pivot, size, mouse);
	}

	if (Gizmo.Active)
	{
		hoveredAxis = Gizmo.Axis;
	}

	HoveredAxis = hoveredAxis;

	if (Gizmo.Active)
	{
		if (ImGui::IsMouseReleased(ImGuiMouseButton_Left))
		{
			ClearGizmo();
			return;
		}

		if (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right))
		{
			SCinemaParticleGroup previous;
			if (particle || light)
			{
				previous.Position = position;
				previous.Orientation = orientation;
			}
			position = Gizmo.StartPosition;
			orientation = Gizmo.StartOrientation;
			if (light)
			{
				TransformSelectedLights(previous.Position, previous.Orientation);
				ApplyCinemaLight(*light);
			}
			if (particle)
			{
				SCinemaParticleGroup current;
				current.Position = position;
				current.Orientation = orientation;
				TransformSelectedParticles(previous, current);
				UpdateParticleTransform(*particle);
			}
			ClearGizmo();
			return;
		}

		SCinemaParticleGroup previous;
		if (particle || light)
		{
			previous.Position = position;
			previous.Orientation = orientation;
		}
		UpdateGizmo(position, orientation);
		if (light)
		{
			TransformSelectedLights(previous.Position, previous.Orientation);
			ApplyCinemaLight(*light);
		}
		if (particle)
		{
			SCinemaParticleGroup current;
			current.Position = position;
			current.Orientation = orientation;
			TransformSelectedParticles(previous, current);
			UpdateParticleTransform(*particle);
		}
		return;
	}

	if (!canInteract)
	{
		return;
	}

	if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		if (hoveredAxis != EGizmoAxis::None)
		{
			BeginGizmo(position, orientation, pivot, hoveredAxis);
		}
		else
		{
			PickActorAtMouse();
		}
	}

	if (hoveredAxis != EGizmoAxis::None)
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	}
}

}

void CinemaFilmMaker_Render(IDSGraphManager* DM)
{
	if (CinemaFilmMaker::g_CinemaFilmMaker != nullptr)
	{
		CinemaFilmMaker::g_CinemaFilmMaker->Render();
	}
}

void CinemaFilmMaker_Shutdown()
{
	if (CinemaFilmMaker::g_CinemaFilmMaker == nullptr)
	{
		return;
	}

	CinemaFilmMaker::g_CinemaFilmMaker->Shutdown();
	Device.seqFrame.Remove(CinemaFilmMaker::g_CinemaFilmMaker);
	xr_delete(CinemaFilmMaker::g_CinemaFilmMaker);
	CinemaFilmMaker::g_CinemaFilmMaker = nullptr;
}

void RenderCinemaFilmMakerEditor()
{
	if (CinemaFilmMaker::g_CinemaFilmMaker == nullptr)
	{
		CinemaFilmMaker::g_CinemaFilmMaker = new CinemaFilmMaker::CCinemaFilmMakerManager();
		Device.seqFrame.Add(CinemaFilmMaker::g_CinemaFilmMaker, REG_PRIORITY_LOW);
	}

	bool& state = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)];

	if (!state)
	{
		return;
	}

	CinemaFilmMaker::g_CinemaFilmMaker->DrawGizmo();

	CinemaFilmMaker::g_CinemaFilmMaker->PushThemeStyle();
	CinemaFilmMaker::g_CinemaFilmMaker->DrawMasterTimeline(state);
	CinemaFilmMaker::g_CinemaFilmMaker->DrawCurveEditor();
	CinemaFilmMaker::g_CinemaFilmMaker->PopThemeStyle();
}
