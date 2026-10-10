#include "StdAfx.h"
#include "pch_script.h"
#include "WatchDevice.h"
#include "WatchInternal.h"
#include "../Include/xrRender/Kinematics.h"
#include "../../xrEngine/date_time.h"
#include "../../xrScripts/script_engine.h"
#include "ai_space.h"
#include "Level.h"
#include "map_manager.h"
#include "map_location.h"
#include "Actor.h"
#include "ActorCondition.h"
#include "AnomalyZone.h"
#include "AnomalyGravity.h"
#include "RadioactiveZone.h"
#include "ui/UIMotionIcon.h"
#include "../../xrCore/Kernel/EngineExternal.h"

namespace
{
const Fvector WorldNorth = {0.0f, 0.0f, 1.0f};

const shared_str CompassModeNorth = "north";
const shared_str CompassModeTarget = "target";
const shared_str CompassNoTargetHide = "hide";

bool IsAnomalyExcluded(CAnomalyZone* Zone, const SWatchAnomalySensor& Sensor, const xr_vector<xr_string>& Exclude)
{
	if (Sensor.ExcludeRadiation && Zone->cast_radioactive_zone())
	{
		return true;
	}

	const char* ZoneSection = Zone->cNameSect().c_str();
	for (const xr_string& Prefix : Exclude)
	{
		if (!strncmp(ZoneSection, Prefix.c_str(), Prefix.size()))
		{
			return true;
		}
	}
	return false;
}

float EvaluateAnomalyZoneIntensity(CAnomalyZone* Zone, const Fvector& ActorPos, const SWatchAnomalySensor& Sensor, const xr_vector<xr_string>& Exclude)
{
	if (!Zone || !Zone->IsEnabled() || Sensor.Radius <= EPS || IsAnomalyExcluded(Zone, Sensor, Exclude))
	{
		return 0.0f;
	}

	const float Distance = std::max(0.0f, Zone->XFORM().c.distance_to(ActorPos) - Zone->Radius());
	if (Distance >= Sensor.Radius)
	{
		return 0.0f;
	}

	const float T = 1.0f - Distance / Sensor.Radius;
	return T * T;
}

float SmoothTowards(float Current, float Target, float Dt, float Rate)
{
	return Current + (Target - Current) * (1.0f - expf(-Dt * Rate));
}

float HashUnit(u32 Value)
{
	Value ^= Value >> 16;
	Value *= 0x7FEB352Du;
	Value ^= Value >> 15;
	Value *= 0x846CA68Bu;
	Value ^= Value >> 16;
	return float(Value & 0xFFFFFFu) / float(0x1000000u);
}

float EvaluateLagUpdateHz(float BaseHz, float MinHz, float Strength)
{
	if (MinHz <= EPS || Strength <= EPS)
	{
		return BaseHz;
	}

	const float From = (BaseHz > EPS) ? BaseHz : WatchDetail::DisplayMaxHz;
	return (MinHz < From) ? From + (MinHz - From) * Strength : From;
}

float DisplayFlickerNoise(float Phase)
{
	const float Wave = 0.55f * sinf(Phase * 11.0f) + 0.30f * sinf(Phase * 29.0f + 1.7f) + 0.15f * sinf(Phase * 67.0f + 0.4f);
	return clampr(0.5f + 0.5f * Wave, 0.0f, 1.0f);
}

void ResetLed(SWatchLedRuntime& Led)
{
	Led.BlinkPhase = 0.0f;
	Led.Value = 0.0f;
}

void UpdateLedBlink(SWatchLedRuntime& Led, const SWatchLedBlink& Blink, float Intensity, float Dt)
{
	if (Intensity <= Blink.Threshold)
	{
		ResetLed(Led);
		return;
	}

	if (Intensity >= Blink.SolidThreshold)
	{
		Led.BlinkPhase = 0.0f;
		Led.Value = 1.0f;
		return;
	}

	const float Span = std::max(Blink.SolidThreshold - Blink.Threshold, EPS);
	const float T = clampr((Intensity - Blink.Threshold) / Span, 0.0f, 1.0f);
	Led.BlinkPhase += Dt * (Blink.BlinkMinHz + (Blink.BlinkMaxHz - Blink.BlinkMinHz) * T);
	Led.BlinkPhase -= floorf(Led.BlinkPhase);
	Led.Value = (Led.BlinkPhase < Blink.BlinkDuty) ? 1.0f : 0.0f;
}

float CompassChaosSpin(float Time)
{
	return sinf(Time * 19.0f) * 1.35f +
		   cosf(Time * 11.0f) * 0.95f +
		   sinf(Time * 31.0f) * 0.55f +
		   sinf(Time * 47.0f) * cosf(Time * 13.0f) * 0.35f;
}

struct SWatchGameTime
{
	u32 Year = 0;
	u32 Month = 0;
	u32 Day = 0;
	u32 Hour = 0;
	u32 Minute = 0;
	u32 Second = 0;
};

void ParseWatchFormat(const char* Format, bool Date, xr_vector<SWatchFormatToken>& Out)
{
	Out.clear();

	for (const char* Cursor = Format; Cursor && *Cursor;)
	{
		auto Match = [&Cursor](const char* Key, size_t Length)
		{
			if (strncmp(Cursor, Key, Length) != 0)
			{
				return false;
			}
			Cursor += Length;
			return true;
		};

		if (Match("YYYY", 4))
		{
			Out.push_back({EWatchFormatToken::Year4});
		}
		else if (Match("YY", 2))
		{
			Out.push_back({EWatchFormatToken::Year2});
		}
		else if (Match("HH", 2))
		{
			Out.push_back({EWatchFormatToken::Hour});
		}
		else if (Match("MM", 2))
		{
			Out.push_back({Date ? EWatchFormatToken::Month : EWatchFormatToken::Minute});
		}
		else if (Match("SS", 2))
		{
			Out.push_back({EWatchFormatToken::Second});
		}
		else if (Match("DD", 2))
		{
			Out.push_back({EWatchFormatToken::Day});
		}
		else
		{
			Out.push_back({EWatchFormatToken::Literal, *Cursor++});
		}
	}
}

void FormatWatchText(const xr_vector<SWatchFormatToken>& Tokens, const SWatchGameTime& Time, string64& Out)
{
	Out[0] = 0;

	string16 Part = {};
	for (const SWatchFormatToken& Token : Tokens)
	{
		switch (Token.Type)
		{
			case EWatchFormatToken::Hour:
				xr_sprintf(Part, "%02u", Time.Hour);
				break;
			case EWatchFormatToken::Minute:
				xr_sprintf(Part, "%02u", Time.Minute);
				break;
			case EWatchFormatToken::Second:
				xr_sprintf(Part, "%02u", Time.Second);
				break;
			case EWatchFormatToken::Day:
				xr_sprintf(Part, "%02u", Time.Day);
				break;
			case EWatchFormatToken::Month:
				xr_sprintf(Part, "%02u", Time.Month);
				break;
			case EWatchFormatToken::Year2:
				xr_sprintf(Part, "%02u", Time.Year % 100);
				break;
			case EWatchFormatToken::Year4:
				xr_sprintf(Part, "%04u", Time.Year);
				break;
			default:
				Part[0] = Token.Literal;
				Part[1] = 0;
				break;
		}
		xr_strcat(Out, Part);
	}
}

bool ParseLightLevel(const char* Name, EWatchLightLevel& Level)
{
	static const xr_pair<const char*, EWatchLightLevel> Levels[] =
		{
			{"day", EWatchLightLevel::Day},
			{"dusk", EWatchLightLevel::Dusk},
			{"night", EWatchLightLevel::Night},
			{"dark", EWatchLightLevel::Dark}
		};

	for (const auto& [Key, Value] : Levels)
	{
		if (!xr_strcmp(Name, Key))
		{
			Level = Value;
			return true;
		}
	}
	return false;
}

bool ParseLightSchedule(const char* Text, xr_vector<SWatchLightKey>& Keys)
{
	Keys.clear();
	if (!Text)
	{
		return false;
	}

	const int Count = _GetItemCount(Text);
	for (int Index = 0; Index < Count; ++Index)
	{
		string64 Item = {};
		string16 Name = {};
		SWatchLightKey Key;
		_GetItem(Text, Index, Item);
		if (sscanf(Item, "%f:%15s", &Key.Hour, Name) != 2 || Key.Hour < 0.0f || Key.Hour >= 24.0f || !ParseLightLevel(Name, Key.Level))
		{
			Keys.clear();
			return false;
		}
		if (!Keys.empty() && Key.Hour <= Keys.back().Hour)
		{
			Keys.clear();
			return false;
		}
		Keys.push_back(Key);
	}
	return !Keys.empty();
}

float LightLevelEmission(const SWatchLight& Light, EWatchLightLevel Level)
{
	switch (Level)
	{
		case EWatchLightLevel::Dusk:
			return Light.DuskEmission;
		case EWatchLightLevel::Night:
			return Light.NightEmission;
		case EWatchLightLevel::Dark:
			return Light.DarkEmission;
		default:
			return Light.DayEmission;
	}
}

float EvaluateLightSchedule(const xr_vector<SWatchLightKey>& Keys, const SWatchLight& Light, float Hour)
{
	if (Keys.empty())
	{
		return 0.0f;
	}

	const SWatchLightKey* From = &Keys.back();
	const SWatchLightKey* To = &Keys.front();
	for (size_t Index = 0; Index + 1 < Keys.size(); ++Index)
	{
		if (Hour >= Keys[Index].Hour && Hour < Keys[Index + 1].Hour)
		{
			From = &Keys[Index];
			To = &Keys[Index + 1];
			break;
		}
	}

	float Span = To->Hour - From->Hour;
	float Offset = Hour - From->Hour;
	if (Span <= 0.0f)
	{
		Span += 24.0f;
	}
	if (Offset < 0.0f)
	{
		Offset += 24.0f;
	}

	const float A = LightLevelEmission(Light, From->Level);
	const float B = LightLevelEmission(Light, To->Level);
	return A + (B - A) * clampr(Offset / Span, 0.0f, 1.0f);
}

void MakeOffsetXform(const Fvector& Position, const Fvector& RotationDeg, Fmatrix& Out)
{
	Fvector RotationRad = RotationDeg;
	RotationRad.mul(PI / 180.0f);
	Out.setHPB(RotationRad.x, RotationRad.y, RotationRad.z);
	Out.translate_over(Position);
}
}

CWatchDevice::~CWatchDevice()
{
	DestroyHudLights();
	Ui.reset();
}

void CWatchDevice::Load(const shared_str& RootSection)
{
	Section = RootSection;
	Loaded = false;
	Config = SWatchConfig{};
	State = SWatchState{};
	Preview = SWatchDebugPreview{};
	ResetRuntime();

	if (!pSettings || !pSettings->section_exist(Section.c_str()))
	{
		return;
	}

	LoadConfig(*pSettings, Section, false);
	ApplyIndicatorMasters();

	Loaded = true;
	ApplyOverrideFromDisk();
	ApplyConditionMasters();
	UpdateDisplayFormats();
	UpdateLightSchedule();
	UpdateAnomalyExclude();
	UpdateUILayout();
	ConfigBaseline = Config;
}

void CWatchDevice::ResetRuntime()
{
	ResetCompassRuntime();
	Display = SWatchDisplayRuntime{};
	Surge = SWatchSurgeRuntime{};
	Light.Timer = 0.0f;
	Zones.SampleTimer = 0.0f;
	Zones.Gravity = 0.0f;
	Zones.AnomalyTarget = 0.0f;
	Zones.GravitySections.clear();
	for (SWatchLedRuntime& Led : Leds)
	{
		ResetLed(Led);
	}
	for (u32 Index = 0; Index < WatchConditionCount; ++Index)
	{
		ConditionRuntime[Index] = SWatchConditionRuntime{};
		State.Condition[Index] = 0.0f;
		State.ConditionDisplay[Index] = 0.0f;
		State.ConditionBadness[Index] = 0.0f;
		State.ConditionVisible[Index] = false;
		State.ConditionSeverity[Index] = EWatchConditionSeverity::None;
	}
}

void CWatchDevice::ResetCompassRuntime()
{
	Compass = SWatchCompassRuntime{};
	Zones.Gravity = 0.0f;
	Zones.SampleTimer = 0.0f;
	State.CompassAngle = 0.0f;
	State.CompassError = 0.0f;
	State.TargetValid = false;
}

void CWatchDevice::BindModel(IKinematics* NewModel)
{
	Unbind();

	Model = NewModel;
	if (!Model)
	{
		return;
	}

	AttachModel();
}

void CWatchDevice::AttachModel()
{
	ResolveBones();
	SetBoneCallbacks();
	UpdateLedMeshes();
	CreateUI();
}

void CWatchDevice::Unbind()
{
	DestroyHudLights();
	for (SWatchLedRuntime& Led : Leds)
	{
		RestoreLedMesh(Led);
	}
	Ui.reset();
	ResetBoneCallbacks();
	ClearBones();
	Zones.Spatial.clear();
	ResetCompassRuntime();
	Model = nullptr;
}

u16 CWatchDevice::ResolveBoneId(const shared_str& Name) const
{
	if (!Model || !Name.size())
	{
		return BI_NONE;
	}

	return Model->LL_BoneID(Name);
}

void CWatchDevice::ResolveBones()
{
	BoneIds.Compass = ResolveBoneId(Config.Bones.Compass);
	BoneIds.CompassLight = ResolveBoneId(Config.Bones.CompassLight);
	BoneIds.Display = ResolveBoneId(Config.Bones.Display);
	BoneIds.RadIndicator = ResolveBoneId(Config.Bones.RadIndicator);
	BoneIds.AnomIndicator = ResolveBoneId(Config.Bones.AnomIndicator);

	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		const shared_str& BoneName = Config.Present[Index].Bone;
		const u16 DefaultBone = (Index == u32(EWatchLedChannel::Anomaly)) ? BoneIds.AnomIndicator : BoneIds.RadIndicator;
		Leds[Index].Bone = BoneName.size() ? ResolveBoneId(BoneName) : DefaultBone;
	}
}

bool CWatchDevice::BonesOk() const
{
	return BoneIds.Compass != BI_NONE &&
		   BoneIds.CompassLight != BI_NONE &&
		   BoneIds.Display != BI_NONE &&
		   BoneIds.RadIndicator != BI_NONE &&
		   BoneIds.AnomIndicator != BI_NONE;
}

void CWatchDevice::RestoreLedMesh(SWatchLedRuntime& Led)
{
	if (Model && Led.HiddenBone != BI_NONE)
	{
		Model->LL_SetBoneVisible(Led.HiddenBone, true, false);
	}
	Led.HiddenBone = BI_NONE;
}

void CWatchDevice::UpdateLedMeshes()
{
	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		const SWatchChannelPresent& Present = Config.Present[Index];
		SWatchLedRuntime& Led = Leds[Index];
		const bool Hide = Model && Present.Enabled && Present.PresentShader && Present.HideMesh;
		const u16 Bone = Hide ? Led.Bone : BI_NONE;
		if (Bone == Led.HiddenBone)
		{
			continue;
		}

		RestoreLedMesh(Led);
		if (Bone != BI_NONE)
		{
			Model->LL_SetBoneVisible(Bone, false, false);
			Led.HiddenBone = Bone;
		}
	}
}

void CWatchDevice::ClearBones()
{
	BoneIds = SWatchBoneIds{};
	for (SWatchLedRuntime& Led : Leds)
	{
		Led.Bone = BI_NONE;
	}
}

bool CWatchDevice::HasCompassBone() const
{
	return Model && BoneIds.Compass != BI_NONE;
}

void CWatchDevice::SetBoneCallbacks()
{
	if (!Model || CallbacksBound)
	{
		return;
	}

	if (HasCompassBone())
	{
		CBoneInstance& Instance = Model->LL_GetBoneInstance(BoneIds.Compass);
		Instance.set_callback(bctCustom, &CWatchDevice::BoneCallbackCompass, this);
	}

	CallbacksBound = true;
}

void CWatchDevice::ResetBoneCallbacks()
{
	if (!Model || !CallbacksBound)
	{
		CallbacksBound = false;
		return;
	}

	if (HasCompassBone())
	{
		Model->LL_GetBoneInstance(BoneIds.Compass).reset_callback();
	}

	CallbacksBound = false;
}

void CWatchDevice::BoneCallbackCompass(CBoneInstance* Bone)
{
	if (!Bone)
	{
		return;
	}

	CWatchDevice* Self = static_cast<CWatchDevice*>(Bone->callback_param());
	if (!Self)
	{
		return;
	}

	Fmatrix Rotation;
	Rotation.rotateY(Self->Compass.Angle);
	Bone->mTransform.mulB_43(Rotation);
}

bool CWatchDevice::WorldDirToCompassAngle(const Fvector& WorldDir, float& OutAngle) const
{
	if (!HasCompassBone())
	{
		return false;
	}

	Fvector Direction = WorldDir;
	Direction.y = 0.0f;
	if (Direction.square_magnitude() < EPS_L)
	{
		return false;
	}

	Direction.normalize();

	const float TargetH = Direction.getH();
	const float CameraH = Device.vCameraDirection.getH();
	OutAngle = angle_normalize_signed(
		-angle_difference_signed(TargetH, CameraH) + deg2rad(Config.Compass.AngleOffset)
	);
	return true;
}

void CWatchDevice::CommitCompassAngle(float Angle, bool ResetVelocity)
{
	Compass.Desired = Angle;
	if (ResetVelocity || !Compass.SpringActive)
	{
		Compass.Angle = Angle;
		Compass.Velocity = 0.0f;
	}
	Compass.SpringActive = true;
	SyncCompassState();
}

void CWatchDevice::SyncCompassState()
{
	State.CompassAngle = Compass.Angle;
	State.CompassError = angle_difference_signed(Compass.Desired, Compass.Angle);
}

bool CWatchDevice::ApplyCompassWorldDir(const Fvector& WorldDir)
{
	float Angle = 0.0f;
	if (!WorldDirToCompassAngle(WorldDir, Angle))
	{
		return false;
	}

	CommitCompassAngle(Angle, false);
	return true;
}

bool CWatchDevice::SectionLooksLikeGravity(const shared_str& ZoneSection)
{
	if (!ZoneSection.size())
	{
		return false;
	}

	auto Cached = Zones.GravitySections.find(ZoneSection);
	if (Cached != Zones.GravitySections.end())
	{
		return Cached->second;
	}

	bool Result = false;
	const char* Name = ZoneSection.c_str();
	if (pSettings && pSettings->section_exist(Name))
	{
		if (strstr(Name, "nogravity") || strstr(Name, "no_gravity"))
		{
			Result = false;
		}
		else if (strstr(Name, "gravi") || strstr(Name, "mincer"))
		{
			Result = true;
		}
		else
		{
			static const char* Keys[] = {"postprocess", "idle_particles", "blowout_particles"};
			for (const char* Key : Keys)
			{
				if (!pSettings->line_exist(Name, Key))
				{
					continue;
				}

				const char* Value = pSettings->r_string(Name, Key);
				if (Value && strstr(Value, "gravi"))
				{
					Result = true;
					break;
				}
			}
		}
	}

	Zones.GravitySections.emplace(ZoneSection, Result);
	return Result;
}

float CWatchDevice::EvaluateGravityZoneIntensity(CAnomalyZone* Zone, const Fvector& ActorPos)
{
	if (!Zone || !Zone->IsEnabled())
	{
		return 0.0f;
	}

	TAnomalyGravity* GravityComponent = Zone->GetComponent<TAnomalyGravity>();
	const bool HasGravityComponent = GravityComponent && GravityComponent->IsEnabled();
	if (!Zone->cast_base_gravi_zone() && !HasGravityComponent && !SectionLooksLikeGravity(Zone->cNameSect()))
	{
		return 0.0f;
	}

	Fvector Delta;
	Delta.sub(Zone->XFORM().c, ActorPos);
	const float DistSqr = Delta.square_magnitude();

	float Radius = Zone->Radius();
	if (Radius < 1.0f)
	{
		Radius = 1.0f;
	}

	float Sense = Radius * 10.0f;
	if (Sense < WatchDetail::CompassGravitySenseMin)
	{
		Sense = WatchDetail::CompassGravitySenseMin;
	}

	if (HasGravityComponent)
	{
		const float ScanRadius = GravityComponent->GetScanRadius();
		if (ScanRadius > Radius)
		{
			Sense = std::max(Sense, ScanRadius * 10.0f);
		}
	}

	if (DistSqr >= Sense * Sense)
	{
		return 0.0f;
	}

	const float Dist = std::sqrt(DistSqr);
	const float Power = Zone->RelativePower(Dist, Radius);
	if (Power > EPS)
	{
		return Power;
	}

	const float T = 1.0f - Dist / Sense;
	return T * T;
}

bool CWatchDevice::IsChannelActive(EWatchLedChannel Channel) const
{
	const SWatchChannelPresent& Present = Config.Present[u32(Channel)];
	const SWatchLedRuntime& Led = Leds[u32(Channel)];
	return Present.Enabled &&
		   ((Present.PresentShader && Led.Bone != BI_NONE) ||
			(Present.PresentUi && BoneIds.Display != BI_NONE));
}

void CWatchDevice::SampleZones(float Dt)
{
	const bool GravityActive = !Preview.PreviewGravity && Config.Compass.GravityStrength > 0.0f && HasCompassBone();
	const bool AnomalyActive = IsChannelActive(EWatchLedChannel::Anomaly);
	if (Preview.PreviewGravity)
	{
		Zones.Gravity = clampr(Preview.PreviewGravityValue, 0.0f, 1.0f);
	}

	CObject* Entity = (g_pGameLevel && g_SpatialSpace) ? Level().CurrentEntity() : nullptr;
	if (!Entity || (!GravityActive && !AnomalyActive))
	{
		if (!Preview.PreviewGravity)
		{
			Zones.Gravity = 0.0f;
		}
		Zones.AnomalyTarget = 0.0f;
		Zones.SampleTimer = 0.0f;
		return;
	}

	Zones.SampleTimer -= Dt;
	if (Zones.SampleTimer > 0.0f)
	{
		return;
	}

	Zones.SampleTimer = 1.0f / WatchDetail::ZoneSampleHz;

	const Fvector ActorPos = Entity->Position();
	const float Radius = std::max(GravityActive ? WatchDetail::CompassGravityQueryRadius : 0.0f, AnomalyActive ? Config.Anomaly.Radius : 0.0f);
	float Gravity = 0.0f;
	float Anomaly = 0.0f;

	Zones.Spatial.clear();
	g_SpatialSpace->q_sphere(
		Zones.Spatial,
		0,
		ESPATIAL_TYPE::ANOMALY_ZONE,
		ActorPos,
		Radius
	);

	for (ISpatialShared& Spatial : Zones.Spatial)
	{
		ISpatial* SpatialObject = Spatial.get();
		if (!SpatialObject)
		{
			continue;
		}

		CObject* Object = SpatialObject->dcast_CObject();
		if (!Object || Object->getDestroy())
		{
			continue;
		}

		CGameObject* GameObject = Object->cast_game_object();
		if (!GameObject)
		{
			continue;
		}

		CAnomalyZone* Zone = GameObject->cast_anomaly_zone();
		if (GravityActive)
		{
			Gravity = std::max(Gravity, EvaluateGravityZoneIntensity(Zone, ActorPos));
		}
		if (AnomalyActive)
		{
			Anomaly = std::max(Anomaly, EvaluateAnomalyZoneIntensity(Zone, ActorPos, Config.Anomaly, Zones.AnomalyExclude));
		}
		if ((!GravityActive || Gravity >= WatchDetail::CompassGravityEarlyOut) && (!AnomalyActive || Anomaly >= 1.0f))
		{
			break;
		}
	}

	if (GravityActive)
	{
		Zones.Gravity = Gravity;
	}
	Zones.AnomalyTarget = Anomaly;
}

bool CWatchDevice::UpdateCompassGravityChaos(float Dt)
{
	const float Intensity = Zones.Gravity;
	if (Intensity <= WatchDetail::CompassGravityChaosEnter || Dt <= 0.0f)
	{
		if (Intensity <= WatchDetail::CompassGravityChaosEnter)
		{
			Compass.ChaosTime = 0.0f;
		}
		return false;
	}

	SetCompassVisible(true);
	Compass.ChaosTime += Dt;

	const float Spin = CompassChaosSpin(Compass.ChaosTime);
	const float Speed = deg2rad(Config.Compass.GravityStrength) * (3.0f + Intensity * 14.0f);
	CommitCompassAngle(
		angle_normalize_signed(Compass.Angle + Spin * Speed * Intensity * Dt),
		true
	);
	State.CompassError = 0.0f;
	return true;
}

void CWatchDevice::SetCompassVisible(bool Visible)
{
	if (!HasCompassBone())
	{
		return;
	}

	if (!!Model->LL_GetBoneVisible(BoneIds.Compass) == Visible)
	{
		return;
	}

	Model->LL_SetBoneVisible(BoneIds.Compass, Visible, true);
}

void CWatchDevice::ApplyCompassNoTarget()
{
	State.TargetValid = false;

	if (Config.Compass.NoTarget.equal(CompassNoTargetHide))
	{
		SetCompassVisible(false);
		Compass.SpringActive = false;
		Compass.Velocity = 0.0f;
		SyncCompassState();
		return;
	}

	SetCompassVisible(true);

	if (Config.Compass.NoTarget.equal(CompassModeNorth))
	{
		ApplyCompassWorldDir(WorldNorth);
		return;
	}

	Compass.Desired = Compass.Angle;
	Compass.Velocity = 0.0f;
	Compass.SpringActive = false;
	SyncCompassState();
}

void CWatchDevice::UpdateCompassTarget()
{
	State.TargetValid = false;

	if (!g_pGameLevel)
	{
		ApplyCompassNoTarget();
		return;
	}

	CMapLocation* Location = Level().MapManager().GetActiveTaskCompassLocation();
	if (!Location || !Location->Update())
	{
		ApplyCompassNoTarget();
		return;
	}

	if (Location->GetLevelName() != Level().name())
	{
		ApplyCompassNoTarget();
		return;
	}

	CObject* Entity = Level().CurrentEntity();
	if (!Entity)
	{
		ApplyCompassNoTarget();
		return;
	}

	Fvector WorldDir;
	WorldDir.sub(Location->GetLastPosition(), Entity->Position());
	if (!ApplyCompassWorldDir(WorldDir))
	{
		ApplyCompassNoTarget();
		return;
	}

	SetCompassVisible(true);
	State.TargetValid = true;
}

void CWatchDevice::UpdateCompassNorth()
{
	State.TargetValid = false;
	SetCompassVisible(true);
	ApplyCompassWorldDir(WorldNorth);
}

void CWatchDevice::UpdateCompassSpring(float Dt)
{
	if (!Compass.SpringActive || !HasCompassBone() || Dt <= 0.0f)
	{
		SyncCompassState();
		return;
	}

	float Remaining = Dt;
	u32 Steps = 0;
	while (Remaining > 0.0f && Steps < WatchDetail::CompassSpringMaxSubsteps)
	{
		const float Step = (Remaining > WatchDetail::CompassSpringMaxStep) ? WatchDetail::CompassSpringMaxStep : Remaining;
		Remaining -= Step;
		++Steps;

		const float Error = angle_difference_signed(Compass.Desired, Compass.Angle);
		if (fabsf(Error) < WatchDetail::CompassSpringSettleRad)
		{
			Compass.Angle = Compass.Desired;
			Compass.Velocity = 0.0f;
			break;
		}

		const float Accel =
			Config.Compass.Stiffness * Error - Config.Compass.Damping * Compass.Velocity;
		Compass.Velocity += Accel * Step;
		Compass.Angle = angle_normalize_signed(Compass.Angle + Compass.Velocity * Step);
	}

	SyncCompassState();
}

void CWatchDevice::UpdateCompass(float Dt)
{
	if (!HasCompassBone())
	{
		State.TargetValid = false;
		Compass.SpringActive = false;
		SyncCompassState();
		return;
	}

	if (Preview.ForceAngle)
	{
		State.TargetValid = false;
		SetCompassVisible(true);
		CommitCompassAngle(deg2rad(Preview.ForceAngleDeg), true);
	}
	else if (Config.Compass.Mode.equal(CompassModeNorth))
	{
		UpdateCompassNorth();
	}
	else if (Config.Compass.Mode.equal(CompassModeTarget))
	{
		UpdateCompassTarget();
	}
	else
	{
		State.TargetValid = false;
		Compass.SpringActive = false;
	}

	if (!UpdateCompassGravityChaos(Dt))
	{
		UpdateCompassSpring(Dt);
	}
}

void CWatchDevice::Update(float Dt)
{
	if (!IsEnabled() || !Model)
	{
		return;
	}

	const float SafeDt = (Dt > 0.0f) ? Dt : 0.0f;
	SampleZones(SafeDt);
	UpdateCompass(SafeDt);
	UpdateSurge(SafeDt);
	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		UpdateChannel(EWatchLedChannel(Index), SafeDt);
	}
	for (u32 Index = 0; Index < WatchConditionCount; ++Index)
	{
		UpdateCondition(EWatchCondition(Index), SafeDt);
	}
	UpdateDisplayLag(SafeDt);
	UpdateDisplay(SafeDt);
	UpdateLight(SafeDt);
}

void CWatchDevice::CreateUI()
{
	Ui.reset();
	State.TimeText.clear();
	State.DateText.clear();
	State.BarometerText.clear();
	Display.Timer = 0.0f;
	Surge.Timer = 0.0f;

	if (!Loaded || !Model || BoneIds.Display == BI_NONE)
	{
		return;
	}

	Ui.reset(new CUIWatchWnd());
	if (!Ui->Init(Config.Display, Config.Fonts))
	{
		Ui.reset();
		return;
	}
	Ui->SetIntensity(Display.Intensity);
}

void CWatchDevice::UpdateUILayout()
{
	const SWatchDisplay& DisplayConfig = Config.Display;
	Fvector Rotation = DisplayConfig.UiRotation;
	Rotation.mul(PI / 180.0f);
	UiOffset.setHPB(Rotation.x, Rotation.y, Rotation.z);
	UiOffset.i.mul(DisplayConfig.UiScale);
	UiOffset.j.mul(DisplayConfig.UiScale);
	UiOffset.k.mul(DisplayConfig.UiScale);
	UiOffset.translate_over(DisplayConfig.UiPosition);

	MakeOffsetXform(Config.Light.UiPosition, Config.Light.UiRotation, Light.Offset);
	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		MakeOffsetXform(Config.Present[Index].UiPosition, Config.Present[Index].UiRotation, Leds[Index].Offset);
	}
	for (u32 Index = 0; Index < WatchConditionCount; ++Index)
	{
		const SWatchConditionPresent& Present = Config.Conditions[Index];
		MakeOffsetXform(Present.UiIconPosition, Present.UiIconRotation, ConditionRuntime[Index].IconOffset);
		MakeOffsetXform(Present.UiBarPosition, Present.UiBarRotation, ConditionRuntime[Index].BarOffset);
	}

	if (Ui)
	{
		Ui->SetLayout(DisplayConfig);
	}
}

void CWatchDevice::UpdateDisplayFormats()
{
	ParseWatchFormat(Config.Display.TimeFormat.c_str(), false, Display.TimeTokens);
	ParseWatchFormat(Config.Display.DateFormat.c_str(), true, Display.DateTokens);
	Display.Timer = 0.0f;
}

void CWatchDevice::UpdateDisplay(float Dt)
{
	if (!Ui || !g_pGameLevel)
	{
		return;
	}

	Display.Timer -= Dt;
	if (Display.Timer > 0.0f)
	{
		return;
	}

	Display.Timer = (Display.Hz > EPS) ? 1.0f / Display.Hz : 0.0f;
	if (Display.Freeze > 0.0f)
	{
		return;
	}

	if (Display.FreezeChance > EPS && HashUnit(++Display.Tick) < Display.FreezeChance)
	{
		Display.Freeze = std::max(Display.Timer, WatchDetail::DisplayMinFreeze);
		return;
	}

	const u64 GameTime = Level().GetGameTime();
	const u64 DelayMs = u64(Display.Delay * 1000.0f);

	SWatchGameTime Time;
	u32 Milliseconds = 0;
	split_time(GameTime > DelayMs ? GameTime - DelayMs : 0, Time.Year, Time.Month, Time.Day, Time.Hour, Time.Minute, Time.Second, Milliseconds);

	string64 Text = {};
	FormatWatchText(Display.TimeTokens, Time, Text);
	if (State.TimeText != Text)
	{
		State.TimeText = Text;
		Ui->SetTime(Text);
	}

	FormatWatchText(Display.DateTokens, Time, Text);
	if (State.DateText != Text)
	{
		State.DateText = Text;
		Ui->SetDate(Text);
	}
}

bool CWatchDevice::SampleSurge(float& SecondsToSurge, float& Progress)
{
	SecondsToSurge = -1.0f;
	Progress = -1.0f;
	if (Surge.BridgeMissing)
	{
		return false;
	}

	if (!Surge.BridgeResolved)
	{
		if (!ai().script_engine().functor(WatchDetail::SurgeSecondsFunction, Surge.SecondsFunction) || !ai().script_engine().functor(WatchDetail::SurgeProgressFunction, Surge.ProgressFunction))
		{
			Surge.BridgeMissing = true;
			Msg("! [watch] surge bridge [%s] not found, barometer has no data", WatchDetail::SurgeSecondsFunction);
			return false;
		}
		Surge.BridgeResolved = true;
	}

	SecondsToSurge = Surge.SecondsFunction();
	Progress = Surge.ProgressFunction();
	return SecondsToSurge >= 0.0f || Progress >= 0.0f;
}

void CWatchDevice::UpdateSurge(float Dt)
{
	if (!Ui || !g_pGameLevel)
	{
		return;
	}

	Surge.Timer -= Dt;
	if (Surge.Timer > 0.0f)
	{
		return;
	}

	Surge.Timer = 1.0f / WatchDetail::SurgeSampleHz;
	Surge.Valid = SampleSurge(Surge.Seconds, Surge.Progress);

	if (Preview.PreviewSurge)
	{
		State.SurgeFactor = Preview.PreviewSurgeValue;
	}
	else if (Surge.Valid && Surge.Progress >= 0.0f)
	{
		State.SurgeFactor = sinf(PI * clampr(Surge.Progress, 0.0f, 1.0f));
	}
	else
	{
		State.SurgeFactor = 0.0f;
	}

	UpdateBarometer();
}

void CWatchDevice::UpdateDisplayLag(float Dt)
{
	const SWatchLag& SurgeLag = Config.SurgeLag;
	const SWatchLag& AnomalyLag = Config.AnomalyLag;
	Surge.Lag = SmoothTowards(Surge.Lag, State.SurgeFactor, Dt, WatchDetail::LagSmoothing);
	Surge.AnomalyLag = SmoothTowards(Surge.AnomalyLag, State.Intensity[u32(EWatchLedChannel::Anomaly)], Dt, WatchDetail::LagSmoothing);

	const float BaseHz = Config.Display.UpdateHz;
	const float SurgeHz = EvaluateLagUpdateHz(BaseHz, SurgeLag.MinUpdateHz, Surge.Lag);
	const float AnomalyHz = EvaluateLagUpdateHz(BaseHz, AnomalyLag.MinUpdateHz, Surge.AnomalyLag);
	const float SurgeFreeze = clampr(SurgeLag.FreezeChance * Surge.Lag, 0.0f, 1.0f);
	const float AnomalyFreeze = clampr(AnomalyLag.FreezeChance * Surge.AnomalyLag, 0.0f, 1.0f);

	State.DisplayQuality = clampr(1.0f - Surge.Lag - Surge.AnomalyLag, 0.0f, 1.0f);
	Display.Delay = SurgeLag.MaxDelay * Surge.Lag + AnomalyLag.MaxDelay * Surge.AnomalyLag;
	Display.Hz = std::min(SurgeHz, AnomalyHz);
	Display.FreezeChance = clampr(1.0f - (1.0f - SurgeFreeze) * (1.0f - AnomalyFreeze), 0.0f, 1.0f);
	Display.Freeze = std::max(0.0f, Display.Freeze - Dt);

	float Intensity = clampr(0.20f + 0.80f * State.DisplayQuality, 0.0f, 1.0f);
	const float Flicker = clampr(SurgeLag.Flicker * Surge.Lag + AnomalyLag.Flicker * Surge.AnomalyLag, 0.0f, 1.0f);
	if (Display.Freeze > 0.0f)
	{
		Intensity = 0.0f;
	}
	else if (Flicker > EPS)
	{
		Display.GlitchPhase += Dt * (4.0f + 12.0f * Flicker);
		if (Display.GlitchPhase > WatchDetail::DisplayGlitchWrap)
		{
			Display.GlitchPhase -= WatchDetail::DisplayGlitchWrap;
		}
		const float Noise = DisplayFlickerNoise(Display.GlitchPhase);
		Intensity *= 1.0f - Flicker * (0.35f + 0.65f * Noise);
		if (Noise > 0.82f && Flicker > 0.20f)
		{
			Intensity = 0.0f;
		}
	}
	else
	{
		Display.GlitchPhase = 0.0f;
	}

	State.DisplayGlitched = Display.Freeze > 0.0f || Intensity < WatchDetail::DisplayGlitchLevel;
	if (Ui && !fsimilar(Intensity, Display.Intensity))
	{
		Ui->SetIntensity(Intensity);
	}
	Display.Intensity = Intensity;
}

void CWatchDevice::UpdateBarometer()
{
	const SWatchDisplay& DisplayConfig = Config.Display;
	if (!DisplayConfig.BarometerEnabled)
	{
		SetBarometerText("");
		return;
	}

	if (Preview.PreviewSurge)
	{
		State.Barometer = Preview.PreviewSurgeValue;
	}
	else if (Surge.Valid)
	{
		float Target = 1.0f;
		if (Surge.Progress < 0.0f)
		{
			const float Minutes = Surge.Seconds / 60.0f;
			const float FarMinutes = DisplayConfig.BarometerLookaheadFar;
			const float NearMinutes = DisplayConfig.BarometerLookaheadNear;
			if (FarMinutes > NearMinutes)
			{
				Target = clampr((FarMinutes - Minutes) / (FarMinutes - NearMinutes), 0.0f, 1.0f);
			}
			else
			{
				Target = (Minutes <= NearMinutes) ? 1.0f : 0.0f;
			}
		}

		State.Barometer += (Target - State.Barometer) * WatchDetail::BarometerResponse;
	}
	else
	{
		State.Barometer = 0.0f;
		SetBarometerText("");
		return;
	}

	if (Display.Freeze > 0.0f)
	{
		return;
	}

	const float DayMinutes = float(Level().GetGameTime() % 86400000ull) / 60000.0f;
	const float Drift = WatchDetail::BarometerDrift * (0.6f * sinf(DayMinutes * 0.21f) + 0.4f * sinf(DayMinutes * 0.047f));
	string16 Text = {};
	xr_sprintf(Text, "%.0f", DisplayConfig.BarometerNormal - State.Barometer * DisplayConfig.BarometerDrop + Drift);
	SetBarometerText(Text);
}

void CWatchDevice::SetBarometerText(const char* Text)
{
	if (State.BarometerText == Text)
	{
		return;
	}

	State.BarometerText = Text;
	if (Ui)
	{
		Ui->SetBarometer(Text);
	}
}

void CWatchDevice::UpdateLightSchedule()
{
	if (!ParseLightSchedule(Config.Light.Schedule.c_str(), Light.Keys))
	{
		Msg("! [watch] light schedule [%s] is invalid, expected ascending 'hour:day|dusk|night|dark' items", Config.Light.Schedule.size() ? Config.Light.Schedule.c_str() : "");
	}
	Light.Timer = 0.0f;
}

void CWatchDevice::UpdateLight(float Dt)
{
	if (Preview.PreviewEmission)
	{
		State.Emission = Preview.PreviewEmissionValue;
		return;
	}

	if (!g_pGameLevel)
	{
		return;
	}

	Light.Timer -= Dt;
	if (Light.Timer > 0.0f)
	{
		return;
	}

	Light.Timer = 1.0f / WatchDetail::LightSampleHz;
	const float Hour = float(Level().GetGameTime() % 86400000ull) / 3600000.0f;
	State.Emission = EvaluateLightSchedule(Light.Keys, Config.Light, Hour) * State.DisplayQuality;
}

void CWatchDevice::CreateHudPointLight(ref_light& PointLight, float Range)
{
	DestroyHudPointLight(PointLight);
	PointLight = ::Render->light_create();
	PointLight->set_type(IRender_Light::POINT);
	PointLight->set_shadow(false);
	PointLight->set_hud_mode(true);
	PointLight->set_occq_mode(false);
	PointLight->set_range(std::max(0.0f, Range));
	PointLight->set_active(false);
}

void CWatchDevice::DestroyHudPointLight(ref_light& PointLight)
{
	if (!PointLight)
	{
		return;
	}

	PointLight->set_active(false);
	PointLight.destroy();
}

void CWatchDevice::TurnOffHudPointLight(ref_light& PointLight)
{
	if (PointLight && PointLight->get_active())
	{
		PointLight->set_active(false);
	}
}

void CWatchDevice::DestroyHudLights()
{
	for (SWatchLedRuntime& Led : Leds)
	{
		DestroyHudPointLight(Led.Light);
	}
}

void CWatchDevice::TurnOffHudLights()
{
	for (SWatchLedRuntime& Led : Leds)
	{
		TurnOffHudPointLight(Led.Light);
	}
}

bool CWatchDevice::MakeLedXform(const SWatchLedRuntime& Led, const Fmatrix& WatchesXform, Fmatrix& Out) const
{
	if (!Model)
	{
		return false;
	}

	if (Led.HiddenBone != BI_NONE)
	{
		const IBoneData& Data = Model->LL_GetData(Led.HiddenBone);
		const u16 Parent = Data.GetParentID();
		Out.mul(WatchesXform, Parent != BI_NONE ? Model->LL_GetTransform(Parent) : Fidentity);
		Out.mulB_43(Data.get_bind_transform());
		Out.mulB_43(Led.Offset);
		return true;
	}

	if (Led.Bone == BI_NONE || !Model->LL_GetBoneVisible(Led.Bone))
	{
		return false;
	}

	Out.mul(WatchesXform, Model->LL_GetTransform(Led.Bone));
	Out.mulB_43(Led.Offset);
	return true;
}

void CWatchDevice::SyncChannelLight(EWatchLedChannel Channel, const Fmatrix& WatchesXform)
{
	const SWatchChannelPresent& Present = Config.Present[u32(Channel)];
	SWatchLedRuntime& Led = Leds[u32(Channel)];

	if (!Present.Enabled || !Present.PresentShader || !Present.LightEnabled)
	{
		DestroyHudPointLight(Led.Light);
		return;
	}

	Fmatrix Xform;
	if (Led.Value <= EPS || !MakeLedXform(Led, WatchesXform, Xform))
	{
		TurnOffHudPointLight(Led.Light);
		return;
	}

	const float Strength = clampr(Led.Value * Present.LightBrightness * (Present.Brightness / WatchLedMaxBrightness), 0.0f, WatchDetail::HudLightMaxStrength);
	if (Strength <= EPS)
	{
		TurnOffHudPointLight(Led.Light);
		return;
	}

	if (!Led.Light)
	{
		CreateHudPointLight(Led.Light, Present.LightRange);
		if (!Led.Light)
		{
			return;
		}
	}

	const WatchDetail::SWatchLedChannelDesc& Desc = WatchDetail::ChannelDesc(Channel);
	const u32 Rgba = Ui ? Ui->GetGlowColor(Desc.Glow) : Desc.FallbackRgba;
	Fcolor Color;
	Color.set(
		float(color_get_R(Rgba)) * (1.0f / 255.0f) * Strength,
		float(color_get_G(Rgba)) * (1.0f / 255.0f) * Strength,
		float(color_get_B(Rgba)) * (1.0f / 255.0f) * Strength,
		1.0f
	);

	Led.Light->set_position(Xform.c);
	Led.Light->set_color(Color);
	Led.Light->set_range(std::max(0.0f, Present.LightRange));
	if (!Led.Light->get_active())
	{
		Led.Light->set_active(true);
	}
}

void CWatchDevice::SyncHudLights(const Fmatrix& WatchesXform)
{
	if (!IsEnabled() || !Model)
	{
		TurnOffHudLights();
		return;
	}

	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		SyncChannelLight(EWatchLedChannel(Index), WatchesXform);
	}
}

void CWatchDevice::UpdateAnomalyExclude()
{
	Zones.AnomalyExclude.clear();
	const char* List = Config.Anomaly.Exclude.c_str();
	const int Count = _GetItemCount(List);
	for (int Index = 0; Index < Count; ++Index)
	{
		string128 Item = {};
		_GetItem(List, Index, Item);
		if (Item[0])
		{
			Zones.AnomalyExclude.emplace_back(Item);
		}
	}
	Zones.SampleTimer = 0.0f;
}

float CWatchDevice::SampleChannel(EWatchLedChannel Channel) const
{
	switch (Channel)
	{
		case EWatchLedChannel::Anomaly:
			return Zones.AnomalyTarget;
		case EWatchLedChannel::Motion:
			return (IsChannelActive(Channel) && g_pMotionIcon) ? g_pMotionIcon->GetThreatNormalized() : 0.0f;
		case EWatchLedChannel::Noise:
			if (IsChannelActive(Channel))
			{
				if (const CActor* Player = Actor())
				{
					return clampr(Player->m_snd_noise, 0.0f, 1.0f);
				}
			}
			return 0.0f;
		default:
			return 0.0f;
	}
}

void CWatchDevice::UpdateChannel(EWatchLedChannel Channel, float Dt)
{
	const u32 Index = u32(Channel);
	float& Intensity = State.Intensity[Index];
	if (Preview.PreviewChannel[Index])
	{
		Intensity = clampr(Preview.PreviewChannelValue[Index], 0.0f, 1.0f);
	}
	else
	{
		Intensity = SmoothTowards(Intensity, SampleChannel(Channel), Dt, WatchDetail::LedSmoothing);
	}

	UpdateLedBlink(Leds[Index], Config.Blink[Index], Intensity, Dt);
}

bool CWatchDevice::IsConditionAvailable(EWatchCondition Condition) const
{
	switch (Condition)
	{
		case EWatchCondition::Thirst:
			return EngineExternal()[EEngineExternalGame::EnableThirst];
		case EWatchCondition::Sleepiness:
			return EngineExternal()[EEngineExternalGame::EnableSleepiness];
		case EWatchCondition::Intoxication:
			return EngineExternal()[EEngineExternalGame::EnableMedIntoxication];
		default:
			return true;
	}
}

bool CWatchDevice::IsConditionActive(EWatchCondition Condition) const
{
	const SWatchConditionPresent& Present = Config.Conditions[u32(Condition)];
	return Present.Enabled && Present.PresentUi && IsConditionAvailable(Condition);
}

float CWatchDevice::SampleCondition(EWatchCondition Condition) const
{
	if (!IsConditionAvailable(Condition))
	{
		return 0.0f;
	}

	CActor* Player = Actor();
	if (!Player)
	{
		return 0.0f;
	}

	CActorCondition& Cond = Player->conditions();
	switch (Condition)
	{
		case EWatchCondition::Health:
			return clampr(Cond.GetHealth(), 0.0f, 1.0f);
		case EWatchCondition::Power:
			return clampr(Cond.GetPower(), 0.0f, 1.0f);
		case EWatchCondition::Radiation:
			return clampr(Cond.GetRadiation(), 0.0f, 1.0f);
		case EWatchCondition::Satiety:
			return clampr(Cond.GetSatiety(), 0.0f, 1.0f);
		case EWatchCondition::Thirst:
			return clampr(Cond.GetThirst(), 0.0f, 1.0f);
		case EWatchCondition::Sleepiness:
			return clampr(Cond.GetSleepiness(), 0.0f, 1.0f);
		case EWatchCondition::Intoxication:
			return clampr(Cond.GetIntoxication(), 0.0f, 1.0f);
		case EWatchCondition::Bleeding:
		{
			const float Normalize = Config.Conditions[u32(Condition)].Normalize;
			if (Normalize <= EPS)
			{
				return 0.0f;
			}
			return clampr(Cond.BleedingSpeed() / Normalize, 0.0f, 1.0f);
		}
		default:
			return 0.0f;
	}
}

void CWatchDevice::UpdateCondition(EWatchCondition Condition, float Dt)
{
	const u32 Index = u32(Condition);
	const SWatchConditionPresent& Present = Config.Conditions[Index];
	float& Value = State.Condition[Index];
	float& DisplayValue = State.ConditionDisplay[Index];
	float& Badness = State.ConditionBadness[Index];
	bool& Visible = State.ConditionVisible[Index];
	EWatchConditionSeverity& Severity = State.ConditionSeverity[Index];

	if (!IsConditionActive(Condition))
	{
		Value = 0.0f;
		DisplayValue = 0.0f;
		Badness = 0.0f;
		Visible = false;
		Severity = EWatchConditionSeverity::None;
		return;
	}

	float Target = Preview.PreviewCondition[Index] ? Preview.PreviewConditionValue[Index] : SampleCondition(Condition);
	Target = clampr(Target, 0.0f, 1.0f);
	Value = SmoothTowards(Value, Target, Dt, WatchDetail::ConditionSmoothing);
	DisplayValue = Present.Invert ? (1.0f - Value) : Value;
	DisplayValue = clampr(DisplayValue, 0.0f, 1.0f);
	Visible = DisplayValue >= Present.VisibleMin && DisplayValue <= Present.VisibleMax;
	Badness = WatchDetail::ConditionBadness(DisplayValue, Present.SeverityHigher);
	Severity = WatchDetail::EvaluateConditionSeverity(Badness, Present);
}

u32 CWatchDevice::ConditionDrawColor(EWatchCondition Condition, u32 Fallback) const
{
	return WatchDetail::ConditionSeverityColor(
		State.ConditionSeverity[u32(Condition)],
		Config.Conditions[u32(Condition)],
		Fallback
	);
}

float CWatchDevice::ConditionDrawGlow(EWatchCondition Condition) const
{
	const u32 Index = u32(Condition);
	const SWatchConditionPresent& Present = Config.Conditions[Index];
	return Present.Glow * WatchDetail::ConditionSeverityGlow(State.ConditionSeverity[Index], Present);
}

bool CWatchDevice::RenderUIQuery() const
{
	return Ui && Model && IsEnabled();
}

void CWatchDevice::RenderUI(const Fmatrix& WatchesXform)
{
	Fmatrix UiXform;
	UiXform.mul(WatchesXform, Model->LL_GetTransform(BoneIds.Display));
	UiXform.mulB_43(UiOffset);
	Ui->Render(UiXform);

	RenderBoneGlow(EWatchGlow::Compass, BoneIds.CompassLight, Light.Offset, Config.Light.UiSize, State.Emission, WatchesXform);
	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		RenderChannel(EWatchLedChannel(Index), WatchesXform);
	}

	if (BoneIds.Display != BI_NONE)
	{
		Fmatrix DisplayXform;
		DisplayXform.mul(WatchesXform, Model->LL_GetTransform(BoneIds.Display));
		DisplayXform.mulB_43(UiOffset);
		for (u32 Index = 0; Index < WatchConditionCount; ++Index)
		{
			RenderCondition(EWatchCondition(Index), DisplayXform);
		}
	}
}

void CWatchDevice::RenderBoneGlow(EWatchGlow Id, u16 Bone, const Fmatrix& Offset, const Fvector2& Size, float Intensity, const Fmatrix& WatchesXform)
{
	if (Intensity <= 0.0f || Bone == BI_NONE || !Model->LL_GetBoneVisible(Bone))
	{
		return;
	}

	Fmatrix GlowXform;
	GlowXform.mul(WatchesXform, Model->LL_GetTransform(Bone));
	GlowXform.mulB_43(Offset);
	Ui->RenderGlow(Id, GlowXform, Size, Intensity);
}

void CWatchDevice::RenderChannel(EWatchLedChannel Channel, const Fmatrix& WatchesXform)
{
	const SWatchChannelPresent& Present = Config.Present[u32(Channel)];
	const SWatchLedRuntime& Led = Leds[u32(Channel)];
	const EWatchGlow Glow = WatchDetail::ChannelDesc(Channel).Glow;
	if (!Present.Enabled)
	{
		return;
	}

	if (Present.PresentShader)
	{
		if (Led.HiddenBone == BI_NONE)
		{
			RenderBoneGlow(Glow, Led.Bone, Led.Offset, Present.UiSize, Led.Value, WatchesXform);
		}
		else
		{
			Fmatrix LedXform;
			if (MakeLedXform(Led, WatchesXform, LedXform))
			{
				Ui->RenderLed(Glow, LedXform, Present.GlassSize, Present.UiSize, Led.Value, Present.Brightness);
			}
		}
	}

	if (!Present.PresentUi || Led.Value <= EPS || BoneIds.Display == BI_NONE)
	{
		return;
	}

	Fmatrix Xform;
	Xform.mul(WatchesXform, Model->LL_GetTransform(BoneIds.Display));
	Xform.mulB_43(UiOffset);
	Xform.mulB_43(Led.Offset);
	Ui->RenderGlow(Glow, Xform, Present.UiSize, Led.Value);
}

void CWatchDevice::RenderCondition(EWatchCondition Condition, const Fmatrix& DisplayXform)
{
	const u32 Index = u32(Condition);
	const SWatchConditionPresent& Present = Config.Conditions[Index];
	if (!State.ConditionVisible[Index] || !Present.PresentUi || !Ui)
	{
		return;
	}

	const EWatchConditionSeverity Severity = State.ConditionSeverity[Index];
	if (Severity == EWatchConditionSeverity::None)
	{
		return;
	}

	const float Intensity = clampr(Display.Intensity * ConditionDrawGlow(Condition), 0.0f, 1.0f);
	if (Intensity <= EPS)
	{
		return;
	}

	const SWatchConditionRuntime& Runtime = ConditionRuntime[Index];
	if (Present.PresentIcon && Ui->HasConditionIcon(Condition))
	{
		Fmatrix IconXform;
		IconXform.mul(DisplayXform, Runtime.IconOffset);
		Ui->RenderConditionIcon(
			Condition,
			IconXform,
			Present.UiIconSize,
			Intensity,
			ConditionDrawColor(Condition, Ui->GetConditionIconColor(Condition))
		);
	}

	if (Present.PresentBar && Ui->HasConditionBar(Condition))
	{
		Fmatrix BarXform;
		BarXform.mul(DisplayXform, Runtime.BarOffset);
		Ui->RenderConditionBar(
			Condition,
			BarXform,
			Present.UiBarSize,
			State.ConditionBadness[Index],
			Intensity,
			ConditionDrawColor(Condition, Ui->GetConditionBarColor(Condition))
		);
	}
}

void CWatchDevice::Revert()
{
	if (!Loaded)
	{
		return;
	}

	Config = ConfigBaseline;
	Preview = SWatchDebugPreview{};
	UpdateDisplayFormats();
	UpdateLightSchedule();
	UpdateAnomalyExclude();
	UpdateLedMeshes();
	UpdateUILayout();
	CreateUI();
	DestroyHudLights();
}

void CWatchDevice::Reload()
{
	if (!Section.size())
	{
		return;
	}

	IKinematics* BoundModel = Model;
	if (BoundModel)
	{
		ResetBoneCallbacks();
	}

	DestroyHudLights();
	Load(Section);

	if (BoundModel)
	{
		Model = BoundModel;
		AttachModel();
	}

	Preview = SWatchDebugPreview{};
}
