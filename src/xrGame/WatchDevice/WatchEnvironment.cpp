#include "StdAfx.h"
#include "pch_script.h"
#include "WatchEnvironment.h"
#include "WatchInternal.h"
#include <luabind/functor.hpp>
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

static bool IsAnomalyExcluded(CAnomalyZone* Zone, const SWatchAnomalySensor& Sensor, const xr_vector<xr_string>& Exclude)
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

static float EvaluateAnomalyZoneIntensity(CAnomalyZone* Zone, const Fvector& ActorPos, const SWatchAnomalySensor& Sensor, const xr_vector<xr_string>& Exclude)
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

static bool SampleMapLocation(CMapLocation* Location, EWatchCompassSourceKind Kind, SWatchCompassSample& Out)
{
	if (!Location || !Location->Update())
	{
		return false;
	}

	if (Location->GetLevelName() != Level().name())
	{
		return false;
	}

	CObject* Entity = Level().CurrentViewEntity();
	if (!Entity)
	{
		Entity = Level().CurrentEntity();
	}
	if (!Entity)
	{
		return false;
	}

	Out.WorldDir.sub(Location->GetLastPosition(), Entity->Position());
	Out.WorldDir.y = 0.0f;
	Out.Kind = Kind;
	Out.Valid = true;
	return true;
}

class WatchGameEnvironment final : public IWatchEnvironment
{
public:
	virtual bool HasLevel() const override
	{
		return g_pGameLevel != nullptr;
	}

	virtual u32 FrameId() const override
	{
		return Device.dwFrame;
	}

	virtual u64 GameTimeMs() const override
	{
		return g_pGameLevel ? Level().GetGameTime() : 0;
	}

	virtual const Fmatrix& ViewMatrix() const override
	{
		return Device.mView;
	}

	virtual bool CanSampleZones() const override
	{
		return g_pGameLevel && g_SpatialSpace && Level().CurrentEntity();
	}

	virtual void SampleZones(bool WantGravity, bool WantAnomaly, const SWatchAnomalySensor& Sensor, const xr_vector<xr_string>& Exclude, float& Gravity, float& Anomaly) override
	{
		Gravity = 0.0f;
		Anomaly = 0.0f;
		if (!CanSampleZones())
		{
			return;
		}

		const Fvector ActorPos = Level().CurrentEntity()->Position();
		const float Radius = std::max(WantGravity ? WatchDetail::CompassGravityQueryRadius : 0.0f, WantAnomaly ? Sensor.Radius : 0.0f);

		Spatial.clear();
		g_SpatialSpace->q_sphere(
			Spatial,
			0,
			ESPATIAL_TYPE::ANOMALY_ZONE,
			ActorPos,
			Radius
		);

		for (ISpatialShared& Item : Spatial)
		{
			ISpatial* SpatialObject = Item.get();
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
			if (WantGravity)
			{
				Gravity = std::max(Gravity, EvaluateGravityZoneIntensity(Zone, ActorPos));
			}
			if (WantAnomaly)
			{
				Anomaly = std::max(Anomaly, EvaluateAnomalyZoneIntensity(Zone, ActorPos, Sensor, Exclude));
			}
			if ((!WantGravity || Gravity >= WatchDetail::CompassGravityEarlyOut) && (!WantAnomaly || Anomaly >= 1.0f))
			{
				break;
			}
		}
	}

	virtual void SampleCompassTarget(EWatchCompassMode Mode, SWatchCompassSample& Out) override
	{
		if (Mode == EWatchCompassMode::North)
		{
			Out.Valid = true;
			Out.Kind = EWatchCompassSourceKind::North;
			return;
		}

		if (!g_pGameLevel)
		{
			return;
		}

		CMapManager& MapManager = Level().MapManager();
		if (Mode == EWatchCompassMode::Nav && SampleMapLocation(MapManager.GetActiveUserNavigationLocation(), EWatchCompassSourceKind::UserNav, Out))
		{
			return;
		}

		const SQuestCompassTarget Target = MapManager.GetActiveQuestCompassTarget();
		switch (Target.Type)
		{
			case eTaskTypeStoryline: SampleMapLocation(Target.Location, EWatchCompassSourceKind::Story, Out); break;
			case eTaskTypeAdditional: SampleMapLocation(Target.Location, EWatchCompassSourceKind::Additional, Out); break;
			default: break;
		}
	}

	virtual bool IsConditionAvailable(EWatchCondition Condition) const override
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

	virtual float SampleCondition(EWatchCondition Condition, float BleedingNormalize) const override
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
				if (BleedingNormalize <= EPS)
				{
					return 0.0f;
				}
				return clampr(Cond.BleedingSpeed() / BleedingNormalize, 0.0f, 1.0f);
			}
			default:
				return 0.0f;
		}
	}

	virtual float SampleMotion() const override
	{
		return g_pMotionIcon ? g_pMotionIcon->GetThreatNormalized() : 0.0f;
	}

	virtual float SampleNoise() const override
	{
		if (const CActor* Player = Actor())
		{
			return clampr(Player->m_snd_noise, 0.0f, 1.0f);
		}
		return 0.0f;
	}

	virtual bool SampleSurge(float& SecondsToSurge, float& Progress) override
	{
		SecondsToSurge = -1.0f;
		Progress = -1.0f;
		if (SurgeBridgeMissing)
		{
			return false;
		}

		if (!SurgeBridgeResolved)
		{
			if (!ai().script_engine().functor(WatchDetail::SurgeSecondsFunction, SurgeSecondsFunction) || !ai().script_engine().functor(WatchDetail::SurgeProgressFunction, SurgeProgressFunction))
			{
				SurgeBridgeMissing = true;
				Msg("! [watch] surge bridge [%s] not found, barometer has no data", WatchDetail::SurgeSecondsFunction);
				return false;
			}
			SurgeBridgeResolved = true;
		}

		SecondsToSurge = SurgeSecondsFunction();
		Progress = SurgeProgressFunction();
		return SecondsToSurge >= 0.0f || Progress >= 0.0f;
	}

	virtual bool IsSurgeBridgeMissing() const override
	{
		return SurgeBridgeMissing;
	}

	virtual u32 ResolveScriptSource(const char* Function) override
	{
		if (!Function || !Function[0])
		{
			return u32(-1);
		}

		for (u32 Index = 0; Index < ScriptSources.size(); ++Index)
		{
			if (ScriptSources[Index].Name.equal(Function))
			{
				return Index;
			}
		}

		ScriptSources.emplace_back().Name = Function;
		return u32(ScriptSources.size() - 1);
	}

	virtual float SampleScriptSource(u32 Handle) override
	{
		if (Handle >= ScriptSources.size())
		{
			return 0.0f;
		}

		SScriptSource& Source = ScriptSources[Handle];
		if (!Source.Resolved)
		{
			Source.Resolved = true;
			Source.Valid = ai().script_engine().functor(Source.Name.c_str(), Source.Functor);
			if (!Source.Valid)
			{
				Msg("! [watch] script source [%s] not found", Source.Name.c_str());
			}
		}

		if (!Source.Valid)
		{
			return 0.0f;
		}

		const float Value = Source.Functor();
		return clampr(Value, 0.0f, 1.0f);
	}

private:
	struct SScriptSource
	{
		shared_str Name;
		luabind::functor<float> Functor;
		bool Resolved = false;
		bool Valid = false;
	};

	bool SectionLooksLikeGravity(const shared_str& ZoneSection)
	{
		if (!ZoneSection.size())
		{
			return false;
		}

		auto Cached = GravitySections.find(ZoneSection);
		if (Cached != GravitySections.end())
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

		GravitySections.emplace(ZoneSection, Result);
		return Result;
	}

	float EvaluateGravityZoneIntensity(CAnomalyZone* Zone, const Fvector& ActorPos)
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

	xr_vector<ISpatialShared> Spatial;
	xr_hash_map<shared_str, bool> GravitySections;
	luabind::functor<float> SurgeSecondsFunction;
	luabind::functor<float> SurgeProgressFunction;
	bool SurgeBridgeMissing = false;
	bool SurgeBridgeResolved = false;
	xr_vector<SScriptSource> ScriptSources;
};

xr_unique_ptr<IWatchEnvironment> CreateGameWatchEnvironment()
{
	xr_unique_ptr<IWatchEnvironment> Result;
	Result.reset(new WatchGameEnvironment());
	return Result;
}
