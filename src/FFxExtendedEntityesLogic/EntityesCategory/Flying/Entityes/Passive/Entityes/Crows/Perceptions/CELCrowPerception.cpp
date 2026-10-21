#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/Perceptions/CELCrowPerception.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELSharedCrowMemory.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSight.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_debug.h"
#include "xrServerEntities/clsid_game.h"
#include "xrGame/GamePersistent.h"
#include "xrEngine/Environment.h"
#include "xrSound/ai_sounds.h"

namespace ExtendedEntityesCrowPerception
{
// Учитывает штатный отладочный режим игнорирования главного героя.
bool Ignore(const CEntityAlive& Entity)
{
	return ExtendedEntityes::IsIgnored(Entity);
}
} // namespace ExtendedEntityesCrowPerception
// Обновляет реакцию вороны на дождь и время суток с заданным интервалом.
void CELCrowPerception::ObserveWeather(SELCrowState& State, const SELCrowSettings& Settings, float DeltaTime)
{
	if (!g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv)
	{
		return;
	}
	if (s32(Device.dwTimeGlobal - NextWeather) >= 0)
	{
		CEnvironment& Environment = g_pGamePersistent->Environment();
		const float Hour = Environment.GetGameTime() / 3600.f;
		const float Rain = Environment.CurrentEnv->rain_density;
		IsRaining = Settings.RainEnabled && (IsRaining ? Rain > Settings.RainStop : Rain > Settings.RainStart);
		IsNight = Settings.NightEnabled && (Settings.NightStart > Settings.NightEnd ? (Hour >= Settings.NightStart || Hour < Settings.NightEnd) : (Hour >= Settings.NightStart && Hour < Settings.NightEnd));
		NextWeather = Device.dwTimeGlobal + u32(Settings.WeatherInterval * 1000.f);
	}
	State.Rain.Update(IsRaining, DeltaTime, Settings.RainDelay, Settings.DryDelay);
	State.Night.Update(IsNight, DeltaTime, Settings.NightDelay, Settings.WakeDelay);
}
// Обновляет личные наблюдения владельца с учётом доступного бюджета.
void CELCrowPerception::Observe(CBaseMonster& Owner, SELCrowState& State, const SELCrowSettings& Settings, float DeltaTime)
{
	CEntityAlive* PreviousThreat = ExtendedEntityes::FindEntity(KnownThreat);
	if (!ThreatWasHeard && PreviousThreat && ExtendedEntityes::IsIgnored(*PreviousThreat))
	{
		KnownThreat = u16(-1);
		State.Fear = 0.f;
	}
	if (ThreatWasHeard && State.Fear > Settings.FearCalm && CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent DebugEvent;
		DebugEvent.OwnerIdentifier = Owner.ID();
		DebugEvent.TargetIdentifier = KnownThreat;
		DebugEvent.Start = Owner.Position();
		DebugEvent.Finish = State.Danger;
		DebugEvent.Radius = .25f;
		DebugEvent.Label = "Услышанная угроза";
		CELSharedDebug::Instance().PublishEvent(DebugEvent);
		DebugEvent.Kind = EELDebugEvent::Fear;
		DebugEvent.Label = "Испуг: уход от звука";
		CELSharedDebug::Instance().PublishEvent(std::move(DebugEvent));
	}
	ObserveWeather(State, Settings, DeltaTime);
	if (s32(Device.dwTimeGlobal - NextScan) < 0)
	{
		return;
	}
	const CELSharedCrowMemory::SObservation* Observation = CELSharedCrowMemory::Instance().Observe(Owner.Position(), std::max(Settings.FoodRadius, Settings.FearRadius));
	if (!Observation)
	{
		return;
	}
	NextScan = Device.dwTimeGlobal + u32(Settings.PerceptionInterval * 1000.f);
	CEntityAlive* Candidate;
	u32 Checked;
	for (Checked = 0; Checked < Observation->Threats.size() && Checked < 16; ++Checked)
	{
		ThreatCursor %= u32(Observation->Threats.size());
		Candidate = ExtendedEntityes::FindEntity(Observation->Threats[ThreatCursor++]);
		if (!Candidate || !Candidate->g_Alive() || ExtendedEntityesCrowPerception::Ignore(*Candidate) || !CELSharedSight::Visible(Owner, *Candidate, 360.f, Settings.FearRadius))
		{
			continue;
		}
		KnownThreat = Candidate->ID();
		ThreatWasHeard = false;
		State.Danger = Candidate->Position();
		State.Fear = std::min(1.f, State.Fear + Settings.FearRise * Settings.PerceptionInterval);
		break;
	}
	if (State.Satiety > Settings.Hungry)
	{
		return;
	}
	for (Checked = 0; Checked < Observation->Food.size() && Checked < 8; ++Checked)
	{
		FoodCursor %= u32(Observation->Food.size());
		Candidate = ExtendedEntityes::FindEntity(Observation->Food[FoodCursor++]);
		if (!Candidate || Candidate->g_Alive() || !CELSharedSight::Visible(Owner, *Candidate, 360.f, Settings.FoodRadius))
		{
			continue;
		}
		KnownFood = Candidate->ID();
		break;
	}
}
// Обрабатывает принятое звуковое событие по настроенным правилам реакции.
void CELCrowPerception::Hear(CBaseMonster& Owner, SELCrowState& State, const SELCrowSettings& Settings, const SELSoundEvent& Event, const SELPerceptionProfile& Profile)
{
	if (Event.SourceIdentifier == Owner.ID() || Event.Power < Settings.NoisePower)
	{
		return;
	}
	if (!Event.SoundType || Event.SoundType == u32(-1) || !_valid(Event.Position))
	{
		return;
	}
	const EELNoiseType Type = CELSharedHearing::Classify(Event);
	if (!(Profile.FearNoiseTypes & ELNoiseTypeMask(Type)))
	{
		return;
	}
	float Radius = Type == EELNoiseType::Explosion ? Settings.ExplosionRadius : Type == EELNoiseType::Shot ? Settings.ShotRadius
																										   : Settings.NoiseRadius;
	if (Event.IsSuppressed)
	{
		Radius *= Profile.SuppressedRatio;
	}
	if (Radius <= 0.f || Owner.Position().distance_to_sqr(Event.Position) > Radius * Radius)
	{
		return;
	}
	const float FearGain = Type == EELNoiseType::Explosion ? Settings.ExplosionFear : Type == EELNoiseType::Shot || Type == EELNoiseType::BulletHit ? Settings.ShotFear
																																					: Settings.NoiseFearGain;
	if (CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent DebugEvent;
		DebugEvent.OwnerIdentifier = Owner.ID();
		DebugEvent.TargetIdentifier = Event.SourceIdentifier;
		DebugEvent.Start = Owner.Position();
		DebugEvent.Finish = Event.Position;
		DebugEvent.Radius = .25f;
		DebugEvent.Label = CELSharedPerceptionProfiles::NoiseName(Type);
		CELSharedDebug::Instance().PublishEvent(std::move(DebugEvent));
	}
	KnownThreat = Event.SourceIdentifier;
	ThreatWasHeard = true;
	State.Danger = Event.Position;
	State.Fear = std::min(1.f, State.Fear + FearGain);
}
// Проверяет известные угрозы рядом с предполагаемым местом кормления.
bool CELCrowPerception::IsFoodSafe(CBaseMonster& Owner, const Fvector& Position, float Radius) const
{
	const CELSharedCrowMemory::SObservation* Observation = CELSharedCrowMemory::Instance().Observe(Position, Radius);
	if (!Observation)
	{
		return false;
	}
	xr_vector<u16>::const_iterator ThreatIdentifier;
	CEntityAlive* Threat;
	for (ThreatIdentifier = Observation->Threats.begin(); ThreatIdentifier != Observation->Threats.end(); ++ThreatIdentifier)
	{
		Threat = ExtendedEntityes::FindEntity(*ThreatIdentifier);
		if (Threat && Threat->g_Alive() && !ExtendedEntityesCrowPerception::Ignore(*Threat) && Threat->Position().distance_to_sqr(Position) < Radius * Radius && CELSharedSight::Check(Owner, *Threat, 360.f, Owner.eye_range) != EELSightResult::Occluded)
		{
			return false;
		}
	}
	return true;
}
// Удаляет запись, относящуюся к указанному объекту.
void CELCrowPerception::Remove(u16 ObjectIdentifier)
{
	if (KnownFood == ObjectIdentifier)
	{
		KnownFood = u16(-1);
	}
	if (KnownThreat == ObjectIdentifier)
	{
		KnownThreat = u16(-1);
	}
}
