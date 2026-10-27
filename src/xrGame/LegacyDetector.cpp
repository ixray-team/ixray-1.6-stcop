#include "StdAfx.h"
#include "LegacyDetector.h"
#include "Actor.h"
#include "AnomalyZone.h"
#include "Level.h"
#include "map_manager.h"
#include "ParticlesObject.h"
#include "ActorEffector.h"
#include "CameraEffector.h"
#include "../xrSound/ai_sounds.h"

CLegacyDetector::~CLegacyDetector()
{
	for (auto& [ClassId, Type] : ZoneTypes)
	{
		HUD_SOUND_ITEM::DestroySound(Type.Sound);
	}
}

void CLegacyDetector::Load(const char* Section)
{
	inherited::Load(Section);
	DetectionRadius = pSettings->r_float(Section, "radius");
	DetectorType = pSettings->r_u32(Section, "ef_detector_type");
	NightVisionParticle = READ_IF_EXISTS(pSettings, r_string, Section, "night_vision_particle", "");

	for (u32 Index = 1;; ++Index)
	{
		string256 Key;
		xr_sprintf(Key, "zone_class_%u", Index);
		if (!pSettings->line_exist(Section, Key))
		{
			break;
		}

		const char* ZoneSection = pSettings->r_string(Section, Key);
		const CLASS_ID ClassId = TEXT2CLSID(pSettings->r_string(ZoneSection, "class"));
		ZoneType& Type = ZoneTypes[ClassId];
		xr_sprintf(Key, "zone_min_freq_%u", Index);
		Type.MinFrequency = pSettings->r_float(Section, Key);
		xr_sprintf(Key, "zone_max_freq_%u", Index);
		Type.MaxFrequency = pSettings->r_float(Section, Key);
		xr_sprintf(Key, "zone_sound_%u_", Index);
		HUD_SOUND_ITEM::LoadSound(Section, Key, Type.Sound, SOUND_TYPE_ITEM);
		xr_sprintf(Key, "zone_map_location_%u", Index);
		Type.MapLocation = READ_IF_EXISTS(pSettings, r_string, Section, Key, "");
	}
}

void CLegacyDetector::net_Destroy()
{
	SetWorking(false);
	inherited::net_Destroy();
}

void CLegacyDetector::OnH_B_Independent(bool JustBeforeDestroy)
{
	SetWorking(false);
	inherited::OnH_B_Independent(JustBeforeDestroy);
}

void CLegacyDetector::OnMoveToSlot(const SInvItemPlace& Previous)
{
	inherited::OnMoveToSlot(Previous);
	SetWorking(true);
}

void CLegacyDetector::OnMoveToBelt(const SInvItemPlace& Previous)
{
	inherited::OnMoveToBelt(Previous);
	SetWorking(true);
}

void CLegacyDetector::OnMoveToRuck(const SInvItemPlace& Previous)
{
	inherited::OnMoveToRuck(Previous);
	SetWorking(false);
}

void CLegacyDetector::SetWorking(bool Enabled)
{
	Working = Enabled;
	if (!Working)
	{
		ClearZones();
		for (auto& [ClassId, Type] : ZoneTypes)
		{
			HUD_SOUND_ITEM::StopSound(Type.Sound);
		}
	}
}

void CLegacyDetector::ClearZones()
{
	for (auto& [Zone, Info] : Zones)
	{
		RemoveZone(Zone, Info);
	}
	Zones.clear();
	feel_touch.clear();
}

bool CLegacyDetector::IsZoneVisible(CAnomalyZone* Zone) const
{
	return READ_IF_EXISTS(pSettings, r_bool, Zone->cNameSect(), "visible_by_detector", true);
}

bool CLegacyDetector::feel_touch_contact(CObject* Object)
{
	CAnomalyZone* Zone = smart_cast<CAnomalyZone*>(Object);
	return Zone && Zone->IsEnabled();
}

void CLegacyDetector::feel_touch_new(CObject* Object)
{
	CAnomalyZone* Zone = smart_cast<CAnomalyZone*>(Object);
	if (!Zone)
	{
		return;
	}
	ZoneInfo& Info = Zones[Zone];
	const auto Type = ZoneTypes.find(Zone->CLS_ID);
	if (Type != ZoneTypes.end() && Type->second.MapLocation.size() && IsZoneVisible(Zone))
	{
		Level().MapManager().AddMapLocation(Type->second.MapLocation, Zone->ID());
		Info.MapLocationAdded = true;
	}
}

void CLegacyDetector::RemoveZone(CAnomalyZone* Zone, ZoneInfo& Info)
{
	Particles::Details::Destroy(Info.Particle);
	const auto Type = ZoneTypes.find(Zone->CLS_ID);
	if (Info.MapLocationAdded && Type != ZoneTypes.end())
	{
		Level().MapManager().RemoveMapLocation(Type->second.MapLocation, Zone->ID());
	}
}

void CLegacyDetector::feel_touch_delete(CObject* Object)
{
	CAnomalyZone* Zone = smart_cast<CAnomalyZone*>(Object);
	const auto It = Zones.find(Zone);
	if (It != Zones.end())
	{
		RemoveZone(Zone, It->second);
		Zones.erase(It);
	}
}

void CLegacyDetector::shedule_Update(u32 Delta)
{
	inherited::shedule_Update(Delta);
	if (!Working || !H_Parent() || H_Parent() != Level().CurrentViewEntity())
	{
		if (!Zones.empty())
		{
			ClearZones();
		}
		return;
	}
	Position().set(H_Parent()->Position());
	Fvector Position = H_Parent()->Position();
	feel_touch_update(Position, DetectionRadius);
}

void CLegacyDetector::UpdateCL()
{
	inherited::UpdateCL();
	CActor* Actor = H_Parent() ? H_Parent()->cast_actor() : nullptr;
	if (!Working || !Actor || Actor != Level().CurrentViewEntity() || DetectionRadius <= 0.0f)
	{
		return;
	}

	for (auto& [Zone, Info] : Zones)
	{
		const auto It = ZoneTypes.find(Zone->CLS_ID);
		if (It == ZoneTypes.end() || !IsZoneVisible(Zone))
		{
			continue;
		}
		ZoneType& Type = It->second;
		const float Distance = std::max(0.0f, Actor->Position().distance_to(Zone->Position()) - 0.8f * Zone->Radius());
		float RelativePower = 1.0f - Distance / DetectionRadius;
		clamp(RelativePower, 0.0f, 1.0f);
		const float Frequency = Type.MinFrequency + (Type.MaxFrequency - Type.MinFrequency) * RelativePower * RelativePower * RelativePower * RelativePower;
		if (Frequency > 0.0f && Info.SoundTime > 1000.0f / Frequency)
		{
			Info.SoundTime = 0;
			HUD_SOUND_ITEM::PlaySound(Type.Sound, Fvector().set(0.0f, 0.0f, 0.0f), this, true);
		}
		else
		{
			Info.SoundTime += Device.dwTimeDelta;
		}
	}
	UpdateNightVision();
}

void CLegacyDetector::UpdateNightVision()
{
	CActor* Actor = H_Parent() ? H_Parent()->cast_actor() : nullptr;
	const bool Enabled = Actor && Actor == Level().CurrentViewEntity() && NightVisionParticle.size() &&
		Actor->Cameras().GetPPEffector(EEffectorPPType(effNightvision));
	for (auto& [Zone, Info] : Zones)
	{
		if (!Enabled)
		{
			Particles::Details::Destroy(Info.Particle);
			continue;
		}
		if (!Info.Particle)
		{
			Info.Particle = Particles::Details::Create(NightVisionParticle.c_str(), false);
		}
		Info.Particle->UpdateParent(Zone->XFORM(), Fvector().set(0.0f, 0.0f, 0.0f));
		if (!Info.Particle->IsPlaying())
		{
			Info.Particle->Play(false);
		}
	}
}
