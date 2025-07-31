#include "StdAfx.h"
#include "EffectorNightVision.h"
#include "../xrSound/ai_sounds.h"

#include "Level.h"

#include "ActorEffector.h"
#include "CustomOutfit.h"
#include "ActorHelmet.h"

void CNightVisionEffector::SwitchNightVision()
{
	if (OnClient())
		return;

	SwitchNightVision(!m_bNightVisionOn);	
}

void CNightVisionEffector::SwitchNightVision(bool vision_on, bool use_sounds)
{
	if (!vision_on)
	{
		if (IsActive())
		{
			Stop(100000.0f, use_sounds);
		}
		m_bNightVisionOn = false;
		return;
	}

	CCustomOutfit* outfit = m_pActor->GetOutfit();
	CHelmet* helmet = m_pActor->GetHelmet();
	bool has_nvs = helmet && helmet->m_NightVisionSect.size() || outfit && outfit->m_NightVisionSect.size();

	if (!has_nvs)
		return;

	for (auto& map : m_disabled_maps)
	{
		if (map == Level().name())
		{
			if (use_sounds)
			{
				PlaySounds(EPlaySounds::eBrokeSound);
			}
			return;
		}
	}

	if (IsActive())
		return;

	if (helmet)
	{
		Start(helmet->m_NightVisionSect, use_sounds);
	}
	else if (outfit)
	{
		Start(outfit->m_NightVisionSect, use_sounds);
	}

	m_bNightVisionOn = true;
}

CNightVisionEffector::CNightVisionEffector(CActor* actor)
{
	m_pActor = actor;

	if (!pGameGlobals->line_exist("night_vision", "night_vision_section"))
	{
		Msg("! Night Vision not registered");
		Msg("! [night_vision] section missing");
		return;
	}

	LPCSTR section = pGameGlobals->r_string("night_vision", "night_vision_section");

	m_sounds.LoadSound(section, "snd_night_vision_on", "NightVisionOnSnd", true, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section, "snd_night_vision_off", "NightVisionOffSnd", true, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section, "snd_night_vision_idle", "NightVisionIdleSnd", true, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section, "snd_night_vision_broken", "NightVisionBrokenSnd", true, SOUND_TYPE_ITEM_USING);

	if (pSettings->line_exist(section, "night_vision_disabled_maps"))
	{
		LPCSTR disabled_maps = pSettings->r_string(section, "night_vision_disabled_maps");
		for (int i = 0, cnt = _GetItemCount(disabled_maps); i < cnt; ++i)
		{
			string512 level_name = {};
			_GetItem(disabled_maps, i, level_name);
			m_disabled_maps.push_back(level_name);
		}
	}
}

CNightVisionEffector::~CNightVisionEffector()
{
	m_sounds.StopAllSounds();
	m_pActor = nullptr;
	m_disabled_maps = {};
	m_bNightVisionOn = false;
}

void CNightVisionEffector::Start(const shared_str& sect, bool play_sound)
{
	AddEffector(m_pActor, effNightvision, sect);
	if (play_sound)
	{
		PlaySounds(eStartSound);
		PlaySounds(eIdleSound);
	}
}

void CNightVisionEffector::Stop(const float factor, bool play_sound)
{
	CEffectorPP* pp = m_pActor->Cameras().GetPPEffector((EEffectorPPType)effNightvision);
	if (pp)
	{
		pp->Stop(factor);

		if (play_sound)
		{
			PlaySounds(eStopSound);
		}

		m_sounds.StopSound("NightVisionIdleSnd");
	}
}

bool CNightVisionEffector::IsActive()
{
	CEffectorPP* pp = m_pActor->Cameras().GetPPEffector((EEffectorPPType)effNightvision);
	return pp != nullptr;
}

void CNightVisionEffector::PlaySounds(EPlaySounds which)
{
	bool bPlaySoundFirstPerson = !!m_pActor->HUDview();
	switch(which)
	{
	case eStartSound:
		{
			m_sounds.PlaySound("NightVisionOnSnd", m_pActor->Position(), nullptr, bPlaySoundFirstPerson);
		}break;
	case eStopSound:
		{
			m_sounds.PlaySound("NightVisionOffSnd", m_pActor->Position(), nullptr, bPlaySoundFirstPerson);
		}break;
	case eIdleSound:
		{
			m_sounds.PlaySound("NightVisionIdleSnd", m_pActor->Position(), nullptr, bPlaySoundFirstPerson, true);
		}break;
	case eBrokeSound:
		{
			m_sounds.PlaySound("NightVisionBrokenSnd", m_pActor->Position(), nullptr, bPlaySoundFirstPerson);
		}break;
	default: NODEFAULT;
	}
}

CWeaponNightVision::CWeaponNightVision(const shared_str& sect, CActor* actor)
{
	m_pActor = actor;
	m_section = sect;
}

CWeaponNightVision::~CWeaponNightVision()
{
	m_bNightVisionOn = false;
	m_pActor = nullptr;
}

bool CWeaponNightVision::IsActive()
{
	CEffectorPP* pp = m_pActor->Cameras().GetPPEffector((EEffectorPPType)effWeaponNightVision);
	return pp != nullptr;
}

void CWeaponNightVision::SwitchNightVision(bool vision_on)
{
	if (vision_on)
	{
		AddEffector(m_pActor, effWeaponNightVision, m_section);
		m_bNightVisionOn = true;
	}
	else
	{
		CEffectorPP* pp = m_pActor->Cameras().GetPPEffector((EEffectorPPType)effWeaponNightVision);
		if (pp)
		{
			pp->Stop(100000.0f);
			m_bNightVisionOn = false;
		}
	}
}