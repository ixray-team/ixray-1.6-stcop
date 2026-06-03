#include "stdafx.h"
#include "EffectorNightVision.h"
#include "../xrSound/ai_sounds.h"

#include "Level.h"

#include "ActorEffector.h"
#include "CustomOutfit.h"
#include "ActorHelmet.h"
#include "Actor.h"
#include "Inventory.h"
#include "nvg.h"

ENGINE_API extern bool turn_nvg;
ENGINE_API extern Fcolor nvg_color;

void CNightVisionEffector::SwitchNightVision()
{
	if (OnClient())
	{
		return;
	}

	SwitchNightVision(!m_bNightVisionOn);	
}

void CNightVisionEffector::SwitchNightVision(bool vision_on, bool use_sounds)
{
	PIItem item_nvg_slot = Actor()->inventory().ItemFromSlot(NVG_SLOT);
	CNVG* oCNVG = (item_nvg_slot != nullptr) ? smart_cast<CNVG*>(item_nvg_slot) : nullptr;

	if (!vision_on)
	{
		if (IsActive())
		{
			Stop(100000.0f, use_sounds);
			if (oCNVG)
			{
				oCNVG->StopNvg();
			}
		}

		m_bNightVisionOn = false;

		return;
	}

	CCustomOutfit* outfit = m_pActor->GetOutfit();
	CHelmet* helmet = m_pActor->GetHelmet();

	bool has_nvs = helmet && helmet->GetNV_Sect().size() || outfit && outfit->GetNV_Sect().size() || oCNVG;

	if (!has_nvs)
	{
		return;
	}

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
	{
		return;
	}

	if (helmet && helmet->GetNV_Sect().size())
	{
		Start(helmet->GetNV_Sect(), use_sounds, helmet->GetNV_Color());
		m_bNightVisionOn = true;
	}
	else if (outfit && outfit->GetNV_Sect().size())
	{
		Start(outfit->GetNV_Sect(), use_sounds, outfit->GetNV_Color());
		m_bNightVisionOn = true;
	}
	else if (oCNVG)
	{
		if (oCNVG->StartNvg())
		{
			m_bNightVisionOn = true;
		}
	}

	
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

	const char* section = pGameGlobals->r_string("night_vision", "night_vision_section");

	m_sounds.LoadSound(section, "snd_night_vision_on", "NightVisionOnSnd", true, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section, "snd_night_vision_off", "NightVisionOffSnd", true, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section, "snd_night_vision_idle", "NightVisionIdleSnd", true, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section, "snd_night_vision_broken", "NightVisionBrokenSnd", true, SOUND_TYPE_ITEM_USING);

	if (pSettings->line_exist(section, "night_vision_disabled_maps"))
	{
		const char* disabled_maps = pSettings->r_string(section, "night_vision_disabled_maps");
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
	turn_nvg = false;
}

void CNightVisionEffector::Start(const shared_str& sect, bool play_sound, const Fcolor& color)
{
	nvg_color = color;
	static const bool used_shader_nvg = !!psDeviceFlags.test(rsR4);

	if (used_shader_nvg)
	{
		turn_nvg = true;
	}
	else
	{
		AddEffector(m_pActor, effNightvision, sect);
	}

	if (play_sound)
	{
		PlaySounds(eStartSound);
		PlaySounds(eIdleSound);
	}
}

void CNightVisionEffector::Stop(const float factor, bool play_sound)
{
	static const bool used_shader_nvg = !!psDeviceFlags.test(rsR4);

	if (used_shader_nvg)
	{
		if (turn_nvg)
		{
			turn_nvg = false;
			if (play_sound)
			{
				PlaySounds(eStopSound);
			}

			m_sounds.StopSound("NightVisionIdleSnd");
		}
	}
	else
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
}

bool CNightVisionEffector::IsActive()
{
	CEffectorPP* pp = m_pActor->Cameras().GetPPEffector((EEffectorPPType)effNightvision);
	return pp != nullptr || turn_nvg;
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

void TWeaponNightVision::Load(const shared_str& sect)
{
	m_section = sect;
}

void TWeaponNightVision::EndComponent()
{
	m_bNightVisionOn = false;
}

bool TWeaponNightVision::IsActive()
{
	CObject* CurrentEntity = Level().CurrentEntity();
	CActor* pActor = CurrentEntity != nullptr ? CurrentEntity->cast_actor() : nullptr;

	if (pActor == nullptr)
	{
		return false;
	}

	CEffectorPP* pp = pActor->Cameras().GetPPEffector((EEffectorPPType)effWeaponNightVision);
	return pp != nullptr;
}

void TWeaponNightVision::SwitchNightVision(bool vision_on)
{
	CObject* CurrentEntity = Level().CurrentEntity();
	CActor* pActor = CurrentEntity != nullptr ? CurrentEntity->cast_actor() : nullptr;

	if (pActor == nullptr)
	{
		return;
	}
	
	if (vision_on)
	{
		AddEffector(pActor, effWeaponNightVision, m_section);
		m_bNightVisionOn = true;
	}
	else
	{
		CEffectorPP* pp = pActor->Cameras().GetPPEffector((EEffectorPPType)effWeaponNightVision);
		if (pp)
		{
			pp->Stop(100000.0f);
			m_bNightVisionOn = false;
		}
	}
}