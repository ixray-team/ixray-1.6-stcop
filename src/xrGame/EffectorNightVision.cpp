#include "StdAfx.h"
#include "EffectorNightVision.h"
#include "../xrSound/ai_sounds.h"

#include "Level.h"

#include "ActorEffector.h"
#include "CustomOutfit.h"
#include "ActorHelmet.h"
#include "Actor.h"
#include "Inventory.h"
#include "nvg.h"
#include "ui/UIGameCustom.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/IGame_Menu.h"

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

void CNightVisionEffector::RefreshNightVision()
{
	if (!GetStatus() || !IsActive())
	{
		return;
	}

	SwitchNightVision(false, false);
	m_pActor->Cameras().RemovePPEffector((EEffectorPPType)effNightvision);
	SwitchNightVision(true, false);
	if (IsActive())
	{
		PlaySounds(eIdleSound);
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
	if (ShaderNightVision)
	{
		turn_nvg = false;
	}
}

void CNightVisionEffector::Start(const shared_str& sect, bool play_sound, const Fcolor& color)
{
	nvg_color = color;
	const u32 Generation = std::min(READ_IF_EXISTS(pSettings, r_u32, sect, "shader_effect_range", 1u), 3u);
	ShaderNightVision = Generation != 0;
	m_bNightVisionOn = true;

	if (ShaderNightVision)
	{
		Fvector4 Postprocess;
		switch (Generation)
		{
			case 2:
				Postprocess.set(0.125f, 0.005f, 0.0125f, 0.9f);
				break;
			case 3:
				Postprocess.set(0.075f, 0.0025f, 0.005f, 1.0f);
				break;
			default:
				Postprocess.set(0.175f, 0.009f, 0.025f, 0.8f);
				break;
		}
		if (pSettings->line_exist(sect, "shader_nvg_postprocessing"))
		{
			Postprocess = pSettings->r_fvector4(sect, "shader_nvg_postprocessing");
		}
		g_pGamePersistent->ShaderParams.NightVisionPostprocess = Postprocess;
		if (nvg_color.a <= 1.0f)
		{
			nvg_color.a = Generation == 3 ? 15.0f : Generation == 2 ? 10.0f
																	: 8.0f;
		}
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
	m_bNightVisionOn = false;

	if (ShaderNightVision)
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
		ShaderNightVision = false;
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
	return pp != nullptr || (ShaderNightVision && turn_nvg);
}

void CNightVisionEffector::UpdateMask(CUIGameCustom& GameUI)
{
	// Interference suppresses the shader, while the powered device keeps its UI mask.
	const bool MenuActive = g_pGamePersistent && g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive();
	if (!MenuActive && m_bNightVisionOn && IsActive() && m_pActor->g_Alive() && m_pActor->HUDview())
	{
		GameUI.AddCustomStatic("nv_tunnel", true);
	}
	else
	{
		GameUI.RemoveCustomStatic("nv_tunnel");
	}
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
	ScopeAnimator.reset();
}

void TWeaponNightVision::UpdateScopePostprocess(float Factor)
{
	if (!pSettings->line_exist(m_section, "pp_eff_name"))
	{
		return;
	}
	if (!ScopeAnimator)
	{
		ScopeAnimator = xr_make_unique<CPostprocessAnimator>(effWeaponNightVision, pSettings->r_bool(m_section, "pp_eff_cyclic"));
		ScopeAnimator->Load(pSettings->r_string(m_section, "pp_eff_name"));
	}
	ScopeAnimator->SetCurrentFactor(Factor);
	// SPPInfo::lerp accumulates several fields, so start from zero like CCameraManager::UpdatePPEffectors
	Device.ScopePostprocess = pp_zero;
	Device.ScopePostprocessActive = ScopeAnimator->Valid() && ScopeAnimator->Process(Device.ScopePostprocess);
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