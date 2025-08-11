#include "StdAfx.h"
#include "script_game_object.h"
#include "WeaponShotgun.h"
#include "Entity.h"
#include "Inventory.h"
#include "Level.h"
#include "Actor.h"
#include "script_game_object.h"
#include "../xrScripts/script_callback_ex.h"

CWeaponShotgun::CWeaponShotgun()
{
	m_eSoundClose = ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	m_eSoundAddCartridge = ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
}

CWeaponShotgun::~CWeaponShotgun()
{
}

void CWeaponShotgun::net_Destroy()
{
	inherited::net_Destroy();
}

void CWeaponShotgun::Load(LPCSTR section)
{
	if (pSettings->line_exist(section, "tri_state_reload"))
	{
		m_bTriStateReload = !!pSettings->r_bool(section, "tri_state_reload");
	}

	inherited::Load(section);
}

void CWeaponShotgun::LoadSounds(LPCSTR section)
{
	inherited::LoadSounds(section);

	if (m_bTriStateReload)
	{
		m_sounds.LoadSound(section, "snd_open_weapon", "sndOpen", false, m_eSoundOpen);
		m_sounds.LoadSound(section, "snd_add_cartridge", "sndAddCartridge", false, m_eSoundAddCartridge);
		m_sounds.LoadSound(section, "snd_close_weapon", "sndClose", false, m_eSoundClose);
	}

	if (SoundExist(section, "snd_open_weapon_empty"))
	{
		m_sounds.LoadSound(section, "snd_open_weapon_empty", "sndOpenEmpty", false, m_eSoundOpen);
	}

	if (SoundExist(section, "snd_add_cartridge_empty"))
	{
		m_sounds.LoadSound(section, "snd_add_cartridge_empty", "sndAddCartridgeEmpty", false, m_eSoundAddCartridge);
	}

	if (SoundExist(section, "snd_add_cartridge_preloaded"))
	{
		m_sounds.LoadSound(section, "snd_add_cartridge_preloaded", "sndAddCartridgePreloaded", false, m_eSoundAddCartridge);
	}

	if (SoundExist(section, "snd_close_weapon_empty"))
	{
		m_sounds.LoadSound(section, "snd_close_weapon_empty", "sndCloseEmpty", false, m_eSoundClose);
	}

	if (SoundExist(section, "snd_close_weapon_preloaded"))
	{
		m_sounds.LoadSound(section, "snd_close_weapon_preloaded", "sndClosePreloaded", false, m_eSoundClose);
	}
}

void CWeaponShotgun::switch2_Fire()
{
	inherited::switch2_Fire();
	bWorking = false;
}

void CWeaponShotgun::OnAnimationEnd(u32 state)
{
	if (!m_bTriStateReload || state != eReload || state == eReload && IsMisfire() && (HudAnimationExist("anm_reload_jammed") || HudAnimationExist("anm_reload_misfire")))
	{
		bStopReloadSignal = false;
		return inherited::OnAnimationEnd(state);
	}

	if (CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
	{
		pActor->callback(GameObject::eActorHudAnimationEnd)(lua_game_object(), hud_sect.c_str(), m_current_motion.c_str(), state, animation_slot());
	}

	switch (m_sub_state)
	{
	case eSubstateReloadBegin:
	{
		if (ParentIsActor() && m_bAddCartridgeInOpen && !m_bIsReloaded)
		{
			AddCartridge(1);
		}

		if (bStopReloadSignal)
		{
			m_sub_state = eSubstateReloadEnd;
		}
		else
		{
			m_sub_state = eSubstateReloadInProcess;
		}
		SwitchState(eReload);
	}break;
	case eSubstateReloadInProcess:
	{
		if ((!ParentIsActor() && 0 != AddCartridge(1)) || ParentIsActor() && (!m_bIsReloaded && 0 != AddCartridge(1) || bStopReloadSignal))
		{
			m_bIsReloaded = true;
			m_sub_state = eSubstateReloadEnd;
		}
		SwitchState(eReload);
	}break;
	case eSubstateReloadEnd:
	{
		bStopReloadSignal = false;
		bReloadKeyPressed = false;
		bAmmotypeKeyPressed = false;
		SwitchState(eIdle);
	}break;

	};
}

void CWeaponShotgun::Reload()
{
	bool is_misfire = IsMisfire() && (HudAnimationExist("anm_reload_jammed") || HudAnimationExist("anm_reload_misfire"));

	if (is_misfire)
	{
		bMisfireReload = true;
	}

	if (m_bTriStateReload && !is_misfire)
		TriStateReload();
	else
		inherited::Reload();
}

void CWeaponShotgun::TriStateReload()
{
	if (m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
		return;

	CWeapon::Reload();
	m_sub_state = eSubstateReloadBegin;
	m_bIsReloaded = false;
	SwitchState(eReload);
}

void CWeaponShotgun::OnStateSwitch(u32 S)
{
	bool is_misfire = S == eReload && IsMisfire() && (HudAnimationExist("anm_reload_jammed") || HudAnimationExist("anm_reload_misfire"));
	if (!m_bTriStateReload || S != eReload || is_misfire)
	{
		bStopReloadSignal = false;
		inherited::OnStateSwitch(S);
		return;
	}

	CWeapon::OnStateSwitch(S);

	if ((u32)m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
	{
		switch2_EndReload();
		m_sub_state = eSubstateReloadEnd;
		return;
	};

	switch (m_sub_state)
	{
	case eSubstateReloadBegin:
	{
		if (HaveCartridgeInInventory(1))
		{
			switch2_StartReload();
		}
		break;
	}
	case eSubstateReloadInProcess:
	{
		if (HaveCartridgeInInventory(1))
		{
			switch2_AddCartgidge();
		}
		break;
	}
	case eSubstateReloadEnd:
	{
		switch2_EndReload();
		break;
	}
	};
}

void CWeaponShotgun::switch2_StartReload()
{
	u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, type_to_update);

	PlayAnimOpenWeapon();
	SetPending(TRUE);

	if (ParentIsActor() && m_sounds.FindSoundItem("sndOpenEmpty", false) && iAmmoElapsed + iAmmoChamberElapsed == 0)
	{
		PlaySound("sndOpenEmpty", get_LastFP());
	}
	else
	{
		PlaySound("sndOpen", get_LastFP());
	}
}

void CWeaponShotgun::switch2_AddCartgidge()
{
	m_bIsReloaded = false;
	PlayAnimAddOneCartridgeWeapon();
	SetPending(TRUE);

	if (ParentIsActor() && m_sounds.FindSoundItem("sndAddCartridgeEmpty", false) && iAmmoElapsed + iAmmoChamberElapsed == 0)
	{
		PlaySound("sndAddCartridgeEmpty", get_LastFP());
	}
	else if (ParentIsActor() && m_sounds.FindSoundItem("sndAddCartridgePreloaded", false) && m_bIsPreloaded)
	{
		PlaySound("sndAddCartridgePreloaded", get_LastFP());
	}
	else
	{
		PlaySound("sndAddCartridge", get_LastFP());
	}
}

void CWeaponShotgun::switch2_EndReload()
{
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, GetTargetAmmoType());
	SetPending(TRUE);

	PlayAnimCloseWeapon();

	if (ParentIsActor() && m_sounds.FindSoundItem("sndCloseEmpty", false) && iAmmoElapsed + iAmmoChamberElapsed == 0)
	{
		PlaySound("sndCloseEmpty", get_LastFP());
	}
	else if (ParentIsActor() && m_sounds.FindSoundItem("sndClosePreloaded", false) && m_bIsPreloaded)
	{
		PlaySound("sndClosePreloaded", get_LastFP());
	}
	else
	{
		PlaySound("sndClose", get_LastFP());
	}
}

shared_str CWeaponShotgun::SelectOpenWeaponAnimation()
{
	shared_str anim = "anm_open";

	if (ParentIsActor())
	{
		if (iAmmoElapsed + iAmmoChamberElapsed == 0)
		{
			AddSuffixName(anim, "_empty");
			m_bIsPreloaded = true;
			m_bJustAfterReload = true;
		}
		else if (m_bJustAfterReload)
		{
			AddSuffixName(anim, "_first");
		}
		else
		{
			m_bJustAfterReload = true;
		}
	}

	return anim;
}

void CWeaponShotgun::PlayAnimOpenWeapon()
{
	VERIFY(GetState() == eReload);

	PlayHUDMotion(SelectOpenWeaponAnimation(), false, eReload);
}

shared_str CWeaponShotgun::SelectAddCartridgeWeaponAnimation()
{
	shared_str anim = "anm_add_cartridge";

	if (ParentIsActor())
	{
		if (iAmmoElapsed + iAmmoChamberElapsed == 0)
		{
			AddSuffixName(anim, "_empty");
		}

		if (m_bIsPreloaded && AddSuffixName(anim, "_preloaded"))
		{
			m_bIsPreloaded = false;
		}
	}

	return anim;
}

void CWeaponShotgun::PlayAnimAddOneCartridgeWeapon()
{
	VERIFY(GetState() == eReload);

	PlayHUDMotion(SelectAddCartridgeWeaponAnimation(), false, eReload);
}

shared_str CWeaponShotgun::SelectCloseWeaponAnimation()
{
	shared_str anim = "anm_close";

	if (ParentIsActor())
	{
		if (m_bIsPreloaded && AddSuffixName(anim, "_preloaded"))
		{
			m_bIsPreloaded = false;
		}

		if (iAmmoElapsed + iAmmoChamberElapsed >= iMagazineSize && AddSuffixName(anim, "_final"))
		{
			m_bJustAfterReload = true;
		}
	}

	return anim;
}

void CWeaponShotgun::PlayAnimCloseWeapon()
{
	VERIFY(GetState() == eReload);

	PlayHUDMotion(SelectCloseWeaponAnimation(), false, eReload);
}

BOOL CWeaponShotgun::net_Spawn(CSE_Abstract* DC)
{
	BOOL bResult = inherited::net_Spawn(DC);

	CSE_ALifeItemWeaponShotGun* E = smart_cast<CSE_ALifeItemWeaponShotGun*>(DC);

	xr_vector<u8> ammo_ids = E->m_AmmoIDs;

	for (u32 i = 0; i < (u32)ammo_ids.size(); i++)
	{
		u8 LocalAmmoType = ammo_ids[i];
		if (i >= m_magazine.size())
		{
			continue;
		}

		CCartridge& l_cartridge = *(m_magazine.begin() + i);
		if (LocalAmmoType == l_cartridge.m_LocalAmmoType)
		{
			continue;
		}

		l_cartridge.Load(m_ammoTypes[LocalAmmoType].c_str(), LocalAmmoType);
	}

	return bResult;
}

void CWeaponShotgun::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);

	P.w_u8(u8(m_magazine.size()));

	for (u32 i = 0; i < m_magazine.size(); i++)
	{
		CCartridge& l_cartridge = *(m_magazine.begin() + i);
		P.w_u8(l_cartridge.m_LocalAmmoType);
	}
}

void CWeaponShotgun::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
	u8 AmmoCount = P.r_u8();
	for (u32 i = 0; i < AmmoCount; i++)
	{
		u8 LocalAmmoType = P.r_u8();
		if (i >= m_magazine.size())
		{
			continue;
		}

		CCartridge& l_cartridge = *(m_magazine.begin() + i);
		if (LocalAmmoType == l_cartridge.m_LocalAmmoType)
		{
			continue;
		}

#ifdef DEBUG
		Msg("! %s reload to %s", *l_cartridge.m_ammoSect, m_ammoTypes[LocalAmmoType].c_str());
#endif
		l_cartridge.Load(m_ammoTypes[LocalAmmoType].c_str(), LocalAmmoType);
	}
}

void CWeaponShotgun::OnMotionMark(u32 state, const motion_marks& mark)
{
	inherited::OnMotionMark(state, mark);

	if (m_bTriStateReload && state == eReload && mark.name == "Right")
	{
		if (m_sub_state == EWeaponSubStates::eSubstateReloadBegin)
		{
			if (iAmmoElapsed < iMagazineSize)
			{
				m_bIsReloaded = true;
				AddCartridge(1);
			}
		}
		else if (m_sub_state == EWeaponSubStates::eSubstateReloadInProcess)
		{
			if (iAmmoElapsed < iMagazineSize)
			{
				m_bIsReloaded = true;
				AddCartridge(1);
			}
		}
	}
}