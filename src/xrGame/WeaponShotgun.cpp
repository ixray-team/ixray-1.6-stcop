#include "StdAfx.h"
#include "script_game_object.h"
#include "WeaponShotgun.h"
#include "Entity.h"
#include "Inventory.h"
#include "Level.h"
#include "Actor.h"
#include "script_game_object.h"

CWeaponShotgun::CWeaponShotgun()
{
	m_eSoundClose			= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	m_eSoundAddCartridge	= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	bStopReloadSignal		= false;
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
	};

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
	};
}

void CWeaponShotgun::switch2_Fire()
{
	inherited::switch2_Fire	();
	bWorking = false;
}

void CWeaponShotgun::OnAnimationEnd(u32 state) 
{
	if (!m_bTriStateReload || state != eReload || state == eReload && IsMisfire() && (HudAnimationExist("anm_reload_jammed") || HudAnimationExist("anm_reload_misfire")))
	{
		bStopReloadSignal = false;
		return inherited::OnAnimationEnd(state);
	}

	if (CActor* pActor = smart_cast<CActor*>(H_Parent()))
	{
		pActor->callback(GameObject::eActorHudAnimationEnd)(lua_game_object(), hud_sect.c_str(), m_current_motion.c_str(), state, animation_slot());
	}

	switch(m_sub_state)
	{
		case eSubstateReloadBegin:
		{
			m_sub_state = eSubstateReloadInProcess;
			SwitchState(eReload);
		}break;
		case eSubstateReloadInProcess:
		{
			if(0 != AddCartridge(1) || bStopReloadSignal)
				m_sub_state = eSubstateReloadEnd;
			SwitchState(eReload);
		}break;
		case eSubstateReloadEnd:
		{
			bStopReloadSignal = false;
			bReloadKeyPressed = false;
			bAmmotypeKeyPressed = false;
			bStopReloadSignal = false;
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
	if(m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
		return;

	CWeapon::Reload();
	m_sub_state = eSubstateReloadBegin;
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

	if(m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
	{
			switch2_EndReload();
			m_sub_state = eSubstateReloadEnd;
			return;
	};

	switch (m_sub_state)
	{
		case eSubstateReloadBegin:
			if(HaveCartridgeInInventory(1))
				switch2_StartReload();
		break;
		case eSubstateReloadInProcess:
			if(HaveCartridgeInInventory(1))
				switch2_AddCartgidge();
		break;
		case eSubstateReloadEnd:
			switch2_EndReload();
		break;
	};
}

void CWeaponShotgun::switch2_StartReload()
{
	u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, type_to_update);

	PlaySound			("sndOpen",get_LastFP());
	PlayAnimOpenWeapon	();
	SetPending			(TRUE);
}

void CWeaponShotgun::switch2_AddCartgidge	()
{
	PlaySound	("sndAddCartridge",get_LastFP());
	PlayAnimAddOneCartridgeWeapon();
	SetPending			(TRUE);
}

void CWeaponShotgun::switch2_EndReload	()
{
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, GetTargetAmmoType());
	SetPending			(FALSE);
	PlaySound			("sndClose",get_LastFP());
	PlayAnimCloseWeapon	();
}

void CWeaponShotgun::PlayAnimOpenWeapon()
{
	VERIFY(GetState()==eReload);
	PlayHUDMotion("anm_open", FALSE, GetState());
}

void CWeaponShotgun::PlayAnimAddOneCartridgeWeapon()
{
	VERIFY(GetState()==eReload);
	PlayHUDMotion("anm_add_cartridge", FALSE, GetState());
}

void CWeaponShotgun::PlayAnimCloseWeapon()
{
	VERIFY(GetState()==eReload);

	PlayHUDMotion("anm_close", FALSE, GetState());
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
