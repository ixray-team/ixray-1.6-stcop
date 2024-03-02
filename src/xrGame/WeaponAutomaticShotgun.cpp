#include "stdafx.h"
#include "script_game_object.h"
#include "WeaponAutomaticShotgun.h"
#include "entity.h"
#include "../xrEngine/xr_level_controller.h"
#include "inventory.h"
#include "level.h"
#include "actor.h"

CWeaponAutomaticShotgun::CWeaponAutomaticShotgun()
{
	m_eSoundClose			= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	m_eSoundAddCartridge	= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	bStopReloadSignal		= false;
}

CWeaponAutomaticShotgun::~CWeaponAutomaticShotgun()
{
}

void CWeaponAutomaticShotgun::Load(LPCSTR section)
{
	inherited::Load(section);

	if (pSettings->line_exist(section, "tri_state_reload")) {
		m_bTriStateReload = !!pSettings->r_bool(section, "tri_state_reload");
	};
	if (m_bTriStateReload)
	{
		m_sounds.LoadSound(section, "snd_open_weapon", "sndOpen", false, m_eSoundOpen);
		m_sounds.LoadSound(section, "snd_add_cartridge", "sndAddCartridge", false, m_eSoundAddCartridge);

		if (WeaponSoundExist(section, "snd_add_cartridge_empty"))
			m_sounds.LoadSound(section, "snd_add_cartridge_empty", "sndAddCartridgeEmpty", false, m_eSoundAddCartridge);

		m_sounds.LoadSound(section, "snd_close_weapon", "sndClose", false, m_eSoundClose);

		if (WeaponSoundExist(section, "snd_close_weapon_empty"))
			m_sounds.LoadSound(section, "snd_close_weapon_empty", "sndCloseEmpty", false, m_eSoundClose);

		if (pSettings->line_exist(hud_sect, "add_cartridge_in_open"))
			m_bAddCartridgeOpen = pSettings->r_bool(hud_sect, "add_cartridge_in_open");

		if (pSettings->line_exist(hud_sect, "empty_preload_mode"))
			m_bEmptyPreloadMode = pSettings->r_bool(hud_sect, "empty_preload_mode");

		if (m_bAddCartridgeOpen)
			m_sounds.LoadSound(section, "snd_open_weapon_empty", "sndOpenEmpty", false, m_eSoundOpen);

		if (m_bEmptyPreloadMode)
		{
			m_sounds.LoadSound(section, "snd_add_cartridge_preloaded", "sndAddCartridgePreloaded", false, m_eSoundOpen);
			m_sounds.LoadSound(section, "snd_close_weapon_preloaded", "sndClosePreloaded", false, m_eSoundClose);
		}
	};

}

bool CWeaponAutomaticShotgun::SwitchAmmoType(u32 flags)
{
	if (IsTriStateReload() && iAmmoElapsed == iMagazineSize)
		return false;

	return inherited::SwitchAmmoType(flags);
}

bool CWeaponAutomaticShotgun::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags))
		return true;

	if(m_bTriStateReload && GetState() == eReload && (m_sub_state == eSubstateReloadInProcess || m_sub_state == eSubstateReloadBegin) && cmd == kWPN_FIRE && flags & CMD_START) //остановить перезарядку
	{
		bStopReloadSignal = true;
		return true;
	}
	return false;
}

void CWeaponAutomaticShotgun::OnAnimationEnd(u32 state)
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (!m_bTriStateReload || state != eReload || (IsMisfire() && isGuns || IsMisfire() && isHUDAnimationExist("anm_reload_misfire")))
	{
		bStopReloadSignal = false;
		bPreloadAnimAdapter = false;
		return inherited::OnAnimationEnd(state);
	}

	if (CActor* pActor = smart_cast<CActor*>(H_Parent()))
	{
		pActor->callback(GameObject::eActorHudAnimationEnd)(lua_game_object(), hud_sect.c_str(), m_current_motion.c_str(), state, animation_slot());
	}

	switch (m_sub_state)
	{
	case eSubstateReloadBegin:
	{
		if (bStopReloadSignal || iAmmoElapsed == iMagazineSize)
		{
			m_sub_state = eSubstateReloadEnd;
			SwitchState(eReload);
		}
		else
		{
			m_sub_state = eSubstateReloadInProcess;
			SwitchState(eReload);
		}
	}break;
	case eSubstateReloadInProcess:
	{
		if (0 != AddCartridge(1) || bStopReloadSignal)
			m_sub_state = eSubstateReloadEnd;

		SwitchState(eReload);
	}break;
	case eSubstateReloadEnd:
	{
		bStopReloadSignal = false;
		bReloadKeyPressed = false;
		bAmmotypeKeyPressed = false;
		m_sub_state = eSubstateReloadBegin;
		SwitchState(eIdle);
	}break;

	};
}

bool CWeaponAutomaticShotgun::TryReload() 
{
	if (m_bTriStateReload)
		return TriStateReload();

	return inherited::TryReload();
}

bool CWeaponAutomaticShotgun::TriStateReload()
{
	if(m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
		return false;

	m_sub_state = eSubstateReloadBegin;
	SwitchState(eReload);
	return true;
}

void CWeaponAutomaticShotgun::OnStateSwitch(u32 S)
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (!m_bTriStateReload || S != eReload || (IsMisfire() && isGuns || IsMisfire() && isHUDAnimationExist("anm_reload_misfire")))
	{
		bStopReloadSignal = false;
		bPreloadAnimAdapter = false;
		inherited::OnStateSwitch(S);
		return;
	}

	CWeapon::OnStateSwitch(S);

	if (m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
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
			bool TempTest = m_bAmmoInChamber ? iAmmoElapsed == 0 && iAmmoInChamberElapsed == 0 : iAmmoElapsed == 0;
			if (TempTest && m_bAddCartridgeOpen || !bPreloadAnimAdapter)
				AddCartridge(1);
		}
	}break;
	case eSubstateReloadInProcess:
	{
		if (HaveCartridgeInInventory(1))
			switch2_AddCartgidge();
	}break;
	case eSubstateReloadEnd:
		switch2_EndReload();
		break;
	};
}

void CWeaponAutomaticShotgun::switch2_StartReload()
{
	PlayAnimOpenWeapon();
	SetPending(TRUE);

	bool TempTest = m_bAmmoInChamber ? iAmmoElapsed == 0 && iAmmoInChamberElapsed == 0 : iAmmoElapsed == 0;

	if (m_sounds.FindSoundItem("sndOpenEmpty", false) && m_bAddCartridgeOpen && TempTest)
		PlaySound("sndOpenEmpty", get_LastFP());
	else
		PlaySound("sndOpen", get_LastFP());
}

void CWeaponAutomaticShotgun::switch2_AddCartgidge()
{
	PlayAnimAddOneCartridgeWeapon();
	SetPending(TRUE);

	bool TempTest = m_bAmmoInChamber ? iAmmoElapsed == 0 && iAmmoInChamberElapsed == 0 : iAmmoElapsed == 0;

	if (m_sounds.FindSoundItem("sndAddCartridgeEmpty", false) && !m_bAddCartridgeOpen && TempTest)
		PlaySound("sndAddCartridgeEmpty", get_LastFP());
	else if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
		PlaySound("sndAddCartridgePreloaded", get_LastFP());
	else
		PlaySound("sndAddCartridge", get_LastFP());
}

void CWeaponAutomaticShotgun::switch2_EndReload()
{
	SetPending(TRUE);
	PlayAnimCloseWeapon();

	bool TempTest = m_bAmmoInChamber ? iAmmoElapsed == 0 && iAmmoInChamberElapsed == 0 : iAmmoElapsed == 0;

	if (m_sounds.FindSoundItem("sndCloseEmpty", false) && !m_bAddCartridgeOpen && TempTest)
		PlaySound("sndCloseEmpty", get_LastFP());
	else if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
		PlaySound("sndClosePreloaded", get_LastFP());
	else
		PlaySound("sndClose", get_LastFP());
}

void CWeaponAutomaticShotgun::PlayAnimOpenWeapon()
{
	VERIFY(GetState() == eReload);

	xr_string anm_name = "anm_open";

	bool TempTest = m_bAmmoInChamber ? iAmmoElapsed == 0 && iAmmoInChamberElapsed == 0 : iAmmoElapsed == 0;

	if (m_bEmptyPreloadMode && TempTest)
	{
		anm_name += "_empty";
		bPreloadAnimAdapter = true;
	}

	PlayHUDMotion(anm_name, false, this, GetState(), false);
}

void CWeaponAutomaticShotgun::PlayAnimAddOneCartridgeWeapon()
{
	VERIFY(GetState() == eReload);

	xr_string anm_name = "anm_add_cartridge";

	bool TempTest = m_bAmmoInChamber ? iAmmoElapsed == 0 && iAmmoInChamberElapsed == 0 : iAmmoElapsed == 0;

	if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
	{
		if (TempTest)
			anm_name += "_empty_preloaded";
		else
			anm_name += "_preloaded";

		bPreloadAnimAdapter = false;
	}
	else if (!m_bAddCartridgeOpen && TempTest)
		anm_name += "_empty";

	PlayHUDMotion(anm_name, false, this, GetState(), false);
}

void CWeaponAutomaticShotgun::PlayAnimCloseWeapon()
{
	VERIFY(GetState() == eReload);

	xr_string anm_name = "anm_close";

	bool TempTest = m_bAmmoInChamber ? iAmmoElapsed == 0 && iAmmoInChamberElapsed == 0 : iAmmoElapsed == 0;

	if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
	{
		if (TempTest)
			anm_name = "anm_add_cartridge_empty_preloaded";
		else
			anm_name += "_preloaded";

		bPreloadAnimAdapter = false;
	}
	else if (!m_bAddCartridgeOpen && TempTest)
		anm_name = "anm_add_cartridge_empty";

	PlayHUDMotion(anm_name, false, this, GetState(), false);
}

bool CWeaponAutomaticShotgun::HaveCartridgeInInventory(u8 cnt)
{
	if (unlimited_ammo())	return true;
	if(!m_pInventory)		return false;

	u32 ac = GetAmmoCount(m_ammoType);
	if(ac<cnt)
	{
		for(u8 i = 0; i < u8(m_ammoTypes.size()); ++i) 
		{
			if(m_ammoType==i) continue;
			ac	+= GetAmmoCount(i);
			if(ac >= cnt)
			{
				m_ammoType = i;
				break; 
			}
		}
	}
	return ac>=cnt;
}


u8 CWeaponAutomaticShotgun::AddCartridge		(u8 cnt)
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (IsMisfire() && !isGuns)
		bMisfire = false;

	if ( m_set_next_ammoType_on_reload != undefined_ammo_type )
	{
		m_ammoType						= m_set_next_ammoType_on_reload;
		m_set_next_ammoType_on_reload	= undefined_ammo_type;
	}

	if( !HaveCartridgeInInventory(1) )
		return 0;

	m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny( m_ammoTypes[m_ammoType].c_str() ));
	VERIFY((u32)iAmmoElapsed == m_magazine.size());


	if (m_DefaultCartridge.m_LocalAmmoType != m_ammoType)
		m_DefaultCartridge.Load(m_ammoTypes[m_ammoType].c_str(), m_ammoType);
	CCartridge l_cartridge = m_DefaultCartridge;
	while(cnt)
	{
		if (!unlimited_ammo())
		{
			if (!m_pCurrentAmmo->Get(l_cartridge)) break;
		}
		--cnt;
		++iAmmoElapsed;
		l_cartridge.m_LocalAmmoType = m_ammoType;
		m_magazine.push_back(l_cartridge);
//		m_fCurrentCartirdgeDisp = l_cartridge.m_kDisp;
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	//�������� ������� ��������, ���� ��� ������
	if(m_pCurrentAmmo && !m_pCurrentAmmo->m_boxCurr && OnServer()) 
		m_pCurrentAmmo->SetDropManual(TRUE);

	GiveAmmoFromMagToChamber();

	return cnt;
}

void	CWeaponAutomaticShotgun::net_Export	(NET_Packet& P)
{
	inherited::net_Export(P);	
	P.w_u8(u8(m_magazine.size()));	
	for (u32 i=0; i<m_magazine.size(); i++)
	{
		CCartridge& l_cartridge = *(m_magazine.begin()+i);
		P.w_u8(l_cartridge.m_LocalAmmoType);
	}
}

void	CWeaponAutomaticShotgun::net_Import	(NET_Packet& P)
{
	inherited::net_Import(P);	
	u8 AmmoCount = P.r_u8();
	for (u32 i=0; i<AmmoCount; i++)
	{
		u8 LocalAmmoType = P.r_u8();
		if (i>=m_magazine.size()) continue;
		CCartridge& l_cartridge = *(m_magazine.begin()+i);
		if (LocalAmmoType == l_cartridge.m_LocalAmmoType) continue;
#ifdef DEBUG
		Msg("! %s reload to %s", *l_cartridge.m_ammoSect, m_ammoTypes[LocalAmmoType].c_str());
#endif
		l_cartridge.Load( m_ammoTypes[LocalAmmoType].c_str(), LocalAmmoType );
	}
}
