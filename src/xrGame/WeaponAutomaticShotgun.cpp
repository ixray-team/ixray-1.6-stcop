#include "stdafx.h"
#include "script_game_object.h"
#include "WeaponAutomaticShotgun.h"
#include "entity.h"
#include "../xrEngine/xr_level_controller.h"
#include "Inventory.h"
#include "Level.h"
#include "Actor.h"
#include "script_game_object.h"

CWeaponAutomaticShotgun::CWeaponAutomaticShotgun()
{
	m_eSoundClose			= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	m_eSoundAddCartridge	= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
}

CWeaponAutomaticShotgun::~CWeaponAutomaticShotgun()
{
}

void CWeaponAutomaticShotgun::Load(LPCSTR section)
{
	inherited::Load(section);

	if(pSettings->line_exist(section, "tri_state_reload")){
		m_bTriStateReload = !!pSettings->r_bool(section, "tri_state_reload");
	};
	if(m_bTriStateReload)
	{
		m_sounds.LoadSound(section, "snd_open_weapon", "sndOpen", false, m_eSoundOpen);

		m_sounds.LoadSound(section, "snd_add_cartridge", "sndAddCartridge", false, m_eSoundAddCartridge);

		m_sounds.LoadSound(section, "snd_close_weapon", "sndClose", false, m_eSoundClose);
	};

}

void CWeaponAutomaticShotgun::OnAnimationEnd(u32 state) 
{
	if (!m_bTriStateReload || state != eReload)
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

void CWeaponAutomaticShotgun::Reload() 
{
	if(m_bTriStateReload)
		TriStateReload();
	else
		inherited::Reload();
}

void CWeaponAutomaticShotgun::TriStateReload()
{
	if(m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
		return;

	CWeapon::Reload();
	m_sub_state = eSubstateReloadBegin;
	SwitchState	(eReload);
}

void CWeaponAutomaticShotgun::OnStateSwitch	(u32 S)
{
	if(!m_bTriStateReload || S != eReload)
	{
		bStopReloadSignal = false;
		inherited::OnStateSwitch(S);
		return;
	}

	CWeapon::OnStateSwitch(S);

	if( m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1) )
	{
			switch2_EndReload();
			m_sub_state = eSubstateReloadEnd;
			return;
	};

	switch (m_sub_state)
	{
		case eSubstateReloadBegin:
			if(HaveCartridgeInInventory(1))
			switch2_StartReload	();
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

void CWeaponAutomaticShotgun::switch2_StartReload()
{
	PlaySound			("sndOpen",get_LastFP());
	PlayAnimOpenWeapon	();
	SetPending			(TRUE);
}

void CWeaponAutomaticShotgun::switch2_AddCartgidge	()
{
	PlaySound	("sndAddCartridge",get_LastFP());
	PlayAnimAddOneCartridgeWeapon();
	SetPending			(TRUE);
}

void CWeaponAutomaticShotgun::switch2_EndReload	()
{
	SetPending			(FALSE);
	PlaySound			("sndClose",get_LastFP());
	PlayAnimCloseWeapon	();
}

void CWeaponAutomaticShotgun::PlayAnimOpenWeapon()
{
	VERIFY(GetState()==eReload);
	PlayHUDMotion("anm_open",FALSE,this,GetState());
}
void CWeaponAutomaticShotgun::PlayAnimAddOneCartridgeWeapon()
{
	VERIFY(GetState()==eReload);
	PlayHUDMotion("anm_add_cartridge",FALSE,this,GetState());
}
void CWeaponAutomaticShotgun::PlayAnimCloseWeapon()
{
	VERIFY(GetState()==eReload);

	PlayHUDMotion("anm_close",FALSE,this,GetState());
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
