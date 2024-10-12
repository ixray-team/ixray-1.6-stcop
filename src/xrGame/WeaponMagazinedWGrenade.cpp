#include "stdafx.h"
#include "weaponmagazinedwgrenade.h"
#include "entity.h"
#include "GrenadeLauncher.h"
#include "xrServer_Objects_ALife_Items.h"
#include "ExplosiveRocket.h"
#include "Actor.h"
#include "../xrEngine/xr_level_controller.h"
#include "Level.h"
#include "object_broker.h"
#include "game_base_space.h"
#include "../xrPhysics/MathUtils.h"
#include "player_hud.h"
#include "Actor_Flags.h"
#include "Inventory.h"
#include "inventoryOwner.h"

#ifdef DEBUG
#	include "phdebug.h"
#endif

CWeaponMagazinedWGrenade::CWeaponMagazinedWGrenade(ESoundTypes eSoundType) : CWeaponMagazined(eSoundType)
{
	m_ammoType2 = 0;
    m_bGrenadeMode = false;
}

CWeaponMagazinedWGrenade::~CWeaponMagazinedWGrenade()
{
}

void CWeaponMagazinedWGrenade::Load	(LPCSTR section)
{
	inherited::Load			(section);
	CRocketLauncher::Load	(section);
	
	
	//// Sounds
	m_sounds.LoadSound(section,"snd_shoot_grenade"	, "sndShotG"		, false, m_eSoundShot);
	m_sounds.LoadSound(section,"snd_reload_grenade"	, "sndReloadG"	, true, m_eSoundReload);
	m_sounds.LoadSound(section,"snd_switch"			, "sndSwitch"		, true, m_eSoundReload);
	

	m_sFlameParticles2 = pSettings->r_string(section, "grenade_flame_particles");

	
	if(m_eGrenadeLauncherStatus == ALife::eAddonPermanent)
	{
		CRocketLauncher::m_fLaunchSpeed = pSettings->r_float(section, "grenade_vel");
	}

	// load ammo classes SECOND (grenade_class)
	m_ammoTypes2.clear	(); 
	LPCSTR				S = pSettings->r_string(section,"grenade_class");
	if (S && S[0]) 
	{
		string128		_ammoItem;
		int				count		= _GetItemCount	(S);
		for (int it=0; it<count; ++it)	
		{
			_GetItem				(S,it,_ammoItem);
			m_ammoTypes2.push_back	(_ammoItem);
		}
	}

	iMagazineSize2 = iMagazineSize;
}

void CWeaponMagazinedWGrenade::net_Destroy()
{
	inherited::net_Destroy();
}


BOOL CWeaponMagazinedWGrenade::net_Spawn(CSE_Abstract* DC) 
{
	CSE_ALifeItemWeapon* const weapon		= smart_cast<CSE_ALifeItemWeapon*>(DC);
	R_ASSERT								(weapon);
	if ( IsGameTypeSingle() )
	{
		inherited::net_Spawn_install_upgrades	(weapon->m_upgrades);
	}

	BOOL l_res = inherited::net_Spawn(DC);
	 
	UpdateGrenadeVisibility(!!iAmmoElapsed);
	SetPending			(FALSE);

	iAmmoElapsed2	= weapon->a_elapsed_grenades.grenades_count;
	m_ammoType2		= weapon->a_elapsed_grenades.grenades_type;

	m_DefaultCartridge2.Load(m_ammoTypes2[m_ammoType2].c_str(), m_ammoType2);

	if (!IsGameTypeSingle())
	{
		if (!m_bGrenadeMode && IsGrenadeLauncherAttached() && !getRocketCount() && iAmmoElapsed2)
		{
			m_magazine2.push_back(m_DefaultCartridge2);

			shared_str grenade_name = m_DefaultCartridge2.m_ammoSect;
			shared_str fake_grenade_name = pSettings->r_string(grenade_name, "fake_grenade_name");

			CRocketLauncher::SpawnRocket(*fake_grenade_name, this);
		}
	}else
	{
		xr_vector<CCartridge>* pM = nullptr;
		bool b_if_grenade_mode	= (m_bGrenadeMode && iAmmoElapsed && !getRocketCount());
		if(b_if_grenade_mode)
			pM = &m_magazine;
			
		bool b_if_simple_mode	= (!m_bGrenadeMode && m_magazine2.size() && !getRocketCount());
		if(b_if_simple_mode)
			pM = &m_magazine2;

		if(b_if_grenade_mode || b_if_simple_mode) 
		{
			shared_str fake_grenade_name = pSettings->r_string(pM->back().m_ammoSect, "fake_grenade_name");
			
			CRocketLauncher::SpawnRocket(*fake_grenade_name, this);
		}
	}
	return l_res;
}

void CWeaponMagazinedWGrenade::switch2_Reload()
{
	VERIFY(GetState()==eReload);
	if(m_bGrenadeMode) 
	{
		PlaySound("sndReloadG", get_LastFP2());

		PlayHUDMotion("anm_reload_g", FALSE, this, GetState());
		SetPending			(TRUE);
	}
	else 
	     inherited::switch2_Reload();
}

bool CWeaponMagazinedWGrenade::SwitchMode() 
{
	if (!IsGrenadeLauncherAttached())
		return false;

	bool bUsefulStateToSwitch = (!IsPending() && !IsZoomed() && (GetState() == eIdle || GetState() == eMisfire));

	if(!bUsefulStateToSwitch)
		return false;

	SwitchState(eSwitch);

	m_BriefInfo_CalcFrame = 0;

	return true;
}

void CWeaponMagazinedWGrenade::PerformSwitchGL()
{
	m_bGrenadeMode = !m_bGrenadeMode;

	iMagazineSize = m_bGrenadeMode ? 1 : iMagazineSize2;

	m_ammoTypes.swap(m_ammoTypes2);

	swap(m_ammoType, m_ammoType2);

	swap(m_DefaultCartridge, m_DefaultCartridge2);

	m_magazine.swap(m_magazine2);

	iAmmoElapsed = (int)m_magazine.size();
	iAmmoElapsed2 = (int)m_magazine2.size();

	m_BriefInfo_CalcFrame = 0;
}

bool CWeaponMagazinedWGrenade::Action(u16 cmd, u32 flags) 
{
	if (inherited::Action(cmd, flags))
		return true;
	
	switch(cmd) 
	{
		case kWPN_FUNC:
		{
            if (flags&CMD_START)
				return SwitchMode();
		}
	}
	return false;
}

void CWeaponMagazinedWGrenade::FireStart()
{
	if (!m_bGrenadeMode)
	{
		inherited::FireStart();
		return;
	}

	if (GetState() != eIdle)
		return;

	if (IsPending())
		return;

	if (!iAmmoElapsed)
	{
		switch2_Empty();
		return;
	}

	CWeapon::FireStart();
	SwitchState(eFire);
}

void CWeaponMagazinedWGrenade::FireEnd()
{
	if (m_bGrenadeMode)
		CWeapon::FireEnd();
	else
		inherited::FireEnd();
}

void CWeaponMagazinedWGrenade::state_Fire(float dt) 
{
	VERIFY(fOneShotTime>0.f);

	//режим стрельбы подствольника
	if (m_bGrenadeMode)
	{
		if (!iAmmoElapsed)
			return;

		Fvector	p1, d; 
		p1.set(get_LastFP2());
		d.set(get_LastFD());
		CEntity* E = smart_cast<CEntity*>(H_Parent());

		if (E)
		{
			CInventoryOwner* io	= smart_cast<CInventoryOwner*>(H_Parent());
			if(nullptr == io->inventory().ActiveItem())
			{
				Msg("current_state %d", GetState());
				Msg("next_state %d", GetNextState());
				Msg("item_sect %s", cNameSect().c_str());
				Msg("H_Parent %s", H_Parent()->cNameSect().c_str());
			}
			E->g_fireParams(this, p1,d);
		}

		if (IsGameTypeSingle())
			p1.set(get_LastFP2());
		
		Fmatrix launch_matrix;
		launch_matrix.identity();
		launch_matrix.k.set(d);
		Fvector::generate_orthonormal_basis(launch_matrix.k, launch_matrix.j, launch_matrix.i);

		launch_matrix.c.set(p1);

		if (IsGameTypeSingle() && IsZoomed() && smart_cast<CActor*>(H_Parent()))
		{
			H_Parent()->setEnabled(FALSE);
			setEnabled(FALSE);

			collide::rq_result RQ;
			BOOL HasPick = Level().ObjectSpace.RayPick(p1, d, 300.0f, collide::rqtStatic, RQ, this);

			setEnabled(TRUE);
			H_Parent()->setEnabled(TRUE);

			if (HasPick)
			{
				Fvector	Transference;
				Transference.mul(d, RQ.range);
				Fvector	res[2];
				u8 canfire0 = TransferenceAndThrowVelToThrowDir(Transference, CRocketLauncher::m_fLaunchSpeed, EffectiveGravity(), res);
				
				if (canfire0 != 0)
					d = res[0];
				else
					LaunchGrenade_Correct(&d);
			}
		};
		
		d.normalize();
		d.mul(CRocketLauncher::m_fLaunchSpeed);
		VERIFY2(_valid(launch_matrix),"CWeaponMagazinedWGrenade::SwitchState. Invalid launch_matrix!");
		CRocketLauncher::LaunchRocket(launch_matrix, d, zero_vel);

		CExplosiveRocket* pGrenade = smart_cast<CExplosiveRocket*>(getCurrentRocket());
		VERIFY(pGrenade);
		pGrenade->SetInitiator(H_Parent()->ID());

		if (Local() && OnServer())
		{
			VERIFY(m_magazine.size());
			m_magazine.pop_back();
			--iAmmoElapsed;
			VERIFY((u32)iAmmoElapsed == m_magazine.size());

			NET_Packet P;
			u_EventGen(P,GE_LAUNCH_ROCKET,ID());
			P.w_u16(getCurrentRocket()->ID());
			u_EventSend(P);
		};
	} 
	//режим стрельбы очередями
	else 
		inherited::state_Fire(dt);
}

void CWeaponMagazinedWGrenade::LaunchGrenade_Correct(Fvector3* v)
{
	Fvector3 camdir = Device.vCameraDirection;

	camdir.y = 0.0f;
	camdir.normalize();

	camdir.y = 1.0f;
	camdir.normalize();

	*v = camdir;
}

void CWeaponMagazinedWGrenade::OnEvent(NET_Packet& P, u16 type) 
{
	inherited::OnEvent(P,type);
	u16 id;
	switch (type) 
	{
		case GE_OWNERSHIP_TAKE: 
			{
				P.r_u16(id);
				CRocketLauncher::AttachRocket(id, this);
			}
			break;
		case GE_OWNERSHIP_REJECT :
		case GE_LAUNCH_ROCKET : 
			{
				bool bLaunch	= (type==GE_LAUNCH_ROCKET);
				P.r_u16			(id);
				CRocketLauncher::DetachRocket(id, bLaunch);
				if(bLaunch)
				{
					PlayAnimShoot		();
					PlaySound			("sndShotG", get_LastFP2(), true);
					AddShotEffector		();
					StartFlameParticles2();
				}
				break;
			}
	}
}

void CWeaponMagazinedWGrenade::ReloadMagazine() 
{
	auto last_bMisfire = bMisfire;
	inherited::ReloadMagazine();
	
	//перезарядка подствольного гранатомета
	if (m_bGrenadeMode)
	{
		bMisfire = last_bMisfire;
		if (!getRocketCount())
		{
			shared_str fake_grenade_name = pSettings->r_string(m_ammoTypes[m_ammoType].c_str(), "fake_grenade_name");
			CRocketLauncher::SpawnRocket(*fake_grenade_name, this);
		}
	}
}

void CWeaponMagazinedWGrenade::UnloadMagazine(bool spawn_ammo)
{
	inherited::UnloadMagazine(spawn_ammo);

	if (m_bGrenadeMode)
	{
		if (getRocketCount())
			dropCurrentRocket();
	}
}

void CWeaponMagazinedWGrenade::OnStateSwitch(u32 S) 
{
	switch (S)
	{
		case eSwitch:
			switch2_SwitchMode();
		break;
	}
	
	inherited::OnStateSwitch(S);
	UpdateGrenadeVisibility(!!iAmmoElapsed || S == eReload);
}

void CWeaponMagazinedWGrenade::switch2_SwitchMode()
{
	SetPending(TRUE);
	PerformSwitchGL();
	PlaySound("sndSwitch", get_LastFP());
	PlayAnimModeSwitch();
}

void CWeaponMagazinedWGrenade::OnAnimationEnd(u32 state)
{
	switch (state)
	{
		case eSwitch:
			SwitchState(eIdle);
		break;
	}
	inherited::OnAnimationEnd(state);
}

void CWeaponMagazinedWGrenade::OnH_B_Independent(bool just_before_destroy)
{
	inherited::OnH_B_Independent(just_before_destroy);

	SetPending(FALSE);
	if (m_bGrenadeMode)
	{
		SetState		(eIdle);
		SetPending		(FALSE);
	}
}

bool CWeaponMagazinedWGrenade::CanAttach(PIItem pIItem)
{
	CGrenadeLauncher* pGrenadeLauncher = smart_cast<CGrenadeLauncher*>(pIItem);
	
	if(pGrenadeLauncher &&
	   ALife::eAddonAttachable == m_eGrenadeLauncherStatus &&
	   0 == (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) &&
	   !xr_strcmp(*m_sGrenadeLauncherName, pIItem->object().cNameSect()))
       return true;
	else
		return inherited::CanAttach(pIItem);
}

bool CWeaponMagazinedWGrenade::CanDetach(LPCSTR item_section_name)
{
	if(ALife::eAddonAttachable == m_eGrenadeLauncherStatus &&
	   0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) &&
	   !xr_strcmp(*m_sGrenadeLauncherName, item_section_name))
	   return true;
	else
	   return inherited::CanDetach(item_section_name);
}

bool CWeaponMagazinedWGrenade::Attach(PIItem pIItem, bool b_send_event)
{
	CGrenadeLauncher* pGrenadeLauncher = smart_cast<CGrenadeLauncher*>(pIItem);
	
	if(pGrenadeLauncher &&
	   ALife::eAddonAttachable == m_eGrenadeLauncherStatus &&
	   0 == (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) &&
	   !xr_strcmp(*m_sGrenadeLauncherName, pIItem->object().cNameSect()))
	{
		m_flagsAddOnState |= CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher;

		CRocketLauncher::m_fLaunchSpeed = pGrenadeLauncher->GetGrenadeVel();

 		//уничтожить подствольник из инвентаря
		if(b_send_event)
		{
			if (OnServer()) 
				pIItem->object().DestroyObject	();
		}
		InitAddons				();
		UpdateAddonsVisibility	();

		if(GetState()==eIdle)
			PlayAnimIdle		();

		return					true;
	}
	else
        return inherited::Attach(pIItem, b_send_event);
}

bool CWeaponMagazinedWGrenade::Detach(LPCSTR item_section_name, bool b_spawn_item)
{
	if (ALife::eAddonAttachable == m_eGrenadeLauncherStatus &&
	   0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) &&
	   !xr_strcmp(*m_sGrenadeLauncherName, item_section_name))
	{
		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher;

		// Now we need to unload GL's magazine
		if (!m_bGrenadeMode)
			PerformSwitchGL();

		UnloadMagazine();
		PerformSwitchGL();

		UpdateAddonsVisibility();

		if(GetState()==eIdle)
			PlayAnimIdle		();

		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}
	else
		return inherited::Detach(item_section_name, b_spawn_item);
}




void CWeaponMagazinedWGrenade::InitAddons()
{	
	inherited::InitAddons();

	if(GrenadeLauncherAttachable())
	{
		if(IsGrenadeLauncherAttached())
		{
			CRocketLauncher::m_fLaunchSpeed = pSettings->r_float(*m_sGrenadeLauncherName,"grenade_vel");
		}
	}
}

bool	CWeaponMagazinedWGrenade::UseScopeTexture()
{
	if (IsGrenadeLauncherAttached() && m_bGrenadeMode) return false;
	
	return true;
};

float	CWeaponMagazinedWGrenade::CurrentZoomFactor	()
{
	if (IsGrenadeLauncherAttached() && m_bGrenadeMode) return m_zoom_params.m_fIronSightZoomFactor;
	return inherited::CurrentZoomFactor();
}

//виртуальные функции для проигрывания анимации HUD
void CWeaponMagazinedWGrenade::PlayAnimShow()
{
	VERIFY(GetState()==eShowing);
	if(IsGrenadeLauncherAttached())
	{
		if(!m_bGrenadeMode)
			PlayHUDMotion("anm_show_w_gl", FALSE, this, GetState());
		else
			PlayHUDMotion("anm_show_g", FALSE, this, GetState());
	}	
	else
		PlayHUDMotion("anm_show", FALSE, this, GetState());
}

void CWeaponMagazinedWGrenade::PlayAnimHide()
{
	VERIFY(GetState()==eHiding);
	
	if(IsGrenadeLauncherAttached())
		if(!m_bGrenadeMode)
			PlayHUDMotion("anm_hide_w_gl", TRUE, this, GetState());
		else
			PlayHUDMotion("anm_hide_g", TRUE, this, GetState());

	else
		PlayHUDMotion("anm_hide", TRUE, this, GetState());
}

void CWeaponMagazinedWGrenade::PlayAnimReload()
{
	VERIFY(GetState()==eReload);

	if (IsGrenadeLauncherAttached())
	{
		if (HudAnimationExist("anm_reload_misfire_w_gl") && IsMisfire())
		{
			PlayHUDMotion("anm_reload_misfire_w_gl", TRUE, this, GetState());
			bMisfireReload = true;
		}
		else if (HudAnimationExist("anm_reload_empty_w_gl") && iAmmoElapsed == 0)
			PlayHUDMotion("anm_reload_empty_w_gl", TRUE, this, GetState());
		else
			PlayHUDMotion("anm_reload_w_gl", TRUE, this, GetState());
	}
	else
		inherited::PlayAnimReload();
}

void CWeaponMagazinedWGrenade::PlayAnimIdle()
{
	if (TryPlayAnimIdle())
		return;

	if(IsGrenadeLauncherAttached())
	{
		if (IsZoomed())
			PlayAnimAim();
		else
		{
			if(m_bGrenadeMode)
				PlayHUDMotion("anm_idle_g", TRUE, nullptr, eIdle);
			else
				PlayHUDMotion("anm_idle_w_gl", TRUE, nullptr, eIdle);
		}
	}
	else
		inherited::PlayAnimIdle();
}

void CWeaponMagazinedWGrenade::PlayAnimAim()
{
	if (IsGrenadeLauncherAttached())
	{
		if (m_bGrenadeMode)
			PlayHUDMotion("anm_idle_g_aim", TRUE, nullptr, eIdle);
		else
			PlayHUDMotion("anm_idle_w_gl_aim", TRUE, nullptr, eIdle);
	}
	else
		inherited::PlayAnimAim();
}

void CWeaponMagazinedWGrenade::PlayAnimIdleMoving()
{
	if (IsGrenadeLauncherAttached())
	{
		if (m_bGrenadeMode)
			PlayHUDMotion("anm_idle_moving_g", TRUE, nullptr, eIdle);
		else
			PlayHUDMotion("anm_idle_moving_w_gl", TRUE, nullptr, eIdle);
	}
	else
		inherited::PlayAnimIdleMoving();
}

void CWeaponMagazinedWGrenade::PlayAnimIdleSprint()
{
	if (IsGrenadeLauncherAttached())
	{
		if (m_bGrenadeMode)
			PlayHUDMotion("anm_idle_sprint_g", TRUE, nullptr, eIdle);
		else
			PlayHUDMotion("anm_idle_sprint_w_gl", TRUE, nullptr, eIdle);
	}
	else
		inherited::PlayAnimIdleSprint();
}

void CWeaponMagazinedWGrenade::PlayAnimShoot()
{
	if(m_bGrenadeMode)
	{
		PlayHUDMotion("anm_shots_g" ,FALSE, this, eFire);
	}
	else
	{
		VERIFY(GetState()==eFire);
		if(IsGrenadeLauncherAttached())
			PlayHUDMotion("anm_shots_w_gl" ,FALSE, this, GetState());
		else
			inherited::PlayAnimShoot();
	}
}

void CWeaponMagazinedWGrenade::PlayAnimModeSwitch()
{
	if(m_bGrenadeMode)
		PlayHUDMotion("anm_switch_g", TRUE, this, eSwitch);
	else 
		PlayHUDMotion("anm_switch", TRUE, this, eSwitch);
}

void CWeaponMagazinedWGrenade::PlayAnimBore()
{
	if(IsGrenadeLauncherAttached())
	{
		if(m_bGrenadeMode)
			PlayHUDMotion	("anm_bore_g", TRUE, this, GetState());
		else
			PlayHUDMotion	("anm_bore_w_gl", TRUE, this, GetState());
	}else
		inherited::PlayAnimBore();
}

void CWeaponMagazinedWGrenade::UpdateSounds	()
{
	if (Device.dwFrame == dwUpdateSounds_Frame)
		return;

	inherited::UpdateSounds			();

	Fvector P						= get_LastFP();
	if (Device.dwFrame % 3 == 0)
		m_sounds.SetPosition("sndShotG", P);
	else if (Device.dwFrame % 3 == 1)
		m_sounds.SetPosition("sndReloadG", P);
	else if (Device.dwFrame % 3 == 2)
		m_sounds.SetPosition("sndSwitch", P);
}

void CWeaponMagazinedWGrenade::UpdateGrenadeVisibility(bool visibility)
{
	if(!GetHUDmode())							return;
	HudItemData()->set_bone_visible				("grenade", visibility, TRUE);
}

void CWeaponMagazinedWGrenade::save(NET_Packet &output_packet)
{
	inherited::save								(output_packet);
	save_data									(m_bGrenadeMode, output_packet);
	save_data									((u32) m_magazine2.size(), output_packet);

}

void CWeaponMagazinedWGrenade::load(IReader &input_packet)
{
	inherited::load				(input_packet);
	bool b;
	load_data					(b, input_packet);
	if(b!=m_bGrenadeMode)		
		PerformSwitchGL();

	u32 sz;
	load_data					(sz, input_packet);

	CCartridge					l_cartridge; 
	l_cartridge.Load			(m_ammoTypes2[m_ammoType2].c_str(), m_ammoType2);

	while (sz > (u32) m_magazine2.size())
		m_magazine2.push_back(l_cartridge);
}

void CWeaponMagazinedWGrenade::net_Export	(NET_Packet& P)
{
	P.w_u8						(m_bGrenadeMode ? 1 : 0);

	inherited::net_Export		(P);
}

void CWeaponMagazinedWGrenade::net_Import	(NET_Packet& P)
{
	bool NewMode				= FALSE;
	NewMode						= !!P.r_u8();	
	if (NewMode != m_bGrenadeMode)
		PerformSwitchGL();

	inherited::net_Import		(P);
}

float CWeaponMagazinedWGrenade::Weight() const {
	float res = inherited::Weight();
	res += GetMagazineWeight(m_magazine2);

	return res;
}

bool CWeaponMagazinedWGrenade::IsNecessaryItem	    (const shared_str& item_sect)
{
	return (	std::find(m_ammoTypes.begin(), m_ammoTypes.end(), item_sect) != m_ammoTypes.end() ||
				std::find(m_ammoTypes2.begin(), m_ammoTypes2.end(), item_sect) != m_ammoTypes2.end() 
			);
}

u8 CWeaponMagazinedWGrenade::GetCurrentHudOffsetIdx()
{
	bool b_aiming		= 	((IsZoomed() && m_zoom_params.m_fZoomRotationFactor<=1.f) ||
							(!IsZoomed() && m_zoom_params.m_fZoomRotationFactor>0.f));
	
	if(!b_aiming)
		return		0;
	else
	if(m_bGrenadeMode)
		return		2;
	else
		return		1;
}

bool CWeaponMagazinedWGrenade::install_upgrade_ammo_class	( LPCSTR section, bool test )
{
	LPCSTR str;

	bool result = process_if_exists( section, "ammo_mag_size", &CInifile::r_s32, iMagazineSize2, test );
	iMagazineSize		= m_bGrenadeMode?1:iMagazineSize2;

	//	ammo_class = ammo_5.45x39_fmj, ammo_5.45x39_ap  // name of the ltx-section of used ammo
	bool result2 = process_if_exists_set( section, "ammo_class", &CInifile::r_string, str, test );
	if ( result2 && !test ) 
	{
		xr_vector<shared_str>& ammo_types	= m_bGrenadeMode ? m_ammoTypes2 : m_ammoTypes;
		ammo_types.clear					(); 
		for ( int i = 0, count = _GetItemCount( str ); i < count; ++i )	
		{
			string128						ammo_item;
			_GetItem						( str, i, ammo_item );
			ammo_types.push_back			( ammo_item );
		}

		m_ammoType  = 0;
		m_ammoType2 = 0;
	}
	result |= result2;

	return result2;
}

bool CWeaponMagazinedWGrenade::install_upgrade_impl( LPCSTR section, bool test )
{
	LPCSTR str;
	bool result = inherited::install_upgrade_impl( section, test );
	
	//	grenade_class = ammo_vog-25, ammo_vog-25p          // name of the ltx-section of used grenades
	bool result2 = process_if_exists_set( section, "grenade_class", &CInifile::r_string, str, test );
	if ( result2 && !test )
	{
		xr_vector<shared_str>& ammo_types	= !m_bGrenadeMode ? m_ammoTypes2 : m_ammoTypes;
		ammo_types.clear					(); 
		for ( int i = 0, count = _GetItemCount( str ); i < count; ++i )	
		{
			string128						ammo_item;
			_GetItem						( str, i, ammo_item );
			ammo_types.push_back			( ammo_item );
		}

		m_ammoType  = 0;
		m_ammoType2 = 0;
	}
	result |= result2;

	result |= process_if_exists( section, "launch_speed", &CInifile::r_float, m_fLaunchSpeed, test );

	result2 = process_if_exists_set( section, "snd_shoot_grenade", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_shoot_grenade", "sndShotG", false, m_eSoundShot );	}
	result |= result2;

	result2 = process_if_exists_set( section, "snd_reload_grenade", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_reload_grenade", "sndReloadG", true, m_eSoundReload );	}
	result |= result2;

	result2 = process_if_exists_set( section, "snd_switch", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_switch", "sndSwitch", true, m_eSoundReload );	}
	result |= result2;

	return result;
}

void CWeaponMagazinedWGrenade::net_Spawn_install_upgrades	( Upgrades_type saved_upgrades )
{
	// do not delete this
	// this is intended behaviour
}


#include "../xrEngine/string_table.h"
bool CWeaponMagazinedWGrenade::GetBriefInfo( II_BriefInfo& info )
{
	VERIFY(m_pInventory);
/*
	if(!inherited::GetBriefInfo(info))
		return false;
*/
	string32 int_str;
	int	ae = GetAmmoElapsed();
	xr_sprintf(int_str, "%d", ae);
	info.cur_ammo._set(int_str);
	if(HasFireModes())
	{
		if(m_iQueueSize == WEAPON_ININITE_QUEUE)
			info.fire_mode._set("A");
		else
		{
			xr_sprintf(int_str, "%d", m_iQueueSize);
			info.fire_mode._set(int_str);
		}
	}	
	if(m_pInventory->ModifyFrame() <= m_BriefInfo_CalcFrame)
		return false;

	GetSuitableAmmoTotal();

	u32 at_size = m_bGrenadeMode ? (u32)m_ammoTypes2.size() : (u32)m_ammoTypes.size();
	if(unlimited_ammo() || at_size == 0)
	{
		info.fmj_ammo._set("∞");
		info.ap_ammo._set("∞");
		info.third_ammo._set("∞");
	}
	else
    {
		//Alundaio: Added third ammo type and cleanup
		info.fmj_ammo._set("");
		info.ap_ammo._set("");
		info.third_ammo._set("");

		u8 ammo_type = m_bGrenadeMode ? m_ammoType2 : m_ammoType;
		xr_sprintf(int_str, "%d", m_bGrenadeMode ? GetAmmoCount2(ammo_type) : GetAmmoCount(ammo_type));

		if (m_ammoType == 0)
			info.fmj_ammo._set(int_str);
		else if (m_ammoType == 1)
			info.ap_ammo._set(int_str);
		else
			info.third_ammo._set(int_str);
		//-Alundaio
    }

	if(ae != 0 && m_magazine.size() != 0)
	{
		LPCSTR ammo_type = m_ammoTypes[m_magazine.back().m_LocalAmmoType].c_str();
		info.name._set(g_pStringTable->translate(pSettings->r_string(ammo_type, "inv_name_short")));
		info.icon._set(ammo_type);
	}
	else
	{
		LPCSTR ammo_type = m_ammoTypes[m_ammoType].c_str();
		info.name._set(g_pStringTable->translate(pSettings->r_string(ammo_type, "inv_name_short")));
		info.icon._set(ammo_type);
	}

	if(!IsGrenadeLauncherAttached())
	{
		info.grenade = "";
		return false;
	}

	int total2 = m_bGrenadeMode ? GetAmmoCount(0) : GetAmmoCount2(0);
	if(unlimited_ammo())
		xr_sprintf(int_str, "∞");
	else
	{
		if(total2)
			xr_sprintf(int_str, "%d", total2);
		else
			xr_sprintf(int_str, "X");
	}
	info.grenade	= int_str;
	
	return true;
}

int CWeaponMagazinedWGrenade::GetAmmoCount2( u8 ammo2_type ) const
{
	VERIFY( m_pInventory );
	R_ASSERT( ammo2_type < m_ammoTypes2.size() );

	return GetAmmoCount_forType( m_ammoTypes2[ammo2_type] );
}



