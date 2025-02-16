#include "stdafx.h"
#include "weaponrpg7.h"
#include "xrserver_objects_alife_items.h"
#include "explosiverocket.h"
#include "entity.h"
#include "level.h"
#include "player_hud.h"
#include "hudmanager.h"

CWeaponRPG7::CWeaponRPG7() : CWeaponCustomPistol("RPG7") 
{
}

CWeaponRPG7::~CWeaponRPG7() 
{
}

void CWeaponRPG7::Load	(LPCSTR section)
{
	inherited::Load						(section);
	CRocketLauncher::Load				(section);

	m_zoom_params.m_fScopeZoomFactor	= pSettings->r_float	(section,"max_zoom_factor");

	m_sRocketSection					= pSettings->r_string	(section,"rocket_class");
}

bool CWeaponRPG7::install_upgrade_impl(LPCSTR section, bool test)
{
	bool result = inherited::install_upgrade_impl( section, test );

	result |= process_if_exists( section, "launch_speed", &CInifile::r_float, m_fLaunchSpeed, test );

	LPCSTR str = nullptr;
	bool result2;
	result2 = process_if_exists_set( section, "rocket_class", &CInifile::r_string, str, test );
	if (result2 && !test)
	{
		m_sRocketSection = str;
	}
	result |= result2;
	return result;
}

bool CWeaponRPG7::AllowBore()
{
	return inherited::AllowBore() && 0!=iAmmoElapsed;
}

void CWeaponRPG7::FireTrace(const Fvector& P, const Fvector& D)
{
	inherited::FireTrace	(P, D);
	if (!IsMisfire())
	{
		LaunchGrenade(P, D);
	}
	UpdateMissileVisibility	();
}

void CWeaponRPG7::on_a_hud_attach()
{
	inherited::on_a_hud_attach		();
	UpdateMissileVisibility			();
}

void CWeaponRPG7::UpdateMissileVisibility()
{
	bool vis_hud,vis_weap;
	vis_hud		= (!!iAmmoElapsed || GetState()==eReload);
	vis_weap	= !!iAmmoElapsed;

	if(GetHUDmode())
	{
		HudItemData()->set_bone_visible("grenade",vis_hud,TRUE);
	}

	IKinematics* pWeaponVisual	= smart_cast<IKinematics*>(Visual()); 
	VERIFY						(pWeaponVisual);
	pWeaponVisual->LL_SetBoneVisible(pWeaponVisual->LL_BoneID("grenade"), vis_weap, TRUE);
}

BOOL CWeaponRPG7::net_Spawn(CSE_Abstract* DC) 
{
	BOOL l_res = inherited::net_Spawn(DC);

	UpdateMissileVisibility();
	if(iAmmoElapsed && !getCurrentRocket())
		CRocketLauncher::SpawnRocket(*m_sRocketSection, this);

	return l_res;
}

void CWeaponRPG7::OnStateSwitch(u32 S) 
{
	inherited::OnStateSwitch(S);
	UpdateMissileVisibility();
}

void CWeaponRPG7::UnloadMagazine(bool spawn_ammo)
{
	inherited::UnloadMagazine	(spawn_ammo);
	UpdateMissileVisibility		();
}

void CWeaponRPG7::ReloadMagazine() 
{
	inherited::ReloadMagazine();

	if(iAmmoElapsed && !getRocketCount()) 
		CRocketLauncher::SpawnRocket(*m_sRocketSection, this);
}

#include "inventory.h"
#include "inventoryOwner.h"
void CWeaponRPG7::switch2_Fire()
{
	m_iShotNum			= 0;
	m_bFireSingleShot	= DelayedShotIsAllowed();
	bWorking			= false;	
}

void CWeaponRPG7::LaunchGrenade(const Fvector& p1, const Fvector& d1)
{
	if (getRocketCount()) 
	{
		Fvector p, d;
		p = p1;
		d = d1;

		Fmatrix								launch_matrix;
		launch_matrix.identity				();
		launch_matrix.k.set					(d);
		Fvector::generate_orthonormal_basis(launch_matrix.k,
											launch_matrix.j, launch_matrix.i);
		launch_matrix.c.set					(p);

		d.normalize							();
		d.mul								(m_fLaunchSpeed);

		CRocketLauncher::LaunchRocket		(launch_matrix, d, zero_vel);

		CExplosiveRocket* pGrenade			= smart_cast<CExplosiveRocket*>(getCurrentRocket());
		VERIFY								(pGrenade);
		pGrenade->SetInitiator				(H_Parent()->ID());

		if (OnServer())
		{
			NET_Packet						P;
			u_EventGen						(P,GE_LAUNCH_ROCKET,ID());
			P.w_u16							(u16(getCurrentRocket()->ID()));
			u_EventSend						(P);
		}
	}
}

void CWeaponRPG7::PlayAnimReload()
{
	VERIFY(GetState()==eReload);
	PlayHUDMotion("anim_reload", FALSE, this, GetState());
}

void CWeaponRPG7::OnEvent(NET_Packet& P, u16 type) 
{
	inherited::OnEvent(P,type);
	u16 id;
	switch (type) {
		case GE_OWNERSHIP_TAKE : {
			P.r_u16(id);
			CRocketLauncher::AttachRocket(id, this);
		} break;
		case GE_OWNERSHIP_REJECT:
		case GE_LAUNCH_ROCKET	: 
			{
			bool bLaunch = (type==GE_LAUNCH_ROCKET);
			P.r_u16(id);
			CRocketLauncher::DetachRocket(id, bLaunch);
			if(bLaunch)
				UpdateMissileVisibility();
		} break;
	}
}

void CWeaponRPG7::net_Import( NET_Packet& P)
{
	inherited::net_Import		(P);
	UpdateMissileVisibility		();
}
