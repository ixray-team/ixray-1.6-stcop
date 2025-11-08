#include "StdAfx.h"
#include "WeaponRPG7.h"
#include "xrServer_Objects_ALife_Items.h"
#include "ExplosiveRocket.h"
#include "Entity.h"
#include "Level.h"
#include "player_hud.h"
#include "HUDManager.h"
#include "Inventory.h"
#include "InventoryOwner.h"

CWeaponRPG7::CWeaponRPG7()
{
	CWeapon::m_bIAmWeaponRPG7 = true;
}

void CWeaponRPG7::Load	(LPCSTR section)
{
	inherited::Load						(section);
	CRocketLauncher::Load				(section);

	m_zoom_params.m_fScopeZoomFactor	= pSettings->r_float	(section,"max_zoom_factor");

	m_sGrenadeBoneName					= READ_IF_EXISTS(pSettings, r_string, section,"grenade_bone", "grenade");
	m_sHudGrenadeBoneName				= READ_IF_EXISTS(pSettings, r_string, hud_sect,"grenade_bone", "grenade");

	m_sRocketSection					= pSettings->r_string	(section,"rocket_class");

	m_rocket_explode_params.start_tr = READ_IF_EXISTS(pSettings, r_float, section, "rocket_misfunc_start_condition", 0.0f);
	m_rocket_explode_params.end_tr = READ_IF_EXISTS(pSettings, r_float, section, "rocket_misfunc_end_condition", 0.0f);
	m_rocket_explode_params.start_prob = READ_IF_EXISTS(pSettings, r_float, section, "rocket_misfunc_start_probability", 0.0f);
	m_rocket_explode_params.end_prob = READ_IF_EXISTS(pSettings, r_float, section, "rocket_misfunc_end_probability", 0.0f);
}

bool CWeaponRPG7::AllowBore()
{
	return inherited::AllowBore() && 0!=iAmmoElapsed;
}

void CWeaponRPG7::FireTrace(const Fvector& P, const Fvector& D)
{
	inherited::FireTrace(P, D);

	m_iShotNum = 0;
	m_bFireSingleShot = true;
	bWorking = false;

	if (GetState() == eFire && getRocketCount())
	{
		Fvector p1, d1, p;
		Fvector p2, d2, d;
		p1.set(P);
		d1.set(D);
		p = p1;
		d = d1;

		if (CEntity* E = H_Parent() != nullptr ? H_Parent()->cast_entity() : nullptr)
		{
			E->g_fireParams(this, p2, d2);
			p = p2;
			d = d2;

			if (IsHudModeNow())
			{
				Fvector	p0;
				float dist = HUD().GetCurrentRayQuery().range;
				p0.mul(d2, dist);
				p0.add(p1);
				p = p1;
				d.sub(p0, p1);
				d.normalize_safe();
			}
		}

		Fmatrix launch_matrix;
		launch_matrix.identity();
		launch_matrix.k.set(d);
		Fvector::generate_orthonormal_basis(launch_matrix.k, launch_matrix.j, launch_matrix.i);
		launch_matrix.c.set(p);

		d.normalize();
		d.mul(m_fLaunchSpeed);

		CRocketLauncher::LaunchRocket(launch_matrix, d, zero_vel);

		CExplosiveRocket* pGrenade = getCurrentRocket()->cast_explosive_rocket();
		VERIFY(pGrenade);
		pGrenade->SetInitiator(H_Parent()->ID());

		if (OnServer())
		{
			NET_Packet P;
			u_EventGen(P, GE_LAUNCH_ROCKET, ID());
			P.w_u16(u16(getCurrentRocket()->ID()));
			u_EventSend(P);
		}
	}
	UpdateMissileVisibility();
}

void CWeaponRPG7::on_a_hud_attach()
{
	inherited::on_a_hud_attach		();
	UpdateMissileVisibility			();
}

void CWeaponRPG7::UpdateMissileVisibility()
{
	bool vis_hud, vis_weap;
	vis_hud = (!!iAmmoElapsed || GetState() == eReload);
	vis_weap = !!iAmmoElapsed;

	if (GetHUDmode())
	{
		HudItemData()->set_bone_visible(m_sHudGrenadeBoneName, vis_hud, TRUE);
	}

	IKinematics* pWeaponVisual = PKinematics(Visual());
	VERIFY(pWeaponVisual);
	pWeaponVisual->LL_SetBoneVisible(pWeaponVisual->LL_BoneID(m_sGrenadeBoneName), vis_weap, TRUE);
}

BOOL CWeaponRPG7::net_Spawn(CSE_Abstract* DC) 
{
	BOOL l_res = inherited::net_Spawn(DC);

	UpdateMissileVisibility();
	if(iAmmoElapsed && !getCurrentRocket())
		CRocketLauncher::SpawnRocket(m_sRocketSection, this);

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
		CRocketLauncher::SpawnRocket(m_sRocketSection.c_str(), this);
}

void CWeaponRPG7::SwitchState(u32 S) 
{
	inherited::SwitchState(S);
}

bool CWeaponRPG7::CheckRLMisfireRocket()
{
	if (!ParentIsActor() || getRocketCount() <= 0 || GetAmmoElapsed() <= 0)
	{
		return false;
	}

	shared_str sect = cNameSect();
	
	float start_tr = m_rocket_explode_params.start_tr;
	float end_tr = m_rocket_explode_params.end_tr;
	float start_prob = m_rocket_explode_params.start_prob;
	float end_prob = m_rocket_explode_params.end_prob;
	float cond = GetCondition();
	
	bool is_expl = false;
	
	if (cond > start_tr || start_tr == end_tr)
	{
		is_expl = false;
	}
	else if (cond < end_tr)
	{
		is_expl = ::Random.randF(0.0f, 1.0f) < end_prob;
	}
	else
	{
		is_expl = ::Random.randF(0.0f, 1.0f) < start_prob + (end_prob - start_prob) * (start_tr - cond) / (start_tr - end_tr);
	}
	
	if (is_expl)
	{
		Fvector p = Position();
		Fvector n = { 0.0f, 1.0f, 0.0f };
		CCustomRocket* r = getCurrentRocket();
		DetachRocket(r->ID(), true);
		r->Contact(p, n);
	
		while (getRocketCount())
		{
			dropCurrentRocket();
		}
	
		m_magazine.pop_back();
		--iAmmoElapsed;
		UpdateMissileVisibility();
	}

	return is_expl;
}

void CWeaponRPG7::FireStart()
{
	if (!iAmmoElapsed)
	{
		if (infinite_fire())
		{
			ReloadMagazine();
		}
	}

	if (CheckRLMisfireRocket())
	{
		return;
	}

	inherited::FireStart();
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
