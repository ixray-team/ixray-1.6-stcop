#include "stdafx.h"
#include "WeaponRPG7.h"
#include "xrServer_Objects_ALife_Items.h"
#include "ExplosiveRocket.h"
#include "Entity.h"
#include "Level.h"
#include "player_hud.h"
#include "HUDManager.h"
#include "Inventory.h"
#include "InventoryOwner.h"
#include "Level_Bullet_Manager.h"
#include "../xrEngine/GameMtlLib.h"

CWeaponRPG7::CWeaponRPG7()
{
	CWeapon::m_bIAmWeaponRPG7 = true;
}

void CWeaponRPG7::Load	(const char* section)
{
	inherited::Load						(section);
	CRocketLauncher::Load				(section);

	m_zoom_params.m_fScopeZoomFactor	= pSettings->r_float	(section,"max_zoom_factor");

	m_sGrenadeBoneName = pSettings->read_if_exists<str_c>(section,"grenade_bone","grenade");
	m_sHudGrenadeBoneName = pSettings->read_if_exists<str_c>(hud_sect,"grenade_bone","grenade");

	m_sRocketSection = pSettings->r_string(section,"rocket_class");

	m_rocket_explode_params.start_tr = pSettings->read_if_exists<float>(section,"rocket_misfunc_start_condition",0.0f);
	m_rocket_explode_params.end_tr = pSettings->read_if_exists<float>(section,"rocket_misfunc_end_condition",0.0f);
	m_rocket_explode_params.start_prob = pSettings->read_if_exists<float>(section,"rocket_misfunc_start_probability",0.0f);
	m_rocket_explode_params.end_prob = pSettings->read_if_exists<float>(section,"rocket_misfunc_end_probability",0.0f);

	m_reactive_hit_params.dist = pSettings->read_if_exists<float>(section,"reactive_hit_dist",0.0f);
	m_reactive_hit_params.power = pSettings->read_if_exists<float>(section,"reactive_hit_power",0.0f);
	m_reactive_hit_params.impulse = pSettings->read_if_exists<float>(section,"reactive_hit_impulse",0.0f);
	m_reactive_hit_params.buck = pSettings->read_if_exists<u32>(section,"reactive_hit_buck",1);
	m_reactive_hit_params.reverse_buck = pSettings->read_if_exists<u32>(section,"reactive_hit_reverse_buck",1);
	m_reactive_hit_params.buck_disp = pSettings->read_if_exists<float>(section,"reactive_hit_buck_disp",1.0f);
	m_reactive_hit_params.reverse_disp = pSettings->read_if_exists<float>(section,"reactive_hit_reverse_disp",0.1f);
	m_reactive_hit_params.reverse_disp2 = pSettings->read_if_exists<float>(section,"reactive_hit_reverse_disp2",0.1f);
	m_reactive_hit_params.reverse_power = pSettings->read_if_exists<float>(section,"reactive_hit_reverse_power",m_reactive_hit_params.power);
	m_reactive_hit_params.type = (ALife::EHitType)pSettings->read_if_exists<u32>(section,"reactive_hit_type",ALife::eHitTypeExplosion);
	m_reactive_hit_params.reverse_k = pSettings->read_if_exists<float>(section,"reactive_hit_reverse_k",1.0f);
	m_reactive_hit_params.bullet_material = pSettings->read_if_exists<str_c>(section,"reactive_hit_bullet_material","default");
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

		LaunchRocket(launch_matrix, d, zero_vel);

		CExplosiveRocket* pGrenade = getCurrentRocket()->cast_explosive_rocket();
		VERIFY(pGrenade);
		pGrenade->SetInitiator(H_Parent() ? H_Parent()->ID() : ID());

		if (OnServer())
		{
			NET_Packet P;
			u_EventGen(P, GE_LAUNCH_ROCKET, ID());
			P << getCurrentRocket()->ID();
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

	if (HudItemData() != nullptr)
	{
		HudItemData()->set_bone_visible(m_sHudGrenadeBoneName, vis_hud, true);
	}

	IKinematics* pWeaponVisual = PKinematics(Visual());
	VERIFY(pWeaponVisual);
	pWeaponVisual->LL_SetBoneVisible(pWeaponVisual->LL_BoneID(m_sGrenadeBoneName), vis_weap, true);
}

bool CWeaponRPG7::net_Spawn(CSE_Abstract* DC) 
{
	bool l_res = inherited::net_Spawn(DC);

	UpdateMissileVisibility();
	if(iAmmoElapsed && !getCurrentRocket())
		CRocketLauncher::SpawnRocket(m_sRocketSection, this);

	return l_res;
}

void CWeaponRPG7::OnStateSwitch(u8 S) 
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

	ReactiveHit();
	inherited::FireStart();
}

void CWeaponRPG7::ReactiveHit()
{
	if (iAmmoElapsed == 0)
	{
		return;
	}

	//При стрельбе НПС не применяем поражение реактивной струей
	if (H_Parent() && !H_Parent()->cast_actor())
	{
		return;
	}

	if (m_reactive_hit_params.dist <= 0 || m_reactive_hit_params.power <= 0 || m_reactive_hit_params.impulse <= 0 || m_reactive_hit_params.buck <= 0)
	{
		return;
	}

	Fvector3 pos = get_LastFP();
	Fvector3 dir = get_LastFD();
	dir.mul(-1);

	CCartridge cartridge;
	cartridge.param_s.kAP = 1.f;
	cartridge.param_s.kAirRes = 1.f;
	cartridge.param_s.fWallmarkSize = 0.05f;
	cartridge.m_LocalAmmoType = 0;
	cartridge.bullet_material_idx = GMLib.GetMaterialIdx(m_reactive_hit_params.bullet_material);
	cartridge.m_InvShortName = "";
	cartridge.m_flags.zero();

	for (int i = 0; i < m_reactive_hit_params.buck - 1; i++)
	{
		Fvector3 tgt_dir;
		// Хитуем тех, кто сзади
		tgt_dir.random_dir(dir, m_reactive_hit_params.buck_disp);

		u16 id = ID();
		if (H_Parent())
		{
			id = H_Parent()->ID();
		};
		Level().BulletManager().AddBullet(pos, tgt_dir, 330, m_reactive_hit_params.power, m_reactive_hit_params.impulse, id, ID(), m_reactive_hit_params.type, m_reactive_hit_params.dist, cartridge, 1, true, false);

		//имитируем отражение струи в стрелка при близком препятствии - для этого используем disp2
		tgt_dir.random_dir(dir, m_reactive_hit_params.reverse_disp);
		collide::rq_result rqr;
		if (Level().ObjectSpace.RayPick(pos, tgt_dir, m_reactive_hit_params.dist, collide::rqtStatic, rqr, H_Parent()))
		{
			//За стрелком обнаружилось препятствие, хитуем стрелка
			for (int j = 0; j < m_reactive_hit_params.reverse_buck - 1; j++)
			{
				Fvector3 point = pos;
				Fvector3 dir2 = tgt_dir;

				dir2.mul(rqr.range * 0.9);
				point.add(dir2);

				dir2 = pos;
				dir2.sub(point);
				dir2.normalize();
				Fvector3 tgt_dir2;
				tgt_dir2.random_dir(dir2, m_reactive_hit_params.reverse_disp2);

				// Вычисляем хит отраженной струи
				float rdist = m_reactive_hit_params.dist - rqr.range;
				if (rdist < 0)
				{
					rdist = 0;
				}
				float rhit_cur = m_reactive_hit_params.reverse_power * rdist / m_reactive_hit_params.dist;

				// Вычисляем дистанцию полета отраженной струи
				rdist = m_reactive_hit_params.dist - rqr.range;
				if (rdist < 0)
				{
					rdist = 0;
				}
				rdist = rdist / m_reactive_hit_params.dist;
				rdist = m_reactive_hit_params.dist * m_reactive_hit_params.reverse_k * rdist;

				Level().BulletManager().AddBullet(point, tgt_dir2, 330, rhit_cur, m_reactive_hit_params.impulse, ID(), ID(), m_reactive_hit_params.type, rdist * (0.9 + Random.randF() * 0.15), cartridge, 1, true, true);
			};
		};
  }
}

void CWeaponRPG7::OnEvent(NET_Packet& P, u16 type) 
{
	inherited::OnEvent(P,type);
	ALife::_OBJECT_ID id;
	switch (type) {
		case GE_OWNERSHIP_TAKE : {
			P >> id;
			CRocketLauncher::AttachRocket(id, this);
		} break;
		case GE_OWNERSHIP_REJECT:
		case GE_LAUNCH_ROCKET	: 
			{
			bool bLaunch = (type==GE_LAUNCH_ROCKET);
			P >> id;
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
