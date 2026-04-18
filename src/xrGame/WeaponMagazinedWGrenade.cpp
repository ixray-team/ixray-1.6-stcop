#include "StdAfx.h"
#include "WeaponMagazinedWGrenade.h"
#include "Entity.h"
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
#include "InventoryOwner.h"
#include "Weapons/Components/WeaponAmmoBones.h"

#ifdef DEBUG
#	include "phdebug.h"
#endif

void CWeaponMagazinedWGrenade::Load(const char* section)
{
	inherited::Load(section);
	CRocketLauncher::Load(section);

	if (m_eGrenadeLauncherStatus == ALife::eAddonPermanent)
	{
		CRocketLauncher::m_fLaunchSpeed = pSettings->r_float(section, "grenade_vel");
	}

	// load ammo classes SECOND (grenade_class)
	m_ammoTypes2.clear();
	const char*				S = pSettings->r_string(section, "grenade_class");
	if (S && S[0])
	{
		string128		_ammoItem;
		int				count = _GetItemCount(S);
		for (int it = 0; it < count; ++it)
		{
			_GetItem(S, it, _ammoItem);
			m_ammoTypes2.push_back(_ammoItem);
		}
	}

	iMagazineSize2 = iMagazineSize;

	auto ReachInAllSections = [&](const char* param_name)
	{
		const char* reached_sect = section;
		const shared_str hud_section = HudSection();
		if (pSettings->line_exist(hud_section, param_name))
		{
			reached_sect = hud_section.c_str();
	}
		return reached_sect;
	};

	for (u8 i = 0; i < m_ammoTypes2.size(); i++)
	{
		shared_str mag_bone_type = shared_str().printf("gl_bone_type_%d", i);

		if (pSettings->line_exist(ReachInAllSections(*mag_bone_type), *mag_bone_type))
		{
			CreateComponent<TGrenadeLauncherAmmoBones>();
			break;
		}
	}
}

void CWeaponMagazinedWGrenade::LoadSounds(const char* section)
{
	inherited::LoadSounds(section);

	m_layered_sounds.LoadSound(section, "snd_shoot_grenade", "sndShotG", false, m_eSoundShot);

	if (SoundExist(section, "snd_shoot_grenade_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_grenade_actor, TRUE);
		m_layered_sounds.LoadSound(section, "snd_shoot_grenade_actor", "sndShotGActor", false, m_eSoundShot);
	}

	if (SoundExist(section, "snd_load_grenade"))
	{
		m_sounds.LoadSound(section, "snd_load_grenade", "sndReloadG", true, m_eSoundReload);
	}
	else
	{
		m_sounds.LoadSound(section, "snd_reload_grenade", "sndReloadG", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_change_grenade"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_grenade_change, TRUE);
		m_sounds.LoadSound(section, "snd_change_grenade", "sndChangeGrenade", true, m_eSoundReload);
	}

	m_sounds.LoadSound(section, "snd_switch", "sndSwitch", true, m_eSoundReload);

	if (SoundExist(section, "snd_switch_g"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_switch_g, TRUE);
		m_sounds.LoadSound(section, "snd_switch_g", "sndSwitchG", true, m_eSoundReload);
	}
}

void CWeaponMagazinedWGrenade::net_Destroy()
{
	inherited::net_Destroy();
}

bool CWeaponMagazinedWGrenade::net_Spawn(CSE_Abstract* DC)
{
	CSE_ALifeItemWeapon* const weapon = DC->cast_item_weapon();
	R_ASSERT(weapon);
	if (IsGameTypeSingle())
	{
		inherited::net_Spawn_install_upgrades(weapon->m_upgrades);
	}

	bool l_res = inherited::net_Spawn(DC);

	UpdateGrenadeVisibility(!!iAmmoElapsed);
	SetPending(FALSE);

	if (!IsGameTypeSingle())
	{
		iAmmoElapsed2 = weapon->a_elapsed_grenades.grenades_count;
		m_ammoType2 = weapon->a_elapsed_grenades.grenades_type;
	}

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
	}
	else
	{
		xr_vector<CCartridge>* pM = nullptr;
		bool b_if_grenade_mode = (m_bGrenadeMode && iAmmoElapsed && !getRocketCount());
		if (b_if_grenade_mode)
			pM = &m_magazine;

		bool b_if_simple_mode = (!m_bGrenadeMode && m_magazine2.size() && !getRocketCount());
		if (b_if_simple_mode)
			pM = &m_magazine2;

		if (b_if_grenade_mode || b_if_simple_mode)
		{
			shared_str fake_grenade_name = pSettings->r_string(pM->back().m_ammoSect, "fake_grenade_name");

			CRocketLauncher::SpawnRocket(*fake_grenade_name, this);
		}
	}
	return l_res;
}

shared_str CWeaponMagazinedWGrenade::SetCurrentReloadAnimation()
{
	if (!IsGrenadeLauncherAttached())
	{
		return inherited::SetCurrentReloadAnimation();
	}

	shared_str anim = "anm_reload";

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		const char* end_suffix = m_bGrenadeMode ? "_g" : "_w_gl";

		if (m_bUseRevolverScheme && !IsGrenadeMode() && !IsMisfire() && !IsChangeAmmoType())
		{
			if (AddSuffixName(anim, shared_str().printf("_%d", iAmmoElapsed).c_str(), end_suffix))
			{
				return anim;
			}
		}

		if (GetQueueSize() == -1)
		{
			AddSuffixName(anim, "_auto");
		}
		else if (GetQueueSize() == 3)
		{
			AddSuffixName(anim, "_triple");
		}

		int GetElapsed = m_bGrenadeMode ? iAmmoElapsed2 : iAmmoElapsed;
		bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 && GetElapsed == 0 : GetElapsed == 0;

		if (IsMisfire())
		{
			if (IsChangeAmmoType() && (!m_bGrenadeMode || iAmmoElapsed))
			{
				AddSuffixName(anim, "_jammed_ammochange", end_suffix);
			}

			if (empty)
			{
				AddSuffixName(anim, "_misfire_last", end_suffix);
				AddSuffixName(anim, "_jammed_last", end_suffix);
			}
			else
			{
				AddSuffixName(anim, "_misfire", end_suffix);
				AddSuffixName(anim, "_jammed", end_suffix);
			}
		}
		else if (empty)
		{
			if (IsChangeAmmoType() && (!m_bGrenadeMode || iAmmoElapsed))
			{
				AddSuffixName(anim, "_empty_ammochange", end_suffix);
			}

			AddSuffixName(anim, "_empty", end_suffix);
		}

		if (IsChangeAmmoType() && (!m_bGrenadeMode || iAmmoElapsed))
		{
			AddSuffixName(anim, "_ammochange", end_suffix);
		}

		if (ScopeAttachable() && !IsScopeAttached())
		{
			AddSuffixName(anim, "_noscope", end_suffix);
		}

		AddSuffixName(anim, end_suffix);
	}

	return anim;
}

void CWeaponMagazinedWGrenade::switch2_Reload()
{
	VERIFY(GetState() == eReload);

	if (IsGrenadeLauncherAttached() && m_bGrenadeMode)
	{
		m_bIsReloaded = false;
		if (TGrenadeLauncherAmmoBones* GLAmmoBones = GetComponent<TGrenadeLauncherAmmoBones>())
		{
			GLAmmoBones->UpdateGLAmmoBones(this, GetAmmoType(true));
		}

		if (IsChangeAmmoType() && iAmmoElapsed && m_eSoundsFlags.test(ESoundsFlags::sf_grenade_change))
		{
			PlaySound("sndChangeGrenade", get_LastFP2());
		}
		else
		{
			PlaySound("sndReloadG", get_LastFP2());
		}
		PlayHUDMotion(SetCurrentReloadAnimation(), EHudMixType::eMixAll, eReload);
	}
	else
	{
		inherited::switch2_Reload();
	}
}

shared_str CWeaponMagazinedWGrenade::SetCurrentShootAnimation()
{
	if (!IsGrenadeLauncherAttached())
	{
		return inherited::SetCurrentShootAnimation();
	}

	shared_str anim = HudAnimationExist("anm_shoot") ? "anm_shoot" : "anm_shots";

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		int GetElapsed = m_bGrenadeMode ? iAmmoElapsed2 : iAmmoElapsed;
		bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 && GetElapsed == 0 : GetElapsed == 0;
		bool last_empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 1 && GetElapsed == 0 : GetElapsed == 1;

		if (IsZoomed())
		{
			AddSuffixName(anim, "_aim");
		}

		if (IsScopeAttached() && !IsGrenadeMode())
		{
			AddSuffixName(anim, "_scope");
		}

		if (GetQueueSize() == -1)
		{
			AddSuffixName(anim, "_auto");
		}
		else if (GetQueueSize() == 3)
		{
			AddSuffixName(anim, "_triple");
		}

		if (IsMisfire())
		{
			AddSuffixName(anim, "_misfire");
			AddSuffixName(anim, "_jammed");
		}

		if (m_bGrenadeMode)
		{
			if (empty)
			{
				AddSuffixName(anim, "_empty", "_g");
			}
		}
		else if (last_empty)
		{
			AddSuffixName(anim, "_last");
			AddSuffixName(anim, "_l");
		}

		AddSuffixName(anim, m_bGrenadeMode ? "_g" : "_w_gl");
	}

	return anim;
}

void CWeaponMagazinedWGrenade::switch2_Hiding()
{
	OnZoomOut();
	CWeapon::FireEnd();

	int default_mag = (m_bGrenadeMode ? iAmmoElapsed2 : iAmmoElapsed) + iAmmoChamberElapsed;

	if (m_sounds_enabled)
	{
		if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_jam_g) && IsGrenadeMode() && IsMisfire())
		{
			PlaySound("sndHideMisG", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_jam_w_gl) && IsGrenadeLauncherAttached() && IsMisfire())
		{
			PlaySound("sndHideMisWGL", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_jam) && IsMisfire())
		{
			PlaySound("sndHideMis", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_empty_g) && IsGrenadeMode() && default_mag == 0)
		{
			PlaySound("sndHideEmptyG", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_empty_w_gl) && IsGrenadeLauncherAttached() && default_mag == 0)
		{
			PlaySound("sndHideEmptyWGL", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_empty) && default_mag == 0)
		{
			PlaySound("sndHideEmpty", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_g) && IsGrenadeMode())
		{
			PlaySound("sndHideG", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_w_gl) && IsGrenadeLauncherAttached())
		{
			PlaySound("sndHideWGL", get_LastFP());
		}
		else
		{
			PlaySound("sndHide", get_LastFP());
		}
	}

	PlayAnimHide();
	SetPending(TRUE);
}

void CWeaponMagazinedWGrenade::switch2_Showing()
{
	int default_mag = (m_bGrenadeMode ? iAmmoElapsed2 : iAmmoElapsed) + iAmmoChamberElapsed;

	if (m_sounds_enabled)
	{
		if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_jam_g) && IsGrenadeMode() && IsMisfire())
		{
			PlaySound("sndShowMisG", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_jam_w_gl) && IsGrenadeLauncherAttached() && IsMisfire())
		{
			PlaySound("sndShowMisWGL", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_jam) && IsMisfire())
		{
			PlaySound("sndShowMis", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_empty_g) && IsGrenadeMode() && default_mag == 0)
		{
			PlaySound("sndShowEmptyG", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_empty_w_gl) && IsGrenadeLauncherAttached() && default_mag == 0)
		{
			PlaySound("sndShowEmptyWGL", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_empty) && default_mag == 0)
		{
			PlaySound("sndShowEmpty", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_g) && IsGrenadeMode())
		{
			PlaySound("sndShowG", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_w_gl) && IsGrenadeLauncherAttached())
		{
			PlaySound("sndShowWGL", get_LastFP());
		}
		else
		{
			PlaySound("sndShow", get_LastFP());
		}
	}

	SetPending(TRUE);
	PlayAnimShow();
}

bool CWeaponMagazinedWGrenade::SwitchMode()
{
	if (!IsGrenadeLauncherAttached())
		return false;

	if (!SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfGLAUNCHSWITCH))
	{
		return false;
	}

	bool bUsefulStateToSwitch = (!IsPending() && !IsZoomed() && (GetState() == eIdle || GetState() == eMisfire));

	if (!bUsefulStateToSwitch)
		return false;

	SwitchState(eSwitch);

	m_BriefInfo_CalcFrame = 0;

	return true;
}

void CWeaponMagazinedWGrenade::PerformSwitchGL()
{
	m_bGrenadeMode = !m_bGrenadeMode;

	iMagazineSize = m_bGrenadeMode ? 1 : iMagazineSize2;

	CShootingObject::fire_mode = m_bGrenadeMode ? eGlauncherFire : IsSilencerAttached() ? eSilencerFire : eDefaultFire;

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

	switch (cmd)
	{
	case kWPN_FUNC:
	{
		if (flags & CMD_START)
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

	u32 CurrentState = GetState();

	if (!iAmmoElapsed)
	{
		if (!infinite_fire())
		{
			if (CurrentState == eIdle || CurrentState == eEmptyClick && !m_bBlockEmptyClick)
			{
				if (m_eAnimationsFlags.test(EAnimationsFlags::af_empty_click))
				{
					SwitchState(eEmptyClick);
				}
				else
				{
					OnEmptyClick();
				}
			}
			return;
		}
		else
		{
			ReloadMagazine();
		}
	}

	if (GetState() != eIdle)
	{
		return;
	}

	if (IsPending())
	{
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
	VERIFY(fOneShotTime > 0.f);

	//режим стрельбы подствольника
	if (m_bGrenadeMode)
	{
		if (!iAmmoElapsed)
			return;

		Fvector	p1, d;
		p1.set(get_LastFP2());
		d.set(get_LastFD());
		if (!H_Parent()) return;
		CGameObject* GO = H_Parent()->cast_game_object();
		if (!GO || GO->getDestroy()) return;
		CEntity* entity = GO->cast_entity();
		if (!entity) return;
		CInventoryOwner* inventory_owner = entity->cast_inventory_owner();
		if (!inventory_owner || !inventory_owner->m_inventory) return;

		entity->g_fireParams(this, p1, d);

		if (IsGameTypeSingle())
			p1.set(get_LastFP2());

		Fmatrix launch_matrix;
		launch_matrix.identity();
		launch_matrix.k.set(d);
		Fvector::generate_orthonormal_basis(launch_matrix.k, launch_matrix.j, launch_matrix.i);

		launch_matrix.c.set(p1);

		if (IsGameTypeSingle() && IsZoomed() && GO->cast_actor())
		{
			H_Parent()->setEnabled(FALSE);
			setEnabled(FALSE);

			collide::rq_result RQ;
			bool HasPick = Level().ObjectSpace.RayPick(p1, d, 300.0f, collide::rqtStatic, RQ, this);

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
		VERIFY2(_valid(launch_matrix), "CWeaponMagazinedWGrenade::SwitchState. Invalid launch_matrix!");
		CRocketLauncher::LaunchRocket(launch_matrix, d, zero_vel); 

		CExplosiveRocket* pGrenade = getCurrentRocket()->cast_explosive_rocket();
		if (!IsGameTypeSingle())
		{
			if (!pGrenade)
			{
				if (m_ammoTypes.size() > m_ammoType && pSettings->line_exist(m_ammoTypes[m_ammoType].c_str(), "fake_grenade_name"))
				{
					shared_str fake_grenade_name = pSettings->r_string(m_ammoTypes[m_ammoType].c_str(), "fake_grenade_name");
					if (fake_grenade_name.size())
					{
						CRocketLauncher::SpawnRocket(*fake_grenade_name, this);
						pGrenade = smart_cast<CExplosiveRocket*>(getCurrentRocket());
					}
				}

				if (!pGrenade)
				{
					return;
				}
			}
		}

		VERIFY(pGrenade);
		pGrenade->SetInitiator(H_Parent()->ID());

		if (Local() && OnServer())
		{
			VERIFY(m_magazine.size());
			m_magazine.pop_back();
			--iAmmoElapsed;
			VERIFY((u32)iAmmoElapsed == m_magazine.size());

			NET_Packet P;
			u_EventGen(P, GE_LAUNCH_ROCKET, ID());
			P.w_u16(getCurrentRocket()->ID());
			u_EventSend(P);
		}
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
	u16 id;
	switch (type)
	{
	case GE_WPN_UNLOAD_AMMO:
	{
		UnloadMagazine();

		u8 full_unload = P.r_u8();
		if (full_unload && IsGrenadeLauncherAttached())
		{
			PerformSwitchGL();
			UnloadMagazine();
			PerformSwitchGL();
		}
	}break;
	case GE_OWNERSHIP_TAKE:
	{
		P.r_u16(id);
		CRocketLauncher::AttachRocket(id, this);
	}
	break;
	case GE_OWNERSHIP_REJECT:
	case GE_LAUNCH_ROCKET:
	{
		bool bLaunch = (type == GE_LAUNCH_ROCKET);
		P.r_u16(id);
		CRocketLauncher::DetachRocket(id, bLaunch);
		if (bLaunch)
		{
			PlayAnimShoot();

			if (m_eSoundsFlags.test(ESoundsFlags::sf_shoot_grenade_actor))
			{
				m_layered_sounds.PlaySound("sndShotGActor", get_LastFP2(), H_Root(), !!GetHUDmode(), false, true);
			}
			else
			{
				m_layered_sounds.PlaySound("sndShotG", get_LastFP2(), H_Root(), !!GetHUDmode(), false, true);
			}

			AddShotEffector();
			StartFlameParticle();
		}
		break;
	}
	default:
	inherited::OnEvent(P, type);
	break;
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
		{
			dropCurrentRocket();
		}
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

	if (m_bGrenadeMode && m_eSoundsFlags.test(ESoundsFlags::sf_switch_g))
	{
		PlaySound("sndSwitchG", get_LastFP());
	}
	else
	{
		PlaySound("sndSwitch", get_LastFP());
	}

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
		SetState(eIdle);
		SetPending(FALSE);
	}
}

bool CWeaponMagazinedWGrenade::CanAttach(PIItem pIItem)
{
	CGrenadeLauncher* pGrenadeLauncher = pIItem->cast_addon_grenade_launcher();

	if (pGrenadeLauncher && ALife::eAddonAttachable == m_eGrenadeLauncherStatus && 0 == (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) && !xr_strcmp(*m_sGrenadeLauncherName, pIItem->object().cNameSect()))
	{
		return true;
	}
	else
	{
		return inherited::CanAttach(pIItem);
	}
}

bool CWeaponMagazinedWGrenade::CanDetach(const char* item_section_name)
{
	if (ALife::eAddonAttachable == m_eGrenadeLauncherStatus && 0 != (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) && !xr_strcmp(*m_sGrenadeLauncherName, item_section_name))
	{
		return true;
	}
	else
	{
		return inherited::CanDetach(item_section_name);
	}
}

bool CWeaponMagazinedWGrenade::Attach(PIItem pIItem, bool b_send_event)
{
	CGrenadeLauncher* pGrenadeLauncher = pIItem->cast_addon_grenade_launcher();

	if (pGrenadeLauncher && ALife::eAddonAttachable == m_eGrenadeLauncherStatus && 0 == (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) && !xr_strcmp(*m_sGrenadeLauncherName, pIItem->object().cNameSect()))
	{
		if (m_bRestGlSil && SilencerAttachable() && IsSilencerAttached())
		{
			Detach(*GetSilencerName(), true);
		}

		m_flagsAddOnState |= CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher;

		CRocketLauncher::m_fLaunchSpeed = pGrenadeLauncher->GetGrenadeVel();

		//уничтожить подствольник из инвентаря
		if (b_send_event)
		{
			if (OnServer())
			{
				pIItem->object().DestroyObject();
			}
		}

		InitAddons();
		UpdateAddonsVisibility();
		UpdateHUDAddonsVisibility();
		ProcessScope();

		if (GetState() == eIdle)
		{
			PlayAnimIdle();
		}

		return true;
	}
	else
	{
		return inherited::Attach(pIItem, b_send_event);
	}
}

bool CWeaponMagazinedWGrenade::Detach(const char* item_section_name, bool b_spawn_item)
{
	if (ALife::eAddonAttachable == m_eGrenadeLauncherStatus && 0 != (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) && !xr_strcmp(*m_sGrenadeLauncherName, item_section_name))
	{
		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher;

		// Now we need to unload GL's magazine
		if (!m_bGrenadeMode)
		{
			PerformSwitchGL();
		}

		UnloadMagazine();
		PerformSwitchGL();

		UpdateAddonsVisibility();
		UpdateHUDAddonsVisibility();
		ProcessScope();

		if (GetState() == eIdle)
		{
			PlayAnimIdle();
		}

		m_fGrenadeAttachedRecoil = 1.0f;

		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}
	else
	{
		return inherited::Detach(item_section_name, b_spawn_item);
	}
}

void CWeaponMagazinedWGrenade::InitAddons()
{
	inherited::InitAddons();

	if (GrenadeLauncherAttachable())
	{
		if (IsGrenadeLauncherAttached())
		{
			CRocketLauncher::m_fLaunchSpeed = pSettings->r_float(*m_sGrenadeLauncherName, "grenade_vel");
			m_fGrenadeAttachedRecoil = READ_IF_EXISTS(pSettings, r_float, *m_sGrenadeLauncherName, "grenade_attached_recoil", 1.0f);
		}
	}

}

bool CWeaponMagazinedWGrenade::UseScopeTexture()
{
	return inherited::UseScopeTexture() && !m_bGrenadeMode;
};

float	CWeaponMagazinedWGrenade::CurrentZoomFactor()
{
	if (IsGrenadeLauncherAttached() && m_bGrenadeMode) return m_zoom_params.m_fIronSightZoomFactor;
	return inherited::CurrentZoomFactor();
}

//виртуальные функции для проигрывания анимации HUD
void CWeaponMagazinedWGrenade::PlayAnimModeSwitch()
{
	PlayHUDMotion(SetCurrentStateAnimation("anm_switch"), EHudMixType::eMixAll, eSwitch);
}

shared_str CWeaponMagazinedWGrenade::SetCurrentStateAnimation(const shared_str& first_name)
{
	if (!IsGrenadeLauncherAttached())
	{
		return inherited::SetCurrentStateAnimation(first_name);
	}

	shared_str anim = first_name;

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		if (GetQueueSize() == -1)
		{
			AddSuffixName(anim, "_auto");
		}
		else if (GetQueueSize() == 3)
		{
			AddSuffixName(anim, "_triple");
		}

		int GetElapsed = m_bGrenadeMode ? iAmmoElapsed2 : iAmmoElapsed;
		bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 && GetElapsed == 0 : GetElapsed == 0;

		const char* end_suffix = m_bGrenadeMode ? "_g" : "_w_gl";

		if (IsZoomed() && IsMisfire())
		{
			AddSuffixName(anim, "_aim_misfire");
			AddSuffixName(anim, "_aim_jammed");
		}

		if (IsZoomed())
		{
			AddSuffixName(anim, "_aim", end_suffix);
		}

		if (IsMisfire())
		{
			AddSuffixName(anim, "_misfire", end_suffix);
			AddSuffixName(anim, "_jammed", end_suffix);
		}
		else if (empty)
		{
			AddSuffixName(anim, "_empty", end_suffix);
		}

		if (const CActor* pActor = Level().CurrentControlEntity()->cast_actor())
		{
			if (pActor->IsSafemode())
			{
				AddSuffixName(anim, "_safemode", end_suffix);
			}
		}

		AddSuffixName(anim, end_suffix);

		if (ScopeAttachable() && !IsScopeAttached())
		{
			AddSuffixName(anim, "_noscope");
		}
	}

	return anim;
}

void CWeaponMagazinedWGrenade::UpdateSounds()
{
	if (Device.dwFrame == dwUpdateSounds_Frame)
		return;

	inherited::UpdateSounds();

	Fvector P = get_LastFP();
	if (Device.dwFrame % 3 == 0)
	{
		m_layered_sounds.SetPosition("sndShotG", P);
		if (m_eSoundsFlags.test(ESoundsFlags::sf_shoot_grenade_actor))
		{
			m_layered_sounds.SetPosition("sndShotGActor", P);
		}
	}
	else if (Device.dwFrame % 3 == 1)
	{
		m_sounds.SetPosition("sndReloadG", P);
		if (m_eSoundsFlags.test(ESoundsFlags::sf_grenade_change))
		{
			m_sounds.SetPosition("sndChangeGrenade", P);
		}
	}
	else if (Device.dwFrame % 3 == 2)
	{
		m_sounds.SetPosition("sndSwitch", P);
		if (m_eSoundsFlags.test(ESoundsFlags::sf_switch_g))
		{
			m_sounds.SetPosition("sndSwitchG", P);
		}
	}
}

void CWeaponMagazinedWGrenade::UpdateGrenadeVisibility(bool visibility)
{
	if (HudItemData() != nullptr)
	{
	HudItemData()->set_bone_visible("grenade", visibility, TRUE);
}
}

void CWeaponMagazinedWGrenade::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	save_data(m_bGrenadeMode, output_packet);
	save_data((u32)m_magazine2.size(), output_packet);
	save_data(m_ammoType2, output_packet);

}

void CWeaponMagazinedWGrenade::load(IReader& input_packet)
{
	inherited::load(input_packet);
	bool b = false;
	load_data(b, input_packet);
	if (b != m_bGrenadeMode)
	{
		PerformSwitchGL();
	}

	u32 sz = 0;
	load_data(sz, input_packet);
	load_data(m_ammoType2, input_packet);

	CCartridge l_cartridge;
	l_cartridge.Load(m_ammoTypes2[m_ammoType2].c_str(), m_ammoType2);

	while (sz > (u32)m_magazine2.size())
	{
		m_magazine2.push_back(l_cartridge);
	}
}

void CWeaponMagazinedWGrenade::net_Export(NET_Packet& P)
{
	P.w_u8(m_bGrenadeMode ? 1 : 0);

	inherited::net_Export(P);
}

void CWeaponMagazinedWGrenade::net_Import(NET_Packet& P)
{
	bool NewMode = FALSE;
	NewMode = !!P.r_u8();
	if (NewMode != m_bGrenadeMode)
		PerformSwitchGL();

	inherited::net_Import(P);
}

float CWeaponMagazinedWGrenade::Weight() const {
	float res = inherited::Weight();
	res += GetMagazineWeight(m_magazine2);

	return res;
}

bool CWeaponMagazinedWGrenade::IsNecessaryItem(const shared_str& item_sect)
{
	return (std::find(m_ammoTypes.begin(), m_ammoTypes.end(), item_sect) != m_ammoTypes.end() ||
		std::find(m_ammoTypes2.begin(), m_ammoTypes2.end(), item_sect) != m_ammoTypes2.end()
		);
}

bool CWeaponMagazinedWGrenade::install_upgrade_ammo_class(const char* section, bool test)
{
	const char* str = {};

	bool result = false;
	bool result2 = false;

	bool grenade_mode = m_bGrenadeMode;

	int& current_size = grenade_mode ? iMagazineSize2 : iMagazineSize;

	result2 = process_if_exists(section, "ammo_mag_size", current_size, test);

	iMagazineSize2 = iMagazineSize;

	result2 = process_if_exists_set(section, "ammo_class", str, test);
	if (result2 && !test)
	{
		xr_vector<shared_str>& ammo_types = grenade_mode ? m_ammoTypes2 : m_ammoTypes;
		ammo_types.clear();
		for (int i = 0, count = _GetItemCount(str); i < count; ++i)
		{
			string128 ammo_item = {};
			_GetItem(str, i, ammo_item);
			ammo_types.push_back(ammo_item);
		}

		m_ammoType = 0;
		m_ammoType2 = 0;
	}
	result |= result2;

	return result2;
}

bool CWeaponMagazinedWGrenade::install_upgrade_impl(const char* section, bool test)
{
	const char* str = {};
	bool result = inherited::install_upgrade_impl(section, test);

	bool result2 = process_if_exists_set(section, "grenade_class", str, test);
	if (result2 && !test)
	{
		RStringVec& ammo_types = !m_bGrenadeMode ? m_ammoTypes2 : m_ammoTypes;
		ammo_types.clear();
		for (int i = 0, count = _GetItemCount(str); i < count; ++i)
		{
			string128 ammo_item = {};
			_GetItem(str, i, ammo_item);
			ammo_types.push_back(ammo_item);
		}

		m_ammoType = 0;
		m_ammoType2 = 0;
	}
	result |= result2;

	result |= process_if_exists(section, "launch_speed", m_fLaunchSpeed, test);

	result2 = process_if_exists_set(section, "snd_shoot_grenade", str, test);
	if (result2 && !test)
	{
		m_layered_sounds.LoadSound(section, "snd_shoot_grenade", "sndShotG", false, m_eSoundShot);
	}
	result |= result2;

	result2 = process_if_exists_set(section, "snd_shoot_grenade_actor", str, test);
	if (result2 && !test)
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_grenade_actor, TRUE);
		m_layered_sounds.LoadSound(section, "snd_shoot_grenade_actor", "sndShotGActor", false, m_eSoundShot);
	}
	result |= result2;

	result2 = process_if_exists_set(section, "snd_reload_grenade", str, test);
	if (result2 && !test)
	{
		if (SoundExist(section, "snd_load_grenade"))
		{
			m_sounds.LoadSound(section, "snd_load_grenade", "sndReloadG", true, m_eSoundReload);
		}
		else
		{
			m_sounds.LoadSound(section, "snd_reload_grenade", "sndReloadG", true, m_eSoundReload);
		}
	}
	result |= result2;

	result2 = process_if_exists_set(section, "snd_change_grenade", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_change_grenade", "sndChangeGrenade", true, m_eSoundReload); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_switch", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_switch", "sndSwitch", true, m_eSoundReload); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_switch_g", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_switch_g", "sndSwitchG", true, m_eSoundReload); }
	result |= result2;

	return result;
}

void CWeaponMagazinedWGrenade::net_Spawn_install_upgrades(Upgrades_type saved_upgrades)
{
	// do not delete this
	// this is intended behaviour
}


#include "../xrEngine/string_table.h"
bool CWeaponMagazinedWGrenade::GetBriefInfo(II_BriefInfo& info)
{
	VERIFY(m_pInventory);
	/*
		if(!inherited::GetBriefInfo(info))
			return false;
	*/
	string32 int_str;
	int	ae = GetAmmoElapsed() + (m_bGrenadeMode ? 0 : iAmmoChamberElapsed);
	xr_sprintf(int_str, "%d", ae);
	info.cur_ammo._set(int_str);

	if (infinite_fire())
	{
		info.cur_ammo = "∞";
	}

	if (m_iQueueSize == WEAPON_ININITE_QUEUE)
		info.fire_mode._set("A");
	else
	{
		xr_sprintf(int_str, "%d", m_iQueueSize);
		info.fire_mode._set(int_str);
	}

	if (m_pInventory->ModifyFrame() <= m_BriefInfo_CalcFrame)
		return false;

	const int at = GetSuitableAmmoTotal() - (GetAmmoElapsed() + (m_bGrenadeMode ? 0 : iAmmoChamberElapsed)); // update m_BriefInfo_CalcFrame
	xr_sprintf(int_str, "%d", at);
	info.total_ammo = int_str;

	u32 at_size = m_bGrenadeMode ? (u32)m_ammoTypes2.size() : (u32)m_ammoTypes.size();
	if (unlimited_ammo() || at_size == 0)
	{
		info.fmj_ammo._set("∞");
		info.ap_ammo._set("∞");
		info.total_ammo._set("∞");
		info.third_ammo._set("∞");
	}
	else
	{
		//Alundaio: Added third ammo type and cleanup
		info.fmj_ammo._set("");
		info.ap_ammo._set("");
		info.third_ammo._set("");

		if (at_size >= 1)
		{
			const int fmj = m_bGrenadeMode ? GetAmmoCount2(0) : GetAmmoCount(0);
			xr_sprintf(int_str, "%d", fmj);
			info.fmj_ammo._set(int_str);
		}
		if (at_size >= 2)
		{
			const int ap = m_bGrenadeMode ? GetAmmoCount2(1) : GetAmmoCount(1);
			xr_sprintf(int_str, "%d", ap);
			info.ap_ammo._set(int_str);
		}
		if (at_size >= 3)
		{
			const int third = m_bGrenadeMode ? GetAmmoCount2(2) : GetAmmoCount(2);
			xr_sprintf(int_str, "%d", third);
			info.third_ammo._set(int_str);
		}
		//-Alundaio
	}

	auto& CurrVector = !m_bGrenadeMode && m_bAmmoInChamber ? m_chamber : m_magazine;
	u8 CurrAmmoType = !m_bGrenadeMode && m_bAmmoInChamber ? m_ChamberAmmoType : m_ammoType;

	if (ae != 0 && CurrVector.size() != 0)
	{
		const char* ammo_type = m_ammoTypes[CurrVector.back().m_LocalAmmoType].c_str();
		info.name._set(g_pStringTable->translate(pSettings->r_string(ammo_type, "inv_name_short")));
		info.icon._set(ammo_type);
	}
	else
	{
		const char* ammo_type = m_ammoTypes[CurrAmmoType].c_str();
		info.name._set(g_pStringTable->translate(pSettings->r_string(ammo_type, "inv_name_short")));
		info.icon._set(ammo_type);
	}

	if (!IsGrenadeLauncherAttached())
	{
		info.grenade = "";
		return false;
	}

	int total2 = m_bGrenadeMode ? GetAmmoCount(0) : GetAmmoCount2(0);
	if (unlimited_ammo())
		xr_sprintf(int_str, "∞");
	else
	{
		if (total2)
			xr_sprintf(int_str, "%d", total2);
		else
			xr_sprintf(int_str, "X");
	}
	info.grenade = int_str;

	return true;
}

int CWeaponMagazinedWGrenade::GetAmmoCount2(u8 ammo2_type) const
{
	VERIFY(m_pInventory);
	R_ASSERT(ammo2_type < m_ammoTypes2.size());

	return GetAmmoCount_forType(m_ammoTypes2[ammo2_type]);
}

u8 CWeaponMagazinedWGrenade::GetTargetAmmoType(bool for_grenade_mode) const
{
	if (m_set_next_ammoType_on_reload != undefined_ammo_type)
	{
		return m_set_next_ammoType_on_reload;
	}

	return GetAmmoType(for_grenade_mode);
}

u8 CWeaponMagazinedWGrenade::GetAmmoType(bool for_grenade_mode) const
{
	if (for_grenade_mode)
	{
		return m_bGrenadeMode ? m_ammoType : m_ammoType2;
	}
	else
	{
		return m_bGrenadeMode ? m_ammoType2 : m_ammoType;
	}
}

void CWeaponMagazinedWGrenade::ForceUpdateHUD()
{
	inherited::ForceUpdateHUD();
	if (TGrenadeLauncherAmmoBones* GLAmmoBones = GetComponent<TGrenadeLauncherAmmoBones>())
	{
		GLAmmoBones->UpdateGLAmmoBones(this, GetAmmoType(true));
	}
}

const xr_vector<shared_str>& CWeaponMagazinedWGrenade::getAmmoTypes(bool for_grenade_mode) const
{
	if (for_grenade_mode)
	{
		return m_bGrenadeMode ? m_ammoTypes : m_ammoTypes2;
	}
	else
	{
		return m_bGrenadeMode ? m_ammoTypes2 : m_ammoTypes;
	}
}

int CWeaponMagazinedWGrenade::GetCurrentElapsed(bool for_grenade_mode) const
{
	if (for_grenade_mode)
	{
		return m_bGrenadeMode ? iAmmoElapsed : iAmmoElapsed2;
	}
	else
	{
		return m_bGrenadeMode ? iAmmoElapsed2 : iAmmoElapsed;
	}
}