#include "stdafx.h"

#include "Flamethrower.h"
#include "actor.h"
#include "inventory.h"
#include "InventoryOwner.h"
#include "xrserver_objects_alife_items.h"
#include "EffectorZoomInertion.h"
#include "../xrEngine/xr_level_controller.h"
#include "UIGameCustom.h"
#include "object_broker.h"
#include "../xrEngine/string_table.h"
#include "MPPlayersBag.h"
#include "../xrUI/Widgets/UIStatic.h"
#include "game_object_space.h"
#include "../xrScripts/script_callback_ex.h"
#include "script_game_object.h"
#include "FlameCanister.h"
#include "FlamethrowerTraceCollision.h"
#include "ai_object_location.h"
#include "BurningComponent.h"

ENGINE_API bool	g_dedicated_server;
ENGINE_API  extern float psHUD_FOV;
ENGINE_API  extern float psHUD_FOV_def;

//CUIXml*				pWpnScopeXml = NULL;

CFlamethrower::CFlamethrower(ESoundTypes eSoundType) : CWeapon(), TraceManager(this)
{
	m_eSoundShow = static_cast<ESoundTypes>(SOUND_TYPE_ITEM_TAKING | eSoundType);
	m_eSoundHide = static_cast<ESoundTypes>(SOUND_TYPE_ITEM_HIDING | eSoundType);
	m_eSoundShot = static_cast<ESoundTypes>(SOUND_TYPE_WEAPON_SHOOTING | eSoundType);
	m_eSoundEmptyClick = static_cast<ESoundTypes>(SOUND_TYPE_WEAPON_EMPTY_CLICKING | eSoundType);
	m_eSoundReload = static_cast<ESoundTypes>(SOUND_TYPE_WEAPON_RECHARGING | eSoundType);
	m_eSoundClose = static_cast<ESoundTypes>(SOUND_TYPE_WEAPON_RECHARGING);
	m_sounds_enabled = true;

	psWpnAnimsFlag = { 0 };

	m_bFireSingleShot = false;
	m_fOldBulletSpeed = 0;
	m_bLockType = false;
	m_bNeedBulletInGun = false;
	m_opened = false;
	m_bUseFiremodeChangeAnim = true;

	m_sSndShotCurrent = nullptr;
}

CFlamethrower::~CFlamethrower()
{
	// sounds
}


void CFlamethrower::net_Destroy()
{
	inherited::net_Destroy();
}

void CFlamethrower::SetAnimFlag(u32 flag, LPCSTR anim_name)
{
	if (pSettings->line_exist(hud_sect, anim_name))
		psWpnAnimsFlag.set(flag, TRUE);
	else
		psWpnAnimsFlag.set(flag, FALSE);
}

void CFlamethrower::Load(LPCSTR section)
{
	inherited::Load(section);

	// ��������� ������� ��������
	SetAnimFlag(ANM_SHOW_EMPTY, "anm_show_empty");
	SetAnimFlag(ANM_HIDE_EMPTY, "anm_hide_empty");
	SetAnimFlag(ANM_IDLE_EMPTY, "anm_idle_empty");
	SetAnimFlag(ANM_AIM_EMPTY, "anm_idle_aim_empty");
	SetAnimFlag(ANM_BORE_EMPTY, "anm_bore_empty");
	SetAnimFlag(ANM_SHOT_EMPTY, "anm_shot_l");
	SetAnimFlag(ANM_SPRINT_EMPTY, "anm_idle_sprint_empty");
	SetAnimFlag(ANM_MOVING_EMPTY, "anm_idle_moving_empty");
	SetAnimFlag(ANM_RELOAD_EMPTY, "anm_reload_empty");
	SetAnimFlag(ANM_MISFIRE, "anm_reload_misfire");
	SetAnimFlag(ANM_SHOT_AIM, "anm_shots_when_aim");

	// Sounds
	m_sounds.LoadSound(section, "snd_draw", "sndShow", false, m_eSoundShow);
	m_sounds.LoadSound(section, "snd_holster", "sndHide", false, m_eSoundHide);

	m_layered_sounds.LoadSound(section, "snd_shoot", "sndShot", false, m_eSoundShot, st_Shooting);
	if (SoundExist(section, "snd_shoot_actor"))
	{
		//m_eSoundsShotFlags.set(ESoundsShotFlags::ssf_shoot_actor, true);
		m_layered_sounds.LoadSound(section, "snd_shoot_actor", "sndShotActor", false, m_eSoundShot, st_Shooting);
	}

	/*if (WeaponSoundExist(section, "snd_shoot_last", true))
		m_sounds.LoadSound(section, "snd_shoot_last", "sndShotLast", false, m_eSoundShot);*/

	m_sSndShotCurrent = "sndShot";

	m_sounds.LoadSound(section, "snd_empty", "sndEmptyClick", false, m_eSoundEmptyClick);
	m_sounds.LoadSound(section, "snd_reload", "sndReload", true, m_eSoundReload);
	m_sounds.LoadSound(section, "snd_reflect", "sndReflect", true, m_eSoundReflect);

	if (WeaponSoundExist(section, "snd_change_zoom", true))
		m_sounds.LoadSound(section, "snd_change_zoom", "sndChangeZoom", m_eSoundEmptyClick);

	// ����� �� ������ ���������
	if (WeaponSoundExist(section, "snd_close", true))
		m_sounds.LoadSound(section, "snd_close", "sndClose", false, m_eSoundClose);

	if (WeaponSoundExist(section, "snd_reload_empty", true))
		m_sounds.LoadSound(section, "snd_reload_empty", "sndReloadEmpty", true, m_eSoundReload);
	if (WeaponSoundExist(section, "snd_reload_misfire", true))
		m_sounds.LoadSound(section, "snd_reload_misfire", "sndReloadMisfire", true, m_eSoundReload);
	if (WeaponSoundExist(section, "snd_reload_jammed", true))
		m_sounds.LoadSound(section, "snd_reload_jammed", "sndReloadJammed", true, m_eSoundReload);
	if (WeaponSoundExist(section, "snd_pump_gun", true))
		m_sounds.LoadSound(section, "snd_pump_gun", "sndPumpGun", true, m_eSoundReload);


	// TODO: Completely remove magazine with bullets implementation from flamethrower

	// load ammo classes
	m_ammoTypes.clear();
	LPCSTR				S = pSettings->r_string(section, "ammo_class");
	if (S && S[0])
	{
		string128		_ammoItem;
		int				count = _GetItemCount(S);
		for (int it = 0; it < count; ++it)
		{
			_GetItem(S, it, _ammoItem);
			m_ammoTypes.emplace_back(_ammoItem);
		}
	}

	m_current_fuel_level = pSettings->r_float(section, "current_fuel_level");

	m_charge_speed = pSettings->r_float(section, "charge_speed");

	m_overheating_decrease_speed = pSettings->r_float(section, "overheating_decrease_speed");
	m_overheating_increase_speed_min = pSettings->r_float(section, "overheating_increase_speed_min");
	m_overheating_increase_speed_max = pSettings->r_float(section, "overheating_increase_speed_max");
	m_overheating_reset_level_max = pSettings->r_float(section, "overheating_reset_level_max");

	m_fuel_reduce_speed_charge = pSettings->r_float(section, "fuel_reduce_speed_charge");
	m_fuel_reduce_speed_shoot = pSettings->r_float(section, "fuel_reduce_speed_shoot");

	m_tick_damage = pSettings->r_float(section, "tick_damage");
	m_damage_decreasing = pSettings->r_float(section, "damage_decreasing");
		
	m_FlameTraceParticlesName = pSettings->r_string(section, "flame_particles");
	TraceManager.Load((xr_string(section)+"_trace").c_str());
	
	m_zoom_params.m_bZoomEnabled = false;
}

bool CFlamethrower::UseScopeTexture()
{
	return bScopeIsHasTexture;
}

void CFlamethrower::FireStart()
{
	u32 CurrentState = GetState();
	if (!IsMisfire())
	{

		bool is_empty = fabs(m_current_fuel_level) < std::numeric_limits<float>::epsilon();

		if (!is_empty)
		{
			if (!IsWorking() || AllowFireWhileWorking())
			{
				if (CurrentState == eReload || CurrentState == eShowing || CurrentState == eHiding || CurrentState == eMisfire)
					return;

				inherited::FireStart();
				R_ASSERT(H_Parent());
				SwitchState(eFire);
			}
		}
		else if (CurrentState == eIdle || CurrentState == eEmptyClick)
		{
			SwitchState(eEmptyClick);
		}
	}
	else
	{
		//misfire
		CGameObject* object = smart_cast<CGameObject*>(H_Parent());
		if (object)
			object->callback(GameObject::eOnWeaponJammed)(object->lua_game_object(), this->lua_game_object());

		if (smart_cast<CActor*>(this->H_Parent()) && (Level().CurrentViewEntity() == H_Parent()))
			CurrentGameUI()->AddCustomStatic("gun_jammed", true);

		OnEmptyClick();
	}
}

void CFlamethrower::FireEnd()
{
	inherited::FireEnd();

	const static bool isAutoreload = EngineExternal()[EEngineExternalGame::EnableAutoreload];
	if (isAutoreload && H_Parent())
	{
		bool is_empty = fabs(m_current_fuel_level) < std::numeric_limits<float>::epsilon();
		if (m_pInventory && is_empty && H_Parent()->cast_actor() && GetState() != eReload)
		{
			Reload();
		}
	}
}

void CFlamethrower::Reload()
{
	inherited::Reload();
	TryReload();
}

void CFlamethrower::OnMotionMark(u8 state, const motion_marks& M)
{
	inherited::OnMotionMark(state, M);
}

bool CFlamethrower::TryReload()
{
	if (m_pInventory)
	{
		if (IsGameTypeSingle() && ParentIsActor())
		{
			int	AC = GetSuitableAmmoTotal();
			Actor()->callback(GameObject::eWeaponNoAmmoAvailable)(lua_game_object(), AC);
		}

		AmmoCanister = m_pInventory->GetAny(m_ammoTypes[AmmoType.MagazineType].c_str())->cast_flame_canister();

		bool is_empty = fabs(m_current_fuel_level) < std::numeric_limits<float>::epsilon();
		if (IsMisfire() && !is_empty)
		{
			SetPending(true);
			return true;
		}

		if (AmmoCanister || unlimited_ammo())
		{
			SetPending(true);
			SwitchState(eReload);
			return				true;
		}
		else for (u8 i = 0; i < static_cast<u8>(m_ammoTypes.size()); ++i)
		{
			for (u32 i = 0; i < m_ammoTypes.size(); ++i)
			{
				AmmoCanister = smart_cast<CFlameCanister*>(m_pInventory->GetAny(*m_ammoTypes[i]));
				if (AmmoCanister)
				{
					m_set_next_ammoType_on_reload = i;
					SetPending(true);
					SwitchState(eReload);
					return				true;
				}
			}
		}

	}

	if (GetState() != eIdle)
	{
		SwitchState(eIdle);
	}

	return false;
}

bool CFlamethrower::IsAmmoAvailable()
{
	if (m_pInventory->GetAny(m_ammoTypes[AmmoType.MagazineType].c_str())->cast_flame_canister())
		return true;
	else
	{
		for (u32 i = 0; i < m_ammoTypes.size(); ++i)
		{
			if (m_pInventory->GetAny(m_ammoTypes[i].c_str())->cast_flame_canister())
				return true;
		}
	}
	return false;
}

void CFlamethrower::OnMagazineEmpty()
{
	if (IsGameTypeSingle() && ParentIsActor())
	{
		int AC = GetSuitableAmmoTotal();
		Actor()->callback(GameObject::eOnWeaponMagazineEmpty)(lua_game_object(), AC);
	}

	if (GetState() == eIdle)
	{
		OnEmptyClick();
		return;
	}

	inherited::OnMagazineEmpty();
}

void CFlamethrower::UnloadMagazine(bool spawn_ammo)
{

	if (IsGameTypeSingle() && ParentIsActor())
	{
		int AC = GetSuitableAmmoTotal();
		Actor()->callback(GameObject::eOnWeaponMagazineEmpty)(lua_game_object(), AC);
	}

	if (!spawn_ammo)
		return;

	if (!unlimited_ammo()) {
		SpawnFuelCanister(m_current_fuel_level, m_fuel_section_name.c_str());
	}

	SwitchState(eIdle);
}

int CFlamethrower::CheckAmmoBeforeReload(u8& v_ammoType)
{
	if (m_set_next_ammoType_on_reload != undefined_ammo_type)
	{
		v_ammoType = m_set_next_ammoType_on_reload;
		m_set_next_ammoType_on_reload	= undefined_ammo_type;
	}

	Msg("Ammo type in next reload : %d", v_ammoType);

	if (m_ammoTypes.size() <= v_ammoType)
	{
		Msg("Ammo type is wrong : %d", v_ammoType);
		return 0;
	}

	LPCSTR tmp_sect_name = m_ammoTypes[v_ammoType].c_str();

	if (!tmp_sect_name)
	{
		Msg("Sect name is wrong");
		return 0;
	}

	auto ammo = m_pInventory->GetAny(tmp_sect_name)->cast_flame_canister();

	if (!ammo && !m_bLockType)
	{
		for (u8 i = 0; i < static_cast<u8>(m_ammoTypes.size()); ++i)
		{
			//��������� ������� ���� ���������� �����
			ammo = m_pInventory->GetAny(m_ammoTypes[i].c_str())->cast_flame_canister();
			if (ammo)
			{
				v_ammoType = i;
				break;
			}
		}
	}

	Msg("Ammo type %d", v_ammoType);

	return GetAmmoCount(v_ammoType);

}

void CFlamethrower::ReloadMagazine()
{
	m_BriefInfo_CalcFrame = 0;

	//��������� ������ ��� �����������
	if (IsMisfire())	bMisfire = false;

	if (!m_bLockType)
	{
		AmmoCanister = nullptr;
	}

	if (!m_pInventory) return;

	if (m_set_next_ammoType_on_reload != undefined_ammo_type)
	{
		AmmoType.MagazineType = m_set_next_ammoType_on_reload;
		m_set_next_ammoType_on_reload = undefined_ammo_type;
	}

	if (!unlimited_ammo())
	{
		if (m_ammoTypes.size() <= AmmoType.MagazineType)
			return;

		LPCSTR tmp_sect_name = m_ammoTypes[AmmoType.MagazineType].c_str();

		if (!tmp_sect_name)
			return;

		xr_vector<PIItem> canisters;
		m_pInventory->GetAll(tmp_sect_name, canisters);

		//���������� ����� � ��������� ������� �������� ���� 
		for(int i = 0; i < canisters.size(); ++i)
		{
			AmmoCanister = smart_cast<CFlameCanister*>(canisters[i]);
			float Cond = AmmoCanister->GetCondition();
			Cond -= (1.0f - m_current_fuel_level);
			if (Cond <= 0.0)
			{
				m_current_fuel_level += AmmoCanister->GetCondition();
				AmmoCanister->SetCondition(0.0f);
				AmmoCanister->SetDropManual(TRUE);
			}
			else
			{
				AmmoCanister->SetCondition(Cond);
				m_current_fuel_level = 1.0f;
			}
		}
	}
}

void CFlamethrower::OnStateSwitch(u8 S)
{
	inherited::OnStateSwitch(S);
	switch (S)
	{
	case eIdle:
		switch2_Idle	();
		break;
	case eFire:
		switch2_Fire	();
		break;
	case eMisfire:
		if(H_Parent() && H_Parent()->cast_actor() && (Level().CurrentViewEntity() == H_Parent()))
			CurrentGameUI()->AddCustomStatic("gun_jammed", true);
		break;
	case eReload:
		if(H_Parent() && H_Parent()->cast_inventory_owner())
			m_sounds_enabled = H_Parent()->cast_inventory_owner()->CanPlayShHdRldSounds();
		switch2_Reload	();
		break;
	case eShowing:
		if (H_Parent() && H_Parent()->cast_inventory_owner())
			m_sounds_enabled = H_Parent()->cast_inventory_owner()->CanPlayShHdRldSounds();
		switch2_Showing	();
		break;
	case eHiding:
		if (H_Parent() && H_Parent()->cast_inventory_owner())
			m_sounds_enabled = H_Parent()->cast_inventory_owner()->CanPlayShHdRldSounds();
		switch2_Hiding	();
		break;
	case eHidden:
		switch2_Hidden	();
		break;
	case eEmptyClick:
		{
			switch2_Empty();
			break;
		}
	}
}


void CFlamethrower::UpdateCL()
{
	inherited::UpdateCL();
	float dt = Device.fTimeDelta;

	if (m_pFlameTraceParticles)
	{
		static Fvector v={0.f,0.f,0.f};
		Fmatrix pos;
		pos.set(get_ParticlesXFORM());
		pos.c.set(get_CurrentFirePoint());
		m_pFlameTraceParticles->UpdateParent(pos, v);
		m_pFlameTraceParticles->SpatialComponent->sphere.P = pos.c;
		m_pFlameTraceParticles->spatial_move();
	}
	
	if (GetNextState() == GetState())
	{
		switch (GetState())
		{
		case eShowing:
		case eHiding:
		case eReload:
		case eSprintStart:
		case eSprintEnd:
		case eIdle:
		{
			if (m_keep_charge && m_current_fuel_level > 0) {
				state_FireCharge(dt);
				//Msg("State: idle, keep charge. Charge - [%f], fuel - [%f], overheat - [%f]", m_current_charge, m_current_fuel_level, m_overheating_state);
			}
			else {
				fShotTimeCounter -= dt;
				clamp(fShotTimeCounter, 0.0f, flt_max);
				state_Idle(dt);
				//Msg("State: idle, uncharge. Charge - [%f], fuel - [%f], overheat - [%f]", m_current_charge, m_current_fuel_level, m_overheating_state);
			}
			break;
		}
		case eFire:
		{
			if(m_current_charge < 1.0f)
			{
				state_FireCharge(dt);
				//Msg("State: fire, charge. Charge - [%f], fuel - [%f], overheat - [%f]", m_current_charge, m_current_fuel_level, m_overheating_state);
			}
			else {
				if (!m_is_particle_active)
				{
					m_pFlameTraceParticles = Particles::Details::Create(m_FlameTraceParticlesName.c_str(), FALSE);
					m_pFlameTraceParticles->m_bAutoStop = true;
					m_pFlameTraceParticles->SetLiveUpdate(TRUE);
					m_pFlameTraceParticles->Play(false);
					m_is_particle_active = true;
				}
				state_Fire(dt);
				//Msg("State: fire, shoot. Charge - [%f], fuel - [%f], overheat - [%f]", m_current_charge, m_current_fuel_level, m_overheating_state);
			}
			break;
		}
		case eMisfire:		state_Misfire(dt);	break;
		case eHidden:		break;
		}
	}

	UpdateSounds();

	if (g_pGameLevel->bReady)
	{
		TraceManager.Update(dt);

		auto& overlapped = TraceManager.GetOverlapped();
		for (auto& elem : overlapped)
		{
			// TODO: implement burning
			auto Comp = elem->GetOrCreateComponent<TBurningComponent>();
			Comp->AddBurning(m_tick_damage, m_damage_decreasing, H_Parent() ? H_Parent()->cast_entity_alive() : nullptr);
		}
	}
}

void CFlamethrower::UpdateSounds()
{
	if (Device.dwFrame == dwUpdateSounds_Frame)  
		return;
	
	dwUpdateSounds_Frame = Device.dwFrame;

	Fvector P						= get_LastFP();

	if (Device.dwFrame % 3 == 0)
		m_sounds.SetPosition("sndShow", P);
	else if (Device.dwFrame % 3 == 1)
	{
		m_sounds.SetPosition("sndReload", P);
		m_sounds.SetPosition("sndHide", P);
	}
	else if (Device.dwFrame % 3 == 2)
	{
		if (m_sounds.FindSoundItem("sndReloadEmpty", false))
			m_sounds.SetPosition("sndReloadEmpty", P);
		if (m_sounds.FindSoundItem("sndReloadMis", false))
			m_sounds.SetPosition("sndReloadMis", P);
	}
}

void CFlamethrower::state_FireCharge(float dt)
{
	if(!IsWorking()&&!m_keep_charge)
	{
		StopShooting();
		return;
	}
	if (m_current_fuel_level > 0) {
		m_current_charge += m_charge_speed * dt;
		clamp(m_current_charge, 0.0f, 1.0f);
		m_current_fuel_level -= m_fuel_reduce_speed_charge * dt;
		clamp(m_current_fuel_level, 0.0f, 1.0f);
		m_overheating_state -= m_overheating_decrease_speed * dt;
		clamp(m_overheating_state, 0.0f, 1.0f);
		if (m_overheating_state <= m_overheating_reset_level_max)
		{
			m_is_overheated = false;
		}
	} else
	{
		OnMagazineEmpty();
		StopShooting();
	}
}

void CFlamethrower::state_Fire(float dt)
{
	if (m_current_fuel_level > 0)
	{
		VERIFY(fOneShotTime > 0.f);

		Fvector					p1, d;
		p1.set(get_LastFP());
		d.set(get_LastFD());

		if (!H_Parent()) return;
		if (smart_cast<CMPPlayersBag*>(H_Parent()) != nullptr)
		{
			Msg("! WARNING: state_Fire of object [%d][%s] while parent is CMPPlayerBag...", ID(), cNameSect().c_str());
			return;
		}

		CEntity* E = smart_cast<CEntity*>(H_Parent());
		E->g_fireParams(this, p1, d);

		if (!E->g_stateFire()) {
			StopShooting();
		}

		m_vStartPos = p1;
		m_vStartDir = d;

		//while (m_current_fuel_level && IsWorking()) {
			if (IsMisfire())
			{
				StopShooting();
				return;
			}
			OnShot();
			m_current_fuel_level -= (m_fuel_reduce_speed_charge + m_fuel_reduce_speed_shoot) * dt;
			clamp(m_current_fuel_level, 0.0f, 1.0f);
			m_overheating_state += m_overheating_increase_speed_max * dt;
			if (m_overheating_state > 1.f)
			{
				m_is_overheated = true;
			}
			clamp(m_current_fuel_level, 0.0f, 1.0f);

		UpdateSounds();
	}
	
	if (fabs(m_current_fuel_level) < std::numeric_limits<float>::epsilon()) {
		OnMagazineEmpty();
		StopShooting();
		return;
	}

	if(!IsWorking()){
		StopShooting();
	}
}

void CFlamethrower::state_Idle(float dt)
{
	m_current_charge = m_current_charge - m_charge_speed * dt;
	clamp(m_current_charge, 0.0f, 1.0f);
	m_overheating_state -= m_overheating_decrease_speed * dt;
	clamp(m_overheating_state, 0.0f, 1.0f);
	if (m_overheating_state <= m_overheating_reset_level_max)
	{
		m_is_overheated = false;
	}
}

void CFlamethrower::state_Misfire(float dt)
{
	VERIFY(false);
	OnEmptyClick();
	SwitchState(eIdle);

	bMisfire = true;

	UpdateSounds();
}

void CFlamethrower::state_MagEmpty(float dt)
{
}

void CFlamethrower::SetDefaults()
{
	CWeapon::SetDefaults();
}


void CFlamethrower::OnShot()
{
	TraceManager.LaunchTrace(m_vStartPos, m_vStartDir);

	if (ParentIsActor())
	{
		string128 sndName;
		xr_strconcat(sndName, m_sSndShotCurrent.c_str(), "Actor");
		if (m_layered_sounds.FindSoundItem(sndName, false))
		{
			m_layered_sounds.PlaySound(sndName, get_LastFP(), H_Root(), !!GetHUDmode(), false);
			return;
		}
	}

	string128 sndName;
	xr_strconcat(sndName, m_sSndShotCurrent.c_str());

	if (m_sounds.FindSoundItem(sndName, false)) {
		m_layered_sounds.PlaySound(sndName, get_LastFP(), H_Root(), !!GetHUDmode(), false);
	}
	else {
		m_layered_sounds.PlaySound(m_sSndShotCurrent.c_str(), get_LastFP(), H_Root(), !!GetHUDmode(), false);
	}

	CGameObject* object = H_Parent() ? H_Parent()->cast_game_object() : nullptr;
	if (object)
		object->callback(GameObject::eOnWeaponFired)(object->lua_game_object(), this->lua_game_object(), 0);
}

void CFlamethrower::StopShooting()
{
	inherited::StopShooting();

	if (I_ASSERT(m_pFlameTraceParticles))
	{
		m_pFlameTraceParticles->Stop();
		m_is_particle_active = false;
	}
	SwitchState(eIdle);

	TraceManager.OnShootingEnd();
}


void CFlamethrower::OnEmptyClick()
{
	PlaySound("sndEmptyClick", get_LastFP());
}

void CFlamethrower::OnAnimationEnd(u8 state)
{
	switch (state)
	{
	case eReload:
	{
		ReloadMagazine();
		SwitchState(eIdle);

		break;// End of reload animation
	}
	case eHiding:	SwitchState(eHidden);   break;	// End of Hide
	case eShowing:	SwitchState(eIdle);		break;	// End of Show
	case eIdle:		switch2_Idle();			break;  // Keep showing idle
	}
	inherited::OnAnimationEnd(state);
}

void CFlamethrower::switch2_Idle()
{
	if (m_fOldBulletSpeed != 0.f)
		SetBulletSpeed(m_fOldBulletSpeed);

	SetPending(FALSE);
	PlayAnimIdle();
}

#ifdef DEBUG
#include "ai\stalker\ai_stalker.h"
#endif
void CFlamethrower::switch2_Fire()
{
	m_bFireSingleShot = true;

	if ((OnClient() || Level().IsDemoPlay()) && !IsWorking())
	{
		FireStart();
	}

}

void CFlamethrower::switch2_Empty()
{
	OnZoomOut();

	if (m_bAutoreloadEnabled)
	{
		if (!TryReload())
		{
			OnEmptyClick();
		}
		else
		{
			inherited::FireEnd();
		}
	}
	else
	{
		OnEmptyClick();
	}
}
void CFlamethrower::PlayReloadSound()
{
	if (m_sounds_enabled)
	{
		PlaySound("sndReload", get_LastFP());
	}
}

void CFlamethrower::switch2_Reload()
{
	CWeapon::FireEnd();

	PlayReloadSound();
	PlayAnimReload();
	SetPending(TRUE);
}
void CFlamethrower::switch2_Hiding()
{
	OnZoomOut();
	CWeapon::FireEnd();

	if (m_sounds_enabled)
	{
		PlaySound("sndHide", get_LastFP());
	}

	PlayAnimHide();
	SetPending(TRUE);
}

void CFlamethrower::switch2_Hidden()
{
	CWeapon::FireEnd();

	StopCurrentAnimWithoutCallback();

	signal_HideComplete();
	RemoveShotEffector();
}

void CFlamethrower::switch2_Showing()
{
	if (m_sounds_enabled)
	{
		PlaySound("sndShow", get_LastFP());
	}

	SetPending(TRUE);
	PlayAnimShow();
}

#include "CustomDetector.h"

bool CFlamethrower::Action(u16 cmd, u32 flags)
{
	if (inherited::Action(cmd, flags)) return true;

	if (IsPending())
	{
		return false;
	}

	switch (cmd)
	{
	case kWPN_RELOAD:
	{
		if (flags & CMD_START) {
			if (m_current_fuel_level < 1.0 || IsMisfire())
			{
				Reload();
			}
		}
		return true;
	}
	case kWPN_ZOOM: {
		CGameObject* object = H_Parent() ? H_Parent()->cast_game_object() : nullptr;
		if (flags & CMD_START) {
			if (object) {
				object->callback(GameObject::eOnWeaponZoomIn)(object->lua_game_object(), this->lua_game_object());
			}
			m_keep_charge = true;
		}
		if (flags & CMD_STOP) {
			if (object) {
				object->callback(GameObject::eOnWeaponZoomOut)(object->lua_game_object(), this->lua_game_object());
			}
			m_keep_charge = false;
		}

		return true;
	}
	}
	return false;
}

void CFlamethrower::InitAddons()
{
	m_zoom_params.m_fIronSightZoomFactor = READ_IF_EXISTS(pSettings, r_float, cNameSect(), "ironsight_zoom_factor", 50.0f);

	SetAnimFlag(ANM_SHOT_AIM, "anm_shots_when_aim");
	SetAnimFlag(ANM_SHOT_AIM_GL, "anm_shots_w_gl_when_aim");


	if (IsScopeAttached())
	{
		if ( m_eScopeStatus == ALife::eAddonAttachable )
		{
			LoadCurrentScopeParams(GetScopeName().c_str());
		}
	}
	else
	{
		if ( m_UIScope )
		{
			xr_delete( m_UIScope );
		}
		
		if ( IsZoomEnabled() )
		{
			m_zoom_params.m_fIronSightZoomFactor = pSettings->r_float( cNameSect(), "scope_zoom_factor" );
		}
	}

	{
		m_sSndShotCurrent = "sndShot";

		//��������� �� ��������
		LoadLights(*cNameSect(), "");
	}

	inherited::InitAddons();
}

void CFlamethrower::PlayAnimShow()
{
	VERIFY(GetState()==eShowing);
	PlayHUDMotion(SetCurrentStateAnimation("anm_show"), EHudMixType::eNoMix, GetState());
}

void CFlamethrower::PlayAnimHide()
{
	VERIFY(GetState() == eHiding);
	PlayHUDMotion(SetCurrentStateAnimation("anm_hide"), EHudMixType::eNoMix, GetState());
}

void CFlamethrower::PlayAnimBore()
{
	inherited::PlayAnimBore();
}

shared_str CFlamethrower::SetCurrentReloadAnimation()
{
	shared_str anim = "anm_reload";
	return anim;
}

shared_str CFlamethrower::SetCurrentStateAnimation(const shared_str& first_name)
{
	shared_str anim = first_name;
	return anim;
}

shared_str CFlamethrower::SetCurrentShootAnimation()
{
	shared_str anim = HudAnimationExist("anm_shoot") ? "anm_shoot" : "anm_shots";
	return anim;
}

shared_str CFlamethrower::SetCurrentAimAnimation()
{
	shared_str anim = "anm_idle_aim";

	if (IsGrenadeLauncherAttached())
	{
		//Hack for original weapon configs
		anim = IsGrenadeMode() && HudAnimationExist("anm_idle_g_aim") ? "anm_idle_g_aim" : (HudAnimationExist("anm_idle_w_gl_aim") ? "anm_idle_w_gl_aim" : anim);
	}

	if (CActor* actor = H_Parent()->cast_actor())
	{
		u32 state = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
		if (state & ACTOR_DEFS::EMoveCommand::mcAnyMove)
		{
			if (IsScopeAttached())
			{
				AddSuffixName(anim, "_scope", "_moving");
			}
			else
			{
				AddSuffixName(anim, "_moving");
			}

			if (state & ACTOR_DEFS::EMoveCommand::mcFwd)
			{
				AddSuffixName(anim, "_moving", "_forward");
			}
			else if (state & ACTOR_DEFS::EMoveCommand::mcBack)
			{
				AddSuffixName(anim, "_moving", "_back");
			}

			if (state & ACTOR_DEFS::EMoveCommand::mcLStrafe)
			{
				AddSuffixName(anim, "_moving", "_left");
			}
			else if (state & ACTOR_DEFS::EMoveCommand::mcRStrafe)
			{
				AddSuffixName(anim, "_moving", "_right");
			}
		}
	}

	return SetCurrentStateAnimation(anim);
}

void CFlamethrower::PlayAnimIdle()
{
	if (GetState() != eIdle)
		return;

	if (m_bIsAimStarted && HudAnimationExist("anm_idle_aim_end"))
	{
		m_bIsAimStarted = false;
		PlayHUDMotion(SetCurrentStateAnimation("anm_idle_aim_end"), EHudMixType::eNoMix, GetState());
		return;
	}

	if (TryPlayAnimIdle())
	{
		return;
	}

	shared_str new_name = SetCurrentIdleAnimation();

	PlayHUDMotion(SetCurrentStateAnimation(new_name), EHudMixType::eNoMix, GetState());
}

void CFlamethrower::PlayAnimReload()
{
	VERIFY(GetState() == eReload);

	PlayHUDMotion(SetCurrentReloadAnimation(), EHudMixType::eNoMix, GetState());
}

void CFlamethrower::PlayAnimShoot()
{
	VERIFY(GetState() == eFire);
	PlayHUDMotion(SetCurrentShootAnimation(), EHudMixType::eNoMix, GetState());
}

void CFlamethrower::OnZoomIn()
{
	inherited::OnZoomIn();

	if (GetState() == eIdle)
		PlayAnimIdle();

	//Alundaio: callback not sure why vs2013 gives error, it's fine
	CGameObject* object = smart_cast<CGameObject*>(H_Parent());

	if (object)
		object->callback(GameObject::eOnWeaponZoomIn)(object->lua_game_object(), this->lua_game_object());
	//-Alundaio

	m_keep_charge = true;
}
void CFlamethrower::OnZoomOut()
{
	if (!IsZoomed())
		return;

	inherited::OnZoomOut();

	if (GetState() == eIdle)
		PlayAnimIdle();

	//Alundaio
	CGameObject* object = H_Parent() ? H_Parent()->cast_game_object() : nullptr;
	if (object)
		object->callback(GameObject::eOnWeaponZoomOut)(object->lua_game_object(), this->lua_game_object());
	//-Alundaio

	m_keep_charge = false;

}

void	CFlamethrower::OnH_A_Chield()
{
	inherited::OnH_A_Chield();
};

float	CFlamethrower::GetWeaponDeterioration()
{
	return conditionDecreasePerQueueShot;
};

void CFlamethrower::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	save_data(m_is_overheated, output_packet);
	save_data(m_overheating_state, output_packet);
	save_data(m_current_charge, output_packet);
	save_data(m_current_fuel_level, output_packet);
	save_data(m_fuel_section_name, output_packet);
	TraceManager.save(output_packet);
}

void CFlamethrower::load(IReader& input_packet)
{
	inherited::load(input_packet);
	load_data(m_is_overheated, input_packet);
	load_data(m_overheating_state, input_packet);
	load_data(m_current_charge, input_packet);
	load_data(m_current_fuel_level, input_packet);
	load_data(m_fuel_section_name, input_packet);
	TraceManager.load(input_packet);
}

void CFlamethrower::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object, "CFlamethrower")
	{
		inherited::Serialize(Object);
		Object << m_is_overheated << m_overheating_state << m_current_charge << m_current_fuel_level << m_fuel_section_name << TraceManager;
	}
}

void CFlamethrower::SpawnFuelCanister(float Condition, LPCSTR ammoSect, ALife::_OBJECT_ID ParentID)
{
	if (OnClient())	return;
	m_bAmmoWasSpawned = true;

	int l_type = 0;
	l_type %= m_ammoTypes.size();

	if (!ammoSect) ammoSect = m_ammoTypes[l_type].c_str();

	++l_type;
	l_type %= m_ammoTypes.size();

	CSE_Abstract* D = F_entity_Create(ammoSect);

	{
		CSE_ALifeItemFuel* l_pA = smart_cast<CSE_ALifeItemFuel*>(D);
		R_ASSERT(l_pA);
		D->s_name = ammoSect;
		D->set_name_replace("");
		D->s_RP = 0xff;
		D->ID = ALife::INVALID_OBJECT_ID;
		if (ParentID == ALife::INVALID_OBJECT_ID)
		{
			D->ID_Parent = H_Parent()->ID();
		}
		else
		{
			D->ID_Parent = ParentID;
		}

		D->ID_Phantom = ALife::INVALID_OBJECT_ID;
		D->s_flags.assign(M_SPAWN_OBJECT_LOCAL);
		D->RespawnTime = 0;
		l_pA->m_tNodeID = g_dedicated_server ? static_cast<u32>(-1) : ai_location().level_vertex_id();

		l_pA->m_fCondition = Condition;
	}
	F_entity_Destroy(D);

}

void CFlamethrower::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);
}

void CFlamethrower::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
}

bool CFlamethrower::GetBriefInfo(II_BriefInfo& info)
{
	VERIFY(m_pInventory);
	string32	int_str, fire_mode, ammo = "";

	float ae = m_current_fuel_level * 100.0f;
	xr_sprintf(int_str, "%.1f%%", ae);
	info.cur_ammo = int_str;
	info.fire_mode._set("");

	info.fire_mode = "";

	info.grenade = "";

	u32 at_size = m_ammoTypes.size();
	if (unlimited_ammo() || at_size == 0)
	{
		info.fmj_ammo._set("--");
		info.ap_ammo._set("--");
	}
	else
	{

		xr_sprintf(ammo, "%.1f%%", m_current_charge * 100.0f);
		info.fmj_ammo._set(ammo);
		xr_sprintf(ammo, "%.1f%%", m_overheating_state * 100.0f);
		info.ap_ammo._set(ammo);
	}

	if (ae != 0 && m_magazine.size() != 0)
	{
		LPCSTR ammo_type = m_ammoTypes[m_magazine.back().m_LocalAmmoType].c_str();
		info.name = CStringTable().translate(pSettings->r_string(ammo_type, "inv_name_short"));
		info.icon = ammo_type;
	}
	else
	{
		LPCSTR ammo_type = m_ammoTypes[AmmoType.MagazineType].c_str();
		info.name = CStringTable().translate(pSettings->r_string(ammo_type, "inv_name_short"));
		info.icon = ammo_type;
	}
	return true;
}

bool CFlamethrower::IsMisfire() const
{
	return m_is_overheated;
}

bool CFlamethrower::install_upgrade_impl(LPCSTR section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	LPCSTR str;

	// sounds (name of the sound, volume (0.0 - 1.0), delay (sec))
	bool result2 = process_if_exists_set(section, "snd_draw", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_draw", "sndShow", false, m_eSoundShow); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_holster", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_holster", "sndHide", false, m_eSoundHide); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_shoot", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_shoot", "sndShot", false, m_eSoundShot); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_empty", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_empty", "sndEmptyClick", false, m_eSoundEmptyClick); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_reload", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_reload", "sndReload", true, m_eSoundReload); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_reflect", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_reflect", "sndReflect", false, m_eSoundReflect); }
	result |= result2;

	// fov for zoom mode
	result |= process_if_exists(section, "ironsight_zoom_factor", m_zoom_params.m_fIronSightZoomFactor, test);

	if (IsScopeAttached())
	{
		result |= process_if_exists(section, "scope_zoom_factor", m_zoom_params.m_fScopeZoomFactor, test);
	}
	else if (IsZoomEnabled())
	{
		result |= process_if_exists(section, "scope_zoom_factor", m_zoom_params.m_fIronSightZoomFactor, test);
	}

	return result;
}

void CFlamethrower::FireBullet(const Fvector& pos,
	const Fvector& shot_dir,
	float fire_disp,
	const CCartridge& cartridge,
	ALife::_OBJECT_ID parent_id,
	ALife::_OBJECT_ID weapon_id,
	bool send_hit)
{
	inherited::FireBullet(pos, shot_dir, fire_disp, cartridge, parent_id, weapon_id, send_hit);
}

// AVO: for custom added sounds check if sound exists
bool CFlamethrower::WeaponSoundExist(LPCSTR section, LPCSTR sound_name, bool log) const
{
	const char* str;
	bool sec_exist = process_if_exists_set(section, sound_name, str, true);
	if (sec_exist)
		return true;
#ifdef DEBUG
	if (log)
		Msg("~ [WARNING] ------ Sound [%s] does not exist in [%s]", sound_name, section);
#endif
	return false;
}