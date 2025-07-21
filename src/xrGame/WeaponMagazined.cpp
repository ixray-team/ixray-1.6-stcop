#include "StdAfx.h"
#include "pch_script.h"

#include "WeaponMagazined.h"
#include "Actor.h"
#include "Scope.h"
#include "Silencer.h"
#include "GrenadeLauncher.h"
#include "Inventory.h"
#include "InventoryOwner.h"
#include "xrServer_Objects_ALife_Items.h"
#include "ActorEffector.h"
#include "EffectorZoomInertion.h"
#include "../xrEngine/xr_level_controller.h"
#include "UIGameCustom.h"
#include "object_broker.h"
#include "../xrEngine/string_table.h"
#include "MPPlayersBag.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "game_object_space.h"
#include "script_game_object.h"
#include "Actor_Flags.h"
#include "player_hud.h"
#include "CustomDetector.h"
#include "WeaponRPG7.h"
#if USE_OLD_OBJECT_PLANNER
#include "Legacy/object_handler_planner.h"
#endif

ENGINE_API bool	g_dedicated_server;

CUIXml* pWpnScopeXml = nullptr;

extern void createWpnScopeXML();

CWeaponMagazined::CWeaponMagazined(ESoundTypes eSoundType) : CWeapon()
{
	m_eSoundShow				= ESoundTypes(SOUND_TYPE_ITEM_TAKING | eSoundType);
	m_eSoundHide				= ESoundTypes(SOUND_TYPE_ITEM_HIDING | eSoundType);
	m_eSoundShot				= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING | eSoundType);
	m_eSoundEmptyClick			= ESoundTypes(SOUND_TYPE_WEAPON_EMPTY_CLICKING | eSoundType);
	m_eSoundReload				= ESoundTypes(SOUND_TYPE_WEAPON_RECHARGING | eSoundType);
	m_eSoundAim 				= ESoundTypes(SOUND_TYPE_WEAPON | eSoundType);
	m_eSoundAimOut 				= ESoundTypes(SOUND_TYPE_WEAPON | eSoundType);
	
	m_sounds_enabled			= true;
	
	m_sSndShotCurrent			= "sndShot";
	m_sSilencerFlameParticles	= m_sSilencerSmokeParticles = nullptr;

	m_bFireSingleShot			= false;
	m_iShotNum					= 0;
	m_fOldBulletSpeed			= 0;
	m_iQueueSize				= WEAPON_ININITE_QUEUE;
	m_bLockType					= false;
	bMisfireReload				= false;
}

CWeaponMagazined::~CWeaponMagazined()
{}

void CWeaponMagazined::net_Destroy()
{
	inherited::net_Destroy();
}

void CWeaponMagazined::Load(LPCSTR section)
{
	inherited::Load(section);

	//звуки и партиклы глушителя, еслит такой есть
	if (m_eSilencerStatus == ALife::eAddonAttachable || m_eSilencerStatus == ALife::eAddonPermanent)
	{
		if (pSettings->line_exist(section, "silencer_flame_particles"))
			m_sSilencerFlameParticles = pSettings->r_string(section, "silencer_flame_particles");
		if (pSettings->line_exist(section, "silencer_smoke_particles"))
			m_sSilencerSmokeParticles = pSettings->r_string(section, "silencer_smoke_particles");
	}

	m_iBaseDispersionedBulletsCount = READ_IF_EXISTS(pSettings, r_u8, section, "base_dispersioned_bullets_count", 0);
	m_fBaseDispersionedBulletsSpeed = READ_IF_EXISTS(pSettings, r_float, section, "base_dispersioned_bullets_speed", m_fStartBulletSpeed);
	m_fBaseDispersionedBulletsTimeDelta = READ_IF_EXISTS(pSettings, r_float, section, "base_dispersioned_bullets_time_delta", 0.0f);
	m_fSingleShootsTimeDelta = READ_IF_EXISTS(pSettings, r_float, section, "singleshoots_time_delta", 0.0f);

	if (pSettings->line_exist(section, "fire_modes"))
	{
		shared_str FireModesList = pSettings->r_string(section, "fire_modes");
		s8 ModesCount = _GetItemCount(FireModesList.c_str());
		m_aFireModes.clear();

		for (s8 i = 0; i < ModesCount; i++)
		{
			string16 sItem = {};
			_GetItem(FireModesList.c_str(), i, sItem);
			m_aFireModes.push_back(static_cast<s8>(atoi(sItem)));
		}

		m_iCurFireMode = ModesCount - 1;
	}
	else
	{
		m_aFireModes.push_back(1);
		m_iCurFireMode = 1;
	}

	LoadSilencerKoeffs();
}

void CWeaponMagazined::LoadSounds(LPCSTR section)
{
	inherited::LoadSounds(section);

	m_sounds.LoadSound(section, "snd_draw", "sndShow", false, m_eSoundShow);
	m_sounds.LoadSound(section, "snd_holster", "sndHide", false, m_eSoundHide);

	m_layered_sounds.LoadSound(section, "snd_shoot", "sndShot", false, m_eSoundShot);
	if (SoundExist(section, "snd_shoot_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor, TRUE);
		m_layered_sounds.LoadSound(section, "snd_shoot_actor", "sndShotActor", false, m_eSoundShot);
	}

	m_layered_sounds.LoadSound(section, "snd_silncer_shot", "sndSilencerShot", false, m_eSoundShot);
	if (SoundExist(section, "snd_silncer_shot_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_sil, TRUE);
		m_layered_sounds.LoadSound(section, "snd_silncer_shot_actor", "sndSilencerShotActor", false, m_eSoundShot);
	}

	if (SoundExist(section, "snd_shot_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_last, TRUE);
		m_layered_sounds.LoadSound(section, "snd_shot_last", "sndShotLast", false, m_eSoundShot);
	}

	if (SoundExist(section, "snd_shot_last_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_last, TRUE);
		m_layered_sounds.LoadSound(section, "snd_shot_last_actor", "sndShotLastActor", false, m_eSoundShot);
	}

	if (SoundExist(section, "snd_silencer_shot_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_last_sil, TRUE);
		m_layered_sounds.LoadSound(section, "snd_silencer_shot_last", "sndSilencerShotLast", false, m_eSoundShot);
	}

	if (SoundExist(section, "snd_silencer_shot_last_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_last_sil, TRUE);
		m_layered_sounds.LoadSound(section, "snd_silencer_shot_last_actor", "sndSilencerShotLastActor", false, m_eSoundShot);
	}

	m_sounds.LoadSound(section, "snd_empty", "sndEmptyClick", false, m_eSoundEmptyClick);
	m_sounds.LoadSound(section, "snd_reload", "sndReload", true, m_eSoundReload);

	if (SoundExist(section, "snd_reload_empty"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_empty, TRUE);
		m_sounds.LoadSound(section, "snd_reload_empty", "sndReloadEmpty", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam, TRUE);
		m_sounds.LoadSound(section, "snd_reload_misfire", "sndReloadMis", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam, TRUE);
		m_sounds.LoadSound(section, "snd_reload_jammed", "sndReloadMis", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last, TRUE);
		m_sounds.LoadSound(section, "snd_reload_misfire_last", "sndReloadMisLast", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last, TRUE);
		m_sounds.LoadSound(section, "snd_reload_jammed_last", "sndReloadMisLast", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_det, TRUE);
		m_sounds.LoadSound(section, "snd_reload_misfire_detector", "sndReloadMisDet", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_det, TRUE);
		m_sounds.LoadSound(section, "snd_reload_jammed_detector", "sndReloadMisDet", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire_last_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last_det, TRUE);
		m_sounds.LoadSound(section, "snd_reload_misfire_last_detector", "sndReloadMisLastDet", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed_last_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last_det, TRUE);
		m_sounds.LoadSound(section, "snd_reload_jammed_last_detector", "sndReloadMisLastDet", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_changecartridgetype"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_change, TRUE);
		m_sounds.LoadSound(section, "snd_changecartridgetype", "sndChangeCartridgeType", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_aim_start"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_aim_start, TRUE);
		m_sounds.LoadSound(section, "snd_aim_start", "sndAimStart", true, m_eSoundAim);
	}

	if (SoundExist(section, "snd_aim_end"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_aim_end, TRUE);
		m_sounds.LoadSound(section, "snd_aim_end", "sndAimEnd", true, m_eSoundAimOut);
	}

	if (SoundExist(section, "snd_changefiremode"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_changefiremode, TRUE);
		m_sounds.LoadSound(section, "snd_changefiremode", "sndChangeFiremode", true, m_eSoundEmptyClick);
	}
}

void CWeaponMagazined::FireStart()
{
	u32 CurrentState = GetState();
	if (!IsMisfire())
	{

		bool is_empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;

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
		else if (CurrentState == eIdle || CurrentState == eEmptyClick && !m_bBlockEmptyClick)
		{
			SwitchState(eEmptyClick);
		}
	}
	else if (CurrentState == eIdle || CurrentState == eEmptyClick && !m_bBlockEmptyClick)
	{
		if (H_Parent())
		{
			if (CGameObject* object = H_Parent()->cast_game_object())
			{
				object->callback(GameObject::eOnWeaponJammed)(object->lua_game_object(), this->lua_game_object());
			}

			if (H_Parent()->cast_actor() && (Level().CurrentViewEntity() == H_Parent()))
				CurrentGameUI()->AddCustomStatic("gun_jammed", true);
		}

		SwitchState(eEmptyClick);
	}
}

void CWeaponMagazined::FireEnd() 
{
	inherited::FireEnd();

	const static bool isAutoreload = EngineExternal()[EEngineExternalGame::EnableAutoreload];
	if (isAutoreload && H_Parent())
	{
		bool is_empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;
		if (m_pInventory && is_empty && H_Parent()->cast_actor() && GetState() != eReload)
		{
			Reload();
		}
	}
}

void CWeaponMagazined::Reload() 
{
	if (ParentIsActor() && Actor()->GetDetector() && Actor()->GetDetector()->GetState() != CCustomDetector::eIdle)
		return;

	inherited::Reload();
	TryReload();
}

bool CWeaponMagazined::TryReload() 
{
	if(m_pInventory) 
	{
		if(IsGameTypeSingle() && ParentIsActor())
		{
			int	AC					= GetSuitableAmmoTotal();
			Actor()->callback(GameObject::eWeaponNoAmmoAvailable)(lua_game_object(), AC);
		}

		m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny( m_ammoTypes[m_ammoType].c_str() ));
		
		if(IsMisfire() && iAmmoElapsed)
		{
			SetPending			(TRUE);
			SwitchState			(eReload); 
			return				true;
		}

		if(m_pCurrentAmmo || unlimited_ammo())  
		{
			SetPending			(TRUE);
			SwitchState			(eReload); 
			return				true;
		} 
		else for(u8 i = 0; i < u8(m_ammoTypes.size()); ++i) 
		{
			m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny( m_ammoTypes[i].c_str() ));
			if(m_pCurrentAmmo) 
			{ 
				m_set_next_ammoType_on_reload = i;
				SetPending			(TRUE);
				SwitchState			(eReload);
				return				true;
			}
		}

	}
	
	if(GetState()!=eIdle)
		SwitchState(eIdle);

	return false;
}

bool CWeaponMagazined::IsAmmoAvailable()
{
	if (smart_cast<CWeaponAmmo*>(m_pInventory->GetAny( m_ammoTypes[m_ammoType].c_str() )))
		return	(true);
	else
		for(u32 i = 0; i < m_ammoTypes.size(); ++i)
			if (smart_cast<CWeaponAmmo*>(m_pInventory->GetAny( m_ammoTypes[i].c_str() )))
				return	(true);
	return		(false);
}

void CWeaponMagazined::UnloadMagazine(bool spawn_ammo)
{
	xr_map<LPCSTR, u16> l_ammo;
	
	while(!m_magazine.empty()) 
	{
		CCartridge &l_cartridge = m_magazine.back();
		xr_map<LPCSTR, u16>::iterator l_it;
		for(l_it = l_ammo.begin(); l_ammo.end() != l_it; ++l_it) 
		{
            if(!xr_strcmp(*l_cartridge.m_ammoSect, l_it->first)) 
            { 
				 ++(l_it->second); 
				 break; 
			}
		}

		if(l_it == l_ammo.end()) l_ammo[*l_cartridge.m_ammoSect] = 1;
		m_magazine.pop_back(); 
		--iAmmoElapsed;
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	if (ParentIsActor())
	{
		int	AC = GetSuitableAmmoTotal();
		Actor()->callback(GameObject::eOnWeaponMagazineEmpty)(lua_game_object(), AC);
	}

	if (!spawn_ammo)
		return;

	xr_map<LPCSTR, u16>::iterator l_it;
	for(l_it = l_ammo.begin(); l_ammo.end() != l_it; ++l_it) 
	{
		if(m_pInventory)
		{
			CWeaponAmmo *l_pA = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny(l_it->first));
			if(l_pA) 
			{
				u16 l_free = l_pA->m_boxSize - l_pA->m_boxCurr;
				l_pA->m_boxCurr = l_pA->m_boxCurr + (l_free < l_it->second ? l_free : l_it->second);
				l_it->second = l_it->second - (l_free < l_it->second ? l_free : l_it->second);
			}
		}
		if(l_it->second && !unlimited_ammo()) SpawnAmmo(l_it->second, l_it->first);
	}

	if (GetState() == eIdle)
		SwitchState(eIdle);

	if (!IsGrenadeMode())
	{
		UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, m_ammoType);
	}
}

void CWeaponMagazined::ReloadMagazine() 
{
	m_BriefInfo_CalcFrame = 0;	

	//устранить осечку при перезарядке
	if(IsMisfire())	bMisfire = false;
	
	if (!m_bLockType)
	{
		m_pCurrentAmmo		= nullptr;
	}
	
	if (!m_pInventory) return;

	if ( m_set_next_ammoType_on_reload != undefined_ammo_type )
	{
		m_ammoType						= m_set_next_ammoType_on_reload;
		m_set_next_ammoType_on_reload	= undefined_ammo_type;
	}
	
	if(!unlimited_ammo()) 
	{
		if (m_ammoTypes.size() <= m_ammoType)
			return;
		
		LPCSTR tmp_sect_name = m_ammoTypes[m_ammoType].c_str();
		
		if (!tmp_sect_name)
			return;

		//попытаться найти в инвентаре патроны текущего типа 
		m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny(tmp_sect_name));
		
		if(!m_pCurrentAmmo && !m_bLockType) 
		{
			for(u8 i = 0; i < u8(m_ammoTypes.size()); ++i) 
			{
				//проверить патроны всех подходящих типов
				m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny( m_ammoTypes[i].c_str() ));
				if(m_pCurrentAmmo) 
				{ 
					m_ammoType = i;
					break; 
				}
			}
		}
	}

	//нет патронов для перезарядки
	if(!m_pCurrentAmmo && !unlimited_ammo() ) return;

	//разрядить магазин, если загружаем патронами другого типа
	if(!m_bLockType && !m_magazine.empty() && 
		(!m_pCurrentAmmo || xr_strcmp(m_pCurrentAmmo->cNameSect(), 
					 *m_magazine.back().m_ammoSect)))
		UnloadMagazine();

	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	if (m_DefaultCartridge.m_LocalAmmoType != m_ammoType)
		m_DefaultCartridge.Load( m_ammoTypes[m_ammoType].c_str(), m_ammoType );

	CCartridge l_cartridge = m_DefaultCartridge;
	while(iAmmoElapsed < iMagazineSize)
	{
		if (!unlimited_ammo())
		{
			if (!m_pCurrentAmmo->Get(l_cartridge)) break;
		}
		++iAmmoElapsed;
		l_cartridge.m_LocalAmmoType = m_ammoType;
		m_magazine.push_back(l_cartridge);
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	//выкинуть коробку патронов, если она пустая
	if(m_pCurrentAmmo && !m_pCurrentAmmo->m_boxCurr && OnServer()) 
		m_pCurrentAmmo->SetDropManual(TRUE);

	if(iMagazineSize > iAmmoElapsed) 
	{ 
		m_bLockType = true; 
		ReloadMagazine(); 
		m_bLockType = false; 
	}

	if (!IsGrenadeMode())
	{
		UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, m_ammoType);
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());
}

bool CWeaponMagazined::HaveCartridgeInInventory(u8 cnt)
{
	if (unlimited_ammo())	return true;
	if (!m_pInventory)		return false;

	u32 ac = GetAmmoCount(m_ammoType);
	if (ac < cnt)
	{
		for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
		{
			if (m_ammoType == i) continue;
			ac += GetAmmoCount(i);
			if (ac >= cnt)
			{
				m_ammoType = i;
				break;
			}
		}
	}
	return ac >= cnt;
}


u8 CWeaponMagazined::AddCartridge(u8 cnt)
{
	if (IsMisfire())	bMisfire = false;

	if (m_set_next_ammoType_on_reload != undefined_ammo_type)
	{
		m_ammoType = m_set_next_ammoType_on_reload;
		m_set_next_ammoType_on_reload = undefined_ammo_type;
	}

	if (!HaveCartridgeInInventory(1))
		return 0;

	m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny(m_ammoTypes[m_ammoType].c_str()));
	VERIFY((u32)iAmmoElapsed == m_magazine.size());


	if (m_DefaultCartridge.m_LocalAmmoType != m_ammoType)
		m_DefaultCartridge.Load(m_ammoTypes[m_ammoType].c_str(), m_ammoType);
	CCartridge l_cartridge = m_DefaultCartridge;
	while (cnt)
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
	if (m_pCurrentAmmo && !m_pCurrentAmmo->m_boxCurr && OnServer())
		m_pCurrentAmmo->SetDropManual(TRUE);

	GiveAmmoFromMagToChamber();

	return cnt;
}

void CWeaponMagazined::OnStateSwitch	(u32 S)
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
	case eSwitchMode:
	{
		switch2_FireMode();
		break;
	}
	case eEmptyClick:
	{
		switch2_Empty();
		break;
	}
	}
}

void CWeaponMagazined::UpdateCL			()
{
	PROF_EVENT("CWeaponMagazined::UpdateCL")
	inherited::UpdateCL	();
	float dt = Device.fTimeDelta;

	//когда происходит апдейт состояния оружия
	//ничего другого не делать
	if(GetNextState() == GetState())
	{
		switch (GetState())
		{
		case eShowing:
		case eHiding:
		case eReload:
		case eIdle:
		case eSwitchMode:
		case eEmptyClick:
			{
				fShotTimeCounter	-=	dt;
				clamp				(fShotTimeCounter, 0.0f, flt_max);
			}break;
		case eFire:			
			{
				if (m_bAmmoInChamber && !IsGrenadeMode())
					state_FireChamber(dt);
				else
					state_Fire		(dt);
			}break;
		case eMisfire:		state_Misfire	(dt);	break;
		case eHidden:		break;
		}
	}

	UpdateSounds		();
}

void CWeaponMagazined::UpdateSounds	()
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
		if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_empty))
			m_sounds.SetPosition("sndReloadEmpty", P);
		if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_jam))
			m_sounds.SetPosition("sndReloadMis", P);
	}
}

void CWeaponMagazined::state_Fire(float dt)
{
	if(iAmmoElapsed > 0)
	{
		VERIFY(fOneShotTime>0.f);

		Fvector					p1, d; 
		p1.set(get_LastFP());
		d.set(get_LastFD());

		if (!H_Parent())
		{
			StopShooting();
			return;
		}
		CGameObject* GO = H_Parent()->cast_game_object();
		if (!GO || GO->getDestroy())
		{
			StopShooting();
			return;
		}

		if(!IsGameTypeSingle())
		{
			if (smart_cast<CMPPlayersBag*>(GO) != nullptr)
			{
				Msg("! WARNING: state_Fire of object [%d][%s] while parent is CMPPlayerBag...", ID(), cNameSect().c_str());
				{
					StopShooting();
					return;
				}
			}
		}

		CEntity* entity = GO->cast_entity();
		if (!entity)
		{
			StopShooting();
			return;
		}
		CInventoryOwner* inventory_owner = entity->cast_inventory_owner();
		if (!inventory_owner || !inventory_owner->m_inventory)
		{
			StopShooting();
			return;
		}

		entity->g_fireParams	(this, p1,d);

		if( !entity->g_stateFire() )
			StopShooting();

		if (m_iShotNum == 0)
		{
			m_vStartPos = p1;
			m_vStartDir = d;
		};
		
		VERIFY(!m_magazine.empty());

		while (	!m_magazine.empty() && 
				fShotTimeCounter<0 && 
				(IsWorking() || m_bFireSingleShot) && 
				(m_iQueueSize<0 || m_iShotNum<m_iQueueSize)
			   )
		{
			if( CheckForMisfire() )
			{
				StopShooting();
				return;
			}

			m_bFireSingleShot		= false;

			if (m_iQueueSize == 1 && m_fSingleShootsTimeDelta > 0.0f)
				fShotTimeCounter += m_fSingleShootsTimeDelta;
			else if (m_fBaseDispersionedBulletsTimeDelta > 0.0f && m_iShotNum < m_iBaseDispersionedBulletsCount)
				fShotTimeCounter += m_fBaseDispersionedBulletsTimeDelta;
			else
				fShotTimeCounter += fOneShotTime;
			
			if (!infinite_fire() || m_bIAmWeaponRPG7)
				++m_iShotNum;
			
			if (m_bUseLastAmmoType)
			{
				u8 type_to_update = m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();
				UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, type_to_update);
			}

			OnShot					();

			if (m_iShotNum>m_iBaseDispersionedBulletsCount)
				FireTrace		(p1,d);
			else
				FireTrace		(m_vStartPos, m_vStartDir);
		}
	
		if(m_iShotNum == m_iQueueSize)
			m_bStopedAfterQueueFired = true;

		UpdateSounds			();
	}

	if(fShotTimeCounter<0)
	{
		if(iAmmoElapsed == 0)
			OnMagazineEmpty();

		StopShooting();
	}
	else
	{
		fShotTimeCounter			-=	dt;
	}
}

void CWeaponMagazined::state_FireChamber(float dt)
{
	if (iAmmoChamberElapsed > 0)
	{
		VERIFY(fOneShotTime > 0.f);

		Fvector					p1, d;
		p1.set(get_LastFP());
		d.set(get_LastFD());

		if (!H_Parent())
		{
			StopShooting();
			return;
		}
		CGameObject* GO = H_Parent()->cast_game_object();
		if (!GO || GO->getDestroy())
		{
			StopShooting();
			return;
		}

		if (!IsGameTypeSingle())
		{
			if (smart_cast<CMPPlayersBag*>(GO) != nullptr)
			{
				Msg("! WARNING: state_Fire of object [%d][%s] while parent is CMPPlayerBag...", ID(), cNameSect().c_str());
				{
					StopShooting();
					return;
				}
			}
		}

		CEntity* entity = GO->cast_entity();
		if (!entity)
		{
			StopShooting();
			return;
		}
		CInventoryOwner* inventory_owner = entity->cast_inventory_owner();
		if (!inventory_owner || !inventory_owner->m_inventory)
		{
			StopShooting();
			return;
		}

		entity->g_fireParams(this, p1, d);

		if (!entity->g_stateFire())
			StopShooting();

		if (m_iShotNum == 0)
		{
			m_vStartPos = p1;
			m_vStartDir = d;
		};

		VERIFY(!m_chamber.empty());

		while (!m_chamber.empty() &&
			fShotTimeCounter < 0 &&
			(IsWorking() || m_bFireSingleShot) &&
			(m_iQueueSize < 0 || m_iShotNum < m_iQueueSize)
			)
		{
			if (CheckForMisfire())
			{
				StopShooting();
				return;
			}

			m_bFireSingleShot = false;

			if (m_iQueueSize == 1 && m_fSingleShootsTimeDelta > 0.0f)
				fShotTimeCounter += m_fSingleShootsTimeDelta;
			else if (m_fBaseDispersionedBulletsTimeDelta > 0.0f && m_iShotNum < m_iBaseDispersionedBulletsCount)
				fShotTimeCounter += m_fBaseDispersionedBulletsTimeDelta;
			else
				fShotTimeCounter += fOneShotTime;

			if (!infinite_fire() || m_bIAmWeaponRPG7)
				++m_iShotNum;

			OnShot();

			if (m_iShotNum > m_iBaseDispersionedBulletsCount)
				FireTraceChamber(p1, d);
			else
				FireTraceChamber(m_vStartPos, m_vStartDir);
		}

		if (m_iShotNum == m_iQueueSize)
			m_bStopedAfterQueueFired = true;

		UpdateSounds();
	}

	if (fShotTimeCounter < 0)
	{
		if (iAmmoChamberElapsed == 0)
			OnMagazineEmpty();

		StopShooting();
	}
	else
	{
		fShotTimeCounter -= dt;
	}
}

void CWeaponMagazined::state_Misfire	(float dt)
{
	OnEmptyClick			();
	SwitchState				(eIdle);
	
	bMisfire				= true;

	UpdateSounds			();
}

void CWeaponMagazined::SetDefaults	()
{
	CWeapon::SetDefaults		();
}

void CWeaponMagazined::SelectShotSound()
{
	int get_elapsed = GetAmmoElapsed() + GetAmmoChamberElapsed();
	bool parent_actor = ParentIsActor();

	if (IsSilencerAttached())
	{
		if (get_elapsed == 1 && m_eSoundsFlags.test(ESoundsFlags::sf_shoot_last_sil))
		{
			if (parent_actor && m_eSoundsFlags.test(ESoundsFlags::sf_shoot_actor_last_sil))
			{
				m_sSndShotCurrent = "sndSilencerShotLastActor";
			}
			else
			{
				m_sSndShotCurrent = "sndSilencerShotLast";
			}
		}
		else
		{
			if (parent_actor && m_eSoundsFlags.test(ESoundsFlags::sf_shoot_actor_sil))
			{
				m_sSndShotCurrent = "sndSilencerShotActor";
			}
			else
			{
				m_sSndShotCurrent = "sndSilencerShot";
			}
		}
	}
	else
	{
		if (get_elapsed == 1 && m_eSoundsFlags.test(ESoundsFlags::sf_shoot_last))
		{
			if (parent_actor && m_eSoundsFlags.test(ESoundsFlags::sf_shoot_actor_last))
			{
				m_sSndShotCurrent = "sndShotLastActor";
			}
			else
			{
				m_sSndShotCurrent = "sndShotLast";
			}
		}
		else
		{
			if (parent_actor && m_eSoundsFlags.test(ESoundsFlags::sf_shoot_actor))
			{
				m_sSndShotCurrent = "sndShotActor";
			}
			else
			{
				m_sSndShotCurrent = "sndShot";
			}
		}
	}

	m_layered_sounds.PlaySound(*m_sSndShotCurrent, get_LastFP(), H_Parent(), !!GetHUDmode(), false, true);

	float fAmmoElapsed = (float)get_elapsed;
	float fmaxMagazineSize_ = iMagazineSize + iChamberSize;
	float factor = fAmmoElapsed / (fmaxMagazineSize_ / 3.0f);
	if (factor <= 1.0f)
	{
		clamp(factor, 0.0f, 1.0f);
		factor = 1.0f - factor;
		HUD_SOUND_ITEM::SetHudSndGlobalVolumeFactor(factor);
		PlaySound("sndMagShot", get_LastFP());
		HUD_SOUND_ITEM::SetHudSndGlobalVolumeFactor(1.0f);
	}
}

void CWeaponMagazined::OnShot()
{
	SelectShotSound();

	// Camera	
	AddShotEffector();

	// Animation
	PlayAnimShoot();

	// Shell Drop
	Fvector vel;
	PHGetLinearVell(vel);
	OnShellDrop(get_LastSP(), vel);

	// Огонь из ствола
	StartFlameParticles();

	//дым из ствола
	ForceUpdateFireParticles();
	StartSmokeParticles(get_LastFP(), vel);

	if (H_Parent())
	{
		if (CGameObject* object = H_Parent()->cast_game_object())
		{
			object->callback(GameObject::eOnWeaponFired)(object->lua_game_object(), this->lua_game_object(), iAmmoElapsed, m_ammoType);
		}
	}
}

void CWeaponMagazined::OnEmptyClick()
{
	PlaySound("sndEmptyClick", get_LastFP());
}

void CWeaponMagazined::OnAnimationEnd(u32 state) 
{
	switch(state) 
	{
		case eReload:
		{
			if (!IsTriStateReload())
			{
				bReloadKeyPressed = false;
				bAmmotypeKeyPressed = false;
			}

			if (bMisfireReload)
			{
				bMisfire = false;
				bMisfireReload = false;
			}
			else
			{
				if (!m_bIsReloaded)
				{
					m_bIsReloaded = true;
					ReloadMagazine();
				}
				GiveAmmoFromMagToChamber();
			}
			SwitchState(eIdle);
		} break;
		case eHiding:
			SwitchState(eHidden);  
		break;
		case eIdle:
			switch2_Idle();
		break;
		case eEmptyClick:
		{
			m_bBlockEmptyClick = false;
			SwitchState(eIdle);
			break;
		}
		case eFire:
		case eFire2:
		case eShowing:
		case eSwitchMode:
			SwitchState(eIdle);
		break;
	}
	inherited::OnAnimationEnd(state);
}

void CWeaponMagazined::switch2_Idle	()
{
	m_iShotNum = 0;
	if(m_fOldBulletSpeed != 0.f)
		SetBulletSpeed(m_fOldBulletSpeed);

	SetPending			(FALSE);
	PlayAnimIdle		();
}

#ifdef DEBUG
#include "ai/stalker/ai_stalker.h"
#endif
void CWeaponMagazined::switch2_Fire	()
{
	if (!H_Parent()) return;
	CInventoryOwner* io		= H_Parent()->cast_inventory_owner();
	CInventoryItem* ii		= cast_inventory_item();
#ifdef DEBUG
	if (!io)
		return;
	//VERIFY2					(io,make_string("no inventory owner, item %s",*cName()));

	if (ii != io->inventory().ActiveItem())
		Msg					("! not an active item, item %s, owner %s, active item %s",*cName(),*H_Parent()->cName(),io->inventory().ActiveItem() ? *io->inventory().ActiveItem()->object().cName() : "no_active_item");

#if USE_OLD_OBJECT_PLANNER
	if ( !(io && (ii == io->inventory().ActiveItem())) ) 
	{
		CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(H_Parent());
		if (stalker) {
			stalker->planner().show						();
			stalker->planner().show_current_world_state	();
			stalker->planner().show_target_world_state	();
		}
	}
#endif
#else
	if (!io)
		return;
#endif // DEBUG

//
//	VERIFY2(
//		io && (ii == io->inventory().ActiveItem()),
//		make_string(
//			"item[%s], parent[%s]",
//			*cName(),
//			H_Parent() ? *H_Parent()->cName() : "no_parent"
//		)
//	);

	m_bStopedAfterQueueFired = false;
	m_bFireSingleShot = true;
	m_iShotNum = 0;

    if((OnClient() || Level().IsDemoPlay())&& !IsWorking())
		FireStart();

}

void CWeaponMagazined::switch2_Empty()
{
	auto play_motion_if_exists = [&](const shared_str& motion_name)
	{
		if (HudAnimationExist(motion_name))
		{
			SetPending(TRUE);
			m_bBlockEmptyClick = true;
			PlayHUDMotion(SetCurrentStateAnimation(motion_name), true, eEmptyClick);
		}
		else
		{
			SwitchState(eIdle);
		}
	};

	shared_str name = "anm_fakeshoot";

	if (ParentIsActor())
	{
		if (IsZoomed())
		{
			name.printf("%s%s", *name, "_aim");
		}

		if (IsMisfire())
		{
			name.printf("%s%s", *name, "_jammed");
		}
		else
		{
			name.printf("%s%s", *name, "_empty");
		}
	}

	const static bool isAutoreload = EngineExternal()[EEngineExternalGame::EnableAutoreload];

	if (!isAutoreload)
	{
		OnEmptyClick();
		play_motion_if_exists(name);
	}
	else
	{
		if (!IsTriStateReload())
		{
			if (!TryReload())
			{
				OnEmptyClick();
				play_motion_if_exists(name);
			}
			else
			{
				inherited::FireEnd();
			}
		}
		else
		{
			if (!HaveCartridgeInInventory(1))
			{
				OnEmptyClick();
				play_motion_if_exists(name);
			}
			else
			{
				inherited::FireEnd();
				Reload();
			}
		}
	}
}

void CWeaponMagazined::PlayReloadSound()
{
	if (!m_sounds_enabled)
	{
		return;
	}

	if (!ParentIsActor())
	{
		PlaySound("sndReload", get_LastFP());
		return;
	}

	bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;
	CActor* actor = Level().CurrentControlEntity() != nullptr ? Level().CurrentControlEntity()->cast_actor() : nullptr;
	bool detector = actor != nullptr && actor->GetDetector() != nullptr;

	if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_jam_last_det) && IsMisfire() && bMisfireReload && empty && detector)
	{
		PlaySound("sndReloadMisLastDet", get_LastFP());
	}
	else if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_jam_det) && IsMisfire() && bMisfireReload && detector)
	{
		PlaySound("sndReloadMisDet", get_LastFP());
	}
	else if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_jam_last) && IsMisfire() && bMisfireReload && empty)
	{
		PlaySound("sndReloadMisLast", get_LastFP());
	}
	else if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_jam) && IsMisfire() && bMisfireReload)
	{
		PlaySound("sndReloadMis", get_LastFP());
	}
	else if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_empty) && empty)
	{
		PlaySound("sndReloadEmpty", get_LastFP());
	}
	else if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_change) && IsChangeAmmoType())
	{
		PlaySound("sndChangeCartridgeType", get_LastFP());
	}
	else
	{
		PlaySound("sndReload", get_LastFP());
	}
}

void CWeaponMagazined::switch2_Reload()
{
	CWeapon::FireEnd	();
	m_bIsReloaded = false;
	PlayAnimReload		();
	PlayReloadSound		();
	SetPending			(TRUE);
}

void CWeaponMagazined::switch2_Hiding()
{
	OnZoomOut();
	CWeapon::FireEnd();
	
	if(m_sounds_enabled)
		PlaySound			("sndHide",get_LastFP());

	PlayAnimHide		();
	SetPending			(TRUE);
}

void CWeaponMagazined::switch2_Hidden()
{
	CWeapon::FireEnd();

	StopCurrentAnimWithoutCallback();

	signal_HideComplete		();
	RemoveShotEffector		();
}

void CWeaponMagazined::switch2_Showing()
{
	if(m_sounds_enabled)
		PlaySound			("sndShow",get_LastFP());

	SetPending			(TRUE);
	PlayAnimShow		();
}

void CWeaponMagazined::switch2_FireMode()
{
	SetPending(TRUE);

	if (m_sounds_enabled && m_eSoundsFlags.test(ESoundsFlags::sf_changefiremode))
	{
		PlaySound("sndChangeFiremode", get_LastFP());
	}

	shared_str anim_name = "anm_changefiremode_from_";
	if (m_iPrevFireMode == -1)
	{
		anim_name.printf("%s%s_to_", *anim_name, "a");
	}
	else
	{
		anim_name.printf("%s%d_to_", *anim_name, m_iPrevFireMode);
	}

	if (GetQueueSize() == -1)
	{
		anim_name.printf("%s%s", *anim_name, "a");
	}
	else
	{
		anim_name.printf("%s%d", *anim_name, GetQueueSize());
	}

	if (HudAnimationExist(anim_name))
	{
		PlayHUDMotion(SetCurrentStateAnimation(anim_name), true, eSwitchMode);
	}
	else
	{
		PlayHUDMotion(SetCurrentStateAnimation("anm_firemode"), true, eSwitchMode);
	}
}

bool CWeaponMagazined::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;
	
	//если оружие чем-то занято, то ничего не делать
	if(IsPending()) return false;
	
	switch(cmd) 
	{
	case kWPN_RELOAD:
		{
			if(flags&CMD_START)
			{

				if (iAmmoElapsed < iMagazineSize || IsMisfire())
				{
					if (!bReloadKeyPressed || !bAmmotypeKeyPressed)
						bReloadKeyPressed = true;

					Reload();
				}
			}
		} 
		return true;
	case kWPN_FIREMODE_PREV:
	case kWPN_FIREMODE_NEXT:
		{
			if (flags & CMD_START) 
			{
				ChangeFireMode(cmd);
				return true;
			};
		}break;
	}
	return false;
}

bool CWeaponMagazined::CanAttach(PIItem pIItem)
{
	CScope* pScope = smart_cast<CScope*>(pIItem);
	CSilencer* pSilencer = smart_cast<CSilencer*>(pIItem);
	CGrenadeLauncher* pGrenadeLauncher = smart_cast<CGrenadeLauncher*>(pIItem);

	if (pScope && m_eScopeStatus == ALife::eAddonAttachable)
	{
		if (IsScopeAttached() && pIItem->object().cNameSect() == GetScopeName())
		{
			return false;
		}

		return ScopeFit(pScope);
	}
	else if (pSilencer &&
		m_eSilencerStatus == ALife::eAddonAttachable &&
		(m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonSilencer) == 0 &&
		(m_sSilencerName == pIItem->object().cNameSect()))
		return true;
	else if (pGrenadeLauncher &&
		m_eGrenadeLauncherStatus == ALife::eAddonAttachable &&
		(m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) == 0 &&
		(m_sGrenadeLauncherName == pIItem->object().cNameSect()))
		return true;
	else
		return inherited::CanAttach(pIItem);
}

bool CWeaponMagazined::CanDetach(const char* item_section_name)
{
	if( m_eScopeStatus == ALife::eAddonAttachable &&
	   0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonScope))/* &&
	   (m_scopes[cur_scope]->m_sScopeName	== item_section_name))*/
	{
		SCOPES_VECTOR_IT it = m_scopes.begin();
		for(; it!=m_scopes.end(); it++)
		{
			if (bUseAltScope)
			{
				if (*it == item_section_name)
					return true;
			}
			else
			{
				if (pSettings->r_string((*it), "scope_name") == item_section_name)
					return true;
			}
		}
		return false;
	}
//	   return true;
	else if(m_eSilencerStatus == ALife::eAddonAttachable &&
	   0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonSilencer) &&
	   (m_sSilencerName == item_section_name))
       return true;
	else if(m_eGrenadeLauncherStatus == ALife::eAddonAttachable &&
	   0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) &&
	   (m_sGrenadeLauncherName == item_section_name))
       return true;
	else
		return inherited::CanDetach(item_section_name);
}

bool CWeaponMagazined::Attach(PIItem pIItem, bool b_send_event)
{
	bool result = false;

	CScope*				pScope					= smart_cast<CScope*>(pIItem);
	CSilencer*			pSilencer				= smart_cast<CSilencer*>(pIItem);
	CGrenadeLauncher*	pGrenadeLauncher		= smart_cast<CGrenadeLauncher*>(pIItem);
	
	if (pScope && m_eScopeStatus == ALife::eAddonAttachable)
	{
		if (IsScopeAttached())
		{
			Detach(GetScopeName().c_str(), true);
		}

		SCOPES_VECTOR_IT it = m_scopes.begin();
		for (; it != m_scopes.end(); it++)
		{
			if (bUseAltScope)
			{
				if (*it == pIItem->object().cNameSect())
					m_cur_scope = u8(it - m_scopes.begin());
			}
			else
			{
				if (pSettings->r_string((*it), "scope_name") == pIItem->object().cNameSect())
					m_cur_scope = u8(it - m_scopes.begin());
			}
		}
		m_flagsAddOnState |= CSE_ALifeItemWeapon::eWeaponAddonScope;
		result = true;
	}
	else if (pSilencer && m_eSilencerStatus == ALife::eAddonAttachable && (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonSilencer) == 0 && (m_sSilencerName == pIItem->object().cNameSect()))
	{
		if (m_bRestGlSil && GrenadeLauncherAttachable() && IsGrenadeLauncherAttached())
		{
			Detach(*GetGrenadeLauncherName(), true);
		}

		m_flagsAddOnState |= CSE_ALifeItemWeapon::eWeaponAddonSilencer;
		result = true;
	}
	else if (pGrenadeLauncher && m_eGrenadeLauncherStatus == ALife::eAddonAttachable && (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) == 0 && (m_sGrenadeLauncherName == pIItem->object().cNameSect()))
	{
		if (m_bRestGlSil && SilencerAttachable() && IsSilencerAttached())
		{
			Detach(*GetSilencerName(), true);
		}

		m_flagsAddOnState |= CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher;
		result = true;
	}

	if(result)
	{
		if (b_send_event && OnServer())
		{
			//уничтожить подсоединенную вещь из инвентаря
//.			pIItem->Drop					();
			pIItem->object().DestroyObject	();
		};
		UpdateAltScope();
		UpdateAddonsVisibility();
		UpdateHUDAddonsVisibility();
		ProcessScope();
		InitAddons();

		return true;
	}
	else
        return inherited::Attach(pIItem, b_send_event);
}

bool CWeaponMagazined::DetachScope(const char* item_section_name, bool b_spawn_item)
{
	bool detached = false;
	SCOPES_VECTOR_IT it = m_scopes.begin();
	shared_str iter_scope_name = "none";

	for(; it!=m_scopes.end(); it++)
	{
		if (bUseAltScope)
		{
			iter_scope_name = (*it);
		}
		else
		{
			iter_scope_name = pSettings->r_string((*it), "scope_name");
		}

		if(!xr_strcmp(iter_scope_name, item_section_name))
		{
			m_cur_scope = 0;
			detached = true;
		}
	}
	return detached;
}

bool CWeaponMagazined::Detach(const char* item_section_name, bool b_spawn_item)
{
	auto UpdateHudInfo = [this]()
	{
		UpdateAddonsVisibility();
		UpdateHUDAddonsVisibility();
		ProcessScope();
		InitAddons();
	};

	if (m_eScopeStatus == ALife::eAddonAttachable && DetachScope(item_section_name, b_spawn_item))
	{
		if ((m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonScope) == 0)
		{
			Msg("ERROR: scope addon already detached.");
			return true;
		}

		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonScope;
		UpdateAltScope();
		UpdateHudInfo();

		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}
	else if (m_eSilencerStatus == ALife::eAddonAttachable && (m_sSilencerName == item_section_name))
	{
		if ((m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonSilencer) == 0)
		{
			Msg("ERROR: silencer addon already detached.");
			return true;
		}

		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonSilencer;
		UpdateHudInfo();

		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}
	else if (m_eGrenadeLauncherStatus == ALife::eAddonAttachable && (m_sGrenadeLauncherName == item_section_name))
	{
		if ((m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) == 0)
		{
			Msg("ERROR: grenade launcher addon already detached.");
			return true;
		}

		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher;

		UpdateHudInfo();

		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}

	return inherited::Detach(item_section_name, b_spawn_item);;
}
/*
void CWeaponMagazined::LoadAddons()
{
	m_zoom_params.m_fIronSightZoomFactor = READ_IF_EXISTS( pSettings, r_float, cNameSect(), "ironsight_zoom_factor", 50.0f );

}
*/
void CWeaponMagazined::InitAddons()
{
	m_zoom_params.m_fIronSightZoomFactor = READ_IF_EXISTS( pSettings, r_float, cNameSect(), "ironsight_zoom_factor", 50.0f );
	if ( IsScopeAttached() )
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

	if ( IsSilencerAttached()/* && SilencerAttachable() */)
	{		
		m_sFlameParticlesCurrent	= m_sSilencerFlameParticles;
		m_sSmokeParticlesCurrent	= m_sSilencerSmokeParticles;

		//подсветка от выстрела
		LoadLights					(*cNameSect(), "silencer_");
		ApplySilencerKoeffs			();
	}
	else
	{
		m_sFlameParticlesCurrent	= m_sFlameParticles;
		m_sSmokeParticlesCurrent	= m_sSmokeParticles;

		//подсветка от выстрела
		LoadLights		(*cNameSect(), "");
		ResetSilencerKoeffs();
	}

	HudSelector();
	inherited::InitAddons();
}

void CWeaponMagazined::HudSelector()
{
	if (m_bUseSilHud && SilencerAttachable() && IsSilencerAttached())
		hud_sect = hud_silencer;
	else if (m_bUseScopeHud && ScopeAttachable() && IsScopeAttached())
		hud_sect = hud_scope;
	else if (m_bUseGLHud && GrenadeLauncherAttachable() && IsGrenadeLauncherAttached())
		hud_sect = hud_gl;
	else
		hud_sect = hud_sect_cache;

	bUpdateHUDBonesVisibility = false;
}

void CWeaponMagazined::LoadSilencerKoeffs()
{
	if ( m_eSilencerStatus == ALife::eAddonAttachable )
	{
		LPCSTR sect = m_sSilencerName.c_str();
		m_silencer_koef.hit_power		= READ_IF_EXISTS( pSettings, r_float, sect, "bullet_hit_power_k", 1.0f );
		m_silencer_koef.hit_impulse		= READ_IF_EXISTS( pSettings, r_float, sect, "bullet_hit_impulse_k", 1.0f );
		m_silencer_koef.bullet_speed	= READ_IF_EXISTS( pSettings, r_float, sect, "bullet_speed_k", 1.0f );
		m_silencer_koef.fire_dispersion	= READ_IF_EXISTS( pSettings, r_float, sect, "fire_dispersion_base_k", 1.0f );
		m_silencer_koef.cam_dispersion	= READ_IF_EXISTS( pSettings, r_float, sect, "cam_dispersion_k", 1.0f );
		m_silencer_koef.cam_disper_inc	= READ_IF_EXISTS( pSettings, r_float, sect, "cam_dispersion_inc_k", 1.0f );
	}

	clamp( m_silencer_koef.hit_power,		0.0f, 1.0f );
	clamp( m_silencer_koef.hit_impulse,		0.0f, 1.0f );
	clamp( m_silencer_koef.bullet_speed,	0.0f, 1.0f );
	clamp( m_silencer_koef.fire_dispersion,	0.0f, 3.0f );
	clamp( m_silencer_koef.cam_dispersion,	0.0f, 1.0f );
	clamp( m_silencer_koef.cam_disper_inc,	0.0f, 1.0f );
}

void CWeaponMagazined::ApplySilencerKoeffs()
{
	cur_silencer_koef = m_silencer_koef;
}

void CWeaponMagazined::ResetSilencerKoeffs()
{
	cur_silencer_koef.Reset();
}

void CWeaponMagazined::PlayAnimShow()
{
	VERIFY(GetState()==eShowing);
	PlayHUDMotion(SetCurrentStateAnimation("anm_show"), FALSE, GetState());
}

void CWeaponMagazined::PlayAnimHide()
{
	VERIFY(GetState()==eHiding);
	PlayHUDMotion(SetCurrentStateAnimation("anm_hide"), TRUE, GetState());
}

shared_str CWeaponMagazined::SetCurrentReloadAnimation()
{
	shared_str anim = "anm_reload";

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;
		if (IsMisfire())
		{
			AddSuffixName(anim, "_misfire");
			AddSuffixName(anim, "_jammed");

			if (empty)
			{
				AddSuffixName(anim, "_last");
			}
		}
		else if (empty)
		{
			AddSuffixName(anim, "_empty");
		}

		if (IsChangeAmmoType())
		{
			AddSuffixName(anim, "_ammochange");
		}

		CActor* actor = Level().CurrentControlEntity()->cast_actor();
		bool detector = actor != nullptr && actor->GetDetector() != nullptr;

		if (detector)
		{
			AddSuffixName(anim, "_detector");
		}

		if (ScopeAttachable() && !IsScopeAttached())
		{
			AddSuffixName(anim, "_noscope");
		}

	}

	return anim;
}

shared_str CWeaponMagazined::SetCurrentStateAnimation(const shared_str& first_name)
{
	shared_str anim = first_name;

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;

		if (IsZoomed())
		{
			AddSuffixName(anim, "_aim");
		}

		if (IsMisfire())
		{
			AddSuffixName(anim, "_misfire");
			AddSuffixName(anim, "_jammed");
		}
		else if (empty)
		{
			AddSuffixName(anim, "_empty");
		}

		if (ScopeAttachable() && !IsScopeAttached())
		{
			AddSuffixName(anim, "_noscope");
		}
	}

	return anim;
}

void CWeaponMagazined::PlayAnimReload()
{
	VERIFY(GetState() == eReload);

	u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, type_to_update);

	PlayHUDMotion(SetCurrentReloadAnimation(), TRUE, GetState());
	if (ParentIsActor())
	{
		if (IsMisfire() && (HudAnimationExist("anm_reload_misfire") || HudAnimationExist("anm_reload_jammed")))
		{
			bMisfireReload = true;
		}

		CActor* actor = Level().CurrentControlEntity()->cast_actor();
		bool detector = actor != nullptr && actor->GetDetector() != nullptr;
		if (detector && HudAnimationExist("anm_reload_detector"))
		{
			bDisablePrepareAnimation = true;
		}
	}
}

shared_str CWeaponMagazined::SetCurrentAimAnimation()
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

void CWeaponMagazined::PlayAnimAim()
{
	PlayHUDMotion(SetCurrentAimAnimation(), TRUE, GetState());
}

void CWeaponMagazined::PlaySoundAim(bool in)
{
	if (!m_sounds_enabled)
		return;

	if (in)
	{
		if (m_eSoundsFlags.test(ESoundsFlags::sf_aim_start))
		{
			PlaySound("sndAimStart", get_LastFP());
		}
	}
	else
	{
		if (m_eSoundsFlags.test(ESoundsFlags::sf_aim_end))
		{
			PlaySound("sndAimEnd", get_LastFP());
		}
	}
}

void CWeaponMagazined::PlayAnimIdle()
{
	if (GetState() != eIdle)
		return;

	if (IsZoomed())
	{
		if (!m_bIsAimStarted && HudAnimationExist("anm_idle_aim_start"))
		{
			m_bIsAimStarted = true;
			PlayHUDMotion(SetCurrentStateAnimation("anm_idle_aim_start"), true, GetState());
			return;
		}

		PlayAnimAim();
	}
	else
	{
		if (m_bIsAimStarted && HudAnimationExist("anm_idle_aim_end"))
		{
			m_bIsAimStarted = false;
			PlayHUDMotion(SetCurrentStateAnimation("anm_idle_aim_end"), true, GetState());
			return;
		}

		if (TryPlayAnimIdle())
		{
			return;
		}

		shared_str new_name = SetCurrentIdleAnimation();

		PlayHUDMotion(SetCurrentStateAnimation(new_name), TRUE, GetState());
	}
}

shared_str CWeaponMagazined::SetCurrentShootAnimation()
{
	bool last = m_bAmmoInChamber ? iAmmoChamberElapsed == 1 && iAmmoElapsed == 0 : iAmmoElapsed == 1;
	shared_str anim = HudAnimationExist("anm_shoot") ? "anm_shoot" : HudAnimationExist("anm_shot_l") && last ? "anm_shot_l" : "anm_shots";

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		if (IsZoomed())
		{
			AddSuffixName(anim, "_aim");
		}

		if (IsMisfire())
		{
			AddSuffixName(anim, "_misfire");
			AddSuffixName(anim, "_jammed");
		}
		else if (last)
		{
			AddSuffixName(anim, "_last");
			AddSuffixName(anim, "_l");
		}
	}

	return anim;
}

void CWeaponMagazined::PlayAnimShoot()
{
	VERIFY(GetState()==eFire);

	if (m_bAmmoInChamber && !m_chamber.empty())
	{
		UpdateShellBones(iAmmoElapsed, m_chamber.back().m_LocalAmmoType);
	}
	else if (!m_magazine.empty())
	{
		UpdateShellBones(iAmmoElapsed, m_magazine.back().m_LocalAmmoType);
	}

	PlayHUDMotion(SetCurrentShootAnimation(), FALSE, GetState());
}

void CWeaponMagazined::OnZoomIn			()
{
	inherited::OnZoomIn();

	if(GetState() == eIdle)
		PlayAnimIdle();

	if(H_Parent())
	{
		CGameObject* object = H_Parent()->cast_game_object();
		if (object)
			object->callback(GameObject::eOnWeaponZoomIn)(object->lua_game_object(), this->lua_game_object());

		if (CActor* actor = H_Parent()->cast_actor())
		{
			CEffectorZoomInertion* effectorZoomInertion = smart_cast<CEffectorZoomInertion*>(actor->Cameras().GetCamEffector(eCEZoom));
			if (!effectorZoomInertion)
			{
				effectorZoomInertion = (CEffectorZoomInertion*)actor->Cameras().AddCamEffector(new CEffectorZoomInertion());
				effectorZoomInertion->Init(this);
			}

			effectorZoomInertion->SetRndSeed(actor->GetZoomRndSeed());
			R_ASSERT(effectorZoomInertion);
		}
	}

	PlaySoundAim();
}

void CWeaponMagazined::OnZoomOut()
{
	if(!IsZoomed())	 
		return;

	inherited::OnZoomOut	();

	if(GetState()==eIdle)
		PlayAnimIdle		();

	if(H_Parent())
	{
		CGameObject* object = H_Parent()->cast_game_object();
		if (object)
			object->callback(GameObject::eOnWeaponZoomOut)(object->lua_game_object(), this->lua_game_object());

		CActor* actor = H_Parent()->cast_actor();
		if (actor)
			actor->Cameras().RemoveCamEffector(eCEZoom);
	}
	
	PlaySoundAim(false);
}

//переключение режимов стрельбы одиночными и очередями
bool CWeaponMagazined::SwitchMode()
{
	if (GetState() != eIdle || IsPending())
	{
		return false;
	}

	m_iQueueSize = SingleShotMode() ? WEAPON_ININITE_QUEUE : 1;

	return true;
}
 
void CWeaponMagazined::ChangeFireMode(u16 cmd)
{
	if (!HasFireModes() || GetState() != eIdle || IsZoomed() && m_eAnimationsFlags.test(EAnimationsFlags::af_firemode) && m_bDisableFireModeAim)
	{
		return;
	}

	m_iPrevFireMode = GetQueueSize();

	if (cmd == kWPN_NEXT)
	{
		m_iCurFireMode = (m_iCurFireMode + 1 + m_aFireModes.size()) % (s8)m_aFireModes.size();
	}
	else
	{
		m_iCurFireMode = (m_iCurFireMode - 1 + m_aFireModes.size()) % (s8)m_aFireModes.size();
	}

	SetQueueSize(GetCurrentFireMode());

	if (m_eAnimationsFlags.test(EAnimationsFlags::af_firemode))
	{
		SwitchState(eSwitchMode);
	}
};

void CWeaponMagazined::OnH_A_Chield()
{
	if (HasFireModes())
	{
		SetQueueSize(H_Parent() && H_Parent()->cast_actor() ? GetCurrentFireMode() : -1);
	}
	else
	{
		SetQueueSize(1);
	}

	inherited::OnH_A_Chield();
};

float CWeaponMagazined::GetWeaponDeterioration()
{
	return (m_iShotNum == 1) ? conditionDecreasePerShot : conditionDecreasePerQueueShot;
};

void CWeaponMagazined::save(NET_Packet &output_packet)
{
	inherited::save	(output_packet);
	save_data		(m_iQueueSize, output_packet);
	save_data		(m_iShotNum, output_packet);
	save_data		(m_iCurFireMode, output_packet);
}

void CWeaponMagazined::load(IReader &input_packet)
{
	inherited::load	(input_packet);
	load_data		(m_iQueueSize, input_packet);SetQueueSize(m_iQueueSize);
	load_data		(m_iShotNum, input_packet);
	load_data		(m_iCurFireMode, input_packet);
}

void CWeaponMagazined::net_Export	(NET_Packet& P)
{
	inherited::net_Export (P);

	P.w_u8(u8(m_iCurFireMode&0x00ff));
}

void CWeaponMagazined::net_Import	(NET_Packet& P)
{
	inherited::net_Import (P);

	m_iCurFireMode = P.r_u8();
	SetQueueSize(GetCurrentFireMode());
}

bool CWeaponMagazined::GetBriefInfo( II_BriefInfo& info )
{
	VERIFY( m_pInventory );
	string32	int_str;

	const int	ae				= GetAmmoElapsed() + iAmmoChamberElapsed;
	xr_sprintf			( int_str, "%d", ae );


	info.cur_ammo = int_str;

	if (infinite_fire())
	{
		info.cur_ammo = "∞";
	}

	if (m_iQueueSize == WEAPON_ININITE_QUEUE)
	{
		info.fire_mode = "A";
	}
	else
	{
		xr_sprintf(int_str, "%d", m_iQueueSize);
		info.fire_mode = int_str;
	}
	
	if ( m_pInventory->ModifyFrame() <= m_BriefInfo_CalcFrame )
	{
		return false;
	}
	const int at = GetSuitableAmmoTotal() - (GetAmmoElapsed() + iAmmoChamberElapsed); // update m_BriefInfo_CalcFrame
	xr_sprintf(int_str, "%d", at);
	info.total_ammo = int_str;
	info.grenade				= "";

	u32 at_size = (u32)m_ammoTypes.size();
	if ( unlimited_ammo() || at_size == 0 )
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
            xr_sprintf(int_str, "%d", GetAmmoCount(0));
            info.fmj_ammo._set(int_str);
        }
        if (at_size >= 2)
        {
            xr_sprintf(int_str, "%d", GetAmmoCount(1));
            info.ap_ammo._set(int_str);
        }
        if (at_size >= 3)
        {
            xr_sprintf(int_str, "%d", GetAmmoCount(2));
            info.third_ammo._set(int_str);
        }
		//-Alundaio
    }
	
	auto& CurrVector = m_bAmmoInChamber ? m_chamber : m_magazine;
	u8 CurrAmmoType = m_bAmmoInChamber ? m_ChamberAmmoType : m_ammoType;

	if ( ae != 0 && CurrVector.size() != 0 )
	{
		LPCSTR ammo_type = m_ammoTypes[CurrVector.back().m_LocalAmmoType].c_str();
		info.name		= g_pStringTable->translate( pSettings->r_string(ammo_type, "inv_name_short") );
		info.icon		= ammo_type;
	}
	else
	{
		LPCSTR ammo_type	= m_ammoTypes[CurrAmmoType].c_str();
		info.name			= g_pStringTable->translate( pSettings->r_string(ammo_type, "inv_name_short") );
		info.icon			= ammo_type;
	}
	return true;
}

bool CWeaponMagazined::install_upgrade_impl( LPCSTR section, bool test )
{
	bool result = inherited::install_upgrade_impl( section, test );
	
	LPCSTR str;
	// fire_modes = 1, 2, -1
	bool result2 = process_if_exists_set( section, "fire_modes", &CInifile::r_string, str, test );
	if ( result2 && !test )
	{
		int ModesCount = _GetItemCount( str );
		m_aFireModes.clear();
		for ( int i = 0; i < ModesCount; ++i )
		{
			string16 sItem;
			_GetItem( str, i, sItem );
			m_aFireModes.push_back( (s8)atoi(sItem) );
		}
		m_iCurFireMode = ModesCount - 1;
	}
	result |= result2;

	result |= process_if_exists_set(section, "base_dispersioned_bullets_count", &CInifile::r_s32, m_iBaseDispersionedBulletsCount, test);
	result |= process_if_exists_set(section, "base_dispersioned_bullets_speed", &CInifile::r_float, m_fBaseDispersionedBulletsSpeed, test);

	// sounds (name of the sound, volume (0.0 - 1.0), delay (sec))
	result2 = process_if_exists_set( section, "snd_draw", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_draw"	    , "sndShow"		, false, m_eSoundShow		);	}
	result |= result2;

	result2 = process_if_exists_set( section, "snd_holster", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_holster"	, "sndHide"		, false, m_eSoundHide		);	}
	result |= result2;

	result2 = process_if_exists_set( section, "snd_shoot", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_shoot"	, "sndShot"		, false, m_eSoundShot		);	}
	result |= result2;

	result2 = process_if_exists_set( section, "snd_empty", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_empty"	, "sndEmptyClick"	, false, m_eSoundEmptyClick);	}
	result |= result2;

	result2 = process_if_exists_set( section, "snd_reload", &CInifile::r_string, str, test );
	if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_reload"	, "sndReload"		, true, m_eSoundReload	);	}
	result |= result2;

	//snd_shoot1     = weapons\ak74u_shot_1 ??
	//snd_shoot2     = weapons\ak74u_shot_2 ??
	//snd_shoot3     = weapons\ak74u_shot_3 ??

	if ( m_eSilencerStatus == ALife::eAddonAttachable || m_eSilencerStatus == ALife::eAddonPermanent )
	{
		result |= process_if_exists_set( section, "silencer_flame_particles", &CInifile::r_string, m_sSilencerFlameParticles, test );
		result |= process_if_exists_set( section, "silencer_smoke_particles", &CInifile::r_string, m_sSilencerSmokeParticles, test );

		result2 = process_if_exists_set( section, "snd_silncer_shot", &CInifile::r_string, str, test );
		if ( result2 && !test ) { m_sounds.LoadSound( section, "snd_silncer_shot"	, "sndSilencerShot", false, m_eSoundShot	);	}
		result |= result2;
	}

	// fov for zoom mode
	result |= process_if_exists( section, "ironsight_zoom_factor", &CInifile::r_float, m_zoom_params.m_fIronSightZoomFactor, test );

	if( IsScopeAttached() )
	{
		//if ( m_eScopeStatus == ALife::eAddonAttachable )
		{
			result |= process_if_exists( section, "scope_zoom_factor", &CInifile::r_float, m_zoom_params.m_fScopeZoomFactor, test );
		}
	}
	else
	{
		if( IsZoomEnabled() )
		{
			result |= process_if_exists( section, "scope_zoom_factor", &CInifile::r_float, m_zoom_params.m_fIronSightZoomFactor, test );
		}
	}

	return result;
}
//текущая дисперсия (в радианах) оружия с учетом используемого патрона и недисперсионных пуль
float CWeaponMagazined::GetFireDispersion(float cartridge_k, bool for_crosshair) 
{
	float fire_disp = GetBaseDispersion(cartridge_k);
	if(for_crosshair || !m_iBaseDispersionedBulletsCount || !m_iShotNum || m_iShotNum > m_iBaseDispersionedBulletsCount)
	{
		fire_disp = inherited::GetFireDispersion(cartridge_k);
	}
	return fire_disp;
}
void CWeaponMagazined::FireBullet(	const Fvector& pos, 
									const Fvector& shot_dir, 
									float fire_disp,
									const CCartridge& cartridge,
									u16 parent_id,
									u16 weapon_id,
									bool send_hit)
{
	if(m_iBaseDispersionedBulletsCount)
	{
		if(m_iShotNum <= 1)
		{
			m_fOldBulletSpeed = GetBulletSpeed();
			SetBulletSpeed(m_fBaseDispersionedBulletsSpeed);
		}
		else if(m_iShotNum > m_iBaseDispersionedBulletsCount)
		{
			SetBulletSpeed(m_fOldBulletSpeed);
		}
	}
	inherited::FireBullet(pos, shot_dir, fire_disp, cartridge, parent_id, weapon_id, send_hit);
}

void CWeaponMagazined::OnMotionMark(u32 state, const motion_marks& mark)
{
	inherited::OnMotionMark(state, mark);

	if (state == eReload && mark.name == "Right" && !m_bIsReloaded)
	{
		m_bIsReloaded = true;
		if (bMisfireReload && !IsGrenadeMode())
		{
			bMisfire = false;
			bMisfireReload = false;
		}
		else
		{
			ReloadMagazine();
			GiveAmmoFromMagToChamber();
		}
	}
}