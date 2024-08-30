#include "stdafx.h"
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
#include "ui/UIXmlInit.h"
#include "ui/UIStatic.h"
#include "game_object_space.h"
#include "script_game_object.h"
#include "Actor_Flags.h"
#include "player_hud.h"
#include "weaponBM16.h"

ENGINE_API	bool	g_dedicated_server;

CUIXml*				pWpnScopeXml = nullptr;

void createWpnScopeXML()
{
	if(!pWpnScopeXml)
	{
		pWpnScopeXml			= new CUIXml();
		pWpnScopeXml->Load		(CONFIG_PATH, UI_PATH, "scopes.xml");
	}
}

CWeaponMagazined::CWeaponMagazined(ESoundTypes eSoundType) : CWeapon()
{
	m_eSoundShow				= ESoundTypes(SOUND_TYPE_ITEM_TAKING | eSoundType);
	m_eSoundHide				= ESoundTypes(SOUND_TYPE_ITEM_HIDING | eSoundType);
	m_eSoundShot				= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING | eSoundType);
	m_eSoundEmptyClick			= ESoundTypes(SOUND_TYPE_WEAPON_EMPTY_CLICKING | eSoundType);
	m_eSoundReload				= ESoundTypes(SOUND_TYPE_WEAPON_RECHARGING | eSoundType);
	m_eSoundAim 				= ESoundTypes(SOUND_TYPE_WEAPON | eSoundType);
	m_eSoundAimOut 				= ESoundTypes(SOUND_TYPE_WEAPON | eSoundType);
	m_eSoundClose				= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	m_eSoundAddCartridge		= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	m_eSoundOpen				= ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	
	m_sounds_enabled			= true;
	
	m_sSndShotCurrent			= nullptr;
	m_sSilencerFlameParticles	= m_sSilencerSmokeParticles = nullptr;

	m_bFireSingleShot			= false;
	m_iShotNum					= 0;
	m_fOldBulletSpeed			= 0;
	m_iQueueSize				= WEAPON_ININITE_QUEUE;
	m_bLockType					= false;
}

CWeaponMagazined::~CWeaponMagazined()
{
	// sounds
}


void CWeaponMagazined::net_Destroy()
{
	inherited::net_Destroy();
}

void CWeaponMagazined::Load	(LPCSTR section)
{
	inherited::Load		(section);

	// Sounds
	m_sounds.LoadSound(section,"snd_draw", "sndShow"		, false, m_eSoundShow		);
	m_sounds.LoadSound(section,"snd_holster", "sndHide"		, false, m_eSoundHide		);
	m_sounds.LoadSound(section,"snd_shoot", "sndShot"		, false, m_eSoundShot		);
	m_sounds.LoadSound(section,"snd_empty", "sndEmptyClick"	, false, m_eSoundEmptyClick	);
	m_sounds.LoadSound(section,"snd_reload", "sndReload"	, true, m_eSoundReload		);

	if (WeaponSoundExist(section, "snd_reload_empty"))
		m_sounds.LoadSound(section,"snd_reload_empty", "sndReloadEmpty"	, true, m_eSoundReload);
	
	if (WeaponSoundExist(section, "snd_reload_jammed"))
		m_sounds.LoadSound(section, "snd_reload_jammed", "sndReloadJammed", true, m_eSoundReload); 

	if (WeaponSoundExist(section, "snd_reload_jammed_last"))
		m_sounds.LoadSound(section, "snd_reload_jammed_last", "sndReloadJammedLast", true, m_eSoundReload);

	if (WeaponSoundExist(section, "snd_changecartridgetype"))
		m_sounds.LoadSound(section, "snd_changecartridgetype", "sndChangeCartridgeType", true, m_eSoundReload);

	if (WeaponSoundExist(section, "snd_jam"))
		m_sounds.LoadSound(section, "snd_jam", "sndJam", true, m_eSoundReload);

	if (WeaponSoundExist(section, "snd_breechblock"))
		m_sounds.LoadSound(section, "snd_breechblock", "sndPump", true, m_eSoundReload);

	if (WeaponSoundExist(section, "snd_jammed_click"))
		m_sounds.LoadSound(section, "snd_jammed_click", "sndJammedClick", true, m_eSoundEmptyClick);

	if (WeaponSoundExist(section, "snd_kick"))
		m_sounds.LoadSound(section, "snd_kick", "sndKick", true, m_eSoundShot);

	if (m_bTriStateReload)
	{
		m_sounds.LoadSound(section, "snd_open_weapon", "sndOpen", false, m_eSoundOpen);
		m_sounds.LoadSound(section, "snd_add_cartridge", "sndAddCartridge", false, m_eSoundAddCartridge);

		if (WeaponSoundExist(section, "snd_add_cartridge_empty"))
			m_sounds.LoadSound(section, "snd_add_cartridge_empty", "sndAddCartridgeEmpty", false, m_eSoundAddCartridge);

		m_sounds.LoadSound(section, "snd_close_weapon", "sndClose", false, m_eSoundClose);

		if (WeaponSoundExist(section, "snd_close_weapon_empty"))
			m_sounds.LoadSound(section, "snd_close_weapon_empty", "sndCloseEmpty", false, m_eSoundClose);

		if (m_bAddCartridgeOpen)
			m_sounds.LoadSound(section, "snd_open_weapon_empty", "sndOpenEmpty", false, m_eSoundOpen);

		if (m_bEmptyPreloadMode)
		{
			m_sounds.LoadSound(section, "snd_add_cartridge_preloaded", "sndAddCartridgePreloaded", false, m_eSoundOpen);
			m_sounds.LoadSound(section, "snd_close_weapon_preloaded", "sndClosePreloaded", false, m_eSoundClose);
		}
	}

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	if (isGuns)
	{
		m_sounds.LoadSound(section, "snd_aim_start", "sndAim", true, m_eSoundAim);
		m_sounds.LoadSound(section, "snd_aim_end", "sndAimOut", true, m_eSoundAimOut);
	}
	else
	{
		if (WeaponSoundExist(section, "snd_aim"))
			m_sounds.LoadSound(section, "snd_aim", "sndAim", true, m_eSoundAim);

		if (WeaponSoundExist(section, "snd_aim_out"))
			m_sounds.LoadSound(section, "snd_aim_out", "sndAimOut", true, m_eSoundAimOut);
	}

	m_sSndShotCurrent = "sndShot";
		
	//звуки и партиклы глушителя, еслит такой есть
	if ( m_eSilencerStatus == ALife::eAddonAttachable || m_eSilencerStatus == ALife::eAddonPermanent )
	{
		if(pSettings->line_exist(section, "silencer_flame_particles"))
			m_sSilencerFlameParticles = pSettings->r_string(section, "silencer_flame_particles");
		if(pSettings->line_exist(section, "silencer_smoke_particles"))
			m_sSilencerSmokeParticles = pSettings->r_string(section, "silencer_smoke_particles");
		
		m_sounds.LoadSound(section,"snd_silncer_shot", "sndSilencerShot", false, m_eSoundShot);
	}

	m_iBaseDispersionedBulletsCount = READ_IF_EXISTS(pSettings, r_u8, section, "base_dispersioned_bullets_count", 0);
	m_fBaseDispersionedBulletsSpeed = READ_IF_EXISTS(pSettings, r_float, section, "base_dispersioned_bullets_speed", m_fStartBulletSpeed);

	if (pSettings->line_exist(section, "fire_modes"))
	{
		shared_str FireModesList = pSettings->r_string(section, "fire_modes");
		int ModesCount = _GetItemCount(FireModesList.c_str());
		m_aFireModes.clear();
		
		for (int i=0; i<ModesCount; i++)
		{
			string16 sItem;
			_GetItem(FireModesList.c_str(), i, sItem);
			m_aFireModes.push_back	((s8)atoi(sItem));
		}
		
		m_iCurFireMode = ModesCount - 1;
		m_iPrefferedFireMode = READ_IF_EXISTS(pSettings, r_s16,section,"preffered_fire_mode",-1);
	}

	LoadSilencerKoeffs();

	if (WeaponSoundExist(section, "snd_changefiremode") && m_bUseChangeFireModeAnim)
		m_sounds.LoadSound(section, "snd_changefiremode", "sndFireMode", true, m_eSoundShow);

	if (pSettings->line_exist(hud_sect, "mask_firemode_1"))
		m_sFireModeMask_1 = pSettings->r_string(hud_sect, "mask_firemode_1");

	if (pSettings->line_exist(hud_sect, "mask_firemode_3"))
		m_sFireModeMask_3 = pSettings->r_string(hud_sect, "mask_firemode_3");

	if (pSettings->line_exist(hud_sect, "mask_firemode_a"))
		m_sFireModeMask_a = pSettings->r_string(hud_sect, "mask_firemode_a");

	if (pSettings->line_exist(hud_sect, "firemode_bones_total"))
	{
		m_sFireModeBonesTotal.clear();
		LPCSTR str = pSettings->r_string(hud_sect, "firemode_bones_total");
		for (int i = 0, count = _GetItemCount(str); i < count; ++i)
		{
			string128 bone_name;
			_GetItem(str, i, bone_name);
			m_sFireModeBonesTotal.push_back(bone_name);
		}

		m_sFireModeBone_1.clear();
		if (pSettings->line_exist(hud_sect, "firemode_bones_1"))
		{
			LPCSTR str = pSettings->r_string(hud_sect, "firemode_bones_1");
			for (int i = 0, count = _GetItemCount(str); i < count; ++i)
			{
				string128 bone_name;
				_GetItem(str, i, bone_name);
				m_sFireModeBone_1.push_back(bone_name);
			}
		}

		m_sFireModeBone_3.clear();
		if (pSettings->line_exist(hud_sect, "firemode_bones_3"))
		{
			LPCSTR str = pSettings->r_string(hud_sect, "firemode_bones_3");
			for (int i = 0, count = _GetItemCount(str); i < count; ++i)
			{
				string128 bone_name;
				_GetItem(str, i, bone_name);
				m_sFireModeBone_3.push_back(bone_name);
			}
		}

		m_sFireModeBone_a.clear();
		if (pSettings->line_exist(hud_sect, "firemode_bones_a"))
		{
			LPCSTR str = pSettings->r_string(hud_sect, "firemode_bones_a");
			for (int i = 0, count = _GetItemCount(str); i < count; ++i)
			{
				string128 bone_name;
				_GetItem(str, i, bone_name);
				m_sFireModeBone_a.push_back(bone_name);
			}
		}
	}
}

bool CWeaponMagazined::OnShoot_CanShootNow()
{
	if (ParentIsActor() && (Actor()->IsActorPlanningSuicide() || Actor()->IsActorSuicideNow()))
		return Actor()->IsSuicideInreversible();

	if (IsActionProcessing())
	{
		if (OnActWhileReload_CanActNow())
			return true;

		xr_string cur_param = "autoshoot_" + GetActualCurrentAnim();
		if (lock_time > 0 && pSettings->line_exist(HudSection(), cur_param.c_str()) && pSettings->r_bool(HudSection(), cur_param.c_str()))
		{
			Actor()->SetActorKeyRepeatFlag(kfFIRE, true);
			return false;
		}
		else
			return false;
	}
	else
	{
		if (ParentIsActor() && Actor()->GetMovementState(eReal) & mcSprint)
		{
			//Add sprint stopping anm
			Actor()->SetMovementState(eWishful, mcSprint, false);
			Actor()->SetActorKeyRepeatFlag(kfFIRE, true);
			return false;
		}
	}

	return true;
}

void CWeaponMagazined::FireStart()
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if(!IsMisfire())
	{
		if (iAmmoElapsed)
		{
			if(!IsWorking() || AllowFireWhileWorking())
			{
				if (!OnActWhileReload_CanActNow())
				{
					if (GetState() == eReload) return;
					if (GetState() == eShowing) return;
					if (GetState() == eHiding) return;
					if (GetState() == eMisfire) return;
					if (!OnShoot_CanShootNow()) return;
				}
				else if (!Action_PrepareEarlyShotInReload())
					return;

				inherited::FireStart();
				SwitchState(eFire);
			}
		}
		else 
		{
			if (isGuns)
			{
				if (GetState() == eEmptyClick && !lock_time || GetState() == eIdle)
					SwitchState(eEmptyClick);
			}
			else
				OnEmptyClick();
		}
	}
	else
	{
		if (GetState() != eIdle)
			return;

		//misfire
		CGameObject* object = smart_cast<CGameObject*>(H_Parent());
		if (object)
			object->callback(GameObject::eOnWeaponJammed)(object->lua_game_object(), this->lua_game_object());

		if(smart_cast<CActor*>(this->H_Parent()) && (Level().CurrentViewEntity()==H_Parent()) )
			CurrentGameUI()->AddCustomStatic("gun_jammed",true);

		if (isGuns)
		{
			SwitchState(eCheckMisfire);
			return;
		}

		OnEmptyClick();
	}
}

void CWeaponMagazined::FireEnd() 
{
	inherited::FireEnd();

	if (EngineExternal()[EEngineExternalGame::EnableAutoreload])
	{
		CActor	*actor = smart_cast<CActor*>(H_Parent());
		if(m_pInventory && !iAmmoElapsed && actor && GetState() != eReload)
			TryReload();
	}
}

bool CWeaponMagazined::TryReload() 
{
	if (IsTriStateReload())
	{
		if (m_magazine.size() == (u32)iMagazineSize || !HaveCartridgeInInventory(1))
			return false;

		m_sub_state = eSubstateReloadBegin;
		SwitchState(eReload);
		return true;
	}

	if(m_pInventory) 
	{
		if(IsGameTypeSingle() && ParentIsActor())
		{
			int	AC					= GetSuitableAmmoTotal();
			Actor()->callback(GameObject::eWeaponNoAmmoAvailable)(lua_game_object(), AC);
		}

		m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny( m_ammoTypes[m_ammoType].c_str() ));

		if (m_pCurrentAmmo || unlimited_ammo())
		{
			SetPending(TRUE);
			SwitchState(eReload);
			return true;
		}
		else if (m_set_next_ammoType_on_reload == undefined_ammo_type && iAmmoElapsed == 0 || m_set_next_ammoType_on_reload != undefined_ammo_type)
		{
			for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
			{
				m_pCurrentAmmo = smart_cast<CWeaponAmmo*>(m_pInventory->GetAny(m_ammoTypes[i].c_str()));
				if (m_pCurrentAmmo)
				{
					m_set_next_ammoType_on_reload = i;
					SetPending(TRUE);
					SwitchState(eReload);
					return true;
				}
			}
		}
		else
			m_set_next_ammoType_on_reload = undefined_ammo_type;
	}
	
	if(GetState()!=eIdle)
		SwitchState(eIdle);

	bAmmotypeKeyPressed = false;
	bReloadKeyPressed = false;

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
	
	while (!is_firstlast_ammo_swapped && !m_magazine.empty() || is_firstlast_ammo_swapped && m_magazine.size() > 1)
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

	_wanim_force_assign = true;
}

void CWeaponMagazined::SwapFirstLastAmmo()
{
	if (IsGrenadeLauncherAttached() && IsGrenadeMode())
		return;

	int cnt = m_magazine.size();
	if (cnt > 1)
	{
		cnt -= 1;
		CCartridge* cs = GetCartridgeFromMagVector(0);
		CCartridge* ce = GetCartridgeFromMagVector(cnt);
		CCartridge tmp;
		CopyCartridge(*cs, tmp);
		CopyCartridge(*ce, *cs);
		CopyCartridge(tmp, *ce);
	}
}

void CWeaponMagazined::SwapLastPrevAmmo()
{
	if (IsGrenadeLauncherAttached() && IsGrenadeMode())
		return;

	int cnt = m_magazine.size();
	if (cnt > 1)
	{
		cnt -= 1;
		CCartridge* cs = GetCartridgeFromMagVector(cnt - 1);
		CCartridge* ce = GetCartridgeFromMagVector(cnt);
		CCartridge tmp;
		CopyCartridge(*cs, tmp);
		CopyCartridge(*ce, *cs);
		CopyCartridge(tmp, *ce);
	}
}

void CWeaponMagazined::CopyCartridge(CCartridge & src, CCartridge & dst)
{
	std::memcpy(&dst, &src, sizeof(CCartridge));
}

void CWeaponMagazined::PerformUnloadAmmo()
{
	is_firstlast_ammo_swapped = false;

	if (!IsGrenadeMode() && m_bAmmoInChamber)
	{
		SwapFirstLastAmmo();
		is_firstlast_ammo_swapped = true;
	}

    bool need_unload = false;
    int cnt = 0;

    if (IsGrenadeMode())
	{
        cnt = GetAmmoInGLCount();
        if (cnt > 0)
		{
            for (int i = 0; i < cnt; ++i)
			{
                if (GetCartridgeType(GetGrenadeCartridgeFromGLVector(i)) != GetAmmoTypeIndex())
				{
                    need_unload = true;
                    break;
                }
            }
        }
    }
	else
	{
        cnt = m_magazine.size();
        if (cnt > 0)
		{
            for (int i = 0; i < cnt; ++i)
			{
                if (GetCartridgeType(GetCartridgeFromMagVector(i)) != GetAmmoTypeIndex())
				{
                    need_unload = true;
                    break;
                }
            }
        }
    }

    if (need_unload)
        UnloadMagazine();
}

void CWeaponMagazined::ReloadMagazine() 
{
	m_BriefInfo_CalcFrame = 0;	
	
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
	if (!m_bLockType && !m_magazine.empty())
		PerformUnloadAmmo();

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

	if (is_firstlast_ammo_swapped)
	{
		is_firstlast_ammo_swapped = false;
		SwapFirstLastAmmo();
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());
}

bool CWeaponMagazined::HaveCartridgeInInventory(u8 cnt)
{
	if (unlimited_ammo())
		return true;

	if (!m_pInventory)
		return false;

	u32 ac = GetAmmoCount(m_ammoType);

	if (m_set_next_ammoType_on_reload != undefined_ammo_type || m_set_next_ammoType_on_reload == undefined_ammo_type && m_magazine.size() == 0 && ac == 0)
	{
		for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
		{
			if (m_ammoType == i)
				continue;

			if (ac = GetAmmoCount(i) >= cnt)
			{
				m_set_next_ammoType_on_reload = i;
				break;
			}
			else
				m_set_next_ammoType_on_reload = undefined_ammo_type;
		}
	}

	if (ac < cnt)
	{
		bAmmotypeKeyPressed = false;
		bReloadKeyPressed = false;
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

	SwapLastPrevAmmo();

	return cnt;
}

void CWeaponMagazined::OnStateSwitch	(u32 S)
{
	inherited::OnStateSwitch(S);
	CInventoryOwner* owner = smart_cast<CInventoryOwner*>(this->H_Parent());
	switch (S)
	{
	case eIdle:
		switch2_Idle	();
		break;
	case eFire:
		switch2_Fire	();
		break;
	case eMisfire:
		if(smart_cast<CActor*>(this->H_Parent()) && (Level().CurrentViewEntity()==H_Parent()) )
			CurrentGameUI()->AddCustomStatic("gun_jammed", true);
		break;
	case eUnjam:
	case eReload:
		if(owner)
			m_sounds_enabled = owner->CanPlayShHdRldSounds();
		switch2_Reload	();
		break;
	case eShowing:
		if(owner)
			m_sounds_enabled = owner->CanPlayShHdRldSounds();
		switch2_Showing	();
		break;
	case eHiding:
		if(owner)
			m_sounds_enabled = owner->CanPlayShHdRldSounds();
		switch2_Hiding	();
		break;
	case eHidden:
		switch2_Hidden	();
		break;
	case eSwitchMode:
		switch2_FireMode();
		break;
	case eEmptyClick:
		switch2_Empty();
		break;
	case eCheckMisfire:
		switch2_CheckMisfire();
		break;
	case eKick:
		switch2_Kick();
		break;
	}
}

xr_string CWeaponMagazined::NeedAddSuffix(const xr_string& M)
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	xr_string new_name = M;

	if (IsZoomed())
		new_name = AddSuffixName(new_name, "_aim");

	if (IsScopeAttached())
		new_name = AddSuffixName(new_name, "_scope");

	int firemode = GetQueueSize();

	if (firemode == -1 && m_sFireModeMask_a != nullptr)
		new_name = AddSuffixName(new_name, m_sFireModeMask_a.c_str());
	else if (firemode == 1 && m_sFireModeMask_1 != nullptr)
		new_name = AddSuffixName(new_name, m_sFireModeMask_1.c_str());
	else if (firemode == 3 && m_sFireModeMask_3 != nullptr)
		new_name = AddSuffixName(new_name, m_sFireModeMask_3.c_str());

	if ((Actor()->IsActorSuicideNow() || Actor()->IsSuicideInreversible()) && READ_IF_EXISTS(pSettings, r_bool, HudSection(), "custom_suicide_shot", false))
		new_name = AddSuffixName(new_name, "_suicide");

	if (!IsMisfire() && iAmmoElapsed == 1)
		new_name = AddSuffixName(new_name, isGuns ? "_last" : "_l");

	if (!IsMisfire() && iAmmoElapsed == 0)
		new_name = AddSuffixName(new_name, "_empty");

	if (IsChangeAmmoType())
		new_name = AddSuffixName(new_name, "_ammochange");

	if (IsMisfire())
	{
		if (isGuns)
			new_name = AddSuffixName(new_name, "_jammed");
		else
			new_name = AddSuffixName(new_name, "_misfire");

		if (iAmmoElapsed == 0)
			new_name = AddSuffixName(new_name, "_last");
	}

	if (Actor()->GetDetector())
		new_name = AddSuffixName(new_name, "_detector");

	if (IsSilencerAttached())
		new_name = AddSuffixName(new_name, "_sil");

	if (!IsScopeAttached())
		new_name = AddSuffixName(new_name, "_noscope");

	return new_name;
}

void CWeaponMagazined::UpdateCL			()
{
	inherited::UpdateCL	();
	float dt = Device.fTimeDelta;

	

	//когда происходит апдейт состояния оружия
	//ничего другого не делать
	if(GetNextState() == GetState())
	{
		switch (GetState())
		{
		case eShowing:
		case eShowingDet:
		case eShowingEndDet:
		case eHiding:
		case eHideDet:
		case eReload:
		case eSwitchMode:
		case eEmptyClick:
		case eUnjam:
		case eCheckMisfire:
		case eSprintStart:
		case eSprintEnd:
		case eIdle:
		case eKick:
			{
				fShotTimeCounter	-=	dt;
				clamp				(fShotTimeCounter, 0.0f, flt_max);
			}break;
		case eFire:			
			{
				state_Fire(dt);
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
	{
		m_sounds.SetPosition("sndShow", P);
		if (m_sounds.FindSoundItem("sndByMotion", false))
			m_sounds.SetPosition("sndByMotion", P);
	}
	else if (Device.dwFrame % 3 == 1)
	{
		m_sounds.SetPosition("sndReload", P);
		m_sounds.SetPosition("sndHide", P);
	}
	else if (Device.dwFrame % 3 == 2)
	{
		if (m_sounds.FindSoundItem("sndReloadEmpty", false))
			m_sounds.SetPosition("sndReloadEmpty", P);
		if (m_sounds.FindSoundItem("sndReloadJammed", false))
			m_sounds.SetPosition("sndReloadJammed", P);
	}
}

void CWeaponMagazined::state_Fire(float dt)
{
	if (iAmmoElapsed > 0)
	{
		VERIFY(fOneShotTime > 0.f);

		Fvector p1, d;
		p1.set(get_LastFP());
		d.set(get_LastFD());

		if (!H_Parent()) return;
		if (smart_cast<CMPPlayersBag*>(H_Parent()) != nullptr)
		{
			Msg("! WARNING: state_Fire of object [%d][%s] while parent is CMPPlayerBag...", ID(), cNameSect().c_str());
			return;
		}

		CInventoryOwner* io = smart_cast<CInventoryOwner*>(H_Parent());
		if (nullptr == io->inventory().ActiveItem())
		{
			Msg("current_state %d", GetState() );
			Msg("next_state %d", GetNextState());
			Msg("item_sect %s", cNameSect().c_str());
			Msg("H_Parent %s", H_Parent()->cNameSect().c_str());
			StopShooting();
			return;
		}

		CEntity* E = smart_cast<CEntity*>(H_Parent());
		E->g_fireParams(this, p1, d);

		if (!E->g_stateFire())
			StopShooting();

		if (m_iShotNum == 0)
		{
			m_vStartPos = p1;
			m_vStartDir = d;
		};

		VERIFY(!m_magazine.empty());

		while (!m_magazine.empty() && fShotTimeCounter < 0 && (IsWorking() || m_bFireSingleShot) && (m_iQueueSize < 0 || m_iShotNum < m_iQueueSize))
		{
			if (m_bJamNotShot)
			{
				if (CheckForMisfire())
				{
					OnShotJammed();
					SwitchState(eIdle);
					return;
				}
			}

			m_bFireSingleShot = false;

			fShotTimeCounter += fOneShotTime;

			++m_iShotNum;

			if (!m_bJamNotShot)
				CheckForMisfire();

			OnShot();

			if (m_iShotNum > m_iBaseDispersionedBulletsCount)
				FireTrace(p1, d);
			else
				FireTrace(m_vStartPos, m_vStartDir);

			if (!m_bJamNotShot && IsMisfire())
			{
				StopShooting();
				return;
			}
		}

		if (m_iShotNum == m_iQueueSize)
			m_bStopedAfterQueueFired = true;
	}

	if (fShotTimeCounter < 0)
	{
		if (iAmmoElapsed == 0)
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


void CWeaponMagazined::OnShot()
{
	// Sound
	PlaySound(m_sSndShotCurrent.c_str(), get_LastFP(), true);

	if (!IsMisfire())
	{
		if (m_sounds.FindSoundItem("sndPump", false))
			PlaySound("sndPump", get_LastFP());
	}
	else
	{
		if (m_sounds.FindSoundItem("sndJam", false))
			PlaySound("sndJam", get_LastFP());
	}
	
	// Camera	
	AddShotEffector				();

	// Animation
	PlayAnimShoot				();
	
	// Shell Drop
	Fvector vel;
	PHGetLinearVell(vel);
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (!isGuns)
		OnShellDrop(get_LastSP(), vel);
	
	// Огонь из ствола
	StartFlameParticles			();

	//дым из ствола
	ForceUpdateFireParticles	();
	StartSmokeParticles			(get_LastFP(), vel);

	
	CGameObject* object = smart_cast<CGameObject*>(H_Parent());
	if (object)
		object->callback(GameObject::eOnWeaponFired)(object->lua_game_object(), this->lua_game_object(), iAmmoElapsed, m_ammoType);
}

void CWeaponMagazined::OnShotJammed()
{
	if (m_sounds.FindSoundItem("sndJam", false))
		PlaySound("sndJam", get_LastFP());

	PlayAnimShoot();
}

void CWeaponMagazined::OnEmptyClick	()
{
	PlaySound	("sndEmptyClick",get_LastFP());
}

void CWeaponMagazined::DoReload()
{
	if (IsReloaded)
	{
		IsReloaded = false;
		return;
	}

	if (IsGrenadeLauncherAttached() && IsGrenadeMode())
	{
		ReloadMagazine();
		return;
	}

	int def_magsize = GetMagCapacity();
	int mod_magsize = def_magsize;

	CWeaponBM16* bm = smart_cast<CWeaponBM16*>(this);

	if (bm)
		mod_magsize = ammo_cnt_to_reload;
	else if (!IsGrenadeMode() && m_bAmmoInChamber && GetAmmoElapsed() == 0)
		mod_magsize = def_magsize - 1;

	iMagazineSize = mod_magsize;
	ReloadMagazine();
	iMagazineSize = def_magsize;
}

void CWeaponMagazined::TriStateEnd()
{
	switch (m_sub_state)
	{
	case eSubstateReloadBegin:
	{
		if (bStopReloadSignal || iAmmoElapsed == iMagazineSize)
			m_sub_state = eSubstateReloadEnd;
		else
			m_sub_state = eSubstateReloadInProcess;

		SwitchState(eReload);
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
		SwitchState(eIdle);
	}break;
	};
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
			else
			{
				TriStateEnd();
				return;
			}
			DoReload();
			SwitchState(eIdle);
		} break;
		case eUnjam:
			SetMisfireStatus(false);
			bUnjamKeyPressed = false;
			SwitchState(eIdle);
		break;
		case eHiding:
			SwitchState(eHidden);  
		break;
		case eIdle:
			switch2_Idle();
		break;
		case eSwitchMode:
			bNextModeKeyPressed = false;
			bPrevModeKeyPressed = false;
			SwitchState(eIdle);
		break;
		case eEmptyClick:
		case eShowing:
		case eCheckMisfire:
		case eFire2:
		case eFire:
		case eKick:
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
#include "object_handler_planner.h"
#endif
void CWeaponMagazined::switch2_Fire	()
{
	CInventoryOwner* io		= smart_cast<CInventoryOwner*>(H_Parent());
	CInventoryItem* ii		= smart_cast<CInventoryItem*>(this);
#ifdef DEBUG
	if (!io)
		return;
	//VERIFY2					(io,make_string("no inventory owner, item %s",*cName()));

	if (ii != io->inventory().ActiveItem())
		Msg					("! not an active item, item %s, owner %s, active item %s",*cName(),*H_Parent()->cName(),io->inventory().ActiveItem() ? *io->inventory().ActiveItem()->object().cName() : "no_active_item");

	if ( !(io && (ii == io->inventory().ActiveItem())) ) 
	{
		CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(H_Parent());
		if (stalker) {
			stalker->planner().show						();
			stalker->planner().show_current_world_state	();
			stalker->planner().show_target_world_state	();
		}
	}
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
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	if (EngineExternal()[EEngineExternalGame::EnableAutoreload] && !isGuns)
		OnZoomOut();

	if (EngineExternal()[EEngineExternalGame::EnableAutoreload])
	{
		if (!IsTriStateReload())
		{
			if (!TryReload())
				OnEmptyClick();
			else
				inherited::FireEnd();
		}
		else
		{
			if (!HaveCartridgeInInventory(1))
				OnEmptyClick();
			else
			{
				inherited::FireEnd();
				TryReload();
			}
		}
	}
	else
	{
		OnEmptyClick();
		if (isGuns)
			PlayAnimFakeshoot();
	}
}

void CWeaponMagazined::switch2_CheckMisfire()
{
	SetPending(TRUE);

	PlayAnimFakeshoot();

	if (m_sounds.FindSoundItem("sndJammedClick", false))
		PlaySound("sndJammedClick", get_LastFP());
}

void CWeaponMagazined::KickCallback()
{
	MakeWeaponKick(Device.vCameraPosition, Device.vCameraDirection);
	MakeLockByConfigParam("lock_time_end_" + GetActualCurrentAnim());
}

void CWeaponMagazined::switch2_Kick()
{
	SetPending(TRUE);
	PlaySound("sndKick", get_LastFP());
	PlayHUDMotion("anm_kick", TRUE, eKick);
	MakeLockByConfigParam("lock_time_start_" + GetActualCurrentAnim(), false, { CHudItem::TAnimationEffector(this, &CWeaponMagazined::KickCallback) });
}

void CWeaponMagazined::PlayAnimFakeshoot()
{
	xr_string anm_name = "anm_fakeshoot";
	int firemode = GetQueueSize();

	if (IsZoomed())
		anm_name += "_aim";

	if (firemode == -1 && m_sFireModeMask_a != nullptr)
		anm_name += m_sFireModeMask_a.c_str();
	else if (firemode == 1 && m_sFireModeMask_1 != nullptr)
		anm_name += m_sFireModeMask_1.c_str();
	else if (firemode == 3 && m_sFireModeMask_3 != nullptr)
		anm_name += m_sFireModeMask_3.c_str();

	PlayHUDMotion(anm_name, TRUE, GetState());

	anm_name = GetActualCurrentAnim();
	anm_name.erase(0, 4);

	StartCompanionAnimIfNeeded(anm_name);
}

void CWeaponMagazined::PlayReloadSound()
{
	if(!m_sounds_enabled)
		return;

	if (IsMisfire())
	{
		if (m_sounds.FindSoundItem("sndReloadJammedLast", false) && iAmmoElapsed == 0)
			PlaySound("sndReloadJammedLast", get_LastFP());
		else if (m_sounds.FindSoundItem("sndReloadJammed", false))
			PlaySound("sndReloadJammed", get_LastFP());
	}
	else if (m_sounds.FindSoundItem("sndReloadEmpty", false) && iAmmoElapsed == 0)
		PlaySound("sndReloadEmpty", get_LastFP());
	else
	{
		if (IsChangeAmmoType() && m_sounds.FindSoundItem("sndChangeCartridgeType", false))
			PlaySound("sndChangeCartridgeType", get_LastFP());
		else
			PlaySound("sndReload", get_LastFP());
	}
}

void CWeaponMagazined::TriStateReload()
{
	CWeapon::OnStateSwitch(GetState());

	if (m_magazine.size() == iMagazineSize || !HaveCartridgeInInventory(1))
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
			if (EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode])
			{
				if (iAmmoElapsed == 0 && m_bAddCartridgeOpen || !bPreloadAnimAdapter)
					AddCartridge(1);
			}
		}
	}break;
	case eSubstateReloadInProcess:
	{
		if (HaveCartridgeInInventory(1))
			switch2_AddCartridge();
	}break;
	case eSubstateReloadEnd:
		switch2_EndReload();
		break;
	};
}

void CWeaponMagazined::switch2_StartReload()
{
	PlayAnimOpenWeapon();
	SetPending(TRUE);

	if (m_sounds.FindSoundItem("sndOpenEmpty", false) && m_bAddCartridgeOpen && iAmmoElapsed == 0)
		PlaySound("sndOpenEmpty", get_LastFP());
	else
		PlaySound("sndOpen", get_LastFP());
}

void CWeaponMagazined::switch2_AddCartridge()
{
	PlayAnimAddOneCartridgeWeapon();
	SetPending(TRUE);

	if (m_sounds.FindSoundItem("sndAddCartridgeEmpty", false) && !m_bAddCartridgeOpen && iAmmoElapsed == 0)
		PlaySound("sndAddCartridgeEmpty", get_LastFP());
	else if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
		PlaySound("sndAddCartridgePreloaded", get_LastFP());
	else
		PlaySound("sndAddCartridge", get_LastFP());
}

void CWeaponMagazined::switch2_EndReload()
{
	SetPending(TRUE);
	PlayAnimCloseWeapon();

	if (m_sounds.FindSoundItem("sndCloseEmpty", false) && !m_bAddCartridgeOpen && iAmmoElapsed == 0)
		PlaySound("sndCloseEmpty", get_LastFP());
	else if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
		PlaySound("sndClosePreloaded", get_LastFP());
	else
		PlaySound("sndClose", get_LastFP());
}

void CWeaponMagazined::PlayAnimOpenWeapon()
{
	VERIFY(GetState() == eReload);

	xr_string anm_name = "anm_open";

	if (m_bEmptyPreloadMode && iAmmoElapsed == 0)
	{
		anm_name += "_empty";
		bPreloadAnimAdapter = true;
	}

	PlayHUDMotion(anm_name, false, GetState(), false, false);
}

void CWeaponMagazined::PlayAnimAddOneCartridgeWeapon()
{
	VERIFY(GetState() == eReload);

	xr_string anm_name = "anm_add_cartridge";

	if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
	{
		if (iAmmoElapsed == 0)
			anm_name += "_empty_preloaded";
		else
			anm_name += "_preloaded";

		bPreloadAnimAdapter = false;
	}
	else if (!m_bAddCartridgeOpen && iAmmoElapsed == 0 && HudAnimationExist("anm_add_cartridge_empty"))
		anm_name += "_empty";

	PlayHUDMotion(anm_name, false, GetState(), false, false);
}

void CWeaponMagazined::PlayAnimCloseWeapon()
{
	VERIFY(GetState() == eReload);

	xr_string anm_name = "anm_close";

	if (m_bEmptyPreloadMode && bPreloadAnimAdapter)
	{
		if (iAmmoElapsed == 0)
			anm_name = "anm_add_cartridge_empty_preloaded";
		else
			anm_name += "_preloaded";

		bPreloadAnimAdapter = false;
	}
	else if (!m_bAddCartridgeOpen && iAmmoElapsed == 0 && HudAnimationExist("anm_add_cartridge_empty"))
		anm_name = "anm_add_cartridge_empty";

	PlayHUDMotion(anm_name, false, GetState(), false, false);
}

void CWeaponMagazined::switch2_Reload()
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (IsMisfire() && !isGuns && !HudAnimationExist("anm_reload_misfire") || !IsMisfire())
	{
		if (IsTriStateReload())
		{
			TriStateReload();
			return;
		}
	}

	IsReloaded = false;
	PlayAnimReload();
	PlayReloadSound();
	SetPending(TRUE);
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

	bReloadKeyPressed = false;
	bAmmotypeKeyPressed = false;
	bUnjamKeyPressed = false;
	bNextModeKeyPressed = false;
	bPrevModeKeyPressed = false;
}

void CWeaponMagazined::switch2_FireMode()
{
	if (m_sounds_enabled && m_sounds.FindSoundItem("sndFireMode", false))
		PlaySound("sndFireMode", get_LastFP());

	SetPending(TRUE);
	PlayAnimFireMode();
}

bool CWeaponMagazined::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;
	
	//если оружие чем-то занято, то ничего не делать
	if(IsPending())
		return false;
	
	if (GetState() != eIdle)
		return false;

	switch(cmd) 
	{
	case kWPN_RELOAD:
		{
			if (!(flags&CMD_START))
				return false;

			if (IsZoomed())
				return false;

			if (!bUnjamKeyPressed && !bReloadKeyPressed && !bAmmotypeKeyPressed)
			{
				if (IsMisfire())
					bUnjamKeyPressed = true;
				else
					bReloadKeyPressed = true;
			}
			else
				return false;

			if (Actor()->GetDetector() && Actor()->GetDetector()->GetState() != CCustomDetector::eIdle)
				return false;

			if (IsMisfire() && !IsGrenadeMode())
			{
				SwitchState(eUnjam);
				return true;
			}

			if (iAmmoElapsed == iMagazineSize)
			{
				bReloadKeyPressed = false;
				return false;
			}

			return TryReload();

		}break;
	case kWPN_FIREMODE_PREV:
	case kWPN_FIREMODE_NEXT:
	{
		return ChangeFiremode(cmd, flags);
	}break;
	}
	return false;
}

bool CWeaponMagazined::CanAttach(PIItem pIItem)
{
	CScope*				pScope				= smart_cast<CScope*>(pIItem);
	CSilencer*			pSilencer			= smart_cast<CSilencer*>(pIItem);
	CGrenadeLauncher*	pGrenadeLauncher	= smart_cast<CGrenadeLauncher*>(pIItem);

	if (pScope && m_eScopeStatus == ALife::eAddonAttachable)
	{
		if (IsScopeAttached() && pIItem->object().cNameSect() == GetScopeName())
			return false;

		return ScopeFit(pScope);
	}
	else if(	pSilencer &&
				m_eSilencerStatus == ALife::eAddonAttachable &&
				(m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonSilencer) == 0 &&
				(m_sSilencerName == pIItem->object().cNameSect()) )
       return true;
	else if (	pGrenadeLauncher &&
				m_eGrenadeLauncherStatus == ALife::eAddonAttachable &&
				(m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) == 0 &&
				(m_sGrenadeLauncherName  == pIItem->object().cNameSect()) )
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
			if(pSettings->r_string((*it),"scope_name")==item_section_name)
				return true;
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
			Detach(GetScopeName().c_str(), true);

		SCOPES_VECTOR_IT it = m_scopes.begin();
		for(; it!=m_scopes.end(); it++)
		{
			if(pSettings->r_string((*it),"scope_name")==pIItem->object().cNameSect())
				m_cur_scope = u8(it-m_scopes.begin());
		}
		m_flagsAddOnState |= CSE_ALifeItemWeapon::eWeaponAddonScope;
		result = true;
	}
	else if (pSilencer && m_eSilencerStatus == ALife::eAddonAttachable && (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonSilencer) == 0 && (m_sSilencerName == pIItem->object().cNameSect()))
	{
		if (m_bRestGL_and_Sil && IsGrenadeLauncherAttached())
			Detach(GetGrenadeLauncherName().c_str(), true);

		m_flagsAddOnState |= CSE_ALifeItemWeapon::eWeaponAddonSilencer;
		result = true;
	}
	else if (pGrenadeLauncher && m_eGrenadeLauncherStatus == ALife::eAddonAttachable && (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) == 0 && (m_sGrenadeLauncherName == pIItem->object().cNameSect()))
	{
		if (m_bRestGL_and_Sil && IsSilencerAttached())
			Detach(GetSilencerName().c_str(), true);

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

		ProcessUpgrade();
		ProcessScope();
		UpdateAddonsVisibility();
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
	for(; it!=m_scopes.end(); it++)
	{
		LPCSTR iter_scope_name = pSettings->r_string((*it),"scope_name");
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
	if(		m_eScopeStatus == ALife::eAddonAttachable &&
			DetachScope(item_section_name, b_spawn_item))
	{
		if ((m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonScope) == 0)
		{
			Msg("ERROR: scope addon already detached.");
			return true;
		}
		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonScope;
		
		ProcessUpgrade();
		ProcessScope();
		UpdateAddonsVisibility();
		InitAddons();

		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}
	else if(m_eSilencerStatus == ALife::eAddonAttachable &&
			(m_sSilencerName == item_section_name))
	{
		if ((m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonSilencer) == 0)
		{
			Msg("ERROR: silencer addon already detached.");
			return true;
		}
		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonSilencer;

		ProcessUpgrade();
		ProcessScope();
		UpdateAddonsVisibility();
		InitAddons();
		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}
	else if(m_eGrenadeLauncherStatus == ALife::eAddonAttachable &&
			(m_sGrenadeLauncherName == item_section_name))
	{
		if ((m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) == 0)
		{
			Msg("ERROR: grenade launcher addon already detached.");
			return true;
		}
		m_flagsAddOnState &= ~CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher;

		ProcessUpgrade();
		ProcessScope();
		UpdateAddonsVisibility();
		InitAddons();
		return CInventoryItemObject::Detach(item_section_name, b_spawn_item);
	}
	else
		return inherited::Detach(item_section_name, b_spawn_item);
}

void CWeaponMagazined::InitAddons()
{
	m_zoom_params.m_fIronSightZoomFactor = READ_IF_EXISTS( pSettings, r_float, cNameSect(), "ironsight_zoom_factor", 50.0f );
	if ( IsScopeAttached() )
	{
		shared_str scope_tex_name;
		if ( m_eScopeStatus == ALife::eAddonAttachable )
		{
			scope_tex_name						= pSettings->r_string(GetScopeName(), "scope_texture");
			m_zoom_params.m_fScopeZoomFactor	= pSettings->r_float( GetScopeName(), "scope_zoom_factor");
			m_zoom_params.m_sUseZoomPostprocess	= READ_IF_EXISTS(pSettings,r_string,GetScopeName(), "scope_nightvision", 0);
			m_zoom_params.m_bUseDynamicZoom		= READ_IF_EXISTS(pSettings,r_bool,GetScopeName(),"scope_dynamic_zoom",FALSE);
			m_zoom_params.m_sUseBinocularVision	= READ_IF_EXISTS(pSettings,r_string,GetScopeName(),"scope_alive_detector",0);
			m_fRTZoomFactor = m_zoom_params.m_fScopeZoomFactor;
			if ( m_UIScope )
			{
				xr_delete( m_UIScope );
			}

			if ( !g_dedicated_server )
			{
				m_UIScope				= new CUIWindow();
				createWpnScopeXML		();
				CUIXmlInit::InitWindow	(*pWpnScopeXml, scope_tex_name.c_str(), 0, m_UIScope);
			}
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

	if ( IsSilencerAttached())
	{		
		m_sFlameParticlesCurrent	= m_sSilencerFlameParticles;
		m_sSmokeParticlesCurrent	= m_sSilencerSmokeParticles;
		m_sSndShotCurrent			= "sndSilencerShot";

		//подсветка от выстрела
		LoadLights					(*cNameSect(), "silencer_");
		ApplySilencerKoeffs			();
	}
	else
	{
		m_sFlameParticlesCurrent	= m_sFlameParticles;
		m_sSmokeParticlesCurrent	= m_sSmokeParticles;
		m_sSndShotCurrent			= "sndShot";

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

void CWeaponMagazined::PlayAnimFireMode()
{
	VERIFY(GetState() == eSwitchMode);

	xr_string anm_name = "anm_changefiremode_from_";
	int cur_mode = GetQueueSize();
	int old_mode = m_iOldFireMode;
	if (old_mode < 0)
		anm_name += "a";
	else
		anm_name += xr_string::ToString(old_mode);

	anm_name += "_to_";

	if (cur_mode < 0)
		anm_name += "a";
	else
		anm_name += xr_string::ToString(cur_mode);


	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	if (Actor()->GetDetector() && isGuns)
	{
		if (iAmmoElapsed == 0)
			bIsNeedCallDet = true;
		else
			bIsNeedCallDet = false;
	}
	else
		bIsNeedCallDet = false;

	PlayHUDMotion(anm_name, TRUE, eSwitchMode);

	anm_name = GetActualCurrentAnim();
	anm_name.erase(0, 4);

	StartCompanionAnimIfNeeded(anm_name);
}

void CWeaponMagazined::PlayAnimShow()
{
	VERIFY(GetState()==eShowing);

	PlayHUDMotion("anm_show", FALSE, GetState());
	StartCompanionAnimIfNeeded("draw");
}

void CWeaponMagazined::PlayAnimHide()
{
	VERIFY(GetState()==eHiding);

	PlayHUDMotion("anm_hide", TRUE, GetState());
	StartCompanionAnimIfNeeded("hide");
}

void CWeaponMagazined::OnAmmoTimer()
{
	if (!ParentIsActor())
		return;

	IsReloaded = false;
	DoReload();
	IsReloaded = true;
	ProcessAmmo();
	ProcessAmmoGL();
	MakeLockByConfigParam("lock_time_end_" + GetActualCurrentAnim(), false);
}

void CWeaponMagazined::PlayAnimReload()
{
	VERIFY(GetState() == eReload);

	xr_string anm_name = "anm_reload";

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	if (IsMisfire() && isGuns)
		anm_name += "_jammed";

	bIsNeedCallDet = Actor()->GetDetector() && isGuns;

	PlayHUDMotion(anm_name, TRUE, GetState());

	MakeLockByConfigParam("lock_time_start_" + GetActualCurrentAnim(), false, { CHudItem::TAnimationEffector(this, &CWeaponMagazined::OnAmmoTimer) });
}

void CWeaponMagazined::ModifierMoving(xr_string& anim_name, const xr_string config_enabler_directions, const xr_string config_enabler_main) const
{
	if (!config_enabler_main.empty())
	{
		if (!pSettings->line_exist(hud_sect, config_enabler_main.c_str()) || !pSettings->r_bool(hud_sect, config_enabler_main.c_str()))
			return;
	}

	anim_name += "_moving";

	if (!pSettings->line_exist(hud_sect, config_enabler_directions.c_str()) || !pSettings->r_bool(hud_sect, config_enabler_directions.c_str()))
		return;

	u32 state = Actor()->GetMovementState(eReal);

	if (state & ACTOR_DEFS::EMoveCommand::mcFwd)
		anim_name += "_forward";
	else if (state & ACTOR_DEFS::EMoveCommand::mcBack)
		anim_name += "_back";

	if (state & ACTOR_DEFS::EMoveCommand::mcLStrafe)
		anim_name += "_left";
	else if (state & ACTOR_DEFS::EMoveCommand::mcRStrafe)
		anim_name += "_right";
}

void CWeaponMagazined::PlayAnimAim()
{
	CActor* actor = smart_cast<CActor*>(H_Parent());
	xr_string anm_name = "anm_idle_aim";

	if (actor && actor->AnyMove() && EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode])
	{
		bool use_scope_anims = ScopeAttachable() && IsScopeAttached() && READ_IF_EXISTS(pSettings, r_bool, GetCurrentScopeSection(), "use_scope_anims", false);
		if (m_bAimScopeAnims && use_scope_anims)
			anm_name += "_scope";

		ModifierMoving(anm_name, "enable_directions_" + anm_name);
	}

	PlayHUDMotion(anm_name, TRUE, GetState());
}

void CWeaponMagazined::PlaySoundAim(bool in)
{
	if (!m_sounds_enabled)
		return;

	if (in && m_sounds.FindSoundItem("sndAim", false))
		PlaySound("sndAim", get_LastFP());
	else if (m_sounds.FindSoundItem("sndAimOut", false))
		PlaySound("sndAimOut", get_LastFP());
}

void CWeaponMagazined::PlayAnimIdle()
{
	if (GetState() != eIdle)
		return;

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	if (IsZoomed())
	{
		if (isGuns)
		{
			if (!IsAimStarted)
			{
				IsAimStarted = true;
				PlayHUDMotion("anm_idle_aim_start", TRUE, GetState());
				return;
			}
		}
		PlayAnimAim();
	}
	else
	{
		if (isGuns)
		{
			if (IsAimStarted)
			{
				IsAimStarted = false;
				PlayHUDMotion("anm_idle_aim_end", TRUE, GetState());
				return;
			}
		}
		inherited::PlayAnimIdle();
	}
}

bool CWeaponMagazined::NeedShootMix() const
{
	return m_bMixAfterIdle && GetState() == eIdle || m_bMixAfterReload && GetState() == eReload || m_bMixAfterQueue && GetState() == eFire && GetQueueSize() < 0;
}

void CWeaponMagazined::PlayAnimShoot()
{
	VERIFY(GetState() == eFire);

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	PlayHUDMotion(isGuns ? "anm_shoot" : "anm_shots", NeedShootMix(), GetState());
	ProcessAmmo(true);
	MakeLockByConfigParam("lock_time_" + GetActualCurrentAnim(), true);

	xr_string anm_name = GetActualCurrentAnim();
	anm_name.erase(0, 4);

	StartCompanionAnimIfNeeded(anm_name);
}

void CWeaponMagazined::OnZoomIn			()
{
	inherited::OnZoomIn();

	if(GetState() == eIdle)
		PlayAnimIdle();

	CGameObject* object = smart_cast<CGameObject*>(H_Parent());
	if (object)
		object->callback(GameObject::eOnWeaponZoomIn)(object->lua_game_object(), this->lua_game_object());

	CActor* actor = smart_cast<CActor*>(H_Parent());
	if (actor)
	{
		CEffectorZoomInertion* effectorZoomInertion = smart_cast<CEffectorZoomInertion*>(actor->Cameras().GetCamEffector(eCEZoom));
		if (!effectorZoomInertion)
		{
			effectorZoomInertion = (CEffectorZoomInertion*)actor->Cameras().AddCamEffector(new CEffectorZoomInertion());
			effectorZoomInertion->Init(this);
		};

		effectorZoomInertion->SetRndSeed(actor->GetZoomRndSeed());
		R_ASSERT(effectorZoomInertion);
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

	CGameObject* object = smart_cast<CGameObject*>(H_Parent());
	if (object)
		object->callback(GameObject::eOnWeaponZoomOut)(object->lua_game_object(), this->lua_game_object());

	CActor* actor = smart_cast<CActor*>(H_Parent());
	if (actor)
		actor->Cameras().RemoveCamEffector(eCEZoom);
	
	PlaySoundAim(false);
}

//переключение режимов стрельбы одиночными и очередями
bool CWeaponMagazined::SwitchMode			()
{
	if(eIdle != GetState() || IsPending()) return false;

	if(SingleShotMode())
		m_iQueueSize = WEAPON_ININITE_QUEUE;
	else
		m_iQueueSize = 1;

	return true;
}
 
xr_string CWeaponMagazined::GetFiremodeSuffix() const
{
	if (GetQueueSize() < 0)
		return "a";
	else
		return xr_string::ToString(GetQueueSize());
}

bool CWeaponMagazined::ChangeFiremode(u16 cmd, u32 flags)
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	if (!HasFireModes())
		return false;
	else if (!(flags & CMD_START))
		return false;
	else if (bPrevModeKeyPressed || bNextModeKeyPressed)
		return false;
	else if (isGuns && IsZoomed())
		return false;

	if (m_bUseChangeFireModeAnim)
	{
		if (cmd == kWPN_FIREMODE_NEXT)
			bNextModeKeyPressed = true;
		else
			bPrevModeKeyPressed = true;

		if (Actor()->GetDetector() && Actor()->GetDetector()->GetState() != CCustomDetector::eIdle)
			return false;
	}

	m_iOldFireMode = m_iQueueSize;

	if (cmd == kWPN_FIREMODE_NEXT)
		m_iCurFireMode = (m_iCurFireMode + 1 + m_aFireModes.size()) % (int)m_aFireModes.size();
	else
		m_iCurFireMode = (m_iCurFireMode - 1 + m_aFireModes.size()) % (int)m_aFireModes.size();

	SetQueueSize(GetCurrentFireMode());

	if (m_bUseChangeFireModeAnim)
		SwitchState(eSwitchMode);

	return true;
}

void	CWeaponMagazined::OnH_A_Chield		()
{
	if (HasFireModes())
	{
		CActor	*actor = smart_cast<CActor*>(H_Parent());
		if (!actor) SetQueueSize(-1);
		else SetQueueSize(GetCurrentFireMode());
	};	
	inherited::OnH_A_Chield();
};

void	CWeaponMagazined::SetQueueSize			(int size)  
{
	m_iQueueSize = size; 
};

float	CWeaponMagazined::GetWeaponDeterioration	()
{
// modified by Peacemaker [17.10.08]
//	if (!m_bHasDifferentFireModes || m_iPrefferedFireMode == -1 || u32(GetCurrentFireMode()) <= u32(m_iPrefferedFireMode)) 
//		return inherited::GetWeaponDeterioration();
//	return m_iShotNum*conditionDecreasePerShot;
	return (m_iShotNum==1) ? conditionDecreasePerShot : conditionDecreasePerQueueShot;
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

bool CWeaponMagazined::GetBriefInfo(II_BriefInfo& info)
{
	VERIFY(m_pInventory);
	string32 int_str;

	int	ae = GetAmmoElapsed();
	xr_sprintf(int_str, "%d", ae);
	info.cur_ammo = int_str;

	if (HasFireModes())
	{
		if (m_iQueueSize == WEAPON_ININITE_QUEUE)
		{
			info.fire_mode = "A";
		}
		else
		{
			xr_sprintf(int_str, "%d", m_iQueueSize);
			info.fire_mode = int_str;
		}
	}
	else
		info.fire_mode = "";

	if (m_pInventory->ModifyFrame() <= m_BriefInfo_CalcFrame)
	{
		return false;
	}
	GetSuitableAmmoTotal();//update m_BriefInfo_CalcFrame
	info.grenade = "";

	u32 at_size = (u32)m_ammoTypes.size();
	if (unlimited_ammo() || at_size == 0)
	{
		info.fmj_ammo._set("--");
		info.ap_ammo._set("--");
	}
	else
	{
		//GetSuitableAmmoTotal(); //mp = all type
		int add_ammo_count = 0;
		for (int i = 0; i < at_size; i++) {
			if (m_ammoType == i) {
				xr_sprintf(int_str, "%d", GetAmmoCount(i));
				info.fmj_ammo = int_str;
			}
			else {
				add_ammo_count += GetAmmoCount(i);
			}
		}
		if (at_size > 1)
			xr_sprintf(int_str, "%d", add_ammo_count);
		else
			xr_sprintf(int_str, "%s", "");
		info.ap_ammo = int_str;
	}

	if (ae != 0 && m_magazine.size() != 0)
	{
		LPCSTR ammo_type = m_ammoTypes[m_magazine.back().m_LocalAmmoType].c_str();
		info.name = CStringTable().translate(pSettings->r_string(ammo_type, "inv_name_short"));
		info.icon = ammo_type;
	}
	else
	{
		LPCSTR ammo_type = m_ammoTypes[m_ammoType].c_str();
		info.name = CStringTable().translate(pSettings->r_string(ammo_type, "inv_name_short"));
		info.icon = ammo_type;
	}
	return true;
}

void CWeaponMagazined::UpdateAddonsVisibility()
{
	inherited::UpdateAddonsVisibility();

	IKinematics* pWeaponVisual = smart_cast<IKinematics*>(Visual());
	R_ASSERT(pWeaponVisual);

	pWeaponVisual->CalculateBones_Invalidate();

	auto ChangeBonesVisible = [&](const RStringVec& bones, bool status)
	{
		for (const shared_str& bone : bones)
		{
			u16 bone_id = pWeaponVisual->LL_BoneID(bone);

			if (bone_id != BI_NONE)
				pWeaponVisual->LL_SetBoneVisible(bone_id, status, TRUE);
		}
	};

	int firemode = GetQueueSize();
	ChangeBonesVisible(m_sFireModeBonesTotal, false);
	ChangeBonesVisible(m_sFireModeBone_1, !!(firemode == 1));
	ChangeBonesVisible(m_sFireModeBone_3, !!(firemode == 3));
	ChangeBonesVisible(m_sFireModeBone_a, !!(firemode == -1));

	pWeaponVisual->CalculateBones_Invalidate();
	pWeaponVisual->CalculateBones(TRUE);
}

void CWeaponMagazined::UpdateHUDAddonsVisibility()
{
	if (!GetHUDmode())
		return;

	inherited::UpdateHUDAddonsVisibility();

	auto ChangeBonesVisible = [&](const RStringVec& bones, bool status)
	{
		for (const shared_str& bone : bones)
		{
			HudItemData()->set_bone_visible(bone, status, TRUE);
		}
	};

	int firemode = GetQueueSize();
	ChangeBonesVisible(m_sFireModeBonesTotal, false);
	ChangeBonesVisible(m_sFireModeBone_1, !!(firemode == 1));
	ChangeBonesVisible(m_sFireModeBone_3, !!(firemode == 3));
	ChangeBonesVisible(m_sFireModeBone_a, !!(firemode == -1));
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
