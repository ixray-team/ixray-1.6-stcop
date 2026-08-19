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
#include "CustomDevice.h"
#include "WeaponRPG7.h"
#if USE_OLD_OBJECT_PLANNER
#include "Legacy/object_handler_planner.h"
#endif
#include "../xrScripts/script_callback_ex.h"
#include "../xrEngine/xr_input.h"
#include "HUDManager.h"
#include "Weapons/Components/WeaponAmmoBones.h"
#include "WeaponBinocularsVision.h"
#include "../xrEngine/GamepadService.h"

CUIXml* pWpnScopeXml = nullptr;

void CWeaponMagazined::net_Destroy()
{
	inherited::net_Destroy();
}

void CWeaponMagazined::Load(const char* section)
{
	inherited::Load(section);

	s32 mag_size = iMagazineSize;

	if (!m_mags_capacity.empty())
	{
		for (auto& capacity : m_mags_capacity)
		{
			if (capacity.second > mag_size)
			{
				mag_size = capacity.second;
			}
		}
	}

	mag_size += iChamberSize;

	for (s32 i = 0; i < mag_size; i++)
	{
		const shared_str name = shared_str().printf("snd_reload_%d", i);
		if (SoundExist(section, *name))
		{
			m_sounds.LoadSound(section, *name, *shared_str().printf("sndReloadR%d", i), false, m_eSoundReload);
		}
	}

	m_iBaseDispersionedBulletsCount = READ_IF_EXISTS(pSettings, r_u8, section, "base_dispersioned_bullets_count", 0);
	m_fBaseDispersionedBulletsSpeed = READ_IF_EXISTS(pSettings, r_float, section, "base_dispersioned_bullets_speed", m_fStartBulletSpeed);
	m_fBaseDispersionedBulletsTimeDelta = READ_IF_EXISTS(pSettings, r_float, section, "base_dispersioned_bullets_time_delta", 0.0f);
	m_fSingleShootsTimeDelta = READ_IF_EXISTS(pSettings, r_float, section, "singleshoots_time_delta", 0.0f);

	if (pSettings->line_exist(section, "fire_modes"))
	{
		shared_str FireModesList = pSettings->r_string(section, "fire_modes");
		u8 ModesCount = _GetItemCount(FireModesList.c_str());
		m_aFireModes.clear();

		for (u8 i = 0; i < ModesCount; i++)
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
		m_iCurFireMode = 0;
	}

	LoadSilencerKoeffs();
	
	m_vibration_time = READ_IF_EXISTS(pSettings, r_float, section, "vibration_time", 0.1f);
	m_vibration_factor_left = READ_IF_EXISTS(pSettings, r_float, section, "vibration_factor_left", 1.0f);
	m_vibration_factor_right = READ_IF_EXISTS(pSettings, r_float, section, "vibration_factor_right", 1.0f);

	m_trigger_effect_time = READ_IF_EXISTS(pSettings, r_float, section, "trigger_effect_time", 1.0f);
}

void CWeaponMagazined::LoadSounds(const char* section)
{
	inherited::LoadSounds(section);

	m_sounds.LoadSound(section, "snd_draw", "sndShow", false, m_eSoundShow);
	m_sounds.LoadSound(section, "snd_holster", "sndHide", false, m_eSoundHide);

	m_layered_sounds.LoadSound(section, "snd_shoot", "sndShot", false, m_eSoundShot, st_Shooting);
	if (SoundExist(section, "snd_shoot_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor, true);
		m_layered_sounds.LoadSound(section, "snd_shoot_actor", "sndShotActor", false, m_eSoundShot, st_Shooting);
	}

	m_layered_sounds.LoadSound(section, "snd_silncer_shot", "sndSilencerShot", false, m_eSoundShot, st_Shooting);
	if (SoundExist(section, "snd_silncer_shot_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_sil, true);
		m_layered_sounds.LoadSound(section, "snd_silncer_shot_actor", "sndSilencerShotActor", false, m_eSoundShot, st_Shooting);
	}

	if (SoundExist(section, "snd_shot_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_last, true);
		m_layered_sounds.LoadSound(section, "snd_shot_last", "sndShotLast", false, m_eSoundShot, st_Shooting);
	}

	if (SoundExist(section, "snd_shot_last_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_last, true);
		m_layered_sounds.LoadSound(section, "snd_shot_last_actor", "sndShotLastActor", false, m_eSoundShot, st_Shooting);
	}

	if (SoundExist(section, "snd_silencer_shot_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_last_sil, true);
		m_layered_sounds.LoadSound(section, "snd_silencer_shot_last", "sndSilencerShotLast", false, m_eSoundShot, st_Shooting);
	}

	if (SoundExist(section, "snd_silencer_shot_last_actor"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_last_sil, true);
		m_layered_sounds.LoadSound(section, "snd_silencer_shot_last_actor", "sndSilencerShotLastActor", false, m_eSoundShot, st_Shooting);
	}

	m_sounds.LoadSound(section, "snd_empty", "sndEmptyClick", true, m_eSoundEmptyClick);
	m_sounds.LoadSound(section, "snd_reload", "sndReload", true, m_eSoundReload);

	if (SoundExist(section, "snd_reload_empty"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_empty, true);
		m_sounds.LoadSound(section, "snd_reload_empty", "sndReloadEmpty", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam, true);
		m_sounds.LoadSound(section, "snd_reload_misfire", "sndReloadMis", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam, true);
		m_sounds.LoadSound(section, "snd_reload_jammed", "sndReloadMis", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last, true);
		m_sounds.LoadSound(section, "snd_reload_misfire_last", "sndReloadMisLast", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last, true);
		m_sounds.LoadSound(section, "snd_reload_jammed_last", "sndReloadMisLast", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_det, true);
		m_sounds.LoadSound(section, "snd_reload_misfire_detector", "sndReloadMisDet", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_det, true);
		m_sounds.LoadSound(section, "snd_reload_jammed_detector", "sndReloadMisDet", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_reload_misfire_last_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last_det, true);
		m_sounds.LoadSound(section, "snd_reload_misfire_last_detector", "sndReloadMisLastDet", true, m_eSoundReload);
	}
	else if (SoundExist(section, "snd_reload_jammed_last_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_jam_last_det, true);
		m_sounds.LoadSound(section, "snd_reload_jammed_last_detector", "sndReloadMisLastDet", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_changecartridgetype"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_reload_change, true);
		m_sounds.LoadSound(section, "snd_changecartridgetype", "sndChangeCartridgeType", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_aim_start"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_aim_start, true);
		m_sounds.LoadSound(section, "snd_aim_start", "sndAimStart", true, m_eSoundAim);
	}

	if (SoundExist(section, "snd_aim_end"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_aim_end, true);
		m_sounds.LoadSound(section, "snd_aim_end", "sndAimEnd", true, m_eSoundAimOut);
	}

	if (SoundExist(section, "snd_changefiremode"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_changefiremode, true);
		m_sounds.LoadSound(section, "snd_changefiremode", "sndChangeFiremode", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_laser_on"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_laser, true);
		m_sounds.LoadSound(section, "snd_laser_on", "sndLaserOn", true, m_eSoundEmptyClick);
		m_sounds.LoadSound(section, "snd_laser_off", "sndLaserOff", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_torch_on"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_tacticaltorch, true);
		m_sounds.LoadSound(section, "snd_torch_on", "sndTorchOn", true, m_eSoundEmptyClick);
		m_sounds.LoadSound(section, "snd_torch_off", "sndTorchOff", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_breechblock"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_breechblock, true);
		m_sounds.LoadSound(section, "snd_breechblock", "sndPump", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_breechblock_aim"))
	{
		m_eSoundsFlags.set(ESoundsFlags2::sf_pump_aim, true);
		m_sounds.LoadSound(section, "snd_breechblock_aim", "sndPumpAim", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_breechblock_aim"))
	{
		m_eSoundsFlags.set(ESoundsFlags2::sf_pump_aim, true);
		m_sounds.LoadSound(section, "snd_breechblock_aim", "sndPumpAim", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_breechblock_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags2::sf_pump_last, true);
		m_sounds.LoadSound(section, "snd_breechblock_last", "sndPumpLast", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_breechblock_aim_last"))
	{
		m_eSoundsFlags.set(ESoundsFlags2::sf_pump_aim_last, true);
		m_sounds.LoadSound(section, "snd_breechblock_aim_last", "sndPumpAimLast", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_breechblock_empty"))
	{
		m_eSoundsFlags.set(ESoundsFlags2::sf_pump_empty, true);
		m_sounds.LoadSound(section, "snd_breechblock_empty", "sndPumpEmpty", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_breechblock_aim_empty"))
	{
		m_eSoundsFlags.set(ESoundsFlags2::sf_pump_aim_empty, true);
		m_sounds.LoadSound(section, "snd_breechblock_aim_empty", "sndPumpAimEmpty", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_chamber_load"))
	{
		m_sounds.LoadSound(section, "snd_chamber_load", "sndChamberLoad", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_chamber_unload"))
	{
		m_sounds.LoadSound(section, "snd_chamber_unload", "sndChamberUnload", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_chamber_check"))
	{
		m_sounds.LoadSound(section, "snd_chamber_check", "sndChamberCheck", true, m_eSoundReload);
	}

	if (SoundExist(section, "snd_chamber_check_empty"))
	{
		m_eSoundsFlags.set(ESoundsFlags2::sf_chamber_check_empty, true);
		m_sounds.LoadSound(section, "snd_chamber_check_empty", "sndChamberCheckEmpty", true, m_eSoundReload);
	}

	//Only for improve misfire external!
	if (SoundExist(section, "snd_jam"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_jam, true);
		m_layered_sounds.LoadSound(section, "snd_jam", "sndJam", true, m_eSoundEmptyClick);
	}

	//Only for improve misfire external!
	if (SoundExist(section, "snd_light_misfire"))
	{
		m_sounds.LoadSound(section, "snd_light_misfire", "sndLightMisfire", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_kick"))
	{
		m_sounds.LoadSound(section, "snd_kick", "sndKick", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_draw_empty"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_empty, true);
		m_sounds.LoadSound(section, "snd_draw_empty", "sndShowEmpty", false, m_eSoundShow);
	}

	if (SoundExist(section, "snd_holster_empty"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_empty, true);
		m_sounds.LoadSound(section, "snd_holster_empty", "sndHideEmpty", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_draw_jammed"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_jam, true);
		m_sounds.LoadSound(section, "snd_draw_jammed", "sndShowMis", false, m_eSoundShow);
	}
	else if (SoundExist(section, "snd_draw_misfire"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_jam, true);
		m_sounds.LoadSound(section, "snd_draw_misfire", "sndShowMis", false, m_eSoundShow);
	}

	if (SoundExist(section, "snd_holster_jammed"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_jam, true);
		m_sounds.LoadSound(section, "snd_holster_jammed", "sndHideMis", false, m_eSoundHide);
	}
	else if (SoundExist(section, "snd_holster_misfire"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_jam, true);
		m_sounds.LoadSound(section, "snd_holster_misfire", "sndHideMis", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_draw_jammed_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_jam_w_gl, true);
		m_sounds.LoadSound(section, "snd_draw_jammed_w_gl", "sndShowMisWGL", false, m_eSoundShow);
	}
	else if (SoundExist(section, "snd_draw_misfire_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_jam_w_gl, true);
		m_sounds.LoadSound(section, "snd_draw_misfire_w_gl", "sndShowMisWGL", false, m_eSoundShow);
	}

	if (SoundExist(section, "snd_holster_jammed_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_jam_w_gl, true);
		m_sounds.LoadSound(section, "snd_holster_jammed_w_gl", "sndHideMisWGL", false, m_eSoundHide);
	}
	else if (SoundExist(section, "snd_holster_misfire_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_jam_w_gl, true);
		m_sounds.LoadSound(section, "snd_holster_misfire_w_gl", "sndHideMisWGL", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_draw_jammed_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_jam_g, true);
		m_sounds.LoadSound(section, "snd_draw_jammed_g", "sndShowMisG", false, m_eSoundShow);
	}
	else if (SoundExist(section, "snd_draw_misfire_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_jam_g, true);
		m_sounds.LoadSound(section, "snd_draw_misfire_g", "sndShowMisG", false, m_eSoundShow);
	}

	if (SoundExist(section, "snd_holster_jammed_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_jam_g, true);
		m_sounds.LoadSound(section, "snd_holster_jammed_g", "sndHideMisG", false, m_eSoundHide);
	}
	else if (SoundExist(section, "snd_holster_misfire_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_jam_g, true);
		m_sounds.LoadSound(section, "snd_holster_misfire_g", "sndHideMisG", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_draw_empty_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_empty_w_gl, true);
		m_sounds.LoadSound(section, "snd_draw_empty_w_gl", "sndShowEmptyWGL", false, m_eSoundShow);
	}

	if (SoundExist(section, "snd_holster_empty_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_empty_w_gl, true);
		m_sounds.LoadSound(section, "snd_holster_empty_w_gl", "sndHideEmptyWGL", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_draw_empty_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_empty_g, true);
		m_sounds.LoadSound(section, "snd_draw_empty_g", "sndShowEmptyG", false, m_eSoundShow);
	}

	if (SoundExist(section, "snd_holster_empty_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_empty_g, true);
		m_sounds.LoadSound(section, "snd_holster_empty_g", "sndHideEmptyG", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_draw_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_w_gl, true);
		m_sounds.LoadSound(section, "snd_draw_w_gl", "sndShowWGL", false, m_eSoundShow);
	}

	if (SoundExist(section, "snd_holster_w_gl"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_w_gl, true);
		m_sounds.LoadSound(section, "snd_holster_w_gl", "sndHideWGL", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_draw_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_draw_g, true);
		m_sounds.LoadSound(section, "snd_draw_g", "sndShowG", false, m_eSoundShow);
	}
	
	if (SoundExist(section, "snd_holster_g"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_holster_g, true);
		m_sounds.LoadSound(section, "snd_holster_g", "sndHideG", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_mag_check"))
	{
		m_sounds.LoadSound(section, "snd_mag_check", "sndMagCheck", false, m_eSoundReload);
	}

	if (SoundExist(section, "snd_mag_check_g"))
	{
		m_sounds.LoadSound(section, "snd_mag_check_g", "sndMagCheckG", false, m_eSoundReload);
	}

	if (SoundExist(section, "snd_firemode_check"))
	{
		m_sounds.LoadSound(section, "snd_firemode_check", "sndFiremodeCheck", false, m_eSoundReload);
	}

	if (SoundExist(section, "snd_mui_on"))
	{
		m_sounds.LoadSound(section, "snd_mui_on", "sndMuiOn", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_mui_off"))
	{
		m_sounds.LoadSound(section, "snd_mui_off", "sndMuiOff", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_mag_shot"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_mag_shot, true);
		m_sounds.LoadSound(section, "snd_mag_shot", "sndMagShot", true, m_eSoundEmptyClick);
	}

	if (SoundExist(section, "snd_bore_empty"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_bore_empty, true);
		m_sounds.LoadSound(section, "snd_bore_empty", "sndBoreEmpty", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_bore_jammed"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_bore_jammed, true);
		m_sounds.LoadSound(section, "snd_bore_jammed", "sndBoreMis", false, m_eSoundHide);
	}
	else if (SoundExist(section, "snd_bore_misfire"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_bore_jammed, true);
		m_sounds.LoadSound(section, "snd_bore_misfire", "sndBoreMis", false, m_eSoundHide);
	}

	if (SoundExist(section, "snd_safemode_in"))
	{
		m_eSoundsFlags2.set(ESoundsFlags2::sf_safemode_in_out, true);
		m_sounds.LoadSound(section, "snd_safemode_in", "sndSafemodeIn", false, m_eSoundHide);
		m_sounds.LoadSound(section, "snd_safemode_out", "sndSafemodeOut", false, m_eSoundShow);
	}
}

void CWeaponMagazined::FireStart()
{
	u8 CurrentState = GetState();
	bool IsActor = ParentIsActor();
	CObject* parent = H_Parent();

	if (!IsMisfire())
	{
		if (m_bIsPumpEnabled && m_bNeedPumpState)
		{
			if (SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfFIRE))
			{
				SwitchState(ePump);
			}
		}
		else if (iAmmoElapsed + iAmmoChamberElapsed > 0)
		{
			if (!IsWorking() || AllowFireWhileWorking())
			{
				if (IsPending())
				{
					return;
				}

				const s32 autoaim_period = GetAutoAimPeriod();
				const bool have_target = IsAutoAimHaveTarget();

				if (autoaim_period != 0)
				{
					if (autoaim_period > 0)
					{
						if (m_bAutoAimShotAfterKeyReleased)
						{
							if (!m_bAutoAimNeedReleaseShot && (!have_target || Device.GetTimeDeltaSafe(GetAutoAimStartTime()) < autoaim_period))
							{
								if (m_bAutoAimAutoShot)
								{
									m_bAutoAimNeedAutoShot = true;
								}
								return;
							}
						}
					}
					else
					{
						if (!have_target)
						{
							if (m_bAutoAimAutoShot)
							{
								m_bAutoAimNeedAutoShot = true;
							}
							return;
						}
					}
				}

				if (m_fRechargeTime > 0.0f && Device.GetTimeDeltaSafe(m_iLastShotTime) < std::floor(m_fLastRechargeTime * 1000.0f))
				{
					return;
				}

				m_bAutoAimNeedReleaseShot = false;
				m_bAutoAimNeedAutoShot = false;
				m_bAutoAimShooted = true;
				SetAutoAimStartTime(0);

				inherited::FireStart();
				//R_ASSERT(parent);
				SwitchState(eFire);
			}
		}
		else if (!IsPending() && GetState() != eFire)
		{
			if (IsActor && m_eAnimationsFlags.test(EAnimationsFlags::af_empty_click))
			{
				SwitchState(eEmptyClick);
			}
			else
			{
				OnEmptyClick();
			}
		}
	}
	else if (!IsPending() && GetState() != eFire)
	{
		if (parent != nullptr)
		{
			if (CGameObject* object = parent->cast_game_object())
			{
				object->callback(GameObject::eOnWeaponJammed)(object->lua_game_object(), this->lua_game_object());
			}

			if (IsActor)
			{
				CurrentGameUI()->AddCustomStatic("gun_jammed", true);
			}
		}

		if (m_eAnimationsFlags.test(EAnimationsFlags::af_empty_click))
		{
			SwitchState(eEmptyClick);
		}
		else
		{
			OnEmptyClick();
		}
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
	inherited::Reload();
	TryReload();
}

bool CWeaponMagazined::TryReload()
{
	if (m_pInventory)
	{
		if (IsGameTypeSingle() && ParentIsActor())
		{
			int	AC = GetSuitableAmmoTotal();
			Actor()->callback(GameObject::eWeaponNoAmmoAvailable)(lua_game_object(), AC);
		}

		PIItem get_any = m_pInventory->GetAny(m_ammoTypes[m_ammoType].c_str());

		m_pCurrentAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;

		if (IsMisfire())
		{
			SetPending(true);
			SwitchState(eReload);
			return true;
		}

		if (m_pCurrentAmmo || unlimited_ammo())
		{
			SetPending(true);
			SwitchState(eReload);
			return true;
		}
		else if (m_set_next_ammoType_on_reload == undefined_ammo_type && iAmmoElapsed + (IsGrenadeMode() ? 0 : iAmmoChamberElapsed) == 0 || m_set_next_ammoType_on_reload != undefined_ammo_type)
		{
			for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
			{
				get_any = m_pInventory->GetAny(m_ammoTypes[i].c_str());
				m_pCurrentAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;
				if (m_pCurrentAmmo)
				{
					m_set_next_ammoType_on_reload = i;
					SetPending(true);
					SwitchState(eReload);
					return true;
				}
			}
		}
		else
		{
			m_set_next_ammoType_on_reload = undefined_ammo_type;
		}

	}

	if (GetState() != eIdle)
	{
		SwitchState(eIdle);
	}

	return false;
}

bool CWeaponMagazined::TryReloadChamber()
{
	if (m_pInventory)
	{
		if (IsGameTypeSingle() && ParentIsActor())
		{
			int	AC = GetSuitableAmmoTotal();
			Actor()->callback(GameObject::eWeaponNoAmmoAvailable)(lua_game_object(), AC);
		}

		PIItem get_any = m_pInventory->GetAny(m_ammoTypes[m_ChamberAmmoType].c_str());

		m_pCurrentAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;

		if (m_pCurrentAmmo != nullptr || unlimited_ammo())
		{
			SetPending(true);
			SwitchState(eLoadChamber);
			return true;
		}
		else if (m_set_next_ammoType_on_reload == undefined_ammo_type && iAmmoChamberElapsed == 0 || m_set_next_ammoType_on_reload != undefined_ammo_type)
		{
			for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
			{
				get_any = m_pInventory->GetAny(m_ammoTypes[i].c_str());
				m_pCurrentAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;
				if (m_pCurrentAmmo != nullptr)
				{
					m_set_next_ammoType_on_reload = i;
					SetPending(true);
					SwitchState(eLoadChamber);
					return true;
				}
			}
		}
		else
		{
			m_set_next_ammoType_on_reload = undefined_ammo_type;
		}

	}

	return false;
}

bool CWeaponMagazined::IsAmmoAvailable()
{
	PIItem get_any = m_pInventory->GetAny(m_ammoTypes[m_ammoType].c_str());

	if (get_any != nullptr && get_any->cast_weapon_ammo())
	{
		return true;
	}
	else for (const shared_str& ammotype : m_ammoTypes)
	{
		get_any = m_pInventory->GetAny(ammotype.c_str());
		if (get_any != nullptr && get_any->cast_weapon_ammo())
		{
			return true;
		}
	}

	return false;
}

void CWeaponMagazined::UnloadMagazine(bool spawn_ammo)
{
	if (!IsGrenadeMode())
	{
		SetMisfireStatus(false);
	}

	xr_map<shared_str, u16> l_ammo;
	xr_map< u16, u16> ammos_to_sync;
	while(!m_magazine.empty()) 
	{
		CCartridge &l_cartridge = m_magazine.back();
		xr_map<shared_str, u16>::iterator l_it;
		for(l_it = l_ammo.begin(); l_ammo.end() != l_it; ++l_it) 
		{
            if(l_cartridge.m_ammoSect == l_it->first) 
            { 
				 ++(l_it->second); 
				 break; 
			}
		}

		if(l_it == l_ammo.end()) l_ammo[l_cartridge.m_ammoSect] = 1;
		m_magazine.pop_back(); 
		--iAmmoElapsed;
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	if (spawn_ammo)
	{
		ReturnAmmoToInventory(l_ammo, &ammos_to_sync);
	}

	if (ParentIsActor())
	{
		int	AC = GetSuitableAmmoTotal();
		Actor()->callback(GameObject::eOnWeaponMagazineEmpty)(lua_game_object(), AC);
	}

	if (!spawn_ammo)
		return;

	if (!IsGrenadeMode())
	{
		m_bJustAfterReload = false;
	}

	if (!IsGameTypeSingle())
	{
		NET_Packet	P;
		CGameObject::u_EventGen(P, GE_WPN_UPDATE_AMMO, ID());
		xr_map<u16, u16>::iterator _it;
		P.w_u32(ammos_to_sync.size());
		for (_it = ammos_to_sync.begin(); ammos_to_sync.end() != _it; ++_it)
		{
			P.w_u16(_it->first);
			P.w_u16(_it->second);
		}
		CGameObject::u_EventSend(P);
	}

	if (GetState() == eIdle)
		SwitchState(eIdle);

	if (!IsGrenadeMode())
	{
		if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
		{
			AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, m_ammoType);
		}

		if (TLiteAmmoBones* LiteAmmoBones = GetComponent<TLiteAmmoBones>())
		{
			LiteAmmoBones->UpdateLiteAmmoBones(this, GetCurrentElapsed() + iAmmoChamberElapsed);
		}
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
	
	if (!unlimited_ammo())
	{
		if (m_ammoTypes.size() <= m_ammoType)
		{
			return;
		}

		const char* tmp_sect_name = m_ammoTypes[m_ammoType].c_str();

		if (!tmp_sect_name)
		{
			return;
		}

		//попытаться найти в инвентаре патроны текущего типа
		PIItem get_any = m_pInventory->GetAny(tmp_sect_name);
		m_pCurrentAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;

		if (!m_pCurrentAmmo && !m_bLockType)
		{
			for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
			{
				//проверить патроны всех подходящих типов
				get_any = m_pInventory->GetAny(m_ammoTypes[i].c_str());
				m_pCurrentAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;
				if (m_pCurrentAmmo)
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
		m_pCurrentAmmo->SetDropManual(true);

	if(iMagazineSize > iAmmoElapsed) 
	{ 
		m_bLockType = true; 
		ReloadMagazine(); 
		m_bLockType = false; 
	}

	if (!IsGrenadeMode())
	{
		if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
		{
			AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, m_ammoType);
		}

		if (TLiteAmmoBones* LiteAmmoBones = GetComponent<TLiteAmmoBones>())
		{
			LiteAmmoBones->UpdateLiteAmmoBones(this, GetCurrentElapsed() + iAmmoChamberElapsed);
		}
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());
}

bool CWeaponMagazined::HaveCartridgeInInventory(u8 cnt)
{
	if (unlimited_ammo())
	{
		return true;
	}

	if (!m_pInventory)
	{
		return false;
	}

	u32 ac = GetAmmoCount(GetTargetAmmoType());

	if (m_set_next_ammoType_on_reload == undefined_ammo_type && ac < cnt && iAmmoElapsed + iAmmoChamberElapsed == 0)
	{
		for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
		{
			if (m_ammoType == i)
			{
				continue;
			}

			ac = GetAmmoCount(i);

			if (ac >= cnt)
			{
				m_set_next_ammoType_on_reload = i;
				break;
			}
		}
	}

	return ac >= cnt;
}

u8 CWeaponMagazined::AddCartridge(u8 cnt)
{
	if (IsMisfire())
	{
		bMisfire = false;
	}

	if (m_set_next_ammoType_on_reload != undefined_ammo_type)
	{
		m_ammoType = m_set_next_ammoType_on_reload;
		m_set_next_ammoType_on_reload = undefined_ammo_type;
	}

	if (!HaveCartridgeInInventory(1))
	{
		return 0;
	}

	PIItem get_any = m_pInventory->GetAny(m_ammoTypes[m_ammoType].c_str());
	m_pCurrentAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;

	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	if (m_DefaultCartridge.m_LocalAmmoType != m_ammoType)
	{
		m_DefaultCartridge.Load(m_ammoTypes[m_ammoType].c_str(), m_ammoType);
	}

	CCartridge l_cartridge = m_DefaultCartridge;
	while (cnt)
	{
		if (!unlimited_ammo())
		{
			if (m_pCurrentAmmo != nullptr && !m_pCurrentAmmo->Get(l_cartridge))
			{
				break;
			}
		}

		--cnt;
		++iAmmoElapsed;
		l_cartridge.m_LocalAmmoType = m_ammoType;
		m_magazine.push_back(l_cartridge);
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	if (m_pCurrentAmmo != nullptr && !m_pCurrentAmmo->m_boxCurr && OnServer())
	{
		m_pCurrentAmmo->SetDropManual(true);
	}

	if (!m_bIsPumpEnabled)
	{
		GiveAmmoFromMagToChamber();
	}

	return cnt;
}

void CWeaponMagazined::OnStateSwitch	(u8 S)
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
	{
		if (H_Parent() && H_Parent()->cast_actor() && (Level().CurrentViewEntity() == H_Parent()))
		{
			CurrentGameUI()->AddCustomStatic("gun_jammed", true);
		}

		SetMisfireStatus(true);

		const static bool isImproveMis = EngineExternal()[EEngineExternalGame::EnableImproveWeaponMisfire];

		if (isImproveMis)
		{
			OnShotJammed();
		}
		else
		{
			OnEmptyClick();
			SwitchState(eIdle);
		}

		break;
	}
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
	{
		if (ParentIsActor() && GetCurrentFireMode() != GetQueueSize())
		{
			SetQueueSize(GetCurrentFireMode());
		}
		if (H_Parent() && H_Parent()->cast_inventory_owner())
			m_sounds_enabled = H_Parent()->cast_inventory_owner()->CanPlayShHdRldSounds();
		switch2_Hiding();
		break;
	}
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
	case eDevice:
	{
		switch2_Device();
		break;
	}
	case eLightMis:
	{
		switch2_LightMis();
		break;
	}
	case eKick:
	{
		switch2_Kick();
		break;
	}
	case eMagCheck:
	{
		switch2_MagCheck();
		break;
	}
	case eFiremodeCheck:
	{
		switch2_FiremodeCheck();
		break;
	}
	case eLoadChamber:
	{
		switch2_ChamberLoad();
		break;
	}
	case eUnloadChamber:
	{
		switch2_ChamberUnload();
		break;
	}
	case eChamberCheck:
	{
		switch2_ChamberCheck();
		break;
	}
	case ePump:
	{
		switch2_Pump();
		break;
	}
	case eSafemodeSwitch:
	{
		switch2_Safemode();
		break;
	}
	}

	if (S == eIdle)
	{
		UpdateIdleAnimations();
	}
}

static bool is_shooting_end_callback = false;

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
		case eLightMis:
		case eKick:
		case eMagCheck:
		case eFiremodeCheck:
		case eLoadChamber:
		case eUnloadChamber:
		case eChamberCheck:
		case ePump:
		case eSafemodeSwitch:
			{
				fShotTimeCounter	-=	dt;
				clamp				(fShotTimeCounter, 0.0f, flt_max);
			}break;
		case eFire:			
			{
				if (m_bAmmoInChamber && !IsGrenadeMode())
				{
					state_FireChamber(dt);
				}
				else
				{
					state_Fire(dt);
				}
			}break;
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
		
		if(!IsGameTypeSingle())
		{
			if (smart_cast<CMPPlayersBag*>(H_Parent()) != nullptr)
			{
				Msg("! WARNING: state_Fire of object [%d][%s] while parent is CMPPlayerBag...", ID(), cNameSect().c_str());
				{
					StopShooting();
					return;
				}
			}
		}

		if (CEntity* entity = H_Parent() ? H_Parent()->cast_entity() : nullptr)
		{
			entity->g_fireParams(this, p1,d);
			
			if (!entity->g_stateFire())
				StopShooting();
		}
		
		if (m_iShotNum == 0)
		{
			m_vStartPos = p1;
			m_vStartDir = d;
		}
		
		VERIFY(!m_magazine.empty());

		while (!m_magazine.empty() && fShotTimeCounter < 0 && (IsWorking() || m_bFireSingleShot) && (m_iQueueSize < 0 || m_iShotNum < m_iQueueSize) && !m_bNeedPumpState)
		{
			if( CheckForMisfire() )
			{
				StopShooting();
				return;
			}

			m_bFireSingleShot		= false;

			static const bool EnableAlternateRPM = EngineExternal()[EEngineExternalGame::EnableWeaponAlternateRPMSystem];

			if (EnableAlternateRPM)
			{
				if (m_iQueueSize == 1 && m_fSingleShootsTimeDelta > 0.0f)
				{
					fShotTimeCounter = m_fSingleShootsTimeDelta;
				}
				else if (m_fBaseDispersionedBulletsTimeDelta > 0.0f && m_iShotNum < m_iBaseDispersionedBulletsCount)
				{
					fShotTimeCounter = m_fBaseDispersionedBulletsTimeDelta;
				}
				else
				{
					fShotTimeCounter = fOneShotTime;
				}
			}
			else
			{
				if (m_iQueueSize == 1 && m_fSingleShootsTimeDelta > 0.0f)
				{
					fShotTimeCounter += m_fSingleShootsTimeDelta;
				}
				else if (m_fBaseDispersionedBulletsTimeDelta > 0.0f && m_iShotNum < m_iBaseDispersionedBulletsCount)
				{
					fShotTimeCounter += m_fBaseDispersionedBulletsTimeDelta;
				}
				else
				{
					fShotTimeCounter += fOneShotTime;
				}
			}
			
			if (!infinite_fire() || m_bIAmWeaponRPG7)
				++m_iShotNum;
			
			if (m_bUseLastAmmoType)
			{
				u8 type_to_update = m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();

				if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
				{
					AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, type_to_update);
				}
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

	if (iAmmoElapsed == 0 ||
		(m_iQueueSize > 0 && m_iShotNum >= m_iQueueSize) ||
		!IsWorking() && H_Parent())
	{
		StopShotEffector(); 
	}

	if(fShotTimeCounter<0)
	{
		if(iAmmoElapsed == 0)
			OnMagazineEmpty();

		StopShooting();

		if (ParentIsActor() && is_shooting_end_callback)
		{
			is_shooting_end_callback = false;
			bWorking = false;
			SwitchState(eIdle);
		}
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

		Fvector p1, d;
		p1.set(get_LastFP());
		d.set(get_LastFD());

		if (!IsGameTypeSingle())
		{
			if (smart_cast<CMPPlayersBag*>(H_Parent()) != nullptr)
			{
				Msg("! WARNING: state_FireChamber of object [%d][%s] while parent is CMPPlayerBag...", ID(), cNameSect().c_str());
				{
					StopShooting();
					return;
				}
			}
		}

		if (CEntity* entity = H_Parent() ? H_Parent()->cast_entity() : nullptr)
		{
			entity->g_fireParams(this, p1, d);

			if (!entity->g_stateFire())
			{
				StopShooting();
			}
		}

		if (m_iShotNum == 0)
		{
			m_vStartPos = p1;
			m_vStartDir = d;
		}

		VERIFY(!m_chamber.empty());

		while (!m_chamber.empty() && fShotTimeCounter < 0 && (IsWorking() || m_bFireSingleShot) && (m_iQueueSize < 0 || m_iShotNum < m_iQueueSize) && !m_bNeedPumpState)
		{
			if (CheckForMisfire())
			{
				StopShooting();
				return;
			}

			m_bFireSingleShot = false;

			static const bool EnableAlternateRPM = EngineExternal()[EEngineExternalGame::EnableWeaponAlternateRPMSystem];

			if (EnableAlternateRPM)
			{
				if (m_iQueueSize == 1 && m_fSingleShootsTimeDelta > 0.0f)
				{
					fShotTimeCounter = m_fSingleShootsTimeDelta;
				}
				else if (m_fBaseDispersionedBulletsTimeDelta > 0.0f && m_iShotNum < m_iBaseDispersionedBulletsCount)
				{
					fShotTimeCounter = m_fBaseDispersionedBulletsTimeDelta;
				}
				else
				{
					fShotTimeCounter = fOneShotTime;
				}
			}
			else
			{
				if (m_iQueueSize == 1 && m_fSingleShootsTimeDelta > 0.0f)
				{
					fShotTimeCounter += m_fSingleShootsTimeDelta;
				}
				else if (m_fBaseDispersionedBulletsTimeDelta > 0.0f && m_iShotNum < m_iBaseDispersionedBulletsCount)
				{
					fShotTimeCounter += m_fBaseDispersionedBulletsTimeDelta;
				}
				else
				{
					fShotTimeCounter += fOneShotTime;
				}
			}

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

	if (iAmmoElapsed == 0 ||
		(m_iQueueSize > 0 && m_iShotNum >= m_iQueueSize) ||
		!IsWorking() && H_Parent())
	{
		StopShotEffector(); 
	}

	if (fShotTimeCounter < 0)
	{
		if (iAmmoChamberElapsed == 0)
			OnMagazineEmpty();

		StopShooting();

		if (ParentIsActor() && is_shooting_end_callback)
		{
			is_shooting_end_callback = false;
			bWorking = false;
			SwitchState(eIdle);
		}
	}
	else
	{
		fShotTimeCounter -= dt;
	}
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
				m_sSndShotCurrent = m_layered_sounds.FindSoundItem("sndSilencerShot", false) ? "sndSilencerShot" : "sndShot";
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

	m_layered_sounds.PlaySound(m_sSndShotCurrent.c_str(), get_LastFP(), H_Parent(), !!GetHUDmode(), false, true);

	if (m_eSoundsFlags2.test(ESoundsFlags2::sf_mag_shot))
	{
		float fAmmoElapsed = (float)get_elapsed;
		float fmaxMagazineSize_ = GetMagCapacity() + iChamberSize;
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

	if (!m_bIsPumpEnabled && m_eSoundsFlags.test(ESoundsFlags::sf_breechblock))
	{
		if (m_eSoundsFlags.test(ESoundsFlags::sf_jam) && IsMisfire())
		{
			m_layered_sounds.PlaySound("sndJam", get_LastFP(), H_Parent(), !!GetHUDmode(), false);
		}
		else
		{
			PlaySound("sndPump", get_LastFP());
		}
	}
}

void CWeaponMagazined::OnShot()
{
	SelectShotSound();

	ApplyPattern();
	// Camera	
	if (H_Parent()) 
		AddShotEffector();

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		pInput->feedback(65535 * m_vibration_factor_left, 65535 * m_vibration_factor_right, m_vibration_time);
		if (GetCurrentFireMode() != 1)
		{
			u8 gamepadRpm = fOneShotTimeSaved / 60;
			GGamepadService->MachineTriggerEffect(true, 127, true, 255, 255, gamepadRpm, 255, (float)gamepadRpm/100);
		}
		else
		{
			GGamepadService->ShotTriggerEffect(true, m_trigger_effect_time);
		}
	}

	// Animation
	PlayAnimShoot();

	CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr;

	if (pActor != nullptr && m_shot_cams[0].size() > 0)
	{
		CAnimatorCamEffector* e = new CAnimatorCamEffector();
		e->SetType(ECamEffectorType(Random.randI(32000, 32999)));
		e->SetCyclic(false);
		e->SetHudAffect(false);

		bool aim = IsZoomed() && m_shot_cams[1].size() > 0;

		e->Start(*m_shot_cams[aim ? 1 : 0][Random.randI(m_shot_cams[aim ? 1 : 0].size())]);
		pActor->Cameras().AddCamEffector(e);
	}

	StartFlameParticle();

	// Shell Drop
	Fvector vel;
	PHGetLinearVell(vel);

	StartShellParticle(vel);
	StartSmokeParticle(vel);

	if (H_Parent())
	{
		if (CGameObject* object = H_Parent()->cast_game_object())
		{
			object->callback(GameObject::eOnWeaponFired)(object->lua_game_object(), this->lua_game_object(), iAmmoElapsed, m_ammoType);
		}
	}
}

void CWeaponMagazined::OnShotJammed()
{
	if (m_eSoundsFlags.test(ESoundsFlags::sf_jam))
	{
		m_layered_sounds.PlaySound("sndJam", get_LastFP(), H_Parent(), !!GetHUDmode(), false);
	}

	PlayAnimShoot();
}

void CWeaponMagazined::OnEmptyClick()
{
	PlaySound(ParentIsActor() && IsMisfire() && m_sounds.FindSoundItem("sndJammedClick", false) ? "sndJammedClick" : "sndEmptyClick", get_LastFP());

	if (HudItemData() != nullptr && m_sFakeShootBlendParams.has_motion)
	{
		PlayBlendAnm(m_sFakeShootBlendParams.camera_name,
			m_sFakeShootBlendParams.speed_power.x, m_sFakeShootBlendParams.speed_power.y,
			m_sFakeShootBlendParams.blend_params, false, false, true,
			g_player_hud->attached_item(1) ? 0 : 2, 0, script_layer::EBlendLayers::eNone);
	}
}

void CWeaponMagazined::OnAnimationEnd(u8 state) 
{
	switch(state) 
	{
		case eReload:
		{
			if (bMisfireReload)
			{
				bMisfire = false;
				bMisfireReload = false;
				m_bJustAfterReload = true;

				if (NeedMisfireAmmo && iAmmoElapsed + iAmmoChamberElapsed > 0)
				{
					if (m_bAmmoInChamber)
					{
						DeleteAmmoInChamber();
						GiveAmmoFromMagToChamber();
					}
					else
					{
						SetAmmoElapsed(iAmmoElapsed - 1);
					}
				}
			}
			else
			{
				if (!m_bIsReloaded)
				{
					m_bIsReloaded = true;
					int base_mag_size = iMagazineSize;
					int new_mag_size = GetMagCapacity();
					iMagazineSize = new_mag_size;
					ReloadMagazine();
					iMagazineSize = base_mag_size;

					if (!IsGrenadeMode())
					{
						m_bJustAfterReload = true;
					}

					if (g_pGamePersistent->GameType() == eGameIDFreeMP && m_pCurrentAmmo != nullptr)
					{
						xr_map<u16, u16> ammos_to_sync;
						ammos_to_sync[m_pCurrentAmmo->ID()] = m_pCurrentAmmo->m_boxCurr;
						NET_Packet	P;
						CGameObject::u_EventGen(P, GE_WPN_UPDATE_AMMO, ID());
						
						P.w_u32(ammos_to_sync.size());
						for (xr_map<u16, u16>::iterator _it = ammos_to_sync.begin(); ammos_to_sync.end() != _it; ++_it)
						{
							P.w_u16(_it->first);
							P.w_u16(_it->second);
						}
						CGameObject::u_EventSend(P);
					}
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
		case eFire:
		{
			if (ParentIsActor())
			{
				if (IsGrenadeMode())
				{
					bWorking = false;
					SwitchState(eIdle);
				}
				else
				{
					is_shooting_end_callback = true;
				}
			}
			break;
		}
		case eLoadChamber:
		{
			LoadChamber();
			m_bNeedPumpState = false;
			m_bHaveShell = false;
			SwitchState(eIdle);
			break;
		}
		case eUnloadChamber:
		{
			UnloadChamber();
			GiveAmmoFromMagToChamber();
			SwitchState(eIdle);
			break;
		}
		case ePump:
		{
			m_bNeedPumpState = false;
			m_bHaveShell = false;
			GiveAmmoFromMagToChamber();
			SwitchState(eIdle);
			break;
		}
		case eChamberCheck:
		case eShowing:
		case eSwitchMode:
		case eDevice:
		case eLightMis:
		case eMisfire:
		case eKick:
		case eMagCheck:
		case eFiremodeCheck:
		case eSafemodeSwitch:
		case eEmptyClick:
		{
			if (state == eSwitchMode)
			{
				SetQueueSize(GetCurrentFireMode());
				UpdateFiremodeAnimations();
			}
			else if (state == eFire || state == eReload || state == eMisfire)
			{
				UpdateIdleAnimations();
			}

			SwitchState(eIdle);
			break;
		}
	}
	inherited::OnAnimationEnd(state);
}

void CWeaponMagazined::switch2_Idle	()
{
	m_iShotNum = 0;
	if(m_fOldBulletSpeed != 0.f)
		SetBulletSpeed(m_fOldBulletSpeed);

	SetPending			(false);
	PlayAnimIdle		();
}

#ifdef DEBUG
#include "ai/stalker/ai_stalker.h"
#endif

void CWeaponMagazined::switch2_Fire	()
{
#if USE_OLD_OBJECT_PLANNER
	if ( !(io && (ii == io->inventory().ActiveItem())) ) 
	{
		CAI_Stalker* stalker = H_Parent() != nullptr ? H_Parent()->cast_stalker() : nullptr;
		if (stalker)
		{
			stalker->planner().show();
			stalker->planner().show_current_world_state();
			stalker->planner().show_target_world_state();
		}
	}
#endif
	
	m_bStopedAfterQueueFired = false;
	m_bFireSingleShot = true;
	m_iShotNum = 0;

    if ((OnClient() || Level().IsDemoPlay())&& !IsWorking())
		FireStart();

}

void CWeaponMagazined::switch2_Empty()
{
	auto play_motion_if_exists = [&](const shared_str& motion_name)
	{
		SetPending(true);
		PlayHUDMotion(SetCurrentStateAnimation(motion_name), EHudMixType::eMixAll, eEmptyClick);
		if (CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
		{
			if (CCustomDevice* pDevice = pActor->GetDevice())
			{
				if (IsMisfire() && pDevice->CanJammed() || pDevice->CanShooting(true))
				{
					pDevice->SwitchState(IsMisfire() ? CCustomDevice::EDeviceStates::eHandJammed : CCustomDevice::EDeviceStates::eHandDry);
				}
			}
		}
		OnEmptyClick();
	};

	shared_str name = "anm_empty_click";

	const static bool isAutoreload = EngineExternal()[EEngineExternalGame::EnableAutoreload];

	if (!isAutoreload)
	{
		play_motion_if_exists(name);
	}
	else
	{
		if (!IsTriStateReload())
		{
			if (!TryReload())
			{
				play_motion_if_exists("anm_empty_click");
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
				play_motion_if_exists("anm_empty_click");
			}
			else
			{
				inherited::FireEnd();
				Reload();
			}
		}
	}
}

void CWeaponMagazined::switch2_Device()
{
	SetPending(true);

	if (m_eDevicesFlags.test(EDevicesFlags::df_tacticaltorch))
	{
		PlaySound(m_bTacticalTorchStatus ? "sndTorchOff" : "sndTorchOn", get_LastFP());
		PlayHUDMotion(SetCurrentStateAnimation("anm_torch_on"), EHudMixType::eMixAll, eDevice);
	}
	else if (m_eDevicesFlags.test(EDevicesFlags::df_laser))
	{
		PlaySound(m_bTacticalTorchStatus ? "sndLaserOff" : "sndLaserOn", get_LastFP());
		PlayHUDMotion(SetCurrentStateAnimation("anm_laser_on"), EHudMixType::eMixAll, eDevice);
	}

	CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr;
	R_ASSERT(pActor);

	if (CCustomDevice* dev = pActor->GetDevice())
	{
		if (dev->CanLam())
		{
			dev->SwitchState(CCustomDevice::EDeviceStates::eHandLam);
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

	s32 elapsed = iAmmoElapsed + iAmmoChamberElapsed;
	const shared_str name = shared_str().printf("sndReloadR%d", elapsed);

	if (m_bUseRevolverScheme && !IsGrenadeMode() && !IsMisfire() && elapsed > 0 && m_sounds.FindSoundItem(*name, false))
	{
		PlaySound(*name, get_LastFP());
		return;
	}

	bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;
	CActor* actor = Level().CurrentControlEntity() != nullptr ? Level().CurrentControlEntity()->cast_actor() : nullptr;
	bool detector = actor != nullptr && actor->GetDevice() != nullptr;

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
	SetPending			(true);
}

void CWeaponMagazined::switch2_Hiding()
{
	OnZoomOut();
	CWeapon::FireEnd();

	if (m_sounds_enabled)
	{
		if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_jam) && IsMisfire())
		{
			PlaySound("sndHideMis", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_holster_empty) && GetAmmoChamberElapsed() + GetAmmoElapsed() == 0)
		{
			PlaySound("sndHideEmpty", get_LastFP());
		}
		else
		{
			PlaySound("sndHide", get_LastFP());
		}
	}

	PlayAnimHide();
	SetPending(true);
}

void CWeaponMagazined::switch2_Bore()
{
	SetPending(false);
	PlayAnimBore();

	const CObject* root = H_Root();
	if (IsMisfire() && m_eSoundsFlags2.test(ESoundsFlags2::sf_bore_jammed))
	{
		PlaySound("sndBoreMis", root->Position());
	}
	else if (iAmmoChamberElapsed + iAmmoElapsed == 0 && m_eSoundsFlags2.test(ESoundsFlags2::sf_bore_empty))
	{
		PlaySound("sndBoreEmpty", root->Position());
	}
	else
	{
		PlaySound("sndBore", root->Position());
	}
}

void CWeaponMagazined::switch2_Safemode()
{
	SetPending(true);

	CObject* parent = H_Parent();

	if (CActor* pActor = parent != nullptr ? parent->cast_actor() : nullptr)
	{
		const bool status = pActor->IsSafemode();
		pActor->SetSafemodeStatus(!status);

		PlayHUDMotion(SetCurrentStateAnimation(status ? "anm_safemode_out" : "anm_safemode_in"), EHudMixType::eMixAll, eSafemodeSwitch);

		if (m_eSoundsFlags2.test(ESoundsFlags2::sf_safemode_in_out))
		{
			PlaySound(status ? "sndSafemodeOut" : "sndSafemodeIn", get_LastFP());
		}
	}
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
	if (m_sounds_enabled)
	{
		if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_jam) && IsMisfire())
		{
			PlaySound("sndShowMis", get_LastFP());
		}
		else if (m_eSoundsFlags2.test(ESoundsFlags2::sf_draw_empty) && GetAmmoChamberElapsed() + GetAmmoElapsed() == 0)
		{
			PlaySound("sndShowEmpty", get_LastFP());
		}
		else
		{
			PlaySound("sndShow", get_LastFP());
		}
	}

	SetPending(true);
	PlayAnimShow();
}

void CWeaponMagazined::switch2_FireMode()
{
	SetPending(true);

	if (m_bGaussScheme)
	{
		if (!m_bGaussScreen)
		{
			PlaySound("sndMuiOn", get_LastFP());
			PlayHUDMotion(SetCurrentStateAnimation("anm_mui_on"), EHudMixType::eMixAll, eSwitchMode);
		}
		else
		{
			PlaySound("sndMuiOff", get_LastFP());
			PlayHUDMotion(SetCurrentStateAnimation("anm_mui_off"), EHudMixType::eMixAll, eSwitchMode);
		}
		return;
	}

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

	if (GetCurrentFireMode() == -1)
	{
		anim_name.printf("%s%s", *anim_name, "a");
	}
	else
	{
		anim_name.printf("%s%d", *anim_name, GetCurrentFireMode());
	}

	if (HudAnimationExist(anim_name))
	{
		PlayHUDMotion(SetCurrentStateAnimation(anim_name), EHudMixType::eMixAll, eSwitchMode);
	}
	else
	{
		PlayHUDMotion(SetCurrentStateAnimation("anm_firemode"), EHudMixType::eMixAll, eSwitchMode);
	}

	if (CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
	{
		if (CCustomDevice* Dev = pActor->GetDevice(true); Dev && (!Dev->IsHidden() || Dev->NeedActivation()))
		{
			bDisablePrepareAnimation = iAmmoElapsed + iAmmoChamberElapsed == 0;
			if (Dev->CanFiremode())
			{
				Dev->SwitchState(CCustomDevice::EDeviceStates::eHandFiremode);
			}
		}
		else
		{
			bDisablePrepareAnimation = false;
		}
	}
}

void CWeaponMagazined::switch2_LightMis()
{
	//SendMessage("gunsl_light_misfire", gd_novice);
	SetPending(true);
	PlaySound("sndLightMisfire", get_LastFP());
	PlayHUDMotion(SetCurrentStateAnimation("anm_shoot_lightmisfire"), EHudMixType::eMixAll, GetState());

	if (CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
	{
		if (CCustomDevice* pDevice = pActor->GetDevice())
		{
			if (pDevice->CanLightMisfire())
			{
				pDevice->SwitchState(CCustomDevice::EDeviceStates::eHandLightMisfire);
			}
		}
	}
}

void CWeaponMagazined::switch2_Kick()
{
	SetPending(true);
	PlaySound("sndKick", get_LastFP());
	PlayHUDMotion(SetCurrentStateAnimation("anm_kick"), EHudMixType::eMixAll, eKick);
}

void CWeaponMagazined::switch2_MagCheck()
{
	SetPending(true);
	PlaySound("sndMagCheck", get_LastFP());
	const shared_str anim = IsGrenadeMode() ? (iAmmoElapsed == 0 ? "anm_grenade_empty_inspect" : "anm_grenade_inspect") : "anm_magazine_inspect";
	PlayHUDMotion(SetCurrentStateAnimation(anim), EHudMixType::eMixAll, eMagCheck);
}

void CWeaponMagazined::switch2_FiremodeCheck()
{
	SetPending(true);
	PlaySound("sndFiremodeCheck", get_LastFP());
	PlayHUDMotion(SetCurrentStateAnimation("anm_firemode_inspect"), EHudMixType::eMixAll, eFiremodeCheck);
}

void CWeaponMagazined::switch2_ChamberLoad()
{
	SetPending(true);
	PlaySound("sndChamberLoad", get_LastFP());

	if (IsGrenadeLauncherAttached() && HudAnimationExist("anm_chamber_load_w_gl"))
	{
		PlayHUDMotion("anm_chamber_load_w_gl", EHudMixType::eMixAll, eLoadChamber);
	}
	else
	{
		PlayHUDMotion("anm_chamber_load", EHudMixType::eMixAll, eLoadChamber);
	}

	if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
	{
		AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, GetTargetAmmoType());
	}
}

void CWeaponMagazined::switch2_ChamberUnload()
{
	SetPending(true);

	PlaySound("sndChamberUnload", get_LastFP());

	if (IsGrenadeLauncherAttached() && HudAnimationExist("anm_chamber_unload_w_gl"))
	{
		PlayHUDMotion("anm_chamber_unload_w_gl", EHudMixType::eMixAll, eUnloadChamber);
	}
	else
	{
		PlayHUDMotion("anm_chamber_unload", EHudMixType::eMixAll, eUnloadChamber);
	}

	if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
	{
		AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, m_chamber.back().m_LocalAmmoType);
	}
}

void CWeaponMagazined::switch2_ChamberCheck()
{
	SetPending(true);

	bool is_empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;

	if (m_eSoundsFlags2.test(ESoundsFlags2::sf_chamber_check_empty) && is_empty)
	{
		PlaySound("sndChamberCheckEmpty", get_LastFP());
	}
	else
	{
		PlaySound("sndChamberCheck", get_LastFP());
	}

	if (is_empty && HudAnimationExist("anm_chamber_check_empty"))
	{
		if (IsGrenadeLauncherAttached() && HudAnimationExist("anm_chamber_check_empty_w_gl"))
		{
			PlayHUDMotion("anm_chamber_check_empty_w_gl", EHudMixType::eMixAll, eChamberCheck);
		}
		else
		{
			PlayHUDMotion("anm_chamber_check_empty", EHudMixType::eMixAll, eChamberCheck);
		}
	}
	else
	{
		if (IsGrenadeLauncherAttached() && HudAnimationExist("anm_chamber_check_empty_w_gl"))
		{
			PlayHUDMotion("anm_chamber_check_w_gl", EHudMixType::eMixAll, eChamberCheck);
		}
		else
		{
			PlayHUDMotion("anm_chamber_check", EHudMixType::eMixAll, eChamberCheck);
		}
	}

	if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
	{
		AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, is_empty ? undefined_ammo_type : m_bAmmoInChamber ? m_chamber.back().m_LocalAmmoType : m_magazine.back().m_LocalAmmoType);
	}

	if (TShellBones* ShellBones = GetComponent<TShellBones>())
	{
		ShellBones->UpdateShellBones(this, m_bHaveShell ? m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType() : undefined_ammo_type);
	}
}

shared_str CWeaponMagazined::SetCurrentPumpAnimation()
{
	shared_str anm = "anm_pump";

	if (m_bHaveShell && iAmmoChamberElapsed + iAmmoElapsed == 0)
	{
		AddSuffixName(anm, "_last");
	}
	else if (!m_bHaveShell && (m_bAmmoInChamber && iAmmoChamberElapsed == 0 && iAmmoElapsed != 0 || iAmmoElapsed == 0))
	{
		AddSuffixName(anm, "_empty");
	}

	if (IsZoomed())
	{
		AddSuffixName(anm, "_aim");
	}

	return anm;
}

void CWeaponMagazined::switch2_Pump()
{
	SetPending(true);

	bool is_shell = m_bHaveShell && iAmmoChamberElapsed + iAmmoElapsed == 0;
	bool is_chamber_empty = !m_bHaveShell && (m_bAmmoInChamber && iAmmoChamberElapsed == 0 && iAmmoElapsed != 0 || iAmmoElapsed == 0);

	if (IsZoomed() && m_eSoundsFlags2.test(ESoundsFlags2::sf_pump_aim))
	{
		if (is_shell && m_eSoundsFlags2.test(ESoundsFlags2::sf_pump_aim_last))
		{
			PlaySound("sndPumpAimLast", get_LastFP());
		}
		else if (is_chamber_empty && m_eSoundsFlags2.test(ESoundsFlags2::sf_pump_aim_empty))
		{
			PlaySound("sndPumpAimEmpty", get_LastFP());
		}
		else
		{
			PlaySound("sndPumpAim", get_LastFP());
		}
	}
	else
	{
		if (is_shell && m_eSoundsFlags2.test(ESoundsFlags2::sf_pump_last))
		{
			PlaySound("sndPumpLast", get_LastFP());
		}
		else if (is_chamber_empty && m_eSoundsFlags2.test(ESoundsFlags2::sf_pump_empty))
		{
			PlaySound("sndPumpEmpty", get_LastFP());
		}
		else
		{
			PlaySound("sndPump", get_LastFP());
		}
	}

	PlayHUDMotion(SetCurrentPumpAnimation(), EHudMixType::eMixAll, ePump);
}

bool CWeaponMagazined::Action(u16 cmd, u32 flags) 
{
	if (inherited::Action(cmd, flags))
	{
		return true;
	}

	switch(cmd) 
	{
	case kWPN_RELOAD:
		{
			if (flags & CMD_START)
			{
				if (!ParentIsActor() && GetNextState() != eIdle)
				{
					return false;
				}

				if (m_bBlockReload && GetNextState() == eFire)
				{
					return false;
				}

				if (IsPending())
				{
					return false;
				}

				if ((iAmmoElapsed < GetMagCapacity() || IsMisfire()))
				{
					if (!unlimited_ammo() && !IsMisfire())
					{
						PIItem get_any = m_pInventory->GetAny(m_ammoTypes[m_ammoType].c_str());

						if (get_any == nullptr)
						{
							if (iAmmoElapsed + iAmmoChamberElapsed > 0)
							{
								return false;
							}

							for (u8 i = 0; i < u8(m_ammoTypes.size()); ++i)
							{
								get_any = m_pInventory->GetAny(m_ammoTypes[i].c_str());
							}

							if (get_any == nullptr || get_any->cast_weapon_ammo() == nullptr)
							{
								return false;
							}
						}
					}

					if (!SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfRELOAD))
					{
						return false;
					}

					if (m_fRechargeTime > 0.0f && Device.GetTimeDeltaSafe(m_iLastShotTime) < std::floor(m_fLastRechargeTime * 1000.0f))
					{
						return false;
					}

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
			if (m_bGaussScheme)
			{
				if (SetKeyRepeatFlag(cmd == kWPN_FIREMODE_NEXT ? ACTOR_DEFS::EActorKeyflags::kfNEXTFIREMODE : ACTOR_DEFS::EActorKeyflags::kfPREVFIREMODE))
				{
					SwitchGaussScreen();
				}
			}
			else
			{
				ChangeFireMode(cmd);
			}
			return true;
		};
	}break;
	case kTACTICALTORCH:
	{
		if (flags & CMD_START && SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfTACTICALTORCH) && GetComponent<THudLightTorch>() != nullptr && !IsZoomed() && !IsPending() && GetState() != eFire)
		{
			m_eDevicesFlags.set(EDevicesFlags::df_tacticaltorch, true);
			SwitchState(eDevice);
			return true;
		}
		break;
	}
	case kLASER:
	{
		if (flags & CMD_START && SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfLASER) && GetComponent<THudLightLaser>() != nullptr && !IsZoomed() && !IsPending() && GetState() != eFire)
		{
			m_eDevicesFlags.set(EDevicesFlags::df_laser, true);
			SwitchState(eDevice);
			return true;
		}
		break;
	}
	case kMAG_CHECK:
	{
		if (flags & CMD_START && m_eAnimationsFlags.test(EAnimationsFlags::af_mag_check) && SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfMAGCHECK) && !IsZoomed() && !IsPending() && GetState() != eFire)
		{
			SwitchState(eMagCheck);
			return true;
		}
		break;
	}
	case kFIREMODE_CHECK:
	{
		if (flags & CMD_START && m_eAnimationsFlags.test(EAnimationsFlags::af_firemode_check) && SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfFIREMODECHECK) && !IsGrenadeMode() && !IsZoomed() && !IsPending() && GetState() != eFire)
		{
			SwitchState(eFiremodeCheck);
			return true;
		}
		break;
	}
	case kWPN_CHAMBER_LOAD:
	{
		if (flags & CMD_START && m_eAnimationsFlags.test(EAnimationsFlags::af_chamber_load) &&
			SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfCHAMBERLOAD) && !IsGrenadeMode() &&
			!IsZoomed() && !IsPending() && GetState() != eFire && !IsMisfire() && m_bAmmoInChamber && iAmmoChamberElapsed == 0 && !m_bHaveShell)
		{
			return TryReloadChamber();
		}
		break;
	}
	case kWPN_CHAMBER_UNLOAD:
	{
		if (flags & CMD_START && m_eAnimationsFlags.test(EAnimationsFlags::af_chamber_unload) &&
			SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfCHAMBERUNLOAD) && !IsGrenadeMode() &&
			!IsZoomed() && !IsPending() && GetState() != eFire && !IsMisfire() && m_bAmmoInChamber && iAmmoChamberElapsed != 0 && !m_bHaveShell)
		{
			SwitchState(eUnloadChamber);
			return true;
		}
		break;
	}
	case kWPN_CHAMBER_CHECK:
	{
		if (flags & CMD_START && m_bAmmoInChamber && m_eAnimationsFlags.test(EAnimationsFlags::af_chamber_check) &&
			SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfCHAMBERCHECK) && !IsGrenadeMode() &&
			!IsZoomed() && !IsPending() && GetState() != eFire && !IsMisfire())
		{
			SwitchState(eChamberCheck);
			return true;
		}
		break;
	}
	}
	return false;
}

bool CWeaponMagazined::CanAttach(PIItem pIItem)
{
	CScope* pScope = pIItem->cast_addon_scope();
	CSilencer* pSilencer = pIItem->cast_addon_silencer();
	CGrenadeLauncher* pGrenadeLauncher = pIItem->cast_addon_grenade_launcher();

	if (pScope && m_eScopeStatus == ALife::eAddonAttachable)
	{
		if (IsScopeAttached() && pIItem->object().cNameSect() == GetScopeName())
		{
			return false;
		}

		return ScopeFit(pScope);
	}
	else if (pSilencer && m_eSilencerStatus == ALife::eAddonAttachable && (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonSilencer) == 0 && (m_sSilencerName == pIItem->object().cNameSect()))
	{
		return true;
	}
	else if (pGrenadeLauncher && m_eGrenadeLauncherStatus == ALife::eAddonAttachable && (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) == 0 && (m_sGrenadeLauncherName == pIItem->object().cNameSect()))
	{
		return true;
	}
	else
	{
		return inherited::CanAttach(pIItem);
	}
}

bool CWeaponMagazined::CanDetach(const char* item_section_name)
{
	if (m_eScopeStatus == ALife::eAddonAttachable && 0 != (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonScope))
	{
		SCOPES_VECTOR_IT it = m_scopes.begin();
		for (; it != m_scopes.end(); it++)
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
	else if (m_eSilencerStatus == ALife::eAddonAttachable && 0 != (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonSilencer) && (m_sSilencerName == item_section_name))
	{
		return true;
	}
	else if (m_eGrenadeLauncherStatus == ALife::eAddonAttachable && 0 != (m_flagsAddOnState & CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher) && (m_sGrenadeLauncherName == item_section_name))
	{
		return true;
	}
	else
	{
		return inherited::CanDetach(item_section_name);
	}
}

bool CWeaponMagazined::Attach(PIItem pIItem, bool b_send_event)
{
	bool result = false;

	CScope* pScope = pIItem->cast_addon_scope();
	CSilencer* pSilencer = pIItem->cast_addon_silencer();
	CGrenadeLauncher* pGrenadeLauncher = pIItem->cast_addon_grenade_launcher();
	
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
	shared_str get_scope_section = cNameSect();

	m_zoom_params.m_fIronSightZoomFactor = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "ironsight_zoom_factor", 50.0f);

	if (IsScopeAttached())
	{
		if (m_eScopeStatus == ALife::eAddonAttachable)
		{
			get_scope_section = GetScopeName();
			LoadCurrentScopeParams(*get_scope_section);

			m_scope_recoil.m_fScopeAttachedRecoil = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "scope_attached_recoil_factor", 1.0f);

			m_scope_recoil.m_fScopeAttachedRecoilReduction = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "scope_attached_recoil_reduction", 1.0f);
		

			m_lens_zoom_params.factor_min = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "min_lens_factor", 1.0f);
			m_lens_zoom_params.factor_max = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "max_lens_factor", 1.0f);
			m_lens_zoom_params.need_lens_frame = READ_IF_EXISTS(pSettings, r_bool, get_scope_section, "need_lens_frame", false);

			m_lens_zoom_params.factor_min = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "min_lens_factor", 1.0f);
			m_lens_zoom_params.factor_max = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "max_lens_factor", 1.0f);
			m_lens_zoom_params.speed = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "lens_speed", 0.0f);
			m_lens_zoom_params.gyro_period = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "lens_gyro_sound_period", 0.0f);

			m_lens_zoom_params.lens_factor_levels_count = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "lens_factor_levels_count", 5.0f);
			m_lens_zoom_params.delta = 1.0f / m_lens_zoom_params.lens_factor_levels_count;

			m_lens_zoom_params.force_zoom_sound = READ_IF_EXISTS(pSettings, r_bool, get_scope_section, "force_zoom_sound", false);

			LoadNightBrightnessParamsFromSection(get_scope_section);

			get_scope_section = GetNameWithAttachmentScope();

			if (SoundExist(get_scope_section.c_str(), "snd_reload"))
			{
				m_sounds.LoadSound(get_scope_section.c_str(), "snd_reload", "sndReload", true, m_eSoundReload);
			}

			if (SoundExist(get_scope_section.c_str(), "snd_reload_empty"))
			{
				m_sounds.LoadSound(get_scope_section.c_str(), "snd_reload_empty", "sndReloadEmpty", true, m_eSoundReload);
			}
		}
	}
	else
	{
		if (m_UIScope)
		{
			xr_delete(m_UIScope);
		}

		if (IsZoomEnabled())
		{
			m_zoom_params.m_fIronSightZoomFactor = pSettings->r_float(get_scope_section, "scope_zoom_factor");
		}

		m_scope_recoil.m_fScopeAttachedRecoil = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "scope_attached_recoil_factor", 1.0f);

		m_scope_recoil.m_fScopeAttachedRecoilReduction = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "scope_attached_recoil_reduction", 1.0f);

		m_zoom_params.m_sUseBinocularVision = READ_IF_EXISTS(pSettings, r_string, get_scope_section, "scope_alive_detector", 0);
		m_zoom_params.m_sUseZoomPostprocess = READ_IF_EXISTS(pSettings, r_string, get_scope_section, "scope_nightvision", 0);

		if (m_zoom_params.m_sUseBinocularVision.size() <= 0)
		{
			DestroyComponent<TBinocularsVision>();
		}
		else
		{
			if (TBinocularsVision* Vision = GetOrCreateComponent<TBinocularsVision>())
			{
				Vision->Load(m_zoom_params.m_sUseBinocularVision);
			}
		}

		if (m_zoom_params.m_sUseZoomPostprocess.size() <= 0)
		{
			DestroyComponent<TWeaponNightVision>();
		}
		else
		{
			if (TWeaponNightVision* NightVision = GetOrCreateComponent<TWeaponNightVision>())
			{
				NightVision->SwitchNightVision(false);
				NightVision->Load(m_zoom_params.m_sUseZoomPostprocess);
			}
		}

		m_lens_zoom_params.factor_min = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "min_lens_factor", 1.0f);
		m_lens_zoom_params.factor_max = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "max_lens_factor", 1.0f);
		m_lens_zoom_params.need_lens_frame = READ_IF_EXISTS(pSettings, r_bool, get_scope_section, "need_lens_frame", false);

		m_lens_zoom_params.factor_min = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "min_lens_factor", 1.0f);
		m_lens_zoom_params.factor_max = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "max_lens_factor", 1.0f);
		m_lens_zoom_params.speed = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "lens_speed", 0.0f);
		m_lens_zoom_params.gyro_period = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "lens_gyro_sound_period", 0.0f);

		m_lens_zoom_params.lens_factor_levels_count = READ_IF_EXISTS(pSettings, r_float, get_scope_section, "lens_factor_levels_count", 5.0f);
		m_lens_zoom_params.delta = 1.0f / m_lens_zoom_params.lens_factor_levels_count;

		m_lens_zoom_params.force_zoom_sound = READ_IF_EXISTS(pSettings, r_bool, get_scope_section, "force_zoom_sound", false);

		LoadNightBrightnessParamsFromSection(get_scope_section);

		if (SoundExist(get_scope_section.c_str(), "snd_reload"))
		{
			m_sounds.LoadSound(get_scope_section.c_str(), "snd_reload", "sndReload", true, m_eSoundReload);
		}

		if (SoundExist(get_scope_section.c_str(), "snd_reload_empty"))
		{
			m_sounds.LoadSound(get_scope_section.c_str(), "snd_reload_empty", "sndReloadEmpty", true, m_eSoundReload);
		}
	}

	if (IsSilencerAttached())
	{
		CShootingObject::fire_mode = eSilencerFire;
		//подсветка от выстрела
		LoadLights(*cNameSect(), "silencer_");
		ApplySilencerKoeffs();
	}
	else
	{
		CShootingObject::fire_mode = eDefaultFire;

		//подсветка от выстрела
		LoadLights(*cNameSect(), "");
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
		const char* sect = m_sSilencerName.c_str();
		m_silencer_koef.hit_power		= READ_IF_EXISTS( pSettings, r_float, sect, "bullet_hit_power_k", 1.0f );
		m_silencer_koef.hit_impulse		= READ_IF_EXISTS( pSettings, r_float, sect, "bullet_hit_impulse_k", 1.0f );
		m_silencer_koef.bullet_speed	= READ_IF_EXISTS( pSettings, r_float, sect, "bullet_speed_k", 1.0f );
		m_silencer_koef.fire_dispersion	= READ_IF_EXISTS( pSettings, r_float, sect, "fire_dispersion_base_k", 1.0f );
		m_silencer_koef.cam_dispersion	= READ_IF_EXISTS( pSettings, r_float, sect, "cam_dispersion_k", 1.0f );
		m_silencer_koef.cam_disper_inc	= READ_IF_EXISTS( pSettings, r_float, sect, "cam_dispersion_inc_k", 1.0f );
		m_silencer_koef.attached_recoil = READ_IF_EXISTS(pSettings, r_float, sect, "attached_recoil_f", 1.0f);
	}

	clamp( m_silencer_koef.hit_power,		0.0f, 1.0f );
	clamp( m_silencer_koef.hit_impulse,		0.0f, 1.0f );
	clamp( m_silencer_koef.bullet_speed,	0.0f, 1.0f );
	clamp( m_silencer_koef.fire_dispersion,	0.0f, 3.0f );
	clamp( m_silencer_koef.cam_dispersion,	0.0f, 1.0f );
	clamp( m_silencer_koef.cam_disper_inc,	0.0f, 1.0f );
	clamp (m_silencer_koef.attached_recoil,  0.01f, 3.0f );
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
	PlayHUDMotion(SetCurrentStateAnimation("anm_show"), SetCurrentStateAnimation("anm_draw"), EHudMixType::eNoMix, GetState());
}

void CWeaponMagazined::PlayAnimHide()
{
	VERIFY(GetState()==eHiding);
	bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;
	PlayHUDMotion(SetCurrentStateAnimation("anm_hide"), empty && HudAnimationExist("anm_close", false) && !HudAnimationExist("anm_add_cartridge", false) ? "anm_close" : SetCurrentStateAnimation("anm_holster"), EHudMixType::eMixAll, GetState());
}

shared_str CWeaponMagazined::SetCurrentReloadAnimation()
{
	shared_str anim = "anm_reload";

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		if (m_bUseRevolverScheme && !IsGrenadeMode() && !IsMisfire() && !IsChangeAmmoType())
		{
			if (AddSuffixName(anim, shared_str().printf("_%d", iAmmoElapsed).c_str()))
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

		if (m_bGaussScreen)
		{
			AddSuffixName(anim, "_mui");
		}

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
		else if (m_bJustAfterReload)
		{
			AddSuffixName(anim, "_first");
		}

		if (IsChangeAmmoType())
		{
			AddSuffixName(anim, "_ammochange");
		}

		if (CActor* pActor = Level().CurrentControlEntity()->cast_actor())
		{
			CCustomDevice* Dev = pActor->GetDevice(true);
			if (Dev && (!Dev->IsHidden() || Dev->NeedActivation()))
			{
				AddSuffixName(anim, "_detector");
				AddSuffixName(anim, "_det");
			}
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

		if (m_bGaussScreen)
		{
			AddSuffixName(anim, "_mui");
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
		else if (empty)
		{
			AddSuffixName(anim, "_empty");
		}
		else if (m_bJustAfterReload)
		{
			AddSuffixName(anim, "_first");
		}

		if (const CActor* pActor = Level().CurrentControlEntity()->cast_actor())
		{
			if (pActor->IsSafemode())
			{
				AddSuffixName(anim, "_safemode");
			}
		}

		if (ScopeAttachable() && !IsScopeAttached())
		{
			AddSuffixName(anim, "_noscope");
		}

		if (CActor* pActor = Level().CurrentControlEntity()->cast_actor())
		{
			CCustomDevice* Dev = pActor->GetDevice(true);
			if (Dev && (!Dev->IsHidden() || Dev->NeedActivation()))
			{
				AddSuffixName(anim, "_detector");
				AddSuffixName(anim, "_det");
			}
		}
	}

	return anim;
}

shared_str CWeaponMagazined::SetCurrentIdleAnimation()
{
	bool empty = m_bAmmoInChamber ? iAmmoChamberElapsed == 0 : iAmmoElapsed == 0;
	if (empty && HudAnimationExist("anm_empty"))
	{
		return "anm_empty";
	}
	return inherited::SetCurrentIdleAnimation();
}

void CWeaponMagazined::PlayAnimReload()
{
	VERIFY(GetState() == eReload);

	PlayHUDMotion(SetCurrentReloadAnimation(), EHudMixType::eMixAll, GetState());
	if (ParentIsActor())
	{
		if (IsMisfire() && (HudAnimationExist("anm_reload_misfire") || HudAnimationExist("anm_reload_jammed")))
		{
			bMisfireReload = true;
		}

		CActor* actor = Level().CurrentControlEntity()->cast_actor();
		CCustomDevice* Dev = actor ? actor->GetDevice(true) : nullptr;
		if (Dev && (!Dev->IsHidden() || Dev->NeedActivation()) && HudAnimationExist("anm_reload_detector"))
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
		anim = IsGrenadeMode() && HudAnimationExist("anm_idle_g_aim") ? "anm_idle_g_aim" : (HudAnimationExist("anm_idle_w_gl_aim") ? "anm_idle_w_gl_aim" : (HudAnimationExist("anm_idle_gl_aim") ? "anm_idle_gl_aim" : anim));
	}

	if (CActor* actor = H_Parent()->cast_actor())
	{
		u32 state = actor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);
		if (state & ACTOR_DEFS::EMoveCommand::mcAnyMove)
		{
			if (!IsScopeAttached() || !AddSuffixName(anim, "_scope", "_moving"))
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
	PlayHUDMotion(SetCurrentAimAnimation(), EHudMixType::eMixAll, GetState());
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
	{
		return;
	}

	m_bIsAimAnimationPlaying = false;

	if (IsZoomed())
	{
		if (ParentIsActor() && !m_bIsAimStarted && m_eAnimationsFlags.test(EAnimationsFlags::af_aim_in_out))
		{
			m_bIsAimStarted = true;
			m_bIsAimAnimationPlaying = true;
			PlayHUDMotion(SetCurrentStateAnimation("anm_idle_aim_start"), EHudMixType::eMixAll, GetState());
			return;
		}

		PlayAnimAim();
	}
	else
	{
		if (ParentIsActor() && m_bIsAimStarted && m_eAnimationsFlags.test(EAnimationsFlags::af_aim_in_out))
		{
			m_bIsAimStarted = false;
			m_bIsAimAnimationPlaying = true;
			PlayHUDMotion(SetCurrentStateAnimation("anm_idle_aim_end"), EHudMixType::eMixAll, GetState());
			return;
		}

		if (TryPlayAnimIdle())
		{
			return;
		}

		shared_str new_name = SetCurrentIdleAnimation();

		PlayHUDMotion(SetCurrentStateAnimation(new_name), EHudMixType::eMixAll, GetState());
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

		if (m_bGaussScreen)
		{
			AddSuffixName(anim, "_mui");
		}

		if (IsScopeAttached())
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
		else if (last)
		{
			AddSuffixName(anim, "_last");
			AddSuffixName(anim, "_l");
		}

		if (m_bJustAfterReload)
		{
			AddSuffixName(anim, "_first");
		}
	}

	return anim;
}

void CWeaponMagazined::PlayAnimShoot()
{
	VERIFY(GetState()==eFire);

	if (m_bAmmoInChamber && !m_chamber.empty())
	{
		if (TShellBones* ShellBones = GetComponent<TShellBones>())
		{
			ShellBones->UpdateShellBones(this, m_chamber.back().m_LocalAmmoType);
		}
	}
	else if (!m_magazine.empty())
	{
		if (TShellBones* ShellBones = GetComponent<TShellBones>())
		{
			ShellBones->UpdateShellBones(this, m_magazine.back().m_LocalAmmoType);
		}
	}

	if (!IsMisfire())
	{
		m_iLastShotTime = Device.dwTimeGlobal;
	}

	PlayHUDMotion(SetCurrentShootAnimation(), EHudMixType::eMixHands, GetState());

	if (CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
	{
		if (CCustomDevice* pDevice = pActor->GetDevice())
		{
			if (pDevice->CanShooting())
			{
				pDevice->SwitchState(CCustomDevice::EDeviceStates::eHandShoot);
			}
		}
	}
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
	if (!HasFireModes())
	{
		return;
	}

	if (IsPending())
	{
		return;
	}

	if (m_bBlockFiremodeinGLM && IsGrenadeMode())
	{
		return;
	}

	if (IsZoomed() && m_eAnimationsFlags.test(EAnimationsFlags::af_firemode) && m_bDisableFireModeAim)
	{
		return;
	}

	if (!SetKeyRepeatFlag(cmd == kWPN_FIREMODE_NEXT ? ACTOR_DEFS::EActorKeyflags::kfNEXTFIREMODE : ACTOR_DEFS::EActorKeyflags::kfPREVFIREMODE))
	{
		return;
	}

	m_iPrevFireMode = GetQueueSize();

	const u8 modes_count = static_cast<u8>(m_aFireModes.size());

	if (cmd == kWPN_FIREMODE_NEXT)
	{
		m_iCurFireMode = (m_iCurFireMode + 1) % modes_count;
	}
	else
	{
		m_iCurFireMode = (m_iCurFireMode + modes_count - 1) % modes_count;
	}

	if (m_eAnimationsFlags.test(EAnimationsFlags::af_firemode))
	{
		SetPending(true);
		SwitchState(eSwitchMode);
	}
	else
	{
		SetQueueSize(GetCurrentFireMode());
	}
};

void CWeaponMagazined::SwitchGaussScreen()
{
	if (IsPending() && GetNextState() != eIdle)
	{
		return;
	}

	if (IsZoomed())
	{
		return;
	}

	if (IsGrenadeMode())
	{
		return;
	}

	m_bGaussScreen = !m_bGaussScreen;

	SetPending(true);
	SwitchState(eSwitchMode);
}

void CWeaponMagazined::OnH_A_Chield()
{
	SetQueueSize(H_Parent() && H_Parent()->cast_actor() ? GetCurrentFireMode() : -1);
	StopShooting();
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

void CWeaponMagazined::OnEvent(NET_Packet& P, u16 type)
{
	switch (type)
	{
	case GE_WPN_UNLOAD_AMMO:
	{
		u8 ignore = P.r_u8();
		UnloadMagazine();
	}break;
	case GE_WPN_UPDATE_AMMO:
	{
		u32 count = P.r_u32();
		for (u16 i = 0; i < count; ++i)
		{
			u16 id = P.r_u16();
			u16 boxSize = P.r_u16();
			CObject* obj = Level().Objects.net_Find(id);

			if (CWeaponAmmo* pA = obj != nullptr ? obj->cast_weapon_ammo() : nullptr)
			{
				pA->m_boxCurr = boxSize;
			}
		}
	}break;
	default:
	{
		inherited::OnEvent(P, type);
	}break;
	}
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
		const char* ammo_type = m_ammoTypes[CurrVector.back().m_LocalAmmoType].c_str();
		info.name		= g_pStringTable->translate( pSettings->r_string(ammo_type, "inv_name_short") );
		info.icon		= ammo_type;
	}
	else
	{
		const char* ammo_type	= m_ammoTypes[CurrAmmoType].c_str();
		info.name			= g_pStringTable->translate( pSettings->r_string(ammo_type, "inv_name_short") );
		info.icon			= ammo_type;
	}
	return true;
}

bool CWeaponMagazined::install_upgrade_impl(const char* section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	const char* str = {};

	bool result2 = process_if_exists_set(section, "fire_modes", str, test);
	if (result2 && !test)
	{
		int ModesCount = _GetItemCount(str);
		m_aFireModes.clear();
		for (int i = 0; i < ModesCount; ++i)
		{
			string16 sItem = {};
			_GetItem(str, i, sItem);
			m_aFireModes.push_back((s8)atoi(sItem));
		}
		SetQueueSize(m_iCurFireMode = ModesCount - 1);
	}
	result |= result2;

	result |= process_if_exists_set(section, "base_dispersioned_bullets_count", m_iBaseDispersionedBulletsCount, test);
	result |= process_if_exists_set(section, "base_dispersioned_bullets_speed", m_fBaseDispersionedBulletsSpeed, test);

	result2 = process_if_exists_set(section, "snd_draw", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_draw", "sndShow", false, m_eSoundShow); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_holster", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_holster", "sndHide", false, m_eSoundHide); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_shoot", str, test);
	if (result2 && !test)
	{
		m_layered_sounds.LoadSound(section, "snd_shoot", "sndShot", false, m_eSoundShot, st_Shooting);
	}
	result |= result2;

	result2 = process_if_exists_set(section, "snd_shoot_actor", str, test);
	if (result2 && !test)
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor, true);
		m_layered_sounds.LoadSound(section, "snd_shoot_actor", "sndShotActor", false, m_eSoundShot, st_Shooting);
	}
	result |= result2;

	result2 = process_if_exists_set(section, "snd_shot_last", str, test);
	if (result2 && !test)
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_last, true);
		m_layered_sounds.LoadSound(section, "snd_shot_last", "sndShotLast", false, m_eSoundShot, st_Shooting);
	}
	result |= result2;

	result2 = process_if_exists_set(section, "snd_shot_last_actor", str, test);
	if (result2 && !test)
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_last, true);
		m_layered_sounds.LoadSound(section, "snd_shot_last_actor", "sndShotLastActor", false, m_eSoundShot, st_Shooting);
	}
	result |= result2;

	result2 = process_if_exists_set(section, "snd_empty", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_empty", "sndEmptyClick", false, m_eSoundEmptyClick); }
	result |= result2;

	result2 = process_if_exists_set(section, "snd_reload", str, test);
	if (result2 && !test) { m_sounds.LoadSound(section, "snd_reload", "sndReload", true, m_eSoundReload); }
	result |= result2;

	if (m_eSilencerStatus == ALife::eAddonAttachable || m_eSilencerStatus == ALife::eAddonPermanent)
	{
		const char* sil_ps = nullptr;
		result |= process_if_exists_set(section, "silencer_flame_particles", sil_ps, test);
		if(sil_ps)
			LoadParticle(section, sil_ps, m_pFlameSilencerParticles);
		result |= process_if_exists_set(section, "silencer_smoke_particles", sil_ps, test);
		if (sil_ps)
			LoadParticle(section, sil_ps, m_pSmokeSilencerParticles);

		result2 = process_if_exists_set(section, "snd_silncer_shot", str, test);
		if (result2 && !test)
		{
			m_layered_sounds.LoadSound(section, "snd_silncer_shot", "sndSilencerShot", false, m_eSoundShot, st_Shooting);
		}
		result |= result2;

		result2 = process_if_exists_set(section, "snd_silncer_shot_actor", str, test);
		if (result2 && !test)
		{
			m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_sil, true);
			m_layered_sounds.LoadSound(section, "snd_silncer_shot_actor", "sndSilencerShotActor", false, m_eSoundShot, st_Shooting);
		}
		result |= result2;

		result2 = process_if_exists_set(section, "snd_silencer_shot_last", str, test);
		if (result2 && !test)
		{
			m_eSoundsFlags.set(ESoundsFlags::sf_shoot_last_sil, true);
			m_layered_sounds.LoadSound(section, "snd_silencer_shot_last", "sndSilencerShotLast", false, m_eSoundShot);
		}
		result |= result2;

		result2 = process_if_exists_set(section, "snd_silencer_shot_last_actor", str, test);
		if (result2 && !test)
		{
			m_eSoundsFlags.set(ESoundsFlags::sf_shoot_actor_last_sil, true);
			m_layered_sounds.LoadSound(section, "snd_silencer_shot_last_actor", "sndSilencerShotLastActor", false, m_eSoundShot);
		}
		result |= result2;
	}

	result |= process_if_exists(section, "ironsight_zoom_factor", m_zoom_params.m_fIronSightZoomFactor, test);

	if (IsScopeAttached())
	{
		result |= process_if_exists(section, "scope_zoom_factor", m_zoom_params.m_fScopeZoomFactor, test);
	}
	else
	{
		if (IsZoomEnabled())
		{
			result |= process_if_exists(section, "scope_zoom_factor", m_zoom_params.m_fIronSightZoomFactor, test);
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

void CWeaponMagazined::OnMotionMark(u8 state, const motion_marks& mark)
{
	inherited::OnMotionMark(state, mark);

	if (ParentIsActor() && (!m_bTriStateReload || bMisfire) && state == eReload && mark.name == "Right" && !m_bIsReloaded)
	{
		m_bIsReloaded = true;
		bool grenade_mode = IsGrenadeMode();
		if (bMisfireReload && !grenade_mode)
		{
			bMisfire = false;
			bMisfireReload = false;

			if (NeedMisfireAmmo && iAmmoElapsed + iAmmoChamberElapsed > 0)
			{
				if (m_bAmmoInChamber)
				{
					DeleteAmmoInChamber();
					GiveAmmoFromMagToChamber();
				}
				else
				{
					SetAmmoElapsed(iAmmoElapsed - 1);
				}
			}
		}
		else
		{
			int base_mag_size = iMagazineSize;
			int new_mag_size = GetMagCapacity();
			iMagazineSize = new_mag_size;
			ReloadMagazine();
			iMagazineSize = base_mag_size;

			GiveAmmoFromMagToChamber();
		}

		if (!grenade_mode)
		{
			m_bJustAfterReload = true;
		}
	}

	if (state == eDevice && mark.name == "Left")
	{
		if (m_eDevicesFlags.test(EDevicesFlags::df_tacticaltorch))
		{
			m_bTacticalTorchStatus = !m_bTacticalTorchStatus;
		}
		else if (m_eDevicesFlags.test(EDevicesFlags::df_laser))
		{
			m_bTacticalLaserStatus = !m_bTacticalLaserStatus;
		}

		m_eDevicesFlags.zero();
	}

	if (state == eSwitchMode && mark.name == "Right")
	{
		SetQueueSize(GetCurrentFireMode());
	}

	if (!IsGrenadeMode() && state == eMagCheck && mark.name == "Right")
	{
		if (CUIGameCustom* ui = CurrentGameUI())
		{
			if (auto magcheck = ui->AddCustomStatic("mag_check", true))
			{
				int CurrMagSize = GetMagCapacity();
				int Elapsed = iAmmoElapsed;

				if (Elapsed == 0)
				{
					magcheck->SetText("st_mag_empty");
				}
				else if (Elapsed == CurrMagSize)
				{
					magcheck->SetText("st_mag_full");
				}
				else
				{
					float mag_factor = (float)Elapsed / (float)CurrMagSize;

					if (mag_factor >= 0.65f)
					{
						magcheck->SetText("st_mag_nearly_full");
					}
					else if (mag_factor >= 0.5f)
					{
						magcheck->SetText("st_mag_near_medium");
					}
					else if (mag_factor >= 0.3f)
					{
						magcheck->SetText("st_mag_less_half");
					}
					else
					{
						magcheck->SetText("st_mag_nearly_empty");
					}
				}
			}
		}
	}

	if (state == eChamberCheck && mark.name == "Right")
	{
		if (CUIGameCustom* ui = CurrentGameUI())
		{
			if (auto magcheck = ui->AddCustomStatic("chamber_check", true))
			{
				shared_str static_info = g_pStringTable->translate("st_chamber_ammo");

				if (m_bHaveShell && m_bNeedPumpState)
				{
					static_info = g_pStringTable->translate("st_chamber_shell");
					const char* ammo_type = m_ammoTypes[m_LastShotAmmoType].c_str();
					static_info.printf("%s %s", static_info.c_str(), g_pStringTable->translate(pSettings->r_string(ammo_type, "inv_name_short")).c_str());
					magcheck->SetText(static_info.c_str());
				}
				else if (!m_chamber.empty())
				{
					static_info.printf("%s %s", static_info.c_str(), m_chamber.back().m_InvShortName.c_str());
					magcheck->SetText(static_info.c_str());
				}
			}
		}
	}
}

void CWeaponMagazined::UpdateBonePartAnimations()
{
	inherited::UpdateBonePartAnimations();

	UpdateFiremodeAnimations();
	UpdateIdleAnimations();
}

void CWeaponMagazined::UpdateFiremodeAnimations()
{
	if (!m_eBonePartAnimationsFlags.test(EBPAnimsFlags::abpf_firemode))
	{
		return;
	}

	shared_str anim_name = "anm_bp_firemode_state_auto";
	if (GetQueueSize() != -1)
	{
		anim_name = "anm_bp_firemode_state_";
		anim_name.printf("%s%d", *anim_name, GetQueueSize());
	}

	PlayBonePartAnim(anim_name, false);
}

void CWeaponMagazined::UpdateIdleAnimations()
{
	if (!m_eBonePartAnimationsFlags.test(EBPAnimsFlags::abpf_idle))
	{
		return;
	}

	shared_str anim_name = "anm_bp_idle";

	if (IsMisfire() && m_eBonePartAnimationsFlags.test(EBPAnimsFlags::abpf_idle_jammed))
	{
		anim_name = "anm_bp_idle_jammed";
	}
	else if (GetCurrentElapsed(false) + iAmmoChamberElapsed == 0 && m_eBonePartAnimationsFlags.test(EBPAnimsFlags::abpf_idle_empty))
	{
		anim_name = "anm_bp_idle_empty";
	}

	PlayBonePartAnim(anim_name, false);
}