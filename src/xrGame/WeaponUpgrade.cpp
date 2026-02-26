#include "StdAfx.h"
#include "Weapon.h"

bool CWeapon::install_upgrade_impl(LPCSTR section, bool test)
{
	//inherited::install_upgrade( section );
	bool result = CInventoryItemObject::install_upgrade_impl(section, test);

	result |= install_upgrade_ammo_class(section, test);
	result |= install_upgrade_disp(section, test);
	result |= install_upgrade_hit(section, test);
	result |= install_upgrade_addon(section, test);
	result |= install_upgrade_hud_sect(section, test);
	result |= install_upgrade_hud_sect_silencer(section, test);
	result |= install_upgrade_hud_sect_scope(section, test);
	result |= install_upgrade_hud_sect_gl(section, test);
	result |= install_upgrade_bones(section, test);
	result |= install_upgrade_ammo_bones(section, test);
	result |= install_upgrade_torch_laser(section, test);
	result |= install_upgrade_scope_zoom(section, test);
	result |= install_upgrade_fast_knife(section, test);
	result |= process_if_exists_set(section, "collimator_problems_level", m_fCollimatorLevelsProblem, test) && !test;

	bool result2 = process_if_exists_set(section, "use_gauss_scheme", m_bGaussScheme, test);

	s32 val = m_iAutoAimTime;
	result2 = process_if_exists_set(section, "autoaim_time", val, test);
	if (result2 && !test)
	{
		m_iAutoAimTime = val;
	}
	result |= result2;

	result |= process_if_exists_set(section, "autoaim_only_alive", m_bAutoAimOnlyAlive, test);
	result |= process_if_exists_set(section, "autoaim_ignore_dead", m_bAutoAimIgnoreDead, test);
	result |= process_if_exists_set(section, "autoaim_shot_after_key_released", m_bAutoAimShotAfterKeyReleased, test);
	result |= process_if_exists_set(section, "autoaim_auto_shot", m_bAutoAimAutoShot, test);

	result |= process_if_exists(section, "recharge_time", m_fRechargeTime, test);

	result |= process_if_exists_set(section, "use_revolver_scheme", m_bUseRevolverScheme, test);
	result |= process_if_exists_set(section, "use_mosin_scheme", m_bUseMosinScheme, test);

	return result;
}

bool CWeapon::install_upgrade_ammo_class(LPCSTR section, bool test)
{
	LPCSTR str;

	bool result = process_if_exists(section, "ammo_mag_size", iMagazineSize, test);

	//	ammo_class = ammo_5.45x39_fmj, ammo_5.45x39_ap  // name of the ltx-section of used ammo
	bool result2 = process_if_exists_set(section, "ammo_class", str, test);
	if (result2 && !test)
	{
		m_ammoTypes.clear();
		string128 ammoItem;
		int count = _GetItemCount(str);
		for (int i = 0; i < count; ++i)
		{
			_GetItem(str, i, ammoItem);
			m_ammoTypes.push_back(ammoItem);
		}
		m_ammoType = 0;
	}
	result |= result2;

	for (u8 i = 0; i < m_ammoTypes.size(); ++i)
	{
		shared_str key;
		key.printf("ammo_mag_size_for_type_%d", i);

		if (!pSettings->line_exist(section, *key))
		{
			continue;
		}

		u32 value = pSettings->r_u32(section, *key);
		bool found = false;

		for (auto& capacity : m_mags_capacity)
		{
			if (capacity.first == i)
			{
				capacity.second = value;
				found = true;
				break;
			}
		}

		if (!found)
		{
			m_mags_capacity.emplace_back(i, value);
		}
	}

	return result;
}

bool CWeapon::install_upgrade_disp(LPCSTR section, bool test)
{
	bool result = process_if_exists(section, "fire_dispersion_condition_factor", fireDispersionConditionFactor, test);
	result |= process_if_exists(section, "fire_distance", fireDistance, test);


	u8 rm = (cam_recoil.ReturnMode) ? 1 : 0;
	result |= process_if_exists_set(section, "cam_return", rm, test);
	cam_recoil.ReturnMode = (rm == 1);

	rm = (cam_recoil.StopReturn) ? 1 : 0;
	result |= process_if_exists_set(section, "cam_return_stop", rm, test);
	cam_recoil.StopReturn = (rm == 1);

	result |= process_if_exists_deg2rad(section, "fire_dispersion_base", fireDispersionBase, test);

	result |= process_if_exists_deg2rad(section, "cam_relax_speed", cam_recoil.RelaxSpeed, test);
	result |= process_if_exists_deg2rad(section, "cam_relax_speed_ai", cam_recoil.RelaxSpeed_AI, test);
	result |= process_if_exists_deg2rad(section, "cam_dispersion", cam_recoil.Dispersion, test);
	result |= process_if_exists_deg2rad(section, "cam_dispersion_inc", cam_recoil.DispersionInc, test);

	result |= process_if_exists(section, "cam_dispersion_frac", cam_recoil.DispersionFrac, test);

	result |= process_if_exists_deg2rad(section, "cam_max_angle", cam_recoil.MaxAngleVert, test);
	result |= process_if_exists_deg2rad(section, "cam_max_angle_horz", cam_recoil.MaxAngleHorz, test);
	result |= process_if_exists_deg2rad(section, "cam_step_angle_horz", cam_recoil.StepAngleHorz, test);

	VERIFY(!fis_zero(cam_recoil.RelaxSpeed));
	VERIFY(!fis_zero(cam_recoil.RelaxSpeed_AI));
	VERIFY(!fis_zero(cam_recoil.MaxAngleVert));
	VERIFY(!fis_zero(cam_recoil.MaxAngleHorz));

	result |= process_if_exists_deg2rad(section, "zoom_cam_relax_speed", zoom_cam_recoil.RelaxSpeed, test);// zoom_ ...
	result |= process_if_exists_deg2rad(section, "zoom_cam_relax_speed_ai", zoom_cam_recoil.RelaxSpeed_AI, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_dispersion", zoom_cam_recoil.Dispersion, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_dispersion_inc", zoom_cam_recoil.DispersionInc, test);

	result |= process_if_exists(section, "zoom_cam_dispersion_frac", zoom_cam_recoil.DispersionFrac, test);

	result |= process_if_exists_deg2rad(section, "zoom_cam_max_angle", zoom_cam_recoil.MaxAngleVert, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_max_angle_horz", zoom_cam_recoil.MaxAngleHorz, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_step_angle_horz", zoom_cam_recoil.StepAngleHorz, test);

	VERIFY(!fis_zero(zoom_cam_recoil.RelaxSpeed));
	VERIFY(!fis_zero(zoom_cam_recoil.RelaxSpeed_AI));
	VERIFY(!fis_zero(zoom_cam_recoil.MaxAngleVert));
	VERIFY(!fis_zero(zoom_cam_recoil.MaxAngleHorz));

	result |= process_if_exists(section, "pattern_factor", cam_recoil.Pattern.Factor, test);
	result |= process_if_exists(section, "pattern_stiffness", cam_recoil.Pattern.Stiffness, test);
	result |= process_if_exists(section, "pattern_damping", cam_recoil.Pattern.Damping, test);
	result |= process_if_exists(section, "pattern_impulse", cam_recoil.Pattern.Impulse, test);
	result |= process_if_exists(section, "pattern_return_speed", cam_recoil.Pattern.ReturnSpeed, test);
	result |= process_if_exists(section, "zoom_pattern_factor", zoom_cam_recoil.Pattern.Factor, test);
	result |= process_if_exists(section, "zoom_pattern_stiffness", zoom_cam_recoil.Pattern.Stiffness, test);
	result |= process_if_exists(section, "zoom_pattern_damping", zoom_cam_recoil.Pattern.Damping, test);
	result |= process_if_exists(section, "zoom_pattern_impulse", zoom_cam_recoil.Pattern.Impulse, test);
	result |= process_if_exists(section, "zoom_pattern_return_speed", zoom_cam_recoil.Pattern.ReturnSpeed, test);


	result |= process_if_exists(section, "PDM_disp_base", m_pdm.m_fPDM_disp_base, test);
	result |= process_if_exists(section, "PDM_disp_vel_factor", m_pdm.m_fPDM_disp_vel_factor, test);
	result |= process_if_exists(section, "PDM_disp_accel_factor", m_pdm.m_fPDM_disp_accel_factor, test);
	result |= process_if_exists(section, "PDM_disp_crouch", m_pdm.m_fPDM_disp_crouch, test);
	result |= process_if_exists(section, "PDM_disp_crouch_no_acc", m_pdm.m_fPDM_disp_crouch_no_acc, test);

	result |= process_if_exists(section, "misfire_probability", misfireProbability, test);
	result |= process_if_exists(section, "misfire_condition_k", misfireConditionK, test);
	result |= process_if_exists(section, "condition_shot_dec", conditionDecreasePerShot, test);
	result |= process_if_exists(section, "condition_queue_shot_dec", conditionDecreasePerQueueShot, test);
	result |= process_if_exists(section, "misfire_start_condition", misfireStartCondition, test);
	result |= process_if_exists(section, "misfire_end_condition", misfireEndCondition, test);
	result |= process_if_exists(section, "misfire_start_prob", misfireStartProbability, test);
	result |= process_if_exists(section, "misfire_end_prob", misfireEndProbability, test);

	bool result2 = process_if_exists_set(section, "zoom_enabled", m_zoom_params.m_bZoomEnabled, test);

	return result;
}

bool CWeapon::install_upgrade_hit(LPCSTR section, bool test)
{
	bool result = false;

	shared_str	s_sHitPower;
	bool result2 = process_if_exists_set(section, "hit_power", s_sHitPower, test);
	if (result2 && !test)
	{
		string32 buffer;
		fvHitPower[egdMaster] = (float)atof(_GetItem(*s_sHitPower, 0, buffer));
		fvHitPower[egdNovice] = fvHitPower[egdStalker] = fvHitPower[egdVeteran] = fvHitPower[egdMaster];

		int num_game_diff_param = _GetItemCount(*s_sHitPower);
		if (num_game_diff_param > 1) { fvHitPower[egdVeteran] = (float)atof(_GetItem(*s_sHitPower, 1, buffer)); }
		if (num_game_diff_param > 2) { fvHitPower[egdStalker] = (float)atof(_GetItem(*s_sHitPower, 2, buffer)); }
		if (num_game_diff_param > 3) { fvHitPower[egdNovice] = (float)atof(_GetItem(*s_sHitPower, 3, buffer)); }
	}
	result |= result2;

	shared_str	s_sHitPowerCritical;
	result2 = process_if_exists_set(section, "hit_power_critical", s_sHitPower, test);
	if (result2 && !test)
	{
		string32 buffer;
		fvHitPowerCritical[egdMaster] = (float)atof(_GetItem(*s_sHitPowerCritical, 0, buffer));
		fvHitPowerCritical[egdNovice] = fvHitPowerCritical[egdStalker] = fvHitPowerCritical[egdVeteran] = fvHitPowerCritical[egdMaster];

		int num_game_diff_param = _GetItemCount(*s_sHitPowerCritical);
		if (num_game_diff_param > 1) { fvHitPowerCritical[egdVeteran] = (float)atof(_GetItem(*s_sHitPowerCritical, 1, buffer)); }
		if (num_game_diff_param > 2) { fvHitPowerCritical[egdStalker] = (float)atof(_GetItem(*s_sHitPowerCritical, 2, buffer)); }
		if (num_game_diff_param > 3) { fvHitPowerCritical[egdNovice] = (float)atof(_GetItem(*s_sHitPowerCritical, 3, buffer)); }
	}
	result |= result2;

	result |= process_if_exists(section, "hit_impulse", fHitImpulse, test);
	result |= process_if_exists(section, "bullet_speed", m_fStartBulletSpeed, test);

	/*
	silencer_hit_power           = 0.55, 0.55, 0.55, 0.55
	silencer_hit_impulse         = 120
	silencer_fire_distance       = 600
	silencer_bullet_speed        = 310
	*/

	result |= process_if_exists_set(section, "use_aim_bullet", m_bUseAimBullet, test);
	if (m_bUseAimBullet) // first super bullet
	{
		result |= process_if_exists(section, "time_to_aim", m_fTimeToAim, test);
	}

	//	LPCSTR weapon_section = cNameSect().c_str(); 
	float rpm = 60.0f / fOneShotTime;//pSettings->r_float( weapon_section, "rpm" ); // fOneShotTime * 60.0f;
	result2 = process_if_exists(section, "rpm", rpm, test);
	if (result2 && !test)
	{
		VERIFY(rpm > 0.0f);
		fOneShotTime = 60.0f / rpm;
	}
	result |= result2;

	return result;
}


bool CWeapon::install_upgrade_addon(LPCSTR section, bool test)
{
	bool result = false;
	//LPCSTR weapon_section = cNameSect().c_str(); 

	// 0 - no addon // 1 - permanent // 2 - attachable
	int temp_int = (int)m_eScopeStatus;
	bool result2 = process_if_exists_set(section, "scope_status", temp_int, test);
	if (result2 && !test)
	{
		m_eScopeStatus = (ALife::EWeaponAddonStatus)temp_int;
		if (m_eScopeStatus == ALife::eAddonAttachable || m_eScopeStatus == ALife::eAddonPermanent)
		{
			result |= process_if_exists(section, "holder_range_modifier", m_addon_holder_range_modifier, test);
			result |= process_if_exists(section, "holder_fov_modifier", m_addon_holder_fov_modifier, test);
			bUseAltScope = pSettings->line_exist(section, "scopes");

			if (bUseAltScope)
			{
				LPCSTR str = pSettings->r_string(section, "scopes");
				for (int i = 0, count = _GetItemCount(str); i < count; ++i)
				{
					string128 scope_section;
					_GetItem(str, i, scope_section);

					if (!xr_strcmp(scope_section, "none"))
					{
						bUseAltScope = 0;
					}
					else
					{
						m_scopes.push_back(scope_section);
					}
				}
			}
			else
			{
				if (m_eScopeStatus == ALife::eAddonAttachable)
				{
					if (pSettings->line_exist(section, "scopes_sect"))
					{
						LPCSTR str = pSettings->r_string(section, "scopes_sect");
						for (int i = 0, count = _GetItemCount(str); i < count; ++i)
						{
							string128						scope_section;
							_GetItem(str, i, scope_section);
							m_scopes.push_back(scope_section);
						}
					}
					else
					{
						m_scopes.push_back(section);
					}
				}
				else
				{
					m_scopes.push_back(section);
					if (m_eScopeStatus == ALife::eAddonPermanent)
						InitAddons();
				}
			}
		}
	}
	result |= process_if_exists_set(section, "scope_dynamic_zoom", m_zoom_params.m_bUseDynamicZoom, test);
	result |= process_if_exists_set(section, "scope_nightvision", m_zoom_params.m_sUseZoomPostprocess, test);
	result |= process_if_exists_set(section, "scope_alive_detector", m_zoom_params.m_sUseBinocularVision, test);

	result |= result2;

	temp_int = (int)m_eSilencerStatus;
	result2 = process_if_exists_set(section, "silencer_status", temp_int, test);
	if (result2 && !test)
	{
		m_eSilencerStatus = (ALife::EWeaponAddonStatus)temp_int;
		if (m_eSilencerStatus == ALife::eAddonAttachable || m_eSilencerStatus == ALife::eAddonPermanent)
		{
			m_sSilencerName = pSettings->r_string(section, "silencer_name");

			m_iSilencerX = pSettings->r_s32(section, "silencer_x") * ScaleIcon;
			m_iSilencerY = pSettings->r_s32(section, "silencer_y") * ScaleIcon;

			if (m_eSilencerStatus == ALife::eAddonPermanent)
				InitAddons();
		}
	}
	result |= result2;

	temp_int = (int)m_eGrenadeLauncherStatus;
	result2 = process_if_exists_set(section, "grenade_launcher_status", temp_int, test);
	if (result2 && !test)
	{
		m_eGrenadeLauncherStatus = (ALife::EWeaponAddonStatus)temp_int;
		if (m_eGrenadeLauncherStatus == ALife::eAddonAttachable || m_eGrenadeLauncherStatus == ALife::eAddonPermanent)
		{
			m_sGrenadeLauncherName = pSettings->r_string(section, "grenade_launcher_name");

			m_iGrenadeLauncherX = pSettings->r_s32(section, "grenade_launcher_x") * ScaleIcon;
			m_iGrenadeLauncherY = pSettings->r_s32(section, "grenade_launcher_y") * ScaleIcon;

			if (m_eGrenadeLauncherStatus == ALife::eAddonPermanent)
				InitAddons();
		}
	}

	result |= result2;
	return result;
}

bool CWeapon::install_upgrade_hud_sect(LPCSTR section, bool test)
{
	LPCSTR str;

	bool result = process_if_exists_set(section, "hud", str, test);

	if (result && !test)
	{
		shared_str new_hud_sect = pSettings->r_string(section, "hud");
		shared_str old_hud = hud_sect_cache;

		if (new_hud_sect == "skip_reassign")
			hud_sect = old_hud;
		else
			hud_sect = new_hud_sect;

		hud_sect_cache = hud_sect;
	}

	InitAddons();

	return result;
}

bool CWeapon::install_upgrade_hud_sect_silencer(LPCSTR section, bool test)
{
	LPCSTR str;

	bool result = process_if_exists_set(section, "hud_silencer", str, test);

	if (result && !test)
	{
		hud_silencer = pSettings->r_string(section, "hud_silencer");
		m_bUseSilHud = pSettings->r_bool(section, "hud_when_silencer_is_attached");
	}

	InitAddons();

	return result;
}

bool CWeapon::install_upgrade_hud_sect_scope(LPCSTR section, bool test)
{
	LPCSTR str;

	bool result = process_if_exists_set(section, "hud_scope", str, test);

	if (result && !test)
	{
		hud_scope = pSettings->r_string(section, "hud_scope");
		m_bUseScopeHud = pSettings->r_bool(section, "hud_when_scope_is_attached");
	}

	InitAddons();

	return result;
}

bool CWeapon::install_upgrade_hud_sect_gl(LPCSTR section, bool test)
{
	LPCSTR str;

	bool result = process_if_exists_set(section, "hud_gl", str, test);

	if (result && !test)
	{
		hud_scope = pSettings->r_string(section, "hud_gl");
		m_bUseGLHud = pSettings->r_bool(section, "hud_when_gl_is_attached");
	}

	InitAddons();

	return result;
}

bool CWeapon::install_upgrade_bones(LPCSTR section, bool test)
{
	LPCSTR str;

	bool result = false;

	bool result2 = process_if_exists_set(section, "hide_bones_override", str, test);

	if (result2 && !test)
	{
		LPCSTR S = pSettings->r_string(section, "hide_bones_override");
		if (S && S[0])
		{
			string128 Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				m_bHideBonesOverride.push_back(Item);
			}
		}
	}

	result |= result2;

	result2 = process_if_exists_set(section, "hide_bones_override_when_silencer_attached", str, test);

	if (result2 && !test)
	{
		LPCSTR S = pSettings->r_string(section, "hide_bones_override_when_silencer_attached");
		if (S && S[0])
		{
			string128 Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				m_bHideBonesSilAttached.push_back(Item);
			}
		}
	}

	result |= result2;

	result2 = process_if_exists_set(section, "hide_bones_override_when_gl_attached", str, test);

	if (result2 && !test)
	{
		LPCSTR S = pSettings->r_string(section, "hide_bones_override_when_gl_attached");
		if (S && S[0])
		{
			string128 Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				m_bHideBonesGLAttached.push_back(Item);
			}
		}
	}

	result |= result2;

	result2 = process_if_exists_set(section, "hide_bones_override_when_scope_attached", str, test);

	if (result2 && !test)
	{
		LPCSTR S = pSettings->r_string(section, "hide_bones_override_when_scope_attached");
		if (S && S[0])
		{
			string128 Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				m_bHideBonesScopeAttached.push_back(Item);
			}
		}
	}

	result |= result2;

	result2 = process_if_exists_set(section, "hide_bones", str, test);

	if (result2 && !test)
	{
		LPCSTR S = pSettings->r_string(section, "hide_bones");
		if (S && S[0])
		{
			string128 Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				m_bHideBonesUpgrade.push_back(Item);
			}
		}
	}

	result |= result2;

	result2 = process_if_exists_set(section, "show_bones", str, test);

	if (result2 && !test)
	{
		LPCSTR S = pSettings->r_string(section, "show_bones");
		if (S && S[0])
		{
			string128 Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				m_bShowBonesUpgToShow.push_back(Item);
			}
		}
	}

	result |= result2;

	UpdateAddonsVisibility();
	UpdateHUDAddonsVisibility();
	ProcessScope();

	return result;
}

bool CWeapon::install_upgrade_ammo_bones(LPCSTR section, bool test)
{
	bool result = false;

	bool need_add[2] = { false };

	if (m_ammo_bones_mag.size() == 1)
	{
		if (m_ammo_bones_mag[0]->AmmoType != undefined_ammo_type)
		{
			need_add[0] = true;
		}
	}

	if (m_shell_bones.size() == 1)
	{
		if (m_shell_bones[0]->AmmoType != undefined_ammo_type)
		{
			need_add[1] = true;
		}
	}

	if (need_add[1])
	{
		for (SAmmoBonesParams* param : m_shell_bones)
		{
			xr_delete(param);
		}
		m_shell_bones.clear();
	}

	if (need_add[0])
	{
		for (SAmmoBonesParams* param : m_ammo_bones_mag)
		{
			xr_delete(param);
		}
		m_ammo_bones_mag.clear();
	}

	if (need_add[1])
	{
		if (pSettings->line_exist(hud_sect, "shell_params_section"))
		{
			SAmmoBonesParams* bone_params = new SAmmoBonesParams(undefined_ammo_type);
			bone_params->Load(pSettings->r_string(hud_sect, "shell_params_section"), -1);
			m_shell_bones.push_back(bone_params);
			result = true;
		}
		else for (int i = 0; i < m_ammoTypes.size(); i++)
		{
			static shared_str params_section;
			params_section.printf("shell_params_section_%d", i);
			if (pSettings->line_exist(hud_sect, *params_section))
			{
				SAmmoBonesParams* bone_params = new SAmmoBonesParams(i);
				bone_params->Load(pSettings->r_string(hud_sect, *params_section), -1);
				m_shell_bones.push_back(bone_params);
				result = true;
			}
		}
	}
	else
	{
		if (pSettings->line_exist(hud_sect, "shell_params_section"))
		{
			for (auto& bone_param : m_shell_bones)
			{
				if (bone_param->AmmoType == undefined_ammo_type)
				{
					bone_param->Load(pSettings->r_string(hud_sect, "shell_params_section"), -1);
					result = true;
				}
			}
		}
		else for (int i = 0; i < m_ammoTypes.size(); i++)
		{
			static shared_str params_section;
			params_section.printf("shell_params_section_%d", i);
			if (pSettings->line_exist(hud_sect, *params_section))
			{
				for (auto& bone_param : m_shell_bones)
				{
					if (bone_param->AmmoType == i)
					{
						bone_param->Load(pSettings->r_string(hud_sect, *params_section), 1);
						result = true;
					}
				}
			}
		}
	}

	if (need_add[0])
	{
		if (pSettings->line_exist(hud_sect, "ammo_params_section") && pSettings->section_exist(pSettings->r_string(hud_sect, "ammo_params_section")))
		{
			SAmmoBonesParams* bone_params = new SAmmoBonesParams(undefined_ammo_type);
			bone_params->Load(pSettings->r_string(hud_sect, "ammo_params_section"), iMagazineSize);
			m_ammo_bones_mag.push_back(bone_params);
			result = true;
		}
		else for (int i = 0; i < m_ammoTypes.size(); i++)
		{
			static shared_str params_section;
			params_section.printf("ammo_params_section_%d", i);
			if (pSettings->line_exist(hud_sect, *params_section))
			{
				SAmmoBonesParams* bone_params = new SAmmoBonesParams(i);
				bone_params->Load(pSettings->r_string(hud_sect, *params_section), iMagazineSize);
				m_ammo_bones_mag.push_back(bone_params);
				result = true;
			}
		}
	}
	else
	{
		if (pSettings->line_exist(hud_sect, "ammo_params_section") && pSettings->section_exist(pSettings->r_string(hud_sect, "ammo_params_section")))
		{
			for (auto& bone_param : m_ammo_bones_mag)
			{
				if (bone_param->AmmoType == undefined_ammo_type)
				{
					bone_param->Load(pSettings->r_string(hud_sect, "ammo_params_section"), iMagazineSize);
					result = true;
				}
			}
		}
		else for (int i = 0; i < m_ammoTypes.size(); i++)
		{
			static shared_str params_section;
			params_section.printf("ammo_params_section_%d", i);
			if (pSettings->line_exist(hud_sect, *params_section))
			{
				for (auto& bone_param : m_ammo_bones_mag)
				{
					if (bone_param->AmmoType == i)
					{
						bone_param->Load(pSettings->r_string(hud_sect, *params_section), iMagazineSize);
						result = true;
					}
				}
			}
		}
	}

	return result;
}

bool CWeapon::install_upgrade_torch_laser(LPCSTR section, bool test)
{
	bool result = false;

	bool value = false;
	bool result2 = process_if_exists_set(section, "torch_installed", value, test);
	if (result2 && !test && value)
	{
		THudLightTorch& LightTorch = CreateComponent<THudLightTorch>();
		LightTorch.NewTorchlight(section);
	}
	result |= result2;

	result2 = process_if_exists_set(section, "laser_installed", value, test);
	if (result2 && !test && value)
	{
		THudLightLaser& LightLaser = CreateComponent<THudLightLaser>();
		LightLaser.NewTorchlight(section);
	}

	result |= result2;

	return result;
}

bool CWeapon::install_upgrade_scope_zoom(LPCSTR section, bool test)
{
	bool result = false;

	float value = 0.0f;
	bool result2 = process_if_exists_set(section, "lens_factor_levels_count", value, test);

	if (result2 && !test && value != 0.0f)
	{
		m_lens_zoom_params.delta = 1.0f / value;
	}
	result |= result2;

	result |= process_if_exists_set(section, "min_lens_factor", m_lens_zoom_params.factor_min, test);
	result |= process_if_exists_set(section, "max_lens_factor", m_lens_zoom_params.factor_max, test);
	result |= process_if_exists_set(section, "lens_speed", m_lens_zoom_params.speed, test);
	result |= process_if_exists_set(section, "lens_gyro_sound_period", m_lens_zoom_params.gyro_period, test);
	result |= process_if_exists_set(section, "lens_factor_levels_count", m_lens_zoom_params.lens_factor_levels_count, test);

	if (result2 && !test)
	{
		m_lens_zoom_params.delta = 1.0f / m_lens_zoom_params.lens_factor_levels_count;
	}
	result |= result2;

	result |= process_if_exists_set(section, "force_zoom_sound", m_lens_zoom_params.force_zoom_sound, test);

	SetLensParams(m_lens_zoom_params);

	stepped_params last = m_lens_night_brightness;

	result2 = process_if_exists_set(section, "max_night_brightness", value, test);
	if (result2 && !test)
	{
		m_lens_night_brightness.max_value = value / 3.0f;
	}
	result |= result2;

	result2 = process_if_exists_set(section, "min_night_brightness", value, test);
	if (result2 && !test)
	{
		m_lens_night_brightness.min_value = value / 3.0f;
	}
	result |= result2;

	result |= process_if_exists_set(section, "steps_brightness", m_lens_night_brightness.steps, test);
	result |= process_if_exists_set(section, "jitter_brightness", m_lens_night_brightness.jitter, test);
	result |= process_if_exists_set(section, "scope_nightvision_min_factor", m_lens_night_brightness.min_factor, test);
	result |= process_if_exists_set(section, "default_brightness_step", m_lens_night_brightness.cur_step, test);

	bool b_r2 = !!psDeviceFlags.test(rsR2);
	b_r2 |= !!psDeviceFlags.test(rsR4);

	if (!b_r2 && m_lens_night_brightness.max_value > 1.0f)
	{
		m_lens_night_brightness.max_value = 1.0f;
	}

	if (abs(m_lens_night_brightness.max_value - last.max_value) > EPS || fabs(m_lens_night_brightness.min_value - last.min_value) > EPS || m_lens_night_brightness.steps != last.steps)
	{
		if (m_lens_night_brightness.lens_night_brightness_saved_step >= 0)
		{
			m_lens_night_brightness.cur_step = m_lens_night_brightness.lens_night_brightness_saved_step;
			m_lens_night_brightness.lens_night_brightness_saved_step = -1;
		}

		SetNightBrightness(m_lens_night_brightness.cur_step, false);
	}

	return result;
}

bool CWeapon::install_upgrade_fast_knife(LPCSTR section, bool test)
{
	bool result = false;
	bool result2 = false;

	u32 int_value = 0;

	result |= process_if_exists_set(section, "kick_material", m_fast_kick_params.material, test);
	result |= process_if_exists_set(section, "kick_ap", m_fast_kick_params.ap, test);
	result |= process_if_exists_set(section, "kick_wallmark_size", m_fast_kick_params.wallmark_size, test);
	result |= process_if_exists_set(section, "kick_hit_count", m_fast_kick_params.cnt, test);
	result |= process_if_exists_set(section, "kick_hit_power", m_fast_kick_params.hp, test);
	result |= process_if_exists_set(section, "kick_hit_impulse", m_fast_kick_params.imp, test);

	result2 = process_if_exists_set(section, "kick_hit_type", int_value, test);
	if (result2 && !test)
	{
		m_fast_kick_params.htype = (ALife::EHitType)int_value;
	}
	result |= result2;

	result |= process_if_exists_set(section, "kick_distance", m_fast_kick_params.hdist, test);
	result |= process_if_exists_set(section, "kick_disp_hor", m_fast_kick_params.disp_hor, test);
	result |= process_if_exists_set(section, "kick_disp_ver", m_fast_kick_params.disp_ver, test);
	result |= process_if_exists_set(section, "disable_kick_anim", m_fast_kick_params.bBlockQK, test);
	result |= process_if_exists_set(section, "disable_kick_anim_when_sil_attached", m_fast_kick_params.bBlockQKSil, test);
	result |= process_if_exists_set(section, "disable_kick_anim_when_scope_attached", m_fast_kick_params.bBlockQKScp, test);
	result |= process_if_exists_set(section, "disable_kick_anim_when_gl_attached", m_fast_kick_params.bBlockQKGL, test);
	result |= process_if_exists_set(section, "disable_kick_anim_when_gl_enabled", m_fast_kick_params.bBlockQKGLM, test);

	return result;
}