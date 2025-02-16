
#include "stdafx.h"
#include "Weapon.h"

bool CWeapon::process_if_exists_deg2rad( LPCSTR section, LPCSTR name, float& value, bool test )
{
	return process_if_exists(section, name, value, test, deg2rad, nullptr);
}

bool CWeapon::install_upgrade_impl( LPCSTR section, bool test )
{
	//inherited::install_upgrade( section );
	bool result = CInventoryItemObject::install_upgrade_impl( section, test );
	
	result |= install_upgrade_ammo_class( section, test );
	result |= install_upgrade_disp      ( section, test );
	result |= install_upgrade_hit       ( section, test );
	result |= install_upgrade_addon     ( section, test );
	return result;
}

bool CWeapon::install_upgrade_ammo_class( LPCSTR section, bool test )
{
	LPCSTR str;

	bool result = process_if_exists(section, "ammo_mag_size", &CInifile::r_s32, maxMagazineSize_, test);

	//	ammo_class = ammo_5.45x39_fmj, ammo_5.45x39_ap  // name of the ltx-section of used ammo
	bool result2 = process_if_exists_set( section, "ammo_class", &CInifile::r_string, str, test );
	if ( result2 && !test )
	{
		m_ammoTypes.clear();
		string128 ammoItem;
		int count = _GetItemCount( str );
		for ( int i = 0; i < count; ++i )
		{
			_GetItem( str, i, ammoItem );
			m_ammoTypes.push_back( ammoItem );
		}
		m_ammoType = 0;
	}
	result |= result2;

	return result;
}

bool CWeapon::install_upgrade_disp( LPCSTR section, bool test )
{
	bool result = process_if_exists( section, "fire_dispersion_condition_factor", &CInifile::r_float, fireDispersionConditionFactor, test );
	result     |= process_if_exists( section, "fire_distance",                    &CInifile::r_float, fireDistance,                  test );

	
	u8 rm = (cam_recoil.camReturnMode) ? 1 : 0;
	result |= process_if_exists_set( section, "cam_return", &CInifile::r_u8, rm, test );
	cam_recoil.camReturnMode = (rm == 1);

	rm = (cam_recoil.camStopReturn) ? 1 : 0;
	result |= process_if_exists_set( section, "cam_return_stop", &CInifile::r_u8, rm, test );
	cam_recoil.camStopReturn = (rm == 1);

	result |= process_if_exists_deg2rad( section, "fire_dispersion_base", fireDispersionBase, test );

	result |= process_if_exists_deg2rad(section, "cam_relax_speed",		  cam_recoil.camRelaxSpeed, test);
	result |= process_if_exists_deg2rad(section, "cam_relax_speed_ai", cam_recoil.camRelaxSpeed_AI, test);
	result |= process_if_exists_deg2rad(section, "cam_dispersion", cam_recoil.camDispersion, test);
	result |= process_if_exists_deg2rad(section, "cam_dispersion_inc", cam_recoil.camDispersionInc, test);
	
	result |= process_if_exists(section, "cam_dispersion_frac", &CInifile::r_float, cam_recoil.camDispersionFrac, test);

	result |= process_if_exists_deg2rad(section, "cam_max_angle", cam_recoil.camMaxAngleVert, test);
	result |= process_if_exists_deg2rad(section, "cam_max_angle_horz", cam_recoil.camMaxAngleHorz, test);
	result |= process_if_exists_deg2rad(section, "cam_step_angle_horz", cam_recoil.camStepAngleHorz, test);

	VERIFY(!fis_zero(cam_recoil.camRelaxSpeed));
	VERIFY(!fis_zero(cam_recoil.camRelaxSpeed_AI));
	VERIFY(!fis_zero(cam_recoil.camMaxAngleVert));
	VERIFY(!fis_zero(cam_recoil.camMaxAngleHorz));

	result |= process_if_exists_deg2rad(section, "zoom_cam_relax_speed", zoom_cam_recoil.camRelaxSpeed, test);// zoom_ ...
	result |= process_if_exists_deg2rad(section, "zoom_cam_relax_speed_ai", zoom_cam_recoil.camRelaxSpeed_AI, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_dispersion", zoom_cam_recoil.camDispersion, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_dispersion_inc", zoom_cam_recoil.camDispersionInc, test);

	result |= process_if_exists(section, "zoom_cam_dispersion_frac", &CInifile::r_float, zoom_cam_recoil.camDispersionFrac, test);

	result |= process_if_exists_deg2rad(section, "zoom_cam_max_angle", zoom_cam_recoil.camMaxAngleVert, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_max_angle_horz", zoom_cam_recoil.camMaxAngleHorz, test);
	result |= process_if_exists_deg2rad(section, "zoom_cam_step_angle_horz", zoom_cam_recoil.camStepAngleHorz, test);

	VERIFY(!fis_zero(zoom_cam_recoil.camRelaxSpeed));
	VERIFY(!fis_zero(zoom_cam_recoil.camRelaxSpeed_AI));
	VERIFY(!fis_zero(zoom_cam_recoil.camMaxAngleVert));
	VERIFY(!fis_zero(zoom_cam_recoil.camMaxAngleHorz));

	result |= process_if_exists( section, "PDM_disp_base",          &CInifile::r_float, m_pdm.m_fPDM_disp_base,          test );
	result |= process_if_exists( section, "PDM_disp_vel_factor",    &CInifile::r_float, m_pdm.m_fPDM_disp_vel_factor,    test );
	result |= process_if_exists( section, "PDM_disp_accel_factor",  &CInifile::r_float, m_pdm.m_fPDM_disp_accel_factor,  test );
	result |= process_if_exists( section, "PDM_disp_crouch",        &CInifile::r_float, m_pdm.m_fPDM_disp_crouch,        test );
	result |= process_if_exists( section, "PDM_disp_crouch_no_acc", &CInifile::r_float, m_pdm.m_fPDM_disp_crouch_no_acc, test );

	result |= process_if_exists( section, "zoom_inertion_factor", &CInifile::r_float, m_fZoomInertCoef, test );

//	result |= process_if_exists( section, "misfire_probability", &CInifile::r_float, misfireProbability,       test );
//	result |= process_if_exists( section, "misfire_condition_k", &CInifile::r_float, misfireConditionK,        test );
	result |= process_if_exists( section, "condition_shot_dec",			&CInifile::r_float, conditionDecreasePerShot,		test );
	result |= process_if_exists( section, "misfire_start_condition",	&CInifile::r_float, misfireStartCondition,			test );
	result |= process_if_exists( section, "misfire_end_condition",		&CInifile::r_float, misfireEndCondition,			test );
	result |= process_if_exists( section, "misfire_start_prob",			&CInifile::r_float, misfireStartProbability,		test );
	result |= process_if_exists( section, "misfire_end_prob",			&CInifile::r_float, misfireEndProbability,			test );

	BOOL value = m_zoom_params.m_bZoomEnabled;
	bool result2 = process_if_exists_set( section, "zoom_enabled", &CInifile::r_bool, value, test );
	if ( result2 && !test )
	{
		m_zoom_params.m_bZoomEnabled = !!value;
	}
	result |= result2;
	
	return result;
}

bool CWeapon::ProcessRpmUpgrade( LPCSTR section, LPCSTR paramName, float &timeToFireProperty, bool test)
{
	bool result2;
	float rpm = 60.0f / timeToFireProperty;
	result2 = process_if_exists( section, paramName, &CInifile::r_float, rpm, test );
	if ( result2 && !test )
	{
		VERIFY( rpm > 0.0f );
		timeToFireProperty = 60.0f / rpm;
	}
	return result2;
}

bool CWeapon::install_upgrade_hit( LPCSTR section, bool test )
{
	bool result = false;

	shared_str	s_sHitPower;
	bool result2 = process_if_exists_set( section, "hit_power", &CInifile::r_string_wb, s_sHitPower, test );
	if ( result2 && !test )
	{
		string32 buffer;
		fvHitPower[egdMaster] = (float)atof( _GetItem( *s_sHitPower, 0, buffer ) );
		fvHitPower[egdNovice] = fvHitPower[egdStalker] = fvHitPower[egdVeteran] = fvHitPower[egdMaster];

		int num_game_diff_param = _GetItemCount( *s_sHitPower );
		if ( num_game_diff_param > 1 ) { fvHitPower[egdVeteran]	= (float)atof( _GetItem( *s_sHitPower, 1, buffer ) ); }
		if ( num_game_diff_param > 2 ) { fvHitPower[egdStalker]	= (float)atof( _GetItem( *s_sHitPower, 2, buffer ) ); }
		if ( num_game_diff_param > 3 ) { fvHitPower[egdNovice]	= (float)atof( _GetItem( *s_sHitPower, 3, buffer ) ); }
	}
	result |= result2;

	shared_str	s_sHitPowerCritical;
	result2 = process_if_exists_set( section, "hit_power_critical", &CInifile::r_string_wb, s_sHitPower, test );
	if ( result2 && !test )
	{
		string32 buffer;
		fvHitPowerCritical[egdMaster] = (float)atof(_GetItem(*s_sHitPowerCritical,0,buffer));
		fvHitPowerCritical[egdNovice] = fvHitPowerCritical[egdStalker] = fvHitPowerCritical[egdVeteran] = fvHitPowerCritical[egdMaster];

		int num_game_diff_param = _GetItemCount(*s_sHitPowerCritical);
		if ( num_game_diff_param > 1 ) { fvHitPowerCritical[egdVeteran]	= (float)atof(_GetItem(*s_sHitPowerCritical,1,buffer)); }
		if ( num_game_diff_param > 2 ) { fvHitPowerCritical[egdStalker]	= (float)atof(_GetItem(*s_sHitPowerCritical,2,buffer)); }
		if ( num_game_diff_param > 3 ) { fvHitPowerCritical[egdNovice]	= (float)atof(_GetItem(*s_sHitPowerCritical,3,buffer)); }
	}
	result |= result2;

	result |= process_if_exists( section, "hit_impulse",  &CInifile::r_float, fHitImpulse,         test );
	result |= process_if_exists( section, "bullet_speed", &CInifile::r_float, m_fStartBulletSpeed, test );
	result |= process_if_exists( section, "air_resistance_factor", &CInifile::r_float, m_air_resistance_factor, test );

	/*	
	silencer_hit_power           = 0.55, 0.55, 0.55, 0.55
	silencer_hit_impulse         = 120
	silencer_fire_distance       = 600
	silencer_bullet_speed        = 310
	*/

	result |= process_if_exists_set( section, "use_aim_bullet",  &CInifile::r_bool, m_bUseAimBullet, test );
	if ( m_bUseAimBullet ) // first super bullet
	{
		result |= process_if_exists( section, "time_to_aim",  &CInifile::r_float, m_fTimeToAim, test );
	}

//	LPCSTR weapon_section = cNameSect().c_str(); 
	//pSettings->r_float( weapon_section, "rpm" ); // fOneShotTime * 60.0f;
	
	result |= ProcessRpmUpgrade( section, "rpm", fTimeToFire, test );

	return result;
}


bool CWeapon::install_upgrade_addon( LPCSTR section, bool test )
{
	bool result = false;
	bool result2;
	//LPCSTR weapon_section = cNameSect().c_str(); 

	// 0 - no addon // 1 - permanent // 2 - attachable
	int temp_int;
	
	result |= result2 = process_if_exists_set(section, "scope_force_icon", &CInifile::r_bool, temp_int, test);
	if (result2 && !test) m_bScopeForceIcon = !!temp_int;
	result |= result2 = process_if_exists_set(section, "silencer_force_icon", &CInifile::r_bool, temp_int, test);
	if (result2 && !test) m_bSilencerForceIcon = !!temp_int;
	result |= result2 = process_if_exists_set(section, "grenade_launcher_force_icon", &CInifile::r_bool, temp_int, test);
	if (result2 && !test) m_bGrenadeLauncherForceIcon = !!temp_int;

	result |= result2 = process_if_exists_set( section, "scope_status", &CInifile::r_s32, temp_int, test );
	if (result2 && !test) m_eScopeStatus = (ALife::EWeaponAddonStatus)temp_int;

	result |= process_if_exists( section, "holder_range_modifier", &CInifile::r_float, m_addon_holder_range_modifier, test );
	result |= process_if_exists( section, "holder_fov_modifier",   &CInifile::r_float, m_addon_holder_fov_modifier,   test );

	if ( m_eScopeStatus == ALife::eAddonAttachable || m_bScopeForceIcon )
	{
		if (pSettings->line_exist(section, "scopes_sect"))		
		{
			LPCSTR str = pSettings->r_string(section, "scopes_sect");
			for (int i = 0, count = _GetItemCount(str); i < count; ++i )	
			{
				string128						scope_section;
				_GetItem						(str, i, scope_section);
				m_scopes.push_back				(scope_section);
			}
		}
		else if (pSettings->line_exist(section, "scope_name"))
		{
			m_scopes.push_back(section);
			// This allows to change "forced scope icon" via upgrade
			if (m_eScopeStatus != ALife::eAddonAttachable)
			{
				m_cur_scope = u8(m_scopes.size() - 1);
			}
		}
	}

	result |= process_if_exists_set( section, "scope_dynamic_zoom", &CInifile::r_bool, m_zoom_params.m_bUseDynamicZoom, test );
	result |= process_if_exists_set( section, "scope_nightvision", &CInifile::r_string_wb, m_zoom_params.m_sUseZoomPostprocess, test );
	result |= process_if_exists_set( section, "scope_alive_detector", &CInifile::r_string_wb, m_zoom_params.m_sUseBinocularVision, test );

	result |= result2 = process_if_exists_set( section, "silencer_status", &CInifile::r_s32, temp_int, test );
	if (result2 && !test) m_eSilencerStatus = (ALife::EWeaponAddonStatus)temp_int;

	result |= process_if_exists_set( section, "silencer_name", &CInifile::r_string, m_sSilencerName, test );
	result |= process_if_exists_set( section, "silencer_x", &CInifile::r_s32, m_iSilencerX, test );
	result |= process_if_exists_set( section, "silencer_y", &CInifile::r_s32, m_iSilencerY, test );

	result |= result2 = process_if_exists_set( section, "grenade_launcher_status", &CInifile::r_s32, temp_int, test );
	if (result2 && !test) m_eGrenadeLauncherStatus = (ALife::EWeaponAddonStatus)temp_int;

	result |= process_if_exists_set( section, "grenade_launcher_name", &CInifile::r_string, m_sGrenadeLauncherName, test );
	result |= process_if_exists_set( section, "grenade_launcher_x", &CInifile::r_s32, m_iGrenadeLauncherX, test );
	result |= process_if_exists_set( section, "grenade_launcher_y", &CInifile::r_s32, m_iGrenadeLauncherY, test );

	return result;
}
