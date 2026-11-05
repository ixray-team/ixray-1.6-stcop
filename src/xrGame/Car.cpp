#include "stdafx.h"
#include "pch_script.h"
#include "ParticlesObject.h"
#include "../xrPhysics/ODE/Physics.h"

#ifdef DEBUG
#	include "../xrEngine/StatGraph.h"
#	include "PHDebug.h"
#endif // DEBUG

#include "ai_space.h"
#include "alife_simulator.h"
#include "script_engine.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "damage_manager.h"
#include "AnomalyZone.h"
#include "HUDManager.h"
#include "cameralook.h"
#include "CameraFirstEye.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "math.h"
#include "script_entity_action.h"
#include "Inventory.h"
#include "xrServer_Objects_ALife_Items.h"
#include "Level.h"
#include "ui/UIMainIngameWnd.h"
#include "CarWeapon.h"
#include "game_object_space.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrPhysics/ODE/PHActivationShape.h"
#include "CharacterPhysicsSupport.h"
#include "car_memory.h"

#include "UIGameSP.h"
#include "GamePersistent.h"
#include "actor_anim_defs.h"
#include "../Include/xrRender/particles_systems_library_interface.hpp"
#include "../xrPhysics/ODE/PHElement.h"
#include "../xrPhysics/ODE/PHShell.h"
#include "ExplosiveRocket.h"
#include "../xrEngine/xr_input.h"
#include "../xrEngine/EnnumerateVertices.h"
collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);

namespace
{
	constexpr const char* DefaultExhaustParticles = "vehiclefx\\exhaust_1";

	bool ParticleEffectExists(const char* Name)
	{
		if (Name == nullptr || Name[0] == '\0')
		{
			return false;
		}

		if (g_dedicated_server)
		{
			return true;
		}

		if (g_pGamePersistent == nullptr || g_pGamePersistent->pEnvironment == nullptr
			|| !g_pGamePersistent->pEnvironment->m_pRender)
		{
			return false;
		}

		const xr_vector<shared_str>& Effects = g_pGamePersistent->pEnvironment->m_pRender->particles_systems_library().vec_all_particles();
		for (const shared_str& Effect : Effects)
		{
			if (xr_strcmp(Effect, Name) == 0)
			{
				return true;
			}
		}

		return false;
	}
}

CCar::CCar()
{
	bone_map = BONE_P_MAP();
	m_memory		= NULL;
	m_driver_anim_type = 0;
	m_bone_trunk	= BI_NONE;
	active_camera	= NULL;

	saved_camera = EctFirstCam;

	camera[EctFirstCam] = new CCameraFirstEye(this, CCameraBase::flRelativeLink|CCameraBase::flPositionRigid);
	camera[EctFirstCam]->tag = EctFirstCam;
	camera[EctFirstCam]->Load("car_firsteye_cam");

	camera[EctSecondCam] = new CCameraLook(this);
	camera[EctSecondCam]->tag = EctSecondCam;
	camera[EctSecondCam]->Load("car_look_cam");

	camera[EctThridCam]	= new CCameraLook(this);
	camera[EctThridCam]->tag = EctThridCam;
	camera[EctThridCam]->Load("car_free_cam");

	camera[EctFourthCam] = new CCameraLook(this);
	camera[EctFourthCam]->tag = EctFourthCam;
	camera[EctFourthCam]->Load("car_free_cam");

	camera[EctFiveCam] = new CCameraFirstEye(this);
	camera[EctFiveCam]->tag = EctFiveCam;
	camera[EctFiveCam]->Load("car_free_cam");

	OnCameraChange(EctFirstCam);

	m_repairing		=false;

	///////////////////////////////
	//////////////////////////////
	/////////////////////////////
	b_wheels_limited=false;
	b_engine_on=false;
	e_state_steer=idle;
	e_state_drive=neutral;
	m_current_gear_ratio=dInfinity;
	rsp=false;lsp=false;fwp=false;bkp=false;brp=false;fwpbkp=false;
	///////////////////////////////
	//////////////////////////////
	/////////////////////////////
	m_exhaust_particles	= DefaultExhaustParticles;
	m_nitro_particles = "";
	m_pop_particles = "";
	m_car_sound			= new SCarSound(this);

	CarCameraDirection.set(0.f,0.f,0.f);

	inventory().SetSlotsUseful(false);
	m_doors_torque_factor = 2.f;
	m_power_increment_factor = 0.5f;
	m_power_decrement_factor = 0.5f;
	m_rpm_increment_factor = 0.5f;
	m_rpm_decrement_factor = 0.5f;
	m_rpm_brp_increment_factor = 1.f;
	m_rpm_brp_decrement_factor = 1.f;
	b_breaks = false;
	m_break_start = 0.f;
	m_handbreak_start = 0.f;
	m_break_time = 1.f;
	m_slip_start_time = 0.f;
	m_hit_engine_time = 0;

	b_exploded = false;
	b_awd = true;
	m_power_neutral_factor = 0.25f;
	m_steer_angle = 0.f;
	m_wheel_angle = 0.f;
	m_current_rpm = 0.f;
	m_current_rpm_brp_n = 1.f;
	m_current_engine_power = 0.f;
	m_camera_inertion_n = 0.9f;
	m_camera_inertion_k = 0.9f;
	m_camera_angle_factor = 1.f;
	break_start_dt = 0.0f;
	m_current_slip_sliding = 0.0f;
	m_vel_sliding_threshold = 5.0f;
	m_slip_sliding_threshold = 0.6f;
	m_camera_inert_x = 0.f;
	m_camera_inert_z = 0.f;
	m_steer_boost = 0.f;
	m_steer_boost_frame = 0;
	m_steer_turn_factor = 1.f;
	m_steer_counter_turn_factor = 0.5f;
	m_steer_turn_compensation = 0.5f;
	m_steer_turn_compensation_max = 0.3f;
	m_steer_turn_delta = 0.2f;

	m_transmission_switch_rpm = 0.f;
	m_transmission_stage = ETransmissionStage::Drive;
	m_transmission_switch_delay = 0.4f;
	m_transmission_switch_power_factor = 0.5f;
	m_transmission_switch_volume = 1.f;
	b_transmission_switch_drive = false;
	m_transmission_sound_played = true;
	m_sound_state_after_switch = 0.f;

	m_limiter_volume = 0.f;
	m_transmission_switch_time = 0.f;
	m_transmission_switch_sound_time = 0.f;
	m_engine_valve = 0;
	m_engine_valve_switch = true;
	m_turbo_scale = 0.f;
	m_turbo_sound_scale = 1.f;
	m_turbo_volume_scale = 1.f;
	m_turbo_scale_min = 0.2f;
	m_turbo_scale_max = 1.f;
	m_turbo_scale_speed = 1.5f;
	m_blower_rpm_scale = 0.9f;
	m_blower_scale_min = 0.3f;
	m_blower_volume_min = 0.3f;
	m_blower_sound_forward = 0.5f;
	m_blower_rpm_current = 0.f;
	m_blower_inertia = 0.1f;
	m_engine_idle_volume = 0.8f;
	m_engine_idle_volume_scale = 1.f;
	m_engine_rpm_scale = 0.f;
	m_idle_state_scale = 0.f;
	m_idle_state_scale_last = 0.f;
	m_engine_state_scale = 0.f;
	m_engine_state_scale_last = 0.f;
	m_engine_state_scale_min = 0.8f;
	m_engine_sound_scale = 1.0f;
	m_engine_sound_scale_absolute = 1.0f;
	m_decel_sound_boost = 1.f;
	m_heavy_sound_boost = 1.f;
	m_engine_sound_scale_max = 1.5f;
	m_engine_sound_scale_min = 0.5f;
	m_engine_sound_scale_factor = 1.0f;
	m_engine_sound_stop_dt = 0.f;
	m_engine_decel_sound_stop_dt = 0.f;
	m_engine_volume_scale = 1.0f;
	m_sound_state_change_up = 3.f;
	m_sound_state_change_down = 2.5f;
	m_sound_idle_change_up = 25.f;
	m_sound_idle_change_down = 0.6f;
	m_sound_state_switch_factor = 3.f;
	m_sound_effect = 0.f;
	m_sound_switch_effect_type = 0;
	m_sound_switch_effect_power = 1.f;
	m_sound_switch_effect_speed = 1.f;
	m_sound_switch_effect_duration = 20.f;
	m_engine_decel_volume_scale = 0.f;
	m_power_reverse_factor = 0.f;
	m_sound = false;
	m_gas_volume_scale = 0.f;
	m_engine_drive = 0.f;
	m_engine_drive_noswitch = 0.f;
	m_engine_drive_time = 0.f;
	m_engine_heavy = 0.f;
	m_engine_heavy_constant = 0.2f;
	m_engine_heavy_rpm = 0.2f;
	m_engine_heavy_switch = 0.f;
	m_valve_volume_scale = 0.f;
	m_valve_switch_volume = 0.8f;
	m_valve_time = 0.f;
	m_sound_rpm = 0.0f;
	m_wheels_spin_factor_start = 0.75f;
	m_wheels_spin_factor_stop = 0.8f;
	b_auto_switch_transmission = false;
	b_auto_transmission_stop = false;
	b_cvt_transmission = false;
	mb_traction = false;
	m_dwStartTime = 0;
	m_dwAttachTime = Device.dwTimeGlobal;
	m_friction_slip_factor = 1.f;
	m_friction2_slip_factor = 1.f;
	m_friction_spin_factor = 1.f;
	m_friction2_spin_factor = 1.f;
	m_friction_slip_speed = 0.005f;
	m_friction2_steering_wheels = 1.f;
	m_friction2_nonsteering_wheels = 1.f;
	m_friction2_brp_boost = 1.f;
	b_drift_boost = false;
	m_drift_start_boost = 0.f;
	m_drift_boost_time = 0.25f;
	m_steer_speed_boost = 0.f;
	m_steer_spin_boost = 0.f;
	m_brake_boost = 0.f;
	m_switch_shake_boost = 0.f;
	m_engine_shake_boost = 0.f;
	m_shake_stage = false;
	m_current_rpm_brp_n_max = 2.f;
	m_current_rpm_brp_decrease = 1.f;
	m_wheels_sound_delay = 0.1f;
	m_wheels_sound_timer = 0.f;
	m_nitro = false;
	m_nitro_power_factor = 2.f;
	m_nitro_fuel_factor = 2.f;
	m_nitro_friction_factor = 1.f;
	m_nitro_wheelie_boost = 0.f;
	m_wheelie_boost = 0.f;
	m_nitro_wheelie_friction_brp_factor = 1.f;
	m_nitro_switch_boost = 0.7f;
	m_nitro_particle_scale = 1.f;
	b_nitro_particles_enabled = false;

	m_speedometer_factor = 1.f;
	m_speedometer_max_speed = flt_max;
	m_speedometer_min_speed = 0.f;
	m_speedometer_current = 0.f;
	m_speedometer_offset = 0.f;
	m_tachometer_factor = 1.f;
	m_tachometer_current = 0.f;
	m_tachometer_min_rpm = 0.f;
	m_fuel_current = 0.f;
	m_fuel_angle = 0.f;
	m_fuel_offset = 0.f;
	m_temp_current = 0.f;
	m_temp_angle = 0.f;
	m_temp_offset = 0.f;
	m_temp_offset_current = 0.f;
	m_temperature = 0.f;
	m_gauge_angle = 0.f;
	m_gauge_current = 0.f;
	m_fan_speed = 1.f;
	m_fan_rotation = 0.f;
	m_fan_speed_current = 0.f;
	m_fan_engine = false;
	m_pressure_max_angle = 0.f;
	m_pressure_idle_angle = 0.f;
	m_pressure_factor = 1.f;
	m_pressure_current = 0.f;
	m_pressure_offset = 0.f;
	m_pressure = 0.f;
	m_blower_angle = 0.f;
	m_blower_current = 0.f;
	m_engine_angle = 0.f;
	m_engine_current = 0.f;
	m_engine_stage = false;
	m_engine_speed = 0.f;
	m_clock_minutes_offset = 0.f;
	m_clock_seconds_offset = 0.f;
	m_clock_hours_offset = 0.f;
	m_clock_seconds_real = true;
	m_nitro_engage = false;
	b_nitro_engage = false;
	m_nitro_engage_time = 0.f;
	m_whine_speed = 50.f;
	m_whine_reverse = true;
	m_whine_sound_scale = 0.f;
	m_whine_exterior_volume = 0.9f;
	m_pop = false;
	m_pop_time = 0.f;
	m_pop_probability = 0.5f;
	m_pop_single = false;
	m_pop_played = false;
	m_steer_start_time = 0.f;
	m_break_abs = false;
	m_break_abs_power = 1.f;
	m_break_abs_individual = true;
	m_break_abs_threshold = 0.85f;
	m_enhanced_sound = true;
	b_burnout = false;
	m_steer_nonlinear = true;
	m_steer_invert = false;
	m_steer_threshold = 0.0002f;
	m_last_velocity = 0.f;
	m_last_avelocity_j = 0.f;
	m_last_avelocity = 0.f;
	m_acceleration = 0.f;
	m_camera_acceleration = 1.f;
	b_diff_lock = true;
	m_diff_lock_power = 1.f;
	m_diff_lock_overpower = 0.f;
	m_firsteye_panel = false;
	m_allow_inventory_control = false;
	m_interior_volume = 0.7f;
	m_hit_stall_threshold = 1.f;
	m_hit_anomaly_scale = 1.f;
	m_brakes_pressure_factor = 0.f;
	m_brakes_pressure_max_angle = 0.f;
	m_brakes_pressure_current = 0.f;
	m_brakes_pressure = 0.f;
	m_brakes_pressure_apply_angle = 0.f;
	m_brakes_pressure_apply_current = 0.f;
	m_battery_factor = 0.f;
	m_battery_angle = 0.f;
	m_battery_current = 0.f; 
	m_gear_park_angle_x = 0.f;
	m_gear_drive_angle_x = -15.f;
	m_gear_reverse_angle_x = -5.f;
	m_gear_current_angle_x = 0.f;
	m_gear_current_angle_z = 0.f;
	m_gear_speed = 1.f;
	m_gear_manual = false;
	m_gear_left_angle = 10.f;
	m_gear_middle_angle = 0.f;
	m_gear_right_angle = -10.f;
	m_car_weapon_slots = 99;
	m_downforce = 0.f;
	m_current_downforce = 0.f;
	b_start_open_recursive = false;
	b_start_close_recursive = false;
	b_all_recurs_opened = false;
	m_torch_usage = false;
	m_camera_last_mmove_time = Device.dwTimeGlobal + 3000;
	characters_scraper_shell = NULL;
	b_event_exit_car = false;
	m_exit_car_time = 0;
	m_exit_door_bone = BI_NONE;
	b_event_drag_activated = false;
#ifdef DEBUG
	InitDebug();
#endif
}

CCar::~CCar(void)
{
	for (int i=0; i<EctMaxCam; ++i)
		xr_delete(camera[i]);

	xr_delete			(m_car_sound);

	ClearExhausts		();

	ClearCarWeapons		();

	xr_delete			(m_memory);
}

void CCar::ClearCarWeapons()
{
	for (CCarTurret* Turret : m_car_weapons)
		xr_delete(Turret);

	m_car_weapons.clear();
}

void CCar::reinit		()
{
	CEntity::reinit			();
	CScriptEntity::reinit	();
	CInventoryOwner::reinit ();
	if(m_memory)
		m_memory->reinit	();
}

void CCar::reload		(LPCSTR section)
{
	CEntity::reload			(section);
	CInventoryOwner::reload (section);
	if(m_memory)
		m_memory->reload	(section);
	// fuel load
	m_fuel_tank = READ_IF_EXISTS(pSettings, r_float, section, "fuel_tank", 10.f);
	m_fuel_consumption = READ_IF_EXISTS(pSettings, r_float, section, "fuel_consumption", 0.0005f);
	m_fuel_speed_factor = READ_IF_EXISTS(pSettings, r_float, section, "fuel_speed_factor", 2.f);
	m_fuel_speed_optimal = READ_IF_EXISTS(pSettings, r_float, section, "fuel_speed_optimal", 50.f);
	clamp(m_fuel_speed_optimal, 1.f, m_fuel_speed_optimal);
	m_fuel_rpm_factor = READ_IF_EXISTS(pSettings, r_float, section, "fuel_rpm_factor", 1.f);
	m_fuel_gas_factor = READ_IF_EXISTS(pSettings, r_float, section, "fuel_gas_factor", 1.f);
	inventory().SetMaxWeight(READ_IF_EXISTS(pSettings, r_float, section, "trunk_capacity", 1000.f));
}

bool CCar::IsMyCar() const
{
	if (!Actor())
		return false;

	return Actor()->Holder() == this;
}

void CCar::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	save_data(m_fuel, output_packet);
}

void CCar::load(IReader& input_packet)
{
	inherited::load(input_packet);
	load_data(m_fuel, input_packet);
}

void CCar::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object, "CCar::Serialize")
	{
		inherited::Serialize(Object);
		Object << m_fuel;
	}
}

void CCar::cb_Steer			(CBoneInstance* B)
{
	CCar*	C			= static_cast<CCar*>(B->callback_param());
	Fmatrix m;

	m.rotateZ(C->m_steer_angle);

	B->mTransform.mulB_43	(m);

}

float rs_car_steer_factor = 1.f;
bool rs_car_god = false;
void CCar::cb_Steer_factor			(CBoneInstance* B)
{
	VERIFY2(fsimilar(DET(B->mTransform),1.f,DET_CHECK_EPS),"Bones receive returns 0 matrix");
	CCar*	C			= static_cast<CCar*>(B->callback_param());
	Fmatrix m;

	m.rotateZ(C->m_steer_angle * C->m_steer_factor * rs_car_steer_factor);

	B->mTransform.mulB_43	(m);
#ifdef DEBUG
	if( !fsimilar(DET(B->mTransform),1.f,DET_CHECK_EPS) ){
	
		Msg("RotatingZ angle=%f", C->m_steer_angle);
		VERIFY2(0,"Bones callback returns BAD!!! matrix");
	}
#endif
}

void CCar::cb_Gauge(CBoneInstance* B)
{
	Gauge_struct* C = static_cast<Gauge_struct*>(B->callback_param());
	if (C == nullptr || C->pcar == nullptr)
		return;

	Fmatrix m;
	bool ApplyTransform = false;
	switch (C->type)
	{
		case gauge_speedometer: ApplyTransform = C->pcar->UpdateSpeedometerGauge(m); break;
		case gauge_tachometer: ApplyTransform = C->pcar->UpdateTachometerGauge(m); break;
		case gauge_fuel: ApplyTransform = C->pcar->UpdateFuelGauge(m); break;
		case gauge_temp: ApplyTransform = C->pcar->UpdateTemperatureGauge(m); break;
		case gauge_battery: ApplyTransform = C->pcar->UpdateBatteryGauge(m); break;
		case gauge_gauge: ApplyTransform = C->pcar->UpdateGenericGauge(m); break;
		case gauge_fan: ApplyTransform = C->pcar->UpdateFanGauge(m); break;
		case gauge_blower: ApplyTransform = C->pcar->UpdateBlowerGauge(m); break;
		case gauge_pressure: ApplyTransform = C->pcar->UpdatePressureGauge(m); break;
		case gauge_brakes_pressure: ApplyTransform = C->pcar->UpdateBrakesPressureGauge(m); break;
		case gauge_brakes_pressure_apply: ApplyTransform = C->pcar->UpdateBrakesPressureApplyGauge(m); break;
		case gauge_engine: ApplyTransform = C->pcar->UpdateEngineGauge(m); break;
		case gauge_gear: ApplyTransform = C->pcar->UpdateGearGauge(B, m); break;
		case clock_seconds: ApplyTransform = C->pcar->UpdateClockSecondsGauge(m); break;
		case clock_minutes: ApplyTransform = C->pcar->UpdateClockMinutesGauge(m); break;
		case clock_hours: ApplyTransform = C->pcar->UpdateClockHoursGauge(m); break;
	}

	if (ApplyTransform)
		B->mTransform.mulB_43(m);
}

bool CCar::UpdateSpeedometerGauge(Fmatrix& m)
{
	if (m_driving_wheels.empty()) return false;

	float avg_speed = 0.f;
	for (auto& WheelDrive : m_driving_wheels)
	{
		avg_speed += _abs(WheelDrive.ASpeed()) * WheelDrive.pwheel->radius;
	}
	avg_speed = -m_speedometer_min_speed * fabsf(m_speedometer_factor) + (avg_speed * 3.6f * fabsf(m_speedometer_factor)) / m_driving_wheels.size();
	clamp(avg_speed, 0.f, m_speedometer_max_speed * fabsf(m_speedometer_factor));

	m_speedometer_current = (1.f - 6.f * Device.fTimeDelta) * m_speedometer_current + 6.f * Device.fTimeDelta * avg_speed;

	if (m_speedometer_factor < 0.f)
		m.rotateZ(deg2rad(m_speedometer_offset) + m_speedometer_current / 30.f);
	else
		m.rotateZ(-deg2rad(m_speedometer_offset) - m_speedometer_current / 30.f);

	return true;
}

bool CCar::UpdateTachometerGauge(Fmatrix& m)
{
	float cur_rpm = m_current_rpm - (m_tachometer_min_rpm * 0.105f);
	clamp(cur_rpm, 0.f, cur_rpm);
	float rpm = (cur_rpm / m_max_rpm) * 3.f * fabsf(m_tachometer_factor);
	if (!b_engine_on)
		rpm = 0.f;

	m_tachometer_current = (1.f - 15.f * Device.fTimeDelta) * m_tachometer_current + 15.f * Device.fTimeDelta * rpm;

	if (m_tachometer_factor < 0.f)
		m.rotateZ(m_tachometer_current);
	else
		m.rotateZ(-m_tachometer_current);

	return true;
}

bool CCar::UpdateFuelGauge(Fmatrix& m)
{
	float fuel = (m_fuel / m_fuel_tank) + deg2rad(m_fuel_offset);
	if (!b_engine_on)
		fuel = 0.f;

	if (m_fuel_current < fuel)
		m_fuel_current = (1.f - 0.9f * Device.fTimeDelta) * m_fuel_current + 0.9f * Device.fTimeDelta * fuel;
	else
		m_fuel_current = (1.f - 3.f * Device.fTimeDelta) * m_fuel_current + 3.f * Device.fTimeDelta * fuel;

	m.rotateZ(-m_fuel_current * deg2rad(m_fuel_angle));

	return true;
}

bool CCar::UpdateTemperatureGauge(Fmatrix& m)
{
	float temp = 0.f;
	float temp_speed = 1.f;
	if (b_engine_on)
	{
		temp = 1.f;
		if (b_engine_on && e_state_drive == drive)
		{
			temp = 1.2f;
			temp_speed = 1.2f;
		}
		float speed = GetSpeedKmH();
		clamp(speed, 0.f, 80.f);
		temp = temp - (speed / 260.f);

		m_temp_offset_current = (1.f - 0.9f * Device.fTimeDelta) * m_temp_offset_current + 0.9f * Device.fTimeDelta * deg2rad(m_temp_offset);
		m_temp_current = (1.f - 0.9f * Device.fTimeDelta) * m_temp_current + 0.9f * Device.fTimeDelta;
	}
	else
	{
		m_temp_offset_current = (1.f - 3.f * Device.fTimeDelta) * m_temp_offset_current;
		m_temp_current = (1.f - 3.f * Device.fTimeDelta) * m_temp_current;
	}

	m_temperature = (1.f - 0.006f * temp_speed * Device.fTimeDelta) * m_temperature + 0.006f * temp_speed * Device.fTimeDelta * deg2rad(m_temp_angle) * temp;

	m.rotateZ(-m_temperature * m_temp_current - m_temp_offset_current);

	return true;
}

bool CCar::UpdateBatteryGauge(Fmatrix& m)
{
	float rpm = (1.f + (m_current_rpm - m_min_rpm) / (m_max_rpm - m_min_rpm) * m_battery_factor);
	float angle = fabsf(m_battery_angle) * rpm;
	if (!b_engine_on)
		angle = 0.f;

	if (m_battery_current < angle)
		m_battery_current = (1.f - 0.9f * Device.fTimeDelta) * m_battery_current + 0.9f * Device.fTimeDelta * deg2rad(angle);
	else
		m_battery_current = (1.f - 3.f * Device.fTimeDelta) * m_battery_current + 3.f * Device.fTimeDelta * deg2rad(angle);

	if (m_battery_angle < 0.f)
		m.rotateZ(m_battery_current);
	else
		m.rotateZ(-m_battery_current);

	return true;
}

bool CCar::UpdateGenericGauge(Fmatrix& m)
{
	float angle = fabsf(m_gauge_angle);
	if (!b_engine_on)
		angle = 0.f;

	if (m_gauge_current < angle)
		m_gauge_current = (1.f - 0.9f * Device.fTimeDelta) * m_gauge_current + 0.9f * Device.fTimeDelta * deg2rad(angle);
	else
		m_gauge_current = (1.f - 3.f * Device.fTimeDelta) * m_gauge_current + 3.f * Device.fTimeDelta * deg2rad(angle);

	if (m_gauge_angle < 0.f)
		m.rotateZ(m_gauge_current);
	else
		m.rotateZ(-m_gauge_current);

	return true;
}

bool CCar::UpdateFanGauge(Fmatrix& m)
{
	float rpm = m_current_rpm;
	clamp(rpm, m_min_rpm, rpm);

	if (b_engine_on)
		m_fan_speed_current += 0.6f * fabsf(m_fan_speed) * Device.fTimeDelta;
	else
		m_fan_speed_current -= 0.6f * fabsf(m_fan_speed) * Device.fTimeDelta;
	clamp(m_fan_speed_current, 0.f, fabsf(m_fan_speed));

	if (!m_fan_engine)
		m_fan_rotation = m_fan_rotation + m_fan_speed_current * 3.f * Device.fTimeDelta;
	else
		m_fan_rotation = m_fan_rotation + m_fan_speed_current * 3.f * Device.fTimeDelta * (rpm / m_max_rpm);

	if (m_fan_speed < 0.f)
		m.rotateZ(-m_fan_rotation);
	else
		m.rotateZ(m_fan_rotation);

	return true;
}

bool CCar::UpdateBlowerGauge(Fmatrix& m)
{
	float angle = fabsf(m_blower_angle);
	if (e_state_drive != drive)
	{
		angle = 0.f;
	}
	else
		if (IsTransmissionSwitching())
			angle = fabsf(m_blower_angle) * 0.3f;

	float speed = 0.3f;
	if (IsTransmissionSwitching())
		speed *= 0.5f;

	if (m_blower_current < angle)
		m_blower_current = (1.f - speed * Device.fTimeDelta * 30.f) * m_blower_current + speed * Device.fTimeDelta * 30.f * deg2rad(angle);
	else
		m_blower_current = (1.f - 1.5f * speed * Device.fTimeDelta * 30.f) * m_blower_current + 1.5f * speed * Device.fTimeDelta * 30.f * deg2rad(angle);

	if (m_blower_angle < 0.f)
		m.rotateX(m_blower_current);
	else
		m.rotateX(-m_blower_current);

	return true;
}

bool CCar::UpdatePressureGauge(Fmatrix& m)
{
	float rpm = m_current_rpm - m_min_rpm;
	clamp(rpm, 0.f, m_max_rpm - m_min_rpm);
	rpm = (rpm / (m_max_rpm - m_min_rpm)) * fabsf(m_pressure_factor) + deg2rad(m_pressure_idle_angle + m_pressure_offset);

	if (b_engine_on)
		m_pressure_current = (1.f - 0.9f * Device.fTimeDelta) * m_pressure_current + 0.9f * Device.fTimeDelta;
	else
		m_pressure_current = (1.f - 3.f * Device.fTimeDelta) * m_pressure_current;

	if (e_state_drive == drive)
		rpm += 0.1f * fabsf(m_pressure_factor);

	m_pressure = (1.f - 1.5f * Device.fTimeDelta) * m_pressure + 1.5f * Device.fTimeDelta * rpm;
	clamp(m_pressure, m_pressure, deg2rad(m_pressure_max_angle + m_pressure_offset));

	if (m_pressure_factor < 0.f)
		m.rotateZ(m_pressure_current * m_pressure);
	else
		m.rotateZ(-m_pressure_current * m_pressure);

	return true;
}

bool CCar::UpdateBrakesPressureGauge(Fmatrix& m)
{
	float angle = fabsf(m_brakes_pressure_max_angle);
	if (!b_engine_on)
		angle = 0.f;

	if (m_brakes_pressure_current < angle)
		m_brakes_pressure_current = (1.f - 0.9f * Device.fTimeDelta) * m_brakes_pressure_current + 0.9f * Device.fTimeDelta * deg2rad(angle);
	else
		m_brakes_pressure_current = (1.f - 3.f * Device.fTimeDelta) * m_brakes_pressure_current + 3.f * Device.fTimeDelta * deg2rad(angle);

	if (b_engine_on)
	{
		if (b_breaks)
		{
			if (Device.fTimeGlobal - m_break_start < m_break_time && m_break_time > 0.f)
			{
				m_brakes_pressure = (1.f - 0.3f * Device.fTimeDelta * (1.f / m_break_time)) * m_brakes_pressure + (0.3f * Device.fTimeDelta * (1.f / m_break_time)) * m_brakes_pressure_factor;
			}
			else
				m_brakes_pressure = (1.f - 0.09f * Device.fTimeDelta) * m_brakes_pressure + (0.09f * Device.fTimeDelta);
		}
		else
			m_brakes_pressure = (1.f - 0.09f * Device.fTimeDelta) * m_brakes_pressure + (0.09f * Device.fTimeDelta);
	}
	else
		m_brakes_pressure = (1.f - 0.05f * Device.fTimeDelta) * m_brakes_pressure;

	if (m_brakes_pressure_max_angle < 0.f)
		m.rotateZ(m_brakes_pressure_current * m_brakes_pressure);
	else
		m.rotateZ(-m_brakes_pressure_current * m_brakes_pressure);

	return true;
}

bool CCar::UpdateBrakesPressureApplyGauge(Fmatrix& m)
{
	float angle = fabsf(m_brakes_pressure_apply_angle);
	float press = 1.f;
	if (!b_engine_on || !b_breaks)
		press = 0.f;
	else
		if (m_break_time > 0.f)
			press = (Device.fTimeGlobal - m_break_start) / m_break_time;

	clamp(press, 0.f, 1.f);
	m_brakes_pressure_apply_current = (1.f - 3.f * Device.fTimeDelta) * m_brakes_pressure_apply_current + 3.f * Device.fTimeDelta * press;

	if (m_brakes_pressure_apply_angle < 0.f)
		m.rotateZ(deg2rad(angle) * m_brakes_pressure * m_brakes_pressure_apply_current);
	else
		m.rotateZ(-deg2rad(angle) * m_brakes_pressure * m_brakes_pressure_apply_current);

	return true;
}

bool CCar::UpdateEngineGauge(Fmatrix& m)
{
	float rpm = 0.f;
	float twist = 0.f;
	float gas = 1.25f;
	if (e_state_drive != drive || IsTransmissionSwitching())
		gas = 1.f;
	if (!b_engine_on)
		m_engine_current = 0.f;
	else
	{
		rpm = 1.2f * gas * ((m_current_rpm - m_min_rpm) / (m_max_rpm - m_min_rpm));
		twist = rpm;
		clamp(twist, 0.f, 1.f);
		clamp(rpm, 0.f, 1.f);
		if (!m_engine_stage)
		{
			m_engine_current += m_engine_speed * (10.f + 50.f * rpm) * gas * Device.fTimeDelta;
			if (m_engine_current >= 1.f)
			{
				m_engine_current = 1.f;
				m_engine_stage = true;
			}
		}
		else
		{
			m_engine_current -= m_engine_speed * (10.f + 50.f * rpm) * gas * Device.fTimeDelta;
			if (m_engine_current <= -1.f)
			{
				m_engine_current = -1.f;
				m_engine_stage = false;
			}
		}
	}

	m.rotateZ(-(m_engine_current * (1.f - rpm) * deg2rad(m_engine_angle) * gas + (twist * deg2rad(m_engine_angle)) * gas));

	return true;
}

bool CCar::UpdateGearGauge(CBoneInstance* B, Fmatrix& m)
{
	float angle_x = m_gear_park_angle_x;
	float angle_z = m_gear_middle_angle;
	if (Owner())
	{
		if (!m_gear_manual)
		{
			if (b_engine_on || GetSpeedKmH() > 1.f)
			{
				if (m_current_transmission_num == 0)
					angle_x = m_gear_reverse_angle_x;
				else
					angle_x = m_gear_drive_angle_x;
			}
		}
		else
		{
			if (IsTransmissionSwitching())
			{
				angle_x = m_gear_park_angle_x;
			}
			else
			{
				if (m_current_transmission_num == 0)
				{
					angle_x = m_gear_reverse_angle_x;
				}
				else
				{
					if (m_current_transmission_num % 2 == 1)
						angle_x = m_gear_drive_angle_x;
					else
						angle_x = m_gear_reverse_angle_x;
				}
			}

			if (m_current_transmission_num == 0 || m_current_transmission_num > 4)
				angle_z = m_gear_right_angle;
			else
				if (m_current_transmission_num < 3)
					angle_z = m_gear_left_angle;
				else
					angle_z = m_gear_middle_angle;
		}
		float delay = m_transmission_switch_delay;
		if (delay > 0.f)
		{
			clamp(delay, 0.f, 1.f);
			float spd = (50.f + 150.f * (1.f - delay)) * m_gear_speed * Device.fTimeDelta;
			if (m_gear_current_angle_x != angle_x)
			{
				if (m_gear_current_angle_x > angle_x)
				{
					m_gear_current_angle_x -= spd;
					clamp(m_gear_current_angle_x, angle_x, m_gear_current_angle_x);
				}
				else
				{
					m_gear_current_angle_x += spd;
					clamp(m_gear_current_angle_x, m_gear_current_angle_x, angle_x);
				}
			}
			else
			{
				if (m_gear_current_angle_z > angle_z)
				{
					m_gear_current_angle_z -= spd * 0.5f;
					clamp(m_gear_current_angle_z, angle_z, m_gear_current_angle_z);
				}
				if (m_gear_current_angle_z < angle_z)
				{
					m_gear_current_angle_z += spd * 0.5f;
					clamp(m_gear_current_angle_z, m_gear_current_angle_z, angle_z);
				}
			}
			angle_x = m_gear_current_angle_x;
			angle_z = m_gear_current_angle_z;
		}
	}
	if (m_gear_manual)
	{
		Fmatrix n;
		n.rotateZ(deg2rad(angle_z));
		B->mTransform.mulB_43(n);
	}
	m.rotateX(deg2rad(angle_x));

	return true;
}

bool CCar::UpdateClockSecondsGauge(Fmatrix& m)
{
	if (!m_clock_seconds_real)
		m.rotateZ(deg2rad(m_clock_seconds_offset * 6.f) - deg2rad(Level().GetGameDayTimeSec() * 6.f));
	else
		m.rotateZ(deg2rad(m_clock_seconds_offset * 6.f) - deg2rad(Level().GetGameDayTimeSec() * 6.f / Level().GetGameTimeFactor()));

	return true;
}

bool CCar::UpdateClockMinutesGauge(Fmatrix& m)
{
	m.rotateZ(deg2rad(m_clock_minutes_offset * 6.f) - deg2rad(Level().GetGameDayTimeSec() / 10.f));

	return true;
}

bool CCar::UpdateClockHoursGauge(Fmatrix& m)
{
	m.rotateZ(deg2rad(m_clock_hours_offset * 30.f) - deg2rad(Level().GetGameDayTimeSec() / 120.f));

	return true;
}

void CCar::cb_Suspension(CBoneInstance* B)
{
	Suspension_struct* C = static_cast<Suspension_struct*>(B->callback_param());
	Fmatrix m;
	if (!C->pcar) return;
	IKinematics* pKinematics = smart_cast<IKinematics*>(C->pcar->Visual());
	
	if (C->second_wheel != BI_NONE) // dependant suspension
	{
		u16 left_wheel_bone_id = C->first_wheel;
		u16 right_wheel_bone_id = C->second_wheel;
		if (left_wheel_bone_id == BI_NONE || left_wheel_bone_id >= pKinematics->LL_BoneCount()) return;
		if (right_wheel_bone_id == BI_NONE || right_wheel_bone_id >= pKinematics->LL_BoneCount()) return;

		Fmatrix left_transform = pKinematics->LL_GetTransform(left_wheel_bone_id);
		Fmatrix right_transform = pKinematics->LL_GetTransform(right_wheel_bone_id);
		if(_abs(left_transform.c.y) > C->pcar->Radius()) return;
		if(_abs(right_transform.c.y) > C->pcar->Radius()) return;
		if (C->steer)
		{
			Fvector dest_dir = Fvector().sub(right_transform.c, left_transform.c);
			safe_normalize(dest_dir);
			B->mTransform.i = dest_dir;
		}

		if (C->move)
		{

			float height = (0.5f * (left_transform.c.y + right_transform.c.y)) + C->suspension_offset;
			B->mTransform.c.y = height;
		}
	}
	else // independant suspension
	{
		u16 wheel_bone_id = C->first_wheel;
		if (C->steer)
		{
			Fmatrix m;
			BONE_P_PAIR_CIT wheel_joint = C->pcar->bone_map.find(wheel_bone_id);
			if (wheel_joint == C->pcar->bone_map.end()) return;
			float angle = dJointGetHinge2Angle1(wheel_joint->second.joint->GetDJoint());
			m.rotateZ(angle * C->steer_factor);

			B->mTransform.mulB_43(m);
		}
		if (C->move)
		{
			if (wheel_bone_id == BI_NONE || wheel_bone_id >= pKinematics->LL_BoneCount()) return;

			Fmatrix wheel_transform = pKinematics->LL_GetTransform(wheel_bone_id);
			if(_abs(wheel_transform.c.y) > C->pcar->Radius()) return;

			float height = wheel_transform.c.y + C->suspension_offset;
			B->mTransform.c.y = height;
		}
	}

}

// Core events
void	CCar::Load					( LPCSTR section )
{
	inherited::Load					(section);
	SpatialComponent->type |= ESPATIAL_TYPE::VISIBLEFORAI;
	SpatialComponent->type |= ESPATIAL_TYPE::REACTTOSOUND;

	inventory().m_pOwner = this;
	CInventoryOwner::Load(section);
}

bool	CCar::net_Spawn				(CSE_Abstract* DC)
{
#ifdef DEBUG
	InitDebug();
#endif
	CSE_Abstract					*e = (CSE_Abstract*)(DC);
	CSE_ALifeCar					*co=smart_cast<CSE_ALifeCar*>(e);
	bool							R = inherited::net_Spawn(DC);

	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);

	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	CInifile* ini = pKinematics->LL_UserData();

	for (auto& Bone : *pKinematics->LL_Bones())
	{
		LPCSTR b_name = Bone.first.c_str();
		if (ini->section_exist(b_name))
		{
			CBoneData& bd = pKinematics->LL_GetData(Bone.second);
			if (ini->line_exist(b_name, "bind_direction"))
			{
				Fvector last_c = bd.bind_transform.c;
				bd.bind_transform.setXYZ(ini->r_fvector3(b_name, "bind_direction"));
				bd.bind_transform.c.set(last_c);
			}

			if (ini->line_exist(b_name, "bind_direction_add"))
			{
				Fvector dir_add = ini->r_fvector3(b_name, "bind_direction_add");
				Fvector last_c = bd.bind_transform.c;
				Fmatrix rX; rX.rotateX(dir_add.x);
				bd.bind_transform.mulB_43(rX);
				Fmatrix rY; rY.rotateY(dir_add.y);
				bd.bind_transform.mulB_43(rY);
				Fmatrix rZ; rZ.rotateZ(dir_add.z);
				bd.bind_transform.mulB_43(rZ);
				bd.bind_transform.c.set(last_c);
			}

			if (ini->line_exist(b_name, "bind_position"))
			{
				bd.bind_transform.c.set(ini->r_fvector3(b_name, "bind_position"));
			}

			if (ini->line_exist(b_name, "bind_position_add"))
			{
				bd.bind_transform.c.add(ini->r_fvector3(b_name, "bind_position_add"));
			}

			if (ini->line_exist(b_name, "bind_mass_pos"))
			{
				bd.center_of_mass.set(ini->r_fvector3(b_name, "bind_mass_pos"));
			}

			if (ini->line_exist(b_name, "bind_mass_pos_add"))
			{
				bd.center_of_mass.add(ini->r_fvector3(b_name, "bind_mass_pos_add"));
			}

			if (ini->line_exist(b_name, "bind_mass"))
			{
				bd.mass = ini->r_float(b_name, "bind_mass");
			}

			if (ini->line_exist(b_name, "bind_mass_add"))
			{
				bd.mass += ini->r_float(b_name, "bind_mass_add");
			}

			if (ini->line_exist(b_name, "bind_shape_type"))
			{
				bd.shape.type = SBoneShape::EShapeType(ini->r_u16(b_name, "bind_shape_type"));
			}
			switch (bd.shape.type)
			{
				case SBoneShape::stBox: 
				{
					if (ini->line_exist(b_name, "bind_shape_box_size"))
					{
						bd.shape.box.m_halfsize.set(ini->r_fvector3(b_name, "bind_shape_box_size"));
					}
					if (ini->line_exist(b_name, "bind_shape_box_size_add"))
					{
						bd.shape.box.m_halfsize.add(ini->r_fvector3(b_name, "bind_shape_box_size_add"));
					}
					if (ini->line_exist(b_name, "bind_shape_box_scale"))
					{
						bd.shape.box.m_halfsize.mul(ini->r_float(b_name, "bind_shape_box_scale"));
					}
					if (ini->line_exist(b_name, "bind_shape_box_pos"))
					{
						bd.shape.box.m_translate.set(ini->r_fvector3(b_name, "bind_shape_box_pos"));
					}
					if (ini->line_exist(b_name, "bind_shape_box_pos_add"))
					{
						bd.shape.box.m_translate.add(ini->r_fvector3(b_name, "bind_shape_box_pos_add"));
					}
					if (ini->line_exist(b_name, "bind_shape_box_dir"))
					{
						Fmatrix box_dir;
						box_dir.j = bd.shape.box.m_rotate.j;
						box_dir.i = bd.shape.box.m_rotate.i;
						box_dir.k = bd.shape.box.m_rotate.k;

						box_dir.setXYZ(ini->r_fvector3(b_name, "bind_shape_box_dir"));

						bd.shape.box.m_rotate.j = box_dir.j;
						bd.shape.box.m_rotate.i = box_dir.i;
						bd.shape.box.m_rotate.k = box_dir.k;
					}

					if (ini->line_exist(b_name, "bind_shape_box_dir_add"))
					{
						Fmatrix box_dir;
						box_dir.j = bd.shape.box.m_rotate.j;
						box_dir.i = bd.shape.box.m_rotate.i;
						box_dir.k = bd.shape.box.m_rotate.k;

						Fvector dir_add = ini->r_fvector3(b_name, "bind_shape_box_dir_add");

						box_dir.rotateX(dir_add.x);
						box_dir.rotateY(dir_add.y);
						box_dir.rotateZ(dir_add.z);

						bd.shape.box.m_rotate.j = box_dir.j;
						bd.shape.box.m_rotate.i = box_dir.i;
						bd.shape.box.m_rotate.k = box_dir.k;
					}
				}
				case SBoneShape::stSphere: 	
				{
					if (ini->line_exist(b_name, "bind_shape_sphere_radius"))
					{
						bd.shape.sphere.R = ini->r_float(b_name, "bind_shape_sphere_radius");
					}
					if (ini->line_exist(b_name, "bind_shape_sphere_radius_add"))
					{
						bd.shape.sphere.R += ini->r_float(b_name, "bind_shape_sphere_radius_add");
					}
					if (ini->line_exist(b_name, "bind_shape_sphere_pos"))
					{
						bd.shape.sphere.P.set(ini->r_fvector3(b_name, "bind_shape_sphere_pos"));
					}
					if (ini->line_exist(b_name, "bind_shape_sphere_pos_add"))
					{
						bd.shape.sphere.P.add(ini->r_fvector3(b_name, "bind_shape_sphere_pos_add"));
					}
				}
				case SBoneShape::stCylinder:
				{
					if (ini->line_exist(b_name, "bind_shape_cylinder_radius"))
					{
						bd.shape.cylinder.m_radius = ini->r_float(b_name, "bind_shape_cylinder_radius");
					}
					if (ini->line_exist(b_name, "bind_shape_cylinder_radius_add"))
					{
						bd.shape.cylinder.m_radius += ini->r_float(b_name, "bind_shape_cylinder_radius_add");
					}
					if (ini->line_exist(b_name, "bind_shape_cylinder_height"))
					{
						bd.shape.cylinder.m_height = ini->r_float(b_name, "bind_shape_cylinder_height");
					}
					if (ini->line_exist(b_name, "bind_shape_cylinder_height_add"))
					{
						bd.shape.cylinder.m_height += ini->r_float(b_name, "bind_shape_cylinder_height_add");
					}
					if (ini->line_exist(b_name, "bind_shape_cylinder_pos"))
					{
						bd.shape.cylinder.m_center.set(ini->r_fvector3(b_name, "bind_shape_cylinder_pos"));
					}
					if (ini->line_exist(b_name, "bind_shape_cylinder_pos_add"))
					{
						bd.shape.cylinder.m_center.add(ini->r_fvector3(b_name, "bind_shape_cylinder_pos_add"));
					}
					if (ini->line_exist(b_name, "bind_shape_cylinder_dir"))
					{
						bd.shape.cylinder.m_direction.set(ini->r_fvector3(b_name, "bind_shape_cylinder_dir"));
					}
					if (ini->line_exist(b_name, "bind_shape_cylinder_dir_add"))
					{
						bd.shape.cylinder.m_direction.add(ini->r_fvector3(b_name, "bind_shape_cylinder_dir_add"));
					}
				}
			};
		}
	}

	CPHSkeleton::Spawn(e);
	setEnabled						(TRUE);
	setVisible						(TRUE);
	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);
	SetfHealth						(co->health);
	m_fuel							= co->fuel;
	if(!g_Alive())					b_exploded=true;
	else							b_exploded=false;
									
	CDamagableItem::RestoreEffect();
	

	if (ini)
	{
		if (ini->section_exist("destroyed"))
			CPHDestroyable::Load(ini, "destroyed");

		if (ini->section_exist("visual_memory_definition"))
		{
			m_memory = new car_memory(this);
			m_memory->reload(ini->r_string("visual_memory_definition", "section"));
		}
	}

	shedule.t_min = shedule.t_max = 1;
	CInventoryOwner::net_Spawn(DC);
	Engine.Sheduler.Register(this, TRUE);

	return							(CScriptEntity::net_Spawn(DC) && R);
	
}

void CCar::ActorObstacleCallback					(bool& do_colide,bool bo1,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2)	
{
	dContact& c = cphc.Native();
	if(!do_colide)
	{
		if(material_1&&material_1->Flags.test(SGameMtl::flActorObstacle))do_colide=true;
		if(material_2&&material_2->Flags.test(SGameMtl::flActorObstacle))do_colide=true;
	}
}

void CCar::CollisionDetectionCallback					(bool& do_colide,bool bo1,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2)	
{
	dContact& c = cphc.Native();
	dxGeomUserData *l_pUD1 = retrieveGeomUserData(c.geom.g1);
	dxGeomUserData *l_pUD2 = retrieveGeomUserData(c.geom.g2);
	
	if(!l_pUD1) return;
	if(!l_pUD2) return;

	CPhysicsShellHolder *Collided_object = NULL;
	CPhysicsShellHolder *ME = smart_cast<CPhysicsShellHolder*>(l_pUD1->ph_ref_object);
	CPhysicsShellHolder *WHO = smart_cast<CPhysicsShellHolder*>(l_pUD2->ph_ref_object);
	Collided_object = ME ? ME : WHO ? WHO : NULL;
	CGameObject* Collided_gobject = smart_cast<CGameObject*>(Collided_object);
	if(!Collided_object || !Collided_object->m_pPhysicsShell || !Collided_gobject || Collided_gobject->getDestroy() || Collided_gobject->cast_actor())
	{
		do_colide = false;
		return;
	}
}
void CCar::SpawnInitPhysics	(CSE_Abstract	*D)
{
	CSE_PHSkeleton		*so = smart_cast<CSE_PHSkeleton*>(D);
	R_ASSERT						(so);
	ParseDefinitions				();//parse ini filling in m_driving_wheels,m_steering_wheels,m_breaking_wheels
	LoadBoneMap						();//fill in bone_map
	CreateSkeleton					(D);//creates m_pPhysicsShell
	IKinematics *K					= smart_cast<IKinematics*>(Visual());
	CInifile* ini					= K->LL_UserData();
	K->CalculateBones_Invalidate();//this need to call callbacks
	K->CalculateBones	(TRUE);
	Init							();//inits m_driving_wheels,m_steering_wheels,m_breaking_wheels values using recieved in ParceDefinitions & from bone_map
	PPhysicsShell()->add_ObjectContactCallback(ActorObstacleCallback);
	SetDefaultNetState				(so);
	CPHUpdateObject::Activate       ();

	if (ini)
	{
		CreateCharacterScraper(K, ini);
		CreateCarTurrets(ini);
	}

	ApplyBonePhysicsParams(K, ini);

	BindGaugeAndSuspensionCallbacks(K, ini);

	NeutralDrive();
	PressBreaks();

	m_pPhysicsShell->Activate	(XFORM(), 0, XFORM());
	smart_cast<IKinematics*>(Visual())->CalculateBones_Invalidate	();
	smart_cast<IKinematics*>(Visual())->CalculateBones(TRUE);

	m_pPhysicsShell->applyForce({ 0.f, 0.f, 0.f }, 0.00001f);
	m_pPhysicsShell->GetGlobalTransformDynamic(&XFORM());
}

void CCar::CreateCharacterScraper(IKinematics* K, CInifile* ini)
{
	if (!ini->line_exist("car_definition", "character_scraper"))
		return;

	characters_scraper_shell = P_create_Shell();
	Fsphere RootSphere;
	RootSphere.set(Fvector().set(0, 1, 0), 0.1f);
	CPhysicsElement* RootElement = P_create_Element();
	RootElement->add_Sphere(RootSphere);
	characters_scraper_shell->add_Element(RootElement);

	const float ZOffsets[] = { -0.7f, 0.f, 0.7f };
	for (float ZOffset : ZOffsets)
	{
		Fvector Center = bind_center;
		Center.z += Radius() * ZOffset;

		Fsphere Sphere;
		Sphere.set(Center, Radius() * 0.5f);
		CPhysicsElement* JointElement = P_create_Element();
		JointElement->add_Sphere(Sphere);
		characters_scraper_shell->add_Element(JointElement);

		CPhysicsJoint* Joint = P_create_Joint(CPhysicsJoint::ball, RootElement, JointElement);
		Joint->SetAnchorVsSecondElement(0.f, 0.f, 0.f);
		Joint->SetJointSDfactors(1000.f, 1000.f);
		characters_scraper_shell->add_Joint(Joint);
	}

	characters_scraper_shell->SetIgnoreDynamic();
	characters_scraper_shell->SetIgnoreStatic();
	characters_scraper_shell->set_ApplyByGravity(FALSE);
	characters_scraper_shell->setMass(50.f);
	characters_scraper_shell->SetMaterial(K->LL_GetData(K->LL_GetBoneRoot()).game_mtl_idx);
	characters_scraper_shell->set_PhysicsRefObject(nullptr);
	characters_scraper_shell->add_ObjectContactCallback(CollisionDetectionCallback);
	characters_scraper_shell->SetPrefereExactIntegration();
	characters_scraper_shell->Activate();
}

void CCar::CreateCarTurrets(CInifile* ini)
{
	if (ini->section_exist("mounted_weapon_definition"))
		m_car_weapons.push_back(new CCarTurret(this, "mounted_weapon_definition"));

	for (int i = 0; i < m_car_weapon_slots; ++i)
	{
		string128 MountedWeaponSect;
		xr_sprintf(MountedWeaponSect, "mounted_weapon_definition_%d", i);
		if (ini->section_exist(MountedWeaponSect))
			m_car_weapons.push_back(new CCarTurret(this, MountedWeaponSect));
	}
}

void CCar::ApplyBonePhysicsParams(IKinematics* K, CInifile* ini)
{
	for (auto& Bone : bone_map)
	{
		if (Bone.second.element)
		{
			LPCSTR b_name = K->LL_BoneName_dbg(Bone.second.element->m_SelfID);
			if (ini->section_exist(b_name))
			{
				if (ini->line_exist(b_name, "linear_factor") && ini->line_exist(b_name, "angular_factor"))
				{
					float linear_factor = default_k_l * ini->r_float(b_name, "linear_factor");
					float angular_factor = default_k_w * ini->r_float(b_name, "angular_factor");
					Bone.second.element->SetAirResistance(linear_factor, angular_factor);
				}
				if (ini->line_exist(b_name, "linear_limit") && ini->line_exist(b_name, "angular_limit"))
				{
					float linear_limit = default_l_limit * ini->r_float(b_name, "linear_limit");
					float angular_limit = default_w_limit * ini->r_float(b_name, "angular_limit");
					Bone.second.element->set_DynamicLimits(linear_limit, angular_limit);
				}
				if (ini->line_exist(b_name, "linear_scale") && ini->line_exist(b_name, "angular_scale"))
				{
					float linear_scale = 1.01f * ini->r_float(b_name, "linear_scale");
					float angular_scale = 1.01f * ini->r_float(b_name, "angular_scale");
					Bone.second.element->set_DynamicScales(linear_scale, angular_scale);
				}
				if (ini->line_exist(b_name, "apply_by_gravity"))
					Bone.second.element->set_ApplyByGravity(ini->r_bool(b_name, "apply_by_gravity"));

				if (Bone.second.joint)
				{
					float velocity_ = 0.0f;
					float force_ = 0.0f;
					int axis_ = -1;
					if (ini->line_exist(b_name, "joint_vel"))
						velocity_ = ini->r_float(b_name, "joint_vel");
					if (ini->line_exist(b_name, "joint_force"))
						force_ = ini->r_float(b_name, "joint_force");
					if (ini->line_exist(b_name, "joint_axis"))
						axis_ = ini->r_s32(b_name, "joint_axis");

					if (ini->line_exist(b_name, "joint_vel") || ini->line_exist(b_name, "joint_force"))
					{
						if (ini->line_exist(b_name, "headlight_hideaway"))
							headlight_hideaways.push_back(SHeadlightHideaway(Bone.second.joint, velocity_, force_, axis_));
						else if (ini->line_exist(b_name, "engine_door"))
							engine_door.push_back(SEngineDoor(Bone.second.joint, velocity_, force_, axis_));
						else if (ini->line_exist(b_name, "suspension_joint"))
							suspension_joints.push_back(SSuspension(Bone.second.joint, velocity_, force_, axis_, ini->line_exist(b_name, "suspension_indpnd") ? ini->r_bool(b_name, "suspension_indpnd") : false));
						else
							Bone.second.joint->SetForceAndVelocity(force_, velocity_, axis_);
					}
				}
			}
		}
	}
}

void CCar::BindGaugeAndSuspensionCallbacks(IKinematics* K, CInifile* ini)
{
	for (auto& Bone : *K->LL_Bones())
	{
		const char* Section = Bone.first.c_str();
		if (!ini->section_exist(Section))
			continue;

		const u16 BoneId = Bone.second;
		if (ini->line_exist(Section, "speedometer"))
			BindSpeedometerGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "tachometer"))
			BindTachometerGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "fuel"))
			BindFuelGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "temp"))
			BindTemperatureGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "gauge"))
			BindGenericGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "fan"))
			BindFanGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "pressure"))
			BindPressureGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "battery"))
			BindBatteryGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "brakes_pressure"))
			BindBrakesPressureGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "brakes_pressure_apply"))
			BindBrakesPressureApplyGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "blower"))
			BindBlowerGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "engine"))
			BindEngineGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "gear"))
			BindGearGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "clock_seconds"))
			BindClockSecondsGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "clock_minutes"))
			BindClockMinutesGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "clock_hours"))
			BindClockHoursGauge(Section, BoneId, K, ini);
		if (ini->line_exist(Section, "suspension"))
			BindSuspension(Section, BoneId, K, ini);
	}
}

void CCar::BindGaugeCallback(IKinematics* K, u16 BoneId, int GaugeType)
{
	m_gauge_params.push_back(xr_make_unique<Gauge_struct>(this, GaugeType));
	m_gauge_bones.push_back(BoneId);
	K->LL_GetBoneInstance(BoneId).set_callback(bctCustom, cb_Gauge, m_gauge_params.back().get());
}

void CCar::BindSpeedometerGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_speedometer);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "speedometer_factor"))
		m_speedometer_factor = ini->r_float(Section, "speedometer_factor");
	if (ini->line_exist(Section, "speedometer_max_speed"))
		m_speedometer_max_speed = ini->r_float(Section, "speedometer_max_speed");
	if (ini->line_exist(Section, "speedometer_min_speed"))
		m_speedometer_min_speed = ini->r_float(Section, "speedometer_min_speed");
	if (ini->line_exist(Section, "speedometer_offset"))
		m_speedometer_offset = ini->r_float(Section, "speedometer_offset");
}

void CCar::BindTachometerGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_tachometer);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "tachometer_factor"))
		m_tachometer_factor = ini->r_float(Section, "tachometer_factor");
	if (ini->line_exist(Section, "tachometer_min_rpm"))
		m_tachometer_min_rpm = ini->r_float(Section, "tachometer_min_rpm");
}

void CCar::BindFuelGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_fuel);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "fuel_angle"))
		m_fuel_angle = ini->r_float(Section, "fuel_angle");
	if (ini->line_exist(Section, "fuel_offset"))
		m_fuel_offset = ini->r_float(Section, "fuel_offset");
}

void CCar::BindTemperatureGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_temp);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "temp_angle"))
		m_temp_angle = ini->r_float(Section, "temp_angle");
	if (ini->line_exist(Section, "temp_offset"))
		m_temp_offset = ini->r_float(Section, "temp_offset");
}

void CCar::BindGenericGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_gauge);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "gauge_angle"))
		m_gauge_angle = ini->r_float(Section, "gauge_angle");
}

void CCar::BindFanGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_fan);
	if (ini->line_exist(Section, "fan_speed"))
		m_fan_speed = ini->r_float(Section, "fan_speed");
	if (ini->line_exist(Section, "fan_engine"))
		m_fan_engine = ini->r_bool(Section, "fan_engine");
}

void CCar::BindPressureGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_pressure);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "pressure_max_angle"))
		m_pressure_max_angle = ini->r_float(Section, "pressure_max_angle");
	if (ini->line_exist(Section, "pressure_idle_angle"))
		m_pressure_idle_angle = ini->r_float(Section, "pressure_idle_angle");
	if (ini->line_exist(Section, "pressure_factor"))
		m_pressure_factor = ini->r_float(Section, "pressure_factor");
	if (ini->line_exist(Section, "pressure_offset"))
		m_pressure_offset = ini->r_float(Section, "pressure_offset");
}

void CCar::BindBatteryGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_battery);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "battery_factor"))
		m_battery_factor = ini->r_float(Section, "battery_factor");
	if (ini->line_exist(Section, "battery_angle"))
		m_battery_angle = ini->r_float(Section, "battery_angle");
}

void CCar::BindBrakesPressureGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_brakes_pressure);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "brakes_pressure_factor"))
		m_brakes_pressure_factor = ini->r_float(Section, "brakes_pressure_factor");
	if (ini->line_exist(Section, "brakes_pressure_max_angle"))
		m_brakes_pressure_max_angle = ini->r_float(Section, "brakes_pressure_max_angle");
}

void CCar::BindBrakesPressureApplyGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_brakes_pressure_apply);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "brakes_pressure_apply_angle"))
		m_brakes_pressure_apply_angle = ini->r_float(Section, "brakes_pressure_apply_angle");
}

void CCar::BindBlowerGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_blower);
	if (ini->line_exist(Section, "blower_angle"))
		m_blower_angle = ini->r_float(Section, "blower_angle");
}

void CCar::BindEngineGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_engine);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "engine_angle"))
		m_engine_angle = ini->r_float(Section, "engine_angle");
	if (ini->line_exist(Section, "engine_speed"))
		m_engine_speed = ini->r_float(Section, "engine_speed");
}

void CCar::BindGearGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, gauge_gear);
	if (ini->line_exist(Section, "gear_manual"))
		m_gear_manual = ini->r_bool(Section, "gear_manual");
	if (ini->line_exist(Section, "gear_speed"))
		m_gear_speed = ini->r_float(Section, "gear_speed");
	if (ini->line_exist(Section, "gear_park_angle_x"))
		m_gear_park_angle_x = ini->r_float(Section, "gear_park_angle_x");
	if (ini->line_exist(Section, "gear_drive_angle_x"))
		m_gear_drive_angle_x = ini->r_float(Section, "gear_drive_angle_x");
	if (ini->line_exist(Section, "gear_reverse_angle_x"))
		m_gear_reverse_angle_x = ini->r_float(Section, "gear_reverse_angle_x");
	if (ini->line_exist(Section, "gear_left_angle"))
		m_gear_left_angle = ini->r_float(Section, "gear_left_angle");
	if (ini->line_exist(Section, "gear_middle_angle"))
		m_gear_middle_angle = ini->r_float(Section, "gear_middle_angle");
	if (ini->line_exist(Section, "gear_right_angle"))
		m_gear_right_angle = ini->r_float(Section, "gear_right_angle");
}

void CCar::BindClockSecondsGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, clock_seconds);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "clock_seconds_offset"))
		m_clock_seconds_offset = ini->r_float(Section, "clock_seconds_offset");
	if (ini->line_exist(Section, "clock_seconds_real"))
		m_clock_seconds_real = ini->r_bool(Section, "clock_seconds_real");
}

void CCar::BindClockMinutesGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, clock_minutes);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "clock_minutes_offset"))
		m_clock_minutes_offset = ini->r_float(Section, "clock_minutes_offset");
}

void CCar::BindClockHoursGauge(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	BindGaugeCallback(K, BoneId, clock_hours);
	pribor_arrows.emplace(BoneId);
	if (ini->line_exist(Section, "clock_hours_offset"))
		m_clock_hours_offset = ini->r_float(Section, "clock_hours_offset");
}

void CCar::BindSuspension(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	LPCSTR first_bone_name = READ_IF_EXISTS(ini, r_string, Section, "first_wheel", "BI_NONE");
	LPCSTR second_bone_name = READ_IF_EXISTS(ini, r_string, Section, "second_wheel", "BI_NONE");
	bool steer = READ_IF_EXISTS(ini, r_bool, Section, "steer", true);
	bool move = READ_IF_EXISTS(ini, r_bool, Section, "move", true);
	float suspension_offset = READ_IF_EXISTS(ini, r_float, Section, "suspension_offset", 0.f);
	float steer_factor = READ_IF_EXISTS(ini, r_float, Section, "steer_factor", 1.f);
	m_suspension_params.push_back(xr_make_unique<Suspension_struct>(this, K->LL_BoneID(first_bone_name), K->LL_BoneID(second_bone_name), suspension_offset, steer, move, steer_factor));
	m_suspension_bones.push_back(BoneId);
	K->LL_GetBoneInstance(BoneId).set_callback(bctCustom, cb_Suspension, m_suspension_params.back().get());
}

void	CCar::net_Destroy()
{
#ifdef DEBUG
	DBgClearPlots();
#endif
	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	if (pKinematics != nullptr)
	{
		auto ResetCallback = [pKinematics](u16 BoneId)
		{
			if (BoneId != BI_NONE)
				pKinematics->LL_GetBoneInstance(BoneId).reset_callback();
		};

		for (u16 BoneId : m_steers)
			ResetCallback(BoneId);
		for (u16 BoneId : m_gauge_bones)
			ResetCallback(BoneId);
		for (u16 BoneId : m_suspension_bones)
			ResetCallback(BoneId);
	}
	m_gauge_params.clear();
	m_suspension_params.clear();
	m_gauge_bones.clear();
	m_suspension_bones.clear();
	m_lights.TurnOffLights();
	m_stoplights.TurnOffLights();
	m_indoorlights.TurnOffLights();
	m_enginelights.TurnOffLights();
	m_reverselights.TurnOffLights();
	CScriptEntity::net_Destroy();
	inherited::net_Destroy();
	CExplosive::net_Destroy();
	CInventoryOwner::net_Destroy();
	if(m_pPhysicsShell)
	{
		m_pPhysicsShell->Deactivate();
		m_pPhysicsShell->ZeroCallbacks();
		xr_delete(m_pPhysicsShell);
	}
	CHolderCustom::detach_Actor();
	ClearExhausts();
	for (auto& i_w : m_wheels_map)
	{
		if (i_w.second.tire_drive._p)
			i_w.second.tire_drive.destroy();
	}
	m_wheels_map.clear();
	m_steering_wheels.clear();
	m_driving_wheels.clear();
	m_exhausts.clear();
	m_steers.clear();
	m_indoor_l_available_doors.clear();
	m_breaking_wheels.clear();
	engine_door.clear();
	headlight_hideaways.clear();
	suspension_joints.clear();
	pribor_arrows.clear();
	RemoveGrassSpheres();
	for (auto& i_d : m_doors)
	{
		i_d.second.snd_door_open.destroy();
		i_d.second.snd_door_close.destroy();
	}
	m_doors_update.clear();
	m_doors_recurs.clear();
	m_doors.clear();
	m_gear_ratious.clear();
	ClearCarWeapons();
	if (m_car_sound != nullptr)
		m_car_sound->Destroy();
	CPHUpdateObject::Deactivate();
	CPHSkeleton::RespawnInit();
	m_damage_particles.Clear();
	CPHDestroyable::RespawnInit();
	CPHCollisionDamageReceiver::Clear();
	bone_map.clear();
	
	b_breaks = false;
	Engine.Sheduler.Unregister(this);
}

void CCar::net_Save(NET_Packet& P)
{
	inherited::net_Save(P);
	SaveNetState(P);
	P.w_float(m_fuel);
}


bool CCar::net_SaveRelevant()
{
	return TRUE;
}

void CCar::SaveNetState(NET_Packet& P)
{
	CPHSkeleton::SaveNetState	   (P);
	P.w_vec3(Position());
	Fvector Angle;
	XFORM().getXYZ(Angle);
	P.w_vec3(Angle);
	{
		P.w_u16(u16(m_doors.size()));
		for (auto& Door : m_doors)
		{
			Door.second.SaveNetState(P);
		}
	}

	{
		P.w_u16(u16(m_wheels_map.size()));
		for (auto& Wheel : m_wheels_map)
		{
			Wheel.second.SaveNetState(P);
		}
	}
	P.w_float(GetfHealth());
}


void CCar::RestoreNetState(CSE_PHSkeleton* po)
{
	if(!po->_flags.test(CSE_PHSkeleton::flSavedData))return;
	CPHSkeleton::RestoreNetState(po);
	
	CSE_ALifeCar* co=smart_cast<CSE_ALifeCar*>(po);

	{
		xr_vector<CSE_ALifeCar::SDoorState>::iterator ii = co->door_states.begin();
		for (auto& Door : m_doors)
		{
			Door.second.RestoreNetState(*ii);
			++ii;
		}
	}
	{
		xr_vector<CSE_ALifeCar::SWheelState>::iterator ii = co->wheel_states.begin();
		for (auto& Wheel : m_wheels_map)
		{
			Wheel.second.RestoreNetState(*ii);
			++ii;
		}
	}
	//as later may kill diable/enable state save it;
	bool enable = PPhysicsShell()->isEnabled();
/////////////////////////////////////////////////////////////////////////
	Fmatrix restored_form;
	PPhysicsShell()->GetGlobalTransformDynamic(&restored_form);
/////////////////////////////////////////////////////////////////////
	Fmatrix inv ,replace,sof;
	sof.setXYZ(co->o_Angle.x,co->o_Angle.y,co->o_Angle.z);
	sof.c.set(co->o_Position);
	inv.set(restored_form);
	inv.invert();
	replace.mul(sof,inv);
////////////////////////////////////////////////////////////////////
	{
		PKinematics(Visual())->CalculateBones_Invalidate();
		PKinematics(Visual())->CalculateBones(TRUE);
	}
////////////////////////////////////////////////////////////////////
	replace.mul(sof,inv);
	PPhysicsShell()->TransformPosition(replace, mh_clear);
	if(enable)PPhysicsShell()->Enable();
	else PPhysicsShell()->Disable();
	PPhysicsShell()->GetGlobalTransformDynamic(&XFORM());
}
void CCar::SetDefaultNetState(CSE_PHSkeleton* po)
{
	if(po->_flags.test(CSE_PHSkeleton::flSavedData))return;
	for (auto& i : m_doors)
	{
		i.second.SetDefaultNetState();
	}
}
void CCar::shedule_Update(u32 dt)
{
	if(Level().Server && !getDestroy() && !b_exploded)
	{
		if(CSE_Abstract* DC = Level().Server->ID_to_entity(ID()))
		{
			if(CSE_ALifeCar* secar = smart_cast<CSE_ALifeCar*>(DC))
			{
				Fvector xyz; XFORM().getXYZ(xyz);
				secar->o_Angle.set(xyz);
				secar->o_Position.set(Position());
			}
 		}
	}
	CInventoryOwner::UpdateInventoryOwner(dt);

	for (CCarTurret* CWPB : m_car_weapons)
	{
		CWPB->DropEmptyAmmoBoxes();
	}

	inherited::shedule_Update(dt);
	if(CPHDestroyable::Destroyed())CPHDestroyable::SheduleUpdate(dt);
	else	CPHSkeleton::Update(dt);
	
	if(CDelayedActionFuse::isActive()&&CDelayedActionFuse::Update(GetfHealth()))
	{
	}
	if(b_exploded&&!m_explosion_flags.test(flExploding)&&!getEnabled())//!m_bExploding
										setEnabled(TRUE);
#ifdef DEBUG
	DbgSheduleUpdate();
#endif
}

void CCar::UpdateEx			(float fov) 
{
	#ifdef DEBUG
	DbgUbdateCl();
	#endif

	VisualUpdate(fov);
	for (CCarTurret* CWPB : m_car_weapons)
	{
		CWPB->UpdateCL();
		if(CWPB->IsActive() && OwnerActor())
		{
			if(active_camera->tag == EctFirstCam && CWPB->CameraEnabled() && OwnerActor()->IsMyCamera())
			{
				CWPB->cam_Update(fov);
				return;
			}
			CCameraBase* C = active_camera;
			collide::rq_result RQ = GetPickResult(C->Position(), C->Direction(), 1000.0f, this);
			Fvector target_position = Fvector(C->Position()).add(Fvector(C->Direction()).mul(RQ.range));
			CWPB->SetParam(CCarTurret::eWpnDesiredPos, target_position);
		}
	}
	if(OwnerActor() && OwnerActor()->IsMyCamera())
	{
		if(active_camera->tag == EctFiveCam)
		{
			Fvector							Da;
			Da.set							(0,0,0);
			Fvector UpdatePos = active_camera->Position();

			Fvector dest_dir, dest_pos;
			dest_pos = Fvector(bind_center);
			XFORM().transform_tiny(dest_pos);
			dest_dir.sub(dest_pos, active_camera->Position());
			safe_normalize(dest_dir);
			active_camera->vDirection.set(dest_dir);
			active_camera->vNormal.set(0,1,0);

			Level().Cameras().UpdateFromCamera(active_camera);
			Level().Cameras().ApplyDevice(Device.fViewportNear);
		}
		else
			cam_Update(Device.fTimeDelta, fov);
	}
}

CCameraBase* CCar::Camera()
{
	for (CCarTurret* CWPB : m_car_weapons)
	{
		if(CWPB->IsActive())
		{
			if(active_camera->tag == EctFirstCam && CWPB->CameraEnabled() && OwnerActor() && OwnerActor()->IsMyCamera())
				return CWPB->Camera();
		}
	}
	return active_camera;
}

bool CCar::AlwaysTheCrow()
{
	bool crow = !!OwnerActor();
	for (CCarTurret* CWPB : m_car_weapons)
	{
		if(CWPB->IsActive())
			crow = TRUE;
	}

	return crow;
}

void CCar::RemoveGrassSpheres()
{
}
void CCar::UpdateCL				( )
{
	// Clear a boost value left by input from a previous frame. Input normally
	// sets m_steer_boost_frame to the current frame before UpdateCL runs; if
	// no steering key was held, the stale value must not be displayed.
	if (m_steer_boost_frame != Device.dwFrame)
	{
		m_steer_boost = 0.f;
	}

	inherited::UpdateCL();
	CExplosive::UpdateCL();

	// Exit must run outside the physics step: detach_Vehicle can recreate the
	// actor character, and destroying it during CPHWorld::Step asserts.
	if (b_event_exit_car)
	{
		if (!OwnerActor())
		{
			b_event_exit_car = false;
			m_exit_door_bone = BI_NONE;
		}
		else if (!Device.Paused() && Device.dwTimeGlobal >= m_exit_car_time)
		{
			// The car may still be settling right after a save is loaded. Recompute
			// the exit point from the current car transform so the actor is not
			// recreated at a stale position inside the vehicle.
			auto ExitDoor = m_doors.find(m_exit_door_bone);
			if (ExitDoor != m_doors.end())
				ExitDoor->second.GetExitPosition(m_exit_position);
			OwnerActor()->detach_Vehicle();
		}
	}

	//Test particles assert
	//for (int i = 0; i < 255; ++i)
	//	if (auto ps = CParticlesObject::Create("phy_fx\\generic_body_dust_01", TRUE))
	//Test Doors
	//for (; i != e; ++i)
	//if (Owner() && m_pPhysicsShell)
	if(!Owner())
	{
		for (CCarTurret* CWPB : m_car_weapons)
		{
			CWPB->UpdateCL();
		}
	}
	if(m_memory && !m_car_weapons.empty())
		m_memory->set_camera(m_car_weapons[0]->ViewCameraPos(), m_car_weapons[0]->ViewCameraDir(), m_car_weapons[0]->ViewCameraNorm());

	ASCUpdate			();
	if(m_pPhysicsShell)
	{
		int odcnt = 0;
		for (auto& i : m_doors)
		{
			if (i.second.state != SDoor::closed)
			{
				for (u16 IndoorDoorBone : m_indoor_l_available_doors)
				{
					if (IndoorDoorBone == i.second.bone_id)
						odcnt++;
				}
				
			}
		}
		if(odcnt)
		{
			m_indoorlights.TurnOnLights();
		}
		else
			m_indoorlights.TurnOffLights();
		if(b_engine_on)
		{
			m_enginelights.TurnOnLights();
			if((brp || (fwp && bkp) || b_breaks) && break_start_dt >= 0.1f)
				m_stoplights.TurnOnLights();
			else
				m_stoplights.TurnOffLights();
			if(0 == CurrentTransmission())
				m_reverselights.TurnOnLights();
			else
				m_reverselights.TurnOffLights();

			
		}
		else
		{
			m_reverselights.TurnOffLights();
			m_stoplights.TurnOffLights();
			m_enginelights.TurnOffLights();
		}

		if (calc_bones_at_once)
		{
			Visual()->dcast_PKinematics()->CalculateBones(TRUE);
			calc_bones_at_once = false;
		}
	}
	if (OwnerActor() && OwnerActor()->inventory().GetActiveSlot() != NO_ACTIVE_SLOT)
		OwnerActor()->inventory().Activate(NO_ACTIVE_SLOT);
	if (b_exploded)DoExit();
	if(Owner()) return;
	VisualUpdate(90);
	if (GetScriptControl())
			ProcessScripts();
}
bool rs_car_info = false;
bool rs_car_transmission = false;
bool rs_car_hud_draw = true;
bool rs_car_unlimitedfuel = false;

void CCar::DrawCarInfo()
{
	if (!m_pPhysicsShell || !OwnerActor())
		return;

	CGameFont* Font = UI().Font().pFontStat;
	if (!Font)
		return;

	Fvector AngularVelocity;
	m_pPhysicsShell->get_AngularVel(AngularVelocity);

	string64 SpeedLine;
	xr_sprintf(SpeedLine, "speed, %f km/hour", GetSpeedKmH());

	Font->SetColor(color_rgba(0xff, 0xff, 0xff, 0xff));
	Font->OutSet(Device.TargetHeight / 2, 30);
	Font->SetAligment(CGameFont::alLeft);
	Font->OutNext("Health:            [%3.2f]", GetfHealth());
	Font->OutNext("Vel magnitude:     [%3.2f]", GetVelocityM());
	Font->OutNext("Vel:               [%3.2f]", GetVelocity().dotproduct(XFORM().k));
	Font->OutNext("Accel magnitude:   [%3.2f]", m_acceleration);
	Font->OutNext("Ang speed:         [%3.2f]", GetAVelocity().dotproduct(XFORM().j));
	Font->OutNext("Ang magnitude:     [%3.2f]", AngularVelocity.magnitude());
	Font->OutNext(SpeedLine);
	if (IsTransmissionSwitching())
		Font->SetColor(color_rgba(255, 0, 0, 255));
	Font->OutNext("Transmission num:  [%d]", int(m_current_transmission_num));
	Font->SetColor(color_rgba(0xff, 0xff, 0xff, 0xff));
	Font->OutNext("Gear ratio:        [%3.2f]", m_current_gear_ratio);
	Font->OutNext("Power:             [%3.2f]", m_current_engine_power / (0.8f * 1000.f));
	Font->OutNext("Rpm:               [%3.2f]", m_current_rpm / (1.f / 60.f * 2.f * M_PI));
	Font->OutNext("Min[%f] Current[%f] Max[%f]", m_min_rpm, m_current_rpm, m_max_rpm);
	Font->OutNext("Wheel torque:      [%3.2f]", RefWheelCurTorque());
	Font->OutNext("Wheels drive rate: [%3.2f]", DriveWheelsMeanAngleRate());
	Font->OutNext("Engine torque:     [%3.2f]", EngineCurTorque());
	Font->OutNext("Fuel:              [%3.2f]", m_fuel);
	Font->OutNext("Fuel consumption:  [%3.4f]", m_fuel_consumption_current);
	Font->OutNext("Fuel consumption/hour: [%3.2f]", 3600.f * m_fuel_consumption_current);
	Font->OutNext("Fuel consumption/100km:[%3.2f]", 100.f * 3600.f * m_fuel_consumption_current / (GetSpeedKmH() + EPS));
	Font->OutNext("curr_slip_sliding:          %f", m_current_slip_sliding);
	Font->OutNext("handbrake_rpm_clutch_factor:%f", m_current_rpm_brp_n);
	Font->OutNext("last_contact_time:           %d", int(m_last_contact_time));
	const float SteerDisplayDt = Device.fTimeDelta > EPS ? Device.fTimeDelta : 1.f;
	const float RawSteerBoost = (m_steer_boost_frame == Device.dwFrame) ? m_steer_boost : 0.f;
	const float SteerBoostDisplay = (GetSpeedKmH() > 10.f && m_steer_speed_boost && !b_burnout)
		? RawSteerBoost * m_steer_speed_boost / SteerDisplayDt
		: 0.f;
	const float SpinBoostDisplay = (GetSpeedKmH() <= 10.f && m_steer_spin_boost && b_burnout)
		? RawSteerBoost * m_steer_spin_boost / SteerDisplayDt
		: 0.f;

	Font->OutNext("steer_boost:                %f", SteerBoostDisplay);
	Font->OutNext("spin_boost:                 %f", SpinBoostDisplay);
	LPCSTR TransmissionStageName = "Drive";
	switch (m_transmission_stage)
	{
		case ETransmissionStage::Drive:			break;
		case ETransmissionStage::SwitchUp:		TransmissionStageName = "SwitchUp";		break;
		case ETransmissionStage::SwitchDown:	TransmissionStageName = "SwitchDown";	break;
	}
	Font->OutNext("trans stage:                %s", TransmissionStageName);
	Font->OutNext("downforce:                  %f", m_current_downforce);
	if (mb_traction)
	{
		Font->SetColor(color_rgba(255, 0, 0, 255));
		Font->OutNext("Surface traction");
		Font->SetColor(color_rgba(0xff, 0xff, 0xff, 0xff));
	}

	LPCSTR StateName = "None";
	switch (e_state_drive)
	{
		case drive:		StateName = "Drive";	break;
		case neutral:	StateName = "Neutral";			break;
	}

	Font->SetColor(color_rgba(255, 255, 0, 255));
	Font->OutNext("ENGINE STATE: %s", StateName);
	if (b_engine_on)
	{
		Font->SetColor(color_rgba(0, 255, 0, 255));
		Font->OutNext("ENGINE ON");
	}
	if (b_starting)
	{
		Font->SetColor(color_rgba(255, 0, 0, 255));
		Font->OutNext("STARTER");
		Font->SetColor(color_rgba(0xff, 0xff, 0xff, 0xff));
	}

	// Per-wheel telemetry, ported from the LA Cars-Ext DBG panel.
	IKinematics* WheelKinematics = PKinematics(Visual());
	if (WheelKinematics)
	{
		u32 ActiveWheelIndex = 0;
		for (auto& WheelEntry : m_wheels_map)
		{
			SWheel& Wheel = WheelEntry.second;
			if (!Wheel.joint || !Wheel.joint->bActive)
				continue;

			if (ActiveWheelIndex % 2 == 0)
				Font->OutSet(Device.TargetHeight * (1.f + 0.25f * float(ActiveWheelIndex / 2)), 30.f);

			Font->SetColor(color_rgba(255, 0, 255, 255));
			Font->OutNext("[%s]", WheelKinematics->LL_BoneName_dbg(Wheel.bone_id));

			LPCSTR WheelStateName = "None";
			switch (Wheel.wheel_state)
			{
				case wheel_idle:	WheelStateName = "Idle";	break;
				case wheel_limit:	WheelStateName = "Limit";	break;
				case wheel_left:	WheelStateName = "Left";	break;
				case wheel_right:	WheelStateName = "Right";	break;
				case wheel_drive:	WheelStateName = "Drive";	break;
				case wheel_brake:	WheelStateName = "Brake";	break;
			}
			Font->SetColor(color_rgba(255, 255, 0, 255));
			Font->OutNext("WHEEL STATE: %s", WheelStateName);

			Font->SetColor(color_rgba(0, 255, 0, 255));
			auto BoneIt = bone_map.find(Wheel.bone_id);
			if (BoneIt != bone_map.end() && BoneIt->second.element)
			{
				Fvector LinearVelocity;
				Fvector AngularVelocityWheel;
				CPhysicsElement* Element = BoneIt->second.element;
				Element->get_AngularVel(AngularVelocityWheel);
				Font->OutNext("global_angular_velocity: %f", AngularVelocityWheel.magnitude());
				Element->get_LinearVel(LinearVelocity);
				Font->OutNext("global_linear_velocity:  %f", LinearVelocity.magnitude());
			}

			dJointID Joint = Wheel.joint->GetDJoint();
			Font->OutNext("torque_by_engine:       %f", Wheel.EngineTorque);
			Font->OutNext("applied_torque_limit:   %f", dJointGetHinge2Param(Joint, dParamFMax2));
			Font->OutNext("diff power:             %f", Wheel.wheel_diff_power);

			if (Wheel.collision_params.buxe)
				Font->SetColor(color_rgba(255, 0, 0, 255));
			if (Wheel.collision_params.breaking)
				Font->SetColor(color_rgba(255, 255, 0, 255));
			if (Wheel.collision_params.slide)
				Font->SetColor(color_rgba(0, 0, 255, 255));

			Font->OutNext("velocity_by_engine_max: %f", dJointGetHinge2Param(Joint, dParamVel2));
			Font->OutNext("velocity_current:       %f", dJointGetHinge2Angle2Rate(Joint));
			if (Wheel.max_control_velocity > 0.f)
				Font->OutNext("velocity_maximum:       %f", Wheel.max_control_velocity);

			if (Wheel.collision_params.slide || Wheel.collision_params.buxe || Wheel.collision_params.breaking)
				Font->SetColor(color_rgba(0, 255, 0, 255));

			Font->OutNext("steering_angle:         %f", dJointGetHinge2Angle1(Joint));
			Font->OutNext("steering_velocity:      %f", dJointGetHinge2Angle1Rate(Joint));
			Font->OutNext("steering_velocity_max:  %f", dJointGetHinge2Param(Joint, dParamVel));
			Font->OutNext("steering_torque:        %f", dJointGetHinge2Param(Joint, dParamFMax));
			Font->OutNext("breaking_torque:        %f", Wheel.BrakeTorque);

			float SpringFactor, DampingFactor;
			Wheel.joint->GetJointSDfactors(SpringFactor, DampingFactor);
			Font->OutNext("current_friction_factor: %f", Wheel.collision_params.current_friction_factor);
			Font->OutNext("current_friction2_factor:%f", Wheel.collision_params.current_friction2_factor);
			Font->OutNext("friction_slip_factor:   %f", Wheel.collision_params.friction_slip_factor);
			Font->OutNext("friction2_slip_factor:  %f", Wheel.collision_params.friction2_slip_factor);
			Font->OutNext("friction_rain_factor:   %f", Wheel.collision_params.friction_rain_factor);
			Font->OutNext("wheel_spring_factor:    %f", SpringFactor);
			Font->OutNext("wheel_damping_factor:   %f", DampingFactor);

			if (!Wheel.collision_params.traction)
				Font->SetColor(color_rgba(255, 0, 0, 255));
			Font->OutNext("traction");

			if (m_break_abs)
			{
				if (Wheel.collision_params.breaking_abs && b_breaks)
					Font->SetColor(color_rgba(255, 0, 0, 255));
				else
					Font->SetColor(color_rgba(0, 255, 0, 255));
				Font->OutNext("ABS");
			}

			++ActiveWheelIndex;
		}
	}
}

 void CCar::VisualUpdate(float fov)
{
	m_pPhysicsShell->InterpolateGlobalTransform(&XFORM());
	if (rs_car_info)
		DrawCarInfo();
	if (e_state_drive == drive && ((!IsTransmissionSwitching() || b_transmission_switch_drive) || brp))
	{
		m_engine_valve = 1;
		m_engine_drive = 1.f;
		m_car_sound->snd_valve.set_volume(0.f);
	}
	else
	{
		if (m_engine_valve_switch)
		{
			if (m_car_sound->snd_valve._p && m_engine_valve == 1 && !(fwp && bkp))
				m_car_sound->Valve();
			m_engine_valve = 0;
		}
		m_engine_drive = 0.f;
		m_valve_time = Device.fTimeGlobal;
	}
	if (e_state_drive == drive)
	{
		m_engine_drive_noswitch = 1.f;
	}
	else
	{
		if (!m_engine_valve_switch)
		{
			if (m_car_sound->snd_valve._p && m_engine_valve == 1 && !(fwp && bkp))
				m_car_sound->Valve();
			m_engine_valve = 0;
		}
		m_engine_drive_noswitch = 0.f;
		m_engine_drive_time = Device.fTimeGlobal;
	}

	float car_rpm = m_current_rpm;
	if (car_rpm < m_min_rpm)
		car_rpm = m_min_rpm;
	m_engine_rpm_scale = 1.f - (m_max_rpm - car_rpm) / (m_max_rpm - m_min_rpm); // 0-1 rpm
	clamp(m_engine_rpm_scale, 0.f, 1.f);

	if (Device.fTimeGlobal > m_transmission_switch_time + 0.4f * m_transmission_switch_delay && !m_transmission_sound_played)
	{
		m_car_sound->TransmissionSwitch();
		m_transmission_sound_played = true;
	}

	m_car_sound->Update();
	m_car_sound->Whine();
	if (!IsTransmissionSwitching() && m_pop_time + 0.15f < Device.fTimeGlobal && ((m_pop && e_state_drive == neutral && m_current_rpm > m_power_rpm * 0.8f && b_engine_on && !b_starting) || (m_pop && !m_pop_single && m_current_rpm >= m_max_rpm - (m_max_rpm * 0.02f) && e_state_drive == drive && m_transmission_stage == ETransmissionStage::Drive)))
	{
		if (Random.randF(EPS, 1.f) <= m_pop_probability && !m_pop_played)
		{
			PlayExhaustsPop();
			m_car_sound->PopSound();
			if (m_pop_single)
				m_pop_played = true;
		}
		m_pop_time = Device.fTimeGlobal;
	}
	else
		if (m_pop_time + 0.15f < Device.fTimeGlobal)
			m_pop_played = false;
	if(Owner())
	{
		Owner()->XFORM().mul_43	(XFORM(),m_sits_transforms);

		if(CurrentGameUI())//car panel ui
		{	
			if (rs_car_hud_draw)
			{
				if (active_camera->tag == EctFirstCam && m_firsteye_panel == false)
					CurrentGameUI()->UIMainIngameWnd->CarPanel().ShowPanel(false);
				else
					CurrentGameUI()->UIMainIngameWnd->CarPanel().ShowPanel(true);
				CurrentGameUI()->UIMainIngameWnd->CarPanel().Show(true);
				CurrentGameUI()->UIMainIngameWnd->CarPanel().SetCarHealth(GetfHealth()/* /100.f*/);
				CurrentGameUI()->UIMainIngameWnd->CarPanel().SetSpeed(GetVelocityM() * 3.6f / 115.f);
				if (b_engine_on)
					CurrentGameUI()->UIMainIngameWnd->CarPanel().SetRPM(m_current_rpm / m_max_rpm / 1.5f - m_min_rpm / m_max_rpm / 3.0f);
				else
					CurrentGameUI()->UIMainIngameWnd->CarPanel().SetRPM(0.f);
				CurrentGameUI()->UIMainIngameWnd->CarPanel().SetFuel(100.0f * (m_fuel / m_fuel_tank));
				if (CurrentTransmission() == 0)
					CurrentGameUI()->UIMainIngameWnd->CarPanel().SetGear("R");
				else
					CurrentGameUI()->UIMainIngameWnd->CarPanel().SetGear(std::to_string(CurrentTransmission()).c_str());
			}
			else
				CurrentGameUI()->UIMainIngameWnd->CarPanel().Show(false);
		}
	}

	UpdateExhausts	();
	m_lights.Update	();
	m_stoplights.Update	();
	m_indoorlights.Update	();
	m_reverselights.Update	();
	m_enginelights.Update	();
}

void CCar::renderable_Render()
{
	inherited::renderable_Render			();
	for (CCarTurret* CWPB : m_car_weapons)
	{
		CWPB->Render_internal();
	}

	//for (; i_w != e_w; ++i_w)
	//
	//
	//	dVector3 vec1;
	//	dVector3 vec2;
	//
	//
	//	Level().draw_line(wheel_xform.c, pos_z, color_rgba(0, 0, 255, 255));//z
	//	Level().draw_line(wheel_xform.c, pos_x, color_rgba(255, 0, 0, 255));//x
	//	Level().draw_line(wheel_xform.c, pos_y, color_rgba(0, 255, 0, 255));//y

		// TRIANGLE PICKER
		//if(!Actor())
		//
		//
		//if(IRenderVisual* V = Visual())
		//	if(IKinematics *K = V->dcast_PKinematics())
		//		if(cam_pos.distance_to(Position()) <= Radius()*2)
		//			for (auto I=K->LL_Bones()->begin(); I != K->LL_Bones()->end(); I++)
		//				if(!K->LL_GetBoneVisible(I->second))
		//
		//
		//
		//				if(K->PickBone(XFORM(), res, Radius()*2, cam_pos, cam_dir, I->second, false))
		//
		//					if(!diff_cam.similar(cam_dir, EPS_L))
		//
}

float CCar::InterpolateStates(u32 element, SCarNetUpdate const& first, SCarNetUpdate const& last, SPHNetState& current)
{
	u32 CurTime = Device.dwTimeGlobal;

	const u32 denom = last.TimeStamp - first.TimeStamp;
	if (denom == 0)
	{
		const Fvector& pos2 = last.StateVec[element].position;
		current.position = pos2;
		current.quaternion = last.StateVec[element].quaternion;
		return 1.f;
	}

	float factor = float(CurTime - first.TimeStamp) / float(denom);
	float result = factor;
	clamp(factor, 0.f, 1.f);

	const Fvector& pos1 = first.StateVec[element].position;
	const Fvector& pos2 = last.StateVec[element].position;

	current.position.x = pos1.x + (factor * (pos2.x - pos1.x));
	current.position.y = pos1.y + (factor * (pos2.y - pos1.y));
	current.position.z = pos1.z + (factor * (pos2.z - pos1.z));

	current.quaternion.slerp(first.StateVec[element].quaternion, last.StateVec[element].quaternion, factor);
	return result;
}

void CCar::Interpolate()
{
	if (!getVisible() || !m_pPhysicsShell || m_CarNetUpdates.empty())
		return;

	if (m_CarNetUpdates.size() >= 2)
	{
		auto& first = m_CarNetUpdates.front();
		auto& last = m_CarNetUpdates.back();
		u32 elementsCnt = first.StateVec.size();

		for (u32 i = 0; i < elementsCnt; i++)
		{
			SPHNetState newState = m_CarNetUpdates.back().StateVec[i];
			float f = InterpolateStates(i, first, last, newState);

			if (f == 0)
				return;

			CPHSynchronize* pSyncObj = this->PHGetSyncItem(i);
			SPHNetState oldState;
			pSyncObj->get_State(oldState);

			newState.previous_position = oldState.position;
			newState.previous_quaternion = oldState.quaternion;
			pSyncObj->set_State(newState);
		}
	}

	m_pPhysicsShell->InterpolateGlobalTransform(&XFORM());
}

void CCar::SyncRead(NET_Packet& P)
{
	u8 flags = P.r_u8();
	std::bitset<8> BitsIn(flags);

	u8 engine = BitsIn.test(0);
	u8 light = BitsIn.test(1);

	ALife::_OBJECT_ID owner;
	P >> owner;
	m_current_transmission_num = P.r_u32();

	if (owner != ALife::INVALID_OBJECT_ID)
	{
		if (!(IsMyCar()))
		{
			CObject* finded_object = Level().Objects.net_Find(owner);
			if (CActor* act = finded_object != nullptr ? finded_object->cast_actor() : nullptr)
			{
				act->attach_Vehicle(this);
			}
		}
	}
	else
	{
		if (OwnerActor())
			detach_Actor();
	}

	if (!IsMyCar())
	{
		bool e = !!engine;

		if (e != b_engine_on)
			SwitchEngine();

		bool l = !!light;

		if (l != m_lights.IsOn())
			m_lights.SwitchHeadLights();
	}

	u16 cnt;
	P.r_u16(cnt);

	SCarNetUpdate update;
	update.TimeStamp = Device.dwTimeGlobal;

	for (int i = 0; i < cnt; i++)
	{
		SPHNetState state;

		P.r_vec3(state.position);
		P.r_float_q8(state.quaternion.x, -1.0, 1.0);
		P.r_float_q8(state.quaternion.y, -1.0, 1.0);
		P.r_float_q8(state.quaternion.z, -1.0, 1.0);
		P.r_float_q8(state.quaternion.w, -1.0, 1.0);

		update.StateVec.push_back(state);
	}

	if (!IsMyCar())
		m_CarNetUpdates.push_back(update);

	while (m_CarNetUpdates.size() > 2)
		m_CarNetUpdates.pop_front();
}

void CCar::SyncWrite(NET_Packet& P)
{
	std::bitset<8> BitsOut;
	BitsOut.set(0, b_engine_on);
	BitsOut.set(1, m_lights.IsOn());

	P.w_u8(static_cast<u8>(BitsOut.to_ulong()));

	if (OwnerActor())
		P << OwnerActor()->ID();
	else
		P << ALife::INVALID_OBJECT_ID;

	P.w_u32((u32)m_current_transmission_num);

	if (OnClient() || m_CarNetUpdates.empty())
	{
		P.w_u16(PHGetSyncItemsNumber());

		for (int i = 0; i < PHGetSyncItemsNumber(); i++)
		{
			auto item = PHGetSyncItem(i);

			SPHNetState State;
			item->get_State(State);

			P.w_vec3(State.position);
			P.w_float_q8(State.quaternion.x, -1.0, 1.0);
			P.w_float_q8(State.quaternion.y, -1.0, 1.0);
			P.w_float_q8(State.quaternion.z, -1.0, 1.0);
			P.w_float_q8(State.quaternion.w, -1.0, 1.0);
		}
	}
	else
	{
		auto& vec = m_CarNetUpdates.back().StateVec;
		P.w_u16(vec.size());

		for (int i = 0; i < vec.size(); i++)
		{
			SPHNetState State = vec[i];

			P.w_vec3(State.position);
			P.w_float_q8(State.quaternion.x, -1.0, 1.0);
			P.w_float_q8(State.quaternion.y, -1.0, 1.0);
			P.w_float_q8(State.quaternion.z, -1.0, 1.0);
			P.w_float_q8(State.quaternion.w, -1.0, 1.0);
		}
	}
}

void	CCar::net_Export			(NET_Packet& P)
{
	inherited::net_Export(P);
}

void	CCar::net_Import			(NET_Packet& P)
{
	inherited::net_Import(P);
}

void	CCar::OnHUDDraw				(CCustomHUD* /**hud/**/)
{
#ifdef DEBUG
	Fvector velocity;
	m_pPhysicsShell->get_LinearVel(velocity);
	UI().Font().pFontStat->SetColor		(0xffffffff);
	UI().Font().pFontStat->OutSet		(120,530);
	UI().Font().pFontStat->OutNext		("Position:      [%3.2f, %3.2f, %3.2f]",VPUSH(Position()));
	UI().Font().pFontStat->OutNext		("Velocity:      [%3.2f]",velocity.magnitude());

#endif
}

//void CCar::Hit(float P,Fvector &dir,CObject * who,s16 element,Fvector p_in_object_space, float impulse, ALife::EHitType hit_type)
void	CCar::Hit							(SHit* pHDS)
{
	if (OwnerActor() && rs_car_god)
		return;

	//if(CDelayedActionFuse::isActive()||Initiator()==u16(-1)&&HDS.hit_type==ALife::eHitTypeStrike)

	//if(HDS.who->ID()!=ID())
	WheelHit(pHDS->damage(),pHDS->bone(),pHDS->hit_type);
	DoorHit(pHDS->damage(),pHDS->bone(),pHDS->hit_type);
	float hitScale=1.f,woundScale=1.f;
	if(pHDS->hit_type!=ALife::eHitTypeStrike) if (TDamageManager* DmgManager = GetComponent<TDamageManager>()) DmgManager->HitScale(pHDS->bone(), hitScale, woundScale);
	pHDS->power *= GetHitImmunity(pHDS->hit_type) * hitScale;

	// LA: strong physical hits stall a running engine; anomaly zones use HitEngine instead.
	if (m_hit_stall_threshold && pHDS->damage() > m_hit_stall_threshold && b_engine_on &&
		(pHDS->who == nullptr || !smart_cast<CAnomalyZone*>(pHDS->who)))
	{
		Stall();
	}

	inherited::Hit(pHDS);
	if(!CDelayedActionFuse::isActive())
	{
		CDelayedActionFuse::CheckCondition(GetfHealth());
	}
	CDamagableItem::HitEffect();
	if(Owner()&&Owner()->ID()==Level().CurrentEntity()->ID())
		CurrentGameUI()->UIMainIngameWnd->CarPanel().SetCarHealth(GetfHealth()/* /100.f */);
}

void CCar::ChangeCondition	(float fDeltaCondition)	
{
	if (OwnerActor() && rs_car_god)
		return;

	CEntity::CalcCondition(-fDeltaCondition);
	CDamagableItem::HitEffect();
	UpdateDamageEffects();
	if (Local() && !g_Alive() && !AlreadyDie())
		KillEntity	(Initiator());
	if(Owner()&&Owner()->ID()==Level().CurrentEntity()->ID())
		CurrentGameUI()->UIMainIngameWnd->CarPanel().SetCarHealth(GetfHealth()/* /100.f */);
}
void CCar::PHHit(float P,Fvector &dir, CObject *who,s16 element,Fvector p_in_object_space, float impulse, ALife::EHitType hit_type)
{
	if(!m_pPhysicsShell)	return;

	if (OwnerActor() && rs_car_god)
		return;

	
	for (u16 SteerBone : m_steers)
	{
		if (SteerBone == element)
			return;
	}

	if (m_hit_stall_threshold && P > m_hit_stall_threshold && b_engine_on && (!who || (who && !smart_cast<CAnomalyZone*>(who))))
		Stall();

	if(CPHUpdateObject::IsActive())
	{
		Fvector vimpulse;vimpulse.set(dir);
		vimpulse.mul(impulse);
		vimpulse.y *=GravityFactorImpulse();
		float mag=vimpulse.magnitude();
		if(!fis_zero(mag))
		{
			 vimpulse.mul(1.f/mag);
			 m_pPhysicsShell->applyHit(p_in_object_space,vimpulse,mag,element,hit_type);
		}

		if (P >= 0.0011f && who)
		{
			IKinematics* K = PKinematics(Visual());
			IKinematics* K_who = PKinematics(who->Visual());
			if (K && K_who)
			{
				SGameMtlPair* mtl_pair = GMLib.GetMaterialPair(K_who->LL_GetData(K_who->LL_GetBoneRoot()).game_mtl_idx, K->LL_GetData(element).game_mtl_idx);
				if (mtl_pair)
				{
					Fvector collide_pos;
					collide_pos.set(XFORM().c);
					collide_pos.add(p_in_object_space);
					Fmatrix pos;
					pos.k.set(dir);
					Fvector::generate_orthonormal_basis(pos.k, pos.j, pos.i);
					pos.c.set(collide_pos);
					if (!mtl_pair->CollideSounds.empty())
					{
						ref_sound* pSound = &mtl_pair->CollideSounds[::Random.randI(0, mtl_pair->CollideSounds.size())];
						if (pSound && pSound->_p)
						{
							float volume = 0.5f+ P;
							float freq = ::Random.randF(0.9f, 1.1f);
							pSound->play_no_feedback(0, 0, 0, &pos.c, &volume, &freq);
						}
					}
					if (P >= 0.011f)
					{
						if (!mtl_pair->CollideParticles.empty())
							Level().ph_commander().add_call(new CPHOnesCondition(), new CCarParticlesPlayCall(pos, zero_vel, *mtl_pair->CollideParticles[::Random.randI(0, mtl_pair->CollideParticles.size())]));
						else
							Level().ph_commander().add_call(new CPHOnesCondition(), new CCarParticlesPlayCall(pos, zero_vel, "phy_fx\\generic_body_dust_01"));
					}
				}
			}
		}
	} else
	{
		m_pPhysicsShell->applyHit(p_in_object_space,dir,impulse,element,hit_type);
	}
}

void CCar::ApplyDamage(u16 level)
{
	if (level == m_level_applied) return;

	if (level > m_level_applied)
	{
		switch(level)
		{
			case 1: m_damage_particles.Play1(this);break;
			case 2: 
				{
					if(!CDelayedActionFuse::isActive())
					{
						CDelayedActionFuse::CheckCondition(GetfHealth());
					}
					m_damage_particles.Play2(this);
				}
												   break;
			case 3: m_fuel=0.f;
				
		}
	}
	else
	{
		if (level < 2)
		{
			m_damage_particles.Stop2(this);
			CDelayedActionFuse::Reset();
		}
		if (level < 1)
		{
			m_damage_particles.Stop1(this);
		}
	}

	CDamagableItem::ApplyDamage(level);
}

void CCar::UpdateDamageEffects()
{
	const u16 level = DamageLevel();
	if (level < m_level_applied)
	{
		ApplyDamage(level);
	}
}

void CCar::RepairDamageItems()
{
	for (auto& door : m_doors)
	{
		door.second.Repair();
	}
	for (auto& wheel : m_wheels_map)
	{
		wheel.second.Repair();
	}
}

float CCar::SetfHealth(float value)
{
	float result = CEntity::SetfHealth(value);
	CDamagableItem::HitEffect();
	UpdateDamageEffects();
	return result;
}

void CCar::detach_Actor()
{
	if(!Owner()) return;
	Owner()->setVisible(1);
	if (OwnerActor())
	{
		OwnerActor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, false);
		}
	CHolderCustom::detach_Actor();
	PPhysicsShell()->remove_ObjectContactCallback(ActorObstacleCallback);
	NeutralDrive();
	Unclutch();
	ResetKeys();
	CurrentGameUI()->UIMainIngameWnd->CarPanel().Show(false);
	PressBreaks();
	for (CCarTurret* CWPB : m_car_weapons)
	{
		CWPB->Action(CCarTurret::eWpnActivate, 0);
	}

	processing_deactivate();
	saved_camera = ECarCamType(active_camera->tag);
#ifdef DEBUG
	DBgClearPlots();
#endif
	b_event_exit_car = false;
	m_exit_car_time = 0;
	m_exit_door_bone = BI_NONE;
}

bool CCar::attach_Actor(CGameObject* actor)
{
	b_event_exit_car = false;
	m_exit_car_time = 0;
	m_exit_door_bone = BI_NONE;
	m_dwAttachTime = Device.dwTimeGlobal;
	if(CPHDestroyable::Destroyed()) return false;
	CHolderCustom::attach_Actor(actor);
	Owner()->setVisible(0);
	actor->XFORM().mul_43	(XFORM(),m_sits_transforms);
	OnCameraChange(saved_camera);
	PPhysicsShell()->Enable();
	PPhysicsShell()->add_ObjectContactCallback(ActorObstacleCallback);
	processing_activate();
	ReleaseBreaks();
	StopBreaking();
	OwnerActor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, true);
	return true;
}

/*
void CCar::TeleportCarAndDriver(Fvector3 pos)
{

	StopEngine();
	HandBreak();

	renderable.xform.c = pos;

	m_pPhysicsShell->Activate(false);
	m_pPhysicsShell->mXFORM.set(XFORM());
	m_pPhysicsShell->Activate(true);

	IKinematics* K = smart_cast<IKinematics*>(Visual());
	CInifile* ini = K->LL_UserData();
	int id;
	Fmatrix	driver_xform;
	if (ini->line_exist("car_definition", "driver_position") && ini->line_exist("car_definition", "driver_direction"))
	{
		driver_xform.c.set(ini->r_fvector3("car_definition", "driver_position"));
		driver_xform.k.set(ini->r_fvector3("car_definition", "driver_direction"));
	}
	else if (ini->line_exist("car_definition", "driver_place")) {

		id = K->LL_BoneID(ini->r_string("car_definition", "driver_place"));
		CBoneInstance& instance = K->LL_GetBoneInstance(u16(id));
		driver_xform.set(instance.mTransform);
	}
	else {
		id = K->LL_GetBoneRoot();
		CBoneInstance& instance = K->LL_GetBoneInstance(u16(id));
		driver_xform.set(instance.mTransform);
	}

	Owner()->setVisible(0);
	m_sits_transforms.set(driver_xform);
	Actor()->XFORM().mul_43(XFORM(), m_sits_transforms);
	
}
*/
CCar::SDoor* CCar::Get_Door(u16 id, bool all)
{
	auto i = m_doors.find(id);
	if (i != m_doors.end())
	{
		if(all)
			return &i->second;

		if(i->second.joint)
			return &i->second;
	}

	return NULL;
}


bool CCar::is_Door(u16 id,xr_map<u16,SDoor>::iterator& i)
{
	i	= m_doors.find(id);
	if (i == m_doors.end()) 
	{
		return false;
	}
	else
	{
		if(i->second.joint)//temp for fake doors
			return true;
		else
			return false;
	}
}
bool CCar::is_Door(u16 id)
{
	xr_map<u16,SDoor>::iterator i;
	i	= m_doors.find(id);
	if (i == m_doors.end()) 
	{
		return false;
	}
	return true;
}

bool CCar::Enter(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos)
{
	Fvector enter_pos;
	enter_pos.add(pos,foot_pos);
	enter_pos.mul(0.5f);
	for(auto i=m_doors.begin();i!=m_doors.end();++i)
	{
		if(i->second.CanEnter(pos,dir,enter_pos) && i->second.IsDistOfSight(pos))
			return true;
	}
	return false;
}

bool CCar::Exit(const Fvector& pos,const Fvector& dir)
{
	for (auto& Door : m_doors)
	{
		if (Door.second.CanExit(pos, dir))
		{
			Door.second.GetExitPosition(m_exit_position);
			return true;
		}
	}
	return false;

}


void CCar::DoExit()
{
	CActor* A=OwnerActor();
	if(A)
	{
		if(!m_doors.empty())m_doors.begin()->second.GetExitPosition(m_exit_position);
		else m_exit_position.set(Position());
		A->detach_Vehicle();

		if (CurrentGameUI()->UIMainIngameWnd->CarPanel().IsShown())
			CurrentGameUI()->UIMainIngameWnd->CarPanel().Show(false);
	}
}
static void loadcamera(LPCSTR section, CInifile *ini, CCameraBase* cam)
{
	if(!ini->section_exist(section)) return;

	if(ini->line_exist(section,"rot_speed"))
		cam->rot_speed			= ini->r_fvector3(section,"rot_speed");
	if(ini->line_exist(section,"lim_yaw"))
		cam->lim_yaw			= ini->r_fvector2(section,"lim_yaw");
	if(ini->line_exist(section,"lim_pitch"))
		cam->lim_pitch			= ini->r_fvector2(section,"lim_pitch");

	CCameraLook *llcam = smart_cast<CCameraLook*>(cam);
	if (llcam)
	{
		if(ini->line_exist(section,"lim_zoom"))
		{
			llcam->SetZoomLimits(ini->r_fvector2(section,"lim_zoom"));
		}
	}

	cam->bClampPitch		= (0!=cam->lim_pitch[0])||(0!=cam->lim_pitch[1]);
	cam->bClampYaw			= (0!=cam->lim_yaw[0])||(0!=cam->lim_yaw[1]);

	if (cam->bClampPitch)	cam->pitch = (cam->lim_pitch[0]+cam->lim_pitch[1])*0.5f;
	if (cam->bClampYaw)		cam->yaw	  = (cam->lim_yaw[0]+cam->lim_yaw[1])*0.5f;
}
void CCar::ParseDefinitions()
{
	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	CInifile* ini = pKinematics->LL_UserData();
	if (ini)
	{
		LoadExplosionAndDriverDefinitions(ini, pKinematics);
		LoadHandlingAndCameraDefinitions(ini);
		LoadEngineDefinitions(ini);
		LoadSoundDefinitions(ini);
		LoadTransmissionDefinitions(ini);
		LoadVisualDefinitions(ini);
	}
}

void CCar::LoadExplosionAndDriverDefinitions(CInifile* ini, IKinematics* pKinematics)
{
	if (ini->section_exist("explosion"))
		CExplosive::Load(ini, "explosion");

	m_sits_transforms.set(pKinematics->LL_GetTransform(pKinematics->LL_BoneID(READ_IF_EXISTS(ini, r_string, "car_definition", "driver_place", pKinematics->LL_BoneName_dbg(pKinematics->LL_GetBoneRoot())))));

	Fmatrix	driver_xform_tune;
	Fvector driver_direction = (READ_IF_EXISTS(ini, r_fvector3, "car_definition", "driver_direction", Fvector().set(0.0f, 0.0f, 0.0f)));
	driver_xform_tune.setXYZ(deg2rad(driver_direction.x), deg2rad(driver_direction.y), deg2rad(driver_direction.z));
	driver_xform_tune.c.set(READ_IF_EXISTS(ini, r_fvector3, "car_definition", "driver_position", Fvector().set(0.0f, 0.0f, 0.0f)));
	m_sits_transforms.mulB_43(driver_xform_tune);

	// Original vehicles use the single camera_pos key. Cars-Ext splits it
	// into first/lookat/free positions; keep the original key as fallback.
	const Fvector DefaultCameraPos = READ_IF_EXISTS(ini, r_fvector3, "car_definition", "camera_pos", Fvector().set(0.0f, 0.0f, 0.0f));
	m_camera_position[EctFirstCam] = READ_IF_EXISTS(ini, r_fvector3, "car_definition", "camera_pos_firsteye", DefaultCameraPos);
	m_camera_position[EctSecondCam] = READ_IF_EXISTS(ini, r_fvector3, "car_definition", "camera_pos_lookat", m_camera_position[EctFirstCam]);
	m_camera_position[EctThridCam] = READ_IF_EXISTS(ini, r_fvector3, "car_definition", "camera_pos_free", m_camera_position[EctSecondCam]);
	m_camera_position[EctFourthCam] = READ_IF_EXISTS(ini, r_fvector3, "car_definition", "camera_pos_free2", m_camera_position[EctThridCam]);

	m_camera_inertion_n = READ_IF_EXISTS(ini, r_float, "car_definition", "camera_inertion_n", m_camera_inertion_n);
	m_camera_inertion_k = READ_IF_EXISTS(ini, r_float, "car_definition", "camera_inertion_k", m_camera_inertion_k);
	m_camera_angle_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "camera_angle_factor", m_camera_angle_factor);
	clamp(m_camera_angle_factor, 0.1f, 2.f);
}

void CCar::LoadHandlingAndCameraDefinitions(CInifile* ini)
{
	m_current_rpm_brp_n_max = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "handbrake_rpm_threshold", m_current_rpm_brp_n_max);
	m_current_rpm_brp_decrease = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "handbrake_rpm_decrease", m_current_rpm_brp_decrease);
	m_slip_sliding_threshold = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "slip_sliding_threshold", m_slip_sliding_threshold);
	m_vel_sliding_threshold = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "vel_sliding_threshold", m_vel_sliding_threshold);
	m_friction_slip_factor = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction_slip_factor", m_friction_slip_factor);
	m_friction2_slip_factor = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction2_slip_factor", m_friction2_slip_factor);
	m_friction_spin_factor = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction_spin_factor", m_friction_spin_factor);
	m_friction2_spin_factor = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction2_spin_factor", m_friction2_spin_factor);
	m_friction_slip_speed = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction_slip_speed", m_friction_slip_speed);
	m_friction2_steering_wheels = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction2_steering_wheels", m_friction2_steering_wheels);
	m_friction2_nonsteering_wheels = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction2_nonsteering_wheels", m_friction2_nonsteering_wheels);
	m_friction2_slip_factor = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction2_slip_factor", m_friction2_slip_factor);
	m_friction2_brp_boost = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "friction2_brp_boost", m_friction2_brp_boost);
	m_drift_start_boost = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "drift_start_boost", m_drift_start_boost);
	m_drift_boost_time = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "drift_boost_time", m_drift_boost_time);
	m_steer_spin_boost = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "steer_spin_boost", m_steer_spin_boost);
	m_steer_speed_boost = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "steer_speed_boost", m_steer_speed_boost);
	m_steer_turn_factor = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "steer_turn_factor", m_steer_turn_factor);
	m_steer_counter_turn_factor = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "steer_counter_turn_factor", m_steer_counter_turn_factor);
	m_steer_turn_compensation = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "steer_turn_compensation", m_steer_turn_compensation);
	m_steer_turn_compensation_max = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "steer_turn_compensation_max", m_steer_turn_compensation_max);
	m_steer_turn_delta = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "steer_turn_delta", m_steer_turn_delta);
	m_brake_boost = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "brake_boost", m_brake_boost);
	m_switch_shake_boost = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "switch_shake_boost", m_switch_shake_boost);
	m_engine_shake_boost = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "engine_shake_boost", m_engine_shake_boost);
	m_wheels_spin_factor_start = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "wheels_spin_factor_start", m_wheels_spin_factor_start);
	m_wheels_spin_factor_stop = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "wheels_spin_factor_stop", m_wheels_spin_factor_stop);
	m_wheels_sound_delay = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "wheels_sound_delay", m_wheels_sound_delay);
	m_firsteye_panel = READ_IF_EXISTS(pSettings, r_bool, cNameSect_str(), "firsteye_panel", m_firsteye_panel);
	m_allow_inventory_control = READ_IF_EXISTS(pSettings, r_bool, cNameSect_str(), "allow_inventory_control", m_allow_inventory_control);
	m_interior_volume = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "interior_volume", m_interior_volume);
	m_hit_stall_threshold = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "hit_stall_threshold", m_hit_stall_threshold);
	m_hit_anomaly_scale = READ_IF_EXISTS(pSettings, r_float, cNameSect_str(), "hit_anomaly_scale", m_hit_anomaly_scale);

	loadcamera("car_firsteye_cam", ini, camera[EctFirstCam]);
	loadcamera("car_look_cam", ini, camera[EctSecondCam]);
	m_roll_lock_second = READ_IF_EXISTS(ini, r_bool, "car_look_cam", "roll_lock", false);
	loadcamera("car_free_cam", ini, camera[EctThridCam]);
	loadcamera("car_free_cam", ini, camera[EctFourthCam]);
}

void CCar::LoadEngineDefinitions(CInifile* ini)
{
	if (ini->line_exist("car_definition", "engine_power"))
	{
		m_max_power = ini->r_float("car_definition", "engine_power");
		m_max_power *= (0.8f * 1000.f);
	}
	if (ini->line_exist("car_definition", "engine_power_reverse_factor"))
	{
		m_power_reverse_factor = ini->r_float("car_definition", "engine_power_reverse_factor");
	}

	if (ini->line_exist("car_definition", "max_engine_rpm"))
	{
		m_max_rpm = ini->r_float("car_definition", "max_engine_rpm");
		m_max_rpm *= (1.f / 60.f * 2.f * M_PI);
	}

	if (ini->line_exist("car_definition", "idling_engine_rpm"))
	{
		m_min_rpm = ini->r_float("car_definition", "idling_engine_rpm");
		m_min_rpm *= (1.f / 60.f * 2.f * M_PI);
	}

	if (ini->line_exist("car_definition", "max_power_rpm"))
	{
		m_power_rpm = ini->r_float("car_definition", "max_power_rpm");
		m_power_rpm *= (1.f / 60.f * 2.f * M_PI);
	}

	if (ini->line_exist("car_definition", "max_torque_rpm"))
	{
		m_torque_rpm = ini->r_float("car_definition", "max_torque_rpm");
		m_torque_rpm *= (1.f / 60.f * 2.f * M_PI);
	}

	if (ini->line_exist("car_definition", "sound_rpm"))
	{
		m_sound_rpm = ini->r_float("car_definition", "sound_rpm");
		m_sound_rpm *= (1.f / 60.f * 2.f * M_PI);
	}

	m_power_increment_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "power_increment_factor", m_power_increment_factor);
	m_power_decrement_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "power_decrement_factor", m_power_decrement_factor);
	m_rpm_increment_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "rpm_increment_factor", m_rpm_increment_factor);
	m_rpm_decrement_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "rpm_decrement_factor", m_rpm_decrement_factor);
	m_rpm_brp_increment_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "rpm_brp_increment_factor", m_rpm_brp_increment_factor);
	m_rpm_brp_decrement_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "rpm_brp_decrement_factor", m_rpm_brp_decrement_factor);

	m_power_neutral_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "power_neutral_factor", m_power_neutral_factor);
	clamp(m_power_neutral_factor, 0.0f, 1.0f);

	m_nitro = READ_IF_EXISTS(ini, r_bool, "car_definition", "nitro", m_nitro);
	m_nitro_power_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "nitro_power_factor", m_nitro_power_factor);
	m_nitro_fuel_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "nitro_fuel_factor", m_nitro_fuel_factor);
	m_nitro_friction_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "nitro_friction_factor", m_nitro_friction_factor);
	m_nitro_wheelie_boost = READ_IF_EXISTS(ini, r_float, "car_definition", "nitro_wheelie_boost", m_nitro_wheelie_boost);
	m_wheelie_boost = READ_IF_EXISTS(ini, r_float, "car_definition", "wheelie_boost", m_wheelie_boost);
	m_nitro_wheelie_friction_brp_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "nitro_wheelie_friction_brp_factor", m_nitro_wheelie_friction_brp_factor);
	m_nitro_particle_scale = READ_IF_EXISTS(ini, r_float, "car_definition", "nitro_particle_scale", m_nitro_particle_scale);
	b_nitro_particles_enabled = READ_IF_EXISTS(ini, r_bool, "car_definition", "nitro_particles_enabled", b_nitro_particles_enabled);
}

void CCar::LoadSoundDefinitions(CInifile* ini)
{
	m_engine_sound_scale_max = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_sound_scale_max", m_engine_sound_scale_max);
	m_engine_sound_scale_min = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_sound_scale_min", m_engine_sound_scale_min);
	m_engine_sound_scale_factor = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_sound_scale_factor", m_engine_sound_scale_factor);
	m_engine_idle_volume = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_idle_volume", m_engine_idle_volume);
	m_sound_state_change_up = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_state_change_up", m_sound_state_change_up);
	m_sound_state_change_down = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_state_change_down", m_sound_state_change_down);
	m_sound_idle_change_up = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_idle_change_up", m_sound_idle_change_up);
	m_sound_idle_change_down = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_idle_change_down", m_sound_idle_change_down);
	m_sound_state_switch_factor = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_state_switch_factor", m_sound_state_switch_factor);
	m_sound_state_after_switch = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_state_after_switch", m_sound_state_after_switch);
	m_sound_switch_effect_type = READ_IF_EXISTS(ini, r_u32, "car_sound", "sound_switch_effect_type", m_sound_switch_effect_type);
	m_sound_switch_effect_power = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_switch_effect_power", m_sound_switch_effect_power);
	m_sound_switch_effect_speed = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_switch_effect_speed", m_sound_switch_effect_speed);
	m_sound_switch_effect_duration = READ_IF_EXISTS(ini, r_float, "car_sound", "sound_switch_effect_duration", m_sound_switch_effect_duration);
	m_turbo_scale_max = READ_IF_EXISTS(ini, r_float, "car_sound", "turbo_scale_max", m_turbo_scale_max);
	m_turbo_scale_min = READ_IF_EXISTS(ini, r_float, "car_sound", "turbo_scale_min", m_turbo_scale_min);
	m_turbo_scale_speed = READ_IF_EXISTS(ini, r_float, "car_sound", "turbo_scale_speed", m_turbo_scale_speed);
	m_valve_switch_volume = READ_IF_EXISTS(ini, r_float, "car_sound", "valve_switch_volume", m_valve_switch_volume);
	m_blower_scale_min = READ_IF_EXISTS(ini, r_float, "car_sound", "blower_scale_min", m_blower_scale_min);
	m_blower_rpm_scale = READ_IF_EXISTS(ini, r_float, "car_sound", "blower_rpm_scale", m_blower_rpm_scale);
	m_blower_volume_min = READ_IF_EXISTS(ini, r_float, "car_sound", "blower_volume_min", m_blower_volume_min);
	m_blower_inertia = READ_IF_EXISTS(ini, r_float, "car_sound", "blower_inertia", m_blower_inertia);
	m_blower_sound_forward = READ_IF_EXISTS(ini, r_float, "car_sound", "blower_sound_forward", m_blower_sound_forward);
	m_engine_state_scale_min = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_state_scale_min", m_engine_state_scale_min);
	m_engine_heavy_constant = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_heavy_constant", m_engine_heavy_constant);
	m_engine_heavy_rpm = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_heavy_rpm", m_engine_heavy_rpm);
	m_engine_heavy_switch = READ_IF_EXISTS(ini, r_float, "car_sound", "engine_heavy_switch", m_engine_heavy_switch);
	m_whine_speed = READ_IF_EXISTS(ini, r_float, "car_sound", "whine_speed", m_whine_speed);
	m_whine_exterior_volume = READ_IF_EXISTS(ini, r_float, "car_sound", "whine_exterior_volume", m_whine_exterior_volume);
	m_whine_reverse = READ_IF_EXISTS(ini, r_bool, "car_sound", "whine_reverse", m_whine_reverse);
	m_enhanced_sound = READ_IF_EXISTS(ini, r_bool, "car_sound", "enhanced_sound", m_enhanced_sound);

	// An empty exhaust_particles value returns nullptr, and a missing effect
	// name would assert inside the renderer. Keep the stock effect in both
	// cases so a bad car config cannot crash the level load.
	const char* ExhaustParticles = READ_IF_EXISTS(ini, r_string, "car_definition", "exhaust_particles", DefaultExhaustParticles);
	if (ExhaustParticles != nullptr && ExhaustParticles[0] != '\0' && ParticleEffectExists(ExhaustParticles))
	{
		m_exhaust_particles = ExhaustParticles;
	}
	else
	{
		if (ExhaustParticles != nullptr && ExhaustParticles[0] != '\0')
		{
			Msg("! CCar [%s]: exhaust_particles effect '%s' does not exist, using default '%s'",
				cNameSect().c_str(), ExhaustParticles, DefaultExhaustParticles);
		}

		if (ParticleEffectExists(DefaultExhaustParticles))
		{
			m_exhaust_particles = DefaultExhaustParticles;
		}
		else
		{
			Msg("! CCar [%s]: default exhaust_particles effect '%s' does not exist, exhaust disabled",
				cNameSect().c_str(), DefaultExhaustParticles);
			m_exhaust_particles = "";
		}
	}
	if (ini->line_exist("car_definition", "nitro_particles"))
		m_nitro_particles = ini->r_string("car_definition", "nitro_particles");
	if (ini->line_exist("car_definition", "pop_particles"))
	{
		m_pop = true;
		m_pop_particles = ini->r_string("car_definition", "pop_particles");
	}
	m_pop_probability = READ_IF_EXISTS(ini, r_float, "car_definition", "pop_probability", m_pop_probability);
	m_pop_single = READ_IF_EXISTS(ini, r_bool, "car_definition", "pop_single", m_pop_single);
}

void CCar::LoadTransmissionDefinitions(CInifile* ini)
{
	b_auto_switch_transmission = !!ini->r_bool("car_definition", "auto_transmission");
	b_cvt_transmission = READ_IF_EXISTS(ini, r_bool, "car_definition", "cvt_transmission", b_cvt_transmission);
	m_transmission_switch_delay = READ_IF_EXISTS(ini, r_float, "car_definition", "transmission_switch_delay", m_transmission_switch_delay);
	m_transmission_switch_power_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "transmission_switch_power_factor", m_transmission_switch_power_factor);
	clamp(m_transmission_switch_power_factor, 0.f, 1.f);
	m_transmission_switch_volume = READ_IF_EXISTS(ini, r_float, "car_definition", "transmission_switch_volume", m_transmission_switch_volume);
	b_transmission_switch_drive = READ_IF_EXISTS(ini, r_bool, "car_definition", "transmission_switch_drive", b_transmission_switch_drive);

	InitParabola();

	m_axle_friction = READ_IF_EXISTS(ini, r_float, "car_definition", "axle_friction", 1.0f);
	m_steering_speed = READ_IF_EXISTS(ini, r_float, "car_definition", "steering_speed", 1.0f);

	if (ini->line_exist("car_definition", "break_time"))
		m_break_time = ini->r_float("car_definition", "break_time");

	m_break_abs = READ_IF_EXISTS(ini, r_bool, "car_definition", "break_abs", m_break_abs);
	m_break_abs_power = READ_IF_EXISTS(ini, r_float, "car_definition", "break_abs_power", m_break_abs_power);
	clamp(m_break_abs_power, 0.f, 1.f);
	m_break_abs_individual = READ_IF_EXISTS(ini, r_bool, "car_definition", "break_abs_individual", m_break_abs_individual);
	m_break_abs_threshold = READ_IF_EXISTS(ini, r_float, "car_definition", "break_abs_threshold", m_break_abs_threshold);
	m_steer_nonlinear = READ_IF_EXISTS(ini, r_bool, "car_definition", "steer_nonlinear", m_steer_nonlinear);
	m_steer_invert = READ_IF_EXISTS(ini, r_bool, "car_definition", "steer_invert", m_steer_invert);
	b_diff_lock = READ_IF_EXISTS(ini, r_bool, "car_definition", "diff_lock", b_diff_lock);
	m_diff_lock_power = READ_IF_EXISTS(ini, r_float, "car_definition", "diff_lock_power", m_diff_lock_power);

	if (ini->section_exist("transmission_gear_ratio"))
	{
		float main_gear_ratio = ini->r_float("car_definition", "main_gear_ratio");

		m_gear_ratious.push_back(ini->r_fvector4("transmission_gear_ratio", "R"));
		m_gear_ratious[0][0] = -m_gear_ratious[0][0] * main_gear_ratio;
		string32 rat_num;
		for (int i = 1; true; ++i)
		{
			xr_sprintf(rat_num, "N%d", i);
			if (!ini->line_exist("transmission_gear_ratio", rat_num)) break;
			Fvector4 gear_rat = ini->r_fvector4("transmission_gear_ratio", rat_num);
			gear_rat[0] *= main_gear_ratio;
			gear_rat[1] *= (1.f / 60.f * 2.f * M_PI);
			gear_rat[2] *= (1.f / 60.f * 2.f * M_PI);
			m_gear_ratious.push_back(gear_rat);
		}
	}
	m_car_sound->Init();
}

void CCar::LoadVisualDefinitions(CInifile* ini)
{
	m_torch_usage = READ_IF_EXISTS(ini, r_bool, "lights", "torchlight", m_torch_usage);

	m_lights.Init(this);
	m_lights.ParseDefinitions("headlights", CCarLights::flHeadLights);
	m_stoplights.Init(this);
	m_stoplights.ParseDefinitions("stoplights", CCarLights::flStopLights);
	m_indoorlights.Init(this);
	m_indoorlights.ParseDefinitions("indoorlights", CCarLights::flIndoorLights);
	m_reverselights.Init(this);
	m_reverselights.ParseDefinitions("reverselights", CCarLights::flReverseLights);
	m_enginelights.Init(this);
	m_enginelights.ParseDefinitions("enginelights", CCarLights::flEngineLights);
	if (ini->section_exist("animations") && ini->line_exist("animations", "driver_animation_type"))
	{
		const u16 DriverAnimType = ini->r_u16("animations", "driver_animation_type");
		if (DriverAnimType < SActorVehicleAnims::TYPES_NUMBER)
		{
			m_driver_anim_type = DriverAnimType;
		}
		else
		{
			m_driver_anim_type = 0;
			Msg("! CCar [%s]: driver_animation_type = %u does not exist, using default 0",
				cNameSect().c_str(), u32(DriverAnimType));
		}
	}

	if (ini->section_exist("doors"))
		m_doors_torque_factor = ini->r_u16("doors", "open_torque_factor");

	m_damage_particles.Init(this);

	// The stock IXRAY material pairs have no collide marks for
	// objects\car_wheel, so the Cars-Ext tire trails use their own
	// surface-specific wallmark arrays instead of relying on gamemtl.xr.
	if (READ_IF_EXISTS(pSettings, r_bool, cNameSect_str(), "tire_trails", true)
		&& m_tire_wallmarks_ground->empty())
	{
		m_tire_wallmarks_ground->AppendMark(READ_IF_EXISTS(pSettings, r_string, cNameSect_str(), "tire_trail_wallmark_name", "wm\\wm_tire_trail_1"));
		m_tire_wallmarks_dirt->AppendMark(READ_IF_EXISTS(pSettings, r_string, cNameSect_str(), "tire_trail_wallmark_name_dirt", "wm\\wm_tire_trail_dirt_1"));
		m_tire_wallmarks_grass->AppendMark(READ_IF_EXISTS(pSettings, r_string, cNameSect_str(), "tire_trail_wallmark_name_grass", "wm\\wm_tire_trail_grass_1"));
	}

	// Wheel sounds are configured in vehicles.ltx. Stock gamemtl.xr uses
	// human footsteps for objects\car_wheel, so the Cars-Ext defaults use
	// the dedicated LA wheel loops and skid sounds instead.
	if (READ_IF_EXISTS(pSettings, r_bool, cNameSect_str(), "tire_sounds", true))
	{
		auto load_sound = [&](ref_sound& sound, LPCSTR key, LPCSTR default_name)
		{
			if (sound._p)
				return;
			LPCSTR name = READ_IF_EXISTS(pSettings, r_string, cNameSect_str(), key, default_name);
			if (name && *name)
				sound.create(name, st_Effect, sg_SourceType);
		};

		load_sound(m_tire_sound_road, "tire_sound_road", "car\\wheel_road");
		load_sound(m_tire_sound_dirt, "tire_sound_dirt", "car\\wheel_dirt");
		load_sound(m_tire_sound_grass, "tire_sound_grass", "car\\wheel_grass");
		load_sound(m_tire_sound_water, "tire_sound_water", "car\\wheel_water");
		load_sound(m_tire_skid_sound_road, "tire_skid_sound_road", "car\\handbrake");
		load_sound(m_tire_skid_sound_dirt, "tire_skid_sound_dirt", "car\\dirt_skid");
		load_sound(m_tire_skid_sound_grass, "tire_skid_sound_grass", "car\\dirt_skid");
		load_sound(m_tire_skid_sound_water, "tire_skid_sound_water", "car\\dirt_skid");
	}
}
CCar::ETireSurface CCar::GetTireSurface(u16 m1_id, u16 m2_id)
{
	const SGameMtl* material1 = GMLib.GetMaterialByID(m1_id);
	const SGameMtl* material2 = GMLib.GetMaterialByID(m2_id);
	const SGameMtl* ground = material1;

	const bool material1_is_wheel = material1 && strstr(*material1->m_Name, "car_wheel");
	const bool material2_is_wheel = material2 && strstr(*material2->m_Name, "car_wheel");
	if (material1_is_wheel && !material2_is_wheel)
		ground = material2;
	else if (!material1_is_wheel && material2_is_wheel)
		ground = material1;

	if (ground)
	{
		LPCSTR name = *ground->m_Name;
		if (strstr(name, "water"))
			return TireSurfaceWater;
		if (strstr(name, "grass") || strstr(name, "bush"))
			return TireSurfaceGrass;
		if (strstr(name, "earth") || strstr(name, "dirt") || strstr(name, "sand") || strstr(name, "gravel"))
			return TireSurfaceDirt;
	}

	return TireSurfaceRoad;
}

IWallMarkArray* CCar::GetTireWallmarks(u16 m1_id, u16 m2_id)
{
	if (m_tire_wallmarks_ground->empty())
		return nullptr;

	switch (GetTireSurface(m1_id, m2_id))
	{
		case TireSurfaceGrass:	return &*m_tire_wallmarks_grass;
		case TireSurfaceDirt:	return &*m_tire_wallmarks_dirt;
		case TireSurfaceWater:	return nullptr;
		default:				return &*m_tire_wallmarks_ground;
	}
}

ref_sound* CCar::GetTireSound(u16 m1_id, u16 m2_id)
{
	switch (GetTireSurface(m1_id, m2_id))
	{
		case TireSurfaceDirt:	return &m_tire_sound_dirt;
		case TireSurfaceGrass:	return &m_tire_sound_grass;
		case TireSurfaceWater:	return &m_tire_sound_water;
		default:				return &m_tire_sound_road;
	}
}

ref_sound* CCar::GetTireSkidSound(u16 m1_id, u16 m2_id)
{
	switch (GetTireSurface(m1_id, m2_id))
	{
		case TireSurfaceDirt:	return &m_tire_skid_sound_dirt;
		case TireSurfaceGrass:	return &m_tire_skid_sound_grass;
		case TireSurfaceWater:	return &m_tire_skid_sound_water;
		default:				return &m_tire_skid_sound_road;
	}
}

void CCar::LoadBoneMap()
{
	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	CInifile* ini = pKinematics->LL_UserData();
	if (ini)
	{
		if (ini->line_exist("car_definition", "driving_wheels"))
			fill_wheel_vector(ini->r_string("car_definition", "driving_wheels"), m_driving_wheels);
		if (ini->line_exist("car_definition", "steering_wheels"))
			fill_wheel_vector(ini->r_string("car_definition", "steering_wheels"), m_steering_wheels);
		if (ini->line_exist("car_definition", "breaking_wheels"))
			fill_wheel_vector(ini->r_string("car_definition", "breaking_wheels"), m_breaking_wheels);
		if (ini->line_exist("car_definition", "exhausts"))
			fill_exhaust_vector(ini->r_string("car_definition", "exhausts"), m_exhausts);
		if (ini->line_exist("car_definition", "doors"))
			fill_doors_map(ini->r_string("car_definition", "doors"), m_doors);

		if (ini->line_exist("car_definition", "trunk_bone"))
		{
			u16 bone_id = pKinematics->LL_BoneID(ini->r_string("car_definition", "trunk_bone"));
			m_bone_trunk = bone_id;
			SDoor						door(this);
			door.bone_id = bone_id;
			m_doors.insert(std::make_pair(bone_id, door));
			if (bone_map.find(bone_id) == bone_map.end())
				bone_map.insert(std::make_pair(bone_id, physicsBone()));
		}

		if (ini->line_exist("car_definition", "doors_use_recursive"))
		{
			string128					S1;
			LPCSTR						S_ = ini->r_string("car_definition", "doors_use_recursive");
			int count = _GetItemCount(S_);
			for (int i = 0; i < count; ++i)
			{
				_GetItem(S_, i, S1);
				if(SDoor* door = Get_Door(pKinematics->LL_BoneID(S1), true))
				{
					m_doors_recurs.insert(std::make_pair(i, door));
					door->recursive_door = true;
				}
			}
		}
	}
	for (auto& Bone : *pKinematics->LL_Bones())
	{
		CBoneData &bd = pKinematics->LL_GetData( Bone.second );

		if (bd.shape.type != SBoneShape::stNone && !bd.shape.flags.test(SBoneShape::sfNoPhysics) && bone_map.find(Bone.second) == bone_map.end())
			bone_map.insert(std::make_pair(Bone.second, physicsBone()));
	}
}

void CCar::CreateSkeleton(CSE_Abstract	*po)
{

	if (!Visual()) return;
	IRenderVisual *pVis = Visual();
	IKinematics* pK = smart_cast<IKinematics*>(pVis);
	IKinematicsAnimated* pKA = smart_cast<IKinematicsAnimated*>(pVis);
	if(pKA)
	{
		pKA->PlayCycle		("idle");
		pK->CalculateBones	(TRUE);
	}

	m_pPhysicsShell		= P_build_Shell(this, false, &bone_map);
	m_pPhysicsShell->SetPrefereExactIntegration();
	ApplySpawnIniToPhysicShell(&po->spawn_ini(),m_pPhysicsShell,false);
	ApplySpawnIniToPhysicShell(pK->LL_UserData(),m_pPhysicsShell,false);
}

void CCar::Init()
{

	CPHCollisionDamageReceiver::Init();

	//get reference wheel radius
	IKinematics* pKinematics=smart_cast<IKinematics*>(Visual());
	CInifile* ini = pKinematics->LL_UserData();

	// Cars-Ext uses a much lower default than vanilla's 5.0. Keep the same
	// default for models that do not override collision_hit_threshold.
	SetCollisionHitThreshold(READ_IF_EXISTS(ini, r_float, "car_definition", "collision_hit_threshold", 1.f));

	pKinematics->LL_GetBindTransform(bones_bind_forms);
	xr_vector<Fvector> m_bind_poses;
	for (auto b_m = bones_bind_forms.begin(); b_m != bones_bind_forms.end(); ++b_m)
		m_bind_poses.push_back(b_m->c);
	bind_center.set(0.f, 0.f, 0.f);
	for (const Fvector& BindPos : m_bind_poses) bind_center.add(BindPos);
	if (!m_bind_poses.empty()) bind_center.div(float(m_bind_poses.size()));


	if(ini->section_exist("air_resistance"))
	{
		PPhysicsShell()->SetAirResistance(default_k_l*ini->r_float("air_resistance","linear_factor"),default_k_w*ini->r_float("air_resistance","angular_factor"));
		m_downforce = READ_IF_EXISTS(ini, r_float, "air_resistance", "downforce", m_downforce);
	}
	PPhysicsShell()->set_DynamicScales(1.f, 1.f);
	SAllDDOParams disable_params;
	disable_params.Load(ini);
	m_pPhysicsShell->set_DisableParams(disable_params);

	if(ini->line_exist("car_definition","steer"))
	{
		string128					S1;
		LPCSTR						S_ = ini->r_string	("car_definition","steer");
		int count =					_GetItemCount(S_);
		for (int i=0 ;i<count; ++i) 
		{
			_GetItem					(S_,i,S1);
			m_steers.push_back(pKinematics->LL_BoneID(S1));
			u16 bone_id	=				m_steers.back();

			string128 steer_factor;
			xr_sprintf(steer_factor, "%s_factor", S1);
			if (ini->line_exist("car_definition", steer_factor))
			{
				m_steer_factor = ini->r_float("car_definition", steer_factor);
				pKinematics->LL_GetBoneInstance(bone_id).set_callback(bctCustom, cb_Steer_factor, this);
			}
			else
				pKinematics->LL_GetBoneInstance(bone_id).set_callback(bctCustom,cb_Steer,this);
		}
	}
	if (ini->line_exist("lights", "indoorlightdoors"))
	{
		string128					S1;
		LPCSTR						S_ = ini->r_string("lights", "indoorlightdoors");
		int count = _GetItemCount(S_);
		for (int i = 0; i < count; ++i)
		{
			_GetItem(S_, i, S1);
			m_indoor_l_available_doors.push_back(pKinematics->LL_BoneID(S1));
		}
	}
	m_steer_angle=0.f;
	m_ref_radius = READ_IF_EXISTS(ini, r_float, "car_definition", "reference_radius", 1.f);
	b_exploded						=false;
	b_engine_on						=false;
	b_clutch						=false;
	b_starting						=false;

	auto root_elem = bone_map.find(pKinematics->LL_GetBoneRoot());
	if(root_elem != bone_map.end() && root_elem->second.element)
		m_root_transform.set(root_elem->second.element->mXFORM);
	else
		Msg("!!! %s bone_map[%s]", cNameVisual().c_str(), pKinematics->LL_BoneName_dbg(pKinematics->LL_GetBoneRoot()));

	m_current_transmission_num=0;
	CDamagableItem::Init(GetfHealth(),3);
	CDelayedActionFuse::Initialize(READ_IF_EXISTS(ini, r_float, "car_definition", "time_to_explosion", 120.f), CDamagableItem::DamageLevelToHealth(2));
	{
		for (auto& i : m_wheels_map)
		{
			i.second.Init();
			i.second.CDamagableHealthItem::Init(100.f,2);
		}
	}

	{
		for (auto& i : m_driving_wheels)
		{
			i.Init();
		}
	}

	{
		for (auto& i : m_breaking_wheels)
		{
			i.Init();
		}
	}

	{
		for (auto& i : m_steering_wheels)
		{
			i.Init();
		}
	}

	{
		for (auto& i : m_exhausts)
		{
			i.Init();
		}
	}

	for (auto& i : m_wheels_map)
	{
		if (!i.second.WheelFlags.test(SWheel::eWheelType::wheel_drive))
			b_awd = false;
	}

	{
		for (auto& i : m_doors)
		{
			i.second.Init();
			i.second.CDamagableHealthItem::Init(100,1);
		}
			
	}

	if(ini->section_exist("damage_items"))
	{
		CInifile::Sect& data		= ini->r_section("damage_items");
		for (CInifile::SectCIt		I=data.Data.begin(); I!=data.Data.end(); I++){
			const CInifile::Item& item	= *I;
			u16 index				= pKinematics->LL_BoneID(*item.first); 
			R_ASSERT3(index != BI_NONE, "Wrong bone name", *item.first);
			xr_map   <u16,SWheel>::iterator i=m_wheels_map.find(index);
			
			if(i!=m_wheels_map.end())
					i->second.CDamagableHealthItem::Init(float(atof(*item.second)),2);
			else 
			{
				xr_map   <u16,SDoor>::iterator i=m_doors.find(index);
				R_ASSERT3(i!=m_doors.end(),"only wheel and doors bones allowed for damage defs",*item.first);
				i->second.CDamagableHealthItem::Init(float(atof(*item.second)),1);
			}

		}
	}
	

	if(ini->section_exist("immunities"))
		LoadImmunities("immunities",ini);

	if (ini->line_exist("car_definition", "damage"))
		if (TDamageManager* DmgManager = GetComponent<TDamageManager>()) DmgManager->reload("car_definition","damage",ini);
	
	HandBreak();
	Transmission(1);
}

void CCar::Revert()
{
	m_pPhysicsShell->applyForce(0,1.5f*EffectiveGravity()*m_pPhysicsShell->getMass(),0);
}

void CCar::NeutralDrive()
{
	e_state_drive = neutral;
	if (fwp || bkp)
		m_current_engine_power = EnginePower();

	for (auto& i : m_wheels_map)
	{
		i.second.Neutral();
	}

	//if (m_car_sound->snd_valve._p && (m_engine_valve == 1) && ((Device.fTimeGlobal - m_engine_state_start_time) > 0.0f) && (Device.fTimeGlobal - (m_transmission_switch_time + m_transmission_switch_delay) > 0.0f))
}
void CCar::ReleaseHandBreak()
{
	for (auto& i : m_wheels_map)
	{
		i.second.Neutral();
	}
	if (e_state_drive == drive)
		Drive();
}
void CCar::Drive()
{
	//if (m_car_sound->snd_gas._p && (m_engine_valve == 0) && ((Device.fTimeGlobal - m_transmission_switch_time) > 0.0f) && m_engine_state_time > 0.0f)
	if(!b_clutch||!b_engine_on) return;
	for (auto& i : m_driving_wheels)
	{
		i.Drive();
	}
	//if (Device.fTimeGlobal < m_transmission_switch_time + m_transmission_switch_delay)
	e_state_drive = drive;
}

void CCar::StartEngine()
{
	if(m_fuel<EPS||b_engine_on) return;
	PlayExhausts();
	m_car_sound->Start();
	b_engine_on=true;
	m_current_rpm=0.f;
	m_current_engine_power = 0.f;
	m_current_rpm_brp_n = 1.f;
	Starter();
}
void CCar::StopEngine()
{
	if(!b_engine_on) return;
	AscCall(ascSndStall);
	AscCall(ascExhoustStop);
	NeutralDrive();//set zero speed
	b_engine_on=false;
	UpdatePower();//set engine friction;
	m_current_rpm=0.f;
}

void CCar::Stall()
{
	AscCall(ascSndStall);
	AscCall(ascExhoustStop);
	NeutralDrive();//set zero speed
	b_engine_on=false;
	UpdatePower();//set engine friction;
	m_current_rpm=0.f;
}

void CCar::SwitchEngine()
{
	if(b_engine_on) StopEngine();
	else			StartEngine();
}

void CCar::SwitchEngineDoors()
{
	VERIFY(!physics_world()->Processing());

	for (auto &it : engine_door)
		it.Switch(b_engine_on);
}

void CCar::Clutch()
{
	b_clutch=true;
}

void CCar::Unclutch()
{
	b_clutch=false;
}

void CCar::Starter()
{
	b_starting=true;
	m_dwStartTime=Device.dwTimeGlobal;
}
void CCar::UpdatePower()
{
	if (Device.dwTimeGlobal - m_hit_engine_time < 1000)
	{
		for (auto& i : m_driving_wheels)
		{
			i.UpdatePower();
		}
		return;
	}

	if (b_engine_on && Device.dwTimeGlobal - m_dwStartTime > m_car_sound->engine_start_delay)
	{
		if (brp)
		{
			if ((fwp || bkp) && e_state_drive != neutral)
			{
				m_current_rpm += (100.f * m_rpm_increment_factor *  m_rpm_brp_increment_factor) * fixed_step * FixedStepCompensation;
				m_current_rpm_brp_n += 0.2f * fixed_step * FixedStepCompensation;
			}
			else
			{
				m_current_rpm = ((1.f - (0.1f * m_rpm_decrement_factor * m_rpm_brp_decrement_factor) * fixed_step * FixedStepCompensation) * m_current_rpm + (0.1f * m_rpm_decrement_factor * m_rpm_brp_decrement_factor) * fixed_step * FixedStepCompensation * m_min_rpm);
				m_current_rpm_brp_n -= m_current_rpm_brp_decrease * 0.01f * (m_current_rpm_brp_n_max - 1.f) * fixed_step * FixedStepCompensation * m_current_rpm_brp_n_max;
			}
			clamp(m_current_rpm, m_min_rpm, m_max_rpm);
			clamp(m_current_rpm_brp_n, 1.f, m_current_rpm_brp_n_max);
		}
		else
		{
			if (!b_awd && bkp && fwp && GetSpeedKmH() < 5.f)
				m_current_rpm_brp_n += 0.2f * fixed_step * FixedStepCompensation;
			else
				m_current_rpm_brp_n -= m_current_rpm_brp_decrease * 0.01f * (m_current_rpm_brp_n_max - 1.f) * fixed_step * FixedStepCompensation * m_current_rpm_brp_n_max;
			clamp(m_current_rpm_brp_n, 1.f, m_current_rpm_brp_n_max);
			m_current_rpm = EngineDriveSpeed();
			if (!b_cvt_transmission)
			{
				if (!b_auto_transmission_stop && !rs_car_transmission && mb_traction && !m_gear_ratious.empty())
				{
					if (m_current_rpm < m_gear_ratious[CurrentTransmission()][1]) TransmissionDown();
					if (m_current_rpm > m_gear_ratious[CurrentTransmission()][2] && GetSpeedKmH() > m_gear_ratious[CurrentTransmission()][3]) TransmissionUp();
				}
			}
			else if (!m_gear_ratious.empty())
			{
				float rpm = m_power_rpm;
				if (e_state_drive != drive)
					rpm = m_torque_rpm;

				if (DriveWheelsMeanAngleRate() != 0.f)
					m_current_gear_ratio = (rpm * m_ref_radius) / (fabsf(DriveWheelsMeanAngleRate()) * m_ref_radius);
				const size_t HighGearIndex = m_gear_ratious.size() > 2 ? 2 : m_gear_ratious.size() - 1;
				const size_t LowGearIndex = m_gear_ratious.size() > 1 ? 1 : 0;
				clamp(m_current_gear_ratio, m_gear_ratious[HighGearIndex][0], m_gear_ratious[LowGearIndex][0]);
				if (!CurrentTransmission())
					m_current_gear_ratio = -m_current_gear_ratio;
			}
			m_diff_lock_overpower = 0.f;
			for (auto& WheelDrive : m_driving_wheels)
			{
				if (b_diff_lock)
				{
					m_diff_lock_overpower += WheelDrive.pwheel->wheel_diff_power;
				}
				WheelDrive.UpdatePower();
			}
		}
		m_current_engine_power = EnginePower();
	}
	else
	{
		if (e_state_drive == neutral)
		{
			m_current_rpm = EngineDriveSpeed();
			m_current_engine_power = EnginePower();
			for (auto& i : m_driving_wheels)
			{
				i.UpdatePower();
			}
			
		}
	}
}

void CCar::SteerRight()
{
	m_steer_start_time = Device.fTimeGlobal;
	b_wheels_limited=true;  //no need to limit wheels when stiring
	for (auto& i : m_steering_wheels)
	{
		i.SteerRight();
	}
	e_state_steer=right;
}

void CCar::SteerLeft()
{
	m_steer_start_time = Device.fTimeGlobal;
	b_wheels_limited=true; //no need to limit wheels when stiring
	for (auto& i : m_steering_wheels)
	{
		i.SteerLeft();
	}
	e_state_steer=left;
}

void CCar::SteerIdle()
{
	m_steer_start_time = Device.fTimeGlobal;
	b_wheels_limited=false;
	for (auto& i : m_steering_wheels)
	{
		i.SteerIdle();
	}
	e_state_steer=idle;
}

void CCar::LimitWheels()
{
	if(b_wheels_limited) return;
	b_wheels_limited=true;
	for (auto& i : m_steering_wheels)
	{
		i.Limit();
	}
}
void CCar::HandBreak()
{
	float k = 1.f;
	float time = (Device.fTimeGlobal - m_break_start);
	if (!brp)
	{
		if (m_handbreak_start + 1.f < Device.fTimeGlobal)
			b_drift_boost = true;
		m_handbreak_start = Device.fTimeGlobal;
	}
	if (time < m_break_time)
	{
		k *= (time / m_break_time);
	}
	for (auto& i : m_breaking_wheels)
	{
		i.HandBreak(k);
	}
}

void CCar::StartBreaking()
{
	if(!b_breaks)
	{
		b_breaks = true;
		m_break_start = Device.fTimeGlobal;
		if (m_break_abs && m_break_abs_individual)
		{
			for (auto& i : m_breaking_wheels)
			{
				i.pwheel->collision_params.break_start = Device.fTimeGlobal;
			}
		}
	}
}
void CCar::StopBreaking()
{
	for (auto& i : m_wheels_map)
	{
		i.second.Neutral();
	}
	if (e_state_drive == drive)
		Drive();

	if (b_breaks && m_break_time > 0.f && b_engine_on && Device.fTimeGlobal - m_break_start > 0.05f)
		m_car_sound->BrakeRelease();
	b_breaks = false;
}
void CCar::PressRight()
{
	if(lsp)
		SteerIdle();
	else
		SteerRight();
	rsp = true;
}
void CCar::PressLeft()
{
	if(rsp)
		SteerIdle();
	else
		SteerLeft();
	lsp=true;
}
void CCar::PressForward()
{
	if (bkp)
	{
		Unclutch();
		NeutralDrive();
	}
	else
	{
		Unclutch();
		NeutralDrive();
		StartBreaking();
	}

	fwp=true;
}
void CCar::PressBack()
{
	if (fwp)
	{
		Unclutch();
		NeutralDrive();
	}
	else
	{
		Unclutch();
		NeutralDrive();
		StartBreaking();
	}
	bkp=true;
}
void CCar::PressBreaks()
{
	/*
	if (!brp && Device.fTimeGlobal - m_handbreak_start > 1.f)
		if (m_pPhysicsShell && m_pPhysicsShell->isActive())
		{
			if (m_pPhysicsShell->get_ElementByStoreOrder(0))
			{
				Fvector angular_velocity = GetAvelAxes(0);

				float spd = GetSpeedKmH() * 0.01f;
				clamp(spd, 0.f, 1.f);
				spd = sinf(0.5f * PI * powf(spd, 0.5f));

				if (e_state_steer == right && angular_velocity.y > 0.f)
				{
					angular_velocity.y += m_drift_start_boost * spd;
					SetAvelAxes(0, angular_velocity);
				}
				if (e_state_steer == left && angular_velocity.y < 0.f)
				{
					angular_velocity.y -= m_drift_start_boost * spd;
					SetAvelAxes(0, angular_velocity);
				}
			}
		}
		*/
	HandBreak();
	brp=true;
}

void CCar::DriveBack()
{
	Clutch();
	Transmission(0);
	Drive();
}
void CCar::DriveForward()
{
	Clutch();
	if (0 == CurrentTransmission()) Transmission(1);
	Drive();
}
void CCar::ReleaseRight()
{
	if(lsp)
		SteerLeft();
	else
		SteerIdle();
	rsp=false;
}
void CCar::ReleaseLeft()
{
	if(rsp)
		SteerRight();
	else
		SteerIdle();
	lsp=false;
}
void CCar::ReleaseForward()
{
	if(b_breaks && !fwpbkp)
	{
		StopBreaking();
	}
	if(bkp)
	{
		Clutch();
		Transmission(0);
		Drive();
	}
	else
	{
		Unclutch();
		NeutralDrive();
	}
	b_burnout = false;
	fwp = false;
}
void CCar::ReleaseBack()
{
	if(b_breaks && !fwpbkp)
	{
		StopBreaking();
	}
	if(fwp)
	{
		Clutch();
		if (0 == CurrentTransmission()) Transmission(1);
		Drive();
	}
	else
	{
		Unclutch();
		NeutralDrive();
	}
	b_burnout = false;
	bkp=false;
}

void CCar::ReleaseBreaks()
{
	ReleaseHandBreak();
	brp=false;
}

void CCar::Transmission(size_t num, ETransmissionStage SwitchRequest)
{
	if (m_gear_ratious.empty())return;
	if(num<m_gear_ratious.size())
	{
		if (CurrentTransmission() != num)
		{

			m_transmission_sound_played = false;
			m_transmission_switch_time = Device.fTimeGlobal;

			if (SwitchRequest == ETransmissionStage::SwitchUp && m_transmission_stage == ETransmissionStage::Drive)
			{
				m_transmission_stage = ETransmissionStage::SwitchUp;
				if (m_pop && e_state_drive == drive && m_current_rpm > m_torque_rpm && b_engine_on && !m_pop_single)
				{
					PlayExhaustsPop();
					m_car_sound->PopSound();
				}
			}
			else if (SwitchRequest == ETransmissionStage::SwitchDown && m_transmission_stage == ETransmissionStage::Drive)
			{
				m_transmission_stage = ETransmissionStage::SwitchDown;
			}

			m_current_transmission_num = num;
			m_current_gear_ratio = m_gear_ratious[num][0];
			Drive();

			if (e_state_drive == drive)
			{
				float rotate = m_switch_shake_boost * m_engine_rpm_scale;
				if (m_pPhysicsShell && m_pPhysicsShell->isActive())
				{
					if (m_pPhysicsShell->get_ElementByStoreOrder(0) && mb_traction)
					{
						Fvector angular_velocity = GetAvelAxes(0);
						angular_velocity.x += rotate;
						SetAvelAxes(0, angular_velocity);
					}
				}
			}
		}
		else if (!IsTransmissionSwitching())
			m_transmission_stage = ETransmissionStage::Drive;
	}

#ifdef DEBUG
#endif
}

void CCar::TransmissionUp()
{
	if (m_gear_ratious.empty())return;
	if(0==CurrentTransmission())return;
	if (b_cvt_transmission)return;
	m_transmission_switch_rpm = m_current_rpm;
	m_transmission_switch_sound_time = Device.fTimeGlobal;
	size_t transmission=1+CurrentTransmission();
	size_t max_transmition_num=m_gear_ratious.size()-1;
	transmission>max_transmition_num ? transmission=max_transmition_num :transmission;
	Transmission(transmission, ETransmissionStage::SwitchUp);
}

void CCar::TransmissionDown()
{
	if (m_gear_ratious.empty())return;
	if(0==CurrentTransmission())return;
	if(b_cvt_transmission)return;
	m_transmission_switch_rpm = m_current_rpm;
	m_transmission_switch_sound_time = Device.fTimeGlobal;
	size_t transmission=CurrentTransmission()-1;
	transmission < 1 ? transmission = 1 : transmission;
	Transmission(transmission, ETransmissionStage::SwitchDown);
}


void CCar::PhTune(float step)
{
	for(u16 i=PPhysicsShell()->get_ElementsNumber();i!=0;i--)	
	{
		CPhysicsElement* e=PPhysicsShell()->get_ElementByStoreOrder(i-1);
		if(e->isActive()&&e->isEnabled())e->applyForce( 0, e->getMass()*AntiGravityAccel(), 0 );
	}
}
float CCar::EffectiveGravity()
{
	float g= physics_world()->Gravity();
	if(CPHUpdateObject::IsActive())g*=0.5f;
	return g;
}
float CCar::AntiGravityAccel()
{
	return physics_world()->Gravity()-EffectiveGravity();
}
float CCar::GravityFactorImpulse()
{
	return _sqrt(EffectiveGravity()/physics_world()->Gravity());
}

void CCar::PlayExhausts()
{

	for (auto& i : m_exhausts)
	{
		i.Play();
	}
}

void CCar::PlayExhaustsNitro()
{
	for (auto& i : m_exhausts)
	{
		i.PlayNitro();
	}
}

void CCar::PlayExhaustsPop()
{
	for (auto& i : m_exhausts)
	{
		if (Random.randF(0.f, 1.f) < 0.9f)
			i.PlayPop();
	}
}

void CCar::StopExhausts()
{
	for (auto& i : m_exhausts)
	{
		i.Stop();
	}
}

void CCar::StopExhaustsNitro()
{
	for (auto& i : m_exhausts)
	{
		i.StopNitro();
	}
}

void CCar::UpdateExhausts()
{
	if(!b_engine_on) return;
	for (auto& i : m_exhausts)
	{
		i.Update();
	}
}

void CCar::ClearExhausts()
{
	for (auto& i : m_exhausts)
	{
		i.Clear();
	}
}
bool CCar::ScriptUseTrunk(u16 bone_id)
{
	if (!pSettings->section_exist("lost_alpha_cfg") || !pSettings->line_exist("lost_alpha_cfg", "on_use_trunk"))
		return false;

	luabind::functor<void>	lua_function;
	string256		fn;
	xr_strcpy(fn, pSettings->r_string("lost_alpha_cfg", "on_use_trunk"));
	if (!ai().script_engine().functor<void>(fn, lua_function))
	{
		Msg("! CCar: cannot find Lua function %s", fn);
		return false;
	}
	lua_function(ID());
	return true;
}

u16 CCar::GetNearestBone(Fvector look_pos, Fvector look_dir, float &range, bool trunk_test)
{
	range = 0.f;
	xr_map<float, u16> m_bones_ids_v;

	// The car collision model is one-sided, so a ray that starts inside the cabin does
	// not report the car itself. Bone picking is double-sided, so scan the bones directly
	// instead of requiring an initial collision hit (needed for first-person door use).
	PKinematics(Visual())->CalculateBones(TRUE);
	for (auto I = PKinematics(Visual())->LL_Bones()->begin(); I != PKinematics(Visual())->LL_Bones()->end(); ++I)
	{
		if(!PKinematics(Visual())->LL_GetBoneVisible(I->second))
			continue;

		Fmatrix bone_xform = PKinematics(Visual())->LL_GetTransform((*I).second);
		bone_xform.mulA_43(XFORM());

		float w	= bone_xform.c.x*Device.mFullTransform._14 + bone_xform.c.y*Device.mFullTransform._24 + bone_xform.c.z*Device.mFullTransform._34 + Device.mFullTransform._44;
		if (w<0) continue;

		IKinematics::pick_result res;
		if(PKinematics(Visual())->PickBone(XFORM(), res, Radius()*2, look_pos, look_dir, I->second))
		{
			Fvector collide_pos = Fvector(look_pos).add(Fvector(look_dir).mul(res.dist));

			Fvector diff_cam;
			diff_cam.sub(collide_pos, look_pos);
			if (!safe_normalize(diff_cam)) continue;
			if(diff_cam.similar(look_dir, EPS_L))
				m_bones_ids_v.insert(std::make_pair(look_pos.distance_to(collide_pos), I->second));
		}
	}
	if(trunk_test)
	{
		RQR.r_clear();
		collide::ray_defs	Q(look_pos, look_dir, Radius()*2, CDB::OPT_CULL,collide::rqtObject);  // CDB::OPT_ONLYFIRST CDB::OPT_ONLYNEAREST
		if (g_pGameLevel->ObjectSpace.RayQuery(RQR, collidable.model, Q))
		{
			for (auto I = RQR.r_begin(); I != RQR.r_end(); ++I)
			{
				Fvector collide_pos = Fvector(look_pos).add(Fvector(look_dir).mul(I->range));

				Fvector diff_cam;
				diff_cam.sub(collide_pos, look_pos);
				if (!safe_normalize(diff_cam)) continue;
				if(diff_cam.similar(look_dir, EPS_L) && m_bone_trunk == u16(I->element))
					m_bones_ids_v.insert(std::make_pair(look_pos.distance_to(collide_pos), u16(I->element)));
			}
		}
	}

	auto nearest_IT = std::min_element(m_bones_ids_v.begin(), m_bones_ids_v.end());
	if(nearest_IT == m_bones_ids_v.end())
		return BI_NONE;
	range = nearest_IT->first;
	return nearest_IT->second;
}
bool drag_activated = false;
void drag_callback(void* ptr)
{
	const u16 CarId = static_cast<u16>(reinterpret_cast<size_t>(ptr));
	if (Device.Paused()) return;
	if (!g_pGameLevel) return;
	if (!g_pGameLevel->CurrentEntity()) return;
	if (!g_actor) return;

	Sleep(150);

	if (!g_pGameLevel || !g_pGameLevel->CurrentEntity() || !g_actor) return;
	CCar* pCar = smart_cast<CCar*>(Level().Objects.net_Find(CarId));
	if (!pCar || pCar->getDestroy()) return;

	drag_activated = true;
	pCar->b_event_drag_activated = true;
	drag_activated = false;
}
CCar::SDoor* CCar::GetNearestDoor(Fvector look_pos, Fvector look_dir, bool special)
{
	if(Owner() && active_camera->tag != EctFirstCam)
		return NULL;

	if(pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))
	{
		if(!special)
		{
			collide::rq_result RQ;
			bool HasPick = Level().ObjectSpace.RayPick(look_pos, look_dir, 1.5f, collide::rqtObject, RQ, Level().CurrentControlEntity());
			if(HasPick && RQ.O == this)
			{
				if (!Owner() && m_pPhysicsShell && g_actor)
				{
					if(!drag_activated)
						thread_spawn(drag_callback,"drag_callback",0,reinterpret_cast<void*>(static_cast<size_t>(ID())));
				}
			}
		}
		return NULL;
	}

	float dist;

	u16 bonebypoly_id = GetNearestBone(look_pos, look_dir, dist, true);
	SDoor* door = Get_Door(bonebypoly_id);

	if(door)
	{
		if(door->recursive_door)
		{
			if(special)
			{
				if(!Owner())
					return door;
				else
					return NULL;
			}
			b_start_close_recursive = false;
			b_start_open_recursive = true;
			return NULL;
		}
		if (m_bone_trunk == bonebypoly_id)
		{
			if(special)
			{
				if(!Owner())
					return door;
				else
					return NULL;
			}
			ScriptUseTrunk(bonebypoly_id);
			return NULL;
		}
	}
	else
	{
		if((m_doors_recurs.empty() || (!b_start_close_recursive && b_all_recurs_opened)) && m_bone_trunk == bonebypoly_id)
		{
			if(special)
			{
				if(!Owner())
					return door;
				else
					return NULL;
			}
			ScriptUseTrunk(bonebypoly_id);
			return NULL;
		}
	}

	if(door && special && Owner() && door->non_enterable)
		return NULL;

	return door;
}
CCar::SDoor* GNearestDoor(CCar* pcar, xr_map<u16,CCar::SDoor> &doors)
{
	xr_map<float, CCar::SDoor*> m_doors;
	for (auto I=doors.begin(); I!=doors.end(); ++I)
	{
		if(!pcar) continue;
		if(!g_actor) continue;
		if(I->second.non_enterable) continue;
		if(I->second.non_exitable) continue;
		Fvector center; Actor()->Center(center);
		m_doors.insert(std::make_pair(I->second.GetWorldPosition().distance_to(center), &I->second));
	}
	if (m_doors.empty())
		return nullptr;

	return std::min_element(m_doors.begin(), m_doors.end())->second;
}
bool CCar::TryTrunk()
{
	if (m_bone_trunk == BI_NONE)
		return false;

	// The trunk uses the same per-bone parameters as doors from the model ltx,
	// including use_distance from its [bone] section.
	float UseDistance = 2.f;
	auto DoorIt = m_doors.find(m_bone_trunk);
	if (DoorIt != m_doors.end())
		UseDistance = DoorIt->second.dist_to_door_avlbl;

	// The trunk is usable only when the aim ray actually hits its bone.
	// GetNearestBone scans the car bones and also adds trunk-specific ray hits,
	// so it works from any side but does not trigger while aiming at a door.
	float distance = 0.f;
	const u16 bone = GetNearestBone(Device.vCameraPosition, Device.vCameraDirection, distance, true);
	if (bone != m_bone_trunk || distance > UseDistance)
	{
		return false;
	}

	return IsGameTypeSingle();
}

bool CCar::TryUsableBones()
{
	if (UsableBones.empty())
		return false;

	UsableBonesActive = BI_NONE;

	RQR.r_clear();
	collide::ray_defs Q(Device.vCameraPosition, Device.vCameraDirection, 3.f, CDB::OPT_CULL, collide::rqtObject);

	if (g_pGameLevel->ObjectSpace.RayQuery(RQR, collidable.model, Q))
	{
		collide::rq_results& R = RQR;
		const int Count = R.r_count();

		for (int k = 0; k < Count; ++k)
		{
			collide::rq_result* I = R.r_begin() + k;
			if (IsGameTypeSingle() && UsableBones.contains(I->element))
			{
				UsableBonesActive = I->element;
				return true;
			}
		}
	}

	return false;
}

bool CCar::Use(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos)
{
	if (!Owner())
	{
		if (TryUsableBones())
		{
			luabind::functor<bool> Callback;
			if (ai().script_engine().functor(UsableBones[UsableBonesActive].c_str(), Callback))
			{
				if (Callback(this))
					return false;
			}
		}

		if (TryTrunk())
		{
			auto DoorIt = m_doors.find(m_bone_trunk);
			if (DoorIt != m_doors.end())
			{
				SDoor& Trunk = DoorIt->second;
				switch (Trunk.state)
				{
					case SDoor::closed:
					case SDoor::closing:
						Trunk.Open();
						break;
					case SDoor::opened:
					case SDoor::broken:
						if (CurrentGameUI())
							CurrentGameUI()->StartCarBody(Actor(), this);
						break;
					case SDoor::opening:
					default:
						break;
				}
			}
			return false;
		}
	}

	if(!Owner())
	{
		if(m_doors.empty())
		{
			RQR.r_clear			();
			collide::ray_defs	Q(pos, dir, Radius(), CDB::OPT_ONLYNEAREST, collide::rqtObject);  // CDB::OPT_ONLYFIRST CDB::OPT_ONLYNEAREST
			if (g_pGameLevel->ObjectSpace.RayQuery(RQR,collidable.model,Q))
			{
				for (auto I = RQR.r_begin(); I != RQR.r_end(); ++I)
				{
					if(I->range <= 1.5f)
						return true;
				}
			}
		}
		// Aiming at the door itself toggles it (open/close) before the entry check.
		// Entering is done by aiming through the free space of the open doorway.
		// use_distance from the model ltx limits how far the door can be used.
		if (SDoor* Door = GetNearestDoor(pos, dir))
		{
			if (Door->IsDistOfSight(pos) && (!Door->non_exitable || Door->non_enterable))
			{
				Door->Use();
				if (Door->state != SDoor::broken)
					return false;
			}
		}
		if(Enter(pos,dir,foot_pos))
		{
			if(active_camera->tag != EctFirstCam)
			{
				if(SDoor* LastDoor = GNearestDoor(this, m_doors))
				{
					if(LastDoor->state!=CCar::SDoor::closed)
						LastDoor->Close();
				}
			}
			return true;
		}
	}
	else
	{
		if (m_doors.empty())
		{
			Fvector P;
			XFORM().transform_tiny(P, m_last_enter_position);
			m_exit_position.set(P);
			return true;
		}

		if (active_camera->tag != EctFirstCam)
		{
			if (SDoor* LastDoor = GNearestDoor(this, m_doors))
			{
				m_exit_door_bone = LastDoor->bone_id;
				LastDoor->GetExitPosition(m_exit_position);

				if (LastDoor->state != CCar::SDoor::opened)
				{
					LastDoor->Open();
					b_event_exit_car = true;
					m_exit_car_time = Device.dwTimeGlobal + 400;
				}
				else
					OwnerActor()->detach_Vehicle();

				return false;
			}
		}
	}
	if(SDoor* Door = GetNearestDoor(pos, dir)) 
	{
		if ((Owner() && !Door->non_enterable) || (!Owner() && Door->IsDistOfSight(pos) && (!Door->non_exitable || Door->non_enterable)))
			Door->Use();

		if (Door->state != SDoor::broken)
			return false;
	}

	if(Owner())
		return Exit(pos,dir);

	return false;

}

bool CCar::DoorSwitch(u16 id)
{
	xr_map<u16,SDoor>::iterator i;
	if(is_Door(id,i)) 
	{
		i->second.Switch();
		return true;
	}
	else
	{
		return false;
	}
}
bool CCar::DoorClose(u16 id)
{

	xr_map<u16,SDoor>::iterator i;
	if(is_Door(id,i)) 
	{
		i->second.Close();
		return true;
	}
	else
	{
		return false;
	}
}

bool CCar::DoorOpen(u16 id)
{

	xr_map<u16,SDoor>::iterator i;
	if(is_Door(id,i)) 
	{
		i->second.Open();
		return true;
	}
	else
	{
		return false;
	}
}
void CCar::InitParabola()
{


	m_a=expf((m_power_rpm - m_torque_rpm)/(2.f*m_power_rpm))*m_max_power/m_power_rpm;
	m_b=m_torque_rpm;
	m_c=_sqrt(2.f*m_power_rpm*(m_power_rpm - m_torque_rpm));
}
float CCar::Parabola(float rpm)
{
	float ex=(rpm-m_b)/m_c;
	float value=m_a*expf(-ex*ex)*rpm;
	if(value<0.f) return 0.f;
	if(e_state_drive == neutral)
		value *= m_power_neutral_factor;
	else
	{
		value *= m_current_rpm_brp_n;
		if (m_transmission_switch_delay > 0.f && m_current_rpm > 0.f && IsTransmissionSwitching())
			value *= m_transmission_switch_power_factor;
	}
	//if (m_turbo_power && m_car_sound->snd_turbo._p)

	if (m_power_reverse_factor && m_current_transmission_num == 0)
		value *= m_power_reverse_factor;

	if (b_diff_lock && !m_driving_wheels.empty() && m_diff_lock_overpower > float(m_driving_wheels.size()))
		value *= 1.f / (m_diff_lock_overpower / float(m_driving_wheels.size()));

	if (m_nitro_engage)
		value *= m_nitro_power_factor;

	if (GetfHealth() < 2.f / 3.f)
	{
		value *= 0.9f;
		if (GetfHealth() < 1.f / 3.f)
			value *= 0.85f;
	}
	return value;
}

float CCar::EnginePower()
{

	float value;value = Parabola(m_current_rpm);
	if(b_starting)
	{
		//if (m_current_rpm<m_min_rpm)
		//else 
			if (Device.dwTimeGlobal - m_dwStartTime > m_car_sound->engine_start_delay)
			{
				b_starting = false;
				m_fuel -= 1000.f * m_fuel_consumption;
			}
	}
	if (value > m_current_engine_power)
		return value * m_power_increment_factor * fixed_step * FixedStepCompensation + m_current_engine_power * (1.f - m_power_increment_factor * fixed_step * FixedStepCompensation);
	else
		return value * m_power_decrement_factor * fixed_step * FixedStepCompensation + m_current_engine_power * (1.f - m_power_decrement_factor * fixed_step * FixedStepCompensation);
}
float CCar::DriveWheelsMeanAngleRate()
{
	if (m_driving_wheels.empty())
		return 0.f;

	float drive_speed = 0.f;
	for (auto& WheelDrive : m_driving_wheels)
	{
		drive_speed += WheelDrive.ASpeed();
	}
	return drive_speed / m_driving_wheels.size();
}
float CCar::EngineDriveSpeed()
{
	float calc_rpm = EngineRpmFromWheels();

	if(calc_rpm<m_min_rpm)
		calc_rpm=m_min_rpm;

	if (calc_rpm > m_current_rpm)
		return	(1.f - m_rpm_increment_factor * fixed_step * FixedStepCompensation) * m_current_rpm + m_rpm_increment_factor * fixed_step * FixedStepCompensation * calc_rpm;
	else
		return	(1.f - m_rpm_decrement_factor * fixed_step * FixedStepCompensation) * m_current_rpm + m_rpm_decrement_factor * fixed_step * FixedStepCompensation * calc_rpm;

	//else					  return 0.f;
}


void CCar::UpdateFuel(float time_delta)
{
	if(!b_engine_on || rs_car_unlimitedfuel) return;
	float rpm = 1.f + (m_fuel_rpm_factor * m_engine_rpm_scale);
	float speed = 1.f + powf((GetSpeedKmH() / m_fuel_speed_optimal), m_fuel_speed_factor);
	float gas = 1.f;
	if (e_state_drive == drive)
		gas = 1.f + 1.f * m_fuel_gas_factor;
	if (m_nitro_engage)
		gas *= m_nitro_fuel_factor;
	m_fuel_consumption_current = 50.f * m_fuel_consumption * gas * speed * rpm;

	m_fuel -= time_delta * m_fuel_consumption_current;

	if(m_fuel<EPS) StopEngine();
}

float CCar::AddFuel(float ammount)
{
	float free_space=m_fuel_tank-m_fuel;
	if(ammount < free_space)
	{
		m_fuel+=ammount;
		return ammount;
	}
	else
	{
		m_fuel=m_fuel_tank;
		return free_space;
	}
}

void CCar::Refuel()
{
	m_fuel = m_fuel_tank;
}

void CCar::ResetKeys()
{
	bkp=false;
	fwp=false;
	lsp=false;
	rsp=false;
	fwpbkp=false;
}

#undef   _USE_MATH_DEFINES

void CCar::OnEvent(NET_Packet& P, u16 type)
{
	inherited::OnEvent		(P,type);
	CExplosive::OnEvent		(P,type);

	if (type == GE_GRENADE_EXPLODE)
	{
		// CExplosive::OnEvent has already collected the blast objects by now.
		// Drop the trunk after that collection, so the items are not thrown away.
		DropTrunkContents();
	}

	// Trunk UI transfers arrive as GE_TRADE_BUY, while spawn/load uses
	// GE_OWNERSHIP_TAKE. Register ammo in both paths so turrets can use it.
	auto AppendAmmoToWeapons = [this](CObject* Object)
	{
		CWeaponAmmo* Ammo = smart_cast<CWeaponAmmo*>(Object);
		if (Ammo == nullptr)
		{
			return;
		}

		for (CCarTurret* Turret : m_car_weapons)
		{
			Turret->AppendAmmoToGuns(Ammo);
		}
	};

	u16 id;
	switch (type) 
	{
		case GE_TRADE_BUY:
			{
				P.r_u16(id);
				if (CObject* O = Level().Objects.net_Find(id))
				{
					CGameObject* GO = O->cast_game_object();
					CInventoryItem* pIItem = GO ? GO->cast_inventory_item() : nullptr;
					const bool hasCapacity = pIItem && (GetInventory()->CalcTotalWeight() + GetInventory()->CalcItemWeight(pIItem) <= GetInventory()->GetMaxWeight() + EPS_S);
					if (hasCapacity && GetInventory()->CanTakeItem(pIItem))
					{
						pIItem->m_ItemCurrPlace.type = eItemPlaceRuck;
						O->H_SetParent(this);
						AppendAmmoToWeapons(O);
						inventory().Take(GO, true, true);
					}
					else if (GO)
					{
						NET_Packet Reject;
						u_EventGen(Reject, GE_OWNERSHIP_REJECT, ID());
						Reject.w_u16(u16(GO->ID()));
						u_EventSend(Reject);
					}
				}
			}break;
		case GE_TRADE_SELL:
			{
				P.r_u16(id);
				if (CObject* O = Level().Objects.net_Find(id))
				{
					bool just_before_destroy = !P.r_eof() && P.r_u8();
					O->SetTmpPreDestroy(just_before_destroy);
					if (inventory().DropItem(O->cast_game_object(), just_before_destroy, true) && !O->getDestroy())
						O->H_SetParent(nullptr, just_before_destroy);
				}
			}break;
		case GE_OWNERSHIP_TAKE : 
			{
				P.r_u16(id);
				if(CObject* O	= Level().Objects.net_Find	(id))
				{
					if(smart_cast<CCustomRocket*>(O))
						CRocketLauncher::AttachRocket(id, this);
					else
					{
						CInventoryItem* Item = smart_cast<CInventoryItem*>(O);
						CGameObject* GameObject = smart_cast<CGameObject*>(O);
						if (Item != nullptr && GameObject != nullptr && GetInventory()->CanTakeItem(Item))
						{
							bool duringSpawn = !P.r_eof() && P.r_u8();
							O->H_SetParent(this);
							AppendAmmoToWeapons(O);
							inventory().Take(GameObject, false, false, duringSpawn);
						}
						else 
						{
							if (!O->H_Parent() || (this != O->H_Parent())) return;
							NET_Packet Reject;
							u_EventGen(Reject,GE_OWNERSHIP_REJECT,ID());
							Reject.w_u16(u16(O->ID()));
							u_EventSend(Reject);
						}
					}
				}
			}break;
		case GE_OWNERSHIP_REJECT : 
		case GE_LAUNCH_ROCKET : 
			{
				P.r_u16		(id);
				if(CObject* O	= Level().Objects.net_Find	(id))
				{
					if(smart_cast<CCustomRocket*>(O))
						CRocketLauncher::DetachRocket(id, !!(type==GE_LAUNCH_ROCKET));
					else
					{
						bool just_before_destroy		= !P.r_eof() && P.r_u8();
						O->SetTmpPreDestroy				(just_before_destroy);
						if (inventory().DropItem(smart_cast<CGameObject*>(O), false, false) && !O->getDestroy())
							O->H_SetParent(nullptr, just_before_destroy);
					}
				}
			}break;
	}
}

void CCar::ResetScriptData(void	*P)
{
	CScriptEntity::ResetScriptData(P);
}
void CCar::UpdateBackForward()
{
	if(b_breaks)
	{
		b_burnout = false;
		fwpbkp = false;
		float k=1.f;
		float time=(Device.fTimeGlobal-m_break_start);
		if(time<m_break_time)
		{
			k*=(time/m_break_time);
		}
		for (auto& i : m_breaking_wheels)
		{
				i.Break(k);
		}
		if((fwp && GetVelocity().dotproduct(Fvector(XFORM().k).invert())<EPS_L))// || (fwp && brp))
		{
			StopBreaking();
			DriveForward();
		}
		if(bkp && GetVelocity().dotproduct(XFORM().k)<EPS_L)// || (bkp && brp))
		{
			StopBreaking();
			DriveBack();
		}
	}
}

void CCar::Burnout()
{
	DriveForward();
	for (auto& i : m_breaking_wheels)
	{
		if (!i.pwheel->WheelFlags.test(SWheel::eWheelType::wheel_drive))
		{
			i.HandBreak(1.f);
		}
	b_breaks = true;
	}
	bool burn = false;
	for (auto& WheelDrive : m_driving_wheels)
	{
		if (WheelDrive.pwheel->collision_params.buxe)
		{
			burn = true;
		}
	}
	b_burnout = burn;
}

collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);


void CCar::UpdateRecursiveDoors()
{
	if(b_start_open_recursive)
	{
		for (int i = 0; i < m_doors_recurs.size(); ++i)
		{
			auto prev_dit = m_doors_recurs.find(i-1);
			auto curr_dit = m_doors_recurs.find(i);
			auto last_dit = m_doors_recurs.find(m_doors_recurs.size()-1);

			if(!curr_dit->second->IsDistOfSight(Device.vCameraPosition) || !curr_dit->second->IsLineOfSight())
			{
				b_start_open_recursive = false;
				break;
			}

			if (last_dit != m_doors_recurs.end())
			{
				if(last_dit->second->state == SDoor::opened)
				{
					ScriptUseTrunk(m_bone_trunk);
					b_start_open_recursive = false;
					b_all_recurs_opened = true;
					break;
				}

			}

			if(curr_dit != m_doors_recurs.end())
			{
				if(curr_dit->second->state == SDoor::closed || curr_dit->second->state == SDoor::closing)
				{
					if (prev_dit != m_doors_recurs.end())
					{
						if(prev_dit->second->state == SDoor::opened)
							curr_dit->second->Open();

					}
					else
						curr_dit->second->Open();
				}
			}
		}
	}

	if(b_start_close_recursive)
	{
		for (int i=m_doors_recurs.size()-1; i>=0; --i)
		{
			auto prev_dit = m_doors_recurs.find(i+1);
			auto curr_dit = m_doors_recurs.find(i);
			auto last_dit = m_doors_recurs.find(0);

			if(!curr_dit->second->IsDistOfSight(Device.vCameraPosition) || !curr_dit->second->IsLineOfSight())
			{
				b_start_close_recursive = false;
				break;
			}

			if (last_dit != m_doors_recurs.end())
			{
				if(last_dit->second->state == SDoor::closed)
				{
					b_start_close_recursive = false;
					b_all_recurs_opened = false;
					break;
				}

			}

			if(curr_dit != m_doors_recurs.end())
			{
				if(curr_dit->second->state == SDoor::opened || curr_dit->second->state == SDoor::opening)
				{
					if (prev_dit != m_doors_recurs.end())
					{
						if(prev_dit->second->state == SDoor::closed)
							curr_dit->second->Close();

					}
					else
						curr_dit->second->Close();
				}
			}
		}
	}
}





void CCar::PhDataUpdate(float step)
{
	UpdatePhDataObjects();
	UpdateEngineAndBrakes(step);
	UpdateDoorState();
	UpdateTransmissionState();
	UpdateSlipSliding();
	UpdateWheelieBoost();
	UpdateDriftBoost();
	UpdateEngineShake();
	UpdateSteeringWheels();

	const bool Slip = UpdateWheelPhysics();
	UpdateTractionAndSuspension(Slip);
}

void CCar::UpdatePhDataObjects()
{
	if (!getDestroy() && !b_exploded && b_event_drag_activated)
	{
		if (!OwnerActor())
		{
			Fvector position_in_bone_space;
			Fmatrix m_inv;
			m_inv.invert(XFORM());
			m_inv.transform_tiny(position_in_bone_space, Device.vCameraPosition_saved);
			float impulse_factor = m_pPhysicsShell->getMass() * .5f + 100.f + g_actor->character_physics_support()->movement()->GetMass();
			m_pPhysicsShell->applyImpulseTrace(position_in_bone_space, Device.vCameraDirection_saved, impulse_factor);
		}
		b_event_drag_activated = false;
	}

	if (characters_scraper_shell)
	{
		characters_scraper_shell->Enable();
		characters_scraper_shell->SetTransform(XFORM(), mh_clear);
		characters_scraper_shell->set_LinearVel(zero_vel);
		characters_scraper_shell->set_AngularVel(zero_vel);
		characters_scraper_shell->Update();
	}

	for (CCarTurret* CWPB : m_car_weapons)
	{
		CWPB->PhDataUpdate();
	}

	for (auto& it : engine_door)
	{
		if (!it.joint || !it.joint->bActive) continue;

		float lo_limit, hi_limit;
		it.joint->GetLimits(lo_limit, hi_limit, 0);
		if (it.joint->GetAxisAngle(0) > lo_limit && it.joint->GetAxisAngle(0) < hi_limit)
		{
			it.joint->PSecond_element()->Enable();
		}
	}

	for (auto& it : headlight_hideaways)
	{
		if (!it.joint || !it.joint->bActive) continue;

		float lo_limit, hi_limit;
		it.joint->GetLimits(lo_limit, hi_limit, 0);
		if (it.joint->GetAxisAngle(0) > lo_limit && it.joint->GetAxisAngle(0) < hi_limit)
		{
			it.joint->PSecond_element()->Enable();
		}
	}
}

void CCar::UpdateEngineAndBrakes(float step)
{
	if (m_repairing)Revert();
	// Debug telemetry is accumulated per physics step. A hinge2 joint has a
	// single motor for the drive axis, so engine and brake torque are two
	// requests for the same dParamFMax2 and only one of them stays applied.
	for (auto& Wheel : m_wheels_map)
	{
		Wheel.second.EngineTorque = 0.f;
		Wheel.second.BrakeTorque = 0.f;
	}
	UpdateFuel(step);
	UpdatePower();
	//if (GetVelocityM() > m_last_velocity)
	//else

	m_acceleration = (GetVelocityM() - m_last_velocity) / fixed_step;

	if (m_nitro)
	{
		if (b_nitro_engage && e_state_drive == drive && Device.fTimeGlobal - m_nitro_engage_time > 1.f && !b_starting)
		{
			if (!m_nitro_engage)
			{
				m_car_sound->NitroStart();
				if (b_nitro_particles_enabled)
					PlayExhaustsNitro();
			}
			m_nitro_engage = 1;
		}
		else
		{
			if (m_nitro_engage)
			{
				m_car_sound->NitroStop();
				if (b_nitro_particles_enabled)
					StopExhaustsNitro();
			}
			m_nitro_engage = 0;
		}
	}
	if (fwp && bkp)
	{
		fwpbkp = true;
		Burnout();
	}
	else
		if (fwp || bkp)
			UpdateBackForward();
		else
			b_breaks = false;

	if (brp)
		HandBreak();

	if (brp || (fwp && bkp) || b_breaks)
		break_start_dt += Device.fTimeDelta;
	else
		break_start_dt = 0.f;
	clamp(break_start_dt, 0.0f, 1.0f);
}

void CCar::UpdateDoorState()
{
	//////////////////////////////////////////////////////////
	for (int k = 0; k < (int)m_doors_update.size(); ++k)
	{
		SDoor* D = m_doors_update[k];
		if (!D->update)
		{
			m_doors_update.erase(m_doors_update.begin() + k);
			--k;
		}
		else
		{
			D->Update();
		}
	}
	UpdateRecursiveDoors();
}

void CCar::UpdateTransmissionState()
{
	// TRANS
	float delay = m_transmission_switch_delay;
	if (m_nitro_engage)
		delay *= m_nitro_switch_boost;
	if (Device.fTimeGlobal > m_transmission_switch_time + delay && IsTransmissionSwitching())
	{
		if (m_transmission_stage == ETransmissionStage::SwitchUp)
			m_sound_effect = 0.f;
		m_transmission_stage = ETransmissionStage::Drive;
		m_car_sound->Gas();
		if (e_state_drive == drive && m_sound_state_after_switch && Device.fTimeGlobal - m_engine_drive_time > 1.1f * delay)
		{
			clamp(m_engine_state_scale_last, m_sound_state_after_switch, m_engine_state_scale_last);
		}
	}
}

void CCar::UpdateSlipSliding()
{
	// TRANS
	Fvector diff_dir, diff_dir2, normalized_vel, side_dir, side_dir2;
	normalized_vel = Fvector(GetVelocity());
	safe_normalize(normalized_vel);
	side_dir = Fvector(XFORM().i);
	side_dir2 = Fvector(XFORM().i).invert();
	diff_dir.sub(side_dir, normalized_vel);
	diff_dir2.sub(side_dir2, normalized_vel);
	m_current_slip_sliding = _abs(diff_dir.magnitude() - diff_dir2.magnitude());
	if (GetVelocity().dotproduct(XFORM().k) < 0.f)
		m_current_slip_sliding *= 0.75f;
	if (GetSpeedKmH() < 1.f)
		m_current_slip_sliding = 0.f;
	clamp(m_current_slip_sliding, 0.f, 3.f);
}

void CCar::UpdateWheelieBoost()
{
	if (GetVelocity().dotproduct(XFORM().k) > 0.f && ((m_wheelie_boost && e_state_drive == drive && !b_starting) || (m_nitro_engage && m_nitro_wheelie_boost)) && !brp && !IsTransmissionSwitching() && !CurrentTransmission() == 0 && !fwpbkp && mb_traction)
	{
		float spd = GetSpeedKmH() / 100.f;
		float boost = m_wheelie_boost;
		if (m_nitro_engage)
		{
			boost = m_nitro_wheelie_boost;
			spd = spd / 2.f;
		}

		clamp(spd, 0.f, 1.f);
		spd = sqrtf(spd);

		float spin_time = (Device.fTimeGlobal - m_slip_start_time) - 0.3f;
		clamp(spin_time, 0.f, 1.f);
		float rotate = boost * fixed_step * (1.f - spd) * (1.f - spin_time);
		if (m_pPhysicsShell && m_pPhysicsShell->isActive())
		{
			if (m_pPhysicsShell->get_ElementByStoreOrder(0))
			{
				Fvector angular_velocity = GetAvelAxes(0);
				angular_velocity.x -= rotate;
				SetAvelAxes(0, angular_velocity);
			}
		}
	}
}

void CCar::UpdateDriftBoost()
{
	if (m_pPhysicsShell && m_pPhysicsShell->isActive() && OwnerActor() && m_drift_start_boost && brp && !b_starting)
		if (b_drift_boost && Device.fTimeGlobal - m_handbreak_start < m_drift_boost_time)
		{
			if (m_pPhysicsShell->get_ElementByStoreOrder(0))
			{
				Fvector angular_velocity = GetAvelAxes(0);

				float spd = GetSpeedKmH() * 0.01f;
				clamp(spd, 0.f, 1.f);
				spd = sinf(0.5f * PI * powf(spd, 0.5f));

				if (e_state_steer == right && angular_velocity.y > 0.f)
				{
					angular_velocity.y += m_drift_start_boost * spd * fixed_step;
					SetAvelAxes(0, angular_velocity);
				}
				if (e_state_steer == left && angular_velocity.y < 0.f)
				{
					angular_velocity.y -= m_drift_start_boost * spd * fixed_step;
					SetAvelAxes(0, angular_velocity);
				}
			}
		}
		else
			b_drift_boost = false;
	else
		b_drift_boost = false;
}

void CCar::UpdateEngineShake()
{
	if (m_engine_shake_boost && mb_traction && b_engine_on && brp && !b_starting)
	{
		float gas = 1.f;
		if (e_state_drive != drive)
			gas = 0.f;
		float twist = sqrtf(m_engine_rpm_scale) * gas;
		float spd = 1.5f - GetSpeedKmH() * 0.15f;
		clamp(spd, 0.f, 1.f);
		//if (!m_shake_stage)
		//	if (m_engine_current >= 1.f)
		//else
		//	if (m_engine_current <= -1.f)
		if (m_pPhysicsShell && m_pPhysicsShell->isActive())
		{
			if (m_pPhysicsShell->get_ElementByStoreOrder(0))
			{
				float rotate = twist * spd * m_engine_shake_boost;
				Fvector angular_velocity = GetAvelAxes(0);
				angular_velocity.z -= rotate * fixed_step * FixedStepCompensation;
				SetAvelAxes(0, angular_velocity);
			}
		}
	}
}

void CCar::UpdateSteeringWheels()
{
	if (e_state_steer == left)
	{
		for (auto& i : m_steering_wheels)
		{
			i.SteerLeft();
		}
	}
	if (e_state_steer == right)
	{
		for (auto& i : m_steering_wheels)
		{
			i.SteerRight();
		}
	}
	if (e_state_steer == idle)
	{
		for (auto& i : m_steering_wheels)
		{
			i.SteerIdle();
		}
	}

	if (!m_steering_wheels.empty())
	{
		m_steer_angle = m_steer_angle + (m_steering_wheels.begin()->GetSteerAngle() - m_steer_angle) * 15.f * fixed_step;
		m_wheel_angle = m_steering_wheels.begin()->GetSteerAngle();
		VERIFY(_valid(m_steer_angle));
		VERIFY(_valid(m_wheel_angle));
	}

	//for (; i_b != e_b; ++i_b)
	//	if (i_b->second.WheelFlags.test(SWheel::eWheelType::wheel_steering))
	//	else
	//	if (Device.dwTimeGlobal - i_b->second.collision_params.last_contact_time > 50)
	//if (steer_size > 0)
	//if (nonsteer_size > 0)
}

bool CCar::UpdateWheelPhysics()
{
	bool Slip = false;

	if (OwnerActor())
		PKinematics(Visual())->CalculateBones(TRUE);

	const float RainFrictionFactor = m_wheels_map.empty() ? 1.f : CalculateRainFrictionFactor();

	for (auto& WheelEntry : m_wheels_map)
	{
		SWheel& Wheel = WheelEntry.second;

		if (Wheel.joint && Wheel.joint->bActive)
		{
			UpdateWheelSuspension(Wheel);
			UpdateWheelDownforce(Wheel);
		}

		UpdateWheelTireSound(Wheel);

		if (Wheel.joint && Wheel.joint->bActive && Device.dwTimeGlobal - m_last_contact_time < 500)
		{
			Slip |= UpdateWheelTraction(Wheel);
		}

		UpdateWheelTractionTimeout(Wheel);
		UpdateWheelAirborneFriction(Wheel);
		Wheel.collision_params.friction_rain_factor = RainFrictionFactor;
	}

	return Slip;
}

void CCar::UpdateWheelSuspension(SWheel& Wheel)
{
	if (Wheel.suspension_force_inc > 0.f)
	{
		Fmatrix bone_bxform = bones_bind_forms[Wheel.bone_id];
		Fmatrix bone_xform = PKinematics(Visual())->LL_GetTransform(Wheel.bone_id);

		float spring_ = Wheel.spring_factor_default;
		float damping_ = Wheel.damping_factor_default;
		float median = (spring_ + damping_) * 0.5f;

		if (bone_bxform.c.distance_to(bone_xform.c) > Wheel.suspension_distance)
		{
			float spring_req = spring_ + (bone_bxform.c.distance_to(bone_xform.c) - Wheel.suspension_distance) * (spring_ / median) * Wheel.suspension_spring_factor * Wheel.suspension_force_inc;
			float damping_req = damping_ + (bone_bxform.c.distance_to(bone_xform.c) - Wheel.suspension_distance) * (damping_ / median) * Wheel.suspension_damping_factor * Wheel.suspension_force_inc;

			if (Wheel.spring_last < spring_req)
			{
				spring_ = Wheel.suspension_inertia * Wheel.spring_last + (1.f - Wheel.suspension_inertia) * spring_req;
				damping_ = Wheel.suspension_inertia * Wheel.damping_last + (1.f - Wheel.suspension_inertia) * spring_req;
			}
			else
			{
				spring_ = spring_req;
				damping_ = damping_req;
			}
		}
		Wheel.spring_last = spring_;
		Wheel.damping_last = damping_;


		Wheel.joint->SetJointSDfactors(spring_, damping_);
	}
	else
	{
		Wheel.spring_last = Wheel.spring_factor_default;
		Wheel.damping_last = Wheel.damping_factor_default;
	}
}

void CCar::UpdateWheelDownforce(SWheel& Wheel)
{
	if (m_downforce)
		if (GetSpeedKmH() > 1.f && GetVelocity().dotproduct(XFORM().k) > 0.f)
		{
			float speedcar = GetSpeedKmH() * 0.00002f * m_downforce;
			float direction_mult;
			Fvector diff_dir, diff_dir2, normalized_vel, side_dir, side_dir2;
			normalized_vel = Fvector(GetVelocity());
			safe_normalize(normalized_vel);
			side_dir = Fvector(XFORM().k);
			side_dir2 = Fvector(XFORM().k).invert();
			diff_dir.sub(side_dir, normalized_vel);
			diff_dir2.sub(side_dir2, normalized_vel);
			direction_mult = 0.5f * _abs(diff_dir.magnitude() - diff_dir2.magnitude());
			clamp(direction_mult, 0.f, 1.f);
			float air_mult = 1.5f - float(Device.dwTimeGlobal - Wheel.collision_params.last_contact_time) * 0.001f;
			clamp(air_mult, 0.f, 1.f);
			m_current_downforce = speedcar * air_mult * direction_mult;
			m_pPhysicsShell->applyImpulseTrace(bones_bind_forms[Wheel.bone_id].c, Fvector(XFORM().j).invert(), ((m_pPhysicsShell->getMass() / m_wheels_map.size()) * physics_world()->Gravity() * m_current_downforce) * fixed_step);
		}
		else
			m_current_downforce = 0.f;
}

void CCar::UpdateWheelTireSound(SWheel& Wheel)
{
	if (!mb_traction && Wheel.tire_drive._p)
	{
		Wheel.UpdateSounds();
		Wheel.tire_drive.stop();
	}
}

bool CCar::UpdateWheelTraction(SWheel& Wheel)
{
	bool WheelSlip = false;
	if (Wheel.joint && Wheel.joint->bActive && !!(Device.dwTimeGlobal - m_last_contact_time < 500))// && GetAVelocityM() > 5.0f)
	{
		float wheel_speed = fabsf(Wheel.GetDriveAngleRate()) * Wheel.radius;
		Fvector wheel_lin_vel;
		Wheel.joint->PSecond_element()->get_LinearVel(wheel_lin_vel);
		float wheel_lin_speed = wheel_lin_vel.magnitude();

		float spd_dif = fabsf(wheel_speed - wheel_lin_speed) * 0.015f;
		clamp(spd_dif, 0.f, 1.f);
		spd_dif = sinf(0.5f * PI * powf(spd_dif, 0.5f));

		if (Wheel.collision_params.traction)
		{
			float threshold = 2.f;
			float wheels_spin_factor = m_wheels_spin_factor_start;

			if (Wheel.collision_params.buxe || Wheel.collision_params.breaking || Wheel.collision_params.slide)
			{
				threshold = 0.5f;
				wheels_spin_factor = m_wheels_spin_factor_stop;
				WheelSlip = true;
			}


			if (e_state_drive == drive && Wheel.WheelFlags.test(SWheel::eWheelType::wheel_drive) && Wheel.WheelFlags.test(SWheel::eWheelType::wheel_steering))
				wheels_spin_factor *= 0.95f;

			if (wheel_speed > threshold && wheel_lin_speed < wheel_speed * wheels_spin_factor && e_state_drive == drive && Wheel.WheelFlags.test(SWheel::eWheelType::wheel_drive)) //_abs(Wheel.GetDriveAngleRate()) * Wheel.radius < GetVelocityM() * 0.6f
			{
				Wheel.collision_params.buxe = true;
				Wheel.collision_params.slide = false;
				Wheel.collision_params.breaking = false;
			}
			else
			{
				Wheel.collision_params.buxe = false;
				if ((wheel_lin_speed > m_vel_sliding_threshold && wheel_speed < wheel_lin_speed * wheels_spin_factor))
					Wheel.collision_params.breaking = true;
				else
				{
					Wheel.collision_params.breaking = false;
					if ((wheel_lin_speed >= m_vel_sliding_threshold) && (m_current_slip_sliding >= m_slip_sliding_threshold) && ((Wheel.wheel_state != wheel_left && Wheel.wheel_state != wheel_right && Wheel.wheel_state != wheel_limit)))
						Wheel.collision_params.slide = true;
					else
						Wheel.collision_params.slide = false;
				}
			}

			if (Wheel.WheelFlags.test(SWheel::eWheelType::wheel_breaking && wheel_lin_speed > m_vel_sliding_threshold && wheel_speed < wheel_lin_speed * m_break_abs_threshold)) //breaking abs
				Wheel.collision_params.breaking_abs = true;
			else
				Wheel.collision_params.breaking_abs = false;
		}
		else
		{
		Wheel.collision_params.buxe = false;
		Wheel.collision_params.slide = false;
		Wheel.collision_params.breaking = false;
		}

		if ((Wheel.collision_params.buxe || (Wheel.collision_params.slide && e_state_drive == drive && Wheel.WheelFlags.test(SWheel::eWheelType::wheel_drive))) && (!m_nitro_engage || (m_nitro_engage && !m_nitro_wheelie_boost)))
		{
			Wheel.collision_params.friction_slip_factor = (1.f - m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation) * Wheel.collision_params.friction_slip_factor + m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation * (1.f - (0.5f + 0.5f * spd_dif) * (1.f - m_friction_spin_factor));
			Wheel.collision_params.friction2_slip_factor = (1.f - m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation) * Wheel.collision_params.friction2_slip_factor + m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation * (1.f - (0.5f + 0.5f * spd_dif) * (1.f - m_friction2_spin_factor));
			Wheel.collision_params.was_buxe = true;
			Wheel.collision_params.was_slide = false;
		}
		else if (Wheel.collision_params.breaking || Wheel.collision_params.slide)
		{
			Wheel.collision_params.friction_slip_factor = (1.f - m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation) * Wheel.collision_params.friction_slip_factor + m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation * (1.f - (0.5f + 0.5f * spd_dif) * (1.f - m_friction_slip_factor));
			Wheel.collision_params.friction2_slip_factor = (1.f - m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation) * Wheel.collision_params.friction2_slip_factor + m_friction_slip_speed * 5.f * fixed_step * FixedStepCompensation * (1.f - (0.5f + 0.5f * spd_dif) * (1.f - m_friction2_slip_factor));
			Wheel.collision_params.was_slide = true;
		}
		else
		{
			Wheel.collision_params.friction_slip_factor += m_friction_slip_speed * (1.f - (m_friction_spin_factor < m_friction_slip_factor ? m_friction_spin_factor : m_friction_slip_factor)) * fixed_step * 350.f;
			Wheel.collision_params.friction2_slip_factor += m_friction_slip_speed * (1.f - (m_friction2_spin_factor < m_friction2_slip_factor ? m_friction2_spin_factor : m_friction2_slip_factor)) * fixed_step * 350.f;
		}

		if (Wheel.collision_params.was_buxe == true && Wheel.collision_params.was_slide == true)
		{
			clamp(Wheel.collision_params.friction_slip_factor, 1.5f, 1.5f);
			clamp(Wheel.collision_params.friction2_slip_factor, 1.5f, 1.5f);
			Wheel.collision_params.was_buxe = false;
		}
		else
		{
			clamp(Wheel.collision_params.friction_slip_factor, Wheel.collision_params.friction_slip_factor, 1.f);
			clamp(Wheel.collision_params.friction2_slip_factor, Wheel.collision_params.friction2_slip_factor, 1.f);
			Wheel.collision_params.was_slide = false;
		}

		if (Wheel.WheelFlags.test(SWheel::eWheelType::wheel_drive) && m_nitro)
		{
			if (m_nitro_engage)
				if (m_nitro_wheelie_boost && !fwpbkp && !brp)
				{
					float spd_brp = (GetSpeedKmH() - 35.f) / 30.f;
					clamp(spd_brp, 0.f, 1.f);
					Wheel.collision_params.friction_nitro_factor = m_nitro_friction_factor * (1.f + (m_current_rpm_brp_n - 1.f) * (1.f - spd_brp) * m_nitro_wheelie_friction_brp_factor);
				}
				else
					Wheel.collision_params.friction_nitro_factor = m_nitro_friction_factor;
			else
			{
				Wheel.collision_params.friction_nitro_factor = 1.f;
			}
		}

	}
	return WheelSlip;
}

void CCar::UpdateWheelTractionTimeout(SWheel& Wheel)
{
	if (Device.dwTimeGlobal - Wheel.collision_params.last_contact_time > 50)
		Wheel.collision_params.traction = false;
}

void CCar::UpdateWheelAirborneFriction(SWheel& Wheel)
{
	// An airborne wheel has no contact friction. Apply the axle friction
	// explicitly so engine torque cannot spin it forever while it hangs
	// in the air. Braking already supplies a stronger wheel torque.
	const bool Airborne = !Wheel.collision_params.traction;
	if (Airborne && !b_breaks)
	{
		Wheel.Neutral();
		Wheel.AirborneFriction = true;
	}
	else if (!Airborne && Wheel.AirborneFriction && !b_breaks)
	{
		// Neutral() left the motor target at zero. Restore it when the
		// wheel touches the ground again, otherwise it would be braked
		// until the next drive/transmission input.
		Wheel.AirborneFriction = false;
		if (e_state_drive == drive && Wheel.WheelFlags.test(SWheel::eWheelType::wheel_drive))
		{
			for (auto& D : m_driving_wheels)
			{
				if (D.pwheel == &Wheel)
				{
					D.Drive();
					break;
				}
			}
		}
	}
}

float CCar::CalculateRainFrictionFactor()
{
	float rain_friction = 0.f;
	if (g_pGamePersistent->Environment().CurrentEnv->rain_density > 0.f)
	{
		float rain_hemi = 1.f;
		rain_friction = 0.25f * g_pGamePersistent->Environment().CurrentEnv->rain_density;
		clamp(rain_friction, 0.f, 1.f);
		rain_friction = 0.25f * sinf(0.5f * PI * powf(rain_friction, 0.8f));
		rain_hemi = ROS()->get_luminocity_hemi() * 100.f;
		clamp(rain_hemi, 0.f, 1.f);
		rain_friction -= (1.f - (1.f + 0.65f * (rain_hemi - 1.f)));
		clamp(rain_friction, 0.f, 1.f);
	}
	return 1.f - rain_friction;
}

void CCar::UpdateTractionAndSuspension(bool slip)
{
	if (mb_traction && Device.dwTimeGlobal - m_last_contact_time > 50)
		mb_traction = false;
	if (!slip)
		m_slip_start_time = Device.fTimeGlobal;

	for (auto& Suspension : suspension_joints)
	{
		Suspension.Update();
	}

	m_last_velocity = GetVelocityM();
	m_last_avelocity_j = GetAVelocity().dotproduct(XFORM().j);
}
bool CCar::UsedAI_Locations()
{
	return					(FALSE);
}

u16 CCar::DriverAnimationType()
{
	return m_driver_anim_type;
}

void CCar::OnAfterExplosion()
{
	DropTrunkContents();
}

void CCar::OnBeforeExplosion()
{
	setEnabled(FALSE);
}

void CCar::CarExplode()
{
	if (OwnerActor() && rs_car_god)
		return;

	if(b_exploded) return;

	for (auto& i_w : m_wheels_map)
	{
		if (i_w.second.tire_drive._p)
			i_w.second.tire_drive.destroy();
	}
	CPHSkeleton::SetNotNeedSave();
	for (CCarTurret* CWPB : m_car_weapons)
	{
		CWPB->Action(CCarTurret::eWpnActivate, 0);
	}
	m_lights.TurnOffLights();
	m_stoplights.TurnOffLights();
	m_indoorlights.TurnOffLights();
	m_enginelights.TurnOffLights();
	m_reverselights.TurnOffLights();
	b_exploded=true;
	b_event_drag_activated = false;
	CExplosive::GenExplodeEvent(Position(),Fvector().set(0.f,1.f,0.f));
	if(CPHDestroyable::CanDestroy())
		CPHDestroyable::Destroy(ID(),"physic_destroyable_object");

	m_car_sound->Destroy();
	m_damage_particles.Stop1(this);
	m_damage_particles.Stop2(this);
	
	RemoveGrassSpheres();
}

//void CCar::object_contactCallbackFun(bool& do_colide,dContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/)
//
//
//
//	if(capturer)
//		if(capture)
//			if(capture->m_taget_element->PhysicsRefObject()==l_pUD2->ph_ref_object)
//
//
//
//
//	if(capturer)
//		if(capture)
//			if(capture->m_taget_element->PhysicsRefObject()==l_pUD1->ph_ref_object)
//
//

template <class T> IC void CCar::fill_wheel_vector(LPCSTR S,xr_vector<T>& type_wheels)
{
	IKinematics* pKinematics	=smart_cast<IKinematics*>(Visual());
	string64					S1;
	int count =					_GetItemCount(S);
	for (int i=0 ;i<count; ++i) 
	{
		_GetItem					(S,i,S1);

		u16 bone_id	=				pKinematics->LL_BoneID(S1);

		type_wheels.push_back		(T());
		T& twheel				= type_wheels.back();

		if (bone_map.find(bone_id) == bone_map.end())
		{
			bone_map.insert(std::make_pair(bone_id,physicsBone()));

			SWheel& wheel			=	(m_wheels_map.insert(std::make_pair(bone_id,SWheel(this)))).first->second;
			wheel.bone_id			=	bone_id;
			twheel.pwheel			=	&wheel;
			wheel						.Load(S1);
			twheel						.Load(S1);
		}
		else
		{
			twheel.pwheel			=	&(m_wheels_map.find(bone_id))->second;
			twheel						.Load(S1);
		}
	}
}

IC void CCar::fill_exhaust_vector(LPCSTR S,xr_vector<SExhaust>& exhausts)
{
	IKinematics* pKinematics	=smart_cast<IKinematics*>(Visual());
	string64					S1;
	int count =					_GetItemCount(S);
	for (int i=0 ;i<count; ++i) 
	{
		_GetItem					(S,i,S1);

		u16 bone_id	=				pKinematics->LL_BoneID(S1);

		exhausts.push_back		(SExhaust(this));
		SExhaust& exhaust				= exhausts.back();
		exhaust.bone_id						= bone_id;
		if (bone_map.find(bone_id) == bone_map.end())
			bone_map.insert(std::make_pair(bone_id,physicsBone()));
	}
}

IC void CCar::fill_doors_map(LPCSTR S,xr_map<u16,SDoor>& doors)
{
	IKinematics* pKinematics	=smart_cast<IKinematics*>(Visual());
	string64					S1;
	int count =					_GetItemCount(S);
	for (int i=0 ;i<count; ++i) 
	{
		_GetItem					(S,i,S1);

		u16 bone_id	=				pKinematics->LL_BoneID(S1);
		SDoor						door(this);
		door.bone_id=				bone_id;
		doors.insert				(std::make_pair(bone_id,door));
		if (bone_map.find(bone_id) == bone_map.end())
			bone_map.insert(std::make_pair(bone_id,physicsBone()));
	}
}

DLL_Pure *CCar::_construct			()
{
	inherited::_construct		();
	CScriptEntity::_construct	();
	CInventoryOwner::_construct		();
	return						(this);
}

ALife::_OBJECT_ID CCar::Initiator()
{
	if(g_Alive() && Owner())
	{
		return Owner()->ID();
	}
	else return ID()	;
}

float	CCar::RefWheelMaxSpeed()
{
	if (fabsf(m_current_gear_ratio) < EPS || m_gear_ratious.empty())
		return 0.f;

	if (!b_cvt_transmission)
		return m_max_rpm / m_current_gear_ratio;

	const size_t ReferenceGear = m_gear_ratious.size() > 2 ? 2 : m_gear_ratious.size() - 1;
	return ((m_max_rpm / m_current_gear_ratio) * fabsf(m_current_gear_ratio)) / m_gear_ratious[ReferenceGear][0];
}

float	CCar::RefWheelMaxSpeedByLastGear()
{
	if(m_gear_ratious.size())
		return m_max_rpm/m_gear_ratious[m_gear_ratious.size()-1][0];

	return 1.0f;
}

float	CCar:: EngineCurTorque()
{
	return m_current_rpm > EPS ? m_current_engine_power / m_current_rpm : 0.f;
}
float	CCar:: RefWheelCurTorque()
{
	return EngineCurTorque()*((m_current_gear_ratio<0.f) ? -m_current_gear_ratio : m_current_gear_ratio);
}
void CCar::GetRayExplosionSourcePos(Fvector &pos)
{
	random_point_in_object_box(pos,this);
}
void CCar::net_Relcase(CObject* O)
{
	CExplosive::net_Relcase(O);
	inherited::net_Relcase(O);
	if(m_memory)
		m_memory->remove_links(O);
}

void CCar::ASCUpdate()
{
	for(u16 i=0;i<cAsCallsnum;++i)
	{
		EAsyncCalls c=EAsyncCalls(1<<i);
		if(async_calls.test(u16(c)))ASCUpdate(c);
	}

}

void CCar::ASCUpdate(EAsyncCalls c)
{
	async_calls.set(u16(c),FALSE);
	switch(c) {
	case ascSndTransmission:m_car_sound->TransmissionSwitch();break;
	case ascSndStall:m_car_sound->Stop();break;
	case ascExhoustStop:StopExhausts();break;
	default: NODEFAULT;
	}
}

void CCar::AscCall(EAsyncCalls c)
{
	async_calls.set(u16(c),TRUE);
}

bool CCar::CanRemoveObject()
{
	if (CExplosive::IsExploded())
	{
		// Fallback for the case when OnAfterExplosion is skipped before removal.
		DropTrunkContents();
	}

	return CExplosive::IsExploded()&&!CExplosive::IsSoundPlaying();
}

void CCar::DropTrunkContents()
{
	if (inventory().m_ruck.empty())
	{
		return;
	}

	Fvector drop_base = Position();
	if (IKinematics* kinematics = PKinematics(Visual()))
	{
		if (m_bone_trunk != BI_NONE && m_bone_trunk < kinematics->LL_BoneCount())
		{
			drop_base = TrunkBoneWorldPos();
		}
	}

	const TIItemContainer trunk_items = inventory().m_ruck;
	for (PIItem item : trunk_items)
	{
		if (item == nullptr || item->object().getDestroy())
			continue;

		Fvector drop_position = drop_base;
		drop_position.x += Random.randF(-0.5f, 0.5f);
		drop_position.y += 1.f;
		drop_position.z += Random.randF(-0.5f, 0.5f);

		// Detach the item on the server first. A direct CInventory::DropItem()
		// only updates the local object and leaves CSE_Abstract::ID_Parent
		// pointing to the car, so the server later rejects the item as a car child.
		if (OnServer() && Level().Server != nullptr)
		{
			NET_Packet dummy;
			Level().Server->Process_event_reject(dummy, BroadcastCID, Device.dwTimeGlobal, ID(), item->object_id(), false);
		}

		item->object().XFORM().c.set(drop_position);
		if (!inventory().DropItem(&item->object(), false, false))
		{
			continue;
		}

		// CPhysicItem::activate_physic_shell() uses the old parent's XFORM.
		// Move the freshly created shell to the intended drop position.
		Fmatrix drop_xform = item->object().XFORM();
		drop_xform.c.set(drop_position);
		item->object().XFORM().set(drop_xform);
		if (CPhysicsShell* shell = item->object().PPhysicsShell())
		{
			shell->SetGlTransformDynamic(drop_xform);
			shell->set_LinearVel(Fvector().set(0.f, 0.f, 0.f));
			shell->set_AngularVel(Fvector().set(0.f, 0.f, 0.f));
		}
		item->object().spatial_move();
	}
}

void		CCar::SetExplodeTime				(u32 et)	
{
	CDelayedActionFuse::Initialize(float(et)/1000.f,CDamagableItem::DamageLevelToHealth(2));
}
u32			CCar::ExplodeTime					()			
{
	if(CDelayedActionFuse::isInitialized())
				return u32(CDelayedActionFuse::Time())*1000;
	else return 0;
}

void	CCar::		Die					(CObject* who)
{
	inherited::Die(who);
	CarExplode();
}

Fvector	CCar::		ExitVelocity				()
{
	CPhysicsShell		*P=PPhysicsShell();
	if(!P||!P->isActive())return Fvector().set(0,0,0);
	CPhysicsElement *E=P->get_ElementByStoreOrder(0);
	Fvector v=ExitPosition();
	dBodyGetPointVel(E->get_body(),v.x,v.y,v.z,cast_fp(v));
	return v;
}

void CCar::ShowTrunk()
{
	if (CurrentGameUI())
		CurrentGameUI()->StartCarBody(Actor(), this);
}

void CCar::OpenTrunkBone()
{
	if(m_doors_recurs.empty())
		DoorOpen(m_bone_trunk);
}

void CCar::CloseTrunkBone()
{
	if(m_doors_recurs.empty())
		DoorClose(m_bone_trunk);
	else
	{
		if(m_pPhysicsShell)
			m_pPhysicsShell->Enable();

		b_start_close_recursive = true;
		b_start_open_recursive = false;
	}
}

Fvector CCar::TrunkBoneWorldPos()
{
	if (IKinematics* pKinematics = PKinematics(Visual()))
	{
		if(m_bone_trunk != BI_NONE && m_bone_trunk < pKinematics->LL_BoneCount())
		{
			Fmatrix BoneMatrix;
			Fmatrix BoneMatrixRes;
			Fobb BoneOBB = pKinematics->LL_GetBox(m_bone_trunk);
			BoneOBB.xform_get(BoneMatrix);
			BoneMatrixRes.mul(pKinematics->LL_GetTransform(m_bone_trunk), BoneMatrix);
			BoneMatrix.mul(XFORM(), BoneMatrixRes);
			return BoneMatrix.c;
		}
	}
	return Fvector(Device.vCameraPosition);
}

void CCar::KillEngine()
{
	if (!b_starting) StopEngine();
}

void CCar::HitEngine(CObject* obj, float scale)
{
	// Do not let an anomaly hit kill the engine during the first two seconds
	// after startup. The physics impulse still applies, only the engine damage
	// waits.
	static constexpr u32 EngineHitDelay = 2000;
	const bool EngineDamageAllowed = b_engine_on && !rs_car_god && Device.dwTimeGlobal >= m_dwStartTime + EngineHitDelay;

	if (EngineDamageAllowed)
	{
		m_hit_engine_time = Device.dwTimeGlobal;
		m_current_engine_power -= m_current_engine_power/2.f;
		m_current_rpm -= m_current_rpm/3.f;
		if (m_current_rpm < m_min_rpm)
			KillEngine();
	}
	if (m_pPhysicsShell)
	{
		Fvector dir;
		dir.sub(Position(), obj->Position());
		if (!safe_normalize(dir)) return;
		m_pPhysicsShell->applyImpulse(dir, (1000.f +  0.25f * m_pPhysicsShell->getMass()) * scale);
	}
}
void CCar::SHeadlightHideaway::Switch(bool hideaways_up)
{
	joint->SetForceAndVelocity(force, hideaways_up ? velocity : -velocity, axis_num);
}
void CCar::SEngineDoor::Switch(bool doors_up)
{
	joint->SetForceAndVelocity(force, doors_up ? velocity : -velocity, axis_num);
}
void CCar::SSuspension::Update()
{
	if (b_independent)
		joint->SetForceAndVelocity(force, velocity, axis_num);
	else
	{
		float cur_angle = dJointGetHingeAngle(joint->GetDJoint());
		if (_abs(cur_angle) > EPS_L)
			joint->SetForceAndVelocity(_abs((1000.0f * force) * cur_angle), -cur_angle * velocity, axis_num);
	}
}


Fvector CCar::GetAvelAxes(u16 elem_id)
{
	CPhysicsElement *elem = m_pPhysicsShell->get_ElementByStoreOrder(elem_id);
	dVector3 tmp;
	dBodyVectorFromWorld(elem->get_body(),elem->get_body()->avel[0], elem->get_body()->avel[1], elem->get_body()->avel[2], tmp);
    return *(Fvector*)tmp;
}
Fvector CCar::GetLvelAxes(u16 elem_id)
{
	CPhysicsElement *elem = m_pPhysicsShell->get_ElementByStoreOrder(elem_id);
	dVector3 tmp;
    dBodyVectorFromWorld(elem->get_body(),elem->get_body()->lvel[0], elem->get_body()->lvel[1], elem->get_body()->lvel[2], tmp);
    return *(Fvector*)tmp;
}



void CCar::SetAvelAxes(u16 elem_id, Fvector avel)
{
	CPhysicsElement *elem = m_pPhysicsShell->get_ElementByStoreOrder(elem_id);
	dVector3 tmp;
	dBodyVectorToWorld(elem->get_body(),avel.x, avel.y, avel.z, tmp);
	dBodySetAngularVel(elem->get_body(), tmp[0], tmp[1], tmp[2]);
}

void CCar::SetLvelAxes(u16 elem_id, Fvector lvel)
{
	CPhysicsElement *elem = m_pPhysicsShell->get_ElementByStoreOrder(elem_id);
	dVector3 tmp;
	dBodyVectorToWorld(elem->get_body(),lvel.x, lvel.y, lvel.z, tmp);
	dBodySetLinearVel(elem->get_body(), tmp[0], tmp[1], tmp[2]);
}
