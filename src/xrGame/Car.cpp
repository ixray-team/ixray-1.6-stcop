#include "StdAfx.h"
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
#include "CameraRig.h"
#include "../Include/xrRender/RenderVisual.h"
#include "damage_manager.h"
#include "AnomalyZone.h"
#include "HUDManager.h"
#include "cameralook.h"
#include "CameraFirstEye.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "CustomOutfit.h"
#include "player_hud.h"
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
#include "../xrPhysics/IActivationShape.h"
#include "../xrPhysics/PHCollideValidator.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"
#include "../xrPhysics/PhysicsCommon.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/stalker/ai_stalker.h"
#include "stalker_movement_manager_smart_cover.h"
#include "ai/monsters/control_movement_base.h"
#include "ai/monsters/monster_velocity_space.h"
#include "car_memory.h"

#include "UIGameSP.h"
#include "GamePersistent.h"
#include "actor_anim_defs.h"
#include "../Include/xrRender/particles_systems_library_interface.hpp"
#include "../xrPhysics/ODE/PHElement.h"
#include "../xrPhysics/ODE/PHShell.h"
#include "ExplosiveRocket.h"
#include "../xrEngine/xr_input.h"
#include "../xrEngine/xr_level_controller.h"
#include "../xrEngine/EnnumerateVertices.h"
collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);

namespace
{
	constexpr const char* DefaultExhaustParticles = "vehiclefx\\exhaust_1";
	constexpr u32 StarterWatchdogMargin = 250;
	constexpr u32 WaterParticleInterval = 100;
	constexpr u32 WaterParticleIntervalJitter = 50;
	constexpr float WaterParticleMinSpeed = 0.2f;
	constexpr float WaterParticleMinImmersion = 0.02f;
	constexpr float WaterParticleMaxDistance = 70.f;
	// Do not wake a return bone for sub-milliradian joint noise.
	constexpr float ReturnBoneAngleEpsilon = 0.001f;
	// Time constant for the wheel slip speed filter. At low physics rates the
	// contact solver can alternate between grip and full wheelspin in adjacent
	// steps; filtering keeps the slip state from following that alternation.
	constexpr float WheelSlipFilterTime = 0.1f;
	// The physics interface does not expose an element's inertia, so the
	// default damping is derived from a nominal vehicle cabin inertia. This
	// keeps the return critically damped instead of oscillating, which an
	// explicit torque proportional to the spring stiffness would do at large
	// return_force values.
	constexpr float ReturnBoneNominalInertia = 56.f;
	// Saved wheel bodies can sit below their spawn pose by the suspension
	// travel. A larger offset means a damaged save, so the wheel falls back to
	// the spawn pose before the shell is enabled.
	constexpr float WheelLoadMaxSpawnDrift = 0.75f;
	// Prefix for the client-side car runtime state; an old save starts with
	// the fuel float instead, which cannot collide with this value.
	constexpr u32 CarRuntimeStateMagic = 0x43415253; // "CARS"

	xr_token DriverAnimationSetToken[] =
	{
		{ "new", 0 },
		{ "old", 1 },
		{ nullptr, 0 }
	};

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
	m_driver_anim_old = false;
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
	b_starting=false;
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
	renderable.pROS_IgnoreFoliage = true;
	m_car_sound			= new SCarSound(this);

	CarCameraDirection.set(0.f,0.f,0.f);

	inventory().SetSlotsUseful(false);
	m_doors_torque_factor = 2.f;
	m_doors_hold_factor = 1.f;
	m_doors_hold_at_speed = false;
	m_doors_vel_factor = 1.f;
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
	m_engine_idle_time = 0.f;
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
	m_collision_sound_time = 0;
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
	m_last_avelocity = 0.f;
	m_acceleration = 0.f;
	m_camera_acceleration = 1.f;
	b_diff_lock = true;
	m_diff_lock_power = 1.f;
	m_diff_lock_overpower = 0.f;
	m_firsteye_panel = false;
	m_allow_inventory_control = false;
	m_allow_mutant_bite_in_car = false;
	m_poltergeist_can_lift = false;
	m_interior_volume = 0.7f;
	m_hit_stall_threshold = 1.f;
	m_start_probability = 1.f;
	m_hit_anomaly_scale = 1.f;
	m_mutant_hit_impulse_factor = 1.f;
	m_burer_gravi_impulse_factor = 1.f;
	m_push_impulse_factor = 1.f;
	m_ram_damage_min_speed = 0.f;
	m_ram_crash_speed_min = -1.f;
	m_ram_crash_speed_max = -1.f;
	m_ram_push_scale = 1.5f;
	m_ram_max_push_speed = 25.f;
	m_ram_contact_memory = 150.f;
	m_ram_min_speed = 1.f;
	m_ram_separation_bias = 3.f;
	m_ram_mutant_push_speed = 0.5f;
	m_ram_mutant_slide_scale = 1.f;
	m_ram_mutant_slide_lock = 400.f;
	m_ram_mutant_slide_flip = 0.5f;
	m_ram_mutant_slide_confirm = 150.f;
	m_ram_mutant_slide_memory = 600.f;
	m_ram_start_push_speed = 0.5f;
	m_gigant_kick_impulse = -1.f;
	m_slide_geometry_step = u64(-1);
	m_slide_geometry_valid = false;
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

	DestroyCharacterScraper();

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
	m_fuel_tank = pSettings->read_if_exists<float>(section, "fuel_tank", 10.f);
	RepairAmount = pSettings->read_if_exists<float>(section, "repair_amount", 0.2f);
	clamp(RepairAmount, 0.f, 1.f);
	RepairItem = pSettings->read_if_exists<str_c>(section, "repair_item", "");
	RefuelItem = pSettings->read_if_exists<str_c>(section, "refuel_item", "");
	RefuelAmount = pSettings->read_if_exists<float>(section, "refuel_amount", -1.f);
	m_fuel_consumption = pSettings->read_if_exists<float>(section, "fuel_consumption", 0.0005f);
	m_fuel_speed_factor = pSettings->read_if_exists<float>(section, "fuel_speed_factor", 2.f);
	m_fuel_speed_optimal = pSettings->read_if_exists<float>(section, "fuel_speed_optimal", 50.f);
	clamp(m_fuel_speed_optimal, 1.f, m_fuel_speed_optimal);
	m_fuel_rpm_factor = pSettings->read_if_exists<float>(section, "fuel_rpm_factor", 1.f);
	m_fuel_gas_factor = pSettings->read_if_exists<float>(section, "fuel_gas_factor", 1.f);
	inventory().SetMaxWeight(pSettings->read_if_exists<float>(section, "trunk_capacity", 1000.f));
}

bool CCar::IsMyCar() const
{
	if (!Actor())
		return false;

	return Actor()->Holder() == this;
}

void CCar::save(NET_Packet& output_packet)
{
	SyncServerRuntimeState();
	inherited::save(output_packet);
	output_packet.w_u32(CarRuntimeStateMagic);
	output_packet.w_u8(b_engine_on ? 1 : 0);
	output_packet.w_u8(m_lights.IsOn() ? 1 : 0);
	save_data(m_fuel, output_packet);

	const bool ExplosionPending = m_zero_health_explosion_timer && CDelayedActionFuse::isActive();
	const float ExplosionTime = ExplosionPending ? CDelayedActionFuse::Time() : 0.f;
	output_packet.w_u8(ExplosionPending ? 1 : 0);
	save_data(ExplosionTime, output_packet);
}

void CCar::load(IReader& input_packet)
{
	inherited::load(input_packet);

	const intptr_t Start = input_packet.tell();
	if (input_packet.elapsed() >= intptr_t(sizeof(u32)) &&
		input_packet.r_u32() == CarRuntimeStateMagic)
	{
		m_saved_engine_on = input_packet.r_u8() != 0;
		m_saved_lights_on = input_packet.r_u8() != 0;
		m_has_saved_runtime_state = true;
	}
	else
	{
		// Save made before engine/light persistence: rewind to the fuel value.
		input_packet.seek(Start);
	}

	load_data(m_fuel, input_packet);

	if (input_packet.elapsed() >= intptr_t(sizeof(u8) + sizeof(float)))
	{
		m_saved_explosion_pending = input_packet.r_u8() != 0;
		load_data(m_saved_explosion_time, input_packet);
		m_has_saved_explosion_state = true;
	}
}

void CCar::Serialize(ISaveObject& Object)
{
	if (Object.IsSave())
		SyncServerRuntimeState();

	BEGIN_CHUNK(Object, "CCar::Serialize")
	{
		inherited::Serialize(Object);
		Object << m_fuel;

		// Keep the new fields in their own chunk so saves created before
		// engine/light persistence remain loadable.
		BEGIN_CHUNK(Object, "CCar::runtime_state")
		{
			u8 EngineOn = b_engine_on ? 1 : 0;
			u8 LightsOn = m_lights.IsOn() ? 1 : 0;
			Object << EngineOn << LightsOn;

			if (!Object.IsSave())
			{
				m_saved_engine_on = EngineOn != 0;
				m_saved_lights_on = LightsOn != 0;
				m_has_saved_runtime_state = true;
			}
		}

		BEGIN_CHUNK(Object, "CCar::explosion_state")
		{
			const bool ExplosionPending = m_zero_health_explosion_timer && CDelayedActionFuse::isActive();
			u8 Pending = ExplosionPending ? 1 : 0;
			float Remaining = ExplosionPending ? CDelayedActionFuse::Time() : 0.f;
			Object << Pending << Remaining;

			if (!Object.IsSave())
			{
				m_saved_explosion_pending = Pending != 0;
				m_saved_explosion_time = Remaining;
				m_has_saved_explosion_state = true;
			}
		}
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
			if (dest_dir.square_magnitude() < EPS_L)
				return;

			// Rotate the whole axle frame instead of assigning only the X axis.
			// The old code left J/K untouched, so the skinned axle became a
			// non-orthonormal (sheared) matrix and visibly stretched.
			const float i_scale = B->mTransform.i.magnitude();
			const float j_scale = B->mTransform.j.magnitude();
			const float k_scale = B->mTransform.k.magnitude();

			Fvector up = B->mTransform.j;
			up.mad(dest_dir, -dest_dir.dotproduct(up));
			if (up.square_magnitude() < EPS_L)
			{
				up = B->mTransform.k;
				up.mad(dest_dir, -dest_dir.dotproduct(up));
			}
			if (up.square_magnitude() < EPS_L)
				return;
			up.normalize();

			Fvector forward;
			forward.crossproduct(dest_dir, up);
			forward.normalize();

			B->mTransform.i.set(dest_dir).mul(i_scale);
			B->mTransform.j.set(up).mul(j_scale);
			B->mTransform.k.set(forward).mul(k_scale);
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

void CCar::ApplyBindPositionAdd(IKinematics* K, const xr_vector<Fmatrix>& BindGlobals, u16 BoneID, const Fvector& Offset)
{
	if (!K || BoneID == BI_NONE)
		return;

	CBoneData& Bone = K->LL_GetData(BoneID);
	Fvector ParentOffset = Offset;

	// bind_transform is local to the parent bone. Config values are authored in
	// model space, so convert the offset by the inverse parent bind rotation.
	// This matters for steering wheels: their suspension parent can be rotated.
	const u16 ParentID = Bone.GetParentID();
	if (ParentID != BI_NONE && ParentID < BindGlobals.size())
	{
		const Fmatrix& Parent = BindGlobals[ParentID];
		ParentOffset.set(Offset.dotproduct(Parent.i), Offset.dotproduct(Parent.j), Offset.dotproduct(Parent.k));
	}

	Bone.bind_transform.c.add(ParentOffset);
}

void CCar::ApplyBindDirectionAdd(IKinematics* K, u16 BoneID, const Fvector& Angles)
{
	if (!K || BoneID == BI_NONE)
		return;

	CBoneData& Bone = K->LL_GetData(BoneID);
	const Fvector LastC = Bone.bind_transform.c;

	Fmatrix rX; rX.rotateX(Angles.x);
	Bone.bind_transform.mulB_43(rX);
	Fmatrix rY; rY.rotateY(Angles.y);
	Bone.bind_transform.mulB_43(rY);
	Fmatrix rZ; rZ.rotateZ(Angles.z);
	Bone.bind_transform.mulB_43(rZ);

	Bone.bind_transform.c.set(LastC);
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

	xr_vector<Fmatrix> BindGlobals;
	pKinematics->LL_GetBindTransform(BindGlobals);

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
				ApplyBindDirectionAdd(pKinematics, Bone.second, ini->r_fvector3(b_name, "bind_direction_add"));
			}

			if (ini->line_exist(b_name, "bind_position"))
			{
				bd.bind_transform.c.set(ini->r_fvector3(b_name, "bind_position"));
			}

			if (ini->line_exist(b_name, "bind_position_add"))
			{
				ApplyBindPositionAdd(pKinematics, BindGlobals, Bone.second, ini->r_fvector3(b_name, "bind_position_add"));
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

	// The scraper is a separate shell that follows the car. Put it into the
	// same collision group as the car body, otherwise the two shells collide
	// with each other and the contact damage receiver treats every overlap as
	// a strike against the car. Characters and ragdolls stay in other groups,
	// so the scraper still pushes them away.
	if (characters_scraper_shell != nullptr && m_pPhysicsShell != nullptr)
	{
		CGID ScraperGroup;
		if (m_pPhysicsShell->IsGroupObject())
		{
			ScraperGroup = m_pPhysicsShell->GetCLGroup();
		}
		else
		{
			ScraperGroup = RegisterGroup();
			m_pPhysicsShell->RegisterToCLGroup(ScraperGroup);
		}
		characters_scraper_shell->RegisterToCLGroup(ScraperGroup);
	}

	setEnabled						(TRUE);
	setVisible						(TRUE);
	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);
	SetfHealth						(co->health);
	m_fuel							= co->fuel;
	b_exploded						= co->Exploded != 0;
	if (!m_has_saved_explosion_state)
	{
		m_saved_explosion_pending	= co->ExplosionPending != 0;
		m_saved_explosion_time		= co->ExplosionTime;
		m_has_saved_explosion_state	= true;
	}
									
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

	// An offline car has no game-object save data, so fall back to the engine
	// and light state stored on the ALife server object.
	if (!m_has_saved_runtime_state && co != nullptr && (co->Engine != 0 || co->Light != 0))
	{
		m_saved_engine_on = co->Engine != 0;
		m_saved_lights_on = co->Light != 0;
		m_has_saved_runtime_state = true;
	}

	if (m_has_saved_runtime_state)
	{
		m_has_saved_runtime_state = false;
		RestoreEngineAndLights(m_saved_engine_on, m_saved_lights_on);
	}

	if (m_zero_health_explosion_timer)
	{
		if (m_has_saved_explosion_state)
		{
			m_has_saved_explosion_state = false;
			RestoreExplosionState();
		}
		else if (GetfHealth() <= 0.f && !CDelayedActionFuse::isActive())
		{
			StartExplosionTimer();
		}
	}
	else if (GetfHealth() <= 0.f)
	{
		// Legacy mode: time_to_explosion is the old health-drain fuse, and a
		// dead car explodes immediately.
		CarExplode();
	}

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
	if(!Collided_object || !Collided_object->m_pPhysicsShell || !Collided_gobject || Collided_gobject->getDestroy() || Collided_gobject->cast_actor() || Collided_gobject->cast_car() || Collided_gobject->cast_inventory_item())
	{
		do_colide = false;
		return;
	}
}

void CCar::CarBodySurfaceCallback(bool& do_colide, bool bo1, CPHContact& cphc, SGameMtl* material_1, SGameMtl* material_2)
{
	// Wheels are handled by WheellCollisionCallback, where the surface friction
	// override is applied before the wheel-specific multipliers.
	if (cphc.HasObjectCallback(bo1, &CCar::SWheel::WheellCollisionCallback))
		return;

	CPhysicsShellHolder* Holder = smart_cast<CPhysicsShellHolder*>(cphc.GetObject(bo1));
	CCar* Car = Holder ? smart_cast<CCar*>(Holder) : nullptr;
	if (!Car || Car->getDestroy())
		return;

	// The car surface table describes static level surfaces only. Other cars,
	// characters and dynamic props keep their normal material behaviour.
	CPhysicsShellHolder* Other = smart_cast<CPhysicsShellHolder*>(cphc.GetObject(!bo1));
	if (Other != nullptr)
	{
		// Characters already produce their own footsteps/hit sounds. Feeding
		// their contacts into the car body impact table made small movable
		// parts such as engine doors rattle audibly when the actor touched them.
		if (do_colide && Other != Car && Other->cast_actor() == nullptr)
		{
			Fvector CarVelocity;
			Fvector OtherVelocity;
			Fvector RelativeVelocity;
			Car->PHGetLinearVell(CarVelocity);
			Other->PHGetLinearVell(OtherVelocity);
			RelativeVelocity.sub(CarVelocity, OtherVelocity);
			const float RelativeSpeed = _abs(RelativeVelocity.dotproduct(cphc.GetNormal()));

			SGameMtl* CarMaterial = bo1 ? material_1 : material_2;
			SGameMtl* OtherMaterial = bo1 ? material_2 : material_1;
			Car->PlayDynamicCollisionSound(CarMaterial, OtherMaterial, cphc.GetPosition(), RelativeSpeed);
		}
		return;
	}

	SGameMtl* SurfaceMaterial = bo1 ? material_2 : material_1;
	if (!SurfaceMaterial)
		return;

	Car->m_body_surface_material = SurfaceMaterial;
	Car->m_body_surface_friction = cphc.GetFriction();
	Car->m_body_surface_flotation = SurfaceMaterial->fFlotationFactor;

	// While a wheel is in the water, UpdateWaterDrag() applies the drag for
	// the whole car. Suppress the body water contact flotation so the body and
	// the wheels do not fight each other when the body touches the surface.
	if (SurfaceMaterial->Flags.test(SGameMtl::flLiquid) && Car->WheelsInWater)
	{
		Car->m_body_surface_flotation = 1.f;
		cphc.SetFlotation(bo1, SurfaceMaterial, 1.f);
		return;
	}

	const SSurfaceParams* Params = Car->FindSurfaceParams(SurfaceMaterial);
	if (!Params)
		return;

	if (Params->HasFriction)
	{
		Car->m_body_surface_friction = Params->Friction;
		cphc.SetFriction(Params->Friction);
	}

	if (Params->HasFlotation)
	{
		Car->m_body_surface_flotation = Params->Flotation;
		cphc.SetFlotation(bo1, SurfaceMaterial, Params->Flotation);
	}
}

void CCar::PrepareSlideGeometry()
{
	const u64 Step = physics_world()->StepsNum();
	if (m_slide_geometry_valid && m_slide_geometry_step == Step)
		return;

	m_slide_geometry_step = Step;
	m_slide_geometry_valid = false;

	Fbox Box = BoundingBox();
	if (!Box.is_valid())
		return;

	Box.get_CD(m_slide_box_center, m_slide_box_half);
	m_slide_box_half.x = std::max(m_slide_box_half.x, 0.25f);
	m_slide_box_half.z = std::max(m_slide_box_half.z, 0.25f);

	m_slide_inv_xform.invert(XFORM());
	XFORM().transform_dir(m_slide_axis_x, Fvector().set(1.f, 0.f, 0.f));
	XFORM().transform_dir(m_slide_axis_z, Fvector().set(0.f, 0.f, 1.f));
	m_slide_axis_x.y = 0.f;
	m_slide_axis_z.y = 0.f;
	m_slide_axis_x.normalize_safe();
	m_slide_axis_z.normalize_safe();
	m_slide_geometry_valid = true;
}

bool CCar::PrepareSlideGeomBox(const CODEGeom* Geom, const Fmatrix& Form, const Fvector& Size,
	Fmatrix& OutInv, Fvector& OutHalf, Fvector& OutAxisX, Fvector& OutAxisZ)
{
	if (Geom == nullptr)
		return false;

	const u64 Step = physics_world()->StepsNum();
	SSlideGeomBox* Entry = nullptr;
	for (SSlideGeomBox& Candidate : m_slide_geom_cache)
	{
		if (Candidate.Geom == Geom)
		{
			Entry = &Candidate;
			break;
		}
	}

	if (Entry == nullptr)
	{
		m_slide_geom_cache.push_back(SSlideGeomBox());
		Entry = &m_slide_geom_cache.back();
		Entry->Geom = Geom;
	}

	if (Entry->Step != Step || !Entry->Valid)
	{
		Entry->Step = Step;
		Entry->Valid = false;
		Entry->Form = Form;
		Entry->Inv.invert(Form);
		Entry->Half.set(Size.x * 0.5f, Size.y * 0.5f, Size.z * 0.5f);
		Entry->Half.x = std::max(Entry->Half.x, 0.05f);
		Entry->Half.y = std::max(Entry->Half.y, 0.05f);
		Entry->Half.z = std::max(Entry->Half.z, 0.05f);
		Entry->Valid = true;
	}

	if (!Entry->Valid)
		return false;

	OutInv = Entry->Inv;
	OutHalf = Entry->Half;
	OutAxisX = Entry->Form.i;
	OutAxisZ = Entry->Form.k;
	OutAxisX.y = 0.f;
	OutAxisZ.y = 0.f;
	OutAxisX.normalize_safe();
	OutAxisZ.normalize_safe();
	return true;
}

void CCar::CarRamCallback(bool& do_colide, bool bo1, CPHContact& cphc, SGameMtl* material_1, SGameMtl* material_2)
{
	if (!do_colide)
		return;

	CPhysicsShellHolder* A = smart_cast<CPhysicsShellHolder*>(cphc.GetObject(true));
	CPhysicsShellHolder* B = smart_cast<CPhysicsShellHolder*>(cphc.GetObject(false));

	CCar* Car = nullptr;
	CPhysicsShellHolder* Other = nullptr;
	if (A != nullptr && A->cast_car() != nullptr)
	{
		Car = static_cast<CCar*>(A);
		Other = B;
	}
	else if (B != nullptr && B->cast_car() != nullptr)
	{
		Car = static_cast<CCar*>(B);
		Other = A;
	}

	if (Car == nullptr || Other == nullptr || Car->m_pPhysicsShell == nullptr)
		return;

	if (Other->cast_actor() != nullptr || Other->cast_car() != nullptr || Other->cast_inventory_item() != nullptr)
		return;

	// Living characters (mutants and NPCs) use the additive ram push.
	// Dead ragdolls and non-character objects are left to physics.
	CEntityAlive* Alive = Other->cast_entity_alive();
	if (Alive == nullptr || !Alive->g_Alive() || Other->getDestroy())
		return;

	CCharacterPhysicsSupport* Support = Other->character_physics_support();
	if (Support == nullptr || Support->movement() == nullptr)
		return;

	CPHMovementControl* Movement = Support->movement();
	if (!Movement->CharacterExist())
		return;

	const bool car_is_first = (A != nullptr && A->cast_car() != nullptr);
	const bool wheel_contact = cphc.HasObjectCallback(car_is_first, &CCar::SWheel::WheellCollisionCallback);

	// The car's velocity at the actual contact point includes its angular
	// motion, so a sideways or spinning hit is calculated correctly.
	CPhysicsElement* CarElement = Car->m_pPhysicsShell->get_ElementByStoreOrder(0);
	if (CarElement == nullptr)
		return;

	Fvector CarVelocity;
	CarElement->GetPointVel(CarVelocity, cphc.GetPosition());

	// The contact normal may be either way. Orient it from the car to the
	// mutant, then cache the contact for the movement manager.
	Fvector Normal = cphc.GetNormal();
	Fvector ToMutant;
	ToMutant.sub(Other->Position(), cphc.GetPosition());
	if (Normal.dotproduct(ToMutant) < 0.f)
		Normal.invert();

	Normal.y = 0.f;
	if (Normal.magnitude() < EPS_L)
	{
		Normal = ToMutant;
		Normal.y = 0.f;
	}
	if (Normal.magnitude() < EPS_L)
		return;
	Normal.normalize_safe();

	// Contact points on the far side of a deep overlap can produce a normal
	// that points into the car. Prefer the stable car-centre-to-mutant
	// direction in that case, otherwise a single contact set can push the
	// mutant back and forth between opposite normals.
	Fvector CenterDirection;
	CenterDirection.sub(Other->Position(), Car->Position());
	CenterDirection.y = 0.f;
	if (CenterDirection.magnitude() > EPS_L)
	{
		CenterDirection.normalize_safe();
		if (Normal.dotproduct(CenterDirection) < 0.25f)
			Normal = CenterDirection;
	}

	// The car collision is a box. Pick the nearest local side and use the
	// other horizontal local axis as the slide direction. This is more stable
	// than deriving a tangent from a noisy corner/edge contact normal.
	Fvector SlideAxis;
	SlideAxis.set(0.f, 0.f, 0.f);
	float MinSlideSpeed = 0.f;
	float SideBias = 0.f;

	// Minimum desired speed for the forced slide. Mutants use their normal
	// run speed; NPCs use the free/stand/run forward value from their
	// movement_speeds section. slide_scale is applied later.
	if (CBaseMonster* Monster = Other->cast_base_monster())
	{
		const SVelocityParam& Run = Monster->move().get_velocity(MonsterMovement::eVelocityParameterRunNormal);
		MinSlideSpeed = Run.velocity.linear;
	}
	else if (CAI_Stalker* Stalker = smart_cast<CAI_Stalker*>(Other))
	{
		MinSlideSpeed = Stalker->movement().free_run_speed();
	}

	Fmatrix GeomForm;
	Fvector GeomSize;
	u16 GeomBone = u16(-1);
	const CODEGeom* Geom = nullptr;
	Fmatrix SlideInv;
	Fvector SlideCenter;
	Fvector SlideHalf;
	Fvector SlideAxisX;
	Fvector SlideAxisZ;
	bool SlideBoxValid = false;
	bool NearEnd = false;

	const bool HasGeomBox = !wheel_contact && cphc.GetGeometryBox(car_is_first, GeomForm, GeomSize, &GeomBone, &Geom);
	if (HasGeomBox && Car->PrepareSlideGeomBox(Geom, GeomForm, GeomSize, SlideInv, SlideHalf, SlideAxisX, SlideAxisZ))
	{
		SlideCenter.set(0.f, 0.f, 0.f);
		SlideBoxValid = true;
	}
	else
	{
		Car->PrepareSlideGeometry();
		if (Car->m_slide_geometry_valid)
		{
			SlideInv = Car->m_slide_inv_xform;
			SlideCenter = Car->m_slide_box_center;
			SlideHalf = Car->m_slide_box_half;
			SlideAxisX = Car->m_slide_axis_x;
			SlideAxisZ = Car->m_slide_axis_z;
			SlideBoxValid = true;
		}
	}

	if (SlideBoxValid)
	{
		Fvector Local;
		SlideInv.transform_tiny(Local, cphc.GetPosition());
		Local.sub(SlideCenter);

		const float DX = Local.x / SlideHalf.x;
		const float DZ = Local.z / SlideHalf.z;

		bool XFace;
		if (std::fabs(DX) >= std::fabs(DZ) * 1.1f)
		{
			XFace = true;
		}
		else if (std::fabs(DZ) >= std::fabs(DX) * 1.1f)
		{
			XFace = false;
		}
		else
		{
			XFace = std::fabs(Normal.dotproduct(SlideAxisX)) >= std::fabs(Normal.dotproduct(SlideAxisZ));
		}

		NearEnd = std::fabs(XFace ? DZ : DX) > 0.85f;

		Fvector LocalSlide;
		if (XFace)
		{
			// side face -> slide along car forward/back
			LocalSlide.set(0.f, 0.f, 1.f); 
		}
		else
		{
			// front/rear face -> slide sideways
			LocalSlide.set(1.f, 0.f, 0.f); 
		}

		SideBias = Local.dotproduct(LocalSlide);

		SlideAxis = XFace ? SlideAxisZ : SlideAxisX;
		if (SlideAxis.magnitude() > EPS_L)
		{
			SlideAxis.normalize_safe();
		}
		else
		{
			SlideAxis.set(0.f, 0.f, 0.f);
		}
	}

	bool HasParentCollision = false;
	if (GeomBone != u16(-1))
	{
		IKinematics* Kinematics = PKinematics(Car->Visual());
		if (Kinematics != nullptr && GeomBone < Kinematics->LL_BoneCount())
		{
			u16 ParentBone = Kinematics->LL_GetData(GeomBone).GetParentID();
			while (ParentBone != BI_NONE)
			{
				CPhysicsElement* ParentElement = Car->PPhysicsShell()->get_Element(ParentBone);
				if (ParentElement != nullptr && ParentElement->has_geoms())
				{
					HasParentCollision = true;
					break;
				}
				ParentBone = Kinematics->LL_GetData(ParentBone).GetParentID();
			}
		}
	}

	if (SlideAxis.magnitude() <= EPS_L)
	{
		SlideAxis.crossproduct(Normal, Fvector().set(0.f, 1.f, 0.f));
		SlideAxis.y = 0.f;
		if (SlideAxis.magnitude() > EPS_L)
		{
			SlideAxis.normalize_safe();
		}
		else
		{
			SlideAxis.set(0.f, 0.f, 0.f);
		}
	}

	float StartPushSpeed = 0.f;
	if (Car->b_engine_on && Car->e_state_drive == CCar::drive && (Car->fwp || Car->bkp))
	{
		Fvector DriveDirection = Car->XFORM().k;
		DriveDirection.y = 0.f;
		DriveDirection.normalize_safe();
		if (Car->CurrentTransmission() == 0)
			DriveDirection.invert(); // Transmission 0 is reverse.
		if (DriveDirection.dotproduct(Normal) > 0.1f)
			StartPushSpeed = Car->m_ram_start_push_speed;
	}

	if (!Movement->RamPushScaleResolved())
	{
		Movement->ResolveRamPushScale(Alive->RamPushType());
	}

	float RamPushScale = Car->m_ram_push_scale;
	const float CharacterRamPushScale = Movement->RamPushScaleOverride();
	if (CharacterRamPushScale >= 0.f)
		RamPushScale = CharacterRamPushScale;

	CPHMovementControl::SCarContactParams Contact{};
	Contact.normal = Normal;
	Contact.car_velocity = CarVelocity;
	Contact.slide_axis = SlideAxis;
	Contact.separation_bias = Car->m_ram_separation_bias;
	Contact.ram_scale = RamPushScale;
	Contact.mutant_push_speed = Car->m_ram_mutant_push_speed;
	Contact.min_car_speed = Car->m_ram_min_speed;
	Contact.max_push_speed = Car->m_ram_max_push_speed;
	Contact.slide_scale = Car->m_ram_mutant_slide_scale;
	Contact.min_slide_speed = MinSlideSpeed;
	Contact.side_bias = SideBias;
	Contact.start_push_speed = StartPushSpeed;
	Contact.slide_lock_ms = Car->m_ram_mutant_slide_lock;
	Contact.slide_flip = Car->m_ram_mutant_slide_flip;
	Contact.slide_confirm_ms = Car->m_ram_mutant_slide_confirm;
	Contact.slide_memory_ms = Car->m_ram_mutant_slide_memory;
	Contact.body_contact = !wheel_contact;
	Contact.depth = cphc.GetDepth();
	Contact.geom_bone = GeomBone;
	Contact.near_end = NearEnd;
	Contact.has_parent_collision = HasParentCollision;
	Contact.duration_ms = static_cast<u32>(Car->m_ram_contact_memory);
	Contact.car_name = Car->cName();
	Contact.car_owned_by_actor = Car->OwnerActor() != nullptr;
	Movement->SetCarContact(Contact);
}

void CCar::SpawnInitPhysics(CSE_Abstract* D)
{
	CSE_PHSkeleton* so = smart_cast<CSE_PHSkeleton*>(D);
	R_ASSERT(so);
	ParseDefinitions(); // parse ini filling in m_driving_wheels,m_steering_wheels,m_breaking_wheels
	LoadBoneMap();		// fill in bone_map
	CreateSkeleton(D);	// creates m_pPhysicsShell
	IKinematics* K = smart_cast<IKinematics*>(Visual());
	CInifile* ini = K->LL_UserData();
	K->CalculateBones_Invalidate(); // this need to call callbacks
	K->CalculateBones(TRUE);
	Init(); // inits m_driving_wheels,m_steering_wheels,m_breaking_wheels values using recieved in ParceDefinitions & from bone_map
	PPhysicsShell()->add_ObjectContactCallback(ActorObstacleCallback);
	PPhysicsShell()->add_ObjectContactCallback(CarBodySurfaceCallback);
	PPhysicsShell()->add_ObjectContactCallback(CarRamCallback);
	SetDefaultNetState(so);
	CPHUpdateObject::Activate();

	if (ini)
	{
		CreateCharacterScraper(K, ini);
		CreateCarTurrets(ini);
	}

	ApplyBonePhysicsParams(K, ini);

	if (!so->_flags.test(CSE_PHSkeleton::flSavedData))
	{
		SwitchEngineDoors();
	}
	else
	{
		for (SEngineDoor& Door : engine_door)
		{
			Door.Hold();
		}
	}

	BindGaugeAndSuspensionCallbacks(K, ini);

	NeutralDrive();
	PressBreaks();

	m_pPhysicsShell->Activate(XFORM(), 0, XFORM());

	UpdateDashboardTelemetry();
	smart_cast<IKinematics*>(Visual())->CalculateBones_Invalidate();
	smart_cast<IKinematics*>(Visual())->CalculateBones(TRUE);

	m_pPhysicsShell->applyForce({0.f, 0.f, 0.f}, 0.00001f);

	for (auto& Door : m_doors)
	{
		Door.second.BuildExitArea();
	}

	m_pPhysicsShell->GetGlobalTransformDynamic(&XFORM());
	StoreAllElementSpawnLocalTransforms();
}

void CCar::CreateCharacterScraper(IKinematics* K, CInifile* ini)
{
	if (!ini->line_exist("car_definition", "character_scraper"))
	{
		return;
	}

	characters_scraper_shell = P_create_Shell();
	CPhysicsElement* RootElement = P_create_Element();
	bool HasGeometry = false;

	for (auto& Bone : bone_map)
	{
		const u16 BoneId = Bone.first;
		if (BoneId >= bones_bind_forms.size())
		{
			continue;
		}

		Fobb BoneBox = K->LL_GetBox(BoneId);
		if (BoneBox.m_halfsize.magnitude() < EPS_L)
		{
			continue;
		}

		Fobb LocalBox;
		LocalBox.transform(BoneBox, bones_bind_forms[BoneId]);
		LocalBox.m_halfsize.add(0.05f);

		RootElement->add_Box(LocalBox);
		HasGeometry = true;
	}

	if (!HasGeometry)
	{
		Fobb FallbackBox;
		BoundingBox().get_CD(FallbackBox.m_translate, FallbackBox.m_halfsize);
		FallbackBox.m_halfsize.add(0.15f);
		FallbackBox.m_rotate.identity();
		RootElement->add_Box(FallbackBox);
	}

	characters_scraper_shell->add_Element(RootElement);

	characters_scraper_shell->SetIgnoreStatic();
	characters_scraper_shell->set_ApplyByGravity(FALSE);
	characters_scraper_shell->setMass(50.f);
	characters_scraper_shell->SetMaterial(K->LL_GetData(K->LL_GetBoneRoot()).game_mtl_idx);
	characters_scraper_shell->set_PhysicsRefObject(nullptr);
	characters_scraper_shell->add_ObjectContactCallback(CollisionDetectionCallback);
	characters_scraper_shell->SetPrefereExactIntegration();
	characters_scraper_shell->mXFORM.set(XFORM());
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
						{
							SEngineDoor Door(this, Bone.second.joint, velocity_, force_, axis_);
							Door.bone_id = Bone.first;
							Door.closed_local = Bone.first < bones_bind_forms.size()
								? bones_bind_forms[Bone.first] : Fidentity;
							Fmatrix InvCar = XFORM();
							InvCar.invert();
							Fvector WorldAxis;
							Bone.second.joint->GetAxisDirDynamic(0, WorldAxis);
							InvCar.transform_dir(Door.axis_local, WorldAxis);
							Door.axis_local.normalize();
							Fvector WorldAnchor;
							Bone.second.joint->GetAnchorDynamic(WorldAnchor);
							InvCar.transform_tiny(Door.anchor_local, WorldAnchor);
							Door.bind_angle = dJointGetHingeAngle(Bone.second.joint->GetDJoint());

							float Lo = 0.f;
							float Hi = 0.f;
							Bone.second.joint->GetLimits(Lo, Hi, 0);
							if (_abs(Lo) > _abs(Hi))
							{
								Door.opened_angle = Lo;
								Door.closed_angle = Hi;
							}
							else
							{
								Door.opened_angle = Hi;
								Door.closed_angle = Lo;
							}
							engine_door.push_back(Door);
						}
						else
							Bone.second.joint->SetForceAndVelocity(force_, velocity_, axis_);
					}

					if (ini->line_exist(b_name, "return_force"))
					{
						const float ReturnForce = ini->r_float(b_name, "return_force");
						if (ReturnForce > 0.f)
						{
							const float DefaultDamping = 2.f * sqrtf(ReturnForce * ReturnBoneNominalInertia);
							const float ReturnDamping = ini->read_if_exists<float>(b_name, "return_damping", DefaultDamping);
							return_bones.push_back(SReturnBone(Bone.second.joint, Bone.second.element, ReturnForce, ReturnDamping));
						}
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
		Dashboard.BindBone(*K, *ini, Section, BoneId);
		if (ini->line_exist(Section, "suspension"))
			BindSuspension(Section, BoneId, K, ini);
	}
}

void CCar::UpdateDashboardTelemetry()
{
	if (Dashboard.Empty())
		return;

	SCarTelemetry Telemetry;
	Telemetry.HasDriveWheels = !m_driving_wheels.empty();
	if (Telemetry.HasDriveWheels)
	{
		float Speed = 0.f;
		for (auto& WheelDrive : m_driving_wheels)
			Speed += _abs(WheelDrive.ASpeed()) * WheelDrive.pwheel->radius;
		Telemetry.DriveWheelsSpeed = Speed / m_driving_wheels.size();
	}

	Telemetry.SpeedKmh = GetSpeedKmH();
	Telemetry.Rpm = m_current_rpm;
	Telemetry.MinRpm = m_min_rpm;
	Telemetry.MaxRpm = m_max_rpm;
	Telemetry.FuelRatio = m_fuel / m_fuel_tank;
	Telemetry.BrakeElapsed = Device.fTimeGlobal - m_break_start;
	Telemetry.BrakeTime = m_break_time;
	Telemetry.TransmissionSwitchDelay = m_transmission_switch_delay;
	Telemetry.Gear = m_current_transmission_num;
	Telemetry.EngineOn = b_engine_on;
	Telemetry.Drive = e_state_drive == drive;
	Telemetry.TransmissionSwitching = IsTransmissionSwitching();
	Telemetry.Brakes = b_breaks;
	Telemetry.HasDriver = Owner() != nullptr;
	Dashboard.SetTelemetry(Telemetry);
}

void CCar::BindSuspension(const char* Section, u16 BoneId, IKinematics* K, CInifile* ini)
{
	LPCSTR first_bone_name = ini->read_if_exists<str_c>(Section, "first_wheel", "BI_NONE");
	LPCSTR second_bone_name = ini->read_if_exists<str_c>(Section, "second_wheel", "BI_NONE");
	bool steer = ini->read_if_exists<bool>(Section, "steer", true);
	bool move = ini->read_if_exists<bool>(Section, "move", true);
	float suspension_offset = ini->read_if_exists<float>(Section, "suspension_offset", 0.f);
	float steer_factor = ini->read_if_exists<float>(Section, "steer_factor", 1.f);
	m_suspension_params.push_back(xr_make_unique<Suspension_struct>(this, K->LL_BoneID(first_bone_name), K->LL_BoneID(second_bone_name), suspension_offset, steer, move, steer_factor));
	m_suspension_bones.push_back(BoneId);
	K->LL_GetBoneInstance(BoneId).set_callback(bctCustom, cb_Suspension, m_suspension_params.back().get());
}

void	CCar::net_Destroy()
{
	SyncServerRuntimeState();
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
		{
			ResetCallback(BoneId);
		}
		Dashboard.Unbind(pKinematics);
		for (u16 BoneId : m_suspension_bones)
		{
			ResetCallback(BoneId);
		}

		for (const auto& Door : m_doors)
		{
			if (Door.first == BI_NONE || Door.first >= pKinematics->LL_BoneCount())
				continue;
			CBoneInstance& BI = pKinematics->LL_GetBoneInstance(Door.first);
			if (BI.callback_type() == bctCustom && BI.callback() == SDoor::ClosedBoneCallback)
				BI.reset_callback();
		}
	}
	Dashboard.Unbind(nullptr);
	m_suspension_params.clear();
	m_suspension_bones.clear();
	return_bones.clear();
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
	const bool WasOwned = Owner() != nullptr;
	CHolderCustom::detach_Actor();
	if (WasOwned)
	{
		ResetCabinSound();
	}
	ClearExhausts();
	for (auto& i_w : m_wheels_map)
	{
		if (i_w.second.tire_drive._p)
			i_w.second.tire_drive.destroy();
	}
	DestroyDriverHands();
	m_wheels_map.clear();
	m_steering_wheels.clear();
	m_driving_wheels.clear();
	m_exhausts.clear();
	m_steers.clear();
	m_indoor_l_available_doors.clear();
	m_breaking_wheels.clear();
	engine_door.clear();
	headlight_hideaways.clear();
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

void CCar::DestroyCharacterScraper()
{
	if (!characters_scraper_shell)
		return;

	characters_scraper_shell->Deactivate();
	characters_scraper_shell->ZeroCallbacks();
	xr_delete(characters_scraper_shell);
	characters_scraper_shell = nullptr;
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
	if (!po->_flags.test(CSE_PHSkeleton::flSavedData))
	{
		return;
	}

	const bool SavedEnabled = po->_flags.test(CSE_PHSkeleton::flActive);

	const bool SavedShellApplied = po->saved_bones.bones.size() == size_t(PHGetSyncItemsNumber());

	const Fvector SavedMin = po->saved_bones.get_min();
	const Fvector SavedMax = po->saved_bones.get_max();
	Fvector SavedRange;
	SavedRange.sub(SavedMax, SavedMin);
	constexpr float MaxSaneSavedRange = 25.f;
	const bool SavedRangeSane =
		SavedRange.x <= MaxSaneSavedRange &&
		SavedRange.y <= MaxSaneSavedRange &&
		SavedRange.z <= MaxSaneSavedRange;
	if (!SavedRangeSane)
	{
		Msg("! [car-load] %s: degraded saved element range (%.1f, %.1f, %.1f); wheels reset, re-save the game",
			cName().c_str(),
			SavedRange.x,
			SavedRange.y,
			SavedRange.z);
	}

	CPHSkeleton::RestoreNetState(po);

	CSE_ALifeCar* co = smart_cast<CSE_ALifeCar*>(po);
	if (co != nullptr)
	{
		size_t DoorIndex = 0;
		for (auto& Door : m_doors)
		{
			if (DoorIndex >= co->door_states.size())
			{
				break;
			}
			Door.second.RestoreNetState(co->door_states[DoorIndex++]);
		}

		size_t WheelIndex = 0;
		for (auto& Wheel : m_wheels_map)
		{
			if (WheelIndex >= co->wheel_states.size())
			{
				break;
			}
			Wheel.second.RestoreNetState(co->wheel_states[WheelIndex++]);
		}
	}

	Fmatrix RestoredForm;
	PPhysicsShell()->GetGlobalTransformDynamic(&RestoredForm);
	XFORM().set(RestoredForm);

	const bool SavedWheelsTrusted = RestoreAllElementSpawnLocalTransforms(RestoredForm, SavedRangeSane);
	PPhysicsShell()->GetGlobalTransformDynamic(&RestoredForm);
	XFORM().set(RestoredForm);

	if (!SavedShellApplied || !SavedWheelsTrusted)
	{
		ResolveLoadPenetration();
	}

	if (SavedEnabled)
	{
		PPhysicsShell()->Enable();
	}
	else
	{
		PPhysicsShell()->Disable();
	}

	if (co != nullptr)
	{
		RestoreServerPose(co);
	}

	PPhysicsShell()->GetGlobalTransformDynamic(&RestoredForm);
	XFORM().set(RestoredForm);

	const bool EngineOn = m_has_saved_runtime_state ? m_saved_engine_on : b_engine_on;
	for (SEngineDoor& Door : engine_door)
	{
		Door.ApplyEngineDoorState(EngineOn);
	}

	for (auto& Door : m_doors)
	{
		if (Door.second.state != SDoor::closed && Door.second.state != SDoor::broken)
		{
			Door.second.RebuildFromSavedOrientation();
		}
	}

	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);

	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);
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
	if (Level().Server && !getDestroy() && !b_exploded)
	{
		if (CSE_Abstract* DC = Level().Server->ID_to_entity(ID()))
		{
			if (CSE_ALifeCar* secar = smart_cast<CSE_ALifeCar*>(DC))
			{
				Fvector xyz;
				XFORM().getXYZ(xyz);
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
	if (CPHDestroyable::Destroyed())
	{
		CPHDestroyable::SheduleUpdate(dt);
	}
	else
	{
		CPHSkeleton::Update(dt);
	}

	if (b_exploded)
	{
		DestroyCharacterScraper();
	}

	if (CDelayedActionFuse::isActive() && CDelayedActionFuse::Update(GetfHealth()))
	{
		CarExplode();
	}

	if (b_exploded && !m_explosion_flags.test(flExploding) && !getEnabled()) //! m_bExploding
	{
		setEnabled(TRUE);
	}
#ifdef DEBUG
	DbgSheduleUpdate();
#endif
}

void CCar::LoadDriverHands()
{
	CCarDriverHands::SLoadParams Params;
	if (OwnerActor() != nullptr && !m_steers.empty())
	{
		Params.CarSection = cNameSect_str();
		Params.CarKinematics = PKinematics(Visual());
		Params.DefaultSteerBone = m_steers.front();
	}

	DriverHands.Load(Params, CCarDriverHands::ResolveSection(OwnerActor()));
}

void CCar::DestroyDriverHands()
{
	DriverHands.Destroy();
}

void CCar::UpdateDriverHands()
{
	CActor* Driver = OwnerActor();
	if (Driver == nullptr || !Driver->IsMyCamera())
	{
		DriverHands.Hide();
		return;
	}

	// The hands model follows the player's equipped outfit. Reload it when
	// the player's HUD hands section changes while sitting in the car.
	if (CCarDriverHands::ResolveSection(Driver) != DriverHands.SourceSection())
		LoadDriverHands();

	if (!HUDView())
	{
		DriverHands.Hide();
		return;
	}

	CCarDriverHands::SFrameParams Params;
	Params.CarSection = cNameSect_str();
	Params.CarKinematics = PKinematics(Visual());
	Params.CarXform = XFORM();
	Params.SeatXform.mul_43(XFORM(), m_sits_transforms);
	Params.SteerAngle = m_steer_angle;
	DriverHands.Update(Params);
}

void CCar::RenderDriverHands()
{
	if (OwnerActor() == nullptr || !OwnerActor()->IsMyCamera() || !HUDView())
		return;

	DriverHands.Render();
}

void CCar::UpdateEx			(float fov) 
{
	#ifdef DEBUG
	DbgUbdateCl();
	#endif

	VisualUpdate(fov);
	UpdateSteerAnimation();
	UpdateDriverHands();
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

void CCar::UpdateCL()
{
	UpdateDashboardTelemetry();

	if (m_has_saved_runtime_state)
	{
		m_has_saved_runtime_state = false;
		RestoreEngineAndLights(m_saved_engine_on, m_saved_lights_on);
	}

	if (m_has_saved_explosion_state)
	{
		m_has_saved_explosion_state = false;
		RestoreExplosionState();
	}

	if (b_starting &&
		Device.dwTimeGlobal - m_dwStartTime > m_car_sound->engine_start_delay + StarterWatchdogMargin)
	{
		b_starting = false;
	}

	if (m_steer_boost_frame != Device.dwFrame)
	{
		m_steer_boost = 0.f;
	}

	inherited::UpdateCL();
	CExplosive::UpdateCL();

	if (Device.dwTimeGlobal - m_last_server_state_sync >= 1000)
	{
		m_last_server_state_sync = Device.dwTimeGlobal;
		SyncServerRuntimeState();
	}

	if (m_restore_engine_effects_attempts > 0)
	{
		--m_restore_engine_effects_attempts;

		bool ExhaustsPlaying = true;
		for (auto& Exhaust : m_exhausts)
		{
			if (Exhaust.p_pgobject && !Exhaust.p_pgobject->IsPlaying())
				ExhaustsPlaying = false;
		}

		if (!ExhaustsPlaying)
			PlayExhausts();
	}

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
		if (!m_indoor_l_available_doors.empty())
		{
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
bool rs_car_unlimitedfuel = false;
bool rs_car_auto_transmission = true;
#if defined(DEBUG) || defined(DEBUG_DRAW)
bool rs_car_anti_gravity = true;
#endif

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
	Font->OutNext("Mass:              [%3.2f]", m_pPhysicsShell->getMass());
	Font->OutNext("Interior volume:   [%3.2f]", m_interior_volume);
	Font->OutNext("Active car volume: [%3.2f]", psSoundCarFactor);
	Font->OutNext("Open cabin door:   [%s]", HasOpenNormalDoor() ? "yes" : "no");
	Font->OutNext("Body surface:      [%s]", m_body_surface_material ? m_body_surface_material->m_Name.c_str() : "none");
	Font->OutNext("Body friction:     [%3.2f]", m_body_surface_friction);
	Font->OutNext("Body flotation:    [%3.2f]", m_body_surface_flotation);
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
				CPhysicsElement* Element = BoneIt->second.element;
				Fvector LinearVelocity;
				Element->get_LinearVel(LinearVelocity);
				Font->OutNext("global_linear_velocity:   %f", LinearVelocity.magnitude());
				Fvector AngularVelocityWheel;
				Element->get_AngularVel(AngularVelocityWheel);
				Font->OutNext("global_angular_velocity:  %f", AngularVelocityWheel.magnitude());
			}

			dJointID Joint = Wheel.joint->GetDJoint();
			Font->OutNext("torque_by_engine:         %f", Wheel.EngineTorque);
			Font->OutNext("applied_torque_limit:     %f", dJointGetHinge2Param(Joint, dParamFMax2));
			Font->OutNext("diff_power:               %f", Wheel.wheel_diff_power);
			Font->OutNext("velocity_by_engine_max:   %f", dJointGetHinge2Param(Joint, dParamVel2));
			Font->OutNext("velocity_current:         %f", dJointGetHinge2Angle2Rate(Joint));
			if (Wheel.max_control_velocity > 0.f)
				Font->OutNext("velocity_maximum:         %f", Wheel.max_control_velocity);

			Font->OutNext("steering_angle:           %f", dJointGetHinge2Angle1(Joint));
			Font->OutNext("steering_velocity:        %f", dJointGetHinge2Angle1Rate(Joint));
			Font->OutNext("steering_velocity_max:    %f", dJointGetHinge2Param(Joint, dParamVel));
			Font->OutNext("steering_torque:          %f", dJointGetHinge2Param(Joint, dParamFMax));
			dVector3 SteerAxis, SpinAxis;
			dJointGetHinge2Axis1(Joint, SteerAxis);
			dJointGetHinge2Axis2(Joint, SpinAxis);
			Font->OutNext("steering_axis_dot:        %f",
				SteerAxis[0] * SpinAxis[0] + SteerAxis[1] * SpinAxis[1] + SteerAxis[2] * SpinAxis[2]);
			Font->OutNext("breaking_torque:          %f", Wheel.BrakeTorque);

			if (m_break_abs)
			{
				if (Wheel.collision_params.breaking_abs && b_breaks)
					Font->SetColor(color_rgba(255, 0, 0, 255));
				else
					Font->SetColor(color_rgba(0, 255, 0, 255));
				Font->OutNext("ABS");
				Font->SetColor(color_rgba(0, 255, 0, 255));
			}

			// Contact friction chain: material pair, then wheel, weather,
			// nitro and slip multipliers, then the value passed to ODE.
			if (!Wheel.collision_params.traction)
				Font->SetColor(color_rgba(255, 0, 0, 255));
			Font->OutNext("traction");
			Font->SetColor(color_rgba(0, 255, 0, 255));

			if (Wheel.collision_params.buxe)
				Font->SetColor(color_rgba(255, 0, 0, 255));
			if (Wheel.collision_params.breaking)
				Font->SetColor(color_rgba(255, 255, 0, 255));
			if (Wheel.collision_params.slide)
				Font->SetColor(color_rgba(0, 0, 255, 255));

			LPCSTR SurfaceMaterialName = Wheel.collision_params.surface_material ? Wheel.collision_params.surface_material->m_Name.c_str() : "none";
			Font->OutNext("surface_material:         %s", SurfaceMaterialName);
			Font->OutNext("surface_friction:         %f", Wheel.collision_params.surface_friction);
			Font->OutNext("surface_flotation:        %f", Wheel.collision_params.surface_flotation);
			Font->OutNext("mu_factor:                %f", Wheel.collision_params.mu_factor);
			Font->OutNext("wheel_in_water:           [%s]", Wheel.InWater ? "yes" : "no");
			Font->OutNext("friction_rain_factor:     %f", Wheel.collision_params.friction_rain_factor);
			Font->OutNext("friction_nitro_factor:    %f", Wheel.collision_params.friction_nitro_factor);
			Font->OutNext("friction_slip_factor:     %f", Wheel.collision_params.friction_slip_factor);
			Font->OutNext("friction2_slip_factor:    %f", Wheel.collision_params.friction2_slip_factor);
			Font->OutNext("current_friction_factor:  %f", Wheel.collision_params.current_friction_factor);
			Font->OutNext("current_friction2_factor: %f", Wheel.collision_params.current_friction2_factor);

			Font->SetColor(color_rgba(0, 255, 0, 255));
			float SpringFactor, DampingFactor;
			Wheel.joint->GetJointSDfactors(SpringFactor, DampingFactor);
			Font->OutNext("wheel_spring_factor:      %f", SpringFactor);
			Font->OutNext("wheel_damping_factor:     %f", DampingFactor);

			++ActiveWheelIndex;
		}
	}
}

 void CCar::VisualUpdate(float fov)
{
	m_pPhysicsShell->InterpolateGlobalTransform(&XFORM());
	UpdateInteriorSoundFactor(active_camera != nullptr && active_camera->tag == EctFirstCam);
	if (rs_car_info)
		DrawCarInfo();
	if ((m_transmission_stage == ETransmissionStage::SwitchDown && GetSpeedKmH() > 15.f) || e_state_drive == drive && ((!IsTransmissionSwitching() || b_transmission_switch_drive) || brp))
	{
		if (e_state_drive == drive)
		{
			if (m_engine_valve == 0 && Device.fTimeGlobal - m_engine_idle_time < 3.f * Device.fTimeDelta && m_transmission_switch_time + m_transmission_switch_delay + 3.f * Device.fTimeDelta < Device.fTimeGlobal) // anti-spam disables valve sound
				m_car_sound->snd_valve.set_volume(0.f);

			// valve resets to 1 so it can be triggered again
			m_engine_valve = 1;
		}

		m_engine_drive = 1.f;
		m_engine_idle_time = Device.fTimeGlobal;
	}
	else
	{
		//Msg("%f", Device.fTimeGlobal - m_engine_idle_time);
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
			CurrentGameUI()->UIMainIngameWnd->CarPanel().SetEngineLamp(b_engine_on);
			CurrentGameUI()->UIMainIngameWnd->CarPanel().SetLightLamp(m_lights.IsOn());
			if (CurrentTransmission() == 0)
				CurrentGameUI()->UIMainIngameWnd->CarPanel().SetGear("R");
			else
				CurrentGameUI()->UIMainIngameWnd->CarPanel().SetGear(std::to_string(CurrentTransmission()).c_str());
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
	inherited::renderable_Render();
	for (CCarTurret* CWPB : m_car_weapons)
	{
		CWPB->Render_internal();
	}

	RenderDriverHands();
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

void CCar::HitFrom(CObject* who, float value, float impulse, const Fvector& dir, ALife::EHitType hit_type)
{
	if (getDestroy())
		return;

	NET_Packet packet;
	SHit hit;
	hit.GenHeader(GE_HIT, ID());
	hit.whoID = who != nullptr ? who->ID() : ID();
	hit.weaponID = who != nullptr ? who->ID() : ID();
	hit.dir = dir;
	hit.power = value;
	hit.boneID = BI_NONE;
	hit.p_in_bone_space.set(0.f, 0.f, 0.f);
	hit.impulse = impulse;
	hit.hit_type = hit_type;
	hit.Write_Packet(packet);
	u_EventSend(packet);
}

namespace
{
	float GetCarAttackerMass(CObject* who)
	{
		if (who == nullptr)
			return 0.f;

		CPhysicsShellHolder* Holder = who->cast_physics_shell_holder();
		if (Holder == nullptr)
			return 0.f;

		// A living creature keeps its mass in the movement controller; the
		// physics shell exists only for ragdolls and physical objects.
		if (CCharacterPhysicsSupport* Support = Holder->character_physics_support())
		{
			if (CPHMovementControl* Movement = Support->movement())
			{
				const float Mass = Movement->GetMass();
				if (Mass > 0.f)
					return Mass;
			}
		}

		return Holder->GetMass();
	}
}

void CCar::HitFromCreature(CObject* who, float value, float impulse, const Fvector& dir, ALife::EHitType hit_type)
{
	if (getDestroy())
		return;

	// Damage keeps using the regular hit event. The car has no single hit bone
	// for that event, so the physical reaction is applied separately below.
	HitFrom(who, value, 0.f, dir, hit_type);

	if (m_pPhysicsShell == nullptr || !m_pPhysicsShell->isActive())
		return;

	const float CarMass = m_pPhysicsShell->getMass();
	if (CarMass <= 0.f)
		return;

	// The attack impulse is tuned for the actor, while the monster body adds
	// momentum of its own. Convert both to momentum, cap the result and divide
	// by the car mass: a heavy truck moves much less than a light car.
	constexpr float ImpulseToMomentum = 8.f;  // kg*m/s per attack impulse unit
	constexpr float AttackerBodySpeed = 3.f;  // m/s, monster body momentum
	constexpr float MaxMomentum = 2400.f;     // kg*m/s, strongest melee hit
	constexpr float MaxSpeed = 3.f;           // m/s, guard for very light cars

	const float AttackerMass = GetCarAttackerMass(who);
	const float AttackImpulse = std::max(impulse, 0.f);
	const float Momentum = std::min(
		(AttackImpulse * ImpulseToMomentum + AttackerMass * AttackerBodySpeed) * m_mutant_hit_impulse_factor,
		MaxMomentum);
	if (Momentum <= 0.f)
		return;

	const float Speed = std::min(Momentum / CarMass, MaxSpeed);

	const Fvector Contact = GetNearestPoint(who != nullptr ? who->Position() : Position());
	ApplyPush(dir, Speed, Contact);
}

void CCar::ApplyPush(const Fvector& dir, float speed, const Fvector& world_point)
{
	if (getDestroy() || m_pPhysicsShell == nullptr || !m_pPhysicsShell->isActive())
		return;

	if (OwnerActor() && rs_car_god)
		return;

	if (speed <= 0.f)
		return;

	const float Mass = m_pPhysicsShell->getMass();
	if (Mass <= 0.f)
		return;

	Fvector PushDir = dir;
	PushDir.y *= GravityFactorImpulse();
	if (PushDir.magnitude() < EPS)
		return;
	PushDir.normalize();

	Fvector LocalPoint;
	Fmatrix Inv;
	Inv.invert(XFORM());
	Inv.transform_tiny(LocalPoint, world_point);

	// Apply the push at mid-body height. A bite at wheel level must not roll the
	// car over, while the horizontal offset still gives a realistic yaw.
	Fbox Box = BoundingBox();
	if (Box.is_valid())
	{
		LocalPoint.y = (Box.min.y + Box.max.y) * 0.5f;
	}

	// ODE expects the point in body space and the direction in world space.
	m_pPhysicsShell->applyImpulseTrace(LocalPoint, PushDir, speed * Mass);
}

void CCar::ApplyGigantKick(const Fvector& dir)
{
	if (getDestroy() || m_pPhysicsShell == nullptr || !m_pPhysicsShell->isActive())
		return;

	// The gigant ground kick is a physical reaction and intentionally ignores
	// rs_car_god, matching the legacy mass-scaled impulse.
	Fvector PushDir = dir;
	if (!safe_normalize(PushDir))
		return;

	// Fixed momentum: heavy cars receive less speed than light ones. A
	// negative configured value keeps the legacy mass-scaled impulse for
	// sections that do not define the new key.
	float Impulse = m_gigant_kick_impulse;
	if (Impulse < 0.f)
		Impulse = 20.f * m_pPhysicsShell->getMass();

	if (Impulse <= 0.f)
		return;

	if (!m_pPhysicsShell->isEnabled())
		m_pPhysicsShell->Enable();

	m_pPhysicsShell->applyImpulse(PushDir, Impulse);
}

void CCar::PushByActor(const Fvector& dir, const Fvector& world_point)
{
	constexpr float BaseImpulse = 750.f;

	const float Impulse = BaseImpulse * m_push_impulse_factor;
	if (Impulse <= 0.f || getDestroy() || m_pPhysicsShell == nullptr || !m_pPhysicsShell->isActive())
		return;

	Fvector PushDir = dir;
	if (!safe_normalize(PushDir))
		return;

	// Apply the impulse exactly at the point the player is aiming at and in
	// the view direction. This lets a hit on a door, fender or wheel produce
	// the corresponding torque instead of always pushing the mid-body.
	Fvector LocalPoint;
	Fmatrix Inv;
	Inv.invert(XFORM());
	Inv.transform_tiny(LocalPoint, world_point);

	if (!m_pPhysicsShell->isEnabled())
		m_pPhysicsShell->Enable();

	m_pPhysicsShell->applyImpulseTrace(LocalPoint, PushDir, Impulse);
}

//void CCar::Hit(float P,Fvector &dir,CObject * who,s16 element,Fvector p_in_object_space, float impulse, ALife::EHitType hit_type)
bool CCar::CollisionHitStartsCooldown(u16 bone_id, float power)
{
	// Wheel contacts have their own damage path and must not consume the
	// body-hit cooldown, otherwise a wheel hit can swallow a following body
	// impact.
	if (m_wheels_map.find(bone_id) != m_wheels_map.end())
		return false;

	if (power <= 0.f || getDestroy() || !g_Alive())
		return false;

	if (OwnerActor() && rs_car_god)
		return false;

	// The collision receiver sends eHitTypeStrike hits. If this car is immune
	// to strikes, no health is lost and the cooldown must not start.
	return GetHitImmunity(ALife::eHitTypeStrike) > 0.f;
}

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

	// CEntity::CalcCondition() sets health through CEntity::SetfHealth, which
	// bypasses the CCar override, so stall here as well.
	if (!b_exploded && GetfHealth() <= 0.f && b_engine_on)
		Stall();

	if (m_zero_health_explosion_timer)
	{
		if (GetfHealth() <= 0.f)
			StartExplosionTimer();
	}
	else if (!b_exploded && GetfHealth() > 0.f && !CDelayedActionFuse::isActive())
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

	const u16 OldLevel = m_level_applied;

	// Mark the level as applied before running the effects, like the original
	// car did. Starting the legacy fuse raises health back to the level 2
	// threshold and calls HitEffect() again; with the level still unapplied
	// that re-entered ApplyDamage(2) and restarted the check.
	CDamagableItem::ApplyDamage(level);

	if (level > OldLevel)
	{
		switch(level)
		{
			case 1: m_damage_particles.Play1(this);break;
			case 2:
				if (!m_zero_health_explosion_timer && !CDelayedActionFuse::isActive())
					CDelayedActionFuse::CheckCondition(GetfHealth());
				m_damage_particles.Play2(this);
				break;
			case 3:
				if (m_zero_health_explosion_timer)
					StartExplosionTimer();
				break;
		}
	}
	else
	{
		if (m_zero_health_explosion_timer)
		{
			if (level < 3)
				StopExplosionTimer();
		}
		else if (level < 2)
		{
			CDelayedActionFuse::Reset();
		}

		if (level < 2)
			m_damage_particles.Stop2(this);
		if (level < 1)
			m_damage_particles.Stop1(this);
	}
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

bool CCar::HasDamagedParts()
{
	for (auto& door : m_doors)
	{
		if (door.second.IsDamaged())
			return true;
	}
	for (auto& wheel : m_wheels_map)
	{
		if (wheel.second.IsDamaged())
			return true;
	}
	return false;
}

bool CCar::RepairFromItem()
{
	bool Repaired = false;

	const float MaxHealth = GetMaxHealth();
	if (MaxHealth > 0.f)
	{
		// A burning car can have negative health after Die(). Repair from the
		// burning state must restore health from zero, otherwise one repair
		// item would leave the car below zero and consume the item for nothing.
		const float OldHealth = std::max(GetfHealth(), 0.f);
		if (OldHealth < MaxHealth)
		{
			float Fraction = RepairAmount;
			clamp(Fraction, 0.f, 1.f);
			const float Amount = MaxHealth * Fraction;
			if (Amount > 0.f)
			{
				float NewHealth = OldHealth + Amount;
				if (NewHealth > MaxHealth)
					NewHealth = MaxHealth;

				SetfHealth(NewHealth);
				Repaired = true;
			}
		}
	}

	// A repair item always restores doors and wheels completely, even when the
	// body health is only partially restored or is already full.
	if (HasDamagedParts())
	{
		RepairDamageItems();
		Repaired = true;
	}

	return Repaired;
}

float CCar::SetfHealth(float value)
{
	float result = CEntity::SetfHealth(value);

	// A destroyed engine cannot keep running. Stall before the explosion
	// timer starts so the car stops immediately at zero health.
	if (value <= 0.f && b_engine_on)
		Stall();

	if (m_car_sound)
	{
		if (value > 0.f)
			m_car_sound->StopFire();
		else
			m_car_sound->StartFire();
	}

	if (m_zero_health_explosion_timer)
	{
		if (value > 0.f)
			StopExplosionTimer();
		else
			StartExplosionTimer();
	}
	CDamagableItem::HitEffect();
	UpdateDamageEffects();
	return result;
}

void CCar::StartExplosionTimer(float time)
{
	if (b_exploded || !m_zero_health_explosion_timer)
		return;

	if (GetfHealth() <= 0.f && b_engine_on)
		Stall();

	if (time < 0.f)
	{
		if (CDelayedActionFuse::isActive())
			return;
		time = m_explosion_delay;
	}

	if (time <= 0.f)
	{
		CarExplode();
		return;
	}

	CDelayedActionFuse::Reset();
	CDelayedActionFuse::Initialize(time, 0.f);
	if (!CDelayedActionFuse::isInitialized())
	{
		CarExplode();
		return;
	}

	CDelayedActionFuse::CheckCondition(0.f);
	m_damage_particles.Play3(this);
	if (m_car_sound)
		m_car_sound->StartFire();
}

void CCar::StopExplosionTimer()
{
	CDelayedActionFuse::Reset();
	m_damage_particles.Stop3(this);
	if (m_car_sound)
		m_car_sound->StopFire();
}

void CCar::RestoreExplosionState()
{
	if (!m_zero_health_explosion_timer)
		return;

	if (m_saved_explosion_pending)
		StartExplosionTimer(m_saved_explosion_time);
	else if (GetfHealth() <= 0.f)
		StartExplosionTimer();
}

void CCar::RecalculateExitPosition()
{
	if (m_doors.empty() || bones_bind_forms.empty())
	{
		m_exit_position.set(Position());
		return;
	}

	auto door = m_doors.find(m_exit_door_bone);
	if (door == m_doors.end())
	{
		door = m_doors.begin();
	}

	door->second.GetExitPosition(m_exit_position);
}

void CCar::detach_Actor()
{
	if(!Owner()) return;

	const bool SprintHeld =
		Level().IR_GetKeyState(SDL_SCANCODE_LSHIFT) ||
		Level().IR_GetKeyState(SDL_SCANCODE_RSHIFT) ||
		(pInput->GetControllerMode() && pInput->iGetAsyncGamepadKeyState(get_action_dik(kSPRINT_TOGGLE)));

	// The actor can be detached without a prior CCar::Exit() call (death,
	// forced exit, vehicle destruction). Recompute the point from the current
	// car transform, otherwise the body is moved to a stale exit position or
	// to the world origin.
	RecalculateExitPosition();

	Owner()->setVisible(1);
	DestroyDriverHands();
	ResetCabinSound();
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
	if (SprintHeld)
		ReleaseBreaks();
	else
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
	m_steer_anim_state = 0;
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
	LoadDriverHands();
	return true;
}

void CCar::TeleportCarAndDriver(const Fvector& position)
{
	if (m_pPhysicsShell == nullptr || OwnerActor() == nullptr)
	{
		return;
	}

	// Teleporting is a camera operation, not an engine stop. Preserve the
	// current engine and drive state; zeroing the shell velocities below keeps
	// the car stationary at the destination. Keep the car orientation and move
	// it so the driver's seat lands at the requested camera position. Moving
	// the actor alone does not work: the car copies Owner()->XFORM() from its
	// seat transform every frame.
	Fvector SeatPosition;
	XFORM().transform_tiny(SeatPosition, m_sits_transforms.c);

	Fvector Delta;
	Delta.sub(position, SeatPosition);

	Fmatrix CarForm = XFORM();
	CarForm.c.add(Delta);

	if (!m_pPhysicsShell->isActive())
	{
		m_pPhysicsShell->Activate(XFORM(), 0, XFORM());
	}

	m_pPhysicsShell->Enable();
	m_pPhysicsShell->SetGlTransformDynamic(CarForm);
	m_pPhysicsShell->set_LinearVel(zero_vel);
	m_pPhysicsShell->set_AngularVel(zero_vel);

	Fmatrix CarWorld;
	m_pPhysicsShell->GetGlobalTransformDynamic(&CarWorld);
	XFORM().set(CarWorld);
	spatial_move();

	OwnerActor()->XFORM().mul_43(XFORM(), m_sits_transforms);

	if (characters_scraper_shell != nullptr)
	{
		characters_scraper_shell->SetTransform(XFORM(), mh_clear);
		characters_scraper_shell->set_LinearVel(zero_vel);
		characters_scraper_shell->set_AngularVel(zero_vel);
	}
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
	SDoor* BestDoor = nullptr;
	float BestScore = -1.0e30f;

	Fvector LookDir = dir;
	LookDir.y = 0.f;
	const bool HasLookDir = safe_normalize(LookDir);

	for (auto& Door : m_doors)
	{
		if (!Door.second.CanExit(pos, dir))
			continue;

		// Prefer the door the player is actually looking through. Without this,
		// several open doors (for example a previously opened left door) make
		// Exit pick the first one in map order instead of the intended door.
		float Score = Door.second.TestPass(pos, dir) ? 2.f : 0.f;
		if (HasLookDir)
		{
			Fvector ToDoor = Door.second.GetWorldPosition();
			ToDoor.sub(pos);
			ToDoor.y = 0.f;
			if (safe_normalize(ToDoor))
				Score += ToDoor.dotproduct(LookDir);
		}

		if (!BestDoor || Score > BestScore)
		{
			BestDoor = &Door.second;
			BestScore = Score;
		}
	}

	if (!BestDoor)
		return false;

	// detach_Actor() recalculates the exit point from m_exit_door_bone.
	m_exit_door_bone = BestDoor->bone_id;
	BestDoor->GetExitPosition(m_exit_position);
	return true;

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
	LoadSurfaceParams();
	LoadCollisionSounds();

	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	CInifile* ini = pKinematics->LL_UserData();
	if (ini)
	{
		LoadExplosionAndDriverDefinitions(ini, pKinematics);
		LoadHandlingAndCameraDefinitions(ini);
		LoadEngineDefinitions(ini);
		LoadSoundDefinitions(ini);
		LoadExhaustDefinitions(ini);
		LoadTransmissionDefinitions(ini);
		LoadVisualDefinitions(ini);
	}
}

void CCar::LoadSurfaceParams()
{
	m_surface_params.clear();

	if (!pSettings || !pSettings->line_exist(cNameSect_str(), "surface_params"))
		return;

	LPCSTR SectionName = pSettings->r_string(cNameSect_str(), "surface_params");
	if (!pSettings->section_exist(SectionName))
	{
		Msg("! [%s] surface_params section '%s' does not exist", cNameSect_str(), SectionName);
		return;
	}

	const CInifile::Sect& Section = pSettings->r_section(SectionName);
	for (const CInifile::Item& Item : Section.Data)
	{
		auto MaterialIt = GMLib.GetMaterialIt(Item.first.c_str());
		if (MaterialIt == GMLib.LastMaterial())
		{
			// The car pack is shared by several games, so a surface entry may
			// legitimately be absent from this game's gamemtl.xr.
#ifndef MASTER_GOLD
			Msg("~ [%s] skipping surface material '%s' from '%s' (not present in this game)", cNameSect_str(), Item.first.c_str(), SectionName);
#endif
			continue;
		}

		const Fvector2 Values = pSettings->r_fvector2(SectionName, Item.first.c_str());

		SSurfaceParams Params;
		Params.HasFriction = Values.x >= 0.f;
		Params.Friction = Params.HasFriction ? Values.x : 1.f;
		Params.HasFlotation = Values.y >= 0.f;
		Params.Flotation = Params.HasFlotation ? Values.y : 1.f;

		if (Params.HasFriction)
			clamp(Params.Friction, 0.f, 10.f);
		if (Params.HasFlotation)
			clamp(Params.Flotation, 0.f, 1.f);

		m_surface_params[(*MaterialIt)->ID] = Params;
	}
}

const CCar::SSurfaceParams* CCar::FindSurfaceParams(const SGameMtl* Material) const
{
	if (!Material)
		return nullptr;

	auto It = m_surface_params.find(Material->ID);
	return It == m_surface_params.end() ? nullptr : &It->second;
}

void CCar::LoadExplosionAndDriverDefinitions(CInifile* ini, IKinematics* pKinematics)
{
	if (ini->section_exist("explosion"))
		CExplosive::Load(ini, "explosion");

	m_sits_transforms.set(pKinematics->LL_GetTransform(pKinematics->LL_BoneID(ini->read_if_exists<str_c>("car_definition", "driver_place", pKinematics->LL_BoneName_dbg(pKinematics->LL_GetBoneRoot())))));

	Fmatrix	driver_xform_tune;
	Fvector driver_direction = (ini->read_if_exists<Fvector3>("car_definition", "driver_direction", Fvector().set(0.0f, 0.0f, 0.0f)));
	driver_xform_tune.setXYZ(deg2rad(driver_direction.x), deg2rad(driver_direction.y), deg2rad(driver_direction.z));
	driver_xform_tune.c.set(ini->read_if_exists<Fvector3>("car_definition", "driver_position", Fvector().set(0.0f, 0.0f, 0.0f)));
	m_sits_transforms.mulB_43(driver_xform_tune);

	// Original vehicles use the single camera_pos key. Cars-Ext splits it
	// into first/lookat/free positions; keep the original key as fallback.
	const Fvector DefaultCameraPos = ini->read_if_exists<Fvector3>("car_definition", "camera_pos", Fvector().set(0.0f, 0.0f, 0.0f));
	m_camera_position[EctFirstCam] = ini->read_if_exists<Fvector3>("car_definition", "camera_pos_firsteye", DefaultCameraPos);
	m_camera_position[EctSecondCam] = ini->read_if_exists<Fvector3>("car_definition", "camera_pos_lookat", m_camera_position[EctFirstCam]);
	m_camera_position[EctThridCam] = ini->read_if_exists<Fvector3>("car_definition", "camera_pos_free", m_camera_position[EctSecondCam]);
	m_camera_position[EctFourthCam] = ini->read_if_exists<Fvector3>("car_definition", "camera_pos_free2", m_camera_position[EctThridCam]);

	m_camera_inertion_n = ini->read_if_exists<float>("car_definition", "camera_inertion_n", m_camera_inertion_n);
	m_camera_inertion_k = ini->read_if_exists<float>("car_definition", "camera_inertion_k", m_camera_inertion_k);
	m_camera_angle_factor = ini->read_if_exists<float>("car_definition", "camera_angle_factor", m_camera_angle_factor);
	clamp(m_camera_angle_factor, 0.1f, 2.f);
}

void CCar::LoadHandlingAndCameraDefinitions(CInifile* ini)
{
	m_current_rpm_brp_n_max = pSettings->read_if_exists<float>(cNameSect_str(), "handbrake_rpm_threshold", m_current_rpm_brp_n_max);
	m_current_rpm_brp_decrease = pSettings->read_if_exists<float>(cNameSect_str(), "handbrake_rpm_decrease", m_current_rpm_brp_decrease);
	m_slip_sliding_threshold = pSettings->read_if_exists<float>(cNameSect_str(), "slip_sliding_threshold", m_slip_sliding_threshold);
	m_vel_sliding_threshold = pSettings->read_if_exists<float>(cNameSect_str(), "vel_sliding_threshold", m_vel_sliding_threshold);
	m_friction_slip_factor = pSettings->read_if_exists<float>(cNameSect_str(), "friction_slip_factor", m_friction_slip_factor);
	m_friction2_slip_factor = pSettings->read_if_exists<float>(cNameSect_str(), "friction2_slip_factor", m_friction2_slip_factor);
	m_friction_spin_factor = pSettings->read_if_exists<float>(cNameSect_str(), "friction_spin_factor", m_friction_spin_factor);
	m_friction2_spin_factor = pSettings->read_if_exists<float>(cNameSect_str(), "friction2_spin_factor", m_friction2_spin_factor);
	m_friction_slip_speed = pSettings->read_if_exists<float>(cNameSect_str(), "friction_slip_speed", m_friction_slip_speed);
	m_friction2_steering_wheels = pSettings->read_if_exists<float>(cNameSect_str(), "friction2_steering_wheels", m_friction2_steering_wheels);
	m_friction2_nonsteering_wheels = pSettings->read_if_exists<float>(cNameSect_str(), "friction2_nonsteering_wheels", m_friction2_nonsteering_wheels);
	m_friction2_slip_factor = pSettings->read_if_exists<float>(cNameSect_str(), "friction2_slip_factor", m_friction2_slip_factor);
	m_friction2_brp_boost = pSettings->read_if_exists<float>(cNameSect_str(), "friction2_brp_boost", m_friction2_brp_boost);
	m_drift_start_boost = pSettings->read_if_exists<float>(cNameSect_str(), "drift_start_boost", m_drift_start_boost);
	m_drift_boost_time = pSettings->read_if_exists<float>(cNameSect_str(), "drift_boost_time", m_drift_boost_time);
	m_steer_spin_boost = pSettings->read_if_exists<float>(cNameSect_str(), "steer_spin_boost", m_steer_spin_boost);
	m_steer_speed_boost = pSettings->read_if_exists<float>(cNameSect_str(), "steer_speed_boost", m_steer_speed_boost);
	m_steer_turn_factor = pSettings->read_if_exists<float>(cNameSect_str(), "steer_turn_factor", m_steer_turn_factor);
	m_steer_counter_turn_factor = pSettings->read_if_exists<float>(cNameSect_str(), "steer_counter_turn_factor", m_steer_counter_turn_factor);
	m_steer_turn_compensation = pSettings->read_if_exists<float>(cNameSect_str(), "steer_turn_compensation", m_steer_turn_compensation);
	m_steer_turn_compensation_max = pSettings->read_if_exists<float>(cNameSect_str(), "steer_turn_compensation_max", m_steer_turn_compensation_max);
	m_steer_turn_delta = pSettings->read_if_exists<float>(cNameSect_str(), "steer_turn_delta", m_steer_turn_delta);
	m_brake_boost = pSettings->read_if_exists<float>(cNameSect_str(), "brake_boost", m_brake_boost);
	m_switch_shake_boost = pSettings->read_if_exists<float>(cNameSect_str(), "switch_shake_boost", m_switch_shake_boost);
	m_engine_shake_boost = pSettings->read_if_exists<float>(cNameSect_str(), "engine_shake_boost", m_engine_shake_boost);
	m_wheels_spin_factor_start = pSettings->read_if_exists<float>(cNameSect_str(), "wheels_spin_factor_start", m_wheels_spin_factor_start);
	m_wheels_spin_factor_stop = pSettings->read_if_exists<float>(cNameSect_str(), "wheels_spin_factor_stop", m_wheels_spin_factor_stop);
	m_wheels_sound_delay = pSettings->read_if_exists<float>(cNameSect_str(), "wheels_sound_delay", m_wheels_sound_delay);
	m_firsteye_panel = pSettings->read_if_exists<bool>(cNameSect_str(), "firsteye_panel", m_firsteye_panel);
	m_allow_inventory_control = pSettings->read_if_exists<bool>(cNameSect_str(), "allow_inventory_control", m_allow_inventory_control);
	m_allow_mutant_bite_in_car = pSettings->read_if_exists<bool>(cNameSect_str(), "allow_mutant_bite_in_car", m_allow_mutant_bite_in_car);
	m_poltergeist_can_lift = pSettings->read_if_exists<bool>(cNameSect_str(), "poltergeist_can_lift", m_poltergeist_can_lift);
	m_interior_volume = pSettings->read_if_exists<float>(cNameSect_str(), "interior_volume", m_interior_volume);
	m_hit_stall_threshold = pSettings->read_if_exists<float>(cNameSect_str(), "hit_stall_threshold", m_hit_stall_threshold);
	m_hit_anomaly_scale = pSettings->read_if_exists<float>(cNameSect_str(), "hit_anomaly_scale", m_hit_anomaly_scale);
	m_mutant_hit_impulse_factor = pSettings->read_if_exists<float>(cNameSect_str(), "mutant_hit_impulse_factor", m_mutant_hit_impulse_factor);
	clamp(m_mutant_hit_impulse_factor, 0.f, 5.f);
	m_burer_gravi_impulse_factor = pSettings->read_if_exists<float>(cNameSect_str(), "burer_gravi_impulse_factor", m_burer_gravi_impulse_factor);
	clamp(m_burer_gravi_impulse_factor, 0.f, 5.f);
	m_push_impulse_factor = pSettings->read_if_exists<float>(cNameSect_str(), "push_impulse_factor", m_push_impulse_factor);
	clamp(m_push_impulse_factor, 0.f, 10.f);
	m_ram_damage_min_speed = pSettings->read_if_exists<float>(cNameSect_str(), "ram_damage_min_speed", m_ram_damage_min_speed);
	if (m_ram_damage_min_speed < 0.f)
		m_ram_damage_min_speed = 0.f;

	m_ram_crash_speed_min = pSettings->read_if_exists<float>(cNameSect_str(), "ram_crash_speed_min", m_ram_crash_speed_min);
	if (m_ram_crash_speed_min >= 0.f)
		clamp(m_ram_crash_speed_min, 0.f, 1000.f);
	else
		m_ram_crash_speed_min = -1.f;
	m_ram_crash_speed_max = pSettings->read_if_exists<float>(cNameSect_str(), "ram_crash_speed_max", m_ram_crash_speed_max);
	if (m_ram_crash_speed_max >= 0.f)
		clamp(m_ram_crash_speed_max, 0.f, 1000.f);
	else
		m_ram_crash_speed_max = -1.f;

	m_ram_push_scale = pSettings->read_if_exists<float>(cNameSect_str(), "ram_push_scale", m_ram_push_scale);
	clamp(m_ram_push_scale, 0.f, 10.f);
	m_ram_max_push_speed = pSettings->read_if_exists<float>(cNameSect_str(), "ram_max_push_speed", m_ram_max_push_speed);
	clamp(m_ram_max_push_speed, 0.f, 100.f);
	m_ram_contact_memory = pSettings->read_if_exists<float>(cNameSect_str(), "ram_contact_memory", m_ram_contact_memory);
	clamp(m_ram_contact_memory, 0.f, 2000.f);
	m_ram_min_speed = pSettings->read_if_exists<float>(cNameSect_str(), "ram_min_speed", m_ram_min_speed);
	clamp(m_ram_min_speed, 0.f, 50.f);
	m_ram_separation_bias = pSettings->read_if_exists<float>(cNameSect_str(), "ram_separation_bias", m_ram_separation_bias);
	clamp(m_ram_separation_bias, 0.f, 15.f);
	m_ram_mutant_push_speed = pSettings->read_if_exists<float>(cNameSect_str(), "ram_mutant_push_speed", m_ram_mutant_push_speed);
	clamp(m_ram_mutant_push_speed, 0.f, 5.f);
	m_ram_mutant_slide_scale = pSettings->read_if_exists<float>(cNameSect_str(), "ram_mutant_slide_scale", m_ram_mutant_slide_scale);
	clamp(m_ram_mutant_slide_scale, 0.f, 1.f);

	m_ram_mutant_slide_lock = pSettings->read_if_exists<float>(cNameSect_str(), "ram_mutant_slide_lock", m_ram_mutant_slide_lock);
	clamp(m_ram_mutant_slide_lock, 0.f, 5000.f);
	m_ram_mutant_slide_flip = pSettings->read_if_exists<float>(cNameSect_str(), "ram_mutant_slide_flip", m_ram_mutant_slide_flip);
	clamp(m_ram_mutant_slide_flip, 0.f, 1.f);
	m_ram_mutant_slide_confirm = pSettings->read_if_exists<float>(cNameSect_str(), "ram_mutant_slide_confirm", m_ram_mutant_slide_confirm);
	clamp(m_ram_mutant_slide_confirm, 0.f, 2000.f);
	m_ram_mutant_slide_memory = pSettings->read_if_exists<float>(cNameSect_str(), "ram_mutant_slide_memory", m_ram_mutant_slide_memory);
	clamp(m_ram_mutant_slide_memory, 0.f, 5000.f);

	m_ram_start_push_speed = pSettings->read_if_exists<float>(cNameSect_str(), "ram_start_push_speed", m_ram_start_push_speed);
	clamp(m_ram_start_push_speed, 0.f, 5.f);

	// Fixed momentum of the pseudogiant ground kick for this car, kg*m/s.
	// -1 keeps the legacy 20 * car_mass impulse, 0 disables the car push.
	m_gigant_kick_impulse = pSettings->read_if_exists<float>(cNameSect_str(), "gigant_kick_impulse", m_gigant_kick_impulse);
	if (m_gigant_kick_impulse >= 0.f)
		clamp(m_gigant_kick_impulse, 0.f, 200000.f);

	loadcamera("car_firsteye_cam", ini, camera[EctFirstCam]);
	loadcamera("car_look_cam", ini, camera[EctSecondCam]);
	m_roll_lock_second = ini->read_if_exists<bool>("car_look_cam", "roll_lock", false);
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

	m_start_probability = ini->read_if_exists<float>("car_definition", "start_probability", m_start_probability);
	clamp(m_start_probability, 0.f, 1.f);

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

	m_power_increment_factor = ini->read_if_exists<float>("car_definition", "power_increment_factor", m_power_increment_factor);
	m_power_decrement_factor = ini->read_if_exists<float>("car_definition", "power_decrement_factor", m_power_decrement_factor);
	m_rpm_increment_factor = ini->read_if_exists<float>("car_definition", "rpm_increment_factor", m_rpm_increment_factor);
	m_rpm_decrement_factor = ini->read_if_exists<float>("car_definition", "rpm_decrement_factor", m_rpm_decrement_factor);
	m_rpm_brp_increment_factor = ini->read_if_exists<float>("car_definition", "rpm_brp_increment_factor", m_rpm_brp_increment_factor);
	m_rpm_brp_decrement_factor = ini->read_if_exists<float>("car_definition", "rpm_brp_decrement_factor", m_rpm_brp_decrement_factor);

	m_power_neutral_factor = ini->read_if_exists<float>("car_definition", "power_neutral_factor", m_power_neutral_factor);
	clamp(m_power_neutral_factor, 0.0f, 1.0f);

	m_nitro = ini->read_if_exists<bool>("car_definition", "nitro", m_nitro);
	m_nitro_power_factor = ini->read_if_exists<float>("car_definition", "nitro_power_factor", m_nitro_power_factor);
	m_nitro_fuel_factor = ini->read_if_exists<float>("car_definition", "nitro_fuel_factor", m_nitro_fuel_factor);
	m_nitro_friction_factor = ini->read_if_exists<float>("car_definition", "nitro_friction_factor", m_nitro_friction_factor);
	m_nitro_wheelie_boost = ini->read_if_exists<float>("car_definition", "nitro_wheelie_boost", m_nitro_wheelie_boost);
	m_wheelie_boost = ini->read_if_exists<float>("car_definition", "wheelie_boost", m_wheelie_boost);
	m_nitro_wheelie_friction_brp_factor = ini->read_if_exists<float>("car_definition", "nitro_wheelie_friction_brp_factor", m_nitro_wheelie_friction_brp_factor);
	m_nitro_particle_scale = ini->read_if_exists<float>("car_definition", "nitro_particle_scale", m_nitro_particle_scale);
	b_nitro_particles_enabled = ini->read_if_exists<bool>("car_definition", "nitro_particles_enabled", b_nitro_particles_enabled);
}

void CCar::LoadExhaustDefinitions(CInifile* ini)
{
	// read_if_exists returns the stock effect for a missing or empty
	// exhaust_particles value. A name that is not a loaded effect would assert
	// inside the renderer, so keep the fallback for a bad car config.
	const char* ExhaustParticles = ini->read_if_exists<str_c>("car_definition", "exhaust_particles", DefaultExhaustParticles);
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
	m_pop_probability = ini->read_if_exists<float>("car_definition", "pop_probability", m_pop_probability);
	m_pop_single = ini->read_if_exists<bool>("car_definition", "pop_single", m_pop_single);
}

void CCar::LoadTransmissionDefinitions(CInifile* ini)
{
	b_cvt_transmission = ini->read_if_exists<bool>("car_definition", "cvt_transmission", b_cvt_transmission);
	m_transmission_switch_delay = ini->read_if_exists<float>("car_definition", "transmission_switch_delay", m_transmission_switch_delay);
	m_transmission_switch_power_factor = ini->read_if_exists<float>("car_definition", "transmission_switch_power_factor", m_transmission_switch_power_factor);
	clamp(m_transmission_switch_power_factor, 0.f, 1.f);
	m_transmission_switch_volume = ini->read_if_exists<float>("car_definition", "transmission_switch_volume", m_transmission_switch_volume);
	b_transmission_switch_drive = ini->read_if_exists<bool>("car_definition", "transmission_switch_drive", b_transmission_switch_drive);

	InitParabola();

	m_axle_friction = ini->read_if_exists<float>("car_definition", "axle_friction", 1.0f);
	m_steering_speed = ini->read_if_exists<float>("car_definition", "steering_speed", 1.0f);

	if (ini->line_exist("car_definition", "break_time"))
		m_break_time = ini->r_float("car_definition", "break_time");

	m_break_abs = ini->read_if_exists<bool>("car_definition", "break_abs", m_break_abs);
	m_break_abs_power = ini->read_if_exists<float>("car_definition", "break_abs_power", m_break_abs_power);
	clamp(m_break_abs_power, 0.f, 1.f);
	m_break_abs_individual = ini->read_if_exists<bool>("car_definition", "break_abs_individual", m_break_abs_individual);
	m_break_abs_threshold = ini->read_if_exists<float>("car_definition", "break_abs_threshold", m_break_abs_threshold);
	m_steer_nonlinear = ini->read_if_exists<bool>("car_definition", "steer_nonlinear", m_steer_nonlinear);
	m_steer_invert = ini->read_if_exists<bool>("car_definition", "steer_invert", m_steer_invert);
	b_diff_lock = ini->read_if_exists<bool>("car_definition", "diff_lock", b_diff_lock);
	m_diff_lock_power = ini->read_if_exists<float>("car_definition", "diff_lock_power", m_diff_lock_power);

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
	m_torch_usage = ini->read_if_exists<bool>("lights", "torchlight", m_torch_usage);

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

	if (ini->section_exist("animations") && ini->line_exist("animations", "driver_animation_set"))
	{
		// new: partial torso/legs cycles (IXRAY set).
		// old: type-specific full-body cycles (LA set).
		m_driver_anim_old = ini->r_token("animations", "driver_animation_set", DriverAnimationSetToken) != 0;
	}

	if (ini->section_exist("doors"))
	{
		m_doors_torque_factor = ini->read_if_exists<float>("doors", "open_torque_factor", m_doors_torque_factor);
		m_doors_hold_factor = ini->read_if_exists<float>("doors", "hold_torque_factor", m_doors_hold_factor);
		m_doors_hold_at_speed = ini->read_if_exists<bool>("doors", "hold_at_speed", m_doors_hold_at_speed);
		m_doors_vel_factor = ini->read_if_exists<float>("doors", "open_vel_factor", m_doors_vel_factor);
	}

	m_damage_particles.Init(this);

	// The stock IXRAY material pairs have no collide marks for
	// objects\car_wheel, so the Cars-Ext tire trails use their own
	// surface-specific wallmark arrays instead of relying on gamemtl.xr.
	if (pSettings->read_if_exists<bool>(cNameSect_str(), "tire_trails", true)
		&& m_tire_wallmarks_ground->empty())
	{
		m_tire_wallmarks_ground->AppendMark(pSettings->read_if_exists<str_c>(cNameSect_str(), "tire_trail_wallmark_name", "wm\\wm_tire_trail_1"));
		m_tire_wallmarks_dirt->AppendMark(pSettings->read_if_exists<str_c>(cNameSect_str(), "tire_trail_wallmark_name_dirt", "wm\\wm_tire_trail_dirt_1"));
		m_tire_wallmarks_grass->AppendMark(pSettings->read_if_exists<str_c>(cNameSect_str(), "tire_trail_wallmark_name_grass", "wm\\wm_tire_trail_grass_1"));
	}

	// Wheel sounds are configured in vehicles.ltx. Stock gamemtl.xr uses
	// human footsteps for objects\car_wheel, so the Cars-Ext defaults use
	// the dedicated LA wheel loops and skid sounds instead.
	if (pSettings->read_if_exists<bool>(cNameSect_str(), "tire_sounds", true))
	{
		auto load_sound = [&](ref_sound& sound, LPCSTR key, LPCSTR default_name)
		{
			if (sound._p)
				return;
			LPCSTR name = pSettings->read_if_exists<str_c>(cNameSect_str(), key, default_name);
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
	SetCollisionHitThreshold(ini->read_if_exists<float>("car_definition", "collision_hit_threshold", 1.f));
	// A single impact can touch several collision geoms at once. Only the first
	// damaging hit is applied within this window.
	SetCollisionHitCooldown(ini->read_if_exists<float>("car_definition", "collision_hit_cooldown", 0.1f));

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
		m_downforce = ini->read_if_exists<float>("air_resistance", "downforce", m_downforce);
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
	m_ref_radius = ini->read_if_exists<float>("car_definition", "reference_radius", 1.f);
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
	m_zero_health_explosion_timer = ini->read_if_exists<bool>("car_definition", "zero_health_explosion_timer", false);
	// Legacy configs never had an explicit default; keep the original 120 s
	// fuse length when time_to_explosion is missing. New-mode configs use an
	// immediate explosion in that case.
	m_explosion_delay = ini->read_if_exists<float>("car_definition", "time_to_explosion",
		m_zero_health_explosion_timer ? 0.f : 120.f);
	CDelayedActionFuse::Initialize(m_explosion_delay,
		m_zero_health_explosion_timer ? 0.f : CDamagableItem::DamageLevelToHealth(2));
	{
		for (auto& i : m_wheels_map)
		{
			i.second.Init();
			// Indestructible by default; [damage_items] opts a wheel into damage.
			i.second.CDamagableHealthItem::Init(0.f,2);
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
			// Indestructible by default; [damage_items] opts a door into damage.
			i.second.CDamagableHealthItem::Init(0.f,1);
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

void CCar::SyncServerRuntimeState()
{
	if (!IsGameTypeSingle() || Level().Server == nullptr)
		return;

	if (CSE_Abstract* DC = Level().Server->ID_to_entity(ID()))
	{
		if (CSE_ALifeCar* Secar = smart_cast<CSE_ALifeCar*>(DC))
		{
			Secar->Engine = b_engine_on ? 1 : 0;
			Secar->Light = m_lights.IsOn() ? 1 : 0;
			Secar->Exploded = b_exploded ? 1 : 0;
			const bool ExplosionPending = m_zero_health_explosion_timer && CDelayedActionFuse::isActive();
			Secar->ExplosionPending = ExplosionPending ? 1 : 0;
			Secar->ExplosionTime = ExplosionPending ? CDelayedActionFuse::Time() : 0.f;
		}
	}
}

void CCar::StartEngine()
{
	if (GetfHealth() <= 0.f)
	{
		// A destroyed car cannot be started; the engine stays off even if
		// the script or the engine key asks for it.
		b_engine_on = false;
		b_starting = false;
		return;
	}

	if (b_engine_on)
		return;

	if (b_starting)
	{
		// A retry is allowed only after the current starter sequence has
		// finished. Otherwise a stale b_starting left by a failed start would
		// block every further attempt.
		if (Device.dwTimeGlobal - m_dwStartTime <= m_car_sound->engine_start_delay + StarterWatchdogMargin)
			return;

		b_starting = false;
	}

	const bool HasFuel = m_fuel >= EPS;
	bool StartSuccessful = HasFuel;
	if (StartSuccessful && OwnerActor())
	{
		const float StartChance = EngineStartSuccessChance();
		StartSuccessful = StartChance > 0.f && Random.randF(0.f, 1.f) < StartChance;
	}

	PlayExhausts();
	m_car_sound->Start();

	b_engine_on = StartSuccessful;
	m_current_rpm = 0.f;
	m_current_engine_power = 0.f;
	m_current_rpm_brp_n = 1.f;
	Starter();
	SyncServerRuntimeState();
	SwitchEngineDoors();
}

void CCar::RestoreEngineAndLights(bool EngineOn, bool LightsOn)
{
	// A restored runtime state supersedes any live start attempt.
	b_starting = false;

	if (EngineOn && GetfHealth() > 0.f)
	{
		// Resume a running engine without replaying the starter sequence.
		if (!b_engine_on)
		{
			m_current_rpm = m_min_rpm;
			m_current_engine_power = 0.f;
			m_current_rpm_brp_n = 1.f;
		}

		// The sound scales belong to the live engine state. A car restored
		// from a save must start from the idle scale instead of inheriting a
		// value from an earlier spawn of the same object or from a state that
		// arrived before the client chunk.
		m_engine_drive = 0.f;
		m_engine_state_scale = 0.f;
		m_engine_state_scale_last = 0.f;
		m_idle_state_scale = 0.f;
		m_idle_state_scale_last = 0.f;
		m_engine_rpm_scale = 0.f;
		m_engine_idle_volume_scale = 1.f;

		b_engine_on = true;
		b_starting = false;

		// Always (re)enter the drive sound state. SCarSound::Init() resets
		// eCarSound to sndOff when the car is spawned, so the reset can happen
		// after this restore depending on the spawn/serialization order.
		if (m_car_sound != nullptr)
			m_car_sound->Drive();

		// The sound and particle subsystems can still be suspended while the
		// level is loading. Start the exhausts on the first game frames after
		// net_Spawn instead of dropping the Play() calls here.
		m_restore_engine_effects_attempts = 30;
	}
	else if (b_engine_on)
	{
		StopEngine();
	}

	if (LightsOn != m_lights.IsOn())
		m_lights.SwitchHeadLights();

	SyncServerRuntimeState();

	// The client state chunk can arrive after net_Spawn; make the engine-door
	// pose match the final engine state even in that case.
	for (SEngineDoor& Door : engine_door)
		Door.ApplyEngineDoorState(b_engine_on);
}

float CCar::EngineStartSuccessChance() const
{
	float StartChance = m_start_probability;
	clamp(StartChance, 0.f, 1.f);

	if (m_levels_num == 0 || m_max_health <= EPS)
		return StartChance;

	const auto LevelChance = [this](u16 Level)
	{
		return DamageLevelToHealth(Level) / m_max_health;
	};

	// The big smoke starts at the lower health threshold, so it must take
	// priority when both damage particle stages are active. The configured
	// start probability is multiplied by the damage-based chance.
	if (m_damage_particles.IsPlaying2())
	{
		float Chance = LevelChance(2);
		clamp(Chance, 0.f, 1.f);
		return StartChance * Chance;
	}
	if (m_damage_particles.IsPlaying1())
	{
		float Chance = LevelChance(1);
		clamp(Chance, 0.f, 1.f);
		return StartChance * Chance;
	}

	return StartChance;
}
void CCar::StopEngine()
{
	b_starting = false;
	if(!b_engine_on) return;
	AscCall(ascSndStall);
	AscCall(ascExhoustStop);
	NeutralDrive();//set zero speed
	b_engine_on=false;
	UpdatePower();//set engine friction;
	m_current_rpm=0.f;
	SyncServerRuntimeState();
	SwitchEngineDoors();
}

void CCar::Stall()
{
	b_starting = false;
	AscCall(ascSndStall);
	AscCall(ascExhoustStop);
	NeutralDrive();//set zero speed
	b_engine_on=false;
	UpdatePower();//set engine friction;
	m_current_rpm=0.f;
	SyncServerRuntimeState();
	SwitchEngineDoors();
}

void CCar::SwitchEngine()
{
	if(b_engine_on) StopEngine();
	else			StartEngine();
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
				// A car rolling backwards in a forward gear must not drive the
				// automatic transmission as if it were moving forward.
				const float SignedSpeed = GetVelocity().dotproduct(XFORM().k);
				const bool RollingBackward = SignedSpeed < -0.1f;

				if (rs_car_auto_transmission && !b_auto_transmission_stop && mb_traction && !m_gear_ratious.empty() && !IsTransmissionSwitching() && !RollingBackward)
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
#if defined(DEBUG) || defined(DEBUG_DRAW)
	if (!rs_car_anti_gravity)
		return;
#endif

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
		m_doors.insert(std::make_pair(I->second.GetClosedWorldPosition().distance_to(center), &I->second));
	}
	if (m_doors.empty())
		return nullptr;

	return std::min_element(m_doors.begin(), m_doors.end())->second;
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
				for (auto I = RQR.results.begin(); I != RQR.results.end(); ++I)
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

				if (LastDoor->state != CCar::SDoor::opened || !LastDoor->IsFullyOpen())
				{
					LastDoor->Reopen();
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
	if(b_starting && b_engine_on)
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
	StabilizeWheelAxes();

	const bool Slip = UpdateWheelPhysics();
	UpdateTraction(Slip);
	UpdateWaterDrag(step);
	UpdateReturnBones(step);

	// A car that is already almost at rest gets a small velocity decay. The
	// hinge-based suspension can keep a light car oscillating around its rest
	// position under a character's weight; damping that residual motion lets
	// the shell settle and disable normally. This is only needed while the
	// anti-gravity handling aid is active: with full weight the car settles
	// by itself.
	bool ApplyRestDamping = true;
#if defined(DEBUG) || defined(DEBUG_DRAW)
	ApplyRestDamping = rs_car_anti_gravity;
#endif

	if (ApplyRestDamping && m_pPhysicsShell && m_pPhysicsShell->isEnabled()
		&& GetVelocityM() < 0.2f && GetAVelocityM() < 0.2f)
	{
		if (CPhysicsElement* Root = m_pPhysicsShell->get_ElementByStoreOrder(0))
		{
			Fvector Vel;
			Root->get_LinearVel(Vel);
			Vel.y *= 0.9f;
			Root->set_LinearVel(Vel);

			Fvector AVel;
			Root->get_AngularVel(AVel);
			AVel.mul(0.9f);
			Root->set_AngularVel(AVel);
		}
	}
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
		if (Owner() != nullptr && !characters_scraper_shell->isEnabled())
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

	// Enabling an element calls CPHShell::EnableObject(), which wakes the
	// whole car. Only do it when the body is actually disabled, otherwise a
	// parked car with a character on it never gets a chance to settle.
	if (m_pPhysicsShell && m_pPhysicsShell->isEnabled())
	{
		for (auto& it : engine_door)
		{
			if (!it.joint || !it.joint->bActive) continue;

			float lo_limit, hi_limit;
			it.joint->GetLimits(lo_limit, hi_limit, 0);
			const float Angle = it.joint->GetAxisAngle(0);
			if (Angle > lo_limit && Angle < hi_limit)
			{
				CPhysicsElement* element = it.joint->PSecond_element();
				if (element && element->isActive() && !dBodyIsEnabled(element->get_body()))
					element->Enable();
			}
		}

		for (auto& it : headlight_hideaways)
		{
			if (!it.joint || !it.joint->bActive) continue;

			float lo_limit, hi_limit;
			it.joint->GetLimits(lo_limit, hi_limit, 0);
			const float Angle = it.joint->GetAxisAngle(0);
			if (Angle > lo_limit && Angle < hi_limit)
			{
				CPhysicsElement* element = it.joint->PSecond_element();
				if (element && element->isActive() && !dBodyIsEnabled(element->get_body()))
					element->Enable();
			}
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
		break_start_dt += step;
	else
		break_start_dt = 0.f;
	clamp(break_start_dt, 0.0f, 1.0f);
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

void CCar::UpdateReturnBones(float step)
{
	if (!m_pPhysicsShell || !m_pPhysicsShell->isActive() || step <= 0.f)
		return;

	if (return_bones.empty())
		return;

	// setTorque() overwrites the body torque and ODE keeps it until the next
	// setTorque() call. Always apply a value for every return element, even
	// zero, so torque from the previous step is not left running.
	xr_map<CPhysicsElement*, Fvector> Torques;

	for (SReturnBone& ReturnBone : return_bones)
	{
		CPhysicsJoint* Joint = ReturnBone.joint;
		CPhysicsElement* Element = ReturnBone.element;
		if (Element == nullptr)
			continue;

		auto It = Torques.find(Element);
		if (It == Torques.end())
		{
			Fvector Zero;
			Zero.set(0.f, 0.f, 0.f);
			It = Torques.insert(std::make_pair(Element, Zero)).first;
		}
		Fvector& Torque = It->second;

		if (Joint == nullptr || !Joint->bActive || !Element->isActive())
		{
			ReturnBone.has_previous_angle = false;
			continue;
		}

		// Hinge2 exposes the wheel spin rate instead of a second angle, sliders
		// return a linear position and balls report no angle at all.
		if (Joint->eType != CPhysicsJoint::hinge && Joint->eType != CPhysicsJoint::full_control)
			continue;

		// full_control always drives three AMotor axes even if the IK data
		// contains more limit entries; hinge has a single angular axis.
		const u16 AxisCount = Joint->eType == CPhysicsJoint::full_control ? 3 : 1;
		bool AllAnglesValid = true;
		for (u16 AxisNum = 0; AxisNum < AxisCount; ++AxisNum)
		{
			const float Angle = angle_normalize_signed(Joint->GetAxisAngle(AxisNum));
			if (!_valid(Angle))
			{
				AllAnglesValid = false;
				continue;
			}

			// dJointGetAMotorAngleRate is not implemented in ODE, so derive
			// the rate from the previous physics step instead.
			float Rate = 0.f;
			if (ReturnBone.has_previous_angle)
				Rate = angle_normalize_signed(Angle - ReturnBone.previous_angle[AxisNum]) / step;

			ReturnBone.previous_angle[AxisNum] = Angle;

			if (fabsf(Angle) < ReturnBoneAngleEpsilon && fabsf(Rate) < ReturnBoneAngleEpsilon)
				continue;

			Fvector Axis;
			Joint->GetAxisDirDynamic(AxisNum, Axis);
			// ODE measures hinge and AMotor angles opposite to the second
			// element rotation, so a positive torque about the axis pulls the
			// angle back to zero.
			Torque.mad(Axis, Angle * ReturnBone.force + Rate * ReturnBone.damping);
		}

		ReturnBone.has_previous_angle = AllAnglesValid;
	}

	// setTorqueNoWake() only writes the torque accumulator. Calling
	// CPHElement::setTorque() here would call EnableObject() every step, which
	// keeps the whole car awake and prevents it from going to disable.
	for (const auto& Pair : Torques)
	{
		if (Pair.first != nullptr)
		{
			Pair.first->setTorqueNoWake(Pair.second);
		}
	}
}

void CCar::UpdateSteeringWheels()
{
	for (auto& Wheel : m_steering_wheels)
	{
		Wheel.SyncJointParams();
	}

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
		const float SteerAngle = m_steering_wheels.begin()->GetSteerAngle();
		m_steer_angle = m_steer_angle + (SteerAngle - m_steer_angle) * 15.f * fixed_step;
		m_wheel_angle = SteerAngle;
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

void CCar::StabilizeWheelAxes()
{
	for (auto& WheelEntry : m_wheels_map)
	{
		SWheel& Wheel = WheelEntry.second;
		if (!Wheel.inited || !Wheel.joint) continue;

		dBodyID Body = Wheel.joint->PSecond_element()->get_body();
		if (!Body) continue;

		dJointID Joint = Wheel.joint->GetDJoint();
		if (!Joint || dJointGetType(Joint) != dJointTypeHinge2) continue;

		dVector3 Axis1, Axis2;
		dJointGetHinge2Axis1(Joint, Axis1);
		dJointGetHinge2Axis2(Joint, Axis2);

		const dReal Dot = Axis1[0] * Axis2[0] + Axis1[1] * Axis2[1] + Axis1[2] * Axis2[2];
		if (dFabs(Dot) < 1e-4f) continue;

		dVector3 Axis;
		dCROSS(Axis, =, Axis1, Axis2);
		if (dDOT(Axis, Axis) < 1e-12f) continue;
		dNormalize3(Axis);

		dReal DotClamped = Dot;
		clamp(DotClamped, -1.f, 1.f);
		dReal Angle = asinf(DotClamped);
		clamp(Angle, -0.2f, 0.2f);

		// The hinge row works at velocity level, so a fast-spinning wheel at
		// 50 Hz still ends the step with a small axle tilt. Project the spin
		// axis back onto the plane perpendicular to the steering axis before
		// the next step and before the HUD reads the joint. The correction is
		// around Axis1 x Axis2 and does not change the steering angle.
		dQuaternion Correction, Result;
		dQFromAxisAndAngle(Correction, Axis[0], Axis[1], Axis[2], Angle);
		dQMultiply0(Result, Correction, dBodyGetQuaternion(Body));
		dBodySetQuaternion(Body, Result);
	}
}

void CCar::UpdateSteerAnimation()
{
	CActor* Driver = OwnerActor();
	if (Driver == nullptr)
		return;

	// Use the effective steering state, not the raw key events: this keeps the
	// animation in sync when both steering keys are pressed or one is released.
	// The idle transition is immediate, as in the original input-driven code.
	s8 NewState = 0;
	if (e_state_steer == right)
		NewState = 1;
	else if (e_state_steer == left)
		NewState = -1;

	if (NewState == m_steer_anim_state)
		return;

	m_steer_anim_state = NewState;
	Driver->steer_Vehicle(float(NewState));
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

void CCar::StoreAllElementSpawnLocalTransforms()
{
	if (m_pPhysicsShell == nullptr)
	{
		m_element_spawn_local.clear();
		return;
	}

	Fmatrix RootForm;
	m_pPhysicsShell->GetGlobalTransformDynamic(&RootForm);

	Fmatrix InvRoot;
	InvRoot.invert(RootForm);

	const u16 Count = m_pPhysicsShell->get_ElementsNumber();
	m_element_spawn_local.resize(Count);

	for (u16 i = 0; i < Count; ++i)
	{
		SElementSpawnLocalForm& Saved = m_element_spawn_local[i];
		Saved.valid = false;

		CPhysicsElement* Element = m_pPhysicsShell->get_ElementByStoreOrder(i);
		if (Element == nullptr || !Element->isActive())
			continue;

		Fmatrix BodyForm;
		Element->GetGlobalTransformDynamic(&BodyForm);
		Saved.form.mul_43(InvRoot, BodyForm);
		Saved.valid = true;
	}
}

bool CCar::RestoreAllElementSpawnLocalTransforms(const Fmatrix& RestoredForm, bool TrustSavedWheels)
{
	if (m_pPhysicsShell == nullptr)
		return false;

	const u16 Count = m_pPhysicsShell->get_ElementsNumber();
	const u16 SavedCount = (u16)m_element_spawn_local.size();

	// Doors must keep the element transform written to the save: an open door
	// should load open, not snap back to its spawn pose. The cab and other
	// parts are still normalised to the spawn pose below.
	auto IsDoorElement = [this](const CPhysicsElement* Element)
	{
		for (const auto& Door : m_doors)
		{
			if (Door.second.joint != nullptr && Door.second.joint->PSecond_element() == Element)
				return true;
		}
		for (const SEngineDoor& Door : engine_door)
		{
			if (Door.joint != nullptr && Door.joint->PSecond_element() == Element)
				return true;
		}
		return false;
	};

	// Wheels are sprung bodies: the saved element transform contains the
	// suspension compression and the steering angle. Resetting them to the
	// spawn pose would raise the chassis when the saved car is restored.
	auto IsWheelElement = [this](const CPhysicsElement* Element)
	{
		for (const auto& Wheel : m_wheels_map)
		{
			const auto Bone = bone_map.find(Wheel.second.bone_id);
			if (Bone != bone_map.end() && Bone->second.element == Element)
				return true;
		}
		return false;
	};

	Fmatrix InvRestoredForm;
	InvRestoredForm.invert(RestoredForm);

	bool HasActiveWheel = false;
	bool AllWheelsTrusted = true;

	for (u16 i = 0; i < Count && i < SavedCount; ++i)
	{
		CPhysicsElement* Element = m_pPhysicsShell->get_ElementByStoreOrder(i);
		if (Element == nullptr || !Element->isActive())
			continue;

		const bool WheelElement = IsWheelElement(Element);
		if (WheelElement)
			HasActiveWheel = true;

		const SElementSpawnLocalForm& Saved = m_element_spawn_local[i];
		if (!Saved.valid)
		{
			if (WheelElement)
				AllWheelsTrusted = false;
			continue;
		}

		if (IsDoorElement(Element))
			continue;

		if (WheelElement)
		{
			if (TrustSavedWheels)
			{
				Fmatrix BodyForm;
				Element->GetGlobalTransformDynamic(&BodyForm);

				Fmatrix Local;
				Local.mul_43(InvRestoredForm, BodyForm);

				const bool Valid =
					_valid(Local.c) && _valid(Local.i) && _valid(Local.j) && _valid(Local.k) &&
					Local.c.distance_to(Saved.form.c) <= WheelLoadMaxSpawnDrift;

				if (Valid)
				{
					Element->ResetInterpolation();
					Element->set_LinearVel(zero_vel);
					Element->set_AngularVel(zero_vel);
					continue;
				}
			}

			AllWheelsTrusted = false;
		}

		Fmatrix Target;
		Target.mul_43(RestoredForm, Saved.form);

		Element->SetTransform(Target, mh_clear);
		Element->ResetInterpolation();
		Element->set_LinearVel(zero_vel);
		Element->set_AngularVel(zero_vel);
	}

	return HasActiveWheel && AllWheelsTrusted;
}

void CCar::ResolveLoadPenetration()
{
	if (m_pPhysicsShell == nullptr)
		return;

	Fbox LoadBounds = BoundingBox();

	// Build the load box from active physical elements. The visual BoundingBox
	// also includes deactivated doors and can extend below the real wheels.
	Fbox PhysicalBounds;
	PhysicalBounds.invalidate();
	{
		Fmatrix InvCar = XFORM();
		InvCar.invert();

		const u16 Count = m_pPhysicsShell->get_ElementsNumber();
		for (u16 i = 0; i < Count; ++i)
		{
			CPhysicsElement* Element = m_pPhysicsShell->get_ElementByStoreOrder(i);
			if (Element == nullptr || !Element->isActive() || Element->get_body() == nullptr)
				continue;

			Fvector ElementSize;
			Fvector ElementCenter;
			Element->get_Box(ElementSize, ElementCenter);

			Fobb ElementBox;
			ElementBox.m_rotate.set(Element->mXFORM);
			ElementBox.m_translate.set(ElementCenter);
			ElementBox.m_halfsize.mul(ElementSize, 0.5f);

			for (int ix = 0; ix < 2; ++ix)
			{
				for (int iy = 0; iy < 2; ++iy)
				{
					for (int iz = 0; iz < 2; ++iz)
					{
						Fvector Corner = ElementBox.m_translate;
						Corner.mad(ElementBox.m_rotate.i, ix ? ElementBox.m_halfsize.x : -ElementBox.m_halfsize.x);
						Corner.mad(ElementBox.m_rotate.j, iy ? ElementBox.m_halfsize.y : -ElementBox.m_halfsize.y);
						Corner.mad(ElementBox.m_rotate.k, iz ? ElementBox.m_halfsize.z : -ElementBox.m_halfsize.z);
						InvCar.transform_tiny(Corner);
						PhysicalBounds.modify(Corner);
					}
				}
			}
		}
	}

	if (PhysicalBounds.is_valid())
		LoadBounds = PhysicalBounds;
	if (!LoadBounds.is_valid())
		return;

	Fvector LocalCenter;
	Fvector LocalHalf;
	LoadBounds.get_CD(LocalCenter, LocalHalf);

	Fvector Size;
	Size.mul(LocalHalf, 2.f);

	Fvector Center;
	XFORM().transform_tiny(Center, LocalCenter);

	const bool MainCollision = !m_pPhysicsShell->IsCollisionDisabled();
	m_pPhysicsShell->DisableCollision();

	bool ScraperCollision = false;
	if (characters_scraper_shell != nullptr)
	{
		ScraperCollision = !characters_scraper_shell->IsCollisionDisabled();
		characters_scraper_shell->DisableCollision();
	}

	Fvector OutPos;
	ActivateShapePhysShellHolder(this, XFORM(), Size, Center, OutPos);

	if (MainCollision)
		m_pPhysicsShell->EnableCollision();
	if (ScraperCollision && characters_scraper_shell != nullptr)
		characters_scraper_shell->EnableCollision();

	Fvector Delta;
	Delta.sub(OutPos, Center);
	// Never drop the car: if the activation shape fell while the car was
	// loaded in the air, the saved pose is still the correct one.
	if (Delta.y < 0.f)
		Delta.y = 0.f;
	if (Delta.magnitude() < 0.001f)
		return;

	Fmatrix Shift;
	Shift.identity();
	Shift.c.set(Delta);
	m_pPhysicsShell->TransformPosition(Shift, mh_clear);

	Fmatrix RestoredForm;
	m_pPhysicsShell->GetGlobalTransformDynamic(&RestoredForm);
	XFORM().set(RestoredForm);
}

void CCar::RestoreServerPose(CSE_ALifeCar* Car)
{
	if (Car == nullptr || m_pPhysicsShell == nullptr)
		return;

	Fmatrix RestoredForm;
	m_pPhysicsShell->GetGlobalTransformDynamic(&RestoredForm);

	Fmatrix ServerForm;
	ServerForm.setXYZ(Car->o_Angle.x, Car->o_Angle.y, Car->o_Angle.z);
	ServerForm.c.set(Car->o_Position);

	// Normal loads keep the saved physics pose: the server position can lag
	// one frame behind. A level transition moves the holder server object to
	// the destination, which is far away from the old-level pose.
	constexpr float MaxPoseDistance = 3.f;
	constexpr float MinPoseDirection = 0.95f;
	if (RestoredForm.c.distance_to(ServerForm.c) <= MaxPoseDistance &&
		RestoredForm.k.dotproduct(ServerForm.k) >= MinPoseDirection)
	{
		return;
	}

	Fmatrix InvRestored;
	InvRestored.invert(RestoredForm);

	// get_box() walks every shell element, including deactivated doors whose
	// geometry may already be gone. Build the box from active elements only.
	Fbox ActiveBounds;
	ActiveBounds.invalidate();
	const u16 ElementCount = m_pPhysicsShell->get_ElementsNumber();
	for (u16 i = 0; i < ElementCount; ++i)
	{
		CPhysicsElement* Element = m_pPhysicsShell->get_ElementByStoreOrder(i);
		if (Element == nullptr || !Element->isActive() || Element->get_body() == nullptr)
			continue;

		Fvector ElementSize;
		Fvector ElementCenter;
		Element->get_Box(ElementSize, ElementCenter);

		Fobb ElementBox;
		ElementBox.m_rotate.set(Element->mXFORM);
		ElementBox.m_translate.set(ElementCenter);
		ElementBox.m_halfsize.mul(ElementSize, 0.5f);

		for (int ix = 0; ix < 2; ++ix)
		{
			for (int iy = 0; iy < 2; ++iy)
			{
				for (int iz = 0; iz < 2; ++iz)
				{
					Fvector Corner = ElementBox.m_translate;
					Corner.mad(ElementBox.m_rotate.i, ix ? ElementBox.m_halfsize.x : -ElementBox.m_halfsize.x);
					Corner.mad(ElementBox.m_rotate.j, iy ? ElementBox.m_halfsize.y : -ElementBox.m_halfsize.y);
					Corner.mad(ElementBox.m_rotate.k, iz ? ElementBox.m_halfsize.z : -ElementBox.m_halfsize.z);
					InvRestored.transform_tiny(Corner);
					ActiveBounds.modify(Corner);
				}
			}
		}
	}

	if (!ActiveBounds.is_valid())
		return;

	Fvector LocalCenter;
	Fvector LocalHalf;
	ActiveBounds.get_CD(LocalCenter, LocalHalf);

	Fvector Size;
	Size.mul(LocalHalf, 2.f);

	Fvector Center;
	RestoredForm.transform_tiny(Center, LocalCenter);

	Fmatrix ToServer;
	ToServer.mul(ServerForm, InvRestored);
	ToServer.transform_tiny(Center);

	const bool MainCollision = !m_pPhysicsShell->IsCollisionDisabled();
	m_pPhysicsShell->DisableCollision();

	bool ScraperCollision = false;
	if (characters_scraper_shell != nullptr)
	{
		ScraperCollision = !characters_scraper_shell->IsCollisionDisabled();
		characters_scraper_shell->DisableCollision();
	}

	Fvector ResolvedCenter;
	ActivateShapePhysShellHolder(this, ServerForm, Size, Center, ResolvedCenter);

	if (MainCollision)
		m_pPhysicsShell->EnableCollision();
	if (ScraperCollision && characters_scraper_shell != nullptr)
		characters_scraper_shell->EnableCollision();

	Fvector Delta;
	Delta.sub(ResolvedCenter, Center);
	ServerForm.c.add(Delta);

	ToServer.mul(ServerForm, InvRestored);
	m_pPhysicsShell->TransformPosition(ToServer, mh_clear);
	m_pPhysicsShell->GetGlobalTransformDynamic(&RestoredForm);
	XFORM().set(RestoredForm);
}

void CCar::UpdateWheelSuspension(SWheel& Wheel)
{
	float spring_ = Wheel.spring_factor_default;
	float damping_ = Wheel.damping_factor_default;
	const bool AutoDamping = Wheel.suspension_damping_factor < 0.f;

	if (Wheel.suspension_force_inc > 0.f)
	{
		const Fmatrix& BoneBindForm = bones_bind_forms[Wheel.bone_id];
		const Fmatrix& BoneForm = PKinematics(Visual())->LL_GetTransform(Wheel.bone_id);
		const float Distance = BoneBindForm.c.distance_to(BoneForm.c);

		float median = (spring_ + damping_) * 0.5f;

		if (Distance > Wheel.suspension_distance)
		{
			float spring_req = spring_ + (Distance - Wheel.suspension_distance) * (spring_ / median) * Wheel.suspension_spring_factor * Wheel.suspension_force_inc;
			float damping_req = damping_;
			if (!AutoDamping)
				damping_req = damping_ + (Distance - Wheel.suspension_distance) * (damping_ / median) * Wheel.suspension_damping_factor * Wheel.suspension_force_inc;

			if (Wheel.spring_last < spring_req)
			{
				spring_ = Wheel.suspension_inertia * Wheel.spring_last + (1.f - Wheel.suspension_inertia) * spring_req;
				if (!AutoDamping)
					damping_ = Wheel.suspension_inertia * Wheel.damping_last + (1.f - Wheel.suspension_inertia) * damping_req;
			}
			else
			{
				spring_ = spring_req;
				if (!AutoDamping)
					damping_ = damping_req;
			}

			if (AutoDamping && Wheel.spring_factor_default > EPS_L && spring_ > EPS_L)
			{
				// Keep the base damping ratio: zeta ~ k_d / sqrt(k_p), so
				// damping follows sqrt(spring), not spring.
				damping_ = Wheel.damping_factor_default * sqrtf(spring_ / Wheel.spring_factor_default);
			}
		}
		Wheel.spring_last = spring_;
		Wheel.damping_last = damping_;
	}
	else
	{
		Wheel.spring_last = Wheel.spring_factor_default;
		Wheel.damping_last = Wheel.damping_factor_default;
	}

	float SpringDampingScale = 1.f;
#if defined(DEBUG) || defined(DEBUG_DRAW)
	if (!rs_car_anti_gravity)
	{
		// With anti-gravity disabled the car is much heavier. Stiffen the
		// suspension in the same proportion to keep the wheels off the body.
		SpringDampingScale = 2.f;
	}
#endif

	if (Wheel.suspension_force_inc > 0.f)
	{
		Wheel.joint->SetJointSDfactors(spring_ * SpringDampingScale, damping_ * SpringDampingScale);
		Wheel.spring_damping_scale = SpringDampingScale;
	}
	else if (Wheel.spring_damping_scale != SpringDampingScale)
	{
		// Scale the current joint values instead of resetting them to the OGF
		// defaults, so wheel damage and other runtime joint changes survive
		// toggling the debug switch.
		float SpringFactor = 0.f;
		float DampingFactor = 0.f;
		Wheel.joint->GetJointSDfactors(SpringFactor, DampingFactor);

		const float Ratio = SpringDampingScale / Wheel.spring_damping_scale;
		Wheel.joint->SetJointSDfactors(SpringFactor * Ratio, DampingFactor * Ratio);
		Wheel.spring_damping_scale = SpringDampingScale;
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

			const float filter_alpha = 1.f - expf(-fixed_step / WheelSlipFilterTime);
			Wheel.collision_params.wheel_speed_filtered += (wheel_speed - Wheel.collision_params.wheel_speed_filtered) * filter_alpha;
			const float filtered_wheel_speed = Wheel.collision_params.wheel_speed_filtered;

			const bool DriveWheel = Wheel.WheelFlags.test(SWheel::eWheelType::wheel_drive);
			const bool SpeedOverThreshold = filtered_wheel_speed > threshold;
			const bool LinearUnderSpin = wheel_lin_speed < filtered_wheel_speed * wheels_spin_factor;
			const bool DriveStateOk = e_state_drive == drive && DriveWheel;
			const bool SlipCondition = SpeedOverThreshold && LinearUnderSpin && DriveStateOk;

			if (SlipCondition) //_abs(Wheel.GetDriveAngleRate()) * Wheel.radius < GetVelocityM() * 0.6f
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
			Wheel.collision_params.wheel_speed_filtered = 0.f;
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
	{
		Wheel.collision_params.traction = false;
		Wheel.collision_params.wheel_speed_filtered = 0.f;
		// Do not keep stale slip state after the wheel leaves the ground: it
		// would retrigger skid sounds on landing.
		Wheel.collision_params.buxe = false;
		Wheel.collision_params.breaking = false;
		Wheel.collision_params.breaking_abs = false;
		Wheel.collision_params.slide = false;
	}
}

void CCar::UpdateWheelAirborneFriction(SWheel& Wheel)
{
	const bool Airborne = !Wheel.collision_params.traction;
	const bool DrivenWheelWithThrottle =
		e_state_drive == drive &&
		b_engine_on &&
		b_clutch &&
		Wheel.WheelFlags.test(SWheel::eWheelType::wheel_drive);

	// An airborne wheel has no contact friction. Apply the axle friction
	// explicitly so engine torque cannot spin it forever while it hangs
	// in the air. Braking already supplies a stronger wheel torque.
	// A driven wheel under throttle must keep the engine drive torque,
	// otherwise the wheels cannot spin in the air.
	if (Airborne && !b_breaks && !DrivenWheelWithThrottle)
	{
		Wheel.Neutral();
		Wheel.AirborneFriction = true;
	}
	else if (Airborne && DrivenWheelWithThrottle && !b_breaks)
	{
		// The wheel is driven again, so there is no neutral state to restore.
		Wheel.AirborneFriction = false;
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

void CCar::UpdateTraction(bool slip)
{
	if (mb_traction && Device.dwTimeGlobal - m_last_contact_time > 50)
		mb_traction = false;
	if (!slip)
		m_slip_start_time = Device.fTimeGlobal;

	m_last_velocity = GetVelocityM();
}

void CCar::UpdateWaterDrag(float step)
{
	WheelsInWater = false;
	m_water_material_idx = GAMEMTL_NONE_IDX;
	m_water_y = -flt_max;

	if (!m_pPhysicsShell || step <= 0.f || m_wheels_map.empty())
		return;

	const Fvector CarVelocity = GetVelocity();
	const float Speed = CarVelocity.magnitude();

	const float Mass = m_pPhysicsShell->getMass();
	if (Mass <= 0.f)
		return;

	if (Speed < EPS_L)
	{
		// A car can be stationary in deep water while the wheels are still
		// spinning. Keep the water probe and the surface splash active for
		// that case; drag itself will be zero below.
		bool WheelsSpinning = false;
		for (auto& WheelEntry : m_wheels_map)
		{
			SWheel& Wheel = WheelEntry.second;
			if (!Wheel.joint || !Wheel.joint->bActive)
				continue;

			const float Radius = Wheel.radius > 0.f ? Wheel.radius : 0.3f;
			if (std::abs(Wheel.GetDriveAngleRate()) * Radius > WaterParticleMinSpeed)
			{
				WheelsSpinning = true;
				break;
			}
		}

		// A car standing still in water still has to update the water material:
		// the actor condition reads it for injurious water while in the car.
		if (!WheelsSpinning && !OwnerActor())
			return;
	}

	collide::rq_results& Results = m_water_ray_results;
	const Fvector Down = { 0.f, -1.f, 0.f };
	float DragFactorSum = 0.f;
	float Flotation = 1.f;
	IKinematics* Kinematics = PKinematics(Visual());

	for (auto& WheelEntry : m_wheels_map)
	{
		SWheel& Wheel = WheelEntry.second;
		Wheel.InWater = false;
		if (!Wheel.joint || !Wheel.joint->bActive)
			continue;

		CPhysicsElement* Element = Wheel.joint->PSecond_element();
		if (!Element || !Element->isActive())
			continue;

		const Fvector Center = Element->XFORM().c;
		const float Radius = Wheel.radius > 0.f ? Wheel.radius : Element->getRadius();
		if (Radius <= 0.f)
			continue;

		// Find the first liquid triangle above the wheel. The probe starts
		// several metres above it so deep water is detected as well, and then
		// scans the hits instead of taking the nearest solid triangle (a bridge,
		// a rock or the bottom) for the water surface.
		const float ProbeUp = Radius + 4.f;
		const Fvector From(Center.x, Center.y + ProbeUp, Center.z);
		const float Range = ProbeUp + Radius + 0.5f;

		collide::ray_defs Ray(From, Down, Range, 0, collide::rqtStatic);
		Level().ObjectSpace.RayQuery(Results, Ray, nullptr, nullptr, nullptr, this);

		SGameMtl* WaterMaterial = nullptr;
		u16 WaterMaterialIdx = u16(-1);
		float WheelWaterY = -flt_max;
		for (auto It = Results.results.begin(); It != Results.results.end(); ++It)
		{
			if (!It->IsStatic() || It->element < 0)
				continue;

			const CDB::TRI& Tri = It->GetStatic()->tris[It->element];
			SGameMtl* Material = GMLib.GetMaterialByIdx(Tri.material);
			if (!Material || !Material->Flags.test(SGameMtl::flLiquid))
				continue;

			WheelWaterY = From.y - It->range;
			WaterMaterial = Material;
			WaterMaterialIdx = Tri.material;
			break;
		}

		if (!WaterMaterial)
			continue;

		const float Bottom = Center.y - Radius;
		float Immersion = (WheelWaterY - Bottom) / (2.f * Radius);
		if (Immersion < 0.f)
			continue;
		if (Immersion > 1.f)
			Immersion = 1.f;

		Wheel.InWater = true;

		if (m_water_material_idx == GAMEMTL_NONE_IDX)
		{
			m_water_material_idx = WaterMaterialIdx;
			m_water_y = WheelWaterY;
		}

		if (Speed > EPS_L)
			WheelsInWater = true;

		// A physics contact with the water surface only exists while a wheel
		// crosses it, so a car driving in water would otherwise splash only
		// on entry. Keep spawning the wheel/water collision effect while the
		// wheel is submerged and either the car or the wheel is moving.
		const float WheelSurfaceSpeed = std::abs(Wheel.GetDriveAngleRate()) * Radius;
		if ((Speed > WaterParticleMinSpeed || WheelSurfaceSpeed > WaterParticleMinSpeed)
			&& Immersion > WaterParticleMinImmersion
			&& Device.dwTimeGlobal >= Wheel.WaterParticleTime)
		{
			Wheel.WaterParticleTime = Device.dwTimeGlobal + WaterParticleInterval + ::Random.randI(0, WaterParticleIntervalJitter);

			const float DistanceSqr = Device.vCameraPosition.distance_to_sqr(Center);
			if (Kinematics && DistanceSqr < _sqr(WaterParticleMaxDistance))
			{
				const u16 WheelMaterialIdx = Kinematics->LL_GetData(Wheel.bone_id).game_mtl_idx;
				if (WheelMaterialIdx < GMLib.GetMaterials().size())
				{
					SGameMtlPair* MaterialPair = GMLib.GetMaterialPair(WheelMaterialIdx, WaterMaterialIdx);
					if (MaterialPair && !MaterialPair->CollideParticles.empty())
					{
						Fvector Velocity;
						Element->get_LinearVel(Velocity);

						Fmatrix ParticleXform = Element->XFORM();
						if (Velocity.magnitude() > EPS_L)
							ParticleXform.k = Fvector(Velocity).normalize().invert();

						ParticleXform.c.set(Center.x, WheelWaterY, Center.z);

						const float EffectSpeed = std::max(Speed, WheelSurfaceSpeed);
						const float ParticleScale = 0.7f + 0.3f * std::min(EffectSpeed / 10.f, 1.f);
						ParticleXform.i.mul(ParticleScale);
						ParticleXform.j.mul(ParticleScale);
						ParticleXform.k.mul(ParticleScale);

						const shared_str& ParticleName = MaterialPair->CollideParticles[::Random.randI(0, MaterialPair->CollideParticles.size())];
						Level().ph_commander().add_call(new CPHOnesCondition(), new CCarParticlesPlayCall(ParticleXform, Velocity, *ParticleName));
					}
				}
			}
		}

		// The drag factor follows the submerged part of the wheel: 0 when it
		// only touches the surface, 1 when it is fully submerged.
		DragFactorSum += Immersion;

		Flotation = WaterMaterial->fFlotationFactor;
		if (const SSurfaceParams* Params = FindSurfaceParams(WaterMaterial))
		{
			if (Params->HasFlotation)
				Flotation = Params->Flotation;
		}
	}

	if (DragFactorSum <= 0.f)
		return;

	clamp(Flotation, 0.f, 1.f);
	const float Recip = 1.f - Flotation;
	const float Effect = 10000.f * Recip * Recip * DragFactorSum;
	float Drag = Speed * Effect;
	const float MaxDrag = Mass / step;
	if (Drag > MaxDrag)
		Drag = MaxDrag;

	Fvector Force;
	Force.mul(CarVelocity, -Drag);
	m_pPhysicsShell->applyForce(Force.x, Force.y, Force.z);
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

	m_fuel = 0.f;
	StopExplosionTimer();

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
	SyncServerRuntimeState();
	b_event_drag_activated = false;
	CExplosive::GenExplodeEvent(Position(),Fvector().set(0.f,1.f,0.f));
	if(CPHDestroyable::CanDestroy())
		CPHDestroyable::Destroy(ID(),"physic_destroyable_object");

	m_car_sound->Destroy();
	m_damage_particles.Stop1(this);
	m_damage_particles.Stop2(this);
	
	RemoveGrassSpheres();
}

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

float	CCar::EngineBrakeTorque()
{
	if (m_power_rpm <= EPS || m_max_rpm <= EPS)
		return 0.f;

	const float Rpm = std::max(m_current_rpm, m_min_rpm);
	const float BrakeRpm = std::min(Rpm, m_max_rpm);
	const float BaseRpm = std::min(BrakeRpm, m_power_rpm);
	if (BaseRpm <= EPS)
		return 0.f;

	// Engine braking must not follow the drive-power falloff above
	// max_power_rpm. Grow linearly up to max_engine_rpm, then hold the torque
	// reached there.
	const float BaseTorque = Parabola(BaseRpm) / BaseRpm;
	const float RpmFactor = std::max(1.f, BrakeRpm / m_power_rpm);
	return BaseTorque * RpmFactor;
}

float	CCar:: EngineCurTorque()
{
	if (e_state_drive == neutral && b_engine_on)
		return EngineBrakeTorque();

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
	case ascSndStall:
		// Same race as the exhaust stop: an immediate restart must not be
		// muted by the stop request from the previous engine state. A start
		// attempt that is still running owns the sound state now; stopping it
		// here would move the state out of sndStarting and leave b_starting
		// set forever when the attempt failed.
		if (!b_engine_on && !b_starting)
			m_car_sound->Stop();
		break;
	case ascExhoustStop:
		// A restart can happen before the deferred stop is processed. Do not
		// kill the exhausts that the new start has already launched.
		if (!b_engine_on)
			StopExhausts();
		break;
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

void		CCar::SetExplodeTime				(u32 et)	
{
	CDelayedActionFuse::Initialize(float(et)/1000.f,
		m_zero_health_explosion_timer ? 0.f : CDamagableItem::DamageLevelToHealth(2));
}
u32			CCar::ExplodeTime					()			
{
	if(CDelayedActionFuse::isInitialized())
				return u32(CDelayedActionFuse::Time())*1000;
	else return 0;
}

void CCar::Die(CObject* who)
{
	inherited::Die(who);
	if (m_zero_health_explosion_timer)
	{
		StartExplosionTimer();
	}
	else
	{
		CarExplode();
	}
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

Fvector CCar::GetNearestPoint(const Fvector& Point) const
{
	Fbox Box = BoundingBox();
	if (!Box.is_valid())
		return Position();

	Fmatrix Inv;
	Inv.invert(XFORM());

	Fvector Local;
	Inv.transform_tiny(Local, Point);

	Fvector Closest = Local;
	clamp(Closest.x, Box.min.x, Box.max.x);
	clamp(Closest.y, Box.min.y, Box.max.y);
	clamp(Closest.z, Box.min.z, Box.max.z);

	Fvector World;
	XFORM().transform_tiny(World, Closest);
	return World;
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
