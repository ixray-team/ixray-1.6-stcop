#include "StdAfx.h"
#include "../../PhysicsShellHolder.h"
#include "telekinetic_object.h"
#include "../../../xrPhysics/PhysicsShell.h"
#include "../../../xrPhysics/MathUtils.h"
#include "WeaponMagazined.h"
#include "Grenade.h"
#include "HUDManager.h"
#include "WeaponMagazinedWGrenade.h"
#include "../../Level.h"
#include "poltergeist/poltergeist.h"
extern ESingleGameDifficulty g_SingleGameDifficulty; 

STelekineticObject::STelekineticObject(CPhysicsShellHolder* owner, float s, float h, u32 ttk, bool rot)
{
	STelekineticObject::switch_state(ETelekineticState::TS_RAISE);
	object = owner;

	target_height = owner->Position().y + h;

	time_keep_started = 0;
	time_keep_updated = 0;
	time_to_keep = ttk;

	strength = s;
	time_throw_started = 0;
	rotate_object = rot;
}

void STelekineticObject::set_sound(const ref_sound& snd_hold, const ref_sound& snd_throw)
{
	sound_hold.clone(snd_hold, st_Effect, sg_SourceType);
	sound_throw.clone(snd_throw, st_Effect, sg_SourceType);
}

void STelekineticObject::set_particle(shared_str& particles_sect)
{
	particle_sect = particles_sect;
}

void STelekineticObject::start_object_particles()
{
	TParticlesPlayer* PPlayer = object->GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->StartParticles(particle_sect, Fvector().set(0.0f, 0.1f, 0.0f), object->ID());
}

void STelekineticObject::stop_object_particles()
{
	TParticlesPlayer* PPlayer = object->GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->StopParticles(particle_sect, BI_NONE, true);
}

void STelekineticObject::raise_update()
{
	if (check_height() || check_raise_time_out())
		prepare_keep();
	else if (rotate_object)
		rotate();
}

void STelekineticObject::keep_update()
{
	if (keep_time_elapsed())
		release();
}

void STelekineticObject::throw_update()
{
	if (throw_time_elapsed())
		release();
}

void STelekineticObject::update_state()
{
	switch (get_state())
	{
	case ETelekineticState::TS_RAISE:
		raise_update();
		break;

	case ETelekineticState::TS_KEEP:
		keep_update();
		break;

	case ETelekineticState::TS_THROW:
		throw_update();
		break;

	case ETelekineticState::TS_NONE:
		break;
	}
}

void STelekineticObject::switch_state(ETelekineticState new_state)
{
	u32 current_time = time();
	
	switch (new_state)
	{
	case ETelekineticState::TS_RAISE:
		time_raise_started = current_time;
		break;
	case ETelekineticState::TS_KEEP:
		time_keep_started = current_time;
		break;
	case ETelekineticState::TS_THROW:
		time_throw_started = current_time;
		break;
	case ETelekineticState::TS_NONE: break;
	}
	
	state = new_state;
}

void STelekineticObject::raise(float step)
{
	if (!object || !object->m_pPhysicsShell || !object->m_pPhysicsShell->isActive()) 
		return;
	
	step *= strength;

	Fvector dir;
	dir.set(0.f, 1.0f, 0.f);

	float elem_size = float(object->m_pPhysicsShell->Elements().size());
	dir.mul(elem_size * elem_size * strength);

	if (OnServer())
		object->m_pPhysicsShell->get_ElementByStoreOrder(0)->applyGravityAccel(dir);
	
	update_hold_sound();
}

void STelekineticObject::prepare_keep()
{
	switch_state(ETelekineticState::TS_KEEP);
	time_keep_updated = 0;
}

bool STelekineticObject::keep_time_elapsed() const
{
	if (time_keep_started + time_to_keep < Device.dwTimeGlobal)
		return true;

	return false;
}

bool STelekineticObject::throw_time_elapsed() const
{
	if (time_throw_started + DELAY_AFTER_THROW < time())
		return true;

	return false;
}

void STelekineticObject::perform_keep_object()
{
	if (!object || !object->m_pPhysicsShell || !object->m_pPhysicsShell->isActive()) 
		return;
	
	Fvector dir;
	float current_height = object->Position().y;
	
	if (current_height > target_height) 
		dir.set(0.f, -1.0f, 0.f);
	else if (current_height < target_height) 
		dir.set(0.f, 1.0f, 0.f);
	else
	{
		dir.set(
			Random.randF(-1.0f, 1.0f), 
			Random.randF(-1.0f, 1.0f), 
			Random.randF(-1.0f, 1.0f)
		);
		dir.normalize_safe();
	}

	dir.mul(5.0f);

	if (OnServer())
		object->m_pPhysicsShell->get_ElementByStoreOrder(0)->applyGravityAccel(dir);
	
	time_keep_updated = Device.dwTimeGlobal;
	update_hold_sound();
}

void STelekineticObject::release()
{
	if (!object || !object->m_pPhysicsShell)
		return;
	
	Fvector random_dir;
	random_dir.random_dir();
	random_dir.normalize();
	
	object->m_pPhysicsShell->set_ApplyByGravity(true);
	
	if (OnServer())
		object->m_pPhysicsShell->applyImpulseTrace(object->Position(), random_dir,
		                                           object->m_pPhysicsShell->getMass() * 2.f);
	
	stop_object_particles();
	switch_state(ETelekineticState::TS_NONE);
}

void STelekineticObject::throw_object_time(const Fvector& target, float time)
{
	switch_state(ETelekineticState::TS_THROW);

	if (!object || !object->m_pPhysicsShell || !object->m_pPhysicsShell->isActive())
		return;

	// включить гравитацию
	object->m_pPhysicsShell->set_ApplyByGravity(true);

	Fvector transference;
	transference.sub(target, object->Position());
	TransferenceToThrowVel(transference, time, object->EffectiveGravity());
	object->m_pPhysicsShell->applyImpulseTrace(object->Position(), transference, object->m_pPhysicsShell->getMass());

	if (sound_throw.handle())
		sound_throw.play_at_pos(object, object->Position());

	if (sound_hold.is_playing())
		sound_hold.stop();
	
	stop_object_particles();
}

void STelekineticObject::throw_object(const Fvector& target, float power)
{
	switch_state(ETelekineticState::TS_THROW);

	if (!object || !object->m_pPhysicsShell || !object->m_pPhysicsShell->isActive())
		return;

	// вычислить направление
	Fvector dir;
	dir.sub(target, object->Position());
	dir.normalize();

	// включить гравитацию
	object->m_pPhysicsShell->set_ApplyByGravity(true);

	if (OnServer())
		for (u32 i = 0; i < object->m_pPhysicsShell->get_ElementsNumber(); i++)
			object->m_pPhysicsShell->get_ElementByStoreOrder(static_cast<u16>(i))->applyImpulse(
				dir, power * 20.f * object->m_pPhysicsShell->getMass() / object->m_pPhysicsShell->Elements().size());
};

bool STelekineticObject::check_height() const
{
	if (!object)
		return true;

	return object->Position().y > target_height;
}

bool STelekineticObject::check_raise_time_out() const
{
	if (time_raise_started + RAISE_MAX_TIME < Device.dwTimeGlobal)
		return true;

	return false;
}

void STelekineticObject::enable() const
{
	if (object->m_pPhysicsShell)
		object->m_pPhysicsShell->Enable();
}

void STelekineticObject::rotate() const
{
	if (!object || !object->m_pPhysicsShell || !object->m_pPhysicsShell->isActive())
		return;
	
	Fvector dir;
	dir.random_dir();
	dir.normalize();

	if (OnServer())
		object->m_pPhysicsShell->applyImpulse(dir, 2.5f * object->m_pPhysicsShell->getMass());
}

void STelekineticObject::update_hold_sound()
{
	if (sound_hold.handle()) 
		return;
	
	if (sound_hold.is_playing())
		sound_hold.set_position(object->Position());
	else
		sound_hold.play_at_pos(object, object->Position());
}

// -------------------- WEAPON CONTROLLER --------------------

STelekineticWeaponObject::STelekineticWeaponObject(ITelekineticEnemy* tele_enemy,
                                                   STelekineticWeaponParams& weapon_params, 
                                                   CPhysicsShellHolder* owner,
                                                   float s, 
                                                   float h,
                                                   u32 ttk,
                                                   bool rot) :
	STelekineticObject(owner, s, h, ttk, rot),
	telekinetic_enemy(tele_enemy),
	weapon(owner->cast_weapon_magazined()),
	weapon_next_phase_time(0),
	weapon_params(weapon_params)
{
	STelekineticWeaponObject::switch_state(ETelekineticState::TS_RAISE);
}

void STelekineticWeaponObject::setup_local_weapon_things()
{
	const CEntityAlive* enemy = telekinetic_enemy->get_enemy();
	
	if (enemy == nullptr)
		return;
	
	if (weapon == nullptr)
		return;
	
	if (IsGameTypeSingle() == false)
		return;
	
	backup_weapon_dispersion = weapon->getFireDispersionBase();
	backup_weapon_fire_mode = weapon->GetQueueSize();
	
	weapon->SetInitiator(telekinetic_enemy->get_self()->ID());
	
	first_shot_delay_ms = time() + weapon_params.delay_before_first_shot;
	
	// WEAPON_ININITE_QUEUE (-1) = auto, 1 = single, 2 = burst
	weapon->SetQueueSize(WEAPON_ININITE_QUEUE); // чтобы пистолетам задать режим стрельбы auto
	
	if (CActor* actor = smart_cast<CActor*>(enemy); actor != nullptr)
	{
		switch (g_SingleGameDifficulty)
		{
		case egdNovice:
			weapon->setFireDispersionBase(0.15f);
			break;
		
		case egdStalker:
			weapon->setFireDispersionBase(0.13f);
			break;
		
		case egdVeteran:
			weapon->setFireDispersionBase(0.11f);
			break;
		
		case egdMaster:
			weapon->setFireDispersionBase(0.1f);
			break;
		}
	}
	else if (CAI_Stalker* cai_stalker = smart_cast<CAI_Stalker*>(enemy); cai_stalker != nullptr)
	{
		// Скипаем некотоыре пушки, ибо шотгану, с его разбросом, ещё поверх крутить точно ничего не нужно и т.д.
		if (weapon->cast_weapon_shotgun() /*|| weapon->cast_weapon_rg6()*/)
			return;
		
		weapon->setFireDispersionBase(0.2f);
	}
}

void STelekineticWeaponObject::restore_global_weapon_things()
{
	if (weapon == nullptr)
		return;
	
	weapon->SetInitiator(-1);
	weapon->SetQueueSize(backup_weapon_fire_mode);
	weapon->setFireDispersionBase(backup_weapon_dispersion);
}

void STelekineticWeaponObject::debug_draw()
{
	const CEntityAlive* enemy_ = telekinetic_enemy->get_enemy();
	if (!enemy_) return;

	Fvector enemy_pos = enemy_->Position();
	Fvector enemy_dir = enemy_pos - weapon->Position();
        	
	float distance_to_enemy = enemy_dir.magnitude();
	
	shared_str state_text;

	switch (get_state())
	{
	case ETelekineticState::TS_RAISE:
		state_text = shared_str().printf("Raising %d ms", time() - time_raise_started);
		break;

	case ETelekineticState::TS_KEEP:
		state_text = shared_str().printf("Keeping %d ms", time_keep_started + time_to_keep - time());
		break;

	case ETelekineticState::TS_THROW:
		state_text = shared_str().printf("Throw %d ms", time_throw_started + DELAY_AFTER_THROW - time());
		break;

	case ETelekineticState::TS_NONE:
		state_text = "NONE";
		break;
	}
        
	shared_str queue_type;
	
	switch (weapon->GetQueueSize())
	{
	case WEAPON_ININITE_QUEUE:
		queue_type = "AUTO";
		break;
		
	case 0:
		queue_type = "SINGLE";
		break;
		
	case 1:
		queue_type = "BURST";
		break;
	}
	
	shared_str time_to_shoot_end;
	
	switch (weapon->IsWorking())
	{
	case true:
		{
			u32 shot_interval = static_cast<u32>(weapon->getRPM() * 1000.f);
			u32 shot_time = weapon_next_phase_time - weapon_phase_start_time;
			u32 doing_shoots = shot_time / shot_interval;
			
			time_to_shoot_end = shared_str().printf("Time to shoot end: %u, doing %u shots", weapon_next_phase_time - time(), doing_shoots);
		}	
		break;
		
	case false:
		time_to_shoot_end = shared_str().printf("Time to start shoot: %u", weapon_next_phase_time - time());	
		break;
	}
	
	shared_str main_text = shared_str().printf(
		"Ammo %d/%d | Distance to enemy: %.2f m | State: %s | Weapon dispersion: %.3f | Queue type: %s | %s",
		weapon->GetAmmoElapsed(), 
		weapon->GetAmmoMagSize(),
		distance_to_enemy,
		state_text.c_str(),
		weapon->getFireDispersionBase(),
		queue_type.c_str(),
		time_to_shoot_end.c_str()
	);
        
	HUD().world_prims.append_text3d(weapon->Position(), main_text);
	HUD().world_prims.append_line(weapon->get_LastFP(), Fvector().mad(weapon->get_LastFP(), weapon->get_LastFD(), telekinetic_enemy->get_tele_distance()), color_rgba(0, 255, 0, 255));
}

void STelekineticWeaponObject::update_auto_aim()
{
	if (weapon->GetAmmoElapsed() <= 0)
		return;
	
	if (weapon->IsMisfire())
		return;
	
	const CEntityAlive* enemy = telekinetic_enemy->get_enemy();
	
	if (enemy == nullptr)
		return;
	
	Fvector pos = smart_cast<CGameObject*>(telekinetic_enemy)->Position();
	float current_distance = enemy->Position().distance_to_sqr(pos);
	float max_tele_work_distance = _sqr(telekinetic_enemy->get_tele_distance());

	if (current_distance > max_tele_work_distance)
		return;
	
	Fmatrix target_xf;
	target_xf.k.set(enemy->Center() - weapon->get_LastFP());

	Fvector::generate_orthonormal_basis_normalized(target_xf.k,target_xf.j,target_xf.i);
	
	Fvector curr_eulers, target_eulers;
	target_xf.getXYZi(target_eulers);
	weapon->XFORM().getXYZi(curr_eulers);

	Fvector diff
	{
		angle_difference_signed(target_eulers.x, curr_eulers.x),
		angle_difference_signed(target_eulers.y, curr_eulers.y),
		angle_difference_signed(target_eulers.z, curr_eulers.z)
	};
	
	diff.mul(weapon->m_pPhysicsShell->getMass() * weapon_params.autoaim_torque_factor);
	weapon->m_pPhysicsShell->setTorque(diff);
    	
	weapon->XFORM().transform_dir(diff);
	weapon->m_pPhysicsShell->set_AngularVel(diff);
}

bool STelekineticWeaponObject::can_shoot()
{
	const CEntityAlive* enemy_ = telekinetic_enemy->get_enemy();
	
	if (enemy_ == nullptr) 
		return false;
	
	if (weapon == nullptr)
		return false;
	
	if (weapon->GetAmmoElapsed() + weapon->GetAmmoChamberElapsed() <= 0)
		return false;
	
	if (!enemy_->g_Alive())
		return false;
	
	if (first_shot_delay_ms > time())
		return false;
	
	if (!is_enemy_tracing())
		return false;
	
	return true;
}

void STelekineticWeaponObject::try_shoot()
{
	if (u32 now = time(); now >= weapon_next_phase_time)
	{
		u32 shot_interval = static_cast<u32>(weapon->getRPM() * 1000.f);
		u32 mag_size_third = weapon->GetAmmoMagSize() / 3;
		mag_size_third = std::max(2u, mag_size_third);
		
		if (weapon->IsWorking())
		{
			u32 shots_skip = Random.randI(1, mag_size_third);
			u32 pause_time = time() + shot_interval * shots_skip;
			weapon_end_shooting(pause_time);
		}
		else
		{
			if (weapon->IsGrenadeMode())
			{
				weapon->cast_weapon_magazined_w_grenade()->state_Fire(Device.fTimeDelta);
				weapon_end_shooting();
				return;
			}
			u32 do_shots = Random.randI(1, mag_size_third);
			u32 end_shoot_time = time() + shot_interval * do_shots;
			weapon_start_shooting(end_shoot_time);
		}
	}
}

void STelekineticWeaponObject::weapon_start_shooting(u32 shoot_time)
{
	weapon_next_phase_time = shoot_time;
	weapon_phase_start_time = time();
	weapon->FireStart();
}

void STelekineticWeaponObject::weapon_end_shooting(u32 pause_time)
{
	weapon_phase_start_time = time();
	weapon_next_phase_time = pause_time;
	weapon->FireEnd();
}

bool STelekineticWeaponObject::is_enemy_tracing()
{
	CEntityAlive* enemy = telekinetic_enemy->get_enemy();
	
	if (enemy == nullptr) 
		return false;
	
	const Fvector& fire_pos = weapon->get_LastFP();
	const Fvector& fire_dir = weapon->get_LastFD();
	
	collide::rq_result rq_result;
	
	Level().ObjectSpace.RayPick(
		fire_pos,
		fire_dir,
		fire_pos.distance_to(enemy->Center()),
		collide::rqtBoth,
		rq_result,
		weapon
	);
	
	return rq_result.O == enemy;
}

bool STelekineticWeaponObject::is_angle_aim_error_correct(float threshold)
{
	const CEntityAlive* enemy = telekinetic_enemy->get_enemy();

	if (enemy == nullptr)
	{
		return false;
	}

	Fvector to_enemy;
	to_enemy.sub(enemy->Position(), weapon->get_LastFP());
	to_enemy.normalize();

	const float dot = weapon->get_LastFD().dotproduct(to_enemy);
	return dot >= cosf(deg2rad(threshold));
}

void STelekineticWeaponObject::perform_keep_object()
{
	inherited::perform_keep_object();
	
	update_auto_aim();

	if (!can_shoot())
	{
		weapon_end_shooting();
		return;
	}
	try_shoot();
}

bool STelekineticWeaponObject::can_be_thrown()
{
	u32 current_elapsed = weapon->GetCurrentElapsed(weapon->IsGrenadeMode());
	u32 current_champer = weapon->GetAmmoChamberElapsed();
	
	return current_elapsed + current_champer <= 0 || weapon->IsMisfire();
}

void STelekineticWeaponObject::release()
{
	inherited::release();
	weapon_end_shooting();
}

void STelekineticWeaponObject::switch_state(ETelekineticState new_state)
{
	inherited::switch_state(new_state);

	if (state == ETelekineticState::TS_RAISE)
	{
		weapon->SetCanTake(false);
		setup_local_weapon_things();
	}	
	
	if (state == ETelekineticState::TS_THROW || new_state == ETelekineticState::TS_NONE)
	{
		weapon->SetCanTake(true);
		weapon_end_shooting();
		restore_global_weapon_things();
	}
}

// -------------------- GRENADE CONTROLLER --------------------

STelekineticGrenadeObject::STelekineticGrenadeObject(ITelekineticEnemy* tele_enemy, CPhysicsShellHolder* owner, float s, float h, u32 ttk, bool rot) :
	STelekineticObject(owner, s, h, ttk, rot),
	grenade(owner->cast_grenade()),
	telekinetic_enemy(tele_enemy)
{
	STelekineticGrenadeObject::switch_state(ETelekineticState::TS_RAISE);
}

void STelekineticGrenadeObject::debug_draw()
{
	shared_str state_text;
	
	switch (get_state())
	{
	case ETelekineticState::TS_RAISE:
		state_text = shared_str().printf("Raising %d ms", time() - time_raise_started);
		break;
	
	case ETelekineticState::TS_KEEP:
		state_text = shared_str().printf("Keeping %d ms", time_keep_started + time_to_keep - time());
		break;
	
	case ETelekineticState::TS_THROW:
		state_text = shared_str().printf("Throw %d ms", time_throw_started + DELAY_AFTER_THROW - time());
		break;
	
	case ETelekineticState::TS_NONE:
		state_text = "NONE";
		break;
	}
	
	HUD().world_prims.append_text3d(grenade->Position(), state_text);
}

void STelekineticGrenadeObject::switch_state(ETelekineticState new_state)
{
	inherited::switch_state(new_state);
}

void STelekineticGrenadeObject::perform_keep_object()
{
	inherited::perform_keep_object();
	
	const CEntityAlive* enemy = telekinetic_enemy->get_enemy();
		
	if (enemy == nullptr)
		return;
	
	if (grenade->destroy_time() == grenade_initial_time)
	{
		grenade->State(CGrenade::eThrowStart);
		grenade->set_destroy_time(time_to_explode);
	}
}

bool STelekineticGrenadeObject::can_be_thrown()  
{
	u32 now = time();
	u32 explode_global_time = grenade->destroy_time();
	
	u32 activation_time = explode_global_time - time_to_explode;
	u32 elapsed_since_activation = now - activation_time;
	
	return elapsed_since_activation > throw_threshold;
}

bool STelekineticGrenadeObject::can_be_picked_up()
{
	if (grenade->destroy_time() != grenade_initial_time)
		return false;
	
	return true;
}
