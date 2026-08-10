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
#include "../../xrEngine/xr_ioc_cmd.h"
#include "../../Inventory.h"
#include "../../ActorCondition.h"

STelekineticObject::STelekineticObject(const STelekineticObjectParams& tele_params) : params(tele_params)
{
	STelekineticObject::switch_state(ETelekineticState::TS_RAISE);
	this->params.target_height = this->params.object->Position().y + this->params.target_height;
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
	TParticlesPlayer* PPlayer = params.object->GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->StartParticles(particle_sect, Fvector().set(0.0f, 0.1f, 0.0f), params.object->ID());
}

void STelekineticObject::stop_object_particles()
{
	TParticlesPlayer* PPlayer = params.object->GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->StopParticles(particle_sect, BI_NONE, true);
}

void STelekineticObject::raise_update()
{
	if (check_height() || check_raise_time_out())
	{
		prepare_keep();
	}
	else if (params.rotate_object)
	{
		rotate();
	}
}

void STelekineticObject::keep_update()
{
	if (keep_time_elapsed())
	{
		release();
	}
}

void STelekineticObject::throw_update()
{
	if (throw_time_elapsed())
	{
		release();
	}
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

void STelekineticObject::collision_callback(bool& do_colide, bool bo1, dContact& c, SGameMtl* material_1, SGameMtl* material_2)
{
	dxGeomUserData* self = bo1 ? PHRetrieveGeomUserData(c.geom.g1) : PHRetrieveGeomUserData(c.geom.g2);
	dxGeomUserData* damage_receiver = bo1 ? PHRetrieveGeomUserData(c.geom.g2) : PHRetrieveGeomUserData(c.geom.g1);

	if (self == nullptr || self->ph_ref_object == nullptr)
	{
		return;
	}

	CPhysicsShellHolder* ph_self_object = smart_cast<CPhysicsShellHolder*>(self->ph_ref_object);
	if (ph_self_object == nullptr || ph_self_object->m_pPhysicsShell == nullptr)
	{
		return;
	}

	ph_self_object->m_pPhysicsShell->remove_ObjectContactCallback(collision_callback);

	CPhysicsShellHolder* ph_damage_receiver = damage_receiver ? smart_cast<CPhysicsShellHolder*>(damage_receiver->ph_ref_object) : nullptr;
	if (ph_damage_receiver == nullptr)
	{
		return;
	}

	CEntityAlive* entity_alive = ph_damage_receiver->cast_entity_alive();
	if (entity_alive == nullptr)
	{
		return;
	}

	CActor* actor = ph_damage_receiver->cast_actor();
	CAI_Stalker* ai_stalker = ph_damage_receiver->cast_stalker();

	if (do_colide && (actor && !GodMode() || ai_stalker) && (entity_alive->g_Alive() && (!entity_alive->cast_creature() || !entity_alive->cast_creature()->invulnerable())))
	{
		Fvector linear_vel;
		ph_self_object->PHGetLinearVell(linear_vel);

		float vel = linear_vel.magnitude();
		float pure_j = EPS_L;

		if (PIItem item = ph_self_object->cast_inventory_item())
		{
			pure_j = item->Weight() * pow(vel, 2.f) * .5f;
		}
		else
		{
			pure_j = ph_self_object->GetMass() * pow(vel, 2.f) * .5f;
		}

		float kinetic_energy = fabsf(1.f - expf(-EPS_L * pure_j));
		float difficulty_modifier = 1.f;

		if (actor)
		{
			switch (g_SingleGameDifficulty)
			{
				case egdNovice:
					difficulty_modifier = 0.50f;
					break;

				case egdStalker:
					difficulty_modifier = 0.70f;
					break;

				case egdVeteran:
					difficulty_modifier = 0.80f;
					break;

				case egdMaster:
					difficulty_modifier = 1.0f;
					break;
			}
		}
		else if (ai_stalker)
		{
			difficulty_modifier = .3f;
		}
		
		float health_loss = kinetic_energy * difficulty_modifier;

		if (actor && EngineExternal()[EEngineExternalGame::EnablePolterStaminaLooseOnHit])
		{
			entity_alive->conditions().SetPower(entity_alive->conditions().GetPower() - health_loss);
		}
		entity_alive->conditions().SetHealth(entity_alive->conditions().GetHealth() - health_loss);

		if (actor && EngineExternal()[EEngineExternalGame::EnablePolterDrop])
		{
			PIItem item = actor->inventory().ActiveItem();
			CCustomDevice* device = actor->GetDevice();

			constexpr float porog_stamini_4tobi_vironit_pushky_iz_arms = 33;
			constexpr float porog_health_4tobi_vironit_pushky_iz_arms = 50;
			bool need_kick_animator = false;
			
			bool need_drop_gun_by_power = actor->conditions().GetPower() < porog_stamini_4tobi_vironit_pushky_iz_arms / 100.f;
			bool need_drop_gun_by_health = actor->conditions().GetHealth() < porog_health_4tobi_vironit_pushky_iz_arms / 100.f;
			
			if (item != nullptr && (need_drop_gun_by_power || need_drop_gun_by_health))
			{
				u16 slot = actor->inventory().ActiveItem()->BaseSlot();

				if (!actor->inventory().SlotIsPersistent(slot) && !actor->inventory().Action(kDROP, CMD_STOP))
				{
					actor->g_PerformDrop();
					need_kick_animator = true;
				}
			}

			constexpr float porog_stamini_4tobi_vironit_detektor_iz_arms = 33;
			constexpr float porog_health_4tobi_vironit_detektor_iz_arms = 50;
			
			bool need_drop_detector_by_power = actor->conditions().GetPower() < porog_stamini_4tobi_vironit_detektor_iz_arms / 100.f;
			bool need_drop_detector_by_health = actor->conditions().GetHealth() < porog_health_4tobi_vironit_detektor_iz_arms / 100.f;
			
			if (device != nullptr && (need_drop_detector_by_power || need_drop_detector_by_health))
			{
				device->SetDropManual(true);
				need_kick_animator = true;
			}

			if (need_kick_animator && !actor->HudAnimator()->ItemAnimator()->IsActive())
			{
				actor->inventory().SetActiveSlot(NO_ACTIVE_SLOT);

				const shared_str& front_kick_animator = actor->m_sFrontKickAnimator;
				const shared_str& back_kick_animator = actor->m_sBackKickAnimator;

				Fvector object_pos = Fvector().set(self->last_pos);
				Fvector damage_receiver_pos = Fvector().set(damage_receiver->last_pos);

				Fvector hit_dir;
				hit_dir.sub(damage_receiver_pos, object_pos);
				hit_dir.normalize();

				if (hit_dir.dotproduct(Device.vCameraDirection) < 0.f)
				{
					if (front_kick_animator != nullptr)
					{
						actor->HudAnimator()->ItemAnimator()->StartAnimator(front_kick_animator);
					}
				}
				else
				{
					if (back_kick_animator != nullptr)
					{
						Actor()->HudAnimator()->ItemAnimator()->StartAnimator(back_kick_animator);
					}
				}
			}
		}
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
		case ETelekineticState::TS_NONE:
			break;
	}

	state = new_state;
}

void STelekineticObject::raise(float step)
{
	if (!params.object || !params.object->m_pPhysicsShell || !params.object->m_pPhysicsShell->isActive())
	{
		return;
	}

	step *= params.strength;

	Fvector dir;
	dir.set(0.f, 1.0f, 0.f);

	float elem_size = float(params.object->m_pPhysicsShell->Elements().size());
	dir.mul(elem_size * elem_size * params.strength);

	if (OnServer())
	{
		params.object->m_pPhysicsShell->get_ElementByStoreOrder(0)->applyGravityAccel(dir);
	}

	update_hold_sound();
}

void STelekineticObject::prepare_keep()
{
	switch_state(ETelekineticState::TS_KEEP);
	time_keep_updated = 0;
}

bool STelekineticObject::keep_time_elapsed() const
{
	return time_keep_started + params.time_to_keep < Device.dwTimeGlobal;
}

bool STelekineticObject::throw_time_elapsed() const
{
	return time_throw_started + DELAY_AFTER_THROW < time();
}

void STelekineticObject::perform_keep_object()
{
	if (!params.object || !params.object->m_pPhysicsShell || !params.object->m_pPhysicsShell->isActive())
	{
		return;
	}

	Fvector dir;
	float current_height = params.object->Position().y;

	if (current_height > params.target_height)
	{
		dir.set(0.f, -1.0f, 0.f);
	}
	else if (current_height < params.target_height)
	{
		dir.set(0.f, 1.0f, 0.f);
	}
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
	{
		params.object->m_pPhysicsShell->get_ElementByStoreOrder(0)->applyGravityAccel(dir);
	}

	time_keep_updated = Device.dwTimeGlobal;
	update_hold_sound();
}

void STelekineticObject::release()
{
	if (!params.object || !params.object->m_pPhysicsShell)
	{
		return;
	}

	Fvector random_dir;
	random_dir.random_dir();
	random_dir.normalize();

	params.object->m_pPhysicsShell->set_ApplyByGravity(true);

	if (OnServer())
	{
		params.object->m_pPhysicsShell->applyImpulseTrace(params.object->Position(), random_dir, params.object->m_pPhysicsShell->getMass() * 2.f);
	}

	stop_object_particles();
	switch_state(ETelekineticState::TS_NONE);
}

void STelekineticObject::throw_object_time(const Fvector& target, float time)
{
	switch_state(ETelekineticState::TS_THROW);

	if (!params.object || !params.object->m_pPhysicsShell || !params.object->m_pPhysicsShell->isActive())
	{
		return;
	}

	// включить гравитацию
	params.object->m_pPhysicsShell->set_ApplyByGravity(true);

	Fvector transference;
	transference.sub(target, params.object->Position());
	TransferenceToThrowVel(transference, time, params.object->EffectiveGravity());

	// Aphile: хак, задаём new SCollisionHitCallback, чтобы физика не считала урон от столкновения, 
	// а все рассчёт проходили в кастомном collide_callback.
	params.object->set_collision_hit_callback(new SCollisionHitCallback(params.object));
	params.object->m_pPhysicsShell->add_ObjectContactCallback(collision_callback);

	params.object->m_pPhysicsShell->applyImpulseTrace(params.object->Position(), transference, params.object->m_pPhysicsShell->getMass());

	if (sound_throw.handle() && sound_hold.is_playing())
	{
		sound_hold.stop();
		sound_throw.play_at_pos(params.object, params.object->Position());
	}
	stop_object_particles();
}

void STelekineticObject::throw_object(const Fvector& target, float power)
{
	switch_state(ETelekineticState::TS_THROW);

	if (!params.object || !params.object->m_pPhysicsShell || !params.object->m_pPhysicsShell->isActive())
	{
		return;
	}

	// вычислить направление
	Fvector dir;
	dir.sub(target, params.object->Position());
	dir.normalize();

	// включить гравитацию
	params.object->m_pPhysicsShell->set_ApplyByGravity(true);

	if (OnServer())
	{
		for (u32 i = 0; i < params.object->m_pPhysicsShell->get_ElementsNumber(); i++)
		{
			params.object->m_pPhysicsShell->get_ElementByStoreOrder(static_cast<u16>(i))->applyImpulse(dir, power * 20.f * params.object->m_pPhysicsShell->getMass() / params.object->m_pPhysicsShell->Elements().size());
		}
	}
}

bool STelekineticObject::check_height() const
{
	return params.object ? params.object->Position().y > params.target_height : true;
}

bool STelekineticObject::check_raise_time_out() const
{
	if (time_raise_started + RAISE_MAX_TIME < Device.dwTimeGlobal)
	{
		return true;
	}
	return false;
}

void STelekineticObject::enable() const
{
	if (params.object->m_pPhysicsShell)
	{
		params.object->m_pPhysicsShell->Enable();
	}
}

void STelekineticObject::rotate() const
{
	if (!params.object || !params.object->m_pPhysicsShell || !params.object->m_pPhysicsShell->isActive())
	{
		return;
	}

	Fvector dir;
	dir.random_dir();
	dir.normalize();

	if (OnServer())
	{
		params.object->m_pPhysicsShell->applyImpulse(dir, 2.5f * params.object->m_pPhysicsShell->getMass());
	}
}

void STelekineticObject::update_hold_sound()
{
	if (sound_hold.handle())
	{
		return;
	}

	if (sound_hold.is_playing())
	{
		sound_hold.set_position(params.object->Position());
	}
	else
	{
		sound_hold.play_at_pos(params.object, params.object->Position());
	}
}

// -------------------- WEAPON CONTROLLER --------------------

STelekineticWeaponObject::STelekineticWeaponObject(STelekineticWeaponParams weapon_params, const STelekineticObjectParams& tele_params) : 
	STelekineticObject(tele_params), weapon_params(weapon_params), weapon(smart_cast<CWeaponMagazined*>(tele_params.object))
{
	STelekineticWeaponObject::switch_state(ETelekineticState::TS_RAISE);
}

void STelekineticWeaponObject::setup_local_weapon_things()
{
	const CEntityAlive* enemy = weapon_params.telekinetic_enemy->get_enemy();

	if (enemy == nullptr)
	{
		return;
	}

	if (weapon == nullptr)
	{
		return;
	}

	if (IsGameTypeSingle() == false)
	{
		return;
	}
	
	backup_weapon_fire_mode = weapon->GetQueueSize();
	weapon->SetInitiator(weapon_params.telekinetic_enemy->get_self()->ID());
	first_shot_delay_ms = time() + weapon_params.delay_before_first_shot;
	// WEAPON_ININITE_QUEUE (-1) = auto, 1 = single, 2 = burst
	weapon->SetQueueSize(WEAPON_ININITE_QUEUE); // чтобы пистолетам задать режим стрельбы auto
}

void STelekineticWeaponObject::restore_global_weapon_things()
{
	if (weapon == nullptr)
	{
		return;
	}
	weapon->SetInitiator(-1);
	weapon->SetQueueSize(backup_weapon_fire_mode);
}

#ifdef DEBUG_DRAW
void STelekineticWeaponObject::debug_draw()
{
	const CEntityAlive* enemy_ = weapon_params.telekinetic_enemy->get_enemy();
	if (!enemy_)
	{
		return;
	}

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
			state_text = shared_str().printf("Keeping %d ms", time_keep_started + params.time_to_keep - time());
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
	HUD().world_prims.append_line(weapon->get_LastFP(), Fvector().mad(weapon->get_LastFP(), weapon->get_LastFD(), weapon_params.telekinetic_enemy->get_tele_distance()), color_rgba(0, 255, 0, 255));
}
#endif

void STelekineticWeaponObject::update_auto_aim()
{
	if (weapon->GetAmmoElapsed() <= 0)
	{
		return;
	}

	if (weapon->IsMisfire())
	{
		return;
	}

	const CEntityAlive* enemy = weapon_params.telekinetic_enemy->get_enemy();

	if (enemy == nullptr)
	{
		return;
	}

	Fvector pos = smart_cast<CGameObject*>(weapon_params.telekinetic_enemy)->Position();
	float current_distance = enemy->Position().distance_to_sqr(pos);
	float max_tele_work_distance = _sqr(weapon_params.telekinetic_enemy->get_tele_distance());

	if (current_distance > max_tele_work_distance)
	{
		return;
	}

	Fmatrix target_xf;
	target_xf.k.set(enemy->Center() - weapon->get_LastFP());

	Fvector::generate_orthonormal_basis_normalized(target_xf.k, target_xf.j, target_xf.i);

	Fvector curr_eulers, target_eulers;
	target_xf.getXYZi(target_eulers);
	weapon->XFORM().getXYZi(curr_eulers);

	Fvector angular_diff{
		angle_difference_signed(target_eulers.x, curr_eulers.x),
		angle_difference_signed(target_eulers.y, curr_eulers.y),
		angle_difference_signed(target_eulers.z, curr_eulers.z)
	};

	float angular_speed = 0.f;
	switch (g_SingleGameDifficulty)
	{
		case egdNovice:
			angular_speed = weapon_params.novice_difficulty_angular_speed;
			break;

		case egdStalker:
			angular_speed = weapon_params.stalker_difficulty_angular_speed;
			break;

		case egdVeteran:
			angular_speed = weapon_params.veteran_difficulty_angular_speed;
			break;

		case egdMaster:
			angular_speed = weapon_params.master_difficulty_angular_speed;
			break;

		case egdCount:
		case egd_force_u32:
			angular_speed = 0.f;
			break;
	}

	if (float velocity = angular_diff.magnitude(); velocity > EPS_L)
	{
		Fvector angular_vel = angular_diff;
		angular_vel.mul(deg2rad(angular_speed));

		weapon->XFORM().transform_dir(angular_vel);
		weapon->m_pPhysicsShell->set_AngularVel(angular_vel);
	}
}

bool STelekineticWeaponObject::can_shoot()
{
	const CEntityAlive* enemy_ = weapon_params.telekinetic_enemy->get_enemy();

	if (enemy_ == nullptr)
	{
		return false;
	}

	if (weapon == nullptr)
	{
		return false;
	}

	if (weapon->GetAmmoElapsed() + weapon->GetAmmoChamberElapsed() <= 0)
	{
		return false;
	}

	if (!enemy_->g_Alive())
	{
		return false;
	}

	if (first_shot_delay_ms > time())
	{
		return false;
	}

	switch (g_SingleGameDifficulty)
	{
		case egdNovice:
		{
			if (!is_enemy_tracing(weapon_params.novice_difficulty_error_angle))
			{
				return false;
			}
		}
		break;

		case egdStalker:
		{
			if (!is_enemy_tracing(weapon_params.stalker_difficulty_error_angle))
			{
				return false;
			}
		}
		break;

		case egdVeteran:
		{
			if (!is_enemy_tracing(weapon_params.veteran_difficulty_error_angle))
			{
				return false;
			}
		}
		break;

		case egdMaster:
		{
			if (!is_enemy_tracing(weapon_params.master_difficulty_error_angle))
			{
				return false;
			}
		}
		break;
	}
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

bool STelekineticWeaponObject::is_enemy_tracing(float threshold)
{
	CEntityAlive* enemy = weapon_params.telekinetic_enemy->get_enemy();

	if (enemy == nullptr)
	{
		return false;
	}

	const Fvector& fire_pos = weapon->get_LastFP();

	Fvector dir_to_enemy;
	dir_to_enemy.sub(enemy->Center(), fire_pos);
	float dist = fire_pos.distance_to(enemy->Center());
	dir_to_enemy.normalize();
	collide::rq_result rq_result;
	Level().ObjectSpace.RayPick(
		fire_pos,
		dir_to_enemy,
		dist,
		collide::rqtBoth,
		rq_result,
		weapon
	);

	float dot = weapon->get_LastFD().dotproduct(dir_to_enemy);
	return rq_result.O == enemy && dot >= cosf(deg2rad(threshold));
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

STelekineticGrenadeObject::STelekineticGrenadeObject(ITelekineticEnemy* tele_enemy, const STelekineticObjectParams& tele_params) : 
	STelekineticObject(tele_params), telekinetic_enemy(tele_enemy), grenade(smart_cast<CGrenade*>(tele_params.object))
{
	STelekineticGrenadeObject::switch_state(ETelekineticState::TS_RAISE);
}

#ifdef DEBUG_DRAW
void STelekineticGrenadeObject::debug_draw()
{
	shared_str state_text;

	switch (get_state())
	{
		case ETelekineticState::TS_RAISE:
			state_text = shared_str().printf("Raising %d ms", time() - time_raise_started);
			break;

		case ETelekineticState::TS_KEEP:
			state_text = shared_str().printf("Keeping %d ms", time_keep_started + params.time_to_keep - time());
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
#endif

void STelekineticGrenadeObject::switch_state(ETelekineticState new_state)
{
	inherited::switch_state(new_state);
}

void STelekineticGrenadeObject::perform_keep_object()
{
	inherited::perform_keep_object();

	const CEntityAlive* enemy = telekinetic_enemy->get_enemy();

	if (enemy == nullptr)
	{
		return;
	}

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
	{
		return false;
	}

	return true;
}
