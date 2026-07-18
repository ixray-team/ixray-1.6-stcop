#pragma once

#include "Grenade.h"
#include "../../../Level.h"

template <typename Object>
CStateBurerAttackTele<Object>::CStateBurerAttackTele(Object* obj) : inherited(obj)
{
	m_anim_end_tick = 0;
	m_last_grenade_scan = 0;
}

template <typename Object>
void CStateBurerAttackTele<Object>::initialize()
{
	inherited::initialize();

	m_action = ACTION_TELE_STARTED;
	selected_object = nullptr;

	SelectObjects();

	time_started = 0;
	m_anim_end_tick = 0;
	m_last_grenade_scan = 0;
	m_initial_health = this->object->conditions().GetHealth();
	m_end_tick = time() + this->object->tele_max_time;

	// запретить взятие скриптом
	this->object->set_script_capture(false);
}

template <typename Object>
void CStateBurerAttackTele<Object>::execute()
{
	switch (m_action)
	{
		case ACTION_TELE_STARTED:
		{
			this->object->anim().set_override_animation(eAnimTelekinesis, 0);

			if (!time_started)
			{
				const float animation_length = this->object->anim().get_animation_length(eAnimTelekinesis, 0);
				m_anim_end_tick = time() + static_cast<TTime>(animation_length * 1000);
				time_started = time();
			}
			else if (time() > m_anim_end_tick)
			{
				m_action = ACTION_TELE_CONTINUE;
			}
		}
		break;

		case ACTION_TELE_CONTINUE:
		{
			this->object->anim().set_override_animation(eAnimTelekinesis, 1);
			ExecuteTeleContinue();
		}
		break;

		case ACTION_TELE_FIRE:
		{
			this->object->anim().set_override_animation(eAnimTeleFire, 0);
			ExecuteTeleFire();

			if (this->object->CTelekinesis::get_controlled_objects_count() > 0)
			{
				m_action = ACTION_TELE_CONTINUE;
				break;
			}

			const float animation_length = this->object->anim().get_animation_length(eAnimTeleFire, 0);
			m_anim_end_tick = time() + static_cast<TTime>(animation_length * 1000);
			m_action = ACTION_WAIT_FIRE_END;
		}
		break;

		case ACTION_WAIT_FIRE_END:
			this->object->anim().set_override_animation(eAnimTeleFire, 0);

			if (time() > m_anim_end_tick)
			{
				m_action = ACTION_COMPLETED;
			}

		case ACTION_COMPLETED:
			break;
	}

	this->object->face_enemy();
}

template <typename Object>
void CStateBurerAttackTele<Object>::deactivate()
{
	tele_objects.clear();

	for (STelekineticObject* tobject : this->object->CTelekinesis::get_tele_objects())
	{
		CPhysicsShellHolder* const cur_object = tobject->params.object;
		if (!cur_object || !cur_object->m_pPhysicsShell || !cur_object->m_pPhysicsShell->isActive())
		{
			continue;
		}

		tobject->stop_object_particles();
	}

	FireAllToEnemy();
	this->object->CTelekinesis::deactivate();
	this->object->set_script_capture(true);
}

template <typename Object>
void CStateBurerAttackTele<Object>::finalize()
{
	deactivate();
	inherited::finalize();
}

template <typename Object>
void CStateBurerAttackTele<Object>::critical_finalize()
{
	deactivate();
	inherited::critical_finalize();
}

template <typename Object>
bool CStateBurerAttackTele<Object>::check_start_conditions()
{
	return CheckTeleStart();
}

template <typename Object>
bool CStateBurerAttackTele<Object>::check_completion()
{
	float dist = this->object->EnemyMan.get_enemy()->Position().distance_to(this->object->Position());

	if (dist < this->object->tele_min_distance)
	{
		return true;
	}

	if (dist > this->object->tele_max_distance)
	{
		return true;
	}

	if (this->object->conditions().GetHealth() < m_initial_health)
	{
		return true;
	}

	if (time() > m_end_tick)
	{
		return true;
	}

	if (m_action == ACTION_COMPLETED)
	{
		return true;
	}

	return false;
}

//////////////////////////////////////////////////////////////////////////

template <typename Object>
void CStateBurerAttackTele<Object>::FindFreeObjects(xr_vector<ISpatialShared>& tpObjects, const Fvector& pos)
{
	g_SpatialSpace->q_sphere(tpObjects, 0, ESPATIAL_TYPE::COLLIDEABLE, pos, this->object->tele_find_radius);
	for (ISpatialShared& SS : tpObjects)
	{
		ISpatial* S = SS.get();
		if (!S)
		{
			continue;
		}
		CObject* object = S->dcast_CObject();
		if (!object || object->getDestroy())
		{
			continue;
		}

		CPhysicsShellHolder* obj = object->cast_physics_shell_holder();
		CCreature* custom_monster = object->cast_creature();
		CGrenade* grenade = object->cast_grenade();

		if ((grenade && (grenade->IsExploding() || grenade->destroy_time() != UINT32_MAX)) ||
			!obj ||
			!obj->PPhysicsShell() ||
			!obj->PPhysicsShell()->isActive() ||
			custom_monster ||
			(obj->spawn_ini() && obj->spawn_ini()->section_exist("ph_heavy")) ||
			obj->cast_car() ||
			(obj->m_pPhysicsShell->getMass() < this->object->tele_object_min_mass) ||
			(obj->m_pPhysicsShell->getMass() > this->object->tele_object_max_mass) ||
			(obj == this->object) ||
			this->object->CTelekinesis::is_active_object(obj) ||
			!obj->m_pPhysicsShell->get_ApplyByGravity())
		{
			continue;
		}

		tele_objects.push_back(obj);
	}
}

template <typename Object>
void CStateBurerAttackTele<Object>::FindObjects()
{
	u32 res_size = (u32)tele_objects.size();
	tele_objects.clear();

	// получить список объектов вокруг врага
	m_nearest.clear();
	m_nearest.reserve(res_size);
	FindFreeObjects(m_nearest, this->object->EnemyMan.get_enemy()->Position());

	// получить список объектов вокруг монстра
	FindFreeObjects(m_nearest, this->object->Position());

	// получить список объектов между монстром и врагом
	float dist = this->object->EnemyMan.get_enemy()->Position().distance_to(this->object->Position());
	Fvector dir;
	dir.sub(this->object->EnemyMan.get_enemy()->Position(), this->object->Position());
	dir.normalize();

	Fvector pos;
	pos.mad(this->object->Position(), dir, dist / 2.f);
	FindFreeObjects(m_nearest, pos);

	std::sort(tele_objects.begin(), tele_objects.end());
	// оставить уникальные объекты
	tele_objects.erase(
		std::unique(tele_objects.begin(), tele_objects.end()),
		tele_objects.end()
	);
}

template <typename Object>
void CStateBurerAttackTele<Object>::FireAllToEnemy()
{
	if (!this->object->CTelekinesis::is_active())
	{
		return;
	}

	if (!this->object->EnemyMan.get_enemy())
	{
		return;
	}

	Fvector enemy_pos = get_head_position(const_cast<CEntityAlive*>(this->object->EnemyMan.get_enemy()));

	for (STelekineticObject* telekinetic_object : this->object->CTelekinesis::get_tele_objects())
	{
		if (!telekinetic_object->can_be_thrown())
		{
			continue;
		}

		ETelekineticState object_state = telekinetic_object->get_state();

		if (object_state != ETelekineticState::TS_KEEP)
		{
			continue;
		}

		CPhysicsShellHolder* object = telekinetic_object->get_object();

		if (object == nullptr)
		{
			continue;
		}

		float const dist_to_enemy = object->Position().distance_to(enemy_pos);
		float const fire_time = dist_to_enemy / this->object->tele_fly_velocity;

		this->object->CTelekinesis::throw_object_time(object, enemy_pos, fire_time);
		this->object->sound().play(CBurer::eMonsterSoundTeleAttack);
	}
}

template <typename Object>
void CStateBurerAttackTele<Object>::ExecuteTeleContinue()
{
	// if (time_started + this->object->m_tele_time_to_hold > Device.dwTimeGlobal) return;

	if (!this->object->EnemyMan.see_enemy_now())
	{
		return;
	}

	for (STelekineticObject* telekinetic_object : this->object->CTelekinesis::get_tele_objects())
	{
		ETelekineticState object_state = telekinetic_object->get_state();

		if (object_state == ETelekineticState::TS_KEEP && telekinetic_object->can_be_thrown())
		{
			m_action = ACTION_TELE_FIRE;
			selected_object = telekinetic_object ? telekinetic_object->get_object() : nullptr;
			return;
		}

		constexpr float max_failure_time = 6000;

		if (!IsActiveObjects() || time_started + max_failure_time < Device.dwTimeGlobal)
		{
			m_action = ACTION_COMPLETED;
			return;
		}
	}
}

template <typename Object>
void CStateBurerAttackTele<Object>::ExecuteTeleFire()
{
	CEntityAlive* enemy = const_cast<CEntityAlive*>(this->object->EnemyMan.get_enemy());

	if (enemy == nullptr)
	{
		return;
	}

	Fvector enemy_pos = get_head_position(enemy);

	const float dist_to_enemy = selected_object->Position().distance_to(enemy_pos);
	const float fire_time = dist_to_enemy / this->object->tele_fly_velocity;

	this->object->CTelekinesis::throw_object_time(selected_object, enemy_pos, fire_time);
	this->object->sound().play(CBurer::eMonsterSoundTeleAttack);
}

template <typename Object>
bool CStateBurerAttackTele<Object>::IsActiveObjects()
{
	return (this->object->CTelekinesis::get_controlled_objects_count() > 0);
}

template <typename Object>
bool CStateBurerAttackTele<Object>::CheckTeleStart()
{
	// проверка на текущую активность
	if (IsActiveObjects())
	{
		return false;
	}

	// проверить дистанцию до врага
	float dist = this->object->Position().distance_to(this->object->EnemyMan.get_enemy()->Position());
	if (dist < this->object->tele_min_distance)
	{
		return false;
	}
	if (dist > this->object->tele_max_distance)
	{
		return false;
	}

	// найти телекинетические объекты
	FindObjects();

	// если нет объектов
	if (tele_objects.empty())
	{
		return false;
	}

	// всё ок можно начинать телекинез
	return true;
}

//////////////////////////////////////////////////////////////////////////
// Выбор подходящих объектов для телекинеза
//////////////////////////////////////////////////////////////////////////
class best_object_predicate
{
	Fvector enemy_pos;
	Fvector monster_pos;

public:
	best_object_predicate(const Fvector& m_pos, const Fvector& pos)
	{
		monster_pos = m_pos;
		enemy_pos = pos;
	}

	bool operator()(const CGameObject* tpObject1, const CGameObject* tpObject2) const
	{
		float dist1 = monster_pos.distance_to(tpObject1->Position());
		float dist2 = enemy_pos.distance_to(tpObject2->Position());
		float dist3 = enemy_pos.distance_to(monster_pos);

		return ((dist1 < dist3) && (dist2 > dist3));
	};
};

class best_object_predicate2
{
	Fvector enemy_pos;
	Fvector monster_pos;

public:
	best_object_predicate2(const Fvector& m_pos, const Fvector& pos)
	{
		monster_pos = m_pos;
		enemy_pos = pos;
	}

	bool operator()(const CGameObject* tpObject1, const CGameObject* tpObject2) const
	{
		float dist1 = enemy_pos.distance_to(tpObject1->Position());
		float dist2 = enemy_pos.distance_to(tpObject2->Position());

		return dist1 < dist2;
	}
};

template <typename Object>
void CStateBurerAttackTele<Object>::SelectObjects()
{
	size_t max = std::min(tele_objects.size(), (size_t)this->object->tele_max_handled_objects);
	u32 controlled_objects_count = this->object->CTelekinesis::get_controlled_objects_count();

	if (controlled_objects_count > max)
	{
		return;
	}

	std::sort(
		tele_objects.begin(), tele_objects.end(), best_object_predicate2(this->object->Position(), this->object->EnemyMan.get_enemy()->Position())
	);

	for (u32 i = 0; i < max; ++i)
	{
		CPhysicsShellHolder* object = tele_objects[i];

		bool rotate = this->object->m_monster_type != CBaseMonster::eMonsterTypeIndoor;
		float height = this->object->tele_object_height;

		if (this->object->m_monster_type == CBaseMonster::eMonsterTypeIndoor)
		{
			height *= 0.7f;
		}

		STelekineticObject* tele_obj = nullptr;
		
		STelekineticObjectParams tele_object_params
		{
			.telekinesis = this->object->telekinesis(),
			.object = object,
			.strength = this->object->tele_raise_speed,
			.target_height = height,
			.time_to_keep = this->object->tele_time_to_hold,
			.rotate_object = rotate,
			.novice_difficulty_object_hit_factor = this->object->novice_difficulty_object_hit_factor,
			.stalker_difficulty_object_hit_factor = this->object->stalker_difficulty_object_hit_factor,
			.veteran_difficulty_object_hit_factor = this->object->veteran_difficulty_object_hit_factor,
			.master_difficulty_object_hit_factor = this->object->master_difficulty_object_hit_factor,
		};
		
		if (object->cast_weapon_magazined() && this->object->shooting_from_weapon_enable)
		{
			size_t weapons_count = std::count_if(this->object->CTelekinesis::get_tele_objects().begin(), this->object->CTelekinesis::get_tele_objects().end(), [](STelekineticObject* tele_object)
												 { return tele_object->cast_telekinetic_weapon_object(); });

			if (weapons_count >= this->object->max_pickuped_weapons)
			{
				return;
			}

			STelekineticWeaponParams weapon_params
			{
				.telekinetic_enemy = this->object,
				.delay_before_first_shot = this->object->delay_before_first_shot,

				.novice_difficulty_angular_speed = this->object->novice_difficulty_angular_speed,
				.stalker_difficulty_angular_speed = this->object->stalker_difficulty_angular_speed,
				.veteran_difficulty_angular_speed = this->object->veteran_difficulty_angular_speed,
				.master_difficulty_angular_speed = this->object->master_difficulty_angular_speed,

				.novice_difficulty_error_angle = this->object->novice_difficulty_error_angle,
				.stalker_difficulty_error_angle = this->object->stalker_difficulty_error_angle,
				.veteran_difficulty_error_angle = this->object->veteran_difficulty_error_angle,
				.master_difficulty_error_angle = this->object->master_difficulty_error_angle,
			};

			tele_obj = new STelekineticWeaponObject(weapon_params, tele_object_params);
		}
		else if (object->cast_grenade() && this->object->shooting_from_weapon_enable)
		{
			tele_obj = new STelekineticGrenadeObject(this->object, tele_object_params);
		}
		else
		{
			tele_obj = new STelekineticObject(tele_object_params);
		}

		if (!tele_obj->can_be_picked_up())
		{
			xr_delete(tele_obj);
			return;
		}

		this->object->CTelekinesis::append_tobject(tele_obj);
		tele_obj->set_sound(this->object->sound_tele_hold, this->object->sound_tele_throw);
		tele_obj->set_particle(this->object->particle_tele_object);
		tele_obj->start_object_particles();
	}
}