#include "StdAfx.h"
#include "HUDManager.h"
#include "poltergeist.h"
#include "WeaponMagazined.h"
#include "../../../PhysicsShellHolder.h"
#include "../../../Level.h"
#include "../../../Actor.h"
#include "../../../../xrPhysics/IColisiondamageInfo.h"
#include "../../../ActorCondition.h"
#include "../../../Inventory.h"
#include "../../../Weapon.h"

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

		return dist1 < dist3 && dist2 > dist3;
	}
};

class best_object_predicate2
{
	Fvector enemy_pos;
	Fvector monster_pos;

public:
	using CObject_ptr = CObject*;

	best_object_predicate2(const Fvector& m_pos, const Fvector& pos)
	{
		monster_pos = m_pos;
		enemy_pos = pos;
	}

	bool operator()(const CObject_ptr& tpObject1, const CObject_ptr& tpObject2) const
	{
		float dist1 = enemy_pos.distance_to(tpObject1->Position());
		float dist2 = enemy_pos.distance_to(tpObject2->Position());

		return dist1 < dist2;
	}
};

CTelekineticPoltergeist::CTelekineticPoltergeist(CPoltergeist* polter) : inherited(polter), object_collision_damage(0.5f)
{
}

CTelekineticPoltergeist::~CTelekineticPoltergeist() = default;

void CTelekineticPoltergeist::load(LPCSTR section)
{
	inherited::load(section);

	radius = READ_IF_EXISTS(pSettings, r_float, section, "Tele_Find_Radius", 10.f);
	object_min_mass = READ_IF_EXISTS(pSettings, r_float, section, "Tele_Object_Min_Mass", 40.f);
	object_max_mass = READ_IF_EXISTS(pSettings, r_float, section, "Tele_Object_Max_Mass", 500.f);
	object_count = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Object_Count", 10);
	time_to_hold = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Hold_Time", 3000);
	time_to_wait = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Wait_Time", 3000);
	time_to_wait_in_objects = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Delay_Between_Objects_Time", 500);
	distance = READ_IF_EXISTS(pSettings, r_float, section, "Tele_Distance", 50.f);
	object_height = READ_IF_EXISTS(pSettings, r_float, section, "Tele_Object_Height", 10.f);
	time_object_keep = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Time_Object_Keep", 10000);
	raise_speed = READ_IF_EXISTS(pSettings, r_float, section, "Tele_Raise_Speed", 3.f);
	raise_time_to_wait_in_objects = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Delay_Between_Objects_Raise_Time", 500);
	fly_velocity = READ_IF_EXISTS(pSettings, r_float, section, "Tele_Fly_Velocity", 30.f);

	shooting_from_weapon_enable = READ_IF_EXISTS(pSettings, r_bool, section, "Tele_Shooting_From_Weapon_Enable", true);
	activate_n_throw_grenade = READ_IF_EXISTS(pSettings, r_bool, section, "Tele_Activate_N_Throw_Grenade", true);
	max_pickuped_weapons = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Max_Pickuped_Weapons", 2);
	delay_before_first_shot = READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Delay_Before_First_Shoot", 0);
	particle_tele_object = READ_IF_EXISTS(pSettings, r_string, section, "Particle_Tele_Object", "static\\fire_distort");

	novice_difficulty_angular_speed = READ_IF_EXISTS(pSettings, r_float, section, "Novice_Difficulty_Angular_Speed", 180.f);
	stalker_difficulty_angular_speed = READ_IF_EXISTS(pSettings, r_float, section, "Novice_Difficulty_Angular_Speed", 200.f);
	veteran_difficulty_angular_speed = READ_IF_EXISTS(pSettings, r_float, section, "Novice_Difficulty_Angular_Speed", 240.f);
	master_difficulty_angular_speed = READ_IF_EXISTS(pSettings, r_float, section, "Novice_Difficulty_Angular_Speed", 280.f);

	clamp(novice_difficulty_angular_speed, EPS_S, 360.f);
	clamp(stalker_difficulty_angular_speed, EPS_L, 360.f);
	clamp(veteran_difficulty_angular_speed, EPS_L, 360.f);
	clamp(master_difficulty_angular_speed, EPS_L, 360.f);

	novice_difficulty_error_angle = READ_IF_EXISTS(pSettings, r_float, section, "Novice_Difficulty_Error_Angle", 30.f);
	stalker_difficulty_error_angle = READ_IF_EXISTS(pSettings, r_float, section, "Stalker_Difficulty_Error_Angle", 20.f);
	veteran_difficulty_error_angle = READ_IF_EXISTS(pSettings, r_float, section, "Veteran_Difficulty_Error_Angle", 15.f);
	master_difficulty_error_angle = READ_IF_EXISTS(pSettings, r_float, section, "Master_Difficulty_Error_Angle", 8.f);

	clamp(novice_difficulty_error_angle, EPS_L, 180.f);
	clamp(stalker_difficulty_error_angle, EPS_L, 180.f);
	clamp(veteran_difficulty_error_angle, EPS_L, 180.f);
	clamp(master_difficulty_error_angle, EPS_L, 180.f);

	novice_difficulty_object_hit_factor = READ_IF_EXISTS(pSettings, r_float, section, "Novice_Difficulty_Throwed_Object_Hit_Factor", 0.1f);
	stalker_difficulty_object_hit_factor = READ_IF_EXISTS(pSettings, r_float, section, "Stalker_Difficulty_Throwed_Object_Hit_Factor", 0.2f);
	veteran_difficulty_object_hit_factor = READ_IF_EXISTS(pSettings, r_float, section, "Veteran_Difficulty_Throwed_Object_Hit_Factor", 0.3f);
	master_difficulty_object_hit_factor = READ_IF_EXISTS(pSettings, r_float, section, "Master_Difficulty_Throwed_Object_Hit_Factor", 0.4f);

	clamp(novice_difficulty_object_hit_factor, 0.f, 1.f);
	clamp(stalker_difficulty_object_hit_factor, 0.f, 1.f);
	clamp(veteran_difficulty_object_hit_factor, 0.f, 1.f);
	clamp(master_difficulty_object_hit_factor, 0.f, 1.f);
	
	Sound->create(sound_tele_hold, pSettings->r_string(section, "sound_tele_hold"), st_Effect, SOUND_TYPE_WORLD);
	Sound->create(sound_tele_throw, pSettings->r_string(section, "sound_tele_throw"), st_Effect, SOUND_TYPE_WORLD);

	m_state = ETeleState::WAIT;
	m_state_start_time = 0;
	m_state_next_update = 0;
}

void CTelekineticPoltergeist::update_schedule()
{
	inherited::update_schedule();
	
	const CEntityAlive* enemy = poltergeist->EnemyMan.get_enemy();

	if (!enemy)
	{
		return;
	}

	if (poltergeist->get_actor_ignore() || enemy->Position().distance_to(poltergeist->Position()) > distance)
	{
		return;
	}

	const Fvector enemy_pos = enemy->Position();
	const float distance_to_enemy = enemy_pos.distance_to(poltergeist->Position());

	if (distance_to_enemy > distance)
	{
		return;
	}

	if (poltergeist->get_actor_ignore())
	{
		return;
	}

	switch (m_state)
	{
		case ETeleState::RAISE_OBJECTS:
			if (m_state_start_time + m_state_next_update < time())
			{
				if (!tele_raise_objects())
				{
					m_state = ETeleState::MAIN_PHASE;
				}

				m_state_start_time = time();
				m_state_next_update = raise_time_to_wait_in_objects / 2 +
									  Random.randI(raise_time_to_wait_in_objects / 2);
			}

			if (m_state == ETeleState::RAISE_OBJECTS)
			{
				if (poltergeist->get_controlled_objects_count() >= object_count)
				{
					m_state_start_time = time();
					m_state = ETeleState::MAIN_PHASE;
				}
			}
			break;

			// Главная фаза телекинеза полтера: стрельба + бросаемся предметами.
		case ETeleState::MAIN_PHASE:
			if (m_state_start_time + time_to_hold > time() &&
				m_state_start_time + m_state_next_update > time())
			{
				break;
			}

			throw_objects(); // подержали m_pmt_time_to_hold + m_state_next_update и кидаемся объектами.

			m_state_start_time = time();
			m_state_next_update = time_to_wait_in_objects / 2 + Random.randI(time_to_wait_in_objects / 2);

			if (poltergeist->get_controlled_objects_count() <= 0)
			{
				m_state_start_time = time();
				m_state = ETeleState::WAIT;
			}

			// Отстрелялись, откидались, ждём три секунды, перед тем как вновь поднимать предметы.
		case ETeleState::WAIT:
			if (m_state_start_time + time_to_wait < time())
			{
				m_state_next_update = 0;
				m_state_start_time = time();
				m_state = ETeleState::RAISE_OBJECTS;
			}
			break;
	}
}

void CTelekineticPoltergeist::update_frame()
{
	inherited::update_frame();
}

void CTelekineticPoltergeist::UpdateCL()
{

}

void CTelekineticPoltergeist::tele_find_objects(xr_vector<CObject*>& objects, const Fvector& pos)
{
	objects.clear();
	g_SpatialSpace->q_sphere(nearest_objects, 0, ESPATIAL_TYPE::COLLIDEABLE, pos, radius);

	for (ISpatialShared& SS : nearest_objects)
	{
		ISpatial* S = SS.get();
		if (!S)
		{
			continue;
		}

		CObject* pObject = S->dcast_CObject();
		if (!pObject || pObject->getDestroy())
		{
			continue;
		}

		CPhysicsShellHolder* obj = pObject->cast_physics_shell_holder();
		CMonsterEnemyManager& enemy = this->poltergeist->EnemyMan;

		if (!obj ||
			!obj->PPhysicsShell() ||
			!obj->PPhysicsShell()->isActive() ||
			obj->cast_creature() ||
			(obj->spawn_ini() && obj->spawn_ini()->section_exist("ph_heavy")) ||
			obj->m_pPhysicsShell->getMass() < object_min_mass ||
			obj->m_pPhysicsShell->getMass() > object_max_mass ||
			obj == poltergeist ||
			poltergeist->is_active_object(obj) ||
			!obj->m_pPhysicsShell->get_ApplyByGravity() || !enemy.get_enemy())
		{
			continue;
		}

		Fvector center;
		enemy.get_enemy()->Center(center);

		CEntityAlive* Enemy = const_cast<CEntityAlive*>(enemy.get_enemy());
		CObject* Object = Enemy->dcast_CObject();

		if (trace_object(obj, center) || trace_object(obj, get_head_position(Object)))
		{
			objects.push_back(obj);
		}
	}
}

bool CTelekineticPoltergeist::tele_raise_objects()
{
	// find objects near enemy
	xr_vector<CObject*>& tele_objects = poltergeist->tele_objects;
	const CEntityAlive* enemy = this->poltergeist->EnemyMan.get_enemy();

	// получить список объектов вокруг врага
	tele_find_objects(tele_objects, enemy->Position());
	// получить список объектов вокруг монстра
	tele_find_objects(tele_objects, poltergeist->Position());

	// получить список объектов между монстром и врагом
	float dist = enemy->Position().distance_to(poltergeist->Position());

	Fvector dir;
	dir.sub(enemy->Position(), poltergeist->Position());
	dir.normalize();

	Fvector pos;
	pos.mad(poltergeist->Position(), dir, dist / 2.f);
	tele_find_objects(tele_objects, pos);

	// сортировать и оставить только необходимое количество объектов
	std::ranges::sort(tele_objects, best_object_predicate2(poltergeist->Position(), enemy->Position()));
	// оставить уникальные объекты
	tele_objects.erase(std::ranges::unique(tele_objects).begin(), tele_objects.end());

	if (tele_objects.empty())
	{
		return false;
	}

	CPhysicsShellHolder* physics_object = tele_objects[0] != nullptr ? tele_objects[0]->cast_physics_shell_holder() : nullptr;
	bool rotate = false;

	STelekineticObject* tele_obj;

	STelekineticObjectParams tele_object_params
	{
		.telekinesis = this->poltergeist->telekinesis(),
		.object = physics_object,
		.strength = raise_speed,
		.target_height = object_height,
		.time_to_keep = time_object_keep,
		.rotate_object = rotate,
		.novice_difficulty_object_hit_factor = novice_difficulty_object_hit_factor,
		.stalker_difficulty_object_hit_factor = stalker_difficulty_object_hit_factor,
		.veteran_difficulty_object_hit_factor = veteran_difficulty_object_hit_factor,
		.master_difficulty_object_hit_factor = master_difficulty_object_hit_factor,
	};

	if (physics_object->cast_weapon_magazined() && shooting_from_weapon_enable)
	{
		size_t weapons_count = std::ranges::count_if(poltergeist->get_tele_objects(), [](STelekineticObject* tele_object)
											 { return tele_object->cast_telekinetic_weapon_object(); });

		if (weapons_count >= max_pickuped_weapons)
		{
			return false;
		}

		STelekineticWeaponParams weapon_params
		{
			.telekinetic_enemy = poltergeist,
			.delay_before_first_shot = delay_before_first_shot,

			.novice_difficulty_angular_speed = novice_difficulty_angular_speed,
			.stalker_difficulty_angular_speed = stalker_difficulty_angular_speed,
			.veteran_difficulty_angular_speed = veteran_difficulty_angular_speed,
			.master_difficulty_angular_speed = master_difficulty_angular_speed,

			.novice_difficulty_error_angle = novice_difficulty_error_angle,
			.stalker_difficulty_error_angle = stalker_difficulty_error_angle,
			.veteran_difficulty_error_angle = veteran_difficulty_error_angle,
			.master_difficulty_error_angle = master_difficulty_error_angle
		};
		
		tele_obj = new STelekineticWeaponObject(weapon_params, tele_object_params);
	}
	else if (physics_object->cast_grenade() && activate_n_throw_grenade)
	{
		tele_obj = new STelekineticGrenadeObject(poltergeist, tele_object_params);
	}
	else
	{
		tele_obj = new STelekineticObject(tele_object_params);
	}

	if (!tele_obj->can_be_picked_up())
	{
		xr_delete(tele_obj);
		return false;
	}

	poltergeist->CTelekinesis::append_tobject(tele_obj);
	tele_obj->set_sound(sound_tele_hold, sound_tele_throw);
	tele_obj->set_particle(particle_tele_object);
	tele_obj->start_object_particles();

	return true;
}

bool CTelekineticPoltergeist::trace_object(CObject* obj, const Fvector& target)
{
	Fvector trace_from;
	obj->Center(trace_from);

	Fvector dir;
	dir.sub(target, trace_from);

	float range = dir.magnitude();

	if (range < EPS)
	{
		return false;
	}

	dir.normalize();

	collide::rq_result rq_result;

	if (Level().ObjectSpace.RayPick(trace_from, dir, range, collide::rqtBoth, rq_result, obj))
	{
		CObject* raypicked_object = rq_result.O;
		const CEntityAlive* our_enemy = this->poltergeist->EnemyMan.get_enemy();

		if (raypicked_object == our_enemy)
		{
			return true;
		}
	}
	return false;
}

void CTelekineticPoltergeist::throw_objects()
{
	const CEntityAlive* enemy = this->poltergeist->EnemyMan.get_enemy();

	for (STelekineticObject* tele_object : poltergeist->telekinetic_objects)
	{
		if (tele_object->get_state() == ETelekineticState::TS_KEEP)
		{
			CEntityAlive* Enemy = const_cast<CEntityAlive*>(enemy);
			CObject* Object = Enemy->dcast_CObject();
			Fvector enemy_head = get_head_position(Object);

			if (tele_object->can_be_thrown() && trace_object(tele_object->get_object(), enemy_head))
			{
				poltergeist->throw_object_time(
					tele_object->get_object(),
					enemy_head,
					tele_object->get_object()->Position().distance_to(enemy_head) / fly_velocity
				);
			}
		}
	}
}