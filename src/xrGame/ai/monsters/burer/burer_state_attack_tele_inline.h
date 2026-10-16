#pragma once

#include "Grenade.h"
#include "../../../Level.h"

template <typename Object>
CStateBurerAttackTele<Object>::CStateBurerAttackTele(Object* obj) : inherited(obj)
{
	m_anim_end_tick = 0;
}

template <typename Object>
void CStateBurerAttackTele<Object>::initialize()
{
	inherited::initialize();

	m_action = ACTION_TELE_STARTED;
	CompletionReached = false;
	this->object->Combat.BeginTele();
	SelectedId = ALife::INVALID_OBJECT_ID;

	SelectObjects();

	time_started = 0;
	m_anim_end_tick = 0;
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

			if (this->object->CTelekinesis::GetHeldObjectsCount() > 0)
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
void CStateBurerAttackTele<Object>::deactivate(bool Fire)
{
	tele_objects.clear();
	SelectedId = ALife::INVALID_OBJECT_ID;

	for (STelekineticObject* tobject : this->object->CTelekinesis::get_tele_objects())
	{
		CPhysicsShellHolder* const cur_object = tobject->params.object;
		if (!cur_object || !cur_object->m_pPhysicsShell || !cur_object->m_pPhysicsShell->isActive())
		{
			continue;
		}

		tobject->stop_object_particles();
	}

	if (Fire && this->object->g_Alive() && (!this->object->Combat.Enabled || this->object->Combat.CanFireAll(time_started, m_end_tick)))
	{
		FireAllToEnemy();
	}
	this->object->CTelekinesis::deactivate(Fire && this->object->g_Alive());
	this->object->set_script_capture(true);
}

template <typename Object>
void CStateBurerAttackTele<Object>::finalize()
{
	deactivate(true);
	inherited::finalize();
}

template <typename Object>
void CStateBurerAttackTele<Object>::critical_finalize()
{
	deactivate(CompletionReached);
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
	const float Distance = this->object->EnemyMan.GetTrackingPosition().distance_to(this->object->Position());
	CompletionReached = !this->object->EnemyMan.get_enemy() || !this->object->g_Alive() ||
						this->object->Combat.NeedStopTele(*this->object, time_started) ||
						(Distance < this->object->tele_min_distance && !(this->object->Combat.Enabled && this->object->Combat.Context.Grenades)) ||
						Distance > this->object->tele_max_distance || this->object->conditions().GetHealth() < m_initial_health ||
						time() > m_end_tick || m_action == ACTION_COMPLETED;
	return CompletionReached;
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

		if (object->H_Parent() || (grenade && (grenade->IsExploding() || (!this->object->Combat.Enabled && grenade->destroy_time() != UINT32_MAX))) ||
			!obj ||
			!obj->PPhysicsShell() ||
			!obj->PPhysicsShell()->isActive() ||
			custom_monster ||
			(obj->spawn_ini() && obj->spawn_ini()->section_exist("ph_heavy")) ||
			obj->cast_car() ||
			(!grenade && obj->m_pPhysicsShell->getMass() < this->object->tele_object_min_mass) ||
			(!grenade && obj->m_pPhysicsShell->getMass() > this->object->tele_object_max_mass) ||
			(obj == this->object) ||
			this->object->CTelekinesis::is_active_object(obj) ||
			!obj->m_pPhysicsShell->get_ApplyByGravity() ||
			!this->object->Combat.CanTeleObject(*obj))
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
	FindFreeObjects(m_nearest, this->object->EnemyMan.GetTrackingPosition());

	// получить список объектов вокруг монстра
	FindFreeObjects(m_nearest, this->object->Position());

	// получить список объектов между монстром и врагом
	float dist = this->object->EnemyMan.GetTrackingPosition().distance_to(this->object->Position());
	Fvector dir;
	dir.sub(this->object->EnemyMan.GetTrackingPosition(), this->object->Position());
	dir.normalize_safe();

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
		bool Grenade = telekinetic_object->cast_telekinetic_grenade_object() != nullptr;
		ETelekineticState object_state = telekinetic_object->get_state();

		if (object_state != ETelekineticState::TS_KEEP && !(Grenade && object_state == ETelekineticState::TS_RAISE))
		{
			continue;
		}

		CPhysicsShellHolder* object = telekinetic_object->get_object();

		if (object == nullptr || object->getDestroy() || object->H_Parent())
		{
			continue;
		}
		if (!telekinetic_object->can_be_thrown() || (this->object->Combat.Enabled && !Grenade && (!this->object->Combat.TeleAllowed() || Random.randF() >= this->object->Combat.FireAllProbability)))
		{
			continue;
		}

		float const dist_to_enemy = object->Position().distance_to(enemy_pos);
		float const fire_time = std::max(.1f, dist_to_enemy / std::max(.1f, this->object->tele_fly_velocity));
		if (auto* Gren = object->cast_grenade(); Gren && this->object->Combat.Enabled && Gren->destroy_time() != u32(-1))
		{
			Gren->set_destroy_time_now(std::min(Gren->destroy_time(), Device.dwTimeGlobal + std::max(1u, this->object->Combat.CriticalGrenadeTimer) - 1));
		}

		this->object->CTelekinesis::throw_object_time(object, enemy_pos, fire_time);
		this->object->sound().play(CBurer::eMonsterSoundTeleAttack);
	}
}

template <typename Object>
void CStateBurerAttackTele<Object>::ExecuteTeleContinue()
{
	if (!IsActiveObjects() || (time_started && Device.dwTimeGlobal - time_started > 6000))
	{
		m_action = ACTION_COMPLETED;
		return;
	}
	if (!this->object->EnemyMan.see_enemy_now() && !(this->object->Combat.Enabled && this->object->Combat.Context.Grenades))
	{
		return;
	}

	STelekineticObject* Candidate = nullptr;
	STelekineticObject* GrenadeCandidate = nullptr;
	for (STelekineticObject* Held : this->object->CTelekinesis::get_tele_objects())
	{
		auto* Item = Held->get_object();
		if (!Item || Item->getDestroy() || Item->H_Parent() || Held->get_state() != ETelekineticState::TS_KEEP || !Held->can_be_thrown())
		{
			continue;
		}
		if (!Candidate)
		{
			Candidate = Held;
		}
		if (Held->cast_telekinetic_grenade_object() && Device.dwTimeGlobal - Held->time_keep_started > 100)
		{
			if (!GrenadeCandidate || Item->cast_grenade()->destroy_time() < GrenadeCandidate->get_object()->cast_grenade()->destroy_time())
			{
				GrenadeCandidate = Held;
			}
		}
	}
	if (this->object->Combat.Enabled && GrenadeCandidate && Random.randF() < .9f)
	{
		Candidate = GrenadeCandidate;
	}
	if (Candidate)
	{
		SelectedId = Candidate->get_object()->ID();
		m_action = ACTION_TELE_FIRE;
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

	auto* Found = Level().Objects.net_Find(SelectedId);
	auto* Selected = Found ? Found->cast_physics_shell_holder() : nullptr;
	SelectedId = ALife::INVALID_OBJECT_ID;
	if (!Selected || Selected->getDestroy() || Selected->H_Parent())
	{
		return;
	}
	const auto& HeldObjects = this->object->CTelekinesis::get_tele_objects();
	auto Held = std::find_if(HeldObjects.begin(), HeldObjects.end(), [Selected](STelekineticObject* Item)
							 { return Item->get_object() == Selected; });
	if (Held == HeldObjects.end() || (*Held)->get_state() != ETelekineticState::TS_KEEP || !(*Held)->can_be_thrown())
	{
		return;
	}
	const float dist_to_enemy = Selected->Position().distance_to(enemy_pos);
	const float fire_time = std::max(.1f, dist_to_enemy / std::max(.1f, this->object->tele_fly_velocity));
	if (auto* Grenade = Selected->cast_grenade(); Grenade && this->object->Combat.Enabled && Grenade->destroy_time() != u32(-1))
	{
		Grenade->set_destroy_time_now(std::min(Grenade->destroy_time(), Device.dwTimeGlobal + std::max(1u, this->object->Combat.CriticalGrenadeTimer) - 1));
	}

	this->object->CTelekinesis::throw_object_time(Selected, enemy_pos, fire_time);
	this->object->sound().play(CBurer::eMonsterSoundTeleAttack);
}

template <typename Object>
bool CStateBurerAttackTele<Object>::IsActiveObjects()
{
	return this->object->CTelekinesis::GetHeldObjectsCount() > 0;
}

template <typename Object>
bool CStateBurerAttackTele<Object>::CheckTeleStart()
{
	this->object->Combat.Refresh(*this->object);
	// проверка на текущую активность
	if (IsActiveObjects())
	{
		return false;
	}

	// проверить дистанцию до врага
	float dist = this->object->Position().distance_to(this->object->EnemyMan.GetTrackingPosition());
	if (dist < this->object->tele_min_distance && !(this->object->Combat.Enabled && this->object->Combat.Context.Grenades))
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
	return this->object->Combat.TeleAllowed();
}

//////////////////////////////////////////////////////////////////////////
// Выбор подходящих объектов для телекинеза
//////////////////////////////////////////////////////////////////////////
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
		tele_objects.begin(), tele_objects.end(), [this](CPhysicsShellHolder* Left, CPhysicsShellHolder* Right)
		{
			auto* LeftGrenade = Left->cast_grenade();
			auto* RightGrenade = Right->cast_grenade();
			const u32 LeftTimer = LeftGrenade ? LeftGrenade->destroy_time() : u32(-1);
			const u32 RightTimer = RightGrenade ? RightGrenade->destroy_time() : u32(-1);
			if (LeftTimer != RightTimer)
			{
				return LeftTimer < RightTimer;
			}
			return Left->Position().distance_to_sqr(this->object->EnemyMan.GetTrackingPosition()) < Right->Position().distance_to_sqr(this->object->EnemyMan.GetTrackingPosition()); });

	size_t Weapons = std::count_if(this->object->CTelekinesis::get_tele_objects().begin(), this->object->CTelekinesis::get_tele_objects().end(), [](STelekineticObject* Held)
								   { return Held->cast_telekinetic_weapon_object() != nullptr; });
	for (u32 i = 0; i < tele_objects.size() && this->object->CTelekinesis::GetHeldObjectsCount() < max; ++i)
	{
		CPhysicsShellHolder* object = tele_objects[i];
		if (!object || object->getDestroy() || object->H_Parent() || !object->PPhysicsShell() || !object->PPhysicsShell()->isActive() || !this->object->Combat.CanTeleObject(*object))
		{
			continue;
		}

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
			if (Weapons >= this->object->max_pickuped_weapons)
			{
				continue;
			}

			STelekineticWeaponParams weapon_params
			{
				.telekinetic_enemy = this->object,

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
		else if (object->cast_grenade() && (this->object->activate_n_throw_grenade || (this->object->Combat.Enabled && object->cast_grenade()->destroy_time() != u32(-1))))
		{
			auto* Grenade = new STelekineticGrenadeObject(this->object, tele_object_params);
			Grenade->ActivateInactive = this->object->activate_n_throw_grenade;
			if (this->object->Combat.Enabled)
			{
				Grenade->EmergencyTime = this->object->Combat.CriticalGrenadeTimer;
				Grenade->throw_threshold = 100;
			}
			tele_obj = Grenade;
		}
		else
		{
			tele_obj = new STelekineticObject(tele_object_params);
		}

		if (!tele_obj->can_be_picked_up())
		{
			xr_delete(tele_obj);
			continue;
		}

		this->object->CTelekinesis::append_tobject(tele_obj);
		if (tele_obj->cast_telekinetic_weapon_object())
		{
			++Weapons;
		}
		tele_obj->set_sound(this->object->sound_tele_hold, this->object->sound_tele_throw);
		tele_obj->set_particle(this->object->particle_tele_object);
		tele_obj->start_object_particles();
	}
}
