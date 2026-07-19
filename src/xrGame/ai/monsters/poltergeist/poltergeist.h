#pragma once
#include "../basemonster/base_monster.h"
#include "../telekinesis.h"
#include "../energy_holder.h"
#include "../../../../xrScripts/script_export_space.h"
#include "Grenade.h"

class CPhysicsShellHolder;
class CStateManagerPoltergeist;
class CPoltergeisMovementManager;
class IPolter;
class CTelekineticPoltergeist;
class CWeaponMagazined;
class CGrenade;

class CPoltergeist final : public CBaseMonster, public CTelekinesis, public CEnergyHolder, public ITelekineticEnemy
{
	using inherited = CBaseMonster;
	using Energy = CEnergyHolder;

	friend class CPoltergeisMovementManager;
	friend class CTelekineticPoltergeist;

	static constexpr f32 IMPULSE = 10.0f;
	static constexpr f32 IMPULSE_RADIUS = 5.0f;
	static constexpr f32 TRACE_DISTANCE = 10.0f;
	static constexpr u32 TRACE_ATTEMPT_COUNT = 3;

	float m_height;
	bool m_disable_hide;

	SMotionVel invisible_vel;
	IPolter* m_poltergeist;

	xr_vector<CObject*> tele_objects;
	bool m_actor_ignore;

	TTime m_last_detection_time;
	Fvector m_last_actor_pos;
	const char* m_detection_pp_effector_name;
	u32 m_detection_pp_type_index;
	float m_detection_near_range_factor;
	float m_detection_far_range_factor;
	float m_detection_far_range;
	float m_detection_speed_factor;
	float m_detection_loose_speed;
	float m_current_detection_level;
	float m_detection_success_level;
	float m_detection_max_level;

	bool m_enable_corpse_on_death;

public:
	bool m_detect_without_sight;

	CPoltergeist();
	~CPoltergeist() override;

	void Load(const char* section) override;
	void reload(const char* section) override;
	void reinit() override;

	bool net_Spawn(CSE_Abstract* DC) override;
	void net_Destroy() override;
	void net_Relcase(CObject* O) override;

	void UpdateCL() override;
	void shedule_Update(u32 dt) override;
	bool AlwaysTheCrow() override;

	void set_actor_ignore(const bool actor_ignore) { m_actor_ignore = actor_ignore; }
	bool get_actor_ignore() const { return m_actor_ignore; }

	void Die(CObject* who) override;

	CMovementManager* create_movement_manager() override;

	void ForceFinalAnimation() override;

	void on_activate() override;
	void on_deactivate() override;
	void Hit(SHit* pHDS) override;
	char* get_monster_class_name() override { return (char*)"poltergeist"; }

	bool detected_enemy();
	float get_fly_around_distance() const { return m_fly_around_distance; }
	float get_fly_around_change_direction_time() const { return m_fly_around_change_direction_time; }
	void renderable_Render() override;

	ICF IPolter* ability() { return m_poltergeist; }
	ICF bool is_hidden() { return state_invisible; }

	// Poltergeist ability
	void PhysicalImpulse(const Fvector& position);
	void StrangeSounds(const Fvector& position);

	ref_sound m_strange_sound;

	// Movement
	Fvector m_current_position; // Позиция на ноде

	// Dynamic Height
	u32 time_height_updated;
	float target_height;

	void UpdateHeight();

	// Invisibility
	void EnableHide() { m_disable_hide = false; }
	void DisableHide() { m_disable_hide = true; }

	CEntityAlive* get_enemy() override
	{
		const CEntityAlive* entity_alive = EnemyMan.get_enemy();
		return entity_alive ? const_cast<CEntityAlive*>(entity_alive) : nullptr;
	}

	float get_tele_distance() override;
	u32 get_tele_keep_time() override;
	CBaseMonster* get_self() override;

	bool run_home_point_when_enemy_inaccessible() const override { return false; }

private:
	void Hide();
	void Show();

	float m_height_change_velocity;
	u32 m_height_change_min_time;
	u32 m_height_change_max_time;
	float m_height_min;
	float m_height_max;

	float m_fly_around_level;
	float m_fly_around_distance;
	float m_fly_around_change_direction_time;

	float get_current_detection_level() const { return m_current_detection_level; }
	bool check_work_condition() const;
	void remove_pp_effector();
	void update_detection();

	float get_detection_near_range_factor();
	float get_detection_far_range_factor();
	float get_detection_loose_speed();
	float get_detection_far_range();
	float get_detection_speed_factor();
	float get_detection_success_level();
	float get_post_process_factor() const;

public:
#ifdef DEBUG
	virtual CBaseMonster::SDebugInfo show_debug_info();
#endif

	friend class CFlamePoltergeist;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

class IPolter
{
	ref_sound m_sound_base;
	CParticlesObject* m_particles_object;
	CParticlesObject* m_particles_object_electro;

	const char* m_particles_hidden;
	const char* m_particles_damage;
	const char* m_particles_death;
	const char* m_particles_idle;

	u32 m_last_hit_frame;

public:
	CPoltergeist* poltergeist;

	IPolter(CPoltergeist* polter);
	virtual ~IPolter();

	virtual void load(const char* section);
	virtual void update_schedule();
	virtual void update_frame();
	virtual void on_hide();
	virtual void on_show();

	virtual void on_destroy()
	{
	}

	virtual void on_die();
	virtual void on_hit(SHit* pHDS);

	virtual void UpdateCL()
	{
	}

	virtual CTelekineticPoltergeist* cast_to_polter_tele() { return nullptr; }
};

class CFlamePoltergeist final : public IPolter
{
	using inherited = IPolter;

	ref_sound m_sound;
	const char* m_particles_prepare;
	const char* m_particles_fire;
	const char* m_particles_stop;
	u32 m_time_fire_delay;
	u32 m_time_fire_play;

	float m_length;
	float m_hit_value;
	u32 m_hit_delay;

	u32 m_count;
	u32 m_delay; // between 2 flames

	u32 m_time_flame_started;

	float m_min_flame_dist;
	float m_max_flame_dist;
	float m_min_flame_height;
	float m_max_flame_height;

	float m_pmt_aura_radius;


	// Scanner
	float m_scan_radius;
	u32 m_scan_delay_min;
	u32 m_scan_delay_max;

	SPPInfo m_scan_effector_info;
	float m_scan_effector_time;
	float m_scan_effector_time_attack;
	float m_scan_effector_time_release;
	ref_sound m_scan_sound;

	bool m_state_scanning;
	u32 m_scan_next_time;

	enum EFlameState
	{
		ePrepare,
		eFire,
		eStop
	};

public:
	struct SFlameElement
	{
		const CObject* target_object;
		Fvector position;
		Fvector target_dir;
		u32 time_started;
		ref_sound sound;
		CParticlesObject* particles_object;
		EFlameState state;
		u32 time_last_hit;
	};

private:
	using FLAME_ELEMS_VEC = xr_vector<SFlameElement*>;
	using FLAME_ELEMS_IT = FLAME_ELEMS_VEC::iterator;

	FLAME_ELEMS_VEC m_flames;

public:
	CFlamePoltergeist(CPoltergeist* polter);
	~CFlamePoltergeist() override;

	void load(const char* section) override;
	void update_schedule() override;
	void on_destroy() override;
	void on_die() override;
	void UpdateCL() override;

private:
	void select_state(SFlameElement* elem, EFlameState state);
	bool get_valid_flame_position(const CObject* target_object, Fvector& res_pos);
	void create_flame(const CObject* target_object);
};

class CTelekineticPoltergeist final : public IPolter
{
public:
	using inherited = IPolter;

	xr_vector<ISpatialShared> nearest_objects;

	ref_sound sound_tele_hold;
	ref_sound sound_tele_throw;

	// external params
	float radius;
	float object_min_mass;
	float object_max_mass;
	float distance;
	float object_height;
	float raise_speed;
	float fly_velocity;
	float object_collision_damage;

	// Максимальное количество объектов, которые полтергейст может одновременно держать в воздухе (телекинез)
	u32 object_count;

	// Сколько времени (мс) полтергейст удерживает все поднятые объекты в "подвешенном" состоянии
	// перед тем, как начать их бросать (фаза MAIN_PHASE / удержание перед атакой)
	u32 time_to_hold;

	// Время паузы / отдыха (мс) после того, как полтергейст выкидал почти все объекты
	// (переход в состояние WAIT -> следующая атака начинается только после этой паузы)
	u32 time_to_wait;

	// Минимальная / базовая задержка (мс) между поднятием двух последовательных объектов
	// во время фазы RAISE_OBJECTS (чем больше — тем медленнее подъём)
	u32 time_to_wait_in_objects;

	// Задержка (мс) между поднятием объектов в фазе RAISE_OBJECTS.
	// За счёт этого объекты поднимаются друг за другом в случайное время.
	u32 raise_time_to_wait_in_objects;
	u32 time_object_keep;

	enum class ETeleState : u8
	{
		RAISE_OBJECTS,
		MAIN_PHASE,
		WAIT
	} m_state;

	u32 m_state_start_time;
	u32 m_state_next_update;

	u32 max_pickuped_weapons;
	float autoaim_torque_factor;
	u32 delay_before_first_shot;
	shared_str particle_tele_object;
	bool shooting_from_weapon_enable;
	bool activate_n_throw_grenade;

	float novice_difficulty_angular_speed;
	float stalker_difficulty_angular_speed;
	float veteran_difficulty_angular_speed;
	float master_difficulty_angular_speed;

	float novice_difficulty_error_angle;
	float stalker_difficulty_error_angle;
	float veteran_difficulty_error_angle;
	float master_difficulty_error_angle;
	
	float novice_difficulty_object_hit_factor;
	float stalker_difficulty_object_hit_factor;
	float veteran_difficulty_object_hit_factor;
	float master_difficulty_object_hit_factor;

	CTelekineticPoltergeist(CPoltergeist* polter);
	~CTelekineticPoltergeist();

	void load(const char* section) override;
	void update_schedule() override;
	void update_frame() override;
	void UpdateCL() override;

	CTelekineticPoltergeist* cast_to_polter_tele() override { return this; }

private:
	void tele_find_objects(xr_vector<CObject*>& objects, const Fvector& pos);
	bool tele_raise_objects();
	void throw_objects();

	bool trace_object(CObject* ignore_object, const Fvector& target);
};