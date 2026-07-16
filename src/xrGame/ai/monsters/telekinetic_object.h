#pragma once
#include "../xrPhysics/ExtendedGeom.h"

class CGameObject;
class CPhysicsShellHolder;
class CPHUpdateObject;
class CTelekinesis;
class CWeaponMagazined;
class CGrenade;
class CPoltergeist;
class ITelekineticEnemy;
struct STelekineticObject;
struct CTeleWhirlwindObject;
struct STelekineticWeaponObject;
struct STelekineticGrenadeObject;

enum class ETelekineticState : u8
{
	TS_NONE,
	TS_RAISE,
	TS_KEEP,
	TS_THROW,
};

enum ETelekineticTimings : u16
{
	KEEP_IMPULSE_UPDATE = 200,
	DELAY_AFTER_THROW = 3000,
	RAISE_MAX_TIME = 5000
};

struct STelekineticObject
{
	ETelekineticState state;

	CPhysicsShellHolder* object;
	ref_sound sound_hold;
	ref_sound sound_throw;
	shared_str particle_sect;

	float target_height;
	float strength;

	// Objects
	u32 time_raise_started;
	u32 time_keep_started;
	u32 time_keep_updated;
	u32 time_to_keep;
	u32 time_throw_started;

	bool rotate_object;

	STelekineticObject(CPhysicsShellHolder* owner, float s, float h, u32 ttk, bool rot);
	virtual ~STelekineticObject() {}

	virtual void set_sound(const ref_sound& snd_hold, const ref_sound& snd_throw);
	virtual void set_particle(shared_str& particles_sect);

	virtual void start_object_particles();
	virtual void stop_object_particles();

	virtual void raise(float step);
	virtual void raise_update();

	virtual void prepare_keep();
	virtual void perform_keep_object();
	virtual void keep_update();
	virtual void release();
	virtual void throw_object(const Fvector& target, float power);
	virtual void throw_object_time(const Fvector& target, float time);
	virtual void throw_update();
	virtual void update_state();
	static  void collision_callback(bool& do_colide,bool bo1,dContact& c,SGameMtl* material_1,SGameMtl* material_2);
	ICF virtual bool is_released() const { return state == ETelekineticState::TS_NONE; }
	virtual void switch_state(ETelekineticState new_state);
	ICF virtual ETelekineticState get_state() const { return state; }
	ICF virtual CPhysicsShellHolder* get_object() const { return object; }

	virtual bool check_height() const;
	virtual bool check_raise_time_out() const;

	virtual bool keep_time_elapsed() const;
	virtual bool throw_time_elapsed() const;

	void enable() const;

	ICF bool operator==(const CPhysicsShellHolder* obj) const
	{
		return object == obj;
	}

	void rotate() const;
	void update_hold_sound();

	virtual bool can_be_thrown() { return true; }
	virtual bool can_be_picked_up() { return true; }

	virtual STelekineticObject* cast_telekinetic_object() { return this; }
	virtual STelekineticWeaponObject* cast_telekinetic_weapon_object() { return nullptr; }
	virtual STelekineticGrenadeObject* cast_telekinetic_grenade_object() { return nullptr; }
	virtual CTeleWhirlwindObject* cast_whirlwind_object() { return nullptr; }
};

struct STelekineticWeaponParams
{
	u32 delay_before_first_shot;

	// Угловая скорость наведения оружия на врага. Указывается в градусах/с.
	// Нужно чтобы оружие не наводилось турель-like. Хотя, вы можете это сделлать, проставив в конфигах 360 для всех сложностей.
	f32 novice_difficulty_angular_speed;
	f32 stalker_difficulty_angular_speed;
	f32 veteran_difficulty_angular_speed;
	f32 master_difficulty_angular_speed;

	// Погрешность наведения на врага, после которой можно стрелять. Указывается в градусах/с (1..180)
	// Чем ниже сложность - тем больше погрешность для начала стрельбы - тем больше будет потрачено патрон.
	// Настраивается вместе с параметрами %difficulty%_Angular_Speed, чтобы при низких уровнях сложности - оружие в случае идеального наведения на модель врага.
	// в конфиге 1 = оружие начнёт стрелять только в случае идеального наведения в центр модели врага.
	// в конфиге 180 = оружие начинает стрелять, даже когда смотрит в противоположну сторону, относительно врага.
	f32 novice_difficulty_error_angle;
	f32 stalker_difficulty_error_angle;
	f32 veteran_difficulty_error_angle;
	f32 master_difficulty_error_angle;
};

struct STelekineticWeaponObject : STelekineticObject
{
	using inherited = STelekineticObject;

	ITelekineticEnemy* telekinetic_enemy;
	// Внешие параметры, приходит от CBurer || CTelePoltergeist
	STelekineticWeaponParams weapon_params;
	CWeaponMagazined* weapon;

	u32 weapon_phase_start_time; // Когда оружие начало/перестало стрелять.
	u32 weapon_next_phase_time;	 // Когда оружию перестать/начать стрелять.

	float backup_weapon_dispersion = 9999.f;
	u32 first_shot_delay_ms = 0;
	s8 backup_weapon_fire_mode = s8(-1);

	STelekineticWeaponObject(ITelekineticEnemy* tele_enemy, STelekineticWeaponParams& weapon_params, CPhysicsShellHolder* owner, float s, float h, u32 ttk, bool rot);

	void setup_local_weapon_things();
	void restore_global_weapon_things();

#ifdef DEBUG_DRAW
	void debug_draw();
#endif
	void update_auto_aim();
	bool can_shoot();
	void try_shoot();
	void weapon_start_shooting(u32 shoot_time);
	void weapon_end_shooting(u32 pause_time = 0);

	/**
	 * @param threshold погрешность наведения в градусах, после которой можно стрелять. Указывается в градусах.
	 * По умолчанию - 10 градусов.
	 */
	bool is_enemy_tracing(float threshold = 10.f);
	void perform_keep_object() override;
	bool can_be_thrown() override;
	void release() override;
	void switch_state(ETelekineticState new_state) override;

	STelekineticWeaponObject* cast_telekinetic_weapon_object() override { return this; }
};

struct STelekineticGrenadeObject : STelekineticObject
{
	using inherited = STelekineticObject;

	ITelekineticEnemy* telekinetic_enemy;
	CGrenade* grenade;

	u32 grenade_initial_time = 0xffffffffui32;
	u32 throw_threshold = 700;
	u32 time_to_explode = 2000;

	STelekineticGrenadeObject(ITelekineticEnemy* tele_enemy, CPhysicsShellHolder* owner, float s, float h, u32 ttk, bool rot);
#ifdef DEBUG_DRAW
	void debug_draw();
#endif
	void perform_keep_object() override;
	void switch_state(ETelekineticState new_state) override;
	bool can_be_thrown() override;
	bool can_be_picked_up() override;

	STelekineticGrenadeObject* cast_telekinetic_grenade_object() override { return this; }
};