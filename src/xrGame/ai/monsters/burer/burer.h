#pragma once
#include "../basemonster/base_monster.h"
#include "../telekinesis.h"
#include "../anim_triple.h"
#include "../scanning_ability.h"
#include "../../../../xrScripts/script_export_space.h"

class CCharacterPhysicsSupport;
class CBurerFastGravi;

class CBurer final : public CBaseMonster, public CTelekinesis, public ITelekineticEnemy
{
public:
	using inherited = CBaseMonster;
	using TTelekinesis = CTelekinesis;

	xr_vector<ISpatialShared> nearest_objects;
	static bool can_scan;
	u32 last_hit_frame;
	u32 time_last_scan;

	struct GraviObject
	{
		bool active;
		Fvector cur_pos;
		Fvector target_pos;
		Fvector from_pos;

		u32 time_last_update;
		const CEntityAlive* enemy;

		GraviObject()
		{
			active = false;
			enemy = nullptr;
		}

		void activate(const CEntityAlive* e, const Fvector& cp, const Fvector& tp)
		{
			active = true;
			from_pos = cp;
			cur_pos = cp;
			target_pos = tp;
			time_last_update = Device.dwTimeGlobal;
			enemy = e;
		}

		void deactivate()
		{
			active = false;
		}
	} gravi_object;

	const char* particle_gravi_wave;
	const char* particle_gravi_prepare;
	shared_str particle_tele_object;

	ref_sound sound_gravi_wave;
	ref_sound sound_scan;

	ref_sound sound_tele_hold;
	ref_sound sound_tele_throw;

	enum EBurerSounds
	{
		eAdditionalSounds = MonsterSound::eMonsterSoundCustom,
		eMonsterSoundGraviAttack = eAdditionalSounds | 0,
		eMonsterSoundTeleAttack = eAdditionalSounds | 1,
	};

	struct gravi_params
	{
		float speed;
		u32 cooldown;
		float min_dist;
		float max_dist;
		float step;
		TTime time_to_hold;
		float radius;
		float impulse_to_objects;
		float impulse_to_enemy;
		float hit_power;
	} gravi;

	u32 tele_max_handled_objects;
	u32 tele_time_to_hold;
	u32 tele_max_time;

	float tele_object_min_mass;
	float tele_object_max_mass;
	float tele_find_radius;
	float tele_min_distance;
	float tele_max_distance;
	float tele_raise_speed;
	float tele_fly_velocity;
	float tele_object_height;

	float weight_to_stamina_hit;
	float weapon_drop_stamina_k;

	float runaway_distance;
	float normal_distance;
	TTime max_runaway_time;
	float weapon_drop_velocity;
	TTime shield_cooldown;
	TTime shield_time;
	TTime m_shield_expire_time;
	bool m_shield_active;
	const char* shield_keep_particle;
	TTime shield_keep_particle_period;
	float shield_penetration_border = 4.0f;
	float shield_penetration_damage_coeff = 0.25f;
	const char* particle_fire_shield;

	CBurerFastGravi* fast_gravi;
	bool use_three_gravi_anims;

	bool shooting_from_weapon_enable;
	bool activate_n_throw_grenade;
	u32 max_pickuped_weapons;
	u32 delay_before_first_shot;

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

	CBurer();
	~CBurer() override;


	void reinit() override;
	void reload(const char* section) override;

	void Load(const char* section) override;
	void PostLoad(const char* section) override;

	void net_Destroy() override;
	void net_Relcase(CObject* O) override;
	void shedule_Update(u32 dt) override;
	void UpdateCL() override;
	void Hit(SHit* pHDS) override;
	void Die(CObject* who) override;
	void CheckSpecParams(u32 spec_params) override;

	void OnEvent(NET_Packet& P, u16 type) override;
	void StartGraviMP();
	void shieldParticlesMP();

	void UpdateGraviObject();
	void UpdateGraviObjectCL();

	void StartGraviPrepare();
	void StopGraviPrepare();

	void ActivateShield();
	void DeactivateShield();

	bool need_shotmark() const override { return !m_shield_active; }

	bool ability_distant_feel() override { return true; }
	char* get_monster_class_name() override { return (char*)"burer"; }

	CEntityAlive* get_enemy() override;
	float get_tele_distance() override;
	u32 get_tele_keep_time() override;
	CBaseMonster* get_self() override;

#ifdef DEBUG
	virtual CBaseMonster::SDebugInfo show_debug_info();
#endif

	void set_force_gravi_attack(bool force_gravi) { m_force_gravi_attack = force_gravi; }
	bool get_force_gravi_attack() const { return m_force_gravi_attack; }

	bool m_force_gravi_attack;

	void StaminaHit();

	DECLARE_SCRIPT_REGISTER_FUNCTION

	void face_enemy();
};

bool actor_is_reloading_weapon();