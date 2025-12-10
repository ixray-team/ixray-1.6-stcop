#pragma once
#include "../abilities/poltergeist/PolterInterface.h"
#include "../BaseMonster/base_monster.h"
#include "../xrGame/ai/monsters/pseudogigant/pseudo_gigant.h"
#include "../telekinesis.h"
#include "../../../../xrGame/Hit.h"
#include "../xrScripts/script_export_space.h"

//#define SCRIPT_REGISTRATOR

class CPolterSpecialAbility;

class CAnomalPseudoGigant final : public CPseudoGigant,
							public CTelekinesis,
							public IPolterInterface {
							//	{

	typedef		CPseudoGigant						inherited;

	bool					m_actor_ignore;

	float					m_current_detection_level;
	float					m_detection_success_level;
	float					m_detection_max_level;

	float m_flame_max_hp_to_activate = 0.0f;
	float m_tele_max_hp_to_activate = 0.0f;
	float m_chem_max_hp_to_activate = 0.0f;

						public:
	CAnomalPseudoGigant();
							~CAnomalPseudoGigant() override;

							void	Load(LPCSTR section) override;
							void	reinit() override;

							void	UpdateCL() override;
							void	shedule_Update(u32 dt) override;
							void	Hit(SHit* pHDS) override;

							bool	check_start_conditions(ControlCom::EControlType type) override;

							char* get_monster_class_name() override { return (char*)"anomalpseudogigant"; }

	void	set_actor_ignore(bool const actor_ignore) { m_actor_ignore = actor_ignore; } // can be used for diabling abilities when no combat
	bool	get_actor_ignore() const { return m_actor_ignore; }

	// snork jump

							void	HitEntityInJump(const CEntity* pEntity) override;

	bool	find_geometry(Fvector& dir);
	float	trace(const Fvector& dir);

	bool	trace_geometry(const Fvector& d, float& range);

							void	jump(const Fvector& position, float factor) override;
							bool	ability_jump_over_physics() override { return true; }



	// begin IPolterInterface
	virtual CBaseMonster* GetMonster() override { return this; }
	virtual CTelekinesis* GetTelekinesis() override { return this; }
	virtual const Fvector& GetCurrentPosition() override { return Position(); }
	virtual float GetTargetHeight() override { return 0.0f; }
	virtual float GetCurrentDetectionLevel() override { return get_current_detection_level(); }
	virtual float GetDetectionSuccessLevel() override { return get_detection_success_level(); }
	virtual bool GetActorIgnore() override { return get_actor_ignore(); }
	virtual xr_vector<CObject*>& GetTeleObjects() override { return tele_objects; }
	// end IPolterInterface

private:

	CPolterSpecialAbility* m_flame = nullptr;
	CPolterSpecialAbility* m_tele = nullptr;
	CPolterSpecialAbility* m_chem = nullptr;
	xr_vector<CObject*> tele_objects = {};

public:

	void	PhysicalImpulse(const Fvector& position);
	void	StrangeSounds(const Fvector& position);

	float	get_current_detection_level() const { return m_current_detection_level; }

	float	get_detection_success_level();

	//IC		CAnomalGigPolterSpecialAbility* ability() { return (m_flame ? m_flame : m_tele); } // remake: tele on 66% hp, flame +tele on 33% hp
	bool use_tele_ability();
	bool use_fire_ability();
	bool use_chem_ability();

	// burer shield

	void	ActivateShield();
	void	DeactivateShield();

	TTime	m_shield_cooldown;
	TTime	m_shield_time;
	bool	m_shield_active;
	LPCSTR	m_shield_keep_particle;
	TTime	m_shield_keep_particle_period;

	LPCSTR	particle_fire_shield;
	u32		last_hit_frame;

	ref_sound m_strange_sound;

	// delegates on events
public:

	/*virtual void on_shield_on();
	virtual void on_shield_off();
	virtual void on_hit();*/
	virtual void on_jump();

	DECLARE_SCRIPT_REGISTER_FUNCTION

};

bool   actor_is_reloading_weapon();