///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../BaseMonster/base_monster.h"
#include "../../ai_monster_bones.h"
#include "../../controlled_entity.h"
#include "../../controlled_actor.h"
#include "../../anim_triple.h"
#include "bloodsucker_ix_alien.h"

class CAI_BloodsuckerIX :
	public CBaseMonster,
	public CControlledActor
{
	using inherited = CBaseMonster;

public:
	CAI_BloodsuckerIX();
	virtual					~CAI_BloodsuckerIX();

	virtual void			reinit();
	virtual	void			reload(LPCSTR section);

	virtual void			UpdateCL();
	virtual void			shedule_Update(u32 dt);
	virtual void			Die(CObject* who);
	virtual BOOL			net_Spawn(CSE_Abstract* DC);
	virtual	void			Load(LPCSTR section);

	virtual	void			CheckSpecParams(u32 spec_params);
	virtual bool			ability_invisibility() { return true; }
	virtual bool			ability_pitch_correction() { return false; }
	virtual	void			post_fsm_update();

	virtual bool			use_center_to_aim() const { return true; }
	virtual bool			check_start_conditions(ControlCom::EControlType);
	virtual void			on_activate_control(ControlCom::EControlType);
	virtual void			HitEntity(const CEntity* pEntity, float fDamage, float impulse, Fvector& dir);

	//--------------------------------------------------------------------
	// Utils
	//--------------------------------------------------------------------
	void			move_actor_cam();

	//--------------------------------------------------------------------
	// Bones
	//--------------------------------------------------------------------
private:
	static	void	    	BoneCallback(CBoneInstance* B);
	void			vfAssignBones();



	bonesManipulation		Bones;

	CBoneInstance* bone_spine;
	CBoneInstance* bone_head;

	//--------------------------------------------------------------------
	// Invisibility
	//--------------------------------------------------------------------
private:
	SMotionVel				invisible_vel;
	LPCSTR					invisible_particle_name;

public:
	void			start_invisible_predator();
	void			stop_invisible_predator();
	u32				threaten_time() { return m_threaten_time; }

	//--------------------------------------------------------------------
	// Vampire
	//--------------------------------------------------------------------
public:
	u32 m_vampire_min_delay;
	static u32 m_time_last_vampire;
	SAnimationTripleData anim_triple_vampire;
	SPPInfo pp_vampire_effector;
	void ActivateVampireEffector();
	bool WantVampire();
	void SatisfyVampire();
private:

	float m_vampire_want_value;
	float m_vampire_want_speed; // load from ltx
	float m_vampire_wound;
	float m_vampire_gain_health;
	float m_vampire_distance;
	void LoadVampirePPEffector(LPCSTR section);
	//--------------------------------------------------------------------
	// Threaten
	//--------------------------------------------------------------------

	u32					m_threaten_time;

	//--------------------------------------------------------------------
	// Alien
	//--------------------------------------------------------------------
public:
	CBloodsuckerIXAlien		m_alien_control;
	u32						m_time_lunge;



	void			set_alien_control(bool val);


	//--------------------------------------------------------------------
	// Predator
	//--------------------------------------------------------------------
public:
	shared_str				m_visual_default;
	LPCSTR					m_visual_predator;
	bool					m_predator;

	void			predator_start();
	void			predator_stop();
	void			predator_freeze();
	void			predator_unfreeze();

	//--------------------------------------------------------------------
	// Sounds
	//--------------------------------------------------------------------
public:

	enum EBloodsuckerIXSounds
	{
		eAdditionalSounds = MonsterSound::eMonsterSoundCustom,
		eGrowl = eAdditionalSounds | 1,
		eChangeVisibility = eAdditionalSounds | 2,
		eAlien = eAdditionalSounds | 3,
		eVampireGrasp = eAdditionalSounds | 4,
		eVampireSucking = eAdditionalSounds | 5,
		eVampireHit = eAdditionalSounds | 6,
		eVampireStartHunt = eAdditionalSounds | 7,
	};

	//--------------------------------------------------------------------

public:
	void	set_manual_control(bool value) {}
	void	manual_activate();
	void	manual_deactivate();
	bool	start_threaten;
	float	get_vampire_distance() const { return m_vampire_distance; }
	virtual	char* get_monster_class_name() { return (char*)"BloodsuckerIX"; }
	u32		m_hits_before_vampire;
	u32		m_sufficient_hits_before_vampire;
	int		m_sufficient_hits_before_vampire_random;
	bool	done_enough_hits_before_vampire();
	virtual void on_attack_on_run_hit();

	DECLARE_SCRIPT_REGISTER_FUNCTION
public:
	virtual bool	can_be_seen() const { return !state_invisible; }

};
