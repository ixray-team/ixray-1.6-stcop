#pragma once
#include "../basemonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"


XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CPseudoGigant final : public CBaseMonster,
					  public CControlledEntity<CPseudoGigant> {
	
	typedef		CBaseMonster						inherited;
	typedef		CControlledEntity<CPseudoGigant>	CControlled;

private:
	xr_vector<ISpatialShared> m_nearest;

	// step_effector
	struct {
		float time;
		float amplitude;	
		float period_number;
	} step_effector;

	SAttackEffector m_threaten_effector;
	ref_sound		m_sound_threaten_hit;		// звук, который играется в голове у актера
	ref_sound		m_sound_start_threaten;		// звук, который играется в голове у актера
	
	u32				m_time_next_threaten;
	
	u32				m_threaten_delay_min;
	u32				m_threaten_delay_max;
	float			m_threaten_dist_min;
	float			m_threaten_dist_max;

	float			m_kick_damage;
	
	u32				m_time_kick_actor_slow_down;

	SVelocityParam	m_fsVelocityJumpPrepare;
	SVelocityParam	m_fsVelocityJumpGround;

	const char*			m_kick_particles;


public:
					CPseudoGigant				();
	virtual ~CPseudoGigant				() override;

	virtual void	Load				(const char* section) override;
	virtual void	reinit				() override;

	virtual bool	ability_earthquake	() override {return true;}
	virtual void	event_on_step		() override;

	virtual bool	check_start_conditions	(ControlCom::EControlType type) override;
	virtual void	on_activate_control		(ControlCom::EControlType) override;

	virtual void	on_threaten_execute	() override;

	virtual void	HitEntityInJump		(const CEntity *pEntity) override;
	virtual void	TranslateActionToPathParams	() override;
	virtual char*	get_monster_class_name () override { return (char*) "pseudogigant"; }

	virtual void	OnEvent(NET_Packet& P, u16 type) override;


};