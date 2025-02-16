#pragma once
#include "../state.h"


template<typename _Object>
class CStateGhostBossShield : public CState<_Object> {
	typedef CState<_Object> inherited;	

	u32 m_last_shield_started;
	u32 m_next_particle_allowed;
	float m_shield_start_anim_length_sec;
	enum {
		ACTION_IDLE,
		ACTION_SHIELD_STARTED,
		ACTION_WAIT_TRIPLE_END,
		ACTION_COMPLETED,
	} m_action;

public:
	CStateGhostBossShield	(_Object *obj);

	virtual	void		initialize				();
	virtual	void		execute					();
	virtual void		finalize				();
	virtual void		critical_finalize		();

	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();

private:
};

#include "GhostBoss_state_attack_shield_inline.h"
