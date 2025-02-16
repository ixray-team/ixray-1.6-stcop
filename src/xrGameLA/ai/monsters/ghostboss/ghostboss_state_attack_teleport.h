#pragma once

#include "../state.h"

template<typename _Object>
class	CStateGhostBossTeleport : public CState<_Object> {
	typedef CState<_Object>	inherited;

public:
						CStateGhostBossTeleport			(_Object *obj);
	virtual	bool		check_start_conditions			();
	virtual	bool		check_completion				();
	virtual	void		initialize						();
	virtual void 		finalize						();
	virtual void 		critical_finalize				();
	virtual void 		execute							();
private:
	u32 m_last_teleport_started;
	enum {
		ACTION_IDLE,
		ACTION_SHIELD_STARTED,
		ACTION_WAIT_TRIPLE_END,
		ACTION_COMPLETED,
	} m_action;
	//Fvector position;
};

#include "ghostboss_state_attack_teleport_inline.h"
