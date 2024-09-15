#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

class	CStateControllerAttack : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

public:
						CStateControllerAttack	(CBaseMonster *obj);
	virtual				~CStateControllerAttack	() {}

	virtual void		initialize				();
	virtual void		finalize				();
	virtual void		critical_finalize		();
	
	virtual void		execute					();
	virtual void		setup_substates			();
	virtual void		check_force_state		();
	virtual void		remove_links			(CObject * ) {}
			bool		check_home_point		();
};

#include "controller_state_attack_inline.h"
