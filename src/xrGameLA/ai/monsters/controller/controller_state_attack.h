#pragma once

//#include "../state.h"
#include "../../../ai_debug.h"
#include "../states/monster_state_attack.h"


template<typename _Object>
class	CStateControllerAttack : public CStateMonsterAttack<_Object> //public CState<_Object> 
{
protected:
	typedef CStateMonsterAttack<_Object>		inherited;
//	typedef CState<_Object>*	state_ptr;

public:
						CStateControllerAttack	(_Object *obj);
	virtual				~CStateControllerAttack	() {}

	virtual void		initialize				();
	virtual void		finalize				();
	virtual void		critical_finalize		();
	
	virtual void		execute					();
	virtual void		setup_substates			();
	virtual void		check_force_state		();
			bool		check_state				(u32 state_id);
};

#include "controller_state_attack_inline.h"
