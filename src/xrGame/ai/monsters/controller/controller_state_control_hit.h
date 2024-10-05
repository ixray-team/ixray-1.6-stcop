#pragma once

#include "../state.h"

class CStateControlAttack : public CState {
	typedef	CState inherited;

	enum {
		eActionPrepare,
		eActionContinue,
		eActionFire,
		eActionWaitTripleEnd,
		eActionCompleted
	} m_action;

	u32				time_control_started;

	CController* m_pController;

public:

					CStateControlAttack		(CBaseMonster *p);
	virtual			~CStateControlAttack	();

	virtual void	initialize				();	
	virtual void	execute					();
	virtual void	finalize				();
	virtual void	critical_finalize		();

	virtual bool 	check_completion		();
	virtual bool 	check_start_conditions	();

private:

			void	execute_hit_fire		();
			void	execute_hit_continue	();
			void	execute_hit_prepare		();
};
