#pragma once

#include "../state.h"

class CStateControlAttack : public CState {
protected:
	using	inherited = CState;

	enum {
		eActionPrepare,
		eActionContinue,
		eActionFire,
		eActionWaitTripleEnd,
		eActionCompleted
	} m_action;

	u32				time_control_started;

	CControllerBase* pControllerBase;

public:

					CStateControlAttack		(CBaseMonster* object);
	virtual			~CStateControlAttack	() override;

	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual void	finalize				() override;
	virtual void	critical_finalize		() override;

	virtual bool 	check_completion		() override;
	virtual bool 	check_start_conditions	() override;

private:

			void	execute_hit_fire		();
			void	execute_hit_continue	();
			void	execute_hit_prepare		();
};
