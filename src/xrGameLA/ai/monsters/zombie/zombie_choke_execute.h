#pragma once
#include "../state.h"

template<typename _Object>
class	CStateZombieChokeExecute : public CState<_Object> {
	typedef CState<_Object>		inherited;

	enum {
		eActionPrepare,
		eActionContinue,
		eActionFire,
		eActionWaitTripleEnd,
		eActionCompleted
	} m_action;

	u32					time_choke_started;
	
	bool				m_effector_activated;

public:
						CStateZombieChokeExecute	(_Object *obj) : inherited(obj) {}

	virtual void		initialize						();
	virtual	void		execute							();
	virtual	void		finalize						();
	virtual	void		critical_finalize				();
	virtual bool		check_start_conditions			();
	virtual bool		check_completion				();

private:
			void		execute_choke_prepare			();
			void		execute_choke_continue		();
			void		execute_choke_hit				();

			void		look_head						();
			void		show_hud						();
			void		cleanup							();
};

#include "zombie_choke_execute_inline.h"