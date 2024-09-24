#pragma once
#include "../state.h"

class	CustomBloodsuckerStateVampireExecute : public CState 
{
	using inherited = CState;

	enum {
		eActionPrepare,
		eActionContinue,
		eActionFire,
		eActionWaitTripleEnd,
		eActionCompleted
	} m_action;

	u32					time_vampire_started;

	bool				m_effector_activated;

	CustomBloodsucker* m_pBloodsucker;

public:
	CustomBloodsuckerStateVampireExecute(CustomBloodsucker* object);
	virtual ~CustomBloodsuckerStateVampireExecute();

	virtual void		initialize();
	virtual	void		execute();
	virtual	void		finalize();
	virtual	void		critical_finalize();
	virtual bool		check_start_conditions();
	virtual bool		check_completion();
	virtual void		remove_links(CObject* object) { inherited::remove_links(object); }

	struct SBloodsuckerStateVampireExecuteProperies
	{
		static constexpr int TimeHold = 4000;           // Время удержания вампира в миллисекундах
	};

private:
	void		execute_vampire_prepare();
	void		execute_vampire_continue();
	void		execute_vampire_hit();

	void		look_head();
	void		show_hud();
	void		cleanup();
};
