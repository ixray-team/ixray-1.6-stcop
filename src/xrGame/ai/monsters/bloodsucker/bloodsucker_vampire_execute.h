#pragma once
#include "../state.h"

class	CustomBloodsuckerStateVampireExecute : public CState 
{
protected:
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

	CBloodsuckerBase* pBloodsuckerBase;

public:
	CustomBloodsuckerStateVampireExecute(CBloodsuckerBase* object);
	virtual ~CustomBloodsuckerStateVampireExecute();

	virtual void		initialize() override;
	virtual	void		execute() override;
	virtual	void		finalize() override;
	virtual	void		critical_finalize() override;
	virtual bool		check_start_conditions() override;
	virtual bool		check_completion() override;
	virtual void		remove_links(CObject* object) override { inherited::remove_links(object); }

private:
	void		execute_vampire_prepare();
	void		execute_vampire_continue();
	void		execute_vampire_hit();

	void		look_head();
	void		show_hud();
	void		cleanup();
};
