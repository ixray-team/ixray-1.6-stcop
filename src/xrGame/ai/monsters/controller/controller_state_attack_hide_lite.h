#pragma once

#include "../state.h"

// Hiding until enemy get out from its sight
class CStateControlHideLite : public CState{
	typedef	CState		inherited;
	typedef	CState*	state_ptr;

	struct {
		Fvector position;
		u32		node;
	} target;
	CControllerBase* m_pController;
	u32				m_time_finished;

public:

	CStateControlHideLite(CBaseMonster* obj);
	virtual			~CStateControlHideLite	() {}

	virtual void	reinit					();

	virtual void	initialize				();
	virtual void	execute					();

	virtual void	finalize				();

	virtual bool 	check_completion		();
	virtual bool 	check_start_conditions	();
	virtual void	remove_links			(CObject* object) {}


private:
	void	select_target_point		();
};
