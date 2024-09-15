#pragma once

class CStateControlHide : public CState {
	typedef	CState	inherited;
	typedef	CState*	state_ptr;

	bool			m_cover_reached;

	struct {
		Fvector position;
		u32		node;
	} target;

	u32				m_time_finished;

	bool			m_state_fast_run;

public:

					CStateControlHide		(CBaseMonster *obj) : inherited(obj) {}
	virtual			~CStateControlHide		() {}

	virtual void	initialize				();
	virtual void	execute					();

	virtual void	finalize				();
	virtual void	critical_finalize		();
	virtual void	remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual bool 	check_completion		();
	virtual bool 	check_start_conditions	();


private:
			void	select_target_point		();
};


#include "controller_state_attack_hide_inline.h"

