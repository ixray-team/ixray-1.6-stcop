#pragma once
#include "../state.h"

class CustomBloodsuckerStateVampireHide : public CState 
{
	using inherited = CState;
	using state_ptr = CState*;

public:
	CustomBloodsuckerStateVampireHide(CustomBloodsucker* object);
	virtual ~CustomBloodsuckerStateVampireHide();

	virtual	void		reselect_state();
	virtual void		setup_substates();
	virtual bool		check_completion();
	virtual void		remove_links(CObject* object) { inherited::remove_links(object); }
};

