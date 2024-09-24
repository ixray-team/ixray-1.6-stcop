#pragma once

#include "../state.h"

class	CustomBloodsuckerStateJump : public CState 
{
protected:
	using inherited = CState;
	using state_ptr = CState*;

public:
	CustomBloodsuckerStateJump(CustomBloodsucker* object);
	virtual				~CustomBloodsuckerStateJump();

	virtual	void		execute();
	virtual void		setup_substates();
	virtual void		remove_links(CObject* object) { inherited::remove_links(object); }
};
