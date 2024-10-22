#pragma once

#include "../state.h"

class	CustomBloodsuckerStateJump : public CState 
{
protected:
	using inherited = CState;
	using state_ptr = CState*;

public:
	CustomBloodsuckerStateJump(CBloodsuckerBase* object);
	virtual				~CustomBloodsuckerStateJump() override;

	virtual	void		execute() override;
	virtual void		setup_substates() override;
	virtual void		remove_links(CObject* object) override { inherited::remove_links(object); }
};
