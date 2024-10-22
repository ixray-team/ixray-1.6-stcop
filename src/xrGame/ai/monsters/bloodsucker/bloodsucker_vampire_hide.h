#pragma once
#include "../state.h"

class CustomBloodsuckerStateVampireHide : public CState 
{
protected:
	using inherited = CState;
	using state_ptr = CState*;

public:
	CustomBloodsuckerStateVampireHide(CBloodsuckerBase* object);
	virtual ~CustomBloodsuckerStateVampireHide() override;

	virtual	void		reselect_state() override;
	virtual void		setup_substates() override;
	virtual bool		check_completion() override;
	virtual void		remove_links(CObject* object) override { inherited::remove_links(object); }
};

