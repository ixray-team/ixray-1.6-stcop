#pragma once
#include "pseudodog_state_manager.h"

class CStateManagerPsyDog : public CStateManagerPseudodog {
	typedef CStateManagerPseudodog inherited;
public:
					CStateManagerPsyDog	(CAI_PseudoDog *monster);
	virtual void	execute				() override;
	virtual void	remove_links		(CObject* object_) override { inherited::remove_links(object_);}
};
