#pragma once
#include "../pseudodog/pseudodog_state_manager.h"

class CStateManagerPsyDog : public CStateManagerPseudodog 
{
protected:
	typedef CStateManagerPseudodog inherited;

public:
					CStateManagerPsyDog	(CPseudoDogBase *monster); 
	virtual void	execute				();
	virtual void	remove_links		(CObject* object_) { inherited::remove_links(object_);}
};
