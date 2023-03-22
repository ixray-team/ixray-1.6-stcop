#pragma once
#include "../monster_state_manager.h"

class CPseudoGigant;

class CStateManagerGigant : public CMonsterStateManager<CPseudoGigant> {
	typedef CMonsterStateManager<CPseudoGigant> inherited;
public:

					CStateManagerGigant	(CPseudoGigant *monster); 
	virtual void	execute				();
	virtual void	remove_links		(CObject* object_) { inherited::remove_links(object_);}
};
