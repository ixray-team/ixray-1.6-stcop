#pragma once
#include "../monster_state_manager.h"

class CPseudoDogBase;

class CStateManagerPseudodog : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;
	
public:

					CStateManagerPseudodog	(CPseudoDogBase *monster); 
	virtual void	execute					();
	virtual void	remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
