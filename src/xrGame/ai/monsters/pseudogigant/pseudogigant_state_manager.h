#pragma once
#include "../monster_state_manager.h"

class CPseudoGiantBase;

class CStateManagerGigant : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

	CPseudoGiantBase* m_pGiant;

public:

					CStateManagerGigant	(CPseudoGiantBase *monster); 
	virtual void	execute				();
	virtual void	remove_links		(CObject* object_) { inherited::remove_links(object_);}
};
