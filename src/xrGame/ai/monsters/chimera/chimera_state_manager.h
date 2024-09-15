#pragma once
#include "../monster_state_manager.h"

class CChimera;

class CStateManagerChimera : public CMonsterStateManager
{
private:
	typedef				CMonsterStateManager	inherited;

public:
						CStateManagerChimera	(CChimera *obj);
	virtual				~CStateManagerChimera	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
