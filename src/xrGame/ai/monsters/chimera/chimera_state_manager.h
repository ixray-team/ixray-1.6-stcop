#pragma once
#include "../monster_state_manager.h"

class CChimeraBase;

class CStateManagerChimera : public CMonsterStateManager
{
private:
	typedef				CMonsterStateManager	inherited;

public:
						CStateManagerChimera	(CChimeraBase *obj);
	virtual				~CStateManagerChimera	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
