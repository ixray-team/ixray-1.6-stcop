#pragma once
#include "../monster_state_manager.h"

class CSnork;

class CStateManagerSnork : public CMonsterStateManager 
{
private:
	typedef				CMonsterStateManager	inherited;

public:
						CStateManagerSnork		(CSnork*obj);
	virtual				~CStateManagerSnork		();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
