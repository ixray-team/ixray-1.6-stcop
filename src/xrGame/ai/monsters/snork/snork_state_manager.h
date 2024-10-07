#pragma once
#include "../monster_state_manager.h"

class CSnorkBase;

class CStateManagerSnork : public CMonsterStateManager 
{
private:
	typedef				CMonsterStateManager	inherited;

	CSnorkBase* m_pSnork;

public:
						CStateManagerSnork		(CSnorkBase*obj);
	virtual				~CStateManagerSnork		();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
