#pragma once
#include "../monster_state_manager.h"

class CTushkanoBase;

class CStateManagerTushkano : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

	CTushkanoBase* m_pTushkano;

public:
						CStateManagerTushkano	(CTushkanoBase *obj);
	virtual				~CStateManagerTushkano	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
