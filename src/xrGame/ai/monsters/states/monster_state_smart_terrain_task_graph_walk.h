#pragma once
#include "../state.h"

class CStateMonsterSmartTerrainTaskGraphWalk : public CStateMove {
	typedef CStateMove inherited;

	CALifeSmartTerrainTask	*m_task;

public:
						CStateMonsterSmartTerrainTaskGraphWalk	(CBaseMonster*obj) : inherited(obj) {}
	virtual void		initialize				();
	virtual	void		execute					();
	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
