#pragma once
#include "../state.h"

class CStateMonsterRestWalkGraph : public CState {
	typedef CState inherited;

public:
						CStateMonsterRestWalkGraph	(CBaseMonster*obj);
	virtual				~CStateMonsterRestWalkGraph	();

	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
