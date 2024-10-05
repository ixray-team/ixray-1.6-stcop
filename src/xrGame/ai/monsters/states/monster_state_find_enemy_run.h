#pragma once

#include "../state.h"

class CStateMonsterFindEnemyRun : public CState {
	typedef CState inherited;

	Fvector				target_point;
	u32					target_vertex;

public:
						CStateMonsterFindEnemyRun	(CBaseMonster*obj);
	virtual				~CStateMonsterFindEnemyRun	();

	virtual void		initialize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
