#pragma once

#include "../state.h"

class	CStateMonsterControlled : public CState {
	typedef CState	inherited;

public:
						CStateMonsterControlled		(CBaseMonster*obj);
	virtual	void		execute						();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
