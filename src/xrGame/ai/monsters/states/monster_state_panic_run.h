#pragma once

#include "../state.h"

class CStateMonsterPanicRun : public CState {
	typedef CState inherited;

public:
						CStateMonsterPanicRun	(CBaseMonster *obj) : inherited(obj) {}
	virtual				~CStateMonsterPanicRun	() {}

	virtual void		initialize				();
	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual bool		check_completion		();
};
