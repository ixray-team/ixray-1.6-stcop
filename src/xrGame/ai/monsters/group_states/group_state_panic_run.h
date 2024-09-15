#pragma once

class CStateGroupPanicRun : public CState{
	typedef CState inherited;

public:
						CStateGroupPanicRun	(CBaseMonster*obj) : inherited(obj) {}
	virtual				~CStateGroupPanicRun	() {}

	virtual void		initialize				();
	virtual	void		execute					();

	virtual bool		check_completion		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};

#include "group_state_panic_run_inline.h"
