#pragma once

#include "../state.h"

class CStateGroupPanicRun : public CState{
	using inherited = CState;

public:
	CStateGroupPanicRun(CBaseMonster* object);
						virtual				~CStateGroupPanicRun();

	virtual void		initialize				();
	virtual	void		execute					();

	virtual bool		check_completion		();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};
