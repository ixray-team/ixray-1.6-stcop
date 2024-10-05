#pragma once
#include "../state.h"
#include "state_data.h"

class CStateMonsterLookToPoint : public CState {
	typedef CState inherited;

	SStateDataLookToPoint	data;

public:
						CStateMonsterLookToPoint	(CBaseMonster*obj);
	virtual				~CStateMonsterLookToPoint	();

	virtual void		initialize					();
	virtual	void		execute						();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}

	virtual bool		check_completion			();
};
