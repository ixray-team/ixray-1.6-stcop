#pragma once
#include "../state.h"
#include "state_data.h"

class CStateMonsterHideFromPoint : public CState {
	typedef CState inherited;

	SStateHideFromPoint data;

public:
						CStateMonsterHideFromPoint	(CBaseMonster*obj) : inherited(obj, &data){}
	virtual				~CStateMonsterHideFromPoint	() {}

	virtual void		initialize					();
	virtual	void		execute						();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}

	virtual bool		check_completion			();
						
};
