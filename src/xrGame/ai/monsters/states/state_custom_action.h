#pragma once
#include "../state.h"
#include "state_data.h"

class CStateMonsterCustomAction : public CState {
	typedef CState  inherited;

	SStateDataAction	data;

public:
						CStateMonsterCustomAction	(CBaseMonster *obj);
	virtual				~CStateMonsterCustomAction	();

	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};

#include "state_custom_action_inline.h"