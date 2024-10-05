#pragma once

#include "../control_animation_base.h"
#include "../control_direction_base.h"

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
