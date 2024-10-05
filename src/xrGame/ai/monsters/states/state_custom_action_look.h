#pragma once
#include "../state.h"
#include "state_data.h"

class CStateMonsterCustomActionLook : public CState {
	typedef CState inherited;

	SStateDataActionLook	data;

public:
						CStateMonsterCustomActionLook	(CBaseMonster*obj);
	virtual				~CStateMonsterCustomActionLook	();

	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
