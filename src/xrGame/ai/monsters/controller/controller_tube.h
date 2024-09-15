#pragma once
#include "../state.h"

class CStateControllerTube : public CState {
	typedef CState		inherited;

public:
						CStateControllerTube	(CBaseMonster *obj) : inherited(obj){}
	virtual void		execute					();
	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
};

#include "controller_tube_inline.h"

