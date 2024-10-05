#pragma once

#include "../state.h"
#include "state_data.h"

class CStateMonsterMoveAroundPoint : public CState {
	typedef CState inherited;

	SStateDataMoveAroundPoint data;

public:
						CStateMonsterMoveAroundPoint	(CBaseMonster*obj) : inherited(obj, &data) {}
	virtual				~CStateMonsterMoveAroundPoint	() {}

	virtual void		initialize						();
	virtual	void		execute							();

	virtual bool		check_completion				();
};
