#pragma once
#include "../state.h"
#include "../states/state_data.h"

class CStateGroupSquadMoveToRadius : public CState {
	using inherited = CState;

protected:

	SStateDataMoveToPointEx data;

public:
	CStateGroupSquadMoveToRadius(CBaseMonster* object);
	virtual				~CStateGroupSquadMoveToRadius();

	virtual void		initialize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object) { inherited::remove_links(object);}
};

