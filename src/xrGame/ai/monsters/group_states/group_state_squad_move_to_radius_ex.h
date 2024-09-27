#pragma once
#include "../state.h"
#include "../states/state_data.h"

class CStateGroupSquadMoveToRadiusEx : public CState {
	using inherited = CState;

protected:
	
	SStateDataMoveToPointEx data;

public:
	CStateGroupSquadMoveToRadiusEx(CBaseMonster* object);
						virtual				~CStateGroupSquadMoveToRadiusEx();

	virtual void		initialize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object) { inherited::remove_links(object);}
};
