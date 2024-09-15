#pragma once
#include "../state.h"
#include "../states/state_data.h"

class CStateGroupSquadMoveToRadiusEx : public CState {
	typedef CState inherited;

protected:
	
	SStateDataMoveToPointEx data;

public:
						CStateGroupSquadMoveToRadiusEx	(CBaseMonster *obj) : inherited(obj, &data) {}
	virtual				~CStateGroupSquadMoveToRadiusEx	() {}
	virtual void		initialize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};

class CStateGroupSquadMoveToRadius : public CState {
	typedef CState inherited;

protected:

	SStateDataMoveToPointEx data;

public:
	CStateGroupSquadMoveToRadius	(CBaseMonster*obj) : inherited(obj, &data) {}
	virtual				~CStateGroupSquadMoveToRadius	() {}
	virtual void		initialize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};


#include "group_state_squad_move_to_radius_inline.h"
