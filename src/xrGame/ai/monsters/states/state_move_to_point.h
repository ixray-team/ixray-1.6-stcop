#pragma once
#include "../state.h"
#include "state_data.h"

class CStateMonsterMoveToPoint : public CState {
	typedef CState inherited;
	
	SStateDataMoveToPoint data;

public:
						CStateMonsterMoveToPoint	(CBaseMonster *obj) : inherited(obj, &data) {}
	virtual				~CStateMonsterMoveToPoint	() {}

	virtual void		initialize					();
	virtual	void		execute						();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}

	virtual bool		check_completion			();

};


class CStateMonsterMoveToPointEx : public CState {
	typedef CState inherited;

protected:
	
	SStateDataMoveToPointEx data;

public:
						CStateMonsterMoveToPointEx	(CBaseMonster*obj) : inherited(obj, &data) {}
	virtual				~CStateMonsterMoveToPointEx	() {}
	virtual void		initialize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
