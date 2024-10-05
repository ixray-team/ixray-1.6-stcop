#pragma once
#include "../state.h"

class CStateMonsterMoveToRestrictor : public CState {
	typedef CState inherited;

public:
						CStateMonsterMoveToRestrictor	(CBaseMonster*obj) : inherited(obj) {}
	virtual				~CStateMonsterMoveToRestrictor	() {}

	virtual void		initialize					();
	virtual	void		execute						();

	virtual bool		check_start_conditions		();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};
