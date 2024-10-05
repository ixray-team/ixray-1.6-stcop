#pragma once
#include "../state.h"


class CStateMonsterSteal : public CState {
	typedef CState inherited;

public:
						CStateMonsterSteal		(CBaseMonster*obj);

	virtual void		initialize				();
	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual bool 		check_completion		();
	virtual bool 		check_start_conditions	();

private:
			bool		check_conditions		();
};
