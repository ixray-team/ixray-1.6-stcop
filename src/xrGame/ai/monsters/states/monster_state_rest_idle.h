#pragma once

#include "../state.h"

class CStateMonsterRestIdle : public CState{
	typedef CState inherited;
	typedef CState *state_ptr;

	u32					m_target_node;

public:
						CStateMonsterRestIdle	(CBaseMonster *obj);
	virtual void 		initialize				();
	virtual void 		finalize				();
	virtual void 		critical_finalize		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual	void		reselect_state			();
	virtual	void		setup_substates			();
};
