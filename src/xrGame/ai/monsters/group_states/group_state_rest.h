#pragma once

#include "../state.h"
#include "../../../entitycondition.h"
#include "../states/state_data.h"

class	CStateGroupRest : public CState {
protected:
	typedef CState	inherited;
	typedef CState*	state_ptr;

	u32					time_for_life;
	u32					time_for_sleep;

public:
						CStateGroupRest		(CBaseMonster *obj);
	virtual				~CStateGroupRest		();

	virtual	void		initialize				();
	virtual	void		execute					();
	virtual	void		finalize				();
	virtual	void		critical_finalize		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};

#include "group_state_rest_inline.h"