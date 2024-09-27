#pragma once

#include "../state.h"
#include "../../../entitycondition.h"
#include "../states/state_data.h"

class	CStateGroupRest : public CState {
protected:
	using inherited = CState;
	using state_ptr = CState*;

	u32					time_for_life;
	u32					time_for_sleep;

	CustomDog*			m_pDog;

public:
						CStateGroupRest		(CBaseMonster * object);
	virtual				~CStateGroupRest		();

	virtual	void		initialize				();
	virtual	void		execute					();
	virtual	void		finalize				();
	virtual	void		critical_finalize		();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};
