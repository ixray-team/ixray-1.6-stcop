#pragma once

#include "../state.h"

class CStateGroupRestIdle : public CState {
	using inherited = CState ;
	using state_ptr = CState *;

	u32					m_target_node;
	int					m_move_type;

	CDogBase* m_pDog;

public:
						CStateGroupRestIdle	(CBaseMonster* object);
						virtual ~CStateGroupRestIdle();

	virtual void 		initialize				();
	virtual void 		finalize				();
	virtual void 		critical_finalize		();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}

	virtual	void		reselect_state			();
	virtual	void		setup_substates			();
};
