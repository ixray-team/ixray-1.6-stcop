#pragma once
#include "../state.h"

class CStateMonsterTestState : public CStat {
	typedef CState inherited;
	typedef CState *state_ptr;

public:
						CStateMonsterTestState	(CBaseMonster*obj);
	virtual	void		reselect_state			();
	virtual	void		setup_substates			();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};

class CStateMonsterTestCover : public CState{
	typedef CState inherited;
	typedef CState *state_ptr;

	u32					m_last_node;

public:
						CStateMonsterTestCover	(CBaseMonster*obj);
	virtual void		initialize				();	
	virtual void		check_force_state		();
	virtual	void		reselect_state			();
	virtual	void		setup_substates			();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};

#include "state_test_state_inline.h"