#pragma once
#include "../state.h"

class CStateMonsterRestMoveToHomePoint : public CStateMove{
protected:
	typedef CStateMove		inherited;
	typedef CStateMove*	state_ptr;

	u32					m_target_node;

public:
						CStateMonsterRestMoveToHomePoint(CBaseMonster *obj) : inherited(obj){}
	virtual	void		initialize				();
	virtual	void		execute					();
	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
