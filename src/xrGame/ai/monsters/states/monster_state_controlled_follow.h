#pragma once

#include "../state.h"

template<typename _Object>
class	CStateMonsterControlledFollow : public CState<_Object> {
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

public:
						CStateMonsterControlledFollow	(_Object *obj);
	virtual void		reselect_state					() override;
	virtual void		setup_substates					() override;
	virtual void		remove_links					(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_controlled_follow_inline.h"
