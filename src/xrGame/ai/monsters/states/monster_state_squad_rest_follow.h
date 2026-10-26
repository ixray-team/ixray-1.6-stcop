#pragma once

#include "../state.h"

template<typename _Object>
class	CStateMonsterSquadRestFollow : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	Fvector		last_point;

public:
						CStateMonsterSquadRestFollow	(_Object *obj);
	virtual ~CStateMonsterSquadRestFollow	() override;

	virtual void		initialize						() override;
	virtual void		reselect_state					() override;
	virtual void		setup_substates					() override;
	virtual void		check_force_state				() override;
	virtual void		remove_links					(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_squad_rest_follow_inline.h"
