#pragma once

#include "../state.h"

template<typename _Object>
class	CStateMonsterSquadRest : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32		time_next_state_reselect;

public:
						CStateMonsterSquadRest		(_Object *obj);
	virtual ~CStateMonsterSquadRest		() override;

	virtual void		reselect_state				() override;
	virtual void		setup_substates				() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_squad_rest_inline.h"