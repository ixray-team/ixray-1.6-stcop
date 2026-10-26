#pragma once

#include "../state.h"

template<typename _Object>
class CStateMonsterRestIdle : public CState<_Object> {
	typedef CState<_Object> inherited;
	typedef CState<_Object> *state_ptr;

	u32					m_target_node;

public:
						CStateMonsterRestIdle	(_Object *obj);
	virtual void 		initialize				() override;
	virtual void 		finalize				() override;
	virtual void 		critical_finalize		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;
};

#include "monster_state_rest_idle_inline.h"
