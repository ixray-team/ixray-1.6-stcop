#pragma once

#include "../state.h"

template<typename _Object>
class CStateGroupRestIdle : public CState<_Object> {
	typedef CState<_Object> inherited;
	typedef CState<_Object> *state_ptr;

	u32					m_target_node;
	int					m_move_type;

public:
						CStateGroupRestIdle	(_Object *obj);
	virtual void 		initialize				() override;
	virtual void 		finalize				() override;
	virtual void 		critical_finalize		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;
};

#include "group_state_rest_idle_inline.h"
