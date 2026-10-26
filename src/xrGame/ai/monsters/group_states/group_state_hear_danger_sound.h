#pragma once

#include "../state.h"

template<typename _Object>
class	CStateGroupHearDangerousSound : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32				m_target_node;


public:
	CStateGroupHearDangerousSound		(_Object *obj);
	virtual ~CStateGroupHearDangerousSound	() override = default;

	virtual void	initialize						() override;
	virtual void	reselect_state					() override;
	virtual void	setup_substates					() override;
	virtual void	remove_links					(CObject* object_) override { inherited::remove_links(object_);}
};

#include "group_state_hear_danger_sound_inline.h"