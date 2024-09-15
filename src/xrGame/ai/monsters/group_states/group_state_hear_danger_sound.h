#pragma once

#include "../state.h"

class	CStateGroupHearDangerousSound : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

	u32				m_target_node;


public:
	CStateGroupHearDangerousSound		(CBaseMonster*obj);
	virtual			~CStateGroupHearDangerousSound	() {}

	virtual void	initialize						();
	virtual void	reselect_state					();
	virtual void	setup_substates					();
	virtual void	remove_links					(CObject* object_) { inherited::remove_links(object_);}
};

#include "group_state_hear_danger_sound_inline.h"