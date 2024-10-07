#pragma once

#include "../state.h"

class	CStateGroupHearDangerousSound : public CState {
protected:
	using inherited = CState		;
	using state_ptr = CState*	;

	u32				m_target_node;

	CDogBase* m_pDog;

public:
	CStateGroupHearDangerousSound		(CBaseMonster*object);
	virtual			~CStateGroupHearDangerousSound();

	virtual void	initialize						();
	virtual void	reselect_state					();
	virtual void	setup_substates					();
	virtual void	remove_links					(CObject* object) { inherited::remove_links(object);}
};
