#pragma once
#include "../state.h"

class CStateBloodsuckerVampireHide : public CState {
	typedef CState		inherited;
	typedef CState*	state_ptr;

public:
						CStateBloodsuckerVampireHide	(CAI_Bloodsucker*obj);

	virtual	void		reselect_state					();
	virtual void		setup_substates					();
	virtual bool		check_completion				();
	virtual void		remove_links					(CObject* object) { inherited::remove_links(object);}
};

#include "bloodsucker_vampire_hide_inline.h"
