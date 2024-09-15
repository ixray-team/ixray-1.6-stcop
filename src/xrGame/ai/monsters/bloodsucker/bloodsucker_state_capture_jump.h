#pragma once

#include "../state.h"
//#include "../../../entitycondition.h"

class	CStateCaptureJumpBloodsucker : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

public:
	CStateCaptureJumpBloodsucker		(CAI_Bloodsucker*obj);
	virtual				~CStateCaptureJumpBloodsucker		();

	virtual	void		execute					();
	virtual void		setup_substates			();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};

#include "bloodsucker_state_capture_jump_inline.h"