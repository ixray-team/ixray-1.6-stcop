#pragma once

#include "../state.h"

class	CStateCustomGroup : public CState{
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

public:
	CStateCustomGroup		(CBaseMonster *obj);
	virtual				~CStateCustomGroup		();

	virtual	void		execute					();
	virtual void		setup_substates			();
	virtual bool 		check_completion		() {return (this->object->b_state_end);}
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};

#include "group_state_custom_inline.h"