#pragma once
#include "../state.h"

#include "../../../detail_path_manager.h"

class CStateMonsterHittedMoveOut : public CState{
	typedef	CState	inherited;
	typedef	CState*	state_ptr;

	struct {
		Fvector position;
		u32		node;
	} target;

public:

					CStateMonsterHittedMoveOut	(CBaseMonster*obj) : inherited(obj) {}
	virtual			~CStateMonsterHittedMoveOut	() {}

	virtual	void	initialize					();
	virtual void	execute						();
	virtual bool 	check_completion			();
	virtual void	remove_links				(CObject* object_) { inherited::remove_links(object_);}

private:
			void	select_target				();

};
