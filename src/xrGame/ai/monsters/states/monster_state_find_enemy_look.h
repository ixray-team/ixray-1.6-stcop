#pragma once

#include "../state.h"

class CStateMonsterFindEnemyLook : public CState {
	typedef CState	inherited;
	typedef CState*	state_ptr;

	bool		look_right_side;
	u8			current_stage;
	Fvector		target_point;

	Fvector		current_dir;
	Fvector		start_position;

	enum {
		eMoveToPoint = u32(0),
		eLookAround,
		eTurnToPoint
	} ;

public:
						CStateMonsterFindEnemyLook	(CBaseMonster*obj);
	virtual				~CStateMonsterFindEnemyLook	();

	virtual	void		initialize					();	
	virtual	void		reselect_state				();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}

	virtual void		setup_substates				();
};
