#pragma once
#include "../state.h"

class	CStateBurerAttackRunAround : public CState {
	typedef CState	inherited;

	Fvector				selected_point;
	u32					time_started;

	Fvector				dest_direction;

public:
						CStateBurerAttackRunAround	(CBaseMonster*obj);
	virtual void		initialize					();
	virtual void		execute						();

	virtual bool		check_start_conditions		();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}
};

#include "burer_state_attack_run_around_inline.h"