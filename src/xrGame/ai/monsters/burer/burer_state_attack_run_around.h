#pragma once
#include "../state.h"

template<typename _Object>
class	CStateBurerAttackRunAround : public CState<_Object> {
	typedef CState<_Object>	inherited;

	Fvector				selected_point;
	u32					time_started;

	Fvector				dest_direction;

public:
						CStateBurerAttackRunAround	(_Object *obj);
	virtual void		initialize					() override;
	virtual void		execute						() override;

	virtual bool		check_start_conditions		() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "burer_state_attack_run_around_inline.h"