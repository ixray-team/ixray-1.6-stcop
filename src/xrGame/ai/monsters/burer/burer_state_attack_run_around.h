#pragma once
#include "../state.h"

class	CStateBurerAttackRunAround : public CState {
protected:
	using inherited = CState;

	Fvector				selected_point;
	u32					time_started;

	Fvector				dest_direction;

	CBurerBase* pBurerBase;

public:
						CStateBurerAttackRunAround	(CBaseMonster* object);
						virtual ~CStateBurerAttackRunAround() override;

	virtual void		initialize					() override;
	virtual void		execute						() override;

	virtual bool		check_start_conditions		() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object) override { inherited::remove_links(object);}
};
