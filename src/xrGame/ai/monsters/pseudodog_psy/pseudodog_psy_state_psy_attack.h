#pragma once

#include "../state.h"

class	CStatePsyDogPsyAttack : public CState 
{
protected:
	using inherited = CState		;
	using state_ptr = CState*;

public:
						CStatePsyDogPsyAttack	(CBaseMonster* object);
						virtual				~CStatePsyDogPsyAttack() override;

	virtual void		reselect_state			() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}
};
