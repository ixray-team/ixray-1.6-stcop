#pragma once

#include "../state.h"

class	CStatePsyDogPsyAttack : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

public:
						CStatePsyDogPsyAttack	(CBaseMonster *obj);
	virtual				~CStatePsyDogPsyAttack	() {}

	virtual void		reselect_state			();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};

#include "psy_dog_state_psy_attack_inline.h"
