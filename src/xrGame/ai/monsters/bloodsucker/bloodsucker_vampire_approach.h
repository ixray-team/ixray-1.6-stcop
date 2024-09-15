#pragma once
#include "../state.h"

class CStateBloodsuckerVampireApproach : public CState
{
	typedef CState inherited;

public:
						CStateBloodsuckerVampireApproach	(CAI_Bloodsucker*obj);
	virtual				~CStateBloodsuckerVampireApproach	();

	virtual void		initialize							();
	virtual	void		execute								();
	virtual void		remove_links						(CObject* object) { inherited::remove_links(object);}
};

#include "bloodsucker_vampire_approach_inline.h"
