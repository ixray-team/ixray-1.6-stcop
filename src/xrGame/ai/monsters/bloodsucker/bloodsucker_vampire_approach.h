#pragma once
#include "../state.h"

class CustomBloodsuckerVampireApproach : public CState
{
	using inherited = CState;

public:
	CustomBloodsuckerVampireApproach(CustomBloodsucker* object);
	virtual				~CustomBloodsuckerVampireApproach();

	virtual void		initialize();
	virtual	void		execute();
	virtual void		remove_links(CObject* object) { inherited::remove_links(object); }
};

