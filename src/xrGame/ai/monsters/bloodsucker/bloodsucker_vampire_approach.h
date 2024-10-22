#pragma once
#include "../state.h"

class CustomBloodsuckerVampireApproach : public CState
{
protected:
	using inherited = CState;

public:
	CustomBloodsuckerVampireApproach(CBloodsuckerBase* object);
	virtual				~CustomBloodsuckerVampireApproach() override;

	virtual void		initialize() override;
	virtual	void		execute() override;
	virtual void		remove_links(CObject* object) override { inherited::remove_links(object); }
};

