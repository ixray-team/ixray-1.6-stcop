#pragma once
#include "../state.h"

template<typename _Object>
class CStateBloodsuckerVampireApproach : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateBloodsuckerVampireApproach	(_Object *obj);
	virtual ~CStateBloodsuckerVampireApproach	() override;

	virtual void		initialize							() override;
	virtual void		execute								() override;
	virtual void		remove_links						(CObject* object) override { inherited::remove_links(object);}
};

#include "bloodsucker_vampire_approach_inline.h"
