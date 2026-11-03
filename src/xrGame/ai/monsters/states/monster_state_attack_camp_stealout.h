#pragma once


template<typename _Object>
class CStateMonsterAttackCampStealOut : public CStateMove<_Object> {
	typedef CStateMove<_Object> inherited;

public:
						CStateMonsterAttackCampStealOut	(_Object *obj);

	virtual void		execute					() override;
	virtual bool 		check_completion		() override;
	virtual bool 		check_start_conditions	() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_attack_camp_stealout_inline.h"
