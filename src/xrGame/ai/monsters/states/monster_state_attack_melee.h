#pragma once

#include "../state.h"

template<typename _Object>
class CStateMonsterAttackMelee : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateMonsterAttackMelee	(_Object *obj);
	virtual ~CStateMonsterAttackMelee	() override;

	virtual void		execute						() override;

	virtual bool 		check_completion			() override;
	virtual bool 		check_start_conditions		() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_attack_melee_inline.h"
