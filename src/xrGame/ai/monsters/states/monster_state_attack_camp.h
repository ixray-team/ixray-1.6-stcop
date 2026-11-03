#pragma once

#include "../state.h"

template<typename _Object>
class CStateMonsterAttackCamp : public CState<_Object> {
	typedef CState<_Object> inherited;
	typedef CState<_Object> *state_ptr;

	u32					m_target_node;
public:
						CStateMonsterAttackCamp	(_Object *obj);

	virtual void 		initialize				() override;
	virtual void 		finalize				() override;
	virtual void 		critical_finalize		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool 		check_completion		() override;
	virtual bool 		check_start_conditions	() override;

	virtual void		check_force_state		() override;
	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;

};

#include "monster_state_attack_camp_inline.h"
