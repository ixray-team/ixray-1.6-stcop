#pragma once
#include "../state.h"

template<typename _Object>
class CStateGroupAttackMoveToHomePoint : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32					m_target_node;
	bool				m_skip_camp;

	TTime				m_first_tick_enemy_inaccessible;
	TTime				m_last_tick_enemy_inaccessible;
	TTime				m_state_started;

public:
						CStateGroupAttackMoveToHomePoint(_Object *obj);
	virtual void		initialize				() override;
	virtual void 		finalize				() override;
	virtual void 		critical_finalize		() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}

	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;

	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;

			bool		enemy_inaccessible		();
};

#include "group_state_home_point_attack_inline.h"
