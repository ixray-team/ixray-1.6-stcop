#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterAttackMoveToHomePoint : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32					m_target_node;
	Fvector				m_target_pos;
	bool				m_skip_camp;
	TTime				m_selected_target_time;

public:
						CStateMonsterAttackMoveToHomePoint(_Object *obj);

	virtual void		initialize				() override;
	virtual void 		finalize				() override;
	virtual void 		critical_finalize		() override;
	virtual void		execute					() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;

private:
			void		select_target			();
			void		clean					();
};

#include "monster_state_home_point_attack_inline.h"
