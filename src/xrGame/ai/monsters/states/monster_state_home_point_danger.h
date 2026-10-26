#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterDangerMoveToHomePoint : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32					m_target_node;
	bool				m_skip_camp;
	Fvector				m_danger_pos;

public:
						CStateMonsterDangerMoveToHomePoint(_Object *obj);
	virtual void		initialize				() override;
	virtual void 		finalize				() override;
	virtual void 		critical_finalize		() override;

	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;
private:
			Fvector		&get_most_danger_pos	();
};

#include "monster_state_home_point_danger_inline.h"
