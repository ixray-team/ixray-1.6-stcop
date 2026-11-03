#pragma once

template<typename _Object>
class CStateControlMoveOut : public CState<_Object> {
	typedef	CState<_Object>		inherited;
	typedef	CState<_Object>*	state_ptr;

	Fvector			m_look_point;
	
	u32				m_last_time_look_point_updated;
	u32				m_current_delay;

	enum {
		eMoveToNodeEnemyLastSeen,
		eMoveToLastEnemyLocation
	} m_state;

	Fvector			m_target_position;
	u32				m_target_node;

	u32				m_enemy_vertex;	

public:

					CStateControlMoveOut	(_Object *obj) : inherited(obj) {}
	virtual ~CStateControlMoveOut	() override = default;

	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual bool 	check_completion		() override;
	virtual bool 	check_start_conditions	() override;

	virtual void	remove_links			(CObject* object) override {}

private:
			void	update_target_point		();	
			void	update_look_point		();

};

#include "controller_state_attack_moveout_inline.h"

