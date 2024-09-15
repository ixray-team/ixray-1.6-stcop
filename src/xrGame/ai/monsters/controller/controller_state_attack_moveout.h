#pragma once

class CStateControlMoveOut : public CState {
	typedef	CState		inherited;
	typedef	CState*	state_ptr;

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

					CStateControlMoveOut	(CBaseMonster *obj) : inherited(obj) {}
	virtual			~CStateControlMoveOut	() {}

	virtual void	initialize				();
	virtual void	execute					();
	virtual bool 	check_completion		();
	virtual bool 	check_start_conditions	();

	virtual void	remove_links			(CObject* object) {}

private:
			void	update_target_point		();	
			void	update_look_point		();

};

#include "controller_state_attack_moveout_inline.h"

