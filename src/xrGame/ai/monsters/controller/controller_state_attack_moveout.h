#pragma once

#include "../state.h"

class CStateControlMoveOut : public CState {
protected:
	using	inherited = CState		;
	using	state_ptr = CState*;

	Fvector			m_look_point;
	
	u32				m_last_time_look_point_updated;
	u32				m_current_delay;

	CControllerBase* m_pController;

	enum {
		eMoveToNodeEnemyLastSeen,
		eMoveToLastEnemyLocation
	} m_state;

	Fvector			m_target_position;
	u32				m_target_node;

	u32				m_enemy_vertex;	

public:

	CStateControlMoveOut(CBaseMonster* object);
	virtual			~CStateControlMoveOut() override;

	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual bool 	check_completion		() override;
	virtual bool 	check_start_conditions	() override;

	virtual void	remove_links(CObject* object) override { inherited::remove_links(object); }

private:
			void	update_target_point		();	
			void	update_look_point		();
};
