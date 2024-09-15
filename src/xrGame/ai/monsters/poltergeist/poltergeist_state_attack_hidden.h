#pragma once

#include "../state.h"

class	CStatePoltergeistAttackHidden : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

public:
					CStatePoltergeistAttackHidden	(CBaseMonster*obj);
	virtual			~CStatePoltergeistAttackHidden	() {}


	virtual void	initialize				();
	virtual void	execute					();
	virtual void	remove_links			(CObject* object_) { inherited::remove_links(object_);}

			bool	check_home_point		();

private:
			void	select_target_for_move	();

			u32		m_fly_side_select_tick;
			float	m_fly_radius_factor;
			bool	m_fly_left;
			Fvector	m_target;
			u32		m_target_vertex;
};

#include "poltergeist_state_attack_hidden_inline.h"
