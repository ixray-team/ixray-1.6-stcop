#pragma once

#include "../state.h"

class	CStatePoltergeistAttackHidden : public CState {
protected:
	using inherited = CState		;
	using state_ptr = CState*;

	CPoltergeistBase* m_pPoltergeist;

public:
					CStatePoltergeistAttackHidden	(CBaseMonster* object);
					virtual			~CStatePoltergeistAttackHidden() override;


	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual void	remove_links			(CObject* object) override { inherited::remove_links(object); }

			bool	check_home_point		();

private:
			void	select_target_for_move	();

			u32		m_fly_side_select_tick;
			float	m_fly_radius_factor;
			bool	m_fly_left;
			Fvector	m_target;
			u32		m_target_vertex;
};
