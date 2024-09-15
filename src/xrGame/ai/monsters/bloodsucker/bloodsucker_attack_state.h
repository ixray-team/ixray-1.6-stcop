#pragma once
#include "../states/monster_state_attack.h"

class	CBloodsuckerStateAttack : public CStateMonsterAttack {
	typedef CStateMonsterAttack inherited_attack;

	u32				m_time_stop_invis;
	Fvector			m_dir_point;

	float           m_last_health;
	bool            m_start_with_encircle;

public:
					CBloodsuckerStateAttack		(CAI_Bloodsucker*obj);
	virtual			~CBloodsuckerStateAttack	();

	virtual	void	initialize					();
	virtual	void	execute						();
	virtual	void	finalize					();
	virtual	void	critical_finalize			();
	
	virtual void	setup_substates				();
private:
			bool	check_hiding				();
			bool	check_vampire				();
};

class CStateMonsterBackstubEnemy : public CState {
	typedef CState inherited;
public:
	struct StateParams : SStateDataMoveToPointEx
	{
		bool   start_with_encircle;
		StateParams() : start_with_encircle(false) {}
	} data;

protected:

	float                   m_last_health;
	bool                    m_encircle;
	TTime                   m_encircle_end_tick;
	TTime                   m_next_change_behaviour_tick;

public:
	CStateMonsterBackstubEnemy	(CAI_Bloodsucker*obj) : inherited(obj, &data) {}
	virtual				~CStateMonsterBackstubEnemy	() {}
	virtual void		initialize					();
	virtual	void		execute						();
	virtual bool 		check_start_conditions	    ();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object) { inherited::remove_links(object);}
};

#include "bloodsucker_attack_state_inline.h"
