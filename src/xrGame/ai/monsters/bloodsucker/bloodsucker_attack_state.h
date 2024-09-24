#pragma once
#include "../states/monster_state_attack.h"

class	CustomBloodsuckerStateAttack : public CStateMonsterAttack 
{
	using inherited_attack  = CStateMonsterAttack;

	u32				m_time_stop_invis;
	Fvector			m_dir_point;

	float           m_last_health;
	bool            m_start_with_encircle;
	CustomBloodsucker* m_pBloodsucker;

public:
	CustomBloodsuckerStateAttack(CustomBloodsucker* obj);
	virtual			~CustomBloodsuckerStateAttack();

	virtual	void	initialize();
	virtual	void	execute();
	virtual	void	finalize();
	virtual	void	critical_finalize();

	virtual void	setup_substates();

	struct SBloodsuckerStateAttackProperies
	{
		static constexpr float loose_health_diff = 0.15f;
	};

private:
	bool	check_hiding();
	bool	check_vampire();
};
