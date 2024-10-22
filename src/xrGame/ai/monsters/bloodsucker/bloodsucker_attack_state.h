#pragma once
#include "../states/monster_state_attack.h"

class	CustomBloodsuckerStateAttack : public CStateMonsterAttack 
{
protected:
	using inherited_attack = CStateMonsterAttack;

	u32				m_time_stop_invis;
	Fvector			m_dir_point;

	float           m_last_health;
	bool            m_start_with_encircle;
	CBloodsuckerBase* m_pBloodsucker;

public:
	CustomBloodsuckerStateAttack(CBloodsuckerBase* object);
	virtual			~CustomBloodsuckerStateAttack() override;

	virtual	void	initialize() override;
	virtual	void	execute() override;
	virtual	void	finalize() override;
	virtual	void	critical_finalize() override;

	virtual void	setup_substates() override;

private:
	bool	check_hiding();
	bool	check_vampire();
};
