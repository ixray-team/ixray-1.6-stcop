///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../states/monster_state_attack.h"

template<typename _Object>
class CBloodsuckerIXStateAttack :
	public CStateMonsterAttack<_Object>
{
	using inherited_attack = CStateMonsterAttack<_Object>;

	u32				m_time_stop_invis;
	Fvector			m_dir_point;

public:
	CBloodsuckerIXStateAttack(_Object* obj);
	virtual			~CBloodsuckerIXStateAttack();

	virtual	void	initialize();
	virtual	void	execute();
	virtual	void	finalize();
	virtual	void	critical_finalize();

	virtual void	setup_substates();
private:
	void	update_invisibility();
	bool	check_hiding();
	bool	check_vampire();
};

#include "Bloodsucker_ix_attack_state_inline.h"
