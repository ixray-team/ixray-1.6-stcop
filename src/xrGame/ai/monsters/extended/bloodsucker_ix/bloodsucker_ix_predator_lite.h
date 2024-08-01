///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../state.h"

template<typename _Object>
class	CStateBloodsuckerIXPredatorLite :
	public CState<_Object>
{
	using inherited = CState<_Object>;
	using state_ptr = CState<_Object>*;

	u32							m_target_node;
	bool						m_freezed;

public:
	CStateBloodsuckerIXPredatorLite(_Object* obj);

	virtual void		reinit();

	virtual void		initialize();
	virtual	void		reselect_state();
	virtual	void		finalize();
	virtual	void		critical_finalize();
	virtual bool		check_completion();

	virtual void		setup_substates();
	virtual void		check_force_state();

private:
	void		select_camp_point();
	bool		enemy_see_me();

};

#include "Bloodsucker_ix_predator_lite_inline.h"