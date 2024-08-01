///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../monster_state_manager.h"

class CAI_BloodsuckerIX;

class CStateManagerBloodsuckerIX : 
	public CMonsterStateManager<CAI_BloodsuckerIX> 
{
	using inherited = CMonsterStateManager<CAI_BloodsuckerIX>;

public:
					CStateManagerBloodsuckerIX	(CAI_BloodsuckerIX *monster); 
	virtual void	execute						();
	virtual void	remove_links				(CObject* object) { inherited::remove_links(object); }
	bool			check_vampire();
};
