///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once

class CAI_BloodsuckerIX;
class CAlienEffectorIX;
class CAlienEffectorIXPP;

class CBloodsuckerIXAlien 
{
	CAI_BloodsuckerIX* m_object;

	bool						m_active;

	CAlienEffectorIX* m_effector;
	CAlienEffectorIXPP* m_effector_pp;

	bool						m_crosshair_show;

public:
	CBloodsuckerIXAlien();
	~CBloodsuckerIXAlien();

	void	init_external(CAI_BloodsuckerIX* obj);
	void	reinit();

	void	activate();
	void	deactivate();

	bool	active() { return m_active; }
};