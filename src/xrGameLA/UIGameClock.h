// File:		UIGameClock.h
// Description:	Timer window on hud
// Created:		16.02.2011
// Author:		Gr1ph00n

#include "UIStatic.h"

class CUIGameClock 
{
	public:
		CUIGameClock();
		virtual ~CUIGameClock();
		void Init();
		void Update();
	protected:
		CUIStatic m_staticDate;
		CUIStatic m_staticTime;
};
