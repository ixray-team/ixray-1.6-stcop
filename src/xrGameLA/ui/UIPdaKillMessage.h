// File:		UIPdaKillMessage.h
// Description:	HUD message about player death. Implementation of visual behavior
// Created:		10.03.2005
// Author:		Serge Vynnychenko
// Mail:		narrator@gsc-game.kiev.ua
// 
// Copyright 2005 GSC GameWorld

#include "KillMessageStruct.h"
#include "UIStatic.h"

class CUIPdaKillMessage : public CUIColorAnimConrollerContainer 
{
	typedef CUIColorAnimConrollerContainer inherited;
public:
	CUIPdaKillMessage();
	virtual ~CUIPdaKillMessage();

			void Init				(KillMessageStruct& msg, CGameFont* F);

protected:
			float InitText(CUITextWnd& refStatic, float x, PlayerInfo& info);
			float InitIcon(CUIStatic& refStatic, float x, IconInfo& info);

	CUITextWnd	m_victim_name;
	CUIStatic	m_initiator;
	CUITextWnd	m_killer_name;
	CUIStatic	m_ext_info;
};
