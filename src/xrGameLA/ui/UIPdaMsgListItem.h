//////////////////////////////////////////////////////////////////////
// UIPdaMsgListItem.h: элемент окна списка в основном 
// экране для сообщений PDA
//////////////////////////////////////////////////////////////////////

#pragma once
#include "UIStatic.h"
#include "..\InventoryOwner.h"

class CUIPdaMsgListItem : public CUIColorAnimConrollerContainer
{
	typedef	CUIColorAnimConrollerContainer	inherited;
public:
			void		InitPdaMsgListItem				(const Fvector2& size);
	virtual void		InitCharacter					(CInventoryOwner* pInvOwner);
	virtual void		SetTextColor					(u32 color);
	virtual void		SetFont							(CGameFont* pFont);
	virtual void		SetColor						(u32 color);
	
	//информация о персонаже
	CUIStatic			UIIcon;
	CUITextWnd			UIName;
	CUITextWnd			UIMsgText;
};