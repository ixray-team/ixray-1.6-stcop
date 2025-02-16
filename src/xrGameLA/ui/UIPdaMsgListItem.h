//////////////////////////////////////////////////////////////////////
// UIPdaMsgListItem.h: ýëåìåíò îêíà ñïèñêà â îñíîâíîì 
// ýêðàíå äëÿ ñîîáùåíèé PDA
//////////////////////////////////////////////////////////////////////

#pragma once
#include "../xrUI/Widgets/UIStatic.h"
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
	
	//èíôîðìàöèÿ î ïåðñîíàæå
	CUIStatic			UIIcon;
	CUITextWnd			UIName;
	CUITextWnd			UIMsgText;
};