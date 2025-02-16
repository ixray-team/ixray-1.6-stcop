// File:        UIListBoxItemEx.cpp
// Description: Extended ListItem
//              Requiered to use feature "Selected Item"
// Created:     
// Author:      Serhiy O. Vynnychenko (adapted to new ui by SkyLoader)

// Copyright:   2004 GSC Game World

#pragma once
#include "uilistboxitem.h"

class CUIListBoxItemEx :
	public CUIListBoxItem
{
private:
	typedef CUIListBoxItem inherited;

public:
					CUIListBoxItemEx		(void);
	virtual 			~CUIListBoxItemEx		(void);

	virtual void 			SendMessage			(CUIWindow *pWnd, s16 msg, void* pData);
	virtual void 			SetSelectionColor 		(u32 dwColor);
	virtual void 			Draw				();
	virtual void 			dummy				() {}

protected:
	u32				m_dwSelectionColor;
};
