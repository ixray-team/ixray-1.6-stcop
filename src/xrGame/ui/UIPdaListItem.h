//////////////////////////////////////////////////////////////////////
// UIPdaListItem.h: элемент окна списка в PDA
// для отображения информации о контакте PDA
//////////////////////////////////////////////////////////////////////

#pragma once
#include "../../xrUI/Widgets/UIHint.h"

class CUIFrameWindow;
class CUICharacterInfo;
class CInventoryOwner;

class CUIPdaListItem : public UIHintWindow
{
private:
	typedef UIHintWindow inherited;
public:
					CUIPdaListItem		();
	virtual			~CUIPdaListItem		();
	virtual void	Init				(float x, float y, float width, float height);
	virtual void	InitCharacter		(CInventoryOwner* pInvOwner);
	
	virtual CUIWindow* ui_cast_window() { return this; }

	void*					m_data;
	CUIFrameWindow*			m_frame_selected = nullptr;
protected:
	//информация о персонаже
	CUIFrameWindow*			UIMask;
	CUICharacterInfo*		UIInfo;
};