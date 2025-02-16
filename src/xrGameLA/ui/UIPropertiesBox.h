#pragma once


#include "uiframewindow.h"
#include "uilistbox.h"

#include "../../xrScripts/script_export_space.h"

class CUIPropertiesBox: public CUIFrameWindow
{
private:
	typedef CUIFrameWindow inherited; 
public:
						CUIPropertiesBox					();
	virtual				~CUIPropertiesBox					();

			void		InitPropertiesBox					(Fvector2 pos, Fvector2 size);

	virtual void		SendMessage							(CUIWindow *pWnd, s16 msg, void *pData);
	virtual bool		OnMouseAction								(float x, float y, EUIMessages mouse_action);
	virtual bool		OnKeyboardAction							(int dik, EUIMessages keyboard_action);

	bool				AddItem								(LPCSTR  str, void* pData = NULL, u32 tag_value = 0);
	bool				AddItem_script						(LPCSTR  str){return AddItem(str);};
	u32					GetItemsCount						() {return m_UIListWnd.GetSize();};
	void				RemoveItemByTAG						(u32 tag_value);
	void				RemoveAll							();

	virtual void		Show								(const Frect& parent_rect, const Fvector2& point);
	virtual void		Hide								();

	virtual void		Update								();
	virtual void		Draw								();

	CUIListBoxItem*		GetClickedItem						();

	void				AutoUpdateSize						();
	
protected:
	CUIListBox			m_UIListWnd;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
