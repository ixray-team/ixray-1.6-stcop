
#pragma once

#include "../../xrUI/Widgets/UIWindow.h"


class CUIFrameWindow;
class CUIFrameLineWnd;
class CUIStatic;
class CUIAnimatedStatic;
class CUIScrollView;
class CInventoryOwner;

class CUIPdaContactsWnd: public CUIWindow  
{
private:
	typedef CUIWindow inherited;
	enum				{flNeedUpdate  =(1<<0),};
	Flags8				m_flags;
	xr_vector<CInventoryOwner*>	m_pda_list;
public:
								CUIPdaContactsWnd		();
	virtual						~CUIPdaContactsWnd		();

	void						Init					();


	virtual void				Update					();
	virtual void				Reset					();

	virtual void				Show					(bool status);

	void 						AddContact				(CInventoryOwner* pda);
	void 						RemoveAll				();
	void 						Reload					();
	void 						UpdateInfo				();

	CUIScrollView*				UIListWnd;
	CUIScrollView*				UIDetailsWnd;

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	CUIFrameWindow*				m_background;
	CUIFrameWindow*				UIFrameContacts;
	CUIFrameLineWnd*			UIContactsHeader;
	CUIFrameWindow*				UIRightFrame;
	CUIFrameLineWnd*			UIRightFrameHeader;
	CUIAnimatedStatic*			UIAnimation;
};

#include "UIPdaListItem.h"
class CUIPdaContactItem :public CUIPdaListItem, public CUISelectable
{
	CUIPdaContactsWnd*			m_cw;
public:
								CUIPdaContactItem		(CUIPdaContactsWnd* cw)		{m_cw = cw;}
	virtual						~CUIPdaContactItem		();
	virtual void				SetSelected				(bool b);
	virtual bool				OnMouseDown				(int mouse_btn);

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUISelectable* ui_cast_selectable() { return this; }
};