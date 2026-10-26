
#pragma once

#include "../../xrUI/Widgets/UIWindow.h"


class CUIFrameWindow;
class CUIFrameLineWnd;
class CUIStatic;
class CUIAnimatedStatic;
class CUIScrollView;
class CInventoryOwner;
class UIHint;
class CUIGamepadLegend;
class CUIXml;

class CUIPdaContactsWnd: public CUIWindow  
{
private:
	typedef CUIWindow inherited;
	enum				{flNeedUpdate  =(1<<0),};
	Flags8				m_flags;
	xr_vector<CInventoryOwner*>	m_pda_list;
public:
								CUIPdaContactsWnd		();
	virtual ~CUIPdaContactsWnd		() override;

	void						Init					();


	virtual void				Draw					() override;
	virtual void				Update					() override;
	virtual void				DrawHint				();
	virtual void				Reset					() override;

	virtual bool				OnGamepadKeyAction		(int id, EUIMessages gamepad_action) override;
	virtual bool				OnGamepadKeyHold		(int id) override;

	virtual void				Show					(bool status) override;

	void 						AddContact				(CInventoryOwner* pda);
	void 						RemoveAll				();
	void 						Reload					();
	void 						UpdateInfo				();

	CUIScrollView*				UIListWnd;
	CUIScrollView*				UIDetailsWnd;
	UIHint*						m_hint_wnd;
	CUIGamepadLegend*			m_gamepad_legend = nullptr;

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
	CUIXml*						_layoutXml = nullptr;
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
private:
	typedef CUIPdaListItem inherited;
public:
								CUIPdaContactItem		(CUIPdaContactsWnd* cw)		{m_cw = cw;}
	virtual ~CUIPdaContactItem		() override = default;
	virtual void				SetSelected				(bool b) override;
	virtual bool				OnMouseDown				(int mouse_btn) override;

	virtual CUIWindow* ui_cast_window() override { return this; }
	virtual CUISelectable* ui_cast_selectable() override { return this; }

	virtual void				OnFocusReceive			() override;
			void				SetHintText				();
};