#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIFrameWindow;
class CUIFrameLineWnd;
class CUIAnimatedStatic;
class CUIStatic;
class CUICharacterInfo;
class CUIScrollView;
class CUIXml;
class CSE_ALifeTraderAbstract;
class UIHint;
class CUIGamepadLegend;

class CUIStalkersRankingWnd: public CUIWindow
{
	typedef CUIWindow inherited;
public:
	CUIStalkersRankingWnd();
	virtual ~CUIStalkersRankingWnd();
	
			void			Init				();
	virtual void			Show				(bool status);
			void			ShowHumanDetails	();
	virtual void			DrawHint			();
	CUIFrameWindow*			UIInfoFrame;
protected:
	CUIFrameWindow*			m_background = nullptr;
	CUIFrameWindow*			UICharIconFrame;
	CUIFrameLineWnd*		UIInfoHeader;
	CUIFrameLineWnd*		UICharIconHeader;
	CUIAnimatedStatic*		UIAnimatedIcon;
	// информация о персонаже
	CUIWindow*				UICharacterWindow;
	CUICharacterInfo*		UICharacterInfo;
	void					FillList			();
	CUIScrollView*			UIList;
	void					AddStalkerItem		(CUIXml* xml, int num, CSE_ALifeTraderAbstract* t);
	void					AddActorItem		(CUIXml* xml, int num, CSE_ALifeTraderAbstract* t);
	s32						m_items_count;

public:
	CUIScrollView&			GetTopList			()			{return *UIList;}
	void					ShowHumanInfo		(u16 id);
	virtual void			Reset				();
	
	virtual bool			OnGamepadKeyAction	(int id, EUIMessages gamepad_action);
	virtual bool			OnGamepadKeyHold	(int id);
	virtual CUIWindow* ui_cast_window() { return this; }
	UIHint*						m_hint_wnd = nullptr;
	CUIGamepadLegend*		m_gamepad_legend = nullptr;
};

class CUIStalkerRankingInfoItem :public CUIWindow, public CUISelectable
{
	CUIStalkersRankingWnd*	m_StalkersRankingWnd;
	u32						m_stored_alpha;
public:
	u16						m_humanID;
	CUIStatic*				m_text1;
	CUIStatic*				m_text2;
	CUIStatic*				m_text3;
public:
							CUIStalkerRankingInfoItem(CUIStalkersRankingWnd*);
	
	void					Init			(CUIXml* xml, const char* path, int idx);
	virtual void			SetSelected		(bool b);
	virtual bool			OnMouseDown		(int mouse_btn);
	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUISelectable* ui_cast_selectable() { return this; }
	virtual void				OnFocusReceive			();
	virtual void				OnFocusLost				();
			void				SetHintText				();
};

class CUIStalkerRankingElipsisItem :public CUIStalkerRankingInfoItem
{
	typedef CUIStalkerRankingInfoItem inherited;
public:
					CUIStalkerRankingElipsisItem(CUIStalkersRankingWnd*);
	virtual void			SetSelected		(bool b);
	virtual bool			OnMouseDown		(int mouse_btn);
	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUISelectable* ui_cast_selectable() { return this; }
};
