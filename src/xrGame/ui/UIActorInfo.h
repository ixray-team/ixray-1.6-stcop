#pragma once

#include "../../xrUI/Widgets/UIWindow.h"

class CUIFrameWindow;
class CUIFrameLineWnd;
class CUIAnimatedStatic;
class CUIStatic;
class CUICharacterInfo;
class CUIScrollView;
class CUIXml;
class CUIActorStaticticHeader;
class CUIGamepadLegend;

class CUIActorInfoWnd: public CUIWindow
{
	typedef CUIWindow inherited;

public:
	CUIGamepadLegend*		m_gamepad_legend = nullptr;
							CUIActorInfoWnd		();
	virtual					~CUIActorInfoWnd	();
	virtual void			Init				();
	virtual void			Show				(bool status);
	CUIScrollView&			DetailList			()				{return *UIDetailList;}
	CUIScrollView&			MasterList			()				{return *UIMasterList;}
	void					FillPointsDetail	(const shared_str& idx);
	virtual void			Reset				();
	
	virtual bool			OnGamepadKeyAction	(int id, EUIMessages gamepad_action);
	virtual bool			OnGamepadKeyHold	(int id);

protected:
	CUIFrameWindow*			UIInfoFrame;
	CUIFrameLineWnd*		UIInfoHeader;
	CUIFrameWindow*			UICharIconFrame;
	CUIFrameLineWnd*		UICharIconHeader;
	CUIAnimatedStatic*		UIAnimatedIcon;

	CUIWindow*				UICharacterWindow;
	CUICharacterInfo*		UICharacterInfo;

	CUIScrollView*			UIMasterList;
	CUIScrollView*			UIDetailList;

	xr_vector<CUIActorStaticticHeader*> UIMasterSelectableItems;

	void					FillPointsInfo				();
	void					FillReputationDetails		(CUIXml* xml, const char* path);
	void					FillMasterPart				(CUIXml* xml, const shared_str& key_name);
	bool					MoveSelectionUp				(bool bAllowLoop);
	bool					MoveSelectionDown			(bool bAllowLoop);
	virtual CUIWindow* ui_cast_window() { return this; }
};

class CUIActorStaticticHeader :public CUIWindow, public CUISelectable
{
	CUIActorInfoWnd*						m_actorInfoWnd;
protected:
	u32				m_stored_alpha;
public:
	CUIStatic*		m_text1;
	CUIStatic*		m_text2;
	bool			m_bSelectable = false;
public:
					CUIActorStaticticHeader	(CUIActorInfoWnd* w);
	void			Init					(CUIXml* xml, const char* path, int idx_in_xml);
	virtual bool	OnMouseDown				(int mouse_btn);
	virtual void	SetSelected				(bool b);

	shared_str								m_id;

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUISelectable* ui_cast_selectable() { return this; }
};

class CUIActorStaticticDetail :public CUIWindow
{
protected:
public:
	CUIStatic*		m_text0;
	CUIStatic*		m_text1;
	CUIStatic*		m_text2;
	CUIStatic*		m_text3;
public:
	void			Init					(CUIXml* xml, const char* path, int xml_idx);
	virtual CUIWindow* ui_cast_window() { return this; }
};
