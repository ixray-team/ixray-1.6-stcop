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
	virtual ~CUIActorInfoWnd	() override;
	virtual void			Init				();
	virtual void			Show				(bool status) override;
	CUIScrollView&			DetailList			()				{return *UIDetailList;}
	CUIScrollView&			MasterList			()				{return *UIMasterList;}
	void					FillPointsDetail	(const shared_str& idx);
	virtual void			Reset				() override;

	virtual bool			OnGamepadKeyAction	(int id, EUIMessages gamepad_action) override;
	virtual bool			OnGamepadKeyHold	(int id) override;

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
	virtual CUIWindow* ui_cast_window() override { return this; }
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
	virtual bool	OnMouseDown				(int mouse_btn) override;
	virtual void	SetSelected				(bool b) override;

	shared_str								m_id;

	virtual CUIWindow* ui_cast_window() override { return this; }
	virtual CUISelectable* ui_cast_selectable() override { return this; }
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
	virtual CUIWindow* ui_cast_window() override { return this; }
};
