#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../../xrUI/UIXmlInit.h"

class CUIFrameWindow;
class CUIFrameLineWnd;
class CUIAnimatedStatic;
class CUIMapWnd;
class CUI3tButton;
class CUITabControl;
class CGameTask;
class CUITaskDescrWnd;
class CUIScrollView;
class CUITaskItemLegacy;
class CUITaskRootItem;
class CUIGamepadLegend;

class CUIEventsWnd	:public CUIWindow, public CUIWndCallback{
	typedef CUIWindow			inherited;
	enum ETaskFilters{	eActiveTask			=	0,
						eAccomplishedTask,
						eFailedTask,
						eMaxTask};
	enum EEventWndFlags{
						flNeedReload	=(1<<0),
						flMapMode		=(1<<1),
	};
	Flags16						m_flags;
	ETaskFilters				m_currFilter;
	CUIFrameWindow*				m_UILeftFrame;
	CUIWindow*					m_UIRightWnd;
	CUIFrameLineWnd*			m_UILeftHeader;
	CUIAnimatedStatic*			m_UIAnimation;
	CUITaskDescrWnd*			m_UITaskInfoWnd;
	CUIScrollView*				m_ListWnd;
	CUITabControl*				m_TaskFilter;

	xr_vector<CUITaskItemLegacy*>		m_SubtaskItemList; // For controller navigation

	bool						Filter					(CGameTask* t);
	void 				OnFilterChanged			(CUIWindow*,void*);
	void						ReloadList				(bool bClearOnly);

	bool						MoveSelectionDown		(bool bAllowLoop);
	bool						MoveSelectionUp			(bool bAllowLoop);
	CUITaskRootItem*			GetTaskRootItem			(CGameTask* t);
	void						SetSubtaskSelected		(CUITaskItemLegacy* pTask);
	void						UpdateGamepadLegend		();

public:
	void						SetDescriptionMode		(bool bMap);
	bool						GetDescriptionMode		();
	void						ShowDescription			(CGameTask* t, int idx);
	bool						ItemHasDescription		(CUITaskItemLegacy*);
	void						DrawHint				();
public:

								CUIEventsWnd			();
	virtual						~CUIEventsWnd			();
	virtual void				SendMessage				(CUIWindow* pWnd, s16 msg, void* pData);
			void				Init					();
	virtual void				Update					();
	virtual void				Draw					();
	virtual void				Show					(bool status);
			void				Reload					();
	virtual void				Reset					();
	virtual bool				OnGamepadKeyAction		(int id, EUIMessages gamepad_action);
	virtual bool				OnGamepadKeyHold		(int id);

	CUIXml						m_ui_task_item_xml;
	CUIGamepadLegend*			m_gamepad_legend = nullptr;
	CUIMapWnd*					m_UIMapWnd;

	virtual CUIWindow* ui_cast_window() { return this; }
};
