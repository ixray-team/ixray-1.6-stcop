////////////////////////////////////////////////////////////////////////////
//	Module 		: UILogsWnd.h
//	Created 	: 25.04.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Logs (PDA) window class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIWndCallback.h"

#include "../ai_space.h"
#include "../../xrEngine/AI/alife_space.h"
#include "../../xrUI/xrUIXmlParser.h"

class CUIStatic;
class CUIXml;
class CUIProgressBar;
class CUIFrameLineWnd;
class CUIFrameWindow;
class CUICharacterInfo;
class CUIScrollView;
class CUI3tButton;
class CUICheckButton;
#include "../game_news.h"

class CUINewsItemWnd;
class CUIGamepadLegend;
class CUICalendar;
class CPdaUiSounds;

class CUILogsWnd final : public CUIWindow, public CUIWndCallback
{
private:
	using inherited = CUIWindow;

	CUIFrameWindow*		m_background;
	CUIFrameLineWnd*	m_background2;
	CUIFrameWindow*		m_center_background;
	CUIStatic*			m_center_background2;

	CUIFrameWindow*		m_left_frame = nullptr;
	CUIFrameWindow*		m_right_frame = nullptr;
	CUIFrameLineWnd*	m_left_frame_line = nullptr;
	CUIFrameLineWnd*	m_right_frame_line = nullptr;

	CUIStatic*			m_center_caption;
	CUICharacterInfo*	m_actor_ch_info;

	CUICheckButton*		m_filter_news = nullptr;
	CUICheckButton*		m_filter_talk = nullptr;

	CUIStatic*			m_date_caption;
	CUIStatic*			m_date;

	CUIStatic*			m_period_caption = nullptr;
	CUIStatic*			m_period = nullptr;

	ALife::_TIME_ID		m_start_game_time;
	ALife::_TIME_ID		m_selected_period;

	CUI3tButton*		m_prev_period = nullptr;
	CUI3tButton*		m_next_period = nullptr;
	CUI3tButton*		m_btn_calendar = nullptr;
	CUICalendar*		m_calendar = nullptr;
	bool				m_ctrl_press;

	bool				m_use_split_lists = false;
	CUIScrollView*		m_list = nullptr;
	CUIScrollView*		m_list_news = nullptr;
	CUIScrollView*		m_list_dialogs = nullptr;

	shared_str			_itemTemplateNews;
	shared_str			_itemTemplateDialogs;
	WINDOW_LIST			_itemsCacheNews;
	WINDOW_LIST			_itemsCacheTalk;

	bool				_talkSortNewestFirst = false;
	bool				_talkPinLastContact = false;
	CUI3tButton*		_btnTalkSortOldest = nullptr;
	CUI3tButton*		_btnTalkSortNewest = nullptr;
	CUI3tButton*		_btnTalkPinLast = nullptr;

	u32					m_previous_time;
	bool				m_need_reload;
	WINDOW_LIST			m_items_cache;
	WINDOW_LIST			m_items_ready;
	WINDOW_LIST			m_items_ready_news;
	WINDOW_LIST			m_items_ready_talk;
	xr_vector<u32>		m_news_in_queue;
	xr_vector<u32>		m_talk_in_queue;

	CUIXml				m_uiXml;
	CPdaUiSounds*		m_pUiSounds = nullptr;

	shared_str			ResolveItemTemplatePath(const char* listNode, const char* siblingNode);
	CUIWindow::WINDOW_LIST&	ItemsCacheForType	(bool forNews);
	CUIWindow*			CreateItem			(bool forNews);
	CUIWindow*			ItemFromCache		(bool forNews);
	void				InitScrollList		(LPCSTR nodeName, CUIScrollView*& outList, CUIWindow* parent = nullptr);
	void				InitColumnFrames	();
	CUIScrollView*		ActiveScrollList	();
	void				ClearListToCache	(CUIScrollView* list, bool forNews);
	void				ApplySplitModeUi	();
	void				FlushReadyItems		(WINDOW_LIST& ready, CUIScrollView* list);
	void				ScrollAllListsToBegin();
	void				ScrollAllListsToEnd	();
	void				ProcessIndexQueue	(xr_vector<u32>& queue, u32 batchSize, bool popFromBack);
	void				InitTalkDialogsToolbar	();
	void				UpdateTalkDialogsToolbarVisual();
	void				OnTalkSortOldest	(CUIWindow* w, void* d);
	void				OnTalkSortNewest	(CUIWindow* w, void* d);
	void				OnTalkPinLastToggle	(CUIWindow* w, void* d);

public:
						CUILogsWnd			();
						~CUILogsWnd			() override;

			void		Init				();
			void		SetUiSounds			(CPdaUiSounds* uiSounds) { m_pUiSounds = uiSounds; }

	void 				Show				( bool status ) override;
	void				Update				() override;
	void				SendMessage			( CUIWindow* pWnd, s16 msg, void* pData ) override;

	bool				OnKeyboardAction	(int dik, EUIMessages keyboard_action) override;
	bool				OnKeyboardHold		(int dik) override;
	bool				OnGamepadKeyAction	(int key, EUIMessages gamepad_action) override;
	bool				OnGamepadKeyHold	(int key) override;

	IC		void		UpdateNews			()
	{
		m_need_reload = true;
		SyncCalendarState();
	}
	void		PerformWork			();

	CUIGamepadLegend*	m_gamepad_legend = nullptr;

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
			void		ReLoadNews			();
			void		AddNewsItem			( GAME_NEWS_DATA& news_data );
	ALife::_TIME_ID		GetShiftPeriod		( ALife::_TIME_ID datetime, int shift_day );
			void		SyncCalendarState	();
			void		OnCalendarDaySelected( ALife::_TIME_ID period );

			void 	UpdateChecks	( CUIWindow* w, void* d);
			void 	PrevPeriod		( CUIWindow* w, void* d);
			void 	NextPeriod		( CUIWindow* w, void* d);
			void	ToggleCalendarPopup	( CUIWindow* w, void* d );

			void 		on_scroll_keys		( int dik, int step = 1 );

/*
protected:
	void		add_faction			( CUIXml& xml, shared_str const& faction_id );
	void		clear_all_factions		();
	bool		SortingLessFunction		( CUIWindow* left, CUIWindow* right );
*/
}; // class CUILogsWnd
