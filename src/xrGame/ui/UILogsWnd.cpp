////////////////////////////////////////////////////////////////////////////
//	Module 		: UILogsWnd.cpp
//	Created 	: 25.04.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Logs (PDA) window class implementation
//
//	Split lists (logs_list_news + logs_list_dialogs):
//	- Per-column logs_item: nested logs_list_*:logs_item, else logs_item_news/logs_item_dialogs, else logs_item.
//	- Optional stack layout in item XML: logs_itm_stack, logs_row_stack, logs_text_stack (sp_align, spacing).
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "UILogsWnd.h"
#include "PdaConstants.h"
#include "PdaUiSound.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIScrollBar.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UICheckButton.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UICursor.h"
#include "UICharacterInfo.h"
#include "UIInventoryUtilities.h"
#include "CUICalendar.h"

#include "../Actor.h"
#include "../game_news.h"
#include "../alife_registry_wrappers.h"
#include "../../xrEngine/string_table.h"
#include "UINewsItemWnd.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/Widgets/UI3tButton.h"

#include <algorithm>

#define PDA_LOGS_XML "pda_logs.xml"

u64 const day2ms			= u64( 24 * 60 * 60 * 1000 );

namespace
{
void itemToCache(CUIWindow* w)
{
	w->SetAutoDelete(false);
	w->SetParent(nullptr);
}

bool CursorInScrollList(CUIScrollView* list)
{
	if (!list || !list->IsShown())
	{
		return false;
	}

	Frect rect;
	list->GetAbsoluteRect(rect);
	Fvector2 pos = UI().GetUICursor().GetCursorPosition();
	return rect.in(pos);
}

void AttachAutoStatics(CUIXml& xml, CUIXmlInit& xmlInit, const char* tag, CUIWindow* parent)
{
	if (!parent)
	{
		return;
	}

	const int count = xml.GetNodesNum(xml.GetRoot(), tag);
	for (int i = 0; i < count; ++i)
	{
		CUIStatic* item = new CUIStatic();
		item->SetAutoDelete(true);
		parent->AttachChild(item);
		xmlInit.InitStatic(xml, tag, i, item);
	}
}

void SortTalkQueueIndices(xr_vector<u32>& indices, const GAME_NEWS_VECTOR& newsVector)
{
	if (indices.size() < 2)
	{
		return;
	}

	std::sort(indices.begin(), indices.end(),
		[&newsVector](u32 leftIdx, u32 rightIdx)
		{
			const ALife::_TIME_ID leftTime = newsVector[leftIdx].receive_time;
			const ALife::_TIME_ID rightTime = newsVector[rightIdx].receive_time;
			if (leftTime != rightTime)
			{
				return leftTime < rightTime;
			}
			return leftIdx < rightIdx;
		});
}

// Last interlocutor is matched by news_caption (display name), not NPC id.
// Empty captions and actor lines are skipped; duplicate display names share one block.
shared_str ResolveLastInterlocutorCaption(
	const xr_vector<u32>& indices, const GAME_NEWS_VECTOR& newsVector, const char* actorName)
{
	if (!actorName || !actorName[0] || indices.empty())
	{
		return {};
	}

	u32 bestIdx = u32(-1);
	for (const u32 idx : indices)
	{
		const GAME_NEWS_DATA& entry = newsVector[idx];
		if (!entry.news_caption.size())
		{
			continue;
		}
		if (0 == xr_strcmp(entry.news_caption.c_str(), actorName))
		{
			continue;
		}

		if (bestIdx == u32(-1) || entry.receive_time > newsVector[bestIdx].receive_time)
		{
			bestIdx = idx;
		}
	}

	if (bestIdx == u32(-1))
	{
		return {};
	}

	return newsVector[bestIdx].news_caption;
}

void PartitionTalkQueuePinLast(
	xr_vector<u32>& indices, const GAME_NEWS_VECTOR& newsVector, const shared_str& pinCaption)
{
	if (!pinCaption.size() || indices.size() < 2)
	{
		return;
	}

	xr_vector<u32> pinned;
	xr_vector<u32> rest;
	pinned.reserve(indices.size());
	rest.reserve(indices.size());

	for (const u32 idx : indices)
	{
		if (newsVector[idx].news_caption == pinCaption)
		{
			pinned.push_back(idx);
		}
		else
		{
			rest.push_back(idx);
		}
	}

	if (pinned.empty())
	{
		return;
	}

	indices.clear();
	indices.insert(indices.end(), pinned.begin(), pinned.end());
	indices.insert(indices.end(), rest.begin(), rest.end());
}
} // namespace

CUILogsWnd::CUILogsWnd()
{
	m_actor_ch_info			= nullptr;
	m_previous_time			= Device.dwTimeGlobal;
	m_selected_period		= 0;
	m_filter_news           = nullptr;
	m_filter_talk           = nullptr;
	m_date_caption          = nullptr;
	m_date                  = nullptr;
	m_period_caption        = nullptr;
	m_period                = nullptr;
	m_prev_period           = nullptr;
	m_next_period           = nullptr;
	m_btn_calendar          = nullptr;
	m_calendar              = nullptr;
	m_list                  = nullptr;
	m_list_news             = nullptr;
	m_list_dialogs          = nullptr;
}

CUILogsWnd::~CUILogsWnd()
{
	if (m_list)
	{
		m_list->Clear();
	}
	if (m_list_news)
	{
		m_list_news->Clear();
	}
	if (m_list_dialogs)
	{
		m_list_dialogs->Clear();
	}
	delete_data(m_items_cache);
	delete_data(_itemsCacheNews);
	delete_data(_itemsCacheTalk);
}

void CUILogsWnd::InitScrollList(LPCSTR nodeName, CUIScrollView*& outList, CUIWindow* parent)
{
	CUIWindow* attachParent = parent ? parent : this;

	outList = new CUIScrollView();
	outList->SetAutoDelete(true);
	attachParent->AttachChild(outList);
	CUIXmlInit::InitScrollView(m_uiXml, nodeName, 0, outList);
}

void CUILogsWnd::InitColumnFrames()
{
	CUIWindow* frameParent = this;
	if (m_background)
	{
		frameParent = m_background;
	}

	m_left_frame = UIHelper::CreateFrameWindow(m_uiXml, PdaXml::ContactsLeftFrame, frameParent, false);
	m_right_frame = UIHelper::CreateFrameWindow(m_uiXml, PdaXml::ContactsRightFrame, frameParent, false);

	CUIXmlInit xmlInit;

	if (m_left_frame && m_uiXml.NavigateToNode(PdaXml::LogsLeftFrameLine))
	{
		m_left_frame_line = UIHelper::CreateFrameLine(m_uiXml, PdaXml::LogsLeftFrameLine, m_left_frame, false);
		AttachAutoStatics(m_uiXml, xmlInit, "left_auto_static", m_left_frame);
	}

	if (m_right_frame && m_uiXml.NavigateToNode(PdaXml::LogsRightFrameLine))
	{
		m_right_frame_line = UIHelper::CreateFrameLine(m_uiXml, PdaXml::LogsRightFrameLine, m_right_frame, false);
		AttachAutoStatics(m_uiXml, xmlInit, "right_auto_static", m_right_frame);
	}
}

void CUILogsWnd::ApplySplitModeUi()
{
	if (m_filter_news)
	{
		m_filter_news->Show(false);
	}
	if (m_filter_talk)
	{
		m_filter_talk->Show(false);
	}
}

CUIScrollView* CUILogsWnd::ActiveScrollList()
{
	if (!m_use_split_lists)
	{
		return m_list;
	}

	if (CursorInScrollList(m_list_dialogs))
	{
		return m_list_dialogs;
	}
	if (CursorInScrollList(m_list_news))
	{
		return m_list_news;
	}
	return m_list_news;
}

shared_str CUILogsWnd::ResolveItemTemplatePath(const char* listNode, const char* siblingNode)
{
	string512 nestedPath;
	xr_strconcat(nestedPath, listNode, ":logs_item");

	if (m_uiXml.NavigateToNode(nestedPath, 0))
	{
		return nestedPath;
	}
	if (m_uiXml.NavigateToNode(siblingNode, 0))
	{
		return siblingNode;
	}
	if (m_uiXml.NavigateToNode("logs_item", 0))
	{
		return "logs_item";
	}

	Msg("! CUILogsWnd: missing logs_item template (tried %s, %s, logs_item) in [%s]",
		nestedPath, siblingNode, m_uiXml.m_xml_file_name);
	return "logs_item";
}

CUIWindow::WINDOW_LIST& CUILogsWnd::ItemsCacheForType(bool forNews)
{
	return forNews ? _itemsCacheNews : _itemsCacheTalk;
}

void CUILogsWnd::ClearListToCache(CUIScrollView* list, bool forNews)
{
	if (!list || list->Empty())
	{
		return;
	}

	CUIWindow::WINDOW_LIST& cache = m_use_split_lists ? ItemsCacheForType(forNews) : m_items_cache;

	xrCriticalSectionGuard guard(list->csUi);
	cache.insert(cache.end(), list->Items().begin(), list->Items().end());
	list->Items().clear();
	std::for_each(cache.begin(), cache.end(), itemToCache);
}

void CUILogsWnd::FlushReadyItems(WINDOW_LIST& ready, CUIScrollView* list)
{
	if (ready.empty() || !list)
	{
		return;
	}

	for (CUIWindow* w : ready)
	{
		list->AddWindow(w, true);
	}
	ready.clear();
}

void CUILogsWnd::ScrollAllListsToBegin()
{
	if (m_use_split_lists)
	{
		if (m_list_news)
		{
			m_list_news->ScrollToBegin();
		}
		if (m_list_dialogs)
		{
			m_list_dialogs->ScrollToBegin();
		}
		return;
	}

	if (m_list)
	{
		m_list->ScrollToBegin();
	}
}

void CUILogsWnd::ScrollAllListsToEnd()
{
	if (m_use_split_lists)
	{
		if (m_list_news)
		{
			m_list_news->ScrollToEnd();
		}
		if (m_list_dialogs)
		{
			m_list_dialogs->ScrollToEnd();
		}
		return;
	}

	if (m_list)
	{
		m_list->ScrollToEnd();
	}
}

void CUILogsWnd::Show( bool status )
{
	m_ctrl_press = false;
	if ( status )
	{
		if (m_actor_ch_info)
			m_actor_ch_info->InitCharacter(Actor());
		m_selected_period = GetShiftPeriod( Level().GetGameTime(), 0 );
		m_need_reload = true;
		SyncCalendarState();
		Update();
	}
	else if (m_calendar)
	{
		m_calendar->HidePopup();
	}
	inherited::Show( status );
}

void CUILogsWnd::Update()
{
	inherited::Update();
	if( m_need_reload )
		ReLoadNews();
	if (IsShown() && m_date && m_date_caption)
	{
		if (Device.dwTimeGlobal - m_previous_time > 1000)
		{
			m_previous_time = Device.dwTimeGlobal;
			m_date->SetText(InventoryUtilities::Get_GameTimeAndDate_AsString().c_str());

			m_date_caption->AdjustWidthToText();
			Fvector2 pos = m_date_caption->GetWndPos();
			pos.x = m_date->GetWndPos().x - m_date_caption->GetWidth() - 5.0f;
			m_date_caption->SetWndPos(pos);
		}
	}

	if (m_use_split_lists)
	{
		FlushReadyItems(m_items_ready_news, m_list_news);
		FlushReadyItems(m_items_ready_talk, m_list_dialogs);
	}
	else
	{
		FlushReadyItems(m_items_ready, m_list);
	}
}

void CUILogsWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	inherited::SendMessage( pWnd, msg, pData );
	CUIWndCallback::OnEvent( pWnd, msg, pData );
}

void CUILogsWnd::Init()
{
	m_uiXml.Load( CONFIG_PATH, UI_PATH, PDA_LOGS_XML );

	CUIXmlInit::InitWindow( m_uiXml, "main_wnd", 0, this );

	m_background = UIHelper::CreateFrameWindow(m_uiXml, "background", this, false);
	if (!m_background)
		m_background2 = UIHelper::CreateFrameLine(m_uiXml, "background", this, false);
	m_center_background = UIHelper::CreateFrameWindow(m_uiXml, "center_background", this, false);

	InitColumnFrames();

	if (m_uiXml.NavigateToNode("actor_ch_info"))
	{
		m_actor_ch_info = new CUICharacterInfo();
		m_actor_ch_info->SetAutoDelete(true);
		AttachChild(m_actor_ch_info);
		m_actor_ch_info->InitCharacterInfo(&m_uiXml, "actor_ch_info");
	}

	if (!m_center_background && m_uiXml.NavigateToNode("center_background"))
		m_center_background2 = UIHelper::CreateStatic(m_uiXml, "center_background", this);

	m_center_caption	= UIHelper::CreateStatic( m_uiXml, "center_caption", this );

	string256 buf;
	xr_strcpy( buf, sizeof(buf), m_center_caption->GetText() );
	xr_strcat( buf, sizeof(buf), g_pStringTable->translate("ui_logs_center_caption").c_str() );
	m_center_caption->SetText( buf );

	const bool hasSplitLists = m_uiXml.NavigateToNode("logs_list_news") && m_uiXml.NavigateToNode("logs_list_dialogs");
	const bool hasLegacyList = m_uiXml.NavigateToNode("logs_list");

	if (hasSplitLists)
	{
		m_use_split_lists = true;
		CUIWindow* newsParent = this;
		CUIWindow* dialogsParent = this;
		if (m_left_frame)
		{
			newsParent = m_left_frame;
		}
		if (m_right_frame)
		{
			dialogsParent = m_right_frame;
		}
		InitScrollList("logs_list_news", m_list_news, newsParent);
		InitScrollList("logs_list_dialogs", m_list_dialogs, dialogsParent);

		_itemTemplateNews = ResolveItemTemplatePath("logs_list_news", "logs_item_news");
		_itemTemplateDialogs = ResolveItemTemplatePath("logs_list_dialogs", "logs_item_dialogs");
		InitTalkDialogsToolbar();
	}
	else if (hasLegacyList)
	{
		m_use_split_lists = false;
		InitScrollList("logs_list", m_list, this);
	}
	else
	{
		Msg("! CUILogsWnd: missing [logs_list] or [logs_list_news]+[logs_list_dialogs] in [%s]", m_uiXml.m_xml_file_name);
		R_ASSERT2(hasLegacyList, "CUILogsWnd: logs list node is required");
	}

	if (m_uiXml.NavigateToNode("filter_news"))
	{
		m_filter_news = UIHelper::CreateCheck(m_uiXml, "filter_news", this);
		if (m_filter_news)
		{
			m_filter_news->SetCheck(true);
		}
	}
	if (m_uiXml.NavigateToNode("filter_talk"))
	{
		m_filter_talk = UIHelper::CreateCheck(m_uiXml, "filter_talk", this);
		if (m_filter_talk)
		{
			m_filter_talk->SetCheck(true);
		}
	}

	if (m_use_split_lists)
	{
		ApplySplitModeUi();
	}

	if (m_uiXml.NavigateToNode("date_caption"))
		m_date_caption = UIHelper::CreateStatic(m_uiXml, "date_caption", this);

	if (m_uiXml.NavigateToNode("date"))
		m_date = UIHelper::CreateStatic(m_uiXml, "date", this);

	if (m_date || m_date_caption)
	{
		R_ASSERT3(m_date && m_date_caption,
			"Please, provide both [date] and [date_caption] tags in xml file", m_uiXml.m_xml_file_name);
	}

	if (m_uiXml.NavigateToNode("period_caption"))
	{
		m_period_caption = UIHelper::CreateStatic(m_uiXml, "period_caption", this);
	}
	if (m_uiXml.NavigateToNode("period"))
	{
		m_period = UIHelper::CreateStatic(m_uiXml, "period", this);
	}

	if (m_uiXml.NavigateToNode("btn_prev_period"))
	{
		m_prev_period = UIHelper::Create3tButton(m_uiXml, "btn_prev_period", this);
	}
	if (m_uiXml.NavigateToNode("btn_next_period"))
	{
		m_next_period = UIHelper::Create3tButton(m_uiXml, "btn_next_period", this);
	}
	if (m_uiXml.NavigateToNode("btn_calendar"))
	{
		m_btn_calendar = UIHelper::Create3tButton(m_uiXml, "btn_calendar", this);
	}

	m_gamepad_legend = UIHelper::CreateGamepadLegend( m_uiXml, "gamepad_legend", this, false );

	if (m_filter_news)
	{
		Register(m_filter_news);
		AddCallback(m_filter_news, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUILogsWnd::UpdateChecks));
	}
	if (m_filter_talk)
	{
		Register(m_filter_talk);
		AddCallback(m_filter_talk, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUILogsWnd::UpdateChecks));
	}
	if (m_prev_period)
	{
		Register(m_prev_period);
		AddCallback(m_prev_period, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUILogsWnd::PrevPeriod));
	}
	if (m_next_period)
	{
		Register(m_next_period);
		AddCallback(m_next_period, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUILogsWnd::NextPeriod));
	}
	if (m_btn_calendar)
	{
		Register(m_btn_calendar);
		AddCallback(m_btn_calendar, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUILogsWnd::ToggleCalendarPopup));
	}

	m_start_game_time = Level().GetStartGameTime();
	m_start_game_time = GetShiftPeriod( m_start_game_time, 0 );

	m_calendar = new CUICalendar();
	if (m_calendar->InitFromXml(m_uiXml, this, m_btn_calendar))
	{
		m_calendar->SetOnDaySelected(xr_delegate<void(ALife::_TIME_ID)>(this, &CUILogsWnd::OnCalendarDaySelected));
		SyncCalendarState();
	}
	else
	{
		xr_delete(m_calendar);
	}

	if (m_pUiSounds)
	{
		m_pUiSounds->LoadSubdialog(m_uiXml, "main_wnd");
	}
}

void CUILogsWnd::ReLoadNews()
{
	m_news_in_queue.clear();
	m_talk_in_queue.clear();
	CActor* pActor = Actor();

	if(pActor == nullptr) {
		m_need_reload = false;
		return;
	}

	const char* date_str = InventoryUtilities::GetDateAsString(m_selected_period, InventoryUtilities::edpDateToDay).c_str();
	if (m_period)
	{
		m_period->TextItemControl()->SetText(date_str);
	}
	if (m_period && m_period_caption && m_prev_period)
	{
		Fvector2 pos = m_period_caption->GetWndPos();
		pos.x = m_period->GetWndPos().x - m_period_caption->GetWidth() - m_prev_period->GetWidth() - 5.0f;
		m_period_caption->SetWndPos(pos);
	}

	ALife::_TIME_ID end_period = GetShiftPeriod(m_selected_period, 1);

	GAME_NEWS_VECTOR& news_vector = pActor->game_news_registry->registry().objects();

	const bool filter_news = m_filter_news ? m_filter_news->GetCheck() : true;
	const bool filter_talk = m_filter_talk ? m_filter_talk->GetCheck() : true;

	GAME_NEWS_VECTOR::iterator ib = news_vector.begin();
	GAME_NEWS_VECTOR::iterator ie = news_vector.end();
	for(u32 idx = 0; ib != ie; ++ib, ++idx)
	{
		bool add = false;
		GAME_NEWS_DATA& gn = (*ib);

		if (m_use_split_lists)
		{
			add = gn.m_type == GAME_NEWS_DATA::eNews || gn.m_type == GAME_NEWS_DATA::eTalk;
		}
		else if(gn.m_type == GAME_NEWS_DATA::eNews && filter_news)
		{
			add = true;
		}
		else if(gn.m_type == GAME_NEWS_DATA::eTalk && filter_talk)
		{
			add = true;
		}

		if(gn.receive_time < m_selected_period || end_period < gn.receive_time)
		{
			add = false;
		}

		if(add)
		{
			if (m_use_split_lists)
			{
				if (gn.m_type == GAME_NEWS_DATA::eNews)
				{
					m_news_in_queue.push_back(idx);
				}
				else
				{
					m_talk_in_queue.push_back(idx);
				}
			}
			else
			{
				m_news_in_queue.push_back(idx);
			}
		}
	}
	if (m_use_split_lists && !m_talk_in_queue.empty())
	{
		SortTalkQueueIndices(m_talk_in_queue, news_vector);
		if (_talkPinLastContact && pActor)
		{
			const shared_str pinCaption = ResolveLastInterlocutorCaption(
				m_talk_in_queue, news_vector, pActor->NameReal());
			if (pinCaption.size())
			{
				PartitionTalkQueuePinLast(m_talk_in_queue, news_vector, pinCaption);
			}
		}
	}
	m_need_reload = false;

	if (m_use_split_lists)
	{
		ClearListToCache(m_list_news, true);
		ClearListToCache(m_list_dialogs, false);
	}
	else
	{
		ClearListToCache(m_list, true);
	}
	PerformWork();
}

void CUILogsWnd::ProcessIndexQueue(xr_vector<u32>& queue, u32 batchSize, bool popFromBack)
{
	if (queue.empty() || !Actor())
	{
		return;
	}

	const u32 count = std::min(batchSize, (u32)queue.size());
	GAME_NEWS_VECTOR& news_vector = Actor()->game_news_registry->registry().objects();

	for (u32 i = 0; i < count; ++i)
	{
		const u32 idx = popFromBack ? queue.back() : queue.front();
		if (popFromBack)
		{
			queue.pop_back();
		}
		else
		{
			queue.erase(queue.begin());
		}
		AddNewsItem(news_vector[idx]);
	}
}

void CUILogsWnd::PerformWork()
{
	if (m_use_split_lists)
	{
		ProcessIndexQueue(m_news_in_queue, 30, true);
		ProcessIndexQueue(m_talk_in_queue, 30, _talkSortNewestFirst);
		return;
	}

	ProcessIndexQueue(m_news_in_queue, 30, true);
}

void CUILogsWnd::InitTalkDialogsToolbar()
{
	if (!m_use_split_lists || !m_list_dialogs)
	{
		return;
	}

	if (m_uiXml.NavigateToNode("logs_list_dialogs:btn_talk_sort_oldest"))
	{
		_btnTalkSortOldest = UIHelper::Create3tButton(
			m_uiXml, "logs_list_dialogs:btn_talk_sort_oldest", m_list_dialogs);
	}
	if (m_uiXml.NavigateToNode("logs_list_dialogs:btn_talk_sort_newest"))
	{
		_btnTalkSortNewest = UIHelper::Create3tButton(
			m_uiXml, "logs_list_dialogs:btn_talk_sort_newest", m_list_dialogs);
	}
	if (m_uiXml.NavigateToNode("logs_list_dialogs:btn_talk_pin_last"))
	{
		_btnTalkPinLast = UIHelper::Create3tButton(
			m_uiXml, "logs_list_dialogs:btn_talk_pin_last", m_list_dialogs);
	}

	if (_btnTalkSortOldest)
	{
		Register(_btnTalkSortOldest);
		AddCallback(_btnTalkSortOldest, BUTTON_CLICKED,
			CUIWndCallback::void_function(this, &CUILogsWnd::OnTalkSortOldest));
	}
	if (_btnTalkSortNewest)
	{
		Register(_btnTalkSortNewest);
		AddCallback(_btnTalkSortNewest, BUTTON_CLICKED,
			CUIWndCallback::void_function(this, &CUILogsWnd::OnTalkSortNewest));
	}
	if (_btnTalkPinLast)
	{
		Register(_btnTalkPinLast);
		AddCallback(_btnTalkPinLast, BUTTON_CLICKED,
			CUIWndCallback::void_function(this, &CUILogsWnd::OnTalkPinLastToggle));
	}

	UpdateTalkDialogsToolbarVisual();
}

void CUILogsWnd::UpdateTalkDialogsToolbarVisual()
{
	if (_btnTalkSortOldest)
	{
		_btnTalkSortOldest->SetHighlighted(!_talkSortNewestFirst);
	}
	if (_btnTalkSortNewest)
	{
		_btnTalkSortNewest->SetHighlighted(_talkSortNewestFirst);
	}
	if (_btnTalkPinLast)
	{
		_btnTalkPinLast->SetHighlighted(_talkPinLastContact);
	}
}

void CUILogsWnd::OnTalkSortOldest(CUIWindow* w, void* d)
{
	if (!_talkSortNewestFirst)
	{
		return;
	}

	_talkSortNewestFirst = false;
	UpdateTalkDialogsToolbarVisual();

	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}

	m_need_reload = true;
}

void CUILogsWnd::OnTalkSortNewest(CUIWindow* w, void* d)
{
	if (_talkSortNewestFirst)
	{
		return;
	}

	_talkSortNewestFirst = true;
	UpdateTalkDialogsToolbarVisual();

	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}

	m_need_reload = true;
}

void CUILogsWnd::OnTalkPinLastToggle(CUIWindow* w, void* d)
{
	_talkPinLastContact = !_talkPinLastContact;
	UpdateTalkDialogsToolbarVisual();

	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}

	m_need_reload = true;
}

CUIWindow* CUILogsWnd::CreateItem(bool forNews)
{
	CUINewsItemWnd* itmRes = new CUINewsItemWnd();
	if (m_use_split_lists)
	{
		const char* templatePath = forNews
			? _itemTemplateNews.c_str()
			: _itemTemplateDialogs.c_str();
		itmRes->Init(m_uiXml, templatePath, true);
	}
	else
	{
		itmRes->Init(m_uiXml, "logs_item", false);
	}
	return itmRes;
}

CUIWindow* CUILogsWnd::ItemFromCache(bool forNews)
{
	CUIWindow::WINDOW_LIST& cache = m_use_split_lists ? ItemsCacheForType(forNews) : m_items_cache;

	CUIWindow* itmRes = nullptr;
	if (cache.empty())
	{
		itmRes = CreateItem(forNews);
	}
	else
	{
		itmRes = cache.back();
		cache.pop_back();
	}
	return itmRes;
}

void CUILogsWnd::AddNewsItem(GAME_NEWS_DATA& news_data)
{
	CUIWindow* news_itm_w		= ItemFromCache(news_data.m_type == GAME_NEWS_DATA::eNews);
	CUINewsItemWnd*	news_itm	= smart_cast<CUINewsItemWnd*>(news_itm_w);
	news_itm->Setup				(news_data);

	if (m_use_split_lists)
	{
		WINDOW_LIST& ready = news_data.m_type == GAME_NEWS_DATA::eNews ? m_items_ready_news : m_items_ready_talk;
		ready.push_back(news_itm);
	}
	else
	{
		m_items_ready.push_back(news_itm);
	}
}

void CUILogsWnd::UpdateChecks( CUIWindow* w, void* d )
{
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}

	if (m_use_split_lists)
	{
		SyncCalendarState();
		return;
	}

	m_need_reload = true;
	SyncCalendarState();
}

void CUILogsWnd::PrevPeriod( CUIWindow* w, void* d )
{
	ALife::_TIME_ID	current_period = m_selected_period;
	m_selected_period = GetShiftPeriod( m_selected_period, -1 );
	if ( m_selected_period < m_start_game_time )
	{
		m_selected_period = m_start_game_time;
	}
	if(current_period != m_selected_period)
	{
		if (m_pUiSounds)
		{
			m_pUiSounds->Play(EPdaUiSound::ListSelect);
		}
		m_need_reload = true;
	}
	SyncCalendarState();
}

void CUILogsWnd::NextPeriod( CUIWindow* w, void* d )
{
	ALife::_TIME_ID	current_period = m_selected_period;
	m_selected_period = GetShiftPeriod( m_selected_period, 1 ); // +1
	ALife::_TIME_ID game_time = GetShiftPeriod( Level().GetGameTime(), 0 );
	if ( m_selected_period > game_time  )
	{
		m_selected_period = game_time;
	}
	if(current_period != m_selected_period)
	{
		if (m_pUiSounds)
		{
			m_pUiSounds->Play(EPdaUiSound::ListSelect);
		}
		m_need_reload = true;
	}
	SyncCalendarState();
}

ALife::_TIME_ID CUILogsWnd::GetShiftPeriod( ALife::_TIME_ID datetime, int shift_day )
{
	datetime -= (datetime % day2ms);
	datetime += (u64)shift_day * day2ms;
	return datetime;
}

void CUILogsWnd::SyncCalendarState()
{
	if (!m_calendar || !m_calendar->HasUi())
	{
		return;
	}

	m_start_game_time = GetShiftPeriod(Level().GetStartGameTime(), 0);

	const bool filterNews = m_use_split_lists || (m_filter_news ? m_filter_news->GetCheck() : true);
	const bool filterTalk = m_use_split_lists || (m_filter_talk ? m_filter_talk->GetCheck() : true);
	m_calendar->UpdateState(m_start_game_time, m_selected_period, filterNews, filterTalk);
}

void CUILogsWnd::OnCalendarDaySelected(ALife::_TIME_ID period)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::ListSelect);
	}
	m_selected_period = period;
	m_need_reload = true;
	ReLoadNews();
}

void CUILogsWnd::ToggleCalendarPopup(CUIWindow* w, void* d)
{
	if (m_calendar)
	{
		if (m_pUiSounds)
		{
			m_pUiSounds->PlayPanel(!m_calendar->IsShown());
		}
		SyncCalendarState();
		m_calendar->TogglePopup();
	}
}

bool CUILogsWnd::OnKeyboardAction( int dik, EUIMessages keyboard_action )
{
	if ( keyboard_action == WINDOW_KEY_PRESSED )
	{
		switch ( dik )
		{
		case SDL_SCANCODE_UP:
		case SDL_SCANCODE_DOWN:
		case SDL_SCANCODE_PAGEUP:
		case SDL_SCANCODE_PAGEDOWN:
			{
				on_scroll_keys( dik );
				return true;
			}break;
		case SDL_SCANCODE_RCTRL:
		case SDL_SCANCODE_LCTRL:
			{
				m_ctrl_press = true;
				return true;
			}break;
		}
	}
	m_ctrl_press = false;
	return inherited::OnKeyboardAction( dik, keyboard_action );
}

bool CUILogsWnd::OnKeyboardHold( int dik )
{
	switch ( dik )
	{
	case SDL_SCANCODE_UP:
	case SDL_SCANCODE_DOWN:
	case SDL_SCANCODE_PAGEUP:
	case SDL_SCANCODE_PAGEDOWN:
		{
			on_scroll_keys( dik );
			return true;
		}break;
	}
	return inherited::OnKeyboardHold( dik );
}

bool CUILogsWnd::OnGamepadKeyAction(int key, EUIMessages gamepad_action)
{
	if (WINDOW_KEY_PRESSED == gamepad_action)
	{
		switch (get_binded_action(key, agUILogMenu))
		{
			case kPDA_LOG_TO_START:
			{
				ScrollAllListsToBegin();
				break;
			}
			case kPDA_LOG_TO_END:
			{
				ScrollAllListsToEnd();
				break;
			}
			case kPDA_LOG_DATE_PREV:
			{
				if (m_prev_period)
				{
					m_prev_period->OnClick();
				}
				break;
			}
			case kPDA_LOG_DATE_NEXT:
			{
				if (m_next_period)
				{
					m_next_period->OnClick();
				}
				break;
			}
			case kPDA_LOG_SHOW_DIALOGS:
			{
				if (!m_use_split_lists && m_filter_talk)
				{
					m_filter_talk->SetCheck(!m_filter_talk->GetCheck());
					m_filter_talk->SendClickCallback();
				}
				break;
			}
			case kPDA_LOG_SHOW_NEWS:
			{
				if (!m_use_split_lists && m_filter_news)
				{
					m_filter_news->SetCheck(!m_filter_news->GetCheck());
					m_filter_news->SendClickCallback();
				}
				break;
			}
			case kPDA_LOG_SCROLL_UP:
			{
				on_scroll_keys(SDL_SCANCODE_UP, 64);
				break;
			}
			case kPDA_LOG_SCROLL_DOWN:
			{
				on_scroll_keys(SDL_SCANCODE_DOWN, 64);
				break;
			}
			return true;
		}
	}

	return inherited::OnGamepadKeyAction(key, gamepad_action);
}

bool CUILogsWnd::OnGamepadKeyHold(int key)
{
	switch (get_binded_action(key, agUILogMenu))
	{
		case kPDA_LOG_SCROLL_UP:
		{
			on_scroll_keys(SDL_SCANCODE_UP, 64);
			break;
		}
		case kPDA_LOG_SCROLL_DOWN:
		{
			on_scroll_keys(SDL_SCANCODE_DOWN, 64);
			break;
		}
		return true;
	}

	return inherited::OnGamepadKeyHold(key);
}

void CUILogsWnd::on_scroll_keys( int dik, int step )
{
	CUIScrollView* list = ActiveScrollList();
	if (!list || !list->ScrollBar())
	{
		return;
	}

	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::ListScroll, true);
	}

	switch ( dik )
	{
	case SDL_SCANCODE_UP:
		{
			int orig = list->ScrollBar()->GetStepSize();
			list->ScrollBar()->SetStepSize( step );
			list->ScrollBar()->TryScrollDec();
			list->ScrollBar()->SetStepSize( orig );
			break;
		}
	case SDL_SCANCODE_DOWN:
		{
			int orig = list->ScrollBar()->GetStepSize();
			list->ScrollBar()->SetStepSize( step );
			list->ScrollBar()->TryScrollInc();
			list->ScrollBar()->SetStepSize( orig );
			break;
		}
	case SDL_SCANCODE_PAGEUP:
		{
			if ( m_ctrl_press )
			{
				ScrollAllListsToBegin();
				break;
			}
			list->ScrollBar()->TryScrollDec();
			break;
		}
	case SDL_SCANCODE_PAGEDOWN:
		{
			if ( m_ctrl_press )
			{
				ScrollAllListsToEnd();
				break;
			}
			list->ScrollBar()->TryScrollInc();
			break;
		}
	}// switch

}
