////////////////////////////////////////////////////////////////////////////
//	Module 		: UISecondTaskWnd.cpp
//	Created 	: 30.05.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Secondary Task Wnd class impl
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "UISecondTaskWnd.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"

#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UICheckButton.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "UITaskWnd.h"
#include "../../xrEngine/string_table.h"
#include "../GameTaskDefs.h"
#include "../GameTask.h"
#include "../map_location.h"
#include "UIInventoryUtilities.h"
#include "../Level.h"
#include "../GametaskManager.h"
#include "../Actor.h"
#include "PdaConstants.h"
#include "PdaUiSound.h"
#include "../../xrUI/Widgets/UIMessages.h"

namespace
{
bool IsWindowOrChildOf(CUIWindow* child, CUIWindow* ancestor)
{
	while (child)
	{
		if (child == ancestor)
		{
			return true;
		}
		child = child->GetParent();
	}
	return false;
}

bool TaskHasMapTarget(const CGameTask* task)
{
	return task && task->HasActiveMapTarget();
}
} // namespace

UITaskListWnd::UITaskListWnd()
	: hint_wnd(nullptr), m_background(nullptr), m_list(nullptr),
	m_caption(nullptr), m_bt_close(nullptr), m_filter_tabs(nullptr),
	_storylineTaskItem(nullptr), _btnStorylineTaskFocus(nullptr),
	m_orig_h(0), m_filter(ETaskListFilter::All)
{
	ActionRepeaters()->Register(this, kPDA_TASKS_NEXT);
	ActionRepeaters()->Register(this, kPDA_TASKS_PREV);
}

UITaskListWnd::~UITaskListWnd()
{
	ActionRepeaters()->UnregisterOwner(this);
}

void UITaskListWnd::init_from_xml( CUIXml& xml, const char* path )
{
	VERIFY( hint_wnd );
	CUIXmlInit::InitWindow( xml, path, 0, this );

	XML_NODE*  stored_root = xml.GetLocalRoot();
	XML_NODE*  tmpl_root   = xml.NavigateToNode( path, 0 );
	xml.SetLocalRoot( tmpl_root );
	
	m_background = UIHelper::CreateFrameWindow( xml, "background_frame", this );
	m_caption    = UIHelper::CreateStatic( xml, "t_caption", this );
//	m_counter    = UIHelper::CreateStatic( xml, "t_counter", this );
	m_bt_close   = UIHelper::Create3tButton( xml, "btn_close", this );

	Register( m_bt_close );
	AddCallback( m_bt_close, BUTTON_DOWN, CUIWndCallback::void_function( this, &UITaskListWnd::OnBtnClose ) );

	if (xml.NavigateToNode(PdaTaskXml::PanelStorylineItemRel))
	{
		_storylineTaskItem = new CUITaskItem();
		_storylineTaskItem->SetAutoDelete(true);
		AttachChild(_storylineTaskItem);
		_storylineTaskItem->Init(xml, PdaTaskXml::PanelStorylineItemRel);
		Register(_storylineTaskItem);
		AddCallback(
			_storylineTaskItem,
			WINDOW_LBUTTON_DB_CLICK,
			CUIWndCallback::void_function(this, &UITaskListWnd::OnStorylineTaskFocus)
		);
	}

	if (_storylineTaskItem && xml.NavigateToNode(PdaTaskXml::PanelStorylineFocusRel))
	{
		_btnStorylineTaskFocus = UIHelper::Create3tButton(
			xml,
			PdaTaskXml::PanelStorylineFocusRel,
			_storylineTaskItem
		);
	}

	if (_btnStorylineTaskFocus)
	{
		_btnStorylineTaskFocus->Show(false);
		Register(_btnStorylineTaskFocus);
		AddCallback(
			_btnStorylineTaskFocus,
			BUTTON_DOWN,
			CUIWndCallback::void_function(this, &UITaskListWnd::OnStorylineTaskFocus)
		);
	}

	m_list = new CUIScrollView();
	m_list->SetAutoDelete( true );
	AttachChild( m_list );
	CUIXmlInit::InitScrollView( xml, "task_list", 0, m_list );
	m_orig_h = GetHeight();

	m_list->SetWindowName("---second_task_list");
	m_list->m_sort_function = fastdelegate::MakeDelegate( this, &UITaskListWnd::SortingLessFunction );

	if (xml.NavigateToNode(PdaTaskXml::PanelFilterTabsRel)
		&& xml.GetNodesNum(PdaTaskXml::PanelFilterTabsRel, 0, "button") > 0)
	{
		m_filter_tabs = new CUITabControl();
		m_filter_tabs->SetAutoDelete(true);
		AttachChild(m_filter_tabs);
		CUIXmlInit::InitTabControl(xml, PdaTaskXml::PanelFilterTabsRel, 0, m_filter_tabs);
		m_filter_tabs->SetMessageTarget(this);
	}

	xml.SetLocalRoot( stored_root );

	if (m_filter_tabs && m_filter_tabs->GetTabsCount() > 0)
	{
		m_filter_tabs->SetActiveTabByIndex(0);
	}
}

bool UITaskListWnd::OnMouseAction( float x, float y, EUIMessages mouse_action )
{
	if ( inherited::OnMouseAction( x, y, mouse_action ) )
	{
		return true;
	}
	return true;
}

void UITaskListWnd::OnMouseScroll( float iDirection )
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::ListScroll, true);
	}

	if ( (u32)iDirection == WINDOW_MOUSE_WHEEL_UP )
	{
		if (CUIScrollBar* bar = m_list->ScrollBar())
		{
			bar->TryScrollDec();
		}
	}
	else if ((u32)iDirection == WINDOW_MOUSE_WHEEL_DOWN )
	{
		if (CUIScrollBar* bar = m_list->ScrollBar())
		{
			bar->TryScrollInc();
		}
	}
}
void UITaskListWnd::Show( bool status )
{
	inherited::Show( status );
	GetMessageTarget()->SendMessage( this, PDA_TASK_HIDE_HINT, nullptr );
	if(status)
		UpdateList();
}

void UITaskListWnd::OnFocusReceive()
{
	inherited::OnFocusReceive();
	GetMessageTarget()->SendMessage( this, PDA_TASK_HIDE_HINT, nullptr );
}

void UITaskListWnd::OnFocusLost()
{
	inherited::OnFocusLost();
	GetMessageTarget()->SendMessage( this, PDA_TASK_HIDE_HINT, nullptr );
}

void UITaskListWnd::Update()
{
	inherited::Update();
	if (_storylineTaskItem && _storylineTaskItem->show_hint && _storylineTaskItem->OwnerTask())
	{
		CGameTask* task = _storylineTaskItem->OwnerTask();
		GetMessageTarget()->SendMessage(_storylineTaskItem, PDA_TASK_SHOW_HINT, (void*)task);
	}
//	UpdateCounter();
}

void UITaskListWnd::SendMessage( CUIWindow* pWnd, s16 msg, void* pData )
{
	if (msg == TAB_CHANGED && m_filter_tabs && pWnd == m_filter_tabs)
	{
		if (m_pUiSounds)
		{
			m_pUiSounds->Play(EPdaUiSound::Tab);
		}

		const shared_str activeId = m_filter_tabs->GetActiveId();
		ETaskListFilter mode = ETaskListFilter::All;
		if (activeId == "story")
		{
			mode = ETaskListFilter::Story;
		}
		else if (activeId == "side")
		{
			mode = ETaskListFilter::Side;
		}

		SetFilterMode(mode);
		return;
	}

	GetMessageTarget()->SendMessage( pWnd, msg, pData );
	inherited::SendMessage( pWnd, msg, pData );
	CUIWndCallback::OnEvent( pWnd, msg, pData );
}

void UITaskListWnd::OnBtnClose( CUIWindow* w, void* d )
{
	CUITaskWnd* wnd = smart_cast<CUITaskWnd*>(GetParent()->GetParent());
	if(wnd)
		wnd->Show_TaskListWnd(false);
//	Show( false );
	m_bt_close->SetButtonState(CUIButton::BUTTON_NORMAL);
}

void UITaskListWnd::OnStorylineTaskFocus(CUIWindow* w, void* d)
{
	CGameTask* task = StorylineTask();
	if (task)
	{
		GetMessageTarget()->SendMessage(this, PDA_TASK_SET_TARGET_MAP, (void*)task);
	}
}

void UITaskListWnd::UpdateStorylineTask(CGameTask* task)
{
	if (!_storylineTaskItem)
	{
		return;
	}

	_storylineTaskItem->InitTask(task);
	UpdateStorylineTaskFocus();
}

CGameTask* UITaskListWnd::StorylineTask() const
{
	return _storylineTaskItem ? _storylineTaskItem->OwnerTask() : nullptr;
}

void UITaskListWnd::UpdateStorylineTaskFocus()
{
	if (!_btnStorylineTaskFocus)
	{
		return;
	}

	CGameTask* task = StorylineTask();
	if (!task || !task->HasActiveMapTarget())
	{
		_btnStorylineTaskFocus->Show(false);
		return;
	}

	_btnStorylineTaskFocus->Show(true);
}

void UITaskListWnd::UpdateList()
{
	int prev_scroll_pos	= m_list->GetCurrentScrollPos	();

	m_list->Clear();
	
	vGameTasks& tasks = Level().GameTaskManager()->GetGameTasks();
	vGameTasks::iterator itb = tasks.begin();
	vGameTasks::iterator ite = tasks.end();
	for ( ; itb != ite; ++itb )
	{
		CGameTask* task = (*itb).getGameTask();
		if ( task && task->GetTaskState() == eTaskStateInProgress )
		{
			const ETaskType taskType = task->GetTaskType();
			switch (m_filter)
			{
				case ETaskListFilter::Story:
					if (taskType != eTaskTypeStoryline)
					{
						continue;
					}
					break;

				case ETaskListFilter::Side:
					if (taskType == eTaskTypeStoryline)
					{
						continue;
					}
					break;

				case ETaskListFilter::All:
				default:
					break;
			}

			UITaskListWndItem* item = new UITaskListWndItem();
			if ( item->init_task( task, this ) )
			{
				m_list->AddWindow( item, true );
			}
		}
	}
	m_list->UpdateChildrenLenght();
	m_list->ForceUpdate();
	if (!m_list->NeedShowScrollBar())
	{
		m_list->ScrollToBegin();
		prev_scroll_pos = m_list->GetMinScrollPos();
	}
	m_list->SetScrollPos(prev_scroll_pos);
}

void UITaskListWnd::SetFilterMode(ETaskListFilter mode)
{
	if (m_filter == mode)
	{
		return;
	}

	m_filter = mode;
	if (IsShown())
	{
		UpdateList();
	}

	if (m_filter_tabs)
	{
		GetMessageTarget()->SendMessage(
			this,
			PDA_TASK_LIST_FILTER_CHANGED,
			reinterpret_cast<void*>(static_cast<intptr_t>(mode))
		);
	}
}

bool UITaskListWnd::SortingLessFunction( CUIWindow* left, CUIWindow* right )
{
	UITaskListWndItem* lpi = smart_cast<UITaskListWndItem*>(left);
	UITaskListWndItem* rpi = smart_cast<UITaskListWndItem*>(right);
	VERIFY( lpi && rpi );
	return ( lpi->get_priority_task() > rpi->get_priority_task() );
}


bool UITaskListWnd::SelectNextToSelected(bool bNext)
{
	CGameTaskManager* taskManager = Level().GameTaskManager();
	ETaskType taskType = taskManager->IsMultipleTask() ? eTaskTypeAdditional : eTaskTypeStoryline;
	CGameTask* pActiveTask = taskManager->ActiveTask(taskType);
	if (pActiveTask)
	{
		WINDOW_LIST& wndList = m_list->Items();
		for (WINDOW_LIST_it it = wndList.begin(); it != wndList.end(); ++it)
		{
			UITaskListWndItem* item = static_cast<UITaskListWndItem*>(*it);
			if (item->get_task() == pActiveTask)
			{
				if (bNext)
				{
					if (it + 1 != wndList.end())
					{
						UITaskListWndItem* nextToItem = static_cast<UITaskListWndItem*>(*(it + 1));
						taskManager->SetActiveTask(nextToItem->get_task());
						const float barHeight = m_list->ScrollBar() ? m_list->ScrollBar()->GetHeight() : 0.0f;
						m_list->ScrollToItem(nextToItem, iFloor(-barHeight / 2.0f + nextToItem->GetWndRect().height() / 2.0f));
						if (m_pUiSounds)
						{
							m_pUiSounds->Play(EPdaUiSound::ListSelect);
						}
						return true;
					}
				}
				else
				{
					if (it != wndList.begin())
					{
						UITaskListWndItem* nextToItem = static_cast<UITaskListWndItem*>(*(it - 1));
						taskManager->SetActiveTask(nextToItem->get_task());
						const float barHeight = m_list->ScrollBar() ? m_list->ScrollBar()->GetHeight() : 0.0f;
						m_list->ScrollToItem(nextToItem, iFloor(-barHeight / 2.0f + nextToItem->GetWndRect().height() / 2.0f));
						if (m_pUiSounds)
						{
							m_pUiSounds->Play(EPdaUiSound::ListSelect);
						}
						return true;
					}
				}
			}
		}
	}
	return false;
}

bool UITaskListWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (IsShown() && gamepad_action == WINDOW_KEY_PRESSED)
	{
		if (is_binded(kPDA_TASKS_NEXT, id))
		{
			if (!any_binded_key_for_action_pressed_c(kPDA_TASKS_PREV))
				SelectNextToSelected(true);
			ActionRepeaters()->SetActionStarted(this, kPDA_TASKS_NEXT);
			return true;
		}
		else if (is_binded(kPDA_TASKS_PREV, id))
		{
			if (!any_binded_key_for_action_pressed_c(kPDA_TASKS_NEXT))
				SelectNextToSelected(false);
			ActionRepeaters()->SetActionStarted(this, kPDA_TASKS_PREV);
			return true;
		}
		else if (is_binded(kPDA_TASKS_FILTER_TOGGLE, id))
		{
			CGameTaskManager* taskManager = Level().GameTaskManager();
			ETaskType taskType = taskManager->IsMultipleTask() ? eTaskTypeAdditional : eTaskTypeStoryline;
			CGameTask* pActiveTask = taskManager->ActiveTask(taskType);
			if (pActiveTask)
			{
				GetMessageTarget()->SendMessage(this, PDA_TASK_SET_TARGET_MAP, (void*)pActiveTask);
			}
			return true;
		}
		else if (is_binded(kUI_HINT, id))
		{
			CGameTaskManager* taskManager = Level().GameTaskManager();
			CGameTask* pActiveTask = taskManager->ActiveTask();
			if (pActiveTask)
			{
				WINDOW_LIST& wndList = m_list->Items();
				for (WINDOW_LIST_it it = wndList.begin(); it != wndList.end(); ++it)
				{
					UITaskListWndItem* item = static_cast<UITaskListWndItem*>(*it);
					if (item->get_task() == pActiveTask)
					{
						if (item->show_hint)
						{
							item->hide_hint();
						}
						else
						{
							item->showHint();
						}
						break;
					}
				}
			}
			return true;
		}
	}

	return inherited::OnGamepadKeyAction(id, gamepad_action);
}

bool UITaskListWnd::OnGamepadKeyHold(int id)
{
	if (is_binded(kPDA_TASKS_NEXT, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kPDA_TASKS_NEXT) && !any_binded_key_for_action_pressed_c(kPDA_TASKS_PREV))
			SelectNextToSelected(true);
		return true;
	}
	else if (is_binded(kPDA_TASKS_PREV, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kPDA_TASKS_PREV) && !any_binded_key_for_action_pressed_c(kPDA_TASKS_NEXT))
			SelectNextToSelected(false);
		return true;
	}
	return inherited::OnGamepadKeyHold(id);
}

// - -----------------------------------------------------------------------------------------------

UITaskListWndItem::UITaskListWndItem()
	: show_hint_can(false), show_hint(false),
	m_task(nullptr), m_name(nullptr),
	m_bt_view(nullptr), m_st_story(nullptr),
	m_task_icon(nullptr),
	m_bt_focus(nullptr), m_btn_task_focus(nullptr)
{
	m_color_states[0] = (u32)(-1);
	m_color_states[1] = (u32)(-1);
	m_color_states[2] = (u32)(-1);
}

IC u32 UITaskListWndItem::get_priority_task() const
{
	VERIFY(m_task);
	return m_task->m_priority;
}

bool UITaskListWndItem::init_task( CGameTask* task, UITaskListWnd* parent )
{
	VERIFY( task );
	if ( !task )
	{
		return false;
	}
	m_task = task;
	SetMessageTarget( parent );
	
	CUIXml		xml;
	xml.Load( CONFIG_PATH, UI_PATH, PDA_TASK_XML );

	CUIXmlInit::InitWindow( xml, "second_task_wnd:task_item", 0, this );
	
	m_name     = UIHelper::Create3tButton( xml, "second_task_wnd:task_item:name", this );

	if (xml.NavigateToNode("second_task_wnd:task_item:btn_view"))
		m_bt_view  = UIHelper::CreateCheck(      xml, "second_task_wnd:task_item:btn_view", this );

	if (xml.NavigateToNode("second_task_wnd:task_item:st_story"))
		m_st_story = UIHelper::CreateStatic( xml, "second_task_wnd:task_item:st_story", this );

	// Optional per-task icon container (uses CGameTask::m_icon_texture_name).
	if (xml.NavigateToNode("second_task_wnd:task_item:t_icon"))
	{
		m_task_icon = UIHelper::CreateStatic(xml, "second_task_wnd:task_item:t_icon", this);
	}

	if (xml.NavigateToNode(PdaTaskXml::TaskItemFocus))
	{
		m_bt_focus = UIHelper::Create3tButton(xml, PdaTaskXml::TaskItemFocus, this);
	}
	else if (xml.NavigateToNode(PdaTaskXml::TaskItemTaskFocus))
	{
		m_bt_focus = UIHelper::Create3tButton(xml, PdaTaskXml::TaskItemTaskFocus, this);
	}

	// Optional second focus button when both btn_focus and btn_task_focus are defined in XML.
	if (m_bt_focus && xml.NavigateToNode(PdaTaskXml::TaskItemTaskFocus)
		&& xml.NavigateToNode(PdaTaskXml::TaskItemFocus))
	{
		m_btn_task_focus = UIHelper::Create3tButton(xml, PdaTaskXml::TaskItemTaskFocus, this);
	}

	if (m_bt_focus)
	{
		m_bt_focus->SetMessageTarget(this);
	}
	if (m_btn_task_focus)
	{
		m_btn_task_focus->SetMessageTarget(this);
	}

	m_color_states[stt_activ ] = CUIXmlInit::GetColor( xml, "second_task_wnd:task_item:activ",  0, (u32)(-1) );
	m_color_states[stt_unread] = CUIXmlInit::GetColor( xml, "second_task_wnd:task_item:unread", 0, (u32)(-1) );
	m_color_states[stt_read  ] = CUIXmlInit::GetColor( xml, "second_task_wnd:task_item:read",   0, (u32)(-1) );
	update_view();
	return true;
}

void UITaskListWndItem::hide_hint()
{
	show_hint_can   = false;
	show_hint       = false;
	GetMessageTarget()->SendMessage( this, PDA_TASK_HIDE_HINT, nullptr );
}

void UITaskListWndItem::showHint()
{
	show_hint_can   = true;
	show_hint       = true;
	GetMessageTarget()->SendMessage( this, PDA_TASK_SHOW_HINT, (void*)m_task );
}

void UITaskListWndItem::Update()
{
	inherited::Update();
	update_view();

	if ( m_task && m_name->CursorOverWindow() && show_hint_can )
	{
		if ( Device.dwTimeContinual > ( m_name->FocusReceiveTime() + 700 ) )
		{
			showHint();
			return;
		}
	}
}

void UITaskListWndItem::update_view()
{
	VERIFY( m_task );
	CMapLocation* ml = m_task->LinkedMapLocation();
	const bool hasMapTarget = TaskHasMapTarget(m_task);
	const bool spotVisible = ml && ml->SpotEnabled();
	if ( spotVisible )
	{
		if (m_bt_view)
		{
			m_bt_view->SetCheck(false);
		}
	}
	else
	{
		if (m_bt_view)
		{
			m_bt_view->SetCheck(true);
		}
	}

	if (m_bt_focus)
	{
		m_bt_focus->Show(hasMapTarget);
	}

	if (m_btn_task_focus)
	{
		m_btn_task_focus->Show(hasMapTarget);
	}

	if (m_st_story)
	{
		if (m_task->GetTaskType() == eTaskTypeStoryline)
			m_st_story->InitTexture("ui_inGame2_PDA_icon_Primary_mission");
		else
			m_st_story->InitTexture("ui_inGame2_PDA_icon_Secondary_mission");
	}

	if (m_task_icon)
	{
		if (m_task->m_icon_texture_name.size())
		{
			m_task_icon->InitTexture(m_task->m_icon_texture_name.c_str());	

			Frect emptyRect = Frect().set(0.f, 0.f, 0.f, 0.f);
			if (!m_task->m_icon_rect.cmp(emptyRect))
			{
				Frect r = m_task->m_icon_rect;
				Frect texture_rect;

				texture_rect.lt.set(r.x1, r.y1);
				texture_rect.rb.set(r.x2, r.y2);
				texture_rect.rb.add(texture_rect.lt);
				m_task_icon->SetTextureRect(texture_rect);
			}

			m_task_icon->TextureOn();
			m_task_icon->SetStretchTexture(true);
			m_task_icon->Show(true);
		}
		else
		{
			m_task_icon->TextureOff();
			m_task_icon->Show(false);
		}
	}

	m_name->TextItemControl()->SetTextST( g_pStringTable->ParseStringFromScript(m_task->m_Title).c_str() );
	m_name->AdjustHeightToText();
	const float prevHeight = GetHeight();
	float h1 = m_name->GetWndPos().y + m_name->GetHeight() + 10.0f;
	h1 = std::max( h1, prevHeight );
	SetHeight( h1 );
	if (!fsimilar(prevHeight, h1))
	{
		CUIWindow* pad = GetParent();
		if (pad)
		{
			CUIScrollView* scroll = smart_cast<CUIScrollView*>(pad->GetParent());
			if (scroll)
			{
				scroll->ForceUpdate();
			}
		}
	}

    const CGameTask* storyTask = Level().GameTaskManager()->ActiveTask(eTaskTypeStoryline);
    const CGameTask* additionalTask = Level().GameTaskManager()->ActiveTask(eTaskTypeAdditional);
    
    if (m_task == storyTask || m_task == additionalTask)
	{
		m_name->SetStateTextColor( m_color_states[stt_activ], S_Enabled );
	}
	else if ( m_task->m_read )
	{
		m_name->SetStateTextColor( m_color_states[stt_read], S_Enabled );
	}
	else
	{
		m_name->SetStateTextColor( m_color_states[stt_unread], S_Enabled );
	}

}

void UITaskListWndItem::SendMessage( CUIWindow* pWnd, s16 msg, void* pData )
{
	const bool isFocusControl = (m_bt_focus && IsWindowOrChildOf(pWnd, m_bt_focus))
		|| (m_btn_task_focus && IsWindowOrChildOf(pWnd, m_btn_task_focus));

	if (isFocusControl && (msg == BUTTON_DOWN || msg == BUTTON_CLICKED))
	{
		GetMessageTarget()->SendMessage(this, PDA_TASK_SET_TARGET_MAP, (void*)m_task);
		return;
	}

	if ( pWnd == m_bt_view )
	{
		if ( m_bt_view->GetCheck() && msg == BUTTON_CLICKED )
		{
			GetMessageTarget()->SendMessage( this, PDA_TASK_HIDE_MAP_SPOT, (void*)m_task );
			return;
		}
		if ( !m_bt_view->GetCheck() && msg == BUTTON_CLICKED )
		{
			GetMessageTarget()->SendMessage( this, PDA_TASK_SHOW_MAP_SPOT, (void*)m_task );
			return;
		}
	}

	if ( pWnd == m_name )
	{
		if ( msg == BUTTON_DOWN )
		{
			Level().GameTaskManager()->SetActiveTask( m_task );
			return;
		}

		if ( msg == WINDOW_LBUTTON_DB_CLICK )
		{
			GetMessageTarget()->SendMessage( this, PDA_TASK_SET_TARGET_MAP, (void*)m_task );
		}
	}

	inherited::SendMessage( pWnd, msg, pData );
}

bool UITaskListWndItem::OnMouseAction( float x, float y, EUIMessages mouse_action )
{
	if ( inherited::OnMouseAction( x, y, mouse_action ) )
	{
		return true;
	}

	switch ( mouse_action )
	{
	case WINDOW_LBUTTON_DOWN:
	case WINDOW_RBUTTON_DOWN:
	case BUTTON_DOWN:
		{
			hide_hint();
			break;
		}
	}//switch

	return false;
}

void UITaskListWndItem::OnFocusReceive()
{
	inherited::OnFocusReceive();
	hide_hint();
	show_hint_can = true;
}

void UITaskListWndItem::OnFocusLost()
{
	inherited::OnFocusLost();
	hide_hint();
}
