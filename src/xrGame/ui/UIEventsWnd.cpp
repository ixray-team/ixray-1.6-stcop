#include "StdAfx.h"
#include "pch_script.h"
#include "UIEventsWnd.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "UIMapWnd.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "UITaskDescrWnd.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../HUDManager.h"
#include "../Level.h"
#include "../Actor.h"
#include "../GametaskManager.h"
#include "../GameTask.h"
#include "../map_manager.h"
#include "../map_location.h"
#include "../../xrEngine/string_table.h"
#include "UITaskItem.h"
#include "../alife_registry_wrappers.h"
#include "../encyclopedia_article.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIGamepadLegend.h"

CUIEventsWnd::CUIEventsWnd			()
{
	m_flags.zero			();

	ActionRepeaters()->Register(this, kUI_UP);
	ActionRepeaters()->Register(this, kUI_DOWN);
	ActionRepeaters()->Register(this, kPDA_TASKS_FILTER_NEXT);
	ActionRepeaters()->Register(this, kPDA_TASKS_FILTER_PREV);
	ActionRepeaters()->Register(this, kUI_SECONDARY_UP);
	ActionRepeaters()->Register(this, kUI_SECONDARY_DOWN);
}

CUIEventsWnd::~CUIEventsWnd			()
{
	ActionRepeaters()->UnregisterOwner(this);
	delete_data			(m_UIMapWnd);
	delete_data			(m_UITaskInfoWnd);
}

void CUIEventsWnd::Init				()
{
	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, "pda_events.xml");

	CUIXmlInit xml_init;
	xml_init.InitWindow				(uiXml, "main_wnd", 0, this);


	m_UILeftFrame					= new CUIFrameWindow(); m_UILeftFrame->SetAutoDelete(true);
	AttachChild						(m_UILeftFrame);
	xml_init.InitFrameWindow		(uiXml, "main_wnd:left_frame", 0, m_UILeftFrame);


	m_UILeftHeader					= new CUIFrameLineWnd(); m_UILeftHeader->SetAutoDelete(true);
	m_UILeftFrame->AttachChild		(m_UILeftHeader);
	xml_init.InitFrameLine			(uiXml, "main_wnd:left_frame:left_frame_header", 0, m_UILeftHeader);

	m_UIAnimation					= new CUIAnimatedStatic(); m_UIAnimation->SetAutoDelete(true);
	xml_init.InitAnimatedStatic		(uiXml, "main_wnd:left_frame:left_frame_header:anim_static", 0, m_UIAnimation);
	m_UILeftHeader->AttachChild		(m_UIAnimation);

	m_UIRightWnd					= new CUIWindow(); m_UIRightWnd->SetAutoDelete(true);
	AttachChild						(m_UIRightWnd);
	xml_init.InitWindow				(uiXml, "main_wnd:right_frame", 0, m_UIRightWnd);

	m_UIMapWnd						= new CUIMapWnd(); m_UIMapWnd->SetAutoDelete(false);
	m_UIMapWnd->Init				("pda_events.xml","main_wnd:right_frame:map_wnd");

	m_UITaskInfoWnd					= new CUITaskDescrWnd(); m_UITaskInfoWnd->SetAutoDelete(false);
	m_UITaskInfoWnd->Init			(&uiXml,"main_wnd:right_frame:task_descr_view");
	

	m_ListWnd						= new CUIScrollView(); m_ListWnd->SetAutoDelete(true);
	m_UILeftFrame->AttachChild		(m_ListWnd);
	xml_init.InitScrollView			(uiXml, "main_wnd:left_frame:list", 0, m_ListWnd);
	m_ListWnd->SetSelectionsAllowed(true);

	m_TaskFilter					= new CUITabControl(); m_TaskFilter->SetAutoDelete(true);
	m_UILeftFrame->AttachChild		(m_TaskFilter);
	xml_init.InitTabControl			(uiXml, "main_wnd:left_frame:filter_tab", 0, m_TaskFilter);
	m_TaskFilter->SetWindowName		("filter_tab");
	Register						(m_TaskFilter);
    AddCallbackStr					("filter_tab",TAB_CHANGED,CUIWndCallback::void_function(this,&CUIEventsWnd::OnFilterChanged));

	SetDescriptionMode				(true);

	m_ui_task_item_xml.Load(CONFIG_PATH, UI_PATH, "job_item.xml");
	m_TaskFilter->SetActiveTabByIndex(0);

	m_gamepad_legend				= UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);
}

void CUIEventsWnd::Update			()
{
	if(m_flags.test(flNeedReload) )
	{
		ReloadList(false);
		m_flags.set(flNeedReload,false );
	}
	inherited::Update		();
	UpdateGamepadLegend		();
}

void CUIEventsWnd::UpdateGamepadLegend()
{
	if (!m_gamepad_legend)
	{
		return;
	}

	CUIWindow* actionAccept = m_gamepad_legend->FindChild("action_accept");
	if (actionAccept)
	{
		actionAccept->Show(GetDescriptionMode());
	}

	CUIWindow* showDescription = m_gamepad_legend->FindChild("show_description");
	if (showDescription)
	{
		if (CUIStatic* showDescriptionS = showDescription->ui_cast_static())
		{
			showDescriptionS->SetTextST(GetDescriptionMode() ? "ui_tasks_show_description" : "ui_tasks_show_description_back");
		}
	}

	CUIWindow* showOnMap = m_gamepad_legend->FindChild("show_on_map");
	if (showOnMap)
	{
		showOnMap->Show(GetDescriptionMode());
	}

	CUIWindow* showMe = m_gamepad_legend->FindChild("show_me");
	if (showMe)
	{
		showMe->Show(GetDescriptionMode());
	}

	CUIWindow* mapZoom = m_gamepad_legend->FindChild("map_zoom");
	if (mapZoom)
	{
		mapZoom->Show(GetDescriptionMode());
	}
}

void CUIEventsWnd::Draw				()
{
	inherited::Draw			();
}

void	CUIEventsWnd::SendMessage			(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIEventsWnd::OnFilterChanged			(CUIWindow* w, void*)
{
	m_currFilter			=(ETaskFilters)m_TaskFilter->GetActiveIndex();
	ReloadList				(false);
	if(!GetDescriptionMode())
		SetDescriptionMode		(true);
}

void CUIEventsWnd::Reload					()
{
		m_flags.set(flNeedReload,true );
}

void CUIEventsWnd::ReloadList(bool bClearOnly)
{
	m_ListWnd->Clear			();
	m_SubtaskItemList.clear		();
	if(bClearOnly)				return;

	if(!g_actor)				return;
	vGameTasks& tasks			= Level().GameTaskManager()->GetGameTasks();
	vGameTasks::iterator it		= tasks.begin();
	CGameTask* task				= nullptr;
	
	for(;it!=tasks.end();++it)
	{
		task					= (*it).getGameTask();
		R_ASSERT				(task);
		R_ASSERT				(task->GetObjectivesCount() > 0);

		if( !Filter(task) )		continue;
		CUITaskItemLegacy* pTaskItem	= nullptr;
/*
		if(task->m_Objectives[0].TaskState()==eTaskUserDefined)
		{
			VERIFY				(task->m_Objectives.size()==1);
			pTaskItem			= new CUIUserTaskItem(this);
			pTaskItem->SetGameTask			(task, 0);
			m_ListWnd->AddWindow			(pTaskItem,true);
		}else
*/
		for (u16 i = 0; i < task->GetObjectivesCount(); ++i)
		{
			if(i==0)
			{
				pTaskItem					= new CUITaskRootItem(this);
			}
			else
			{
				pTaskItem					= new CUITaskSubItem(this);
				m_SubtaskItemList.push_back(pTaskItem);
			}
			pTaskItem->SetGameTask			(task, i);
			m_ListWnd->AddWindow			(pTaskItem,true);
		}

	}
	if (pInput->GetControllerMode())
	{
		if (m_SubtaskItemList.size() > 0)
		{
			SetSubtaskSelected(m_SubtaskItemList.front());
		}
	}
}

void CUIEventsWnd::Show(bool status)
{
	inherited::Show			(status);
	m_UIMapWnd->Show		(status);
	m_UITaskInfoWnd->Show	(status);

	ReloadList				(status == false);

}

bool CUIEventsWnd::Filter(CGameTask* t)
{
	ETaskState task_state		= t->Objective(0).GetTaskState();

	return 
				(
					(m_currFilter==eAccomplishedTask	&& task_state==eTaskStateCompleted )||
					(m_currFilter==eFailedTask			&& task_state==eTaskStateFail )||
					(m_currFilter==eActiveTask			&& task_state==eTaskStateInProgress )
				);
}


void CUIEventsWnd::SetDescriptionMode		(bool bMap)
{
	if(bMap){
		if (m_UIRightWnd->IsChild(m_UITaskInfoWnd))
			m_UIRightWnd->DetachChild	(m_UITaskInfoWnd);
		if (!m_UIRightWnd->IsChild(m_UIMapWnd))
			m_UIRightWnd->AttachChild	(m_UIMapWnd);
	}else{
		if (m_UIRightWnd->IsChild(m_UIMapWnd))
			m_UIRightWnd->DetachChild		(m_UIMapWnd);
		if (!m_UIRightWnd->IsChild(m_UITaskInfoWnd))
			m_UIRightWnd->AttachChild		(m_UITaskInfoWnd);
	}
	m_flags.set(flMapMode, bMap);
}

bool CUIEventsWnd::GetDescriptionMode		()
{
	return !!m_flags.test(flMapMode);
}

void CUIEventsWnd::ShowDescription			(CGameTask* t, int idx)
{
	if(GetDescriptionMode())
	{//map
		SGameTaskObjective& o		= t->Objective(idx);
		CMapLocation* ml			= o.LinkedMapLocation();

		if (ml && ml->SpotEnabled())
		{
			ml->CalcPosition();
			m_UIMapWnd->SetTargetMap(ml->GetLevelName(), ml->GetPosition(), true);
		}
	}
	else
	{//articles
		SGameTaskObjective& o		= t->Objective(0);
		idx							= 0;

		m_UITaskInfoWnd->ClearAll	();

		if(Actor()->encyclopedia_registry->registry().objects_ptr())
		{
			string512 need_group;
			if(0==idx)
			{
				strcpy(need_group,*t->m_ID);
			}
			else if(o.m_article_key.size())
			{
				xr_sprintf(need_group, "%s/%s", *t->m_ID, *o.m_article_key);
			}
			else
			{
				xr_sprintf(need_group, "%s/%d", *t->m_ID, idx);
			}

			ARTICLE_VECTOR::const_iterator it		= Actor()->encyclopedia_registry->registry().objects_ptr()->begin();

			for(; it != Actor()->encyclopedia_registry->registry().objects_ptr()->end(); ++it)
			{
				if (ARTICLE_DATA::eTaskArticle == it->article_type)
				{
					CEncyclopediaArticle	A;
					A.Load					(it->article_id);

					const shared_str& group = A.data()->group;

					if( strstr(group.c_str(), need_group)== group.c_str() )
					{
						u32 sz			= xr_strlen(need_group);
						if ( group.size()== sz || group.c_str()[sz]=='/' )
							m_UITaskInfoWnd->AddArticle(&A);
					}else
					if(o.m_article_id.size() && it->article_id ==o.m_article_id)
					{
						CEncyclopediaArticle			A;
						A.Load							(it->article_id);
						m_UITaskInfoWnd->AddArticle		(&A);
					}
				}
			}
		}
	}

	int sz			= m_ListWnd->GetSize		();

	for(int i=0; i<sz;++i)
	{
		CUITaskItemLegacy* itm			= (CUITaskItemLegacy*)m_ListWnd->GetItem(i);

		if((itm->GameTask()==t) && (itm->ObjectiveIdx()==idx) )	
			itm->MarkSelected		(true);
		else
			itm->MarkSelected		(false);
	}
}

bool CUIEventsWnd::ItemHasDescription(CUITaskItemLegacy* itm)
{
	if(itm->ObjectiveIdx()==0)// root
	{
		return itm->GameTask()->LinkedMapLocation();
	}
	else
	{
		SGameTaskObjective	*obj				= itm->Objective();
		CMapLocation* ml						= obj->LinkedMapLocation();
		bool bHasLocation						= (nullptr != ml);
		bool bIsMapMode							= GetDescriptionMode(); 
		return (bIsMapMode && bHasLocation && ml->SpotEnabled());
	}
}
void CUIEventsWnd::Reset()
{
	inherited::Reset	();
	Reload				();
		
	// need to clear the tasks list here
	// cause the list refills in the update and someone using it before that update will get invalid data
	ReloadList(true);
	
	if (!GetDescriptionMode())
		SetDescriptionMode(true);
}

void CUIEventsWnd::DrawHint()
{
	m_UIMapWnd->DrawHint();
}

bool CUIEventsWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED)
	{
		switch (get_binded_action(id, agUIGeneral))
		{
			case kUI_UP:
			{
				if (!any_binded_key_for_action_pressed_c(kUI_DOWN))
					MoveSelectionUp(true);
				ActionRepeaters()->SetActionStarted(this, kUI_UP);
				return true;
			}
			case kUI_DOWN:
			{
				if (!any_binded_key_for_action_pressed_c(kUI_UP))
					MoveSelectionDown(true);
				ActionRepeaters()->SetActionStarted(this, kUI_DOWN);
				return true;
			}
			case kUI_SECONDARY_UP:
			{
				if (!GetDescriptionMode())
				{
					break;
				}

				if (!any_binded_key_for_action_pressed_c(kUI_SECONDARY_DOWN))
					m_UITaskInfoWnd->ScrollUp();
				ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_UP);
				return true;
			}
			case kUI_SECONDARY_DOWN:
			{
				if (!GetDescriptionMode())
				{
					break;
				}

				if (!any_binded_key_for_action_pressed_c(kUI_SECONDARY_UP))
					m_UITaskInfoWnd->ScrollDown();
				ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_DOWN);
				return true;
			}
		}
		switch (get_binded_action(id, agUITaskMenu))
		{
			case kPDA_TASKS_MAP_SHOW_ME:
			{
				m_UIMapWnd->ViewActor();
				return true;
				break;
			}
			case kPDA_TASKS_TOGGLE_LEGEND:
			{
				SetDescriptionMode(!GetDescriptionMode());

				CUITaskItemLegacy* pItem = static_cast<CUITaskItemLegacy*>(m_ListWnd->GetSelected());
				if (pItem)
					SetSubtaskSelected(pItem);
				return true;
			}
			case kPDA_TASKS_FILTER_NEXT:
			{
				if (!any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_PREV))
					m_TaskFilter->NextTab(true);
				ActionRepeaters()->SetActionStarted(this, kPDA_TASKS_FILTER_NEXT);
				return true;
			}
			case kPDA_TASKS_FILTER_PREV:
			{
				if (!any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_NEXT))
					m_TaskFilter->PrevTab(true);
				ActionRepeaters()->SetActionStarted(this, kPDA_TASKS_FILTER_PREV);
				return true;
			}
			case kPDA_TASKS_TOGGLE_LIST:
			{
				CUITaskSubItem* pItem = dynamic_cast<CUITaskSubItem*>(m_ListWnd->GetSelected());
				if (pItem)
					pItem->OnActiveObjectiveClicked();
				return true;
			}
			case kPDA_TASKS_FILTER_TOGGLE:
			{
				CUITaskSubItem* pItem = dynamic_cast<CUITaskSubItem*>(m_ListWnd->GetSelected());
				if (pItem)
					pItem->OnDbClick();
				return true;
			}
		}
	}

	return inherited::OnGamepadKeyAction(id, gamepad_action);
}

bool CUIEventsWnd::OnGamepadKeyHold(int id)
{
	switch (get_binded_action(id, agUIGeneral))
	{
		case kUI_UP:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_UP) && !any_binded_key_for_action_pressed_c(kUI_DOWN))
				MoveSelectionUp(false);
			return true;
		}
		case kUI_DOWN:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_DOWN) && !any_binded_key_for_action_pressed_c(kUI_UP))
				MoveSelectionDown(false);
			return true;
		}
		case kUI_SECONDARY_UP:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_UP) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_DOWN))
				m_UITaskInfoWnd->ScrollUp();
			return true;
		}
		case kUI_SECONDARY_DOWN:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_DOWN) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_UP))
				m_UITaskInfoWnd->ScrollDown();
			return true;
		}
	}
	switch (get_binded_action(id, agUITaskMenu))
	{
		case kPDA_TASKS_FILTER_NEXT:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kPDA_TASKS_FILTER_NEXT) && !any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_PREV))
				m_TaskFilter->NextTab(false);
			return true;
		}
		case kPDA_TASKS_FILTER_PREV:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kPDA_TASKS_FILTER_PREV) && !any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_NEXT))
				m_TaskFilter->PrevTab(false);
			return true;
		}
	}

	return inherited::OnGamepadKeyHold(id);
}

bool CUIEventsWnd::MoveSelectionDown(bool bAllowLoop)
{
	CUITaskItemLegacy* pNewSelection = nullptr;
	if (!::MoveSelectionDown<CUITaskItemLegacy>(m_SubtaskItemList, static_cast<CUITaskItemLegacy*>(m_ListWnd->GetSelected()), pNewSelection, bAllowLoop))
		return false;

	SetSubtaskSelected(pNewSelection);
	return true;
}

bool CUIEventsWnd::MoveSelectionUp(bool bAllowLoop)
{
	CUITaskItemLegacy* pNewSelection = nullptr;
	if (!::MoveSelectionUp<CUITaskItemLegacy>(m_SubtaskItemList, static_cast<CUITaskItemLegacy*>(m_ListWnd->GetSelected()), pNewSelection, bAllowLoop))
		return false;

	SetSubtaskSelected(pNewSelection);
	return true;
}

CUITaskRootItem* CUIEventsWnd::GetTaskRootItem(CGameTask* t)
{
	WINDOW_LIST& witems = m_ListWnd->Items();
	for (WINDOW_LIST::iterator it = witems.begin(); it != witems.end(); ++it)
	{
		CUITaskRootItem* pItem = static_cast<CUITaskRootItem*>(*it);
		if (!pItem)
			continue;
		if (pItem->GameTask() == t)
			return pItem;
	}
	return nullptr;
}

void CUIEventsWnd::SetSubtaskSelected(CUITaskItemLegacy* pTaskItem)
{
	m_ListWnd->SetSelected(pTaskItem);

	CUITaskRootItem* pRoot = GetTaskRootItem(pTaskItem->GameTask());
	m_ListWnd->ScrollToItem(pRoot, 0);

	CUITaskItemLegacy* pItem = static_cast<CUITaskItemLegacy*>(m_ListWnd->GetSelected());
	ShowDescription(pItem->GameTask(), pItem->ObjectiveIdx());

//	UpdateInputLegend();
}
