#include "StdAfx.h"
#include "UITaskWnd.h"
#include "UIMapWnd.h"
#include "object_broker.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "UISecondTaskWnd.h"
#include "UIMapLegend.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrEngine/xr_input.h"
#include "../GameTask.h"
#include "../map_location.h"
#include "../map_location_defs.h"
#include "../map_manager.h"
#include "UIInventoryUtilities.h"
#include "../Level.h"
#include "../GametaskManager.h"
#include "../Actor.h"
#include "../../xrUI/Widgets/UICheckButton.h"
#include "../../xrUI/Widgets/UIMessages.h"
#include "../../xrEngine/string_table.h"
#include "../../xrUI/Widgets/UIGamepadLegend.h"
#include "PdaConstants.h"
#include "PdaUiSound.h"

CUITaskWnd::CUITaskWnd()
	: m_background(nullptr), m_background2(nullptr),
	m_center_background(nullptr), m_right_bottom_background(nullptr),
	m_task_split(nullptr), m_pMapWnd(nullptr),
	m_pStoryLineTaskItem(nullptr), m_pSecondaryTaskItem(nullptr),
	m_BtnTaskListWnd(nullptr), m_second_task_index(nullptr),
	m_devider(nullptr), m_actual_frame(0),
	m_btn_focus(nullptr), m_btn_focus2(nullptr),
	m_bTreasuresEnabled(false), m_bQuestNpcsEnabled(false),
	m_bSecondaryTasksEnabled(false), m_bPrimaryObjectsEnabled(false),
	m_bPersonalSpotsEnabled(true),
	m_task_wnd(nullptr), m_task_wnd_show(false),
	m_map_legend_wnd(nullptr), hint_wnd(nullptr)
{
	for (int i = 0; i < MAP_MARKS_FILTER_MAX; ++i)
		m_cbFilters[i] = nullptr;

	ActionRepeaters()->Register(this, kPDA_TASKS_FILTER_NEXT);
	ActionRepeaters()->Register(this, kPDA_TASKS_FILTER_PREV);
}

CUITaskWnd::~CUITaskWnd()
{
	delete_data						(m_pMapWnd);

	for (int i = 0; i < MAP_MARKS_FILTER_MAX; ++i)
		m_cbFilters[i] = nullptr;

	ActionRepeaters()->UnregisterOwner(this);
}

void CUITaskWnd::Init()
{
	CUIXml							xml;
	xml.Load						(CONFIG_PATH, UI_PATH, PDA_TASK_XML);
	VERIFY							(hint_wnd);

	CUIXmlInit::InitWindow			(xml, "main_wnd", 0, this);

	m_background					= UIHelper::CreateFrameWindow( xml, "background", this, false);

	m_background2					= UIHelper::CreateFrameLine(xml, "background", this, false);

	if (xml.NavigateToNode("task_split"))
		m_task_split = UIHelper::CreateFrameLine(xml, "task_split", this);

	m_features = DetectTaskWndFeatures(xml);

	if (xml.NavigateToNode("filter_treasures"))
	{
		m_cbFilters[MAP_MARKS_FILTER_TREASURES] = UIHelper::CreateCheck(xml, "filter_treasures", this);
		m_cbFilters[MAP_MARKS_FILTER_TREASURES]->SetCheck(true);
		AddCallback(m_cbFilters[MAP_MARKS_FILTER_TREASURES], BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnShowTreasures));
	}
	m_bTreasuresEnabled = true;

	if (xml.NavigateToNode("filter_primary_objects"))
	{
		m_cbFilters[MAP_MARKS_FILTER_PRIMARY_OBJECTS] = UIHelper::CreateCheck(xml, "filter_primary_objects", this);
		m_cbFilters[MAP_MARKS_FILTER_PRIMARY_OBJECTS]->SetCheck(true);
		AddCallback(m_cbFilters[MAP_MARKS_FILTER_PRIMARY_OBJECTS], BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnShowPrimaryObjects));
	}
	m_bPrimaryObjectsEnabled		= true;

	if (!m_features.filterTabs && xml.NavigateToNode("filter_secondary_tasks"))
	{
		m_cbFilters[MAP_MARKS_FILTER_SECONDARY_TASKS] = UIHelper::CreateCheck(xml, "filter_secondary_tasks", this);
		m_cbFilters[MAP_MARKS_FILTER_SECONDARY_TASKS]->SetCheck(true);
		AddCallback(m_cbFilters[MAP_MARKS_FILTER_SECONDARY_TASKS], BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnShowSecondaryTasks));
	}
	m_bSecondaryTasksEnabled		= true;

	if (xml.NavigateToNode("filter_quest_npcs"))
	{
		m_cbFilters[MAP_MARKS_FILTER_NPCS] = UIHelper::CreateCheck(xml, "filter_quest_npcs", this);
		m_cbFilters[MAP_MARKS_FILTER_NPCS]->SetCheck(true);
		AddCallback(m_cbFilters[MAP_MARKS_FILTER_NPCS], BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnShowQuestNpcs));
	}
	m_bQuestNpcsEnabled				= true;

	if (xml.NavigateToNode("filter_personal_spots"))
	{
		m_cbFilters[MAP_MARKS_FILTER_PERSONAL_SPOTS] = UIHelper::CreateCheck(xml, "filter_personal_spots", this);
		m_cbFilters[MAP_MARKS_FILTER_PERSONAL_SPOTS]->SetCheck(true);
		AddCallback(m_cbFilters[MAP_MARKS_FILTER_PERSONAL_SPOTS], BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnShowPersonalSpots));
	}

	if (xml.NavigateToNode("task_scope_story"))
	{
		m_btnScopeStory = UIHelper::Create3tButton(xml, "task_scope_story", this);
		AddCallback(m_btnScopeStory, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnTaskScopeStory));
	}
	if (xml.NavigateToNode("task_scope_side"))
	{
		m_btnScopeSide = UIHelper::Create3tButton(xml, "task_scope_side", this);
		AddCallback(m_btnScopeSide, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnTaskScopeSide));
	}
	if (xml.NavigateToNode("task_scope_failed"))
	{
		m_btnScopeFailed = UIHelper::Create3tButton(xml, "task_scope_failed", this);
		AddCallback(m_btnScopeFailed, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnTaskScopeFailed));
	}
	
	m_pMapWnd						= new CUIMapWnd(); 
	m_pMapWnd->SetAutoDelete		(false);
	m_pMapWnd->hint_wnd				= hint_wnd;
	m_pMapWnd->SetUiSounds			(m_pUiSounds);
	m_pMapWnd->Init					(PDA_TASK_XML, PdaConfig::MapSubdialogWindowName);
	AttachChild						(m_pMapWnd);

	m_center_background				= UIHelper::CreateStatic( xml, "center_background", this );
	if (xml.NavigateToNode("line_devider"))
	{
		m_devider = UIHelper::CreateStatic(xml, "line_devider", this);
	}

	InitStorylineWidgets(xml);
	InitStorylineFocusButton(xml);

	if (xml.NavigateToNode("secondary_task_item"))
    {
        Level().GameTaskManager()->AllowMultipleTask(true);
        m_pSecondaryTaskItem = new CUITaskItem();
        m_pSecondaryTaskItem->Init(xml, "secondary_task_item");
        AttachChild(m_pSecondaryTaskItem);
        m_pSecondaryTaskItem->SetAutoDelete(true);
        AddCallback(m_pSecondaryTaskItem, WINDOW_LBUTTON_DB_CLICK, CUIWndCallback::void_function(this, &CUITaskWnd::OnTask2DbClicked));
    }

    if (xml.NavigateToNode("btn_task_focus2"))
    {
		m_btn_focus2 = UIHelper::Create3tButton(xml, "btn_task_focus2", this);
        Register(m_btn_focus2);
        AddCallback(m_btn_focus2, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUITaskWnd::OnTask2DbClicked));
    }
	// btn_second_task is optional: mods can move the task list toggle to the main PDA tab (eptTaskList) instead.
	if (xml.NavigateToNode("btn_second_task"))
	{
		m_BtnTaskListWnd = UIHelper::Create3tButton(xml, "btn_second_task", this);
		AddCallback(m_BtnTaskListWnd, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskWnd::OnShowTaskListWnd));
	}

	if (xml.NavigateToNode("second_task_index"))
		m_second_task_index = UIHelper::CreateStatic(xml, "second_task_index", this);

	m_task_wnd					= new UITaskListWnd(); 
	m_task_wnd->SetAutoDelete	(true);
	m_task_wnd->hint_wnd		= hint_wnd;
	m_task_wnd->init_from_xml	(xml, "second_task_wnd");
	m_task_wnd->SetUiSounds		(m_pUiSounds);

	m_pMapWnd->AttachChild		(m_task_wnd);
	m_task_wnd->SetMessageTarget(this);
	m_task_wnd->Show			(false);
	m_task_wnd_show				= false;

	if (!m_features.filterTabs)
	{
		m_task_wnd->SetFilterMode(
			m_pSecondaryTaskItem ? ETaskListFilter::Side : ETaskListFilter::All
		);
	}
	else
	{
		OnTaskListFilterChanged(ETaskListFilter::All);
	}

	m_map_legend_wnd					= new UIMapLegend(); 
	m_map_legend_wnd->SetAutoDelete		(true);
	m_map_legend_wnd->SetUiSounds		(m_pUiSounds);
	m_map_legend_wnd->init_from_xml		(xml, "map_legend_wnd");
	m_pMapWnd->AttachChild				(m_map_legend_wnd);
	m_map_legend_wnd->SetMessageTarget	(this);
	m_map_legend_wnd->Show				(false);

	m_gamepad_legend			= UIHelper::CreateGamepadLegend(xml, "gamepad_legend", this, false);

	if (m_pUiSounds)
	{
		m_pUiSounds->LoadTaskWindow(xml);
	}
}

void CUITaskWnd::Update()
{
	if(Level().GameTaskManager()->ActualFrame() != m_actual_frame)
	{
		ReloadTaskInfo();
	}

	CUITaskItem* storylineHintItem = StorylineHintItem();

	if (storylineHintItem && storylineHintItem->show_hint && storylineHintItem->OwnerTask())
	{
		m_pMapWnd->ShowHintTask(storylineHintItem->OwnerTask(), storylineHintItem);
	}
	else if (m_pSecondaryTaskItem && m_pSecondaryTaskItem->show_hint && m_pSecondaryTaskItem->OwnerTask())
	{
		if (storylineHintItem)
		{
			storylineHintItem->show_hint = false;
		}
		m_pMapWnd->ShowHintTask(m_pSecondaryTaskItem->OwnerTask(), m_pSecondaryTaskItem);
	}
	else
	{
		m_pMapWnd->HideCurHint();
	}
	UpdateFilterHighlight();
	UpdateGamepadLegend();
	inherited::Update				();
}

void CUITaskWnd::Draw()
{
	inherited::Draw					();
}

void CUITaskWnd::DrawHint()
{
	m_pMapWnd->DrawHint();
}


void CUITaskWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if ( msg == PDA_TASK_SET_TARGET_MAP && pData )
	{
		CGameTask* task = static_cast<CGameTask*>( pData );
		TaskSetTargetMap( task );
		return;
	}
	if ( msg == PDA_TASK_SHOW_MAP_SPOT && pData )
	{
		CGameTask* task = static_cast<CGameTask*>( pData );
		TaskShowMapSpot( task, true );
		return;
	}
	if ( msg == PDA_TASK_HIDE_MAP_SPOT && pData )
	{
		CGameTask* task = static_cast<CGameTask*>( pData );
		TaskShowMapSpot( task, false );
		return;
	}
	
	if ( msg == PDA_TASK_SHOW_HINT && pData )
	{
		CGameTask* task = static_cast<CGameTask*>( pData );
		m_pMapWnd->ShowHintTask( task, pWnd );
		return;
	}
	if ( msg == PDA_TASK_HIDE_HINT )
	{
		m_pMapWnd->HideCurHint();
		return;
	}
	if ( msg == PDA_TASK_LIST_FILTER_CHANGED )
	{
		const auto mode = static_cast<ETaskListFilter>(reinterpret_cast<intptr_t>(pData));
		OnTaskListFilterChanged(mode);
		return;
	}

	inherited::SendMessage(  pWnd, msg, pData );
	CUIWndCallback::OnEvent( pWnd, msg, pData );
}

void CUITaskWnd::ReloadTaskInfo()
{
	CGameTask* primaryTask = nullptr;
	CGameTask* secondaryTask = nullptr;
	ResolveTaskRows(primaryTask, secondaryTask);

	if (m_pStoryLineTaskItem)
	{
		m_pStoryLineTaskItem->InitTask(primaryTask);
	}
	if (m_pSecondaryTaskItem)
	{
		m_pSecondaryTaskItem->InitTask(secondaryTask);
	}
	if (m_features.panelStoryline)
	{
		m_task_wnd->UpdateStorylineTask(primaryTask);
	}

	if (m_btn_focus)
	{
		if (!primaryTask || !primaryTask->HasActiveMapTarget())
		{
			m_btn_focus->Show(false);
		}
		else
		{
			m_btn_focus->Show(true);
		}
	}

	if (m_pMapWnd)
	{
		m_pMapWnd->UpdateNavTaskFocusVisibility(primaryTask);
	}

	if (m_btn_focus2)
	{
		if (!secondaryTask || !secondaryTask->HasActiveMapTarget())
			m_btn_focus2->Show(false);
		else
			m_btn_focus2->Show(true);
	}

	Locations map_locs			= Level().MapManager().Locations();
	Locations_it b				= map_locs.begin(),
				 e				= map_locs.end();
	for (; b != e; b++)
	{
		if (b->location && b->location->IsUserDefined())
		{
			const bool showPersonal = !m_cbFilters[MAP_MARKS_FILTER_PERSONAL_SPOTS] || m_bPersonalSpotsEnabled;
			showPersonal ? b->location->EnableSpot() : b->location->DisableSpot();
			continue;
		}

		shared_str spot = b->spot_type;
		if (spot == PdaMapSpot::Treasure)
			m_bTreasuresEnabled ? b->location->EnableSpot() : b->location->DisableSpot();
		else if (spot == PdaMapSpot::PrimaryObject)
			m_bPrimaryObjectsEnabled ? b->location->EnableSpot() : b->location->DisableSpot();
		else if (spot == PdaMapSpot::SecondaryTask || spot == PdaMapSpot::SecondaryTaskComplexTimer)
			m_bSecondaryTasksEnabled ? b->location->EnableSpot() : b->location->DisableSpot();
		else if (spot == PdaMapSpot::Trader || spot == PdaMapSpot::Mechanic ||
				 spot == PdaMapSpot::Scout || spot == PdaMapSpot::QuestNpc ||
				 spot == PdaMapSpot::Medic || spot == PdaMapSpot::ActorBox ||
				 spot == PdaMapSpot::ActorSleep)
			m_bQuestNpcsEnabled ? b->location->EnableSpot() : b->location->DisableSpot();
	}

	if (primaryTask || secondaryTask)
	{
		m_actual_frame = Level().GameTaskManager()->ActualFrame();
		if (m_task_wnd->IsShown())
			m_task_wnd->UpdateList();
	}

	if (!m_second_task_index)
		return;

	const bool legacyTaskCounter = (!m_btnScopeStory || m_taskScopeMode == ETaskScopeMode::Story);
	if (!legacyTaskCounter)
	{
		m_second_task_index->SetVisible(false);
		m_second_task_index->TextItemControl()->SetText("");
		return;
	}

	if (primaryTask && !secondaryTask)
	{
		const auto task_count = Level().GameTaskManager()->GetTaskCount(eTaskStateInProgress, eTaskTypeStoryline);
		if (task_count)
		{
			const auto task_index = Level().GameTaskManager()->GetTaskIndex(primaryTask, eTaskStateInProgress, eTaskTypeStoryline);
			string32 buf;
			xr_sprintf(buf, sizeof(buf), "%d / %d", task_index, task_count);

			m_second_task_index->SetVisible(true);
			m_second_task_index->TextItemControl()->SetText(buf);
		}
		else
		{
			m_second_task_index->SetVisible(false);
			m_second_task_index->TextItemControl()->SetText("");
		}
	}

	if (secondaryTask)
	{
		const auto task2_count = Level().GameTaskManager()->GetTaskCount(eTaskStateInProgress, eTaskTypeAdditional);

		if (task2_count)
		{
			const auto task2_index = Level().GameTaskManager()->GetTaskIndex(secondaryTask, eTaskStateInProgress, eTaskTypeAdditional);
			string32 buf;
			xr_sprintf(buf, sizeof(buf), "%d / %d", task2_index, task2_count);

			m_second_task_index->SetVisible(true);
			m_second_task_index->TextItemControl()->SetText(buf);
		}
		else
		{
			m_second_task_index->SetVisible(false);
			m_second_task_index->TextItemControl()->SetText("");
		}
	}
}
void CUITaskWnd::Show(bool status)
{
	inherited::Show			(status);
	m_pMapWnd->Show			(status);
	m_pMapWnd->HideCurHint	();
	m_map_legend_wnd->Show	(false);
	if ( status )
	{
		ReloadTaskInfo();
		m_task_wnd->Show( m_task_wnd_show );
	}
	else
	{
//		m_task_wnd_show = false;
		m_task_wnd->Show(false);
	}
}

void CUITaskWnd::Reset()
{
	inherited::Reset	();
}

void CUITaskWnd::OnNextTaskClicked()
{
}

void CUITaskWnd::OnPrevTaskClicked()
{
}

void CUITaskWnd::OnShowTaskListWnd( CUIWindow* w, void* d )
{
	m_task_wnd_show = !m_task_wnd_show;
	const bool showList = !m_task_wnd->IsShown();
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayPanel(showList);
	}
	m_task_wnd->Show(showList);
}

void CUITaskWnd::Show_TaskListWnd(bool status)
{
	m_task_wnd->Show( status );
	m_task_wnd_show = status;
}

bool CUITaskWnd::CanUseTaskMapSpot(CGameTask* task, bool forShow) const
{
	if (!task)
	{
		return false;
	}

	if (!forShow)
	{
		return true;
	}

	if (task->GetTaskType() == eTaskTypeStoryline)
	{
		return true;
	}

	return m_bSecondaryTasksEnabled;
}

void CUITaskWnd::TaskSetTargetMap( CGameTask* task )
{
	if (!CanUseTaskMapSpot(task, true))
	{
		return;
	}

	TaskShowMapSpot( task, true );
	CMapLocation* ml = task->LinkedMapLocation();
	if ( ml && ml->SpotEnabled() )
	{
		ml->CalcPosition();
		m_pMapWnd->SetTargetMap( ml->GetLevelName(), ml->GetPosition(), true );
	}
}

void CUITaskWnd::TaskShowMapSpot( CGameTask* task, bool show )
{
	if (!task || !CanUseTaskMapSpot(task, show))
	{
		return;
	}

	CMapLocation* ml = task->LinkedMapLocation();
	if ( ml )
	{
		if ( show )
		{
			ml->EnableSpot();
			ml->CalcPosition();
			m_pMapWnd->SetTargetMap( ml->GetLevelName(), ml->GetPosition(), true );
		}
		else
		{
			ml->DisableSpot();
		}
	}
}

void CUITaskWnd::FocusPrimaryTaskOnMap()
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::ListSelect);
	}
	CGameTask* primary = nullptr;
	CGameTask* secondary = nullptr;
	ResolveTaskRows(primary, secondary);
	TaskSetTargetMap(primary);
}

void CUITaskWnd::OnTask1DbClicked(CUIWindow*, void*)
{
	FocusPrimaryTaskOnMap();
}

void CUITaskWnd::OnTask2DbClicked(CUIWindow*, void*)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::ListSelect);
	}
	CGameTask* primary = nullptr;
	CGameTask* secondary = nullptr;
	ResolveTaskRows(primary, secondary);
	TaskSetTargetMap(secondary);
}

void CUITaskWnd::ShowMapLegend( bool status )
{
	m_map_legend_wnd->Show( status );
}

void CUITaskWnd::Switch_ShowMapLegend()
{
	const bool showLegend = !m_map_legend_wnd->IsShown();
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayPanel(showLegend);
	}
	m_map_legend_wnd->Show(showLegend);
}

void CUITaskWnd::OnShowTreasures(CUIWindow* ui, void* d)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
	m_bTreasuresEnabled = !m_bTreasuresEnabled;
	ReloadTaskInfo();
}
void CUITaskWnd::OnShowPrimaryObjects(CUIWindow* ui, void* d)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
	m_bPrimaryObjectsEnabled = !m_bPrimaryObjectsEnabled;
	ReloadTaskInfo();
}
void CUITaskWnd::OnShowSecondaryTasks(CUIWindow* ui, void* d)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
	SecondaryTasksEnabled(!m_bSecondaryTasksEnabled);
}
void CUITaskWnd::OnShowQuestNpcs(CUIWindow* ui, void* d)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
	m_bQuestNpcsEnabled = !m_bQuestNpcsEnabled;
	ReloadTaskInfo();
}

void CUITaskWnd::OnShowPersonalSpots(CUIWindow*, void*)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
	m_bPersonalSpotsEnabled = !m_bPersonalSpotsEnabled;
	ReloadTaskInfo();
}

void CUITaskWnd::OnTaskScopeStory(CUIWindow*, void*)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::Tab);
	}
	m_taskScopeMode = ETaskScopeMode::Story;
	ReloadTaskInfo();
}

void CUITaskWnd::OnTaskScopeSide(CUIWindow*, void*)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::Tab);
	}
	m_taskScopeMode = ETaskScopeMode::Side;
	ReloadTaskInfo();
}

void CUITaskWnd::OnTaskScopeFailed(CUIWindow*, void*)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::Tab);
	}
	m_taskScopeMode = ETaskScopeMode::Failed;
	ReloadTaskInfo();
}

void CUITaskWnd::ResolveTaskRows(CGameTask*& outPrimary, CGameTask*& outSecondary) const
{
	outPrimary = nullptr;
	outSecondary = nullptr;

	CGameTaskManager* tm = Level().GameTaskManager();
	if (!m_btnScopeStory)
	{
		outPrimary = tm->ActiveTask(eTaskTypeStoryline);
		if (m_pSecondaryTaskItem)
			outSecondary = tm->ActiveTask(eTaskTypeAdditional);
		return;
	}

	switch (m_taskScopeMode)
	{
	case ETaskScopeMode::Story:
		outPrimary = tm->ActiveTask(eTaskTypeStoryline);
		if (m_pSecondaryTaskItem)
			outSecondary = tm->ActiveTask(eTaskTypeAdditional);
		break;
	case ETaskScopeMode::Side:
		outPrimary = tm->ActiveTask(eTaskTypeAdditional);
		outSecondary = nullptr;
		break;
	case ETaskScopeMode::Failed:
		outPrimary = tm->IterateGet(nullptr, eTaskStateFail, eTaskTypeStoryline, true);
		if (m_pSecondaryTaskItem)
			outSecondary = tm->IterateGet(nullptr, eTaskStateFail, eTaskTypeAdditional, true);
		break;
	default:
		break;
	}
}

void CUITaskWnd::SecondaryTasksEnabled(bool enable)
{
	if (m_features.filterTabs)
	{
		return;
	}

	ApplySecondaryTasksMapFilter(enable);
}

void CUITaskWnd::ApplySecondaryTasksMapFilter(bool enable)
{
	m_bSecondaryTasksEnabled = enable;
	if (m_cbFilters[MAP_MARKS_FILTER_SECONDARY_TASKS])
	{
		m_cbFilters[MAP_MARKS_FILTER_SECONDARY_TASKS]->SetCheck(enable);
	}
	ReloadTaskInfo();
}

void CUITaskWnd::OnTaskListFilterChanged(ETaskListFilter mode)
{
	if (!m_features.filterTabs)
	{
		return;
	}

	const bool enableSecondary = mode != ETaskListFilter::Story;
	ApplySecondaryTasksMapFilter(enableSecondary);
}

CUITaskItem* CUITaskWnd::StorylineHintItem() const
{
	if (m_pStoryLineTaskItem)
	{
		return m_pStoryLineTaskItem;
	}

	if (m_features.panelStoryline && m_task_wnd)
	{
		return m_task_wnd->GetStorylineTaskItem();
	}

	return nullptr;
}

void CUITaskWnd::InitStorylineWidgets(CUIXml& xml)
{
	if (!m_features.legacyHeader)
	{
		return;
	}

	m_pStoryLineTaskItem = new CUITaskItem();
	m_pStoryLineTaskItem->Init(xml, PdaTaskXml::LegacyStorylineItem);
	AttachChild(m_pStoryLineTaskItem);
	m_pStoryLineTaskItem->SetAutoDelete(true);
	AddCallback(
		m_pStoryLineTaskItem,
		WINDOW_LBUTTON_DB_CLICK,
		CUIWndCallback::void_function(this, &CUITaskWnd::OnTask1DbClicked)
	);
}

void CUITaskWnd::InitStorylineFocusButton(CUIXml& xml)
{
	if (!m_features.legacyHeader || !xml.NavigateToNode(PdaTaskXml::LegacyTaskFocus))
	{
		return;
	}

	m_btn_focus = UIHelper::Create3tButton(xml, PdaTaskXml::LegacyTaskFocus, this);
	Register(m_btn_focus);
	AddCallback(m_btn_focus, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUITaskWnd::OnTask1DbClicked));
}

bool CUITaskWnd::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (m_pMapWnd && IsShown()
		&& (mouse_action == WINDOW_MOUSE_WHEEL_UP || mouse_action == WINDOW_MOUSE_WHEEL_DOWN))
	{
		if (m_pMapWnd->ApplyMouseWheelZoom(mouse_action))
		{
			return true;
		}
	}

	return inherited::OnMouseAction(x, y, mouse_action);
}

bool CUITaskWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED)
	{
		switch (get_binded_action(id, agUITaskMenu))
		{
			case kPDA_TASKS_MAP_SHOW_ME:
			{
				if (!m_task_wnd->IsShown())
				{
					m_pMapWnd->ViewActor();
					return true;
				}
				break;
			}
			case kPDA_TASKS_TOGGLE_LIST:
			{
				OnShowTaskListWnd(this, nullptr);
				return true;
			}
			case kPDA_TASKS_TOGGLE_LEGEND:
			{
				Switch_ShowMapLegend();
				return true;
			}
			case kPDA_TASKS_FILTER_NEXT:
			{
				if (!any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_PREV))
					SwitchToNextFilter(false);
				ActionRepeaters()->SetActionStarted(this, kPDA_TASKS_FILTER_NEXT);
				return true;
			}
			case kPDA_TASKS_FILTER_PREV:
			{
				if (!any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_NEXT))
					SwitchToPrevFilter(false);
				ActionRepeaters()->SetActionStarted(this, kPDA_TASKS_FILTER_PREV);
				return true;
			}
			case kPDA_TASKS_FILTER_TOGGLE:
			{
				if (!m_task_wnd->IsShown())
				{
					if (m_cbFilters[m_currentFilterIndex])
					{
						CUICheckButton* pChkButton = m_cbFilters[m_currentFilterIndex];
						if (pChkButton)
						{
							pChkButton->SetCheck(!pChkButton->GetCheck());
							GetMessageTarget()->SendMessage(pChkButton, BUTTON_CLICKED, nullptr);
						}
					}
					else
					{
						OnTask1DbClicked(this, nullptr);
					}
					return true;
				}
				break;
			}
		}
	}

	return inherited::OnGamepadKeyAction(id, gamepad_action);
}

bool CUITaskWnd::OnGamepadKeyHold(int id)
{
	switch (get_binded_action(id, agUITaskMenu))
	{
		case kPDA_TASKS_FILTER_NEXT:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kPDA_TASKS_FILTER_NEXT) && !any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_PREV))
				SwitchToNextFilter(false);
			return true;
		}
		case kPDA_TASKS_FILTER_PREV:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kPDA_TASKS_FILTER_PREV) && !any_binded_key_for_action_pressed_c(kPDA_TASKS_FILTER_NEXT))
				SwitchToPrevFilter(false);
			return true;
		}
	}

	return inherited::OnGamepadKeyHold(id);
}

bool CUITaskWnd::SwitchToNextFilter(bool bLoop)
{
	for (u32 step = 0; step < MAP_MARKS_FILTER_MAX; ++step)
	{
		int newFilterIndex = m_currentFilterIndex + 1;
		if (newFilterIndex >= MAP_MARKS_FILTER_MAX)
		{
			if (bLoop)
				newFilterIndex = 0;
			else
				return false;
		}
		m_currentFilterIndex = newFilterIndex;
		if (m_cbFilters[m_currentFilterIndex])
		{
			if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
			return true;
		}
	}
	return false;
}

bool CUITaskWnd::SwitchToPrevFilter(bool bLoop)
{
	for (u32 step = 0; step < MAP_MARKS_FILTER_MAX; ++step)
	{
		int newFilterIndex = m_currentFilterIndex - 1;
		if (newFilterIndex < 0)
		{
			if (bLoop)
				newFilterIndex = MAP_MARKS_FILTER_MAX - 1;
			else
				return false;
		}
		m_currentFilterIndex = newFilterIndex;
		if (m_cbFilters[m_currentFilterIndex])
		{
			if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
			return true;
		}
	}
	return false;
}

void CUITaskWnd::UpdateFilterHighlight()
{
	for (int i = 0; i < MAP_MARKS_FILTER_MAX; ++i)
	{
		if (m_cbFilters[i])
		{
			m_cbFilters[i]->SetHighlighted(i == m_currentFilterIndex && pInput->GetControllerMode());
		}
	}

}

void CUITaskWnd::UpdateGamepadLegend()
{
	if (!m_gamepad_legend)
	{
		return;
	}

	CUIWindow* actionAccept = m_gamepad_legend->FindChild("action_accept");
	if (actionAccept)
	{
		if (CUIStatic* actionAcceptS = actionAccept->ui_cast_static())
		{
			{
				bool hasAnyFilter = false;
				for (int fi = 0; fi < MAP_MARKS_FILTER_MAX; ++fi)
				{
					if (m_cbFilters[fi])
					{
						hasAnyFilter = true;
						break;
					}
				}
				actionAcceptS->SetTextST(m_task_wnd->IsShown() ? "ui_tasks_show_on_map" : hasAnyFilter ? "ui_tasks_filter_toggle" : "ui_tasks_show_on_map_main");
			}
		}
	}

	CUIWindow* showMissions = m_gamepad_legend->FindChild("show_missions");
	if (showMissions)
	{
		if (CUIStatic* showMissionsS = showMissions->ui_cast_static())
		{
			showMissionsS->SetTextST(m_task_wnd->IsShown() ? "ui_pda_hide_mission" : "ui_pda_show_mission");
		}
	}

	CUIWindow* filterNext = m_gamepad_legend->FindChild("filter_next");
	if (filterNext)
	{
		{
			bool hasAnyFilter = false;
			for (int fi = 0; fi < MAP_MARKS_FILTER_MAX; ++fi)
			{
				if (m_cbFilters[fi])
				{
					hasAnyFilter = true;
					break;
				}
			}
			filterNext->Show(hasAnyFilter);
		}
	}

}

// --------------------------------------------------------------------------------------------------
CUITaskItem::CUITaskItem() : m_owner(nullptr), show_hint_can(false), show_hint(false), m_hint_wt(500) {}

CUIStatic* init_static_field(CUIXml& uiXml, const char* path, const char* path2);

void CUITaskItem::Init(CUIXml& uiXml, const char* path)
{
	CUIXmlInit::InitWindow			(uiXml,path,0,this);
	m_hint_wt						= uiXml.ReadAttribInt(path, 0, "hint_wt", 500);

	string256		buff;
	CUIStatic* S					= nullptr;

	xr_strconcat(buff, path, ":t_icon" );
	if ( uiXml.NavigateToNode( buff ) )
	{
		S = init_static_field		(uiXml, path, "t_icon");
		AttachChild					(S);
	}
	m_info["t_icon"]				= S;
	
	xr_strconcat(buff, path, ":t_icon_over" );
	if ( uiXml.NavigateToNode( buff ) )
	{
		S = init_static_field		(uiXml, path, "t_icon_over");
		AttachChild					(S);
	}
	m_info["t_icon_over"]			= S;
	
	S = init_static_field			(uiXml, path, "t_caption");
	AttachChild						(S);
	m_info["t_caption"]				= S;

	show_hint_can = false;
	show_hint     = false;
}

void CUITaskItem::InitTask(CGameTask* task)
{
	m_owner							= task;
	CUIStatic* S					= m_info["t_icon"];
	if ( S )
	{
		if ( task )
		{
			S->InitTexture			(task->m_icon_texture_name.c_str());
			S->TextureOn();
			Frect emptyRect = Frect().set(0.f, 0.f, 0.f, 0.f);
			if (!task->m_icon_rect.cmp(emptyRect))
			{
				Frect r = task->m_icon_rect;
				Frect texture_rect;

				texture_rect.lt.set(r.x1, r.y1);
				texture_rect.rb.set(r.x2, r.y2);
				texture_rect.rb.add(texture_rect.lt);
				S->SetTextureRect(texture_rect);
			}
			S->SetStretchTexture	(true);
			m_info["t_icon_over"]->Show(true);
		}
		else
		{
			S->TextureOff			();
			m_info["t_icon_over"]->Show(false);
		}
	}

	S								= m_info["t_caption"];
	shared_str finalStr = "";
	if (task)
	{
		finalStr = g_pStringTable->ParseStringFromScript(task->m_Title);
	}
	S->TextItemControl()->SetTextST	(finalStr.c_str());
}

void CUITaskItem::OnFocusReceive()
{
	inherited::OnFocusReceive();
	show_hint_can = true;
	show_hint     = false;
}

void CUITaskItem::OnFocusLost()
{
	inherited::OnFocusLost();
	show_hint_can = false;
	show_hint     = false;
}

void CUITaskItem::Update()
{
	inherited::Update();
	if ( m_owner && m_bCursorOverWindow && show_hint_can )
	{
		if ( Device.dwTimeContinual > ( m_dwFocusReceiveTime + m_hint_wt ) )
		{
			show_hint = true;
			return;
		}
	}
}

void CUITaskItem::OnMouseScroll( float iDirection )
{
}

bool CUITaskItem::OnMouseAction( float x, float y, EUIMessages mouse_action )
{
	if ( inherited::OnMouseAction( x, y, mouse_action ) )
	{
		//return true;
	}

	switch ( mouse_action )
	{
	case WINDOW_LBUTTON_DOWN:
	case WINDOW_RBUTTON_DOWN:
	case BUTTON_DOWN:
		show_hint_can = false;
		show_hint     = false;
		break;
	}//switch

	return true;
}

void CUITaskItem::SendMessage( CUIWindow* pWnd, s16 msg, void* pData )
{
	inherited::SendMessage( pWnd, msg, pData );
}
