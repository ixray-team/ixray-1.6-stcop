#include "StdAfx.h"
#include "pch_script.h"
#include "GametaskManager.h"
#include "alife_registry_wrappers.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "GameTask.h"
#include "Level.h"
#include "map_manager.h"
#include "map_location.h"
#include "Actor.h"
#include "UIGameSP.h"
#include "ui/UIPdaWnd.h"
#include "encyclopedia_article.h"
#include "ui/UIMapWnd.h"

#pragma warning(push)
#pragma warning(disable:4995)
#include <malloc.h>
#pragma warning(pop)

shared_str g_active_task_id[eTaskTypeCount] =
{
	g_active_task_no_task___internal,
	g_active_task_no_task___internal,
	g_active_task_no_task___internal
};

struct FindTaskByID{
	shared_str	id;
	bool		b_only_inprocess;
	FindTaskByID(const shared_str& s, bool search_only_inprocess):id(s),b_only_inprocess(search_only_inprocess){}
	bool operator () (const SGameTaskKey& key)
		{
			if(b_only_inprocess)
				return (id == key.task_id && (key.getGameTask() && key.getGameTask()->GetTaskState() == eTaskStateInProgress));
			else
				return (id==key.task_id);
		}
};

bool task_prio_pred(const SGameTaskKey& k1, const SGameTaskKey& k2)
{
	return k1.getGameTask() && k2.getGameTask() && k1.getGameTask()->m_priority > k2.getGameTask()->m_priority;
}

CGameTaskManager::CGameTaskManager()
{
	m_gametasks_wrapper			= new CGameTaskWrapper();
	m_gametasks_wrapper->registry().init(0);// actor's id
	m_flags.zero				();
	m_flags.set					(eChanged, TRUE);
	m_gametasks					= nullptr;

	for (auto& taskId : g_active_task_id)
	{
		if (!taskId.size())
			taskId = g_active_task_no_task___internal;

		if (taskId != g_active_task_no_task___internal)
		{
			CGameTask* t = HasGameTask(taskId, true);
			if (t)
				SetActiveTask(t);
		}
	}
}

CGameTaskManager::~CGameTaskManager()
{
	delete_data					(m_gametasks_wrapper);
	for (auto& taskId : g_active_task_id)
		taskId = nullptr;
}

vGameTasks&	CGameTaskManager::GetGameTasks	() 
{
	if(!m_gametasks)
	{
		m_gametasks = &m_gametasks_wrapper->registry().objects();
#ifdef DEBUG
		Msg("m_gametasks size=%d",m_gametasks->size());
#endif // #ifdef DEBUG
	}

	return *m_gametasks;
}

CGameTask* CGameTaskManager::HasGameTask(const shared_str& id, bool only_inprocess)
{
	FindTaskByID key(id, only_inprocess);
	vGameTasks_it it = std::find_if(GetGameTasks().begin(),GetGameTasks().end(),key);
	if( it!=GetGameTasks().end() )
		return (*it).getGameTask();
	
	return 0;
}

CGameTask*	CGameTaskManager::GiveGameTaskToActor(CGameTask* t, u32 timeToComplete, bool bCheckExisting, u32 timer_ttl)
{
	t->CommitScriptHelperContents	();
	if(/* bCheckExisting &&*/ HasGameTask(t->m_ID, true) ) 
	{
 		Msg("! task [%s] already inprocess",t->m_ID.c_str());
		VERIFY2( 0, make_string<const char*>( "give_task : Task [%s] already inprocess!", t->m_ID.c_str()) );
		return nullptr;
	}

	m_flags.set						(eChanged, TRUE);

	GetGameTasks().push_back		(SGameTaskKey(t->m_ID) );
	GetGameTasks().back().setGameTask(t);
	t->m_ReceiveTime				= Level().GetGameTime();
	t->m_TimeToComplete				= t->m_ReceiveTime + timeToComplete * 1000; //ms
	t->m_timer_finish				= t->m_ReceiveTime + timer_ttl      * 1000; //ms

	std::stable_sort				(GetGameTasks().begin(), GetGameTasks().end(), task_prio_pred);

	t->OnArrived					();

	if (!m_flags.test(eMultipleTasks))
		SetActiveTask(t);
	else
	{
		const ETaskType taskType = t->GetTaskType();
		CGameTask* activeTask = ActiveTask(t->GetTaskType());
		if (taskType == eTaskTypeStoryline || taskType == eTaskTypeAdditional)
		{
			if ((activeTask == nullptr) || (activeTask->m_priority < t->m_priority))
			{
				SetActiveTask(t);
			}
		}
	}

	//установить флажок необходимости прочтения тасков в PDA
	if ( CurrentGameUI() )
		CurrentGameUI()->UpdatePda();

	t->ChangeStateCallback();

	return t;
}

void CGameTaskManager::test_groid()
{
	int a = 0;
}

void CGameTaskManager::SetTaskState(CGameTask* t, ETaskState state)
{
	PROF_EVENT("CGameTaskManager::SetTaskState");
	m_flags.set						(eChanged, TRUE);

    ETaskType type = eTaskTypeStoryline;
    if (m_flags.test(eMultipleTasks))
        type = t->GetTaskType();

    t->SetTaskState(state);

    if (ActiveTask(type) == t)
    {
        //SetActiveTask	("", t->GetTaskType());
        g_active_task_id[type] = "";
    }

	if ( CurrentGameUI() )
		CurrentGameUI()->UpdatePda();
}

void CGameTaskManager::SetTaskState(const shared_str& id, ETaskState state)
{
	CGameTask* t				= HasGameTask(id, true);
	if (nullptr==t)				{Msg("actor does not has task [%s] or it is completed", *id);	return;}
	SetTaskState				(t, state);
}

void CGameTaskManager::UpdateTasks						()
{
	if(Device.Paused())
		return;
		
	PROF_EVENT("CGameTaskManager::UpdateTasks");

	Level().MapManager().DisableAllPointers();

	if(GetGameTasks().empty())	
		return;

	
	const vGameTasks& vTasks = GetGameTasks();

	for (const SGameTaskKey& task : vTasks)
	{
		CGameTask* const pGameTask = task.getGameTask();

		if (pGameTask)
		{
			if (pGameTask->GetTaskState() != eTaskStateInProgress)
				continue;

			const ETaskState state = pGameTask->UpdateState();

			if ((state == eTaskStateFail) || (state == eTaskStateCompleted))
				SetTaskState(pGameTask, state);
		}
	}
	

	for (int i = 0; i < eTaskTypeCount; ++i)
	{
		CGameTask* activeTask = ActiveTask(static_cast<ETaskType>(i));
		if (activeTask)
		{
			CMapLocation* ml = activeTask->LinkedMapLocation();
			if (ml && !ml->PointerEnabled())
				ml->EnablePointer();
		}
	}

	if(	m_flags.test(eChanged) )
		UpdateActiveTask();
}


void CGameTaskManager::UpdateActiveTask()
{
	std::stable_sort			(GetGameTasks().begin(), GetGameTasks().end(), task_prio_pred);

    for (u32 i = eTaskTypeStoryline; i < eTaskTypeCount; ++i)
    {
        CGameTask* activeTask = ActiveTask(static_cast<ETaskType>(i));
        if (!activeTask)
        {
            CGameTask* frontTask = IterateGet(nullptr, eTaskStateInProgress, static_cast<ETaskType>(i), true);
            if (frontTask)
                SetActiveTask(frontTask);
        }
    }

	m_flags.set					(eChanged, FALSE);
	m_actual_frame				= Device.dwFrame;
}

CGameTask* CGameTaskManager::ActiveTask(ETaskType type)
{
	ETaskType t = eTaskTypeStoryline;
	if (m_flags.test(eMultipleTasks))
		t = type;

	shared_str& t_id = g_active_task_id[t];

	if (!t_id.size())
		t_id = g_active_task_no_task___internal;

	if (t_id == g_active_task_no_task___internal)
		return nullptr;

	return HasGameTask(t_id, true);
}
/*
void CGameTaskManager::SetActiveTask(const shared_str& id, ETaskType type)
{
	ETaskType t = eTaskTypeStoryline;
	if (m_flags.test(eMultipleTasks))
		t = type;

	g_active_task_id[t] = id;
	m_flags.set(eChanged, TRUE);
	m_read = true;
}*/

void CGameTaskManager::SetActiveTask(CGameTask* task)
{
    VERIFY(task);
    if (task)
    {
        ETaskType type = eTaskTypeStoryline;
        if (m_flags.test(eMultipleTasks))
            type = task->GetTaskType();

        g_active_task_id[type] = task->m_ID;
        m_flags.set(eChanged, TRUE);
        task->m_read = true;
    }
}

CUIMapWnd* GetMapWnd();

void CGameTaskManager::MapLocationRelcase(CMapLocation* ml)
{
	CUIMapWnd* mwnd = GetMapWnd();
	if(mwnd)
		mwnd->MapLocationRelcase(ml);

	CGameTask* gt = HasGameTask(ml, false);
	if(gt)
		gt->RemoveMapLocations(true);
}

CGameTask* CGameTaskManager::HasGameTask(const CMapLocation* ml, bool only_inprocess)
{
	vGameTasks_it it		= GetGameTasks().begin();
	vGameTasks_it it_e		= GetGameTasks().end();

	for(; it!=it_e; ++it)
	{
		CGameTask* gt = (*it).getGameTask();
		if(gt->LinkedMapLocation()==ml)
		{
			if(only_inprocess && gt->GetTaskState()!=eTaskStateInProgress)
				continue;

			return gt;
		}
	}
	return nullptr;
}

CGameTask* CGameTaskManager::IterateGet(CGameTask* t, ETaskState state, ETaskType type, bool bForward)
{
	vGameTasks& v		= GetGameTasks();
	u32 cnt				= (u32)v.size();
	for(u32 i=0; i<cnt; ++i)
	{
		CGameTask* gt	= v[i].getGameTask();
		if(gt==t || nullptr==t)
		{
			bool			allow;
			if(bForward)	
			{
				if(t)		++i;
				allow		= i < cnt;
			}else
			{
				allow		= (i>0) && (--i >= 0);
			}
			if(allow)
			{
				CGameTask* found		= v[i].getGameTask();
				if (found->GetTaskState() == state && found->GetTaskType() == type)
					return found;
				else
					return IterateGet(found, state, type, bForward);
			}else
				return nullptr;
		}
	}
	return nullptr;
}

u32 CGameTaskManager::GetTaskIndex(CGameTask* t, ETaskState state, ETaskType type)
{
	if ( !t )
	{
		return 0;
	}

	vGameTasks& v	= GetGameTasks();
	u32 cnt			= (u32)v.size();
	u32 res			= 0;
	for ( u32 i = 0; i < cnt; ++i )
	{
		CGameTask* gt = v[i].getGameTask();
		if (gt->GetTaskType() == type && gt->GetTaskState() == state)
		{
			++res;
			if ( gt == t )
			{
				return res;
			}
		}
	}
	return 0;
}

u32 CGameTaskManager::GetTaskCount(ETaskState state, ETaskType type)
{
	vGameTasks& v	= GetGameTasks();
	u32 cnt			= (u32)v.size();
	u32 res			= 0;
	for ( u32 i = 0; i < cnt; ++i )
	{
		CGameTask* gt = v[i].getGameTask();
		if (gt->GetTaskType() == type && gt->GetTaskState() == state)
		{
			++res;
		}
	}
	return res;
}

constexpr pcstr sTaskStates[] = 
{ 
	"TaskStateFail", 
	"TaskStateInProgress", 
	"TaskStateCompleted", 
	"TaskStateDummy" 
};
constexpr pcstr sTaskTypes[] = 
{ 
	"TaskTypeStoryline", 
	"TaskTypeAdditional", 
	"TaskTypeInsignificant", 
};

void CGameTaskManager::DumpTasks()
{
	for (auto& it : GetGameTasks())
	{
		const CGameTask* gt = it.getGameTask();
		Msg( " ID=[%s] state=[%s] prio=[%d] ",
			gt->m_ID.c_str(),
			sTaskTypes[gt->GetTaskType()],
			sTaskStates[gt->GetTaskState()],
			gt->m_priority);
	}
}

CGameTaskManager* get_task_manager() { return Level().GameTaskManager(); }

void CGameTaskManager::script_register(lua_State* pState)
{
	if (pState)
	{
		luabind::module(pState)
			[
				// register class
				luabind::class_<CGameTaskManager>("game_task_manager")
					.def("give_task", &CGameTaskManager::GiveGameTaskToActor),

				// register globals
				luabind::def("get_game_task_manager", get_task_manager)
			];
	}
}
