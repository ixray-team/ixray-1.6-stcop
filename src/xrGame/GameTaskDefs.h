#pragma once
#include "alife_abstract_registry.h"

enum ETaskState{
	eTaskStateFail			= 0,
	eTaskStateInProgress,
	eTaskStateCompleted,
	eTaskStateDummy			= u16(-1)
};

typedef shared_str		TASK_ID;

using TASK_ID_VECTOR = xr_vector<TASK_ID>;
using TASK_ID_IT = TASK_ID_VECTOR::iterator;

// all task has `storyline`-type now (10.10.2008)(sea)
enum ETaskType{
	eTaskTypeStoryline		= 0,
	eTaskTypeAdditional,
	eTaskTypeInsignificant,
	eTaskTypeCount,
	eTaskTypeDummy	= u16(-1)
};

constexpr auto ROOT_TASK_OBJECTIVE = static_cast<u16>(0); // task itself

extern shared_str g_active_task_id[eTaskTypeCount];

class CGameTask;

struct SGameTaskKey : public IPureSerializeObject<IReader,IWriter>,public IPureDestroyableObject 
{
	shared_str		task_id;

	SGameTaskKey	(const shared_str& t_id):task_id(t_id),game_task(NULL)	{};
	SGameTaskKey	():task_id(NULL),game_task(NULL)						{};


	virtual void 	save								(IWriter &stream);
	virtual void 	load								(IReader &stream);
	virtual void serialize(ISaveObject& Object);
	virtual void 	destroy								();

	IC CGameTask* getGameTask(void) const { return game_task; }
	void setGameTask(CGameTask* pTask) { game_task = pTask; }

private:
	CGameTask* game_task;
};

ISaveObject& operator<<(ISaveObject& Object, SGameTaskKey& Data);

using vGameTasks = xr_vector<SGameTaskKey>;
using vGameTasks_it = vGameTasks::iterator;

struct CGameTaskRegistry : public CALifeAbstractRegistry<u16, vGameTasks> 
{
	virtual void save(IWriter &stream)
	{
		CALifeAbstractRegistry<u16, vGameTasks>::save(stream);
		for (auto& taskId : g_active_task_id)
		{
			save_data(taskId, stream);
		}
	};
	virtual void load(IReader &stream)
	{
		CALifeAbstractRegistry<u16, vGameTasks>::load(stream);
		for (auto& taskId : g_active_task_id)
		{
			load_data(taskId, stream);
		}
	};
};
