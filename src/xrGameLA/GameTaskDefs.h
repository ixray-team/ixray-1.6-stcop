#pragma once

enum ETaskState {
	eTaskStateFail			= 0,
	eTaskStateInProgress,
	eTaskStateCompleted,
	eTaskStateSkiped,
//.	eTaskUserDefined,
	eTaskStateDummy			= u32(-1)
};


typedef shared_str		TASK_ID;
using TASK_ID_VECTOR = xr_vector<TASK_ID>;
using TASK_ID_IT = TASK_ID_VECTOR::iterator;

extern shared_str		g_active_task_id;
extern u16				g_active_task_objective_id;

#include "alife_abstract_registry.h"

class CGameTask;

struct SGameTaskKey : public IPureSerializeObject<IReader,IWriter>,public IPureDestroyableObject {
	TASK_ID			task_id;
	CGameTask*		game_task;
	SGameTaskKey	(TASK_ID t_id):task_id(t_id),game_task(nullptr){};
	SGameTaskKey	():task_id(nullptr),game_task(nullptr){};


	virtual void save								(IWriter &stream);
	virtual void load								(IReader &stream);
	virtual void destroy							();
};

//#define DEFINE_VECTOR(T,N,I)		typedef xr_vector< T > N;		typedef N::iterator I;

//DEFINE_VECTOR(SGameTaskKey, GameTasksVec, GameTasks_it);


typedef xr_vector<SGameTaskKey> GameTasksVec; 
typedef GameTasksVec::iterator GameTasks_it;

struct CGameTaskRegistry : public CALifeAbstractRegistry<u16, GameTasksVec> {
	virtual void save(IWriter &stream){
		CALifeAbstractRegistry::save(stream);
		save_data		(g_active_task_id,				stream);
		save_data		(g_active_task_objective_id,	stream);
	};
	virtual void load(IReader &stream){
		CALifeAbstractRegistry::load(stream);
		load_data		(g_active_task_id,				stream);
		load_data		(g_active_task_objective_id,	stream);
	};
};
