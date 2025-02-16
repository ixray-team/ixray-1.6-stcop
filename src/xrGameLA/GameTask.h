#pragma once

#include "encyclopedia_article_defs.h"
#include "GameTaskDefs.h"
#include "script_export_space.h"
#include "pch_script.h"

class CGameTaskManager;
class CMapLocation;
class CGameTask;

typedef xr_vector<luabind::functor<bool> > task_state_functors;

class SScriptObjectiveHelper: public IPureSerializeObject<IReader,IWriter>
{
public:
	xr_vector<shared_str>	m_s_complete_lua_functions;
	xr_vector<shared_str>	m_s_fail_lua_functions;
	xr_vector<shared_str>	m_s_skiped_lua_functions;

	xr_vector<shared_str>	m_s_lua_functions_on_complete;
	xr_vector<shared_str>	m_s_lua_functions_on_fail;
	xr_vector<shared_str>	m_s_lua_functions_on_skiped;
public:
	bool			not_empty		() {return m_s_complete_lua_functions.size()	||
												m_s_fail_lua_functions.size()		||
												m_s_skiped_lua_functions.size()		||
												m_s_lua_functions_on_complete.size()||
												m_s_lua_functions_on_fail.size()	||
												m_s_lua_functions_on_skiped.size();
	}

	virtual void			save			(IWriter &stream);
	virtual void			load			(IReader &stream);
			
			void			init_functors	(xr_vector<shared_str>& v_src, task_state_functors& v_dest);
};

class SGameTaskObjective : public IPureSerializeObject<IReader,IWriter>
{
	friend struct SGameTaskKey;
	friend class CGameTaskManager;
private:
	ETaskState				task_state;
	CGameTask*				parent;
	int						idx;
	void					SendInfo		(xr_vector<shared_str>&);
	void					CallAllFuncs	(xr_vector<luabind::functor<bool> >& v);
	bool					CheckInfo		(xr_vector<shared_str>&);
	bool					CheckFunctions	(xr_vector<luabind::functor<bool> >& v);
	void					SetTaskState	(ETaskState new_state);
public:
	SScriptObjectiveHelper	m_pScriptHelper;
	virtual void			save			(IWriter &stream);
	virtual void			load			(IReader &stream);
	
	SGameTaskObjective		(CGameTask* parent, int idx);
	SGameTaskObjective		();
	shared_str				description;
	shared_str				article_id;
	shared_str				map_hint;
	shared_str				map_location;
	u16						object_id;
	shared_str				article_key;
	CMapLocation*			LinkedMapLocation	();
	ETaskState				TaskState			()	{return task_state;};
	ETaskState				UpdateState			();

	shared_str							icon_texture_file_name;
	shared_str							icon_texture_node_name; // for storing name of texture node from texture xml
	Frect								icon_rect;
	bool								def_location_enabled;
	bool								visible_subtask;
//complete/fail stuff
	xr_vector<shared_str>				m_completeInfos;
	xr_vector<shared_str>				m_failInfos;
	xr_vector<shared_str>				m_skipedInfos;
	xr_vector<shared_str>				m_infos_on_complete;
	xr_vector<shared_str>				m_infos_on_fail;
	xr_vector<shared_str>				m_infos_on_skiped;

	task_state_functors						m_fail_lua_functions;
	task_state_functors						m_complete_lua_functions;
	task_state_functors						m_skiped_lua_functions;

	task_state_functors						m_lua_functions_on_complete;
	task_state_functors						m_lua_functions_on_fail;
	task_state_functors						m_lua_functions_on_skiped;

// for scripting access
	void					SetDescription_script	(LPCSTR _descr);
	void					SetArticleID_script		(LPCSTR _id);
	int						GetIDX_script			()					{return idx;};
	void					SetMapHint_script		(LPCSTR _str);
	void					SetMapLocation_script	(LPCSTR _str);

	void					SetObjectID_script		(u16 id);
	LPCSTR					GetIconName_script		()		{ return icon_texture_node_name.size() ? icon_texture_node_name.c_str() : "null icon"; };

	void					SetArticleKey_script	(LPCSTR _str);

	void					SetIconName_script		(LPCSTR _str);

	void					AddCompleteInfo_script	(LPCSTR _str);
	void					AddFailInfo_script		(LPCSTR _str);
	void					AddSkipedInfo_script	(LPCSTR _str);
	void					AddOnCompleteInfo_script(LPCSTR _str);
	void					AddOnFailInfo_script	(LPCSTR _str);
	void					AddOnSkipedInfo_script  (LPCSTR _str);

	void					AddCompleteFunc_script	(LPCSTR _str);
	void					AddFailFunc_script		(LPCSTR _str);
	void					AddSkipedFunc_script	(LPCSTR _str);
	void					AddOnCompleteFunc_script(LPCSTR _str);
	void					AddOnFailFunc_script	(LPCSTR _str);
	void					AddOnSkipedFunc_script	(LPCSTR _str);
	LPCSTR					GetDescription_script	()					{return *description;};
	void					ChangeStateCallback		();
	void					ShowSubtask			(bool show);
};

using OBJECTIVE_VECTOR = xr_vector<SGameTaskObjective>;
using OBJECTIVE_VECTOR_IT = OBJECTIVE_VECTOR::iterator;

class CGameTask
{
private:
							CGameTask				(const CGameTask&){}; //disable copy ctor
protected:
	void					Load					(const TASK_ID& id);
public:
							CGameTask				(const TASK_ID& id);
							CGameTask				();

	bool					HasLinkedMapLocations	();
	bool					HasInProgressObjective	();

	SGameTaskObjective&		Objective				(int objectice_id)	{return m_Objectives[objectice_id];};

	TASK_ID					m_ID;
	shared_str				m_Title;
	OBJECTIVE_VECTOR		m_Objectives;
	ALife::_TIME_ID			m_ReceiveTime;
	ALife::_TIME_ID			m_FinishTime;
	ALife::_TIME_ID			m_TimeToComplete;
	u32						m_priority;

// for scripting access
	void					Load_script				(LPCSTR _id);
	void					SetTitle_script			(LPCSTR _title);
	LPCSTR					GetTitle_script			()							{return *m_Title;};
	void					SetPriority_script		(int _prio);
	int						GetPriority_script		()							{return m_priority;};
	void					AddObjective_script		(SGameTaskObjective* O);
	SGameTaskObjective*		GetObjective_script		(int objective_id)			{return &(Objective(objective_id));}
	LPCSTR					GetID_script			()							{return *m_ID;}
	void					SetID_script			(LPCSTR _id)				{m_ID = _id;}
	int						GetObjectiveSize_script	()							{return m_Objectives.size();}

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
add_to_type_list(CGameTask)
#undef script_type_list
#define script_type_list save_type_list(CGameTask)
