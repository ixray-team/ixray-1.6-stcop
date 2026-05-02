#pragma once

#include "encyclopedia_article_defs.h"
#include "GameTaskDefs.h"
#include "../xrScripts/script_export_space.h"
#include <luabind/functor.hpp>

class CGameTaskManager;
class CMapLocation;
class CGameTask;

typedef xr_vector<luabind::functor<bool> > task_state_functors;

class SScriptTaskHelper: public IPureSerializeObject<IReader,IWriter>
{
public:
	xr_vector<shared_str>					m_s_complete_lua_functions;
	xr_vector<shared_str>					m_s_fail_lua_functions;

	xr_vector<shared_str>					m_s_lua_functions_on_complete;
	xr_vector<shared_str>					m_s_lua_functions_on_fail;
public:
	bool					not_empty		()	{return m_s_complete_lua_functions.size()	||
														m_s_fail_lua_functions.size()		||
														m_s_lua_functions_on_complete.size()||
														m_s_lua_functions_on_fail.size() ;}

	virtual void			save			(IWriter &stream);
	virtual void			load			(IReader &stream);
			
			void			init_functors	(xr_vector<shared_str>& v_src, task_state_functors& v_dest);
};

class SGameTaskObjective : public IPureSerializeObject<IReader, IWriter>
{
    friend struct SGameTaskKey;
    friend class CGameTask;
    friend class CGameTaskManager;

protected:
    CGameTask* m_parent;
    ETaskState m_task_state;
    ETaskType m_task_type;
    SScriptTaskHelper m_pScriptHelper;

public:
    u16 m_idx;
    shared_str m_Title;
    shared_str m_Description;

    // encyclopedia
    shared_str m_article_id;
    shared_str m_article_key;

    // icon
    Frect m_icon_rect;
    shared_str m_icon_texture_name;

    // map
    shared_str m_map_hint;
    shared_str m_map_location;
    u16 m_map_object_id;
    bool m_def_location_enabled;
    CMapLocation* m_linked_map_location;

    // timing
    ALife::_TIME_ID m_ReceiveTime;
    ALife::_TIME_ID m_FinishTime;
    ALife::_TIME_ID m_TimeToComplete;
    ALife::_TIME_ID m_timer_finish;
    bool m_rewardPending;

private:
    // infos
    xr_vector<shared_str> m_completeInfos;
    xr_vector<shared_str> m_failInfos;
    xr_vector<shared_str> m_infos_on_complete;
    xr_vector<shared_str> m_infos_on_fail;

    // functions
    task_state_functors m_fail_lua_functions;
    task_state_functors m_complete_lua_functions;

    task_state_functors m_lua_functions_on_complete;
    task_state_functors m_lua_functions_on_fail;

public:
    SGameTaskObjective();
    SGameTaskObjective(CGameTask* parent, u16 idx);

    CGameTask* GetParent() const { return m_parent; }

    void SetTaskState(ETaskState state);
    auto GetTaskState() const { return m_task_state; }

    auto GetTaskType() const { return m_task_type; }
    virtual CMapLocation* LinkedMapLocation();

    ETaskState UpdateState();

    void save(IWriter& stream) override;
    void load(IReader& stream) override;

private:
    void SendInfo(const xr_vector<shared_str>&);
    bool CheckInfo(const xr_vector<shared_str>&) const;
    void CallAllFuncs(const task_state_functors& v);
    bool CheckFunctions(const task_state_functors& v) const;

protected:
    virtual void ChangeStateCallback();
    void CreateMapLocation(bool on_load);

public:
    void RemoveMapLocations(bool notify);
    void ChangeMapLocation(const char* new_map_location, u16 new_map_object_id);

    // for scripting access
    auto GetType_script() const { return m_task_type; }
    void SetType_script(int t)  { m_task_type = (ETaskType)t; }

    auto GetID() const { return m_idx; }

    auto GetTitle_script() const { return m_Title.c_str(); }
    void SetTitle_script(const char* title) { m_Title = title; }

    auto GetDescription_script() const { return m_Description.c_str(); }
    void SetDescription_script(const char* desc) { m_Description = desc; }

    // encyclopedia
    void SetArticleID_script(const char* id) { m_article_id = id; }
    void SetArticleKey_script(const char* key) { m_article_key = key; }

    auto GetIconName_script() const { return m_icon_texture_name.c_str(); }
    void SetIconName_script(const char* tex);

    // map
    void SetMapHint_script(const char* hint) { m_map_hint = hint; }
    void SetMapLocation_script(const char* mls) { m_map_location = mls; }
    void SetMapObjectID_script(int id) { m_map_object_id = (u16)id; }

    // callbacks and infos
    void AddCompleteInfo_script(const char* str);
    void AddCompleteFunc_script(const char* str);

    void AddOnCompleteInfo_script(const char* str);
    void AddOnCompleteFunc_script(const char* str);

    void AddFailInfo_script(const char* str);
    void AddFailFunc_script(const char* str);

    void AddOnFailInfo_script(const char* str);
    void AddOnFailFunc_script(const char* str);

    void CommitScriptHelperContents();
};

using OBJECTIVES_VECTOR = xr_vector<SGameTaskObjective>;

class CGameTask : public SGameTaskObjective
{
public:
    shared_str  m_ID;
    u32 m_priority;
    bool m_read;
    bool m_remoteAllowed;
    // Aggregate: true if any objective completed while rewards were deferred (e.g. PDA session).
    // Distinct from SGameTaskObjective::m_rewardPending (per-objective deferral).
    bool m_hasPendingRewardDispatch;

private:
    OBJECTIVES_VECTOR m_Objectives;
    u16 m_active_objective{ ROOT_TASK_OBJECTIVE };

public:
    CGameTask();
    CGameTask(const TASK_ID& id);

    void Load(const shared_str& id);

    void save(IWriter& stream) override;
    void load(IReader& stream) override;

    void ChangeStateCallback() override;

    u16 ActiveObjectiveIdx() const;
    SGameTaskObjective& ActiveObjective() { return Objective(m_active_objective); }
    SGameTaskObjective& Objective(u16 idx);
    const SGameTaskObjective& Objective(u16 idx) const;
    ETaskState ObjectiveState(u16 idx) const;
    void SetActiveObjective(u16 idx);
    u16 GetObjectivesCount(bool without_root = false) const;

    using SGameTaskObjective::SetTaskState;
    void SetTaskState(ETaskState state, u16 objective_id);
    bool HasObjectiveInProgress() const;

    // map
    void OnArrived();
    CMapLocation* LinkedMapLocation() override;
    bool HasActiveMapTarget() const;

    void FillEncyclopedia() const;

    // for scripting access
    void Load_script(const char* id) { Load(id); }
    
    auto GetID_script() const { return m_ID.c_str(); }
    void SetID_script(const char* id) { m_ID = id; }

    auto GetPriority_script() const { return m_priority; }
    void SetPriority_script(int prio) { m_priority = prio; }
    bool IsRemoteAllowed_script() const { return m_remoteAllowed; }
    void SetRemoteAllowed_script(bool isAllowed) { m_remoteAllowed = isAllowed; }
    bool IsRewardPending_script() const { return m_hasPendingRewardDispatch; }
    void SetRewardPending_script(bool isPending) { m_hasPendingRewardDispatch = isPending; }

    void AddObjective_script(SGameTaskObjective* O);
    SGameTaskObjective* GetObjective_script(u16 objective_id);
    DECLARE_SCRIPT_REGISTER_FUNCTION
};
