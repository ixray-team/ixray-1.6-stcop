////////////////////////////////////////////////////////////////////////////
//	Module 		: action_base.h
//	Created 	: 28.01.2004
//  Modified 	: 10.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Base action
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "property_storage.h"
#include "../xrScripts/script_export_space.h"
#include "operator_abstract.h"
#include "alife_space.h"

class CScriptGameObject;

template <typename _object_type>
class CActionBase :
	public GraphEngineSpace::CWorldOperator 
{
protected:
	using inherited = GraphEngineSpace::CWorldOperator;
	using COperatorCondition = CWorldProperty;

protected:
	enum EActionStates
	{
		eActionStateConstructed		= u32(0),
		eActionStateSetup,
		eActionStateInitialized,
		eActionStateExecuted,
		eActionStateFinalized,
		eActionStateDummy			= u32(-1),
	};

public:
	_object_type				*m_object;
	CPropertyStorage			*m_storage;

protected:
	u32							m_start_level_time;
	u32							m_start_game_time;
	u32							m_inertia_time;
	mutable u16	m_weight;
	bool						m_first_time;

#ifdef LOG_ACTION
public:
	const char*						m_action_name;
	bool						m_use_log;
	bool						m_switched;

public:
	virtual void				debug_log			(const EActionStates state_state) const;
	virtual	void				set_use_log			(bool value);
	virtual void				show				(const char* offset = "");
#endif

public:
	IC							CActionBase			(const xr_vector<COperatorCondition> &conditions, const xr_vector<COperatorCondition> &effects, _object_type *object = 0, const char* action_name = "");
	IC							CActionBase			(_object_type *object, const char* action_name = "");
	virtual						~CActionBase		();
	IC		void				init				(_object_type *object, const char* action_name);
	virtual void				setup				(_object_type *object, CPropertyStorage *storage);
	virtual void				initialize			();
	virtual void				execute				();
	virtual void				finalize			();
	virtual u16	weight				(const CSConditionState &condition0, const CSConditionState &condition1) const;
	IC		void				set_inertia_time	(u32 inertia_time);
	IC		u32					start_level_time	() const;
	IC		u32					inertia_time		() const;
	IC		bool				completed			() const;
	IC		void				set_property		(const u32 &condition_id, const bool&value);
	IC		const bool	&property			(const u32 &condition_id) const;
	IC		void 				set_weight			(const u16 &weight);
	IC		bool				first_time			() const;

	virtual	void				save				(NET_Packet &packet) {}
	virtual	void				load				(IReader &packet) {}

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
typedef CActionBase<CScriptGameObject> CScriptActionBase;

#include "action_base_inline.h"
