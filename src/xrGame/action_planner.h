////////////////////////////////////////////////////////////////////////////
//	Module 		: action_planner.h
//	Created 	: 28.01.2004
//  Modified 	: 10.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Action planner
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "problem_solver.h"
#include "action_base.h"
#include "property_evaluator.h"
#include "property_storage.h"
#include "../xrScripts/script_export_space.h"
#include "ai_debug.h"

class CScriptGameObject;

template <
	typename _object_type,
	bool	 _reverse_search = false,
	typename __world_operator = CActionBase<_object_type>,
	typename _condition_evaluator = CPropertyEvaluator<_object_type>,
	typename _world_operator_ptr = __world_operator*,
	typename _condition_evaluator_ptr = _condition_evaluator*
>
class CActionPlanner : 
	public CProblemSolver<
		__world_operator,
		_condition_evaluator,
		_reverse_search,
		_world_operator_ptr,
		_condition_evaluator_ptr
	> 
{
public:
	typedef CProblemSolver<
		__world_operator,
		_condition_evaluator,
		_reverse_search,
		_world_operator_ptr,
		_condition_evaluator_ptr
	> CProblemSolver;

	using inherited = CProblemSolver;
	using COperator = typename inherited::COperator;
	using CConditionEvaluator = typename inherited::CConditionEvaluator;
	using _operator_ptr = typename inherited::_operator_ptr;
	
	using _world_operator = __world_operator ; 

protected:
	bool						m_initialized;
	u32				m_current_action_id;

	void SerializeEval(ISaveObject& Object, CProblemSolver::EVALUATORS::value_type& elem);
	void SerializeOper(ISaveObject& Object, CProblemSolver::SOperator& elem);
	void SerializeStor(ISaveObject& Object, CSolverConditionValue& elem);

#ifdef LOG_ACTION
public:
	bool						m_use_log;
	string64					m_temp_string;

public:
	virtual	void				set_use_log				(bool value);
#endif

public:
	_object_type				*m_object;
	CPropertyStorage			m_storage;
	bool						m_loaded;
	bool						m_solving;

#ifdef LOG_ACTION
public:
	virtual const char*				action2string			(const u32&action_id);
	virtual const char*				property2string			(const u32 &action_id);
	virtual const char*				object_name				() const;
	virtual void				show					(const char* offset = "");
	IC		void				show_current_world_state();
	IC		void				show_target_world_state	();
#endif

public:
								CActionPlanner			();
	virtual						~CActionPlanner			();
	virtual	void				setup					(_object_type *object);
	virtual	void				update					();
	virtual void				finalize				();
	IC		COperator			&action					(const u32&action_id);
	IC		CConditionEvaluator	&evaluator				(const u32 &evaluator_id);
	IC		u32		current_action_id		() const;
	IC		COperator			&current_action			();
	IC		bool				initialized				() const;
	IC		void				add_condition			(_world_operator *action, u32 condition_id, bool condition_value);
	IC		void				add_effect				(_world_operator *action, u32 condition_id, bool condition_value);
	IC		virtual void		add_operator			(const u32& operator_id,	_operator_ptr _operator);
	IC		virtual void		remove_operator			(const u32& operator_id);
	IC		virtual void		add_evaluator			(const u32 &condition_id, _condition_evaluator_ptr evaluator);
	IC		virtual void		remove_evaluator		(const u32 &condition_id);
	IC		_object_type		&object					() const;
	virtual	void				save					(NET_Packet &packet);
	virtual	void				load					(IReader &packet);
	virtual void Serialize(ISaveObject& Object);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
typedef CActionPlanner<CScriptGameObject> CScriptActionPlanner;

#include "action_planner_inline.h"