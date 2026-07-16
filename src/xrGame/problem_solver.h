////////////////////////////////////////////////////////////////////////////
//	Module 		: problem_solver.h
//	Created 	: 24.02.2004
//  Modified 	: 10.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Problem solver
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../xrEngine/AI/graph_engine_space.h"
#include "../xrCore/Containers/associative_vector.h"
#include "condition_state.h"

template <
	typename _operator,
	typename _condition_evaluator,
	bool	 _reverse_search = false,
	typename __operator_ptr = _operator*,
	typename __condition_evaluator_ptr = _condition_evaluator*
>
class CProblemSolver
{
public:
	enum {
		reverse_search = _reverse_search,
	};

private:
	typedef CProblemSolver<
		_operator,
		_condition_evaluator,
		_reverse_search,
		__operator_ptr,
		__condition_evaluator_ptr
	> self_type;

public:
	using _operator_ptr = __operator_ptr;
	using _condition_evaluator_ptr = __condition_evaluator_ptr;

	typedef _operator										COperator;
	typedef CWorldState										CState;
	typedef _condition_evaluator							CConditionEvaluator;
	typedef CState											_index_type;

	struct SOperator
	{
		u32	m_operator_id;
		_operator_ptr m_operator;

		IC SOperator(const u32& operator_id, _operator_ptr OperatorValue) :
			m_operator_id(operator_id),
			m_operator(OperatorValue)
		{
		}

		bool operator<(const u32& operator_id) const
		{
			return (m_operator_id < operator_id);
		}

		_operator_ptr get_operator() const
		{
			return (m_operator);
		}
	};
	typedef xr_vector<SOperator>											OPERATOR_VECTOR;
	typedef typename OPERATOR_VECTOR::const_iterator						const_iterator;
	typedef associative_vector<u32 ,_condition_evaluator_ptr> EVALUATORS;

protected:
	OPERATOR_VECTOR				m_operators;
	EVALUATORS					m_evaluators;
	xr_vector<u32>				m_solution;
	CState						m_target_state;
	mutable CState				m_current_state;
	mutable CState				m_temp;
	mutable bool				m_applied;
	bool						m_actuality;
	bool						m_solution_changed;
	bool						m_failed;

private:
	template <bool>
	IC		bool						is_goal_reached_impl	(const _index_type	&vertex_index) const {return is_goal_reached_impl(vertex_index);}
	template <>
	IC		bool						is_goal_reached_impl<true>	(const _index_type	&vertex_index) const {return is_goal_reached_impl(vertex_index,true);}

	IC		bool						is_goal_reached_impl	(const _index_type	&vertex_index) const;
	IC		bool						is_goal_reached_impl	(const _index_type	&vertex_index, bool) const;
	
	IC		u16			estimate_edge_weight_impl(const _index_type	&vertex_index) const;
	IC		u16			estimate_edge_weight_impl(const _index_type	&vertex_index, bool) const;

private:
	template <bool>
	struct helper {
		static IC	u16	estimate_edge_weight_impl( self_type const& self_, const _index_type	&vertex_index) {return self_.estimate_edge_weight_impl(vertex_index);}
	}; // struct helper

	template <>
	struct helper<true> {
		static IC	u16	estimate_edge_weight_impl( self_type const& self, const _index_type	&vertex_index) {return self.estimate_edge_weight_impl(vertex_index,true);}
	}; // struct helper

protected:
#ifdef DEBUG
	IC		void						validate_properties		(const CState &conditions) const;
#endif


public:
	// common interface
	IC									CProblemSolver			();
	virtual								~CProblemSolver			();
			void						init					();
	virtual void						setup					();
	IC		bool						actual					() const;

	// graph interface
	IC		u16			get_edge_weight			(const _index_type	&vertex_index0,	const _index_type &vertex_index1,	const const_iterator	&i) const;
	IC		bool						is_accessible			(const _index_type	&vertex_index) const;
	IC		const _index_type			&value					(const _index_type	&vertex_index,	const_iterator		&i,				bool					reverse_search) const;
	IC		void						begin					(const _index_type	&vertex_index,	const_iterator		&b,				const_iterator			&e) const;
	IC		bool						is_goal_reached			(const _index_type	&vertex_index) const;
	IC		u16			estimate_edge_weight	(const _index_type	&vertex_index) const;

	// operator interface
	IC		virtual void				add_operator			(const u32& operator_id,	_operator_ptr OperatorValue);
	IC		virtual void				remove_operator			(const u32& operator_id);
	IC		_operator_ptr				get_operator			(const u32& operator_id);
	IC		const OPERATOR_VECTOR		&operators				() const;

	// state interface
	IC		void						set_target_state		(const CState		&state);
	IC		const CState				&current_state			() const;
	IC		const CState				&target_state			() const;

	// evaluator interface
	IC		virtual void				add_evaluator			(const u32 &condition_id, _condition_evaluator_ptr evaluator);
	IC		virtual void				remove_evaluator		(const u32 &condition_id);
	IC		_condition_evaluator_ptr	evaluator				(const u32 &condition_id) const;
	IC		const EVALUATORS			&evaluators				() const;
	IC		void						evaluate_condition		(typename xr_vector<CWorldProperty>::const_iterator &I, typename xr_vector<CWorldProperty>::const_iterator &E, const u32 &condition_id) const;

	// solver interface
	IC		bool						Search					(const CState start_vertex_id, const CState&dest_vertex_id,xr_vector<u32>& OutPath, u16 MaxRange, u32 MaxIterationCount,u32 MaxVisitedNodeCount) const;
	IC		void						solve					();
	IC		const xr_vector<u32>&		solution				() const;
	virtual	void						clear					();
};

#ifndef AI_COMPILER
#	include "ai_space.h"
#endif

#include "graph_engine.h"
#include "object_broker.h"

#include "problem_solver_inline.h"