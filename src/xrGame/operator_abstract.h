////////////////////////////////////////////////////////////////////////////
//	Module 		: operator_abstract.h
//	Created 	: 24.02.2004
//  Modified 	: 24.02.2004
//	Author		: Dmitriy Iassenev
//	Description : Operator abstract
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "condition_state.h"

class COperatorAbstract
{
protected:
	typedef CConditionState<CWorldProperty>	inherited;
	typedef inherited							CSConditionState;
	typedef CWorldProperty						COperatorCondition;

protected:
	CSConditionState				m_conditions;
	CSConditionState				m_effects;
	bool							*m_actuality;
	mutable bool					m_weight_actual;
	mutable u16		m_min_weight;

protected:
	void actual(bool value);

public:
	COperatorAbstract	();
	COperatorAbstract	(const CSConditionState &conditions, const CSConditionState &effects);
	virtual							~COperatorAbstract	();
	virtual	void					Load				(const char* section);
	virtual void					setup				(bool *actuality);
	const CSConditionState	&conditions			() const;
	const CSConditionState	&effects			() const;
	void					add_condition		(const COperatorCondition &condition);
	void					add_effect			(const COperatorCondition &effect);
	void					remove_condition	(const typename u32 &condition);
	void					remove_effect		(const typename u32 &effect);
	u16		min_weight			() const;
	
	template <typename T>
	IC bool applicable(const CSConditionState &condition, const CSConditionState &start, const CSConditionState &self_condition, T &problem_solver) const;
	
	virtual bool					applicable_reverse	(const CSConditionState &condition, const CSConditionState &start, const CSConditionState &self_condition) const;
	
	template <typename T>
	IC		const CSConditionState	&apply				(const CSConditionState &condition, const CSConditionState &self_condition, CSConditionState &result, CSConditionState &current, T &problem_solver) const;
	virtual const CSConditionState	&apply				(const CSConditionState &condition, const CSConditionState &self_condition, CSConditionState &result) const;
	
	virtual bool					apply_reverse		(const CSConditionState &condition, const CSConditionState &start, CSConditionState &result, const CSConditionState &self_condition) const;
	virtual u16		weight				(const CSConditionState &condition0, const CSConditionState &condition1) const;
};



template <typename T>
IC	bool COperatorAbstract::applicable(const CSConditionState& current, const CSConditionState& start, const CSConditionState& conditions, T& problem_solver) const
{
	typename xr_vector<COperatorCondition>::const_iterator	I = current.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	E = current.conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	i = conditions.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	e = conditions.conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	II = start.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	EE = start.conditions().end();
	for (; (I != E) && (i != e); ) {
		if ((*I).condition() < (*i).condition())
			++I;
		else
			if ((*I).condition() > (*i).condition()) {
				for (; (II != EE) && ((*II).condition() < (*i).condition()); )
					++II;
				if ((II == EE) || ((*II).condition() > (*i).condition()))
					problem_solver.evaluate_condition(II, EE, (*i).condition());
				if ((*II).value() != (*i).value())
					return	(false);
				++II;
				++i;
			}
			else {
				if ((*I).value() != (*i).value())
					return	(false);
				++I;
				++i;
			}
	}

	if (I == E) {
		I = II;
		E = EE;
	}
	else
		return	(true);

	for (; i != e; ) {
		if ((I == E) || ((*I).condition() > (*i).condition()))
			problem_solver.evaluate_condition(I, E, (*i).condition());

		if ((*I).condition() < (*i).condition())
			++I;
		else {
			VERIFY((*I).condition() == (*i).condition());
			if ((*I).value() != (*i).value())
				return	(false);
			++I;
			++i;
		}
	}

	return		(true);
}

template <typename T>
IC	const typename COperatorAbstract::CSConditionState& COperatorAbstract::apply(const CSConditionState& current, const CSConditionState& effects, CSConditionState& result, CSConditionState& start, T& problem_solver) const
{
	result.clear();
	auto I = current.conditions().begin();
	auto E = current.conditions().end();
	auto i = effects.conditions().begin();
	auto e = effects.conditions().end();
	auto II = start.conditions().begin();
	auto EE = start.conditions().end();
	for (; (I != E) && (i != e); ) {
		if ((*I).condition() < (*i).condition()) {
			result.add_condition_back(*I);
			++I;
		}
		else
			if ((*I).condition() > (*i).condition()) {
				for (; (II != EE) && ((*II).condition() < (*i).condition()); )
					++II;
				if ((II == EE) || ((*II).condition() > (*i).condition()))
					problem_solver.evaluate_condition(II, EE, (*i).condition());
				if ((*II).value() != (*i).value())
					result.add_condition_back(*i);
				++II;
				++i;
			}
			else {
				if ((*I).value() == (*i).value())
					result.add_condition_back(*i);
				++I;
				++i;
			}
	}
	if (I == E) {
		I = II;
		E = EE;
	}
	else {
		for (; I != E; ++I)
			result.add_condition_back(*I);
		return	(result);
	}

	for (; i != e; ) {
		if ((I == E) || ((*I).condition() > (*i).condition()))
			problem_solver.evaluate_condition(I, E, (*i).condition());

		if ((*I).condition() < (*i).condition())
			++I;
		else {
			VERIFY((*I).condition() == (*i).condition());
			if ((*I).value() != (*i).value())
				result.add_condition_back(*i);
			++I;
			++i;
		}
	}

	return		(result);
}
