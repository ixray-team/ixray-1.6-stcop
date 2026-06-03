////////////////////////////////////////////////////////////////////////////
//	Module 		: operator_abstract_inline.h
//	Created 	: 24.02.2004
//  Modified 	: 24.02.2004
//	Author		: Dmitriy Iassenev
//	Description : Operator abstract inline functions
////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "operator_abstract.h"

COperatorAbstract::COperatorAbstract	()
{
	m_actuality			= 0;
	m_weight_actual		= true;
	m_min_weight		= 0;
}

COperatorAbstract::COperatorAbstract	(const CSConditionState &conditions, const CSConditionState &effects)
{
	m_conditions		= conditions;
	m_effects			= effects;
	m_actuality			= 0;
	m_weight_actual		= false;
	m_min_weight		= 0;
}

COperatorAbstract::~COperatorAbstract	()
{
}

void COperatorAbstract::Load						(const char* section)
{
}

void COperatorAbstract::setup						(bool *actuality)
{
	VERIFY				(actuality);
	m_actuality			= actuality;
	*m_actuality		= false;
}

void COperatorAbstract::actual					(bool value)
{
	if (!m_actuality)
		return;

	*m_actuality		= *m_actuality && value;
}

const typename COperatorAbstract::CSConditionState	&COperatorAbstract::conditions	() const
{
	return				(m_conditions);
}

const typename COperatorAbstract::CSConditionState	&COperatorAbstract::effects		() const
{
	return				(m_effects);
}

void COperatorAbstract::add_condition	(const COperatorCondition &condition)
{
	actual						(false);
	m_conditions.add_condition	(condition);
}

void COperatorAbstract::remove_condition(const u32 &condition)
{
	actual						(false);
	m_conditions.remove_condition(condition);
}

void COperatorAbstract::add_effect		(const COperatorCondition &effect)
{
	actual						(false);
	m_effects.add_condition		(effect);
}

void COperatorAbstract::remove_effect	(const u32&effect)
{
	actual						(false);
	m_effects.remove_condition	(effect);
}

bool COperatorAbstract::applicable_reverse	(const CSConditionState &condition, const CSConditionState &start, const CSConditionState &self_condition) const
{
	typename xr_vector<COperatorCondition>::const_iterator	i = self_condition.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	e = self_condition.conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	I = condition.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	E = condition.conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	J = start.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	EE = start.conditions().end();
	for ( ; (I != E) && (i != e); )
		if (I->condition() < i->condition())
			++I;
		else
			if (I->condition() > i->condition()) {
				while ((J != EE) && (J->condition() < i->condition()))
					++J;
				if ((J != EE) && (J->condition() == i->condition())) {
					if (J->value() != i->value())
						return	(false);
					++J;
				}
				++i;
			}
			else {
				if (I->value() != i->value())
					return	(false);
				++I;
				++i;
			}

	if (i == e)
		return				(true);

	for ( ; (J != EE) && (i != e); )
		if (J->condition() < i->condition())
			++J;
		else
			if (J->condition() > i->condition())
				++i;
			else {
				if (J->value() != i->value())
					return	(false);
				++J;
				++i;
			}
	return					(true);
}

bool COperatorAbstract::apply_reverse	(const CSConditionState &condition, const CSConditionState &start, CSConditionState &result_, const CSConditionState &self_condition) const
{
	result_.clear			();
	bool					changed = false;
	typename xr_vector<COperatorCondition>::const_iterator	i = self_condition.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	e = self_condition.conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	I = condition.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	E = condition.conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	J = start.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	EE = start.conditions().end();
	for ( ; (I != E) && (i != e); )
		if (I->condition() < i->condition()) {
			while ((J != EE) && (J->condition() < I->condition()))
				++J;
			if ((J != EE) && (J->condition() == I->condition())) {
				VERIFY		(J->value() == I->value());
				changed		= true;
				++J;
			}
			else
				result_.add_condition_back(*I);
			++I;
		}
		else
			if (I->condition() > i->condition()) {
				result_.add_condition_back(*i);
				++i;
			}
			else {
				if (I->value() != i->value())
					changed	= true;
				result_.add_condition_back(*i);
				++I;
				++i;
			}

	if (I == E) {
		if (!changed)
			return			(false);
		for ( ; (i != e); ++i)
			result_.add_condition_back(*i);
		return				(true);
	}

	for ( ; (J != EE) && (I != E); )
		if (J->condition() < I->condition())
			++J;
		else
			if (J->condition() > I->condition()) {
				result_.add_condition_back(*I);
				++I;
			}
			else {
				VERIFY		(J->value() == I->value());
				changed		= true;
				++J;
				++I;
			}

	if (!changed)
		return				(false);

	if ((J == EE) && (I != E))
		for ( ; (I != E); ++I)
			result_.add_condition_back(*I);

	return					(true);
}

const typename COperatorAbstract::CSConditionState &COperatorAbstract::apply	(const CSConditionState &condition, const CSConditionState &self_condition, CSConditionState &result_) const
{
	result_.clear			();
	typename xr_vector<COperatorCondition>::const_iterator	i = self_condition.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	e = self_condition.conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	I = condition.conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	E = condition.conditions().end();
	for ( ; (I != E) && (i != e); )
		if (I->condition() < i->condition()) {
			result_.add_condition_back(*I);
			++I;
		}
		else
			if (I->condition() > i->condition()) {
				result_.add_condition_back(*i);
				++i;
			}
			else {
				VERIFY			(I->condition() == i->condition());
				result_.add_condition_back(*i);
				++I;
				++i;
			}

	if (i == e) {
		for ( ; I != E; ++I)
			result_.add_condition_back(*I);
	}
	else {
		for ( ; i != e; ++i)
			result_.add_condition_back(*i);
	}

	return					(result_);
}

IC u16 COperatorAbstract::weight	(const CSConditionState &condition0, const CSConditionState &condition1) const
{
	return					(min_weight());
}

IC u16 COperatorAbstract::min_weight	() const
{
	if (m_weight_actual)
		return				(m_min_weight);

	m_min_weight			= 0;
	typename xr_vector<COperatorCondition>::const_iterator	I = conditions().conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	E = conditions().conditions().end();
	typename xr_vector<COperatorCondition>::const_iterator	i = effects().conditions().begin();
	typename xr_vector<COperatorCondition>::const_iterator	e = effects().conditions().end();
	for ( ; (i != e) && (I != E); ) {
		if (I->condition() < i->condition())
			++I;
		else
			if (I->condition() > i->condition()) {
				++m_min_weight;
				++i;
			}
			else {
				if (I->value() != i->value())
					++m_min_weight;
				++I;
				++i;
			}
	}
	if (i != e)
		m_min_weight		= m_min_weight + u16(e - i);

	m_weight_actual			= true;
	return					(m_min_weight);
}