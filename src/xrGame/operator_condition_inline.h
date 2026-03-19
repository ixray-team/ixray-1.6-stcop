////////////////////////////////////////////////////////////////////////////
//	Module 		: operator_condition_inline.h
//	Created 	: 24.02.2004
//  Modified 	: 24.02.2004
//	Author		: Dmitriy Iassenev
//	Description : Operator condition inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "random32.h"

IC CWorldProperty::CWorldProperty(const u32 condition, const bool value) :
	m_condition(condition),
	m_value(value)
{
	u32 seed = ::Random32.seed();
	::Random32.seed(u32(condition) + 1);
	m_hash = ::Random32.random(0xffffffff);
	::Random32.seed(m_hash + u32(value));
	m_hash ^= ::Random32.random(0xffffffff);
	::Random32.seed(seed);
}

IC const u32& CWorldProperty::condition() const
{
	return (m_condition);
}

IC const bool& CWorldProperty::value() const
{
	return (m_value);
}

IC const u32& CWorldProperty::hash_value() const
{
	return (m_hash);
}

IC bool CWorldProperty::operator<(const CWorldProperty& _condition) const
{
	if (condition() < _condition.condition())
		return			(true);
	if (condition() > _condition.condition())
		return			(false);
	if (value() < _condition.value())
		return			(true);
	return				(false);
}

IC bool CWorldProperty::operator==(const CWorldProperty& _condition) const
{
	if ((condition() == _condition.condition()) && (value() == _condition.value()))
		return			(true);
	return				(false);
}