////////////////////////////////////////////////////////////////////////////
//	Module 		: operator_condition.h
//	Created 	: 24.02.2004
//  Modified 	: 24.02.2004
//	Author		: Dmitriy Iassenev
//	Description : Operator condition
////////////////////////////////////////////////////////////////////////////

#pragma once

class CWorldProperty
{
protected:
	u32 m_condition;
	u32 m_hash;
	bool m_value;

public:
	IC CWorldProperty(const u32 condition, const bool value);
	IC const u32& condition() const;
	IC const bool& value() const;
	IC const u32& hash_value() const;

	IC bool operator<(const CWorldProperty& condition) const;
	IC bool operator==(const CWorldProperty& condition) const;
};

#include "operator_condition_inline.h"