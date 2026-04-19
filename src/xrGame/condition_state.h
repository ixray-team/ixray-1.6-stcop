#pragma once
#include "operator_condition.h"

class CWorldState
{
protected:
	xr_vector<CWorldProperty> m_conditions;
	u32 m_hash = 0;

public:
	constexpr CWorldState() = default;
	virtual ~CWorldState() = default;

	IC const xr_vector<CWorldProperty>& conditions() const;
	IC u8 weight(const CWorldState& condition) const;
	IC void add_condition(const CWorldProperty& condition);
	IC void remove_condition(const u32& condition);
	IC void add_condition(typename xr_vector<CWorldProperty>::const_iterator& J, const CWorldProperty& condition);
	IC void add_condition_back(const CWorldProperty& condition);
	IC bool includes(const CWorldState& condition) const;
	IC void clear();
	IC bool operator<(const CWorldState& condition) const;
	IC CWorldState& operator-=(const CWorldState& condition);
	IC bool operator==(const CWorldState& condition) const;
	IC u32 hash_value() const;
	IC const CWorldProperty* property(const u32& condition) const;
};

#include "condition_state_inline.h"