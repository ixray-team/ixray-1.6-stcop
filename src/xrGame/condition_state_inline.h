#pragma once

IC const xr_vector<CWorldProperty>& CWorldState::conditions() const
{
	return m_conditions;
}

IC void CWorldState::add_condition_back(const CWorldProperty& condition)
{
	THROW(m_conditions.empty() || (m_conditions.back().condition() < condition.condition()));
	if (m_conditions.empty() || (m_conditions.back().condition() < condition.condition()))
	{
		m_conditions.push_back(condition);
		m_hash ^= condition.hash_value();
	}
}

IC void CWorldState::add_condition(const CWorldProperty& condition)
{
	xr_vector<CWorldProperty>::iterator	I = std::lower_bound(m_conditions.begin(), m_conditions.end(), condition);
	THROW((I == m_conditions.end()) || ((*I).condition() != condition.condition()));
	if ((I == m_conditions.end()) || ((*I).condition() != condition.condition()))
	{
		m_conditions.insert(I, condition);
		m_hash ^= condition.hash_value();
	}
}

IC void CWorldState::remove_condition(const u32& condition)
{
	xr_vector<CWorldProperty>::iterator I = std::lower_bound(m_conditions.begin(), m_conditions.end(), CWorldProperty(condition, false));
	THROW((I != m_conditions.end()) && ((*I).condition() == condition));

	if ((I != m_conditions.end()) && ((*I).condition() == condition)) 
	{
		m_hash ^= (*I).hash_value();
		m_conditions.erase(I);
	}
}

IC void CWorldState::add_condition(xr_vector<CWorldProperty>::const_iterator& J, const CWorldProperty& condition)
{
	m_conditions.insert(m_conditions.begin() + (J - m_conditions.begin()), condition);
	m_hash ^= condition.hash_value();
}

IC void CWorldState::clear()
{
	m_conditions.clear();
	m_hash = 0;
}

IC u8 CWorldState::weight(const CWorldState& condition) const
{
	u8 result = 0;
	xr_vector<CWorldProperty>::const_iterator I = conditions().begin();
	xr_vector<CWorldProperty>::const_iterator E = conditions().end();
	xr_vector<CWorldProperty>::const_iterator i = condition.conditions().begin();
	xr_vector<CWorldProperty>::const_iterator e = condition.conditions().end();

	for (; (I != E) && (i != e); )
	{
		if ((*I).condition() < (*i).condition())
		{
			++I;
		}
		else if ((*I).condition() > (*i).condition())
		{
			++i;
		}
		else
		{
			if ((*I).value() != (*i).value())
			{
				++result;
			}

			++I;
			++i;
		}
	}

	return result;
}

IC bool CWorldState::operator<(const CWorldState& condition) const
{
	xr_vector<CWorldProperty>::const_iterator I = conditions().begin();
	xr_vector<CWorldProperty>::const_iterator E = conditions().end();
	xr_vector<CWorldProperty>::const_iterator i = condition.conditions().begin();
	xr_vector<CWorldProperty>::const_iterator e = condition.conditions().end();
	for (; (I != E) && (i != e); ++I, ++i)
	{
		if (*I < *i)
		{
			return true;
		}
		else if (*i < *I)
		{
			return false;
		}
	}

	if (I == E)
	{
		if (i == e)
		{
			return false;
		}

		return true;
	}

	return false;
}

IC bool CWorldState::operator==(const CWorldState& condition) const
{
	if (hash_value() != condition.hash_value())
	{
		return (false);
	}

	xr_vector<CWorldProperty>::const_iterator I = conditions().begin();
	xr_vector<CWorldProperty>::const_iterator E = conditions().end();
	xr_vector<CWorldProperty>::const_iterator i = condition.conditions().begin();
	xr_vector<CWorldProperty>::const_iterator e = condition.conditions().end();
	for (; (I != E) && (i != e); ++I, ++i)
	{
		if (!(*I == *i))
		{
			return (false);
		}
	}

	if ((I == E) && (i == e))
	{
		return true;
	}
	return false;
}

IC CWorldState& CWorldState::operator-=(const CWorldState& condition)
{
	m_hash = 0;
	xr_vector<CWorldProperty> temp;
	xr_vector<CWorldProperty>::const_iterator I = conditions().begin();
	xr_vector<CWorldProperty>::const_iterator E = conditions().end();
	xr_vector<CWorldProperty>::const_iterator i = condition.conditions().begin();
	xr_vector<CWorldProperty>::const_iterator e = condition.conditions().end();

	for (; (I != E) && (i != e); )
	{
		if ((*I).condition() < (*i).condition())
		{
			++I;
		}
		else if ((*I).condition() > (*i).condition())
		{
			++i;
		}
		else
		{
			if ((*I).value() != (*i).value())
			{
				temp.push_back(*I);
				m_hash ^= (*I).hash_value();
			}
			++I;
			++i;
		}
	}

	m_conditions = temp;
	return *this;
}

IC bool CWorldState::includes(const CWorldState& condition) const
{
	xr_vector<CWorldProperty>::const_iterator I = conditions().begin();
	xr_vector<CWorldProperty>::const_iterator E = conditions().end();
	xr_vector<CWorldProperty>::const_iterator i = condition.conditions().begin();
	xr_vector<CWorldProperty>::const_iterator e = condition.conditions().end();
	for (; (I != E) && (i != e); )
	{
		if ((*I).condition() < (*i).condition())
		{
			++I;
		}
		else if ((*I).condition() > (*i).condition())
		{
			return false;
		}
		else if ((*I).value() != (*i).value())
		{
			return false;
		}
		else
		{
			++I;
			++i;
		}
	}

	return (i == e);
}

IC u32 CWorldState::hash_value() const
{
	return m_hash;
}

IC const CWorldProperty* CWorldState::property(const u32& condition) const
{
	auto I = std::lower_bound(conditions().begin(), conditions().end(), CWorldProperty(condition, false));
	if (I == m_conditions.end())
	{
		return (0);
	}

	return (&*I);
}