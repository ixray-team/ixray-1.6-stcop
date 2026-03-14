////////////////////////////////////////////////////////////////////////////
//	Module 		: property_storage_inline.h
//	Created 	: 29.03.2004
//  Modified 	: 29.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Property storage class inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC void CPropertyStorage::set_property(const u32& condition_id, const bool& value)
{
	auto I = std::find(m_storage.begin(), m_storage.end(), condition_id);
	if (m_storage.end() != I)
		(*I).m_value = value;
	else
		m_storage.push_back(CConditionValue(condition_id, value));
}

IC const bool& CPropertyStorage::property(const u32& condition_id) const
{
	auto I = std::find(m_storage.begin(), m_storage.end(), condition_id);
	THROW(m_storage.end() != I);
	return ((*I).m_value);
}

IC void CPropertyStorage::clear()
{
	m_storage.clear();
}
