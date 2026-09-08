////////////////////////////////////////////////////////////////////////////
//	Module 		: danger_manager_inline.h
//	Created 	: 11.02.2005
//  Modified 	: 11.02.2005
//	Author		: Dmitriy Iassenev
//	Description : Danger manager inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC	CDangerManager::CDangerManager							(CCreature *object)
{
	VERIFY			(object);
	m_object		= object;
}

IC	void CDangerManager::reset								()
{
	m_objects.clear	();
	m_selected_index = -1;
}

IC	const CDangerObject *CDangerManager::selected			() const
{
	if (m_selected_index >= 0 && (size_t)m_selected_index < m_objects.size())
		return			(&m_objects[m_selected_index]);
	return			(nullptr);
}

IC	const CDangerManager::OBJECTS &CDangerManager::objects	() const
{
	return			(m_objects);
}

IC	u32	CDangerManager::time_line							() const
{
	return			(m_time_line);
}

IC	void CDangerManager::time_line							(u32 value)
{
	m_time_line		= value;
}
