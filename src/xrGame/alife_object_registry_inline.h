#include "alife_object_registry.h"
////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_object_registry_רעהרעף.h
//	Created 	: 15.01.2003
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife object registry inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once



IC void CALifeObjectRegistry::add(CSE_ALifeDynamicObject* object)
{
	if (objects().find(object->ID) != objects().end()) {
		THROW2(objects().find(object->ID)->second == object, "The specified object is already presented in the Object Registry!");
		THROW2(objects().find(object->ID)->second != object, "Object with the specified ID is already presented in the Object Registry!");
	}

	m_objects.insert(std::make_pair(object->ID, object));

	m_objects_as_vec.push_back(object);

	if (object->name_replace() && object->name_replace()[0])
	{
		auto h = MurmurOAAT_32(object->name_replace());
		auto it = m_objectsNameCache.find(h);
		if (it != m_objectsNameCache.end())
		{
			it->second.addId(object->ID);
		}
		else
		{
			m_objectsNameCache.insert(std::make_pair(h, ALIFE_OBJECT_ID_S(object->ID)));
		}
	}
}

IC void CALifeObjectRegistry::remove(const ALife::_OBJECT_ID& id, bool no_assert)
{
	OBJECT_REGISTRY::iterator	I = m_objects.find(id);
	if (I == m_objects.end()) {
		THROW2(no_assert, "The specified object hasn't been found in the Object Registry!");
		return;
	}

	if (I->second->name_replace() && I->second->name_replace()[0])
	{
		auto h = MurmurOAAT_32(I->second->name_replace());
		auto it = m_objectsNameCache.find(h);
		if (it != m_objectsNameCache.end())
		{
			if (!it->second.removeId(id))
			{
				m_objectsNameCache.erase(it);
			}
		}
	}

	m_objects.erase(I);
	auto to_delete = std::find_if(m_objects_as_vec.begin(), m_objects_as_vec.end(), [id](const CSE_ALifeDynamicObject* pObject) {
		return pObject->ID == id;
		});

	m_objects_as_vec.erase(to_delete);
}

IC	CSE_ALifeDynamicObject *CALifeObjectRegistry::object	(const ALife::_OBJECT_ID &id, bool no_assert) const
{
	PROF_EVENT("ALife/objects::object")
	OBJECT_REGISTRY::const_iterator	I = objects().find(id);

	if (objects().end() == I)
	{
#ifdef DEBUG
		if (!no_assert)
			Msg					("There is no object with id %d!",id);
#endif
		THROW2					(no_assert,"Specified object hasn't been found in the object registry!");
		return					(nullptr);
	}

	return						(I->second);
}

IC CALifeObjectRegistry::NAME_CACHE& CALifeObjectRegistry::objectsNameCache() const
{
	return m_objectsNameCache;
}

IC u32 CALifeObjectRegistry::MurmurOAAT_32(const char* str)
{
	u32 h = 0x12345678;
	for (; *str; ++str)
	{
		h ^= (u8)*str;
		h *= 0x5bd1e995;
		h ^= h >> 15;
	}
	return h;
}

IC CSE_ALifeDynamicObject* CALifeObjectRegistry::object(const char* name) const
{
	if (!name || !name[0])
		return nullptr;

	auto h = MurmurOAAT_32(name);
	auto itc = m_objectsNameCache.find(h);
	if (itc != m_objectsNameCache.end())
	{
		for (u16 i = 0; i < ALIFE_OBJECT_ID_S::SIZE; ++i)
		{
			if (itc->second.ids[i] != 0xFFFF)
			{
				auto ito = m_objects.find(itc->second.ids[i]);
				if (ito != m_objects.end() && xr_strcmp(ito->second->name_replace(), name) == 0)
				{
					return ito->second;
				}
			}
		}
	}
	return nullptr;
}

IC	const CALifeObjectRegistry::OBJECT_REGISTRY &CALifeObjectRegistry::objects	() const
{
	return						(m_objects);
}

IC	CALifeObjectRegistry::OBJECT_REGISTRY &CALifeObjectRegistry::objects		()
{
	return						(m_objects);
}

inline const xr_vector<CSE_ALifeDynamicObject*> CALifeObjectRegistry::objects_vec() const
{
	return m_objects_as_vec;
}
