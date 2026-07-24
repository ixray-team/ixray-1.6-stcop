////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_object_registry.h
//	Created 	: 15.01.2003
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife object registry
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "xrServerEntities/xrServer_Objects_ALife.h"

enum class EAlifeActionCallbackType
{
	CESpawned,
	CEDespawned,
	Num
};

struct SAlifeActionBase
{
	virtual ~SAlifeActionBase() = default;
	virtual void Process() = 0;
};

class CALifeObjectRegistry
{
public:
	typedef xr_map<ALife::_OBJECT_ID, CSE_ALifeDynamicObject*>	OBJECT_REGISTRY;

protected:
	OBJECT_REGISTRY					m_objects;

	// todo: see search manager, random access iterator must be for using clipper, sadly maps are not usable for this due to linear complexity (and so clipper is useless in such containers)
	// todo: needed to be refactored because Level contains objects as vector so it is better to have vector and some unordered_set for searching id if we want to make searching faster than std::find of vector?
	xr_vector<CSE_ALifeDynamicObject*> m_objects_as_vec;
private:
	void save(IWriter &memory_stream, CSE_ALifeDynamicObject *object, u32 &object_count);
	void Serialize(ISaveObject& Object, CSE_ALifeDynamicObject* object, u32& object_count);

	void SerializeElem(ISaveObject& Object, CSE_ALifeDynamicObject* elem);
	u32 m_serializable_object_count = 0;

	xrCriticalSection m_actionsCS;
	xr_vector<xr_map<ALife::_OBJECT_ID, xr_vector<SAlifeActionBase*>>> m_actions;
public:
	void BindAction(EAlifeActionCallbackType Event, ALife::_OBJECT_ID ID, SAlifeActionBase *Action);
	void TriggerActions(EAlifeActionCallbackType Event, ALife::_OBJECT_ID ID);
	
	static	CSE_ALifeDynamicObject	*get_object				(IReader &file_stream);
	static	CSE_ALifeDynamicObject* get_object(ISaveObject& Object);

public:
									CALifeObjectRegistry	(const char* section);
	virtual							~CALifeObjectRegistry	();
	virtual	void					save					(IWriter &memory_stream);
			void					load					(IReader &file_stream);
			virtual void			Serialize(ISaveObject& Object);
	IC		void					add						(CSE_ALifeDynamicObject *object);
	IC		void					remove					(const ALife::_OBJECT_ID &id, bool no_assert = false);
	IC		CSE_ALifeDynamicObject	*object					(const ALife::_OBJECT_ID &id, bool no_assert = false) const;
	IC		const OBJECT_REGISTRY	&objects				() const;
	IC		OBJECT_REGISTRY			&objects				();
	const xr_vector<CSE_ALifeDynamicObject*> objects_vec() const;
};

IC void CALifeObjectRegistry::add(CSE_ALifeDynamicObject* object)
{
	if (objects().find(object->ID) != objects().end()) {
		VERIFY(objects().find(object->ID)->second == object, "The specified object is already presented in the Object Registry!");
		VERIFY(objects().find(object->ID)->second != object, "Object with the specified ID is already presented in the Object Registry!");
	}

	m_objects.insert(std::make_pair(object->ID, object));

	m_objects_as_vec.push_back(object);
}

IC void CALifeObjectRegistry::remove(const ALife::_OBJECT_ID& id, bool no_assert)
{
	OBJECT_REGISTRY::iterator	I = m_objects.find(id);
	if (I == m_objects.end()) {
		VERIFY(no_assert, "The specified object hasn't been found in the Object Registry!");
		return;
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
		VERIFY					(no_assert,"Specified object hasn't been found in the object registry!");
		return					(nullptr);
	}

	return						(I->second);
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