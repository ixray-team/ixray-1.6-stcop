////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_object_registry.cpp
//	Created 	: 15.01.2003
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife object registry
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "alife_object_registry.h"
#include "ai_debug.h"

CALifeObjectRegistry::CALifeObjectRegistry	(const char* section)
{
}

CALifeObjectRegistry::~CALifeObjectRegistry	()
{
	OBJECT_REGISTRY::iterator const B	= m_objects.begin();
	OBJECT_REGISTRY::iterator I			= B;
	OBJECT_REGISTRY::iterator const E	= m_objects.end();
	for ( ; I != E; ++I)
		(*I).second->on_unregister	();

	for (I = B; I != E; ++I)
		xr_delete					((*I).second);

	m_objects_as_vec.clear();
}

void CALifeObjectRegistry::save				(IWriter &memory_stream, CSE_ALifeDynamicObject *object, u32 &object_count)
{
	++object_count;

	NET_Packet					tNetPacket;
	// Spawn
	object->Spawn_Write			(tNetPacket,true);
	auto& data = tNetPacket.B.data;
	I_ASSERT_M(data.size() <= u16(-1), "(Spawn_Write) Object [%s] contains more data than save data limit, current size [%d], max [%d]", data.size(), u16(-1));
	memory_stream.w_u16			(u16(data.size()));
	memory_stream.w				(data.data(),data.size());

	// Update
	tNetPacket.w_begin			(M_UPDATE);
	object->UPDATE_Write		(tNetPacket);
	I_ASSERT_M(data.size() <= u16(-1), "(UPDATE_Write) Object [%s] contains more data than save data limit, current size [%d], max [%d]", data.size(), u16(-1));
	memory_stream.w_u16			(u16(data.size()));
	memory_stream.w				(data.data(),data.size());

	for (auto ID : object->children) {
		CSE_ALifeDynamicObject* child = this->object(ID,true);
		if (!child)
		{
			continue;
		}

		if (!child->can_save())
		{
			continue;
		}

		save(memory_stream,child,object_count);
	}
}

void CALifeObjectRegistry::Serialize(ISaveObject& Object, CSE_ALifeDynamicObject* object, u32& object_count)
{
	VERIFY(Object.IsSave());
	if (!Object.IsSave()) {
		return;
	}
	auto ChunkDepth = Object.GetChunkStackDepth();
	BEGIN_CHUNK(Object,"CALifeObjectRegistry::single_object")
	{
		shared_str temp = object->name();
		Object << temp;

		++object_count;

		object->Spawn_Serialize(Object, true);
		object->UPDATE_Serialize(Object);
		R_ASSERT4(ChunkDepth + 1 == Object.GetChunkStackDepth(), "Saving object result invalid chunk opening and closing tags!", "UPDATE_Serialize", object->name());

	}
	R_ASSERT3(ChunkDepth == Object.GetChunkStackDepth(), "Saving object result invalid chunk opening and closing tags!", object->name());

	ALife::OBJECT_VECTOR::const_iterator	I = object->children.begin();
	ALife::OBJECT_VECTOR::const_iterator	E = object->children.end();
	for (; I != E; ++I) {
		CSE_ALifeDynamicObject* child = this->object(*I, true);
		if (!child)
			continue;

		if (!child->can_save())
			continue;

		Serialize(Object, child, object_count);
	}
}

void CALifeObjectRegistry::save				(IWriter &memory_stream)
{
	Msg							("* Saving objects...");
	memory_stream.open_chunk	(OBJECT_CHUNK_DATA);

	u32							position = memory_stream.tell();
	memory_stream.w_u32			(u32(-1));

	u32							object_count = 0;
	for (auto& obj : m_objects) {
		if (!obj.second->can_save())
		{
			continue;
		}

		if (obj.second->redundant())
		{
			continue;
		}

		if (obj.second->ID_Parent != ALife::INVALID_OBJECT_ID)
		{
			continue;
		}

		save(memory_stream,obj.second, object_count);
	}
	
	u32							last_position = memory_stream.tell();
	memory_stream.seek			(position);
	memory_stream.w_u32			(object_count);
	memory_stream.seek			(last_position);

	memory_stream.close_chunk	();
	
	Msg							("* %d objects are successfully saved",object_count);
}

void CALifeObjectRegistry::BindAction(EAlifeActionCallbackType Event, ALife::_OBJECT_ID ID, SAlifeActionBase* Action)
{
	xrCriticalSectionGuard g(m_actionsCS);
	if (m_actions.empty())
	{
		m_actions.resize((size_t)EAlifeActionCallbackType::Num);
	}
	auto& Type = m_actions[(size_t)Event];
	if (!Type.contains(ID))
	{
		Type[ID] = {};
	}
	Type[ID].emplace_back(Action);
}

void CALifeObjectRegistry::TriggerActions(EAlifeActionCallbackType Event, ALife::_OBJECT_ID ID)
{
	xrCriticalSectionGuard g(m_actionsCS);
	if (m_actions.empty())
	{
		return;
	}
	auto& Type = m_actions[(size_t)Event];
	if (auto It = Type.find(ID); It != Type.end())
	{
		for (auto Action : It->second)
		{
			Action->Process();
			xr_delete(Action);
		}
		Type.erase(ID);
	}
}

CSE_ALifeDynamicObject *CALifeObjectRegistry::get_object		(IReader &file_stream)
{
	NET_Packet				tNetPacket;
	u16						u_id;
	// Spawn
	tNetPacket.B.data.resize(file_stream.r_u16());
	file_stream.r			(tNetPacket.B.data.data(),tNetPacket.B.data.size());
	tNetPacket.r_begin		(u_id);
	R_ASSERT2				(M_SPAWN==u_id,"Invalid packet ID (!= M_SPAWN)");

	string64				s_name;
	tNetPacket.r_stringZ	(s_name);
#ifdef DEBUG
	if (psAI_Flags.test(aiALife)) {
		Msg					("Loading object %s [%d]b", s_name, tNetPacket.B.data.size());
	}
#endif
	// create entity
	CSE_Abstract			*tpSE_Abstract = F_entity_Create	(s_name);
	R_ASSERT2				(tpSE_Abstract,"Can't create entity.");
	CSE_ALifeDynamicObject	*tpALifeDynamicObject = smart_cast<CSE_ALifeDynamicObject*>(tpSE_Abstract);
	R_ASSERT2				(tpALifeDynamicObject,"Non-ALife object in the saved game!");
	tpALifeDynamicObject->Spawn_Read(tNetPacket);

	// Update
	tNetPacket.B.data.resize(file_stream.r_u16());
	file_stream.r			(tNetPacket.B.data.data(),tNetPacket.B.data.size());
	tNetPacket.r_begin		(u_id);
	R_ASSERT2				(M_UPDATE==u_id,"Invalid packet ID (!= M_UPDATE)");
	tpALifeDynamicObject->UPDATE_Read(tNetPacket);

	return					(tpALifeDynamicObject);
}

CSE_ALifeDynamicObject* CALifeObjectRegistry::get_object(ISaveObject& Object)
{
	shared_str				s_name;
	CSE_ALifeDynamicObject* tpALifeDynamicObject = nullptr;
	BEGIN_CHUNK(Object,"CALifeObjectRegistry::single_object")
	{
		Object << s_name;
#ifdef DEBUG
		if (psAI_Flags.test(aiALife)) {
			Msg("Loading object %s", s_name);
		}
#endif
		// create entity
		CSE_Abstract* tpSE_Abstract = F_entity_Create(s_name.c_str());
		R_ASSERT2(tpSE_Abstract, "Can't create entity.");
		tpALifeDynamicObject = smart_cast<CSE_ALifeDynamicObject*>(tpSE_Abstract);
		R_ASSERT2(tpALifeDynamicObject, "Non-ALife object in the saved game!");
		tpALifeDynamicObject->Spawn_Serialize(Object, true);
		tpALifeDynamicObject->UPDATE_Serialize(Object);
	}

	VERIFY(tpALifeDynamicObject);
	return					(tpALifeDynamicObject);
}

void CALifeObjectRegistry::load(IReader& file_stream)
{
	Msg("* Loading objects...");
	R_ASSERT2(file_stream.find_chunk(OBJECT_CHUNK_DATA), "Can't find chunk OBJECT_CHUNK_DATA!");

	m_objects.clear();

	u32 count = file_stream.r_u32();
	for (u32 i = 0; i < count; ++i)
	{
		auto Temp = get_object(file_stream);
		add(Temp);
	}

	Msg("* %d objects are successfully loaded", count);
}

void CALifeObjectRegistry::SerializeElem(ISaveObject& Object, CSE_ALifeDynamicObject* elem)
{
	if (!elem->can_save()) {
		return;
	}

	if (elem->redundant()) {
		return;
	}

	if (elem->ID_Parent != ALife::_OBJECT_ID(-1)) {
		return;
	}

	Serialize(Object, elem, m_serializable_object_count);
}

void CALifeObjectRegistry::Serialize(ISaveObject& Object)
{
	if (Object.IsSave()) {
		BEGIN_CHUNK(Object,"CALifeObjectRegistry")
		{
			Msg("* Saving objects...");

			m_serializable_object_count = 0;
			BEGIN_CHUNK(Object,"CALifeObjectRegistry::objects")
			{
				BEGIN_ARRAY(Object)
				{
					for (auto& elem : m_objects) {
						SerializeElem(Object, elem.second);
					}
				}
			}

			BEGIN_CHUNK(Object,"CALifeObjectRegistry::object_count")
			{
				Object << m_serializable_object_count;
			}
			Msg("* %d objects are successfully saved", m_serializable_object_count);
		}
	}
	else {
		BEGIN_CHUNK(Object,"CALifeObjectRegistry")
		{
			Msg("* Loading objects...");
			m_serializable_object_count = 0;
			m_objects.clear();
			
			BEGIN_CHUNK(Object,"CALifeObjectRegistry::object_count")
			{
				Object << m_serializable_object_count;
			}
			
			BEGIN_CHUNK(Object,"CALifeObjectRegistry::objects")
			{
				BEGIN_ARRAY(Object)
				{
					for (u64 i = 0; i < m_serializable_object_count; ++i) {
						add(get_object(Object));
					}
				}
			}

			Msg("* %d objects are successfully loaded", m_serializable_object_count);
		}

	}
}
