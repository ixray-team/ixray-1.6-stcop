////////////////////////////////////////////////////////////////////////////
//	Module 		: server_entity_wrapper.cpp
//	Created 	: 16.10.2004
//  Modified 	: 16.10.2004
//	Author		: Dmitriy Iassenev
//	Description : Server entity wrapper
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "server_entity_wrapper.h"
#include "xrServer_Objects.h"
#include "xrMessages.h"
#include "../xrCore/Save/MemoryBuffer.h"
#include "../xrCore/Save/SaveManager.h"

#ifdef AI_COMPILER
#	include "factory_api.h"
#endif

struct ISE_Abstract;

CServerEntityWrapper::~CServerEntityWrapper	()
{
#ifndef _LEVEL_EDITOR
	F_entity_Destroy		(m_object);
#endif
}

void CServerEntityWrapper::save				(IWriter &stream)
{
	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		SSaveTask dummy;
		{
			auto ObjPtr = CSaveManager::GetInstance().EditorBeginSave();
			auto& Obj = *ObjPtr;

			stream.open_chunk(0);
			CMemoryBuffer buffer;
			shared_str Name = m_object->name();
			Obj << Name;
			m_object->Spawn_Serialize(Obj, true);
			buffer.Write(ESaveVariableType::t_chunk);
			Obj.Write(&buffer, &dummy);
			buffer.Write(&stream);
			stream.close_chunk();
			xr_delete(ObjPtr);
		}
		{
			
			auto ObjPtr = CSaveManager::GetInstance().EditorBeginSave();
			auto& Obj = *ObjPtr;

			stream.open_chunk(1);
			CMemoryBuffer buffer;
			m_object->UPDATE_Serialize(Obj);
			buffer.Write(ESaveVariableType::t_chunk);
			Obj.Write(&buffer, &dummy);
			buffer.Write(&stream);
			stream.close_chunk();
			xr_delete(ObjPtr);
		}
	} else
	{
		NET_Packet				net_packet;

		// Spawn
		stream.open_chunk		(0);

		m_object->Spawn_Write	(net_packet,TRUE);
		stream.w_u16			(net_packet.B.data.size());
		stream.w				(net_packet.B.data.data(),net_packet.B.data.size());
	
		stream.close_chunk		();

		// Update
		stream.open_chunk		(1);

		net_packet.w_begin		(M_UPDATE);
		m_object->UPDATE_Write	(net_packet);
		stream.w_u16			(net_packet.B.data.size());
		stream.w				(net_packet.B.data.data(),net_packet.B.data.size());

		//	u16						ID;
		//	net_packet.r_begin		(ID);
		//	VERIFY					(ID==M_UPDATE);
		//	m_object->UPDATE_Read	(net_packet);
	
		stream.close_chunk		();
	}
}

void CServerEntityWrapper::load				(IReader &stream)
{
#ifndef _LEVEL_EDITOR
	
	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		{
			auto chunk = stream.open_chunk(0);
			auto ObjPtr = CSaveManager::GetInstance().EditorBeginLoad(chunk);
			auto& Obj = *ObjPtr;
			shared_str Name;
			Obj << Name;
			VERIFY(!m_object);
			m_object = F_entity_Create(Name.c_str());
			m_object->Spawn_Serialize(Obj);
			xr_delete(ObjPtr);
			chunk->close();
		}
		{
			auto chunk = stream.open_chunk(1);
			auto ObjPtr = CSaveManager::GetInstance().EditorBeginLoad(chunk);
			auto& Obj = *ObjPtr;
			m_object->UPDATE_Serialize(Obj);
			xr_delete(ObjPtr);
			chunk->close();
		}
	} else
	{
		NET_Packet				net_packet;
		u16						ID;
		IReader					*chunk;
	
		chunk					= stream.open_chunk(0);

		net_packet.B.data.resize(chunk->r_u16());
		chunk->r(net_packet.B.data.data(),net_packet.B.data.size());

		chunk->close			();

		net_packet.r_begin		(ID);
		R_ASSERT2				(M_SPAWN == ID,"Invalid packet ID (!= M_SPAWN)!");

		string64				s_name;
		net_packet.r_stringZ	(s_name);
	
		m_object				= F_entity_Create(s_name);

		R_ASSERT3				(m_object,"Can't create entity.",s_name);
		m_object->Spawn_Read	(net_packet);
	
		chunk					= stream.open_chunk(1);
	
		net_packet.B.data.resize(chunk->r_u16());
		chunk->r(net_packet.B.data.data(),net_packet.B.data.size());
	
		chunk->close			();

		net_packet.r_begin		(ID);
		R_ASSERT2				(M_UPDATE == ID,"Invalid packet ID (!= M_UPDATE)!");
		m_object->UPDATE_Read	(net_packet);
	}
#else
	R_ASSERT(false);
#endif
}

void CServerEntityWrapper::save_update		(IWriter &stream)
{
//	NET_Packet				net_packet;
//	net_packet.w_begin		(M_UPDATE);
//	m_object->save_update	(net_packet);
//	stream.w_u16			(u16(net_packet.B.count));
//	stream.w				(net_packet.B.data,net_packet.B.count);
}

void CServerEntityWrapper::load_update		(IReader &stream)
{
//	NET_Packet				net_packet;
//	u16						ID;
//
//	net_packet.B.count		= stream.r_u16();
//	stream.r				(net_packet.B.data,net_packet.B.count);
//
//	net_packet.r_begin		(ID);
//	R_ASSERT2				(M_UPDATE == ID,"Invalid packet ID (!= M_UPDATE)!");
//	m_object->load_update	(net_packet);
}

void CServerEntityWrapper::serialize_update(ISaveObject& Object)
{
}
