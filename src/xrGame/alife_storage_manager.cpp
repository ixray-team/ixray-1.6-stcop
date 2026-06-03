////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_storage_manager.cpp
//	Created 	: 25.12.2002
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator storage manager
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "alife_storage_manager.h"

#include "Actor.h"
#include "alife_simulator_header.h"
#include "alife_time_manager.h"
#include "alife_spawn_registry.h"
#include "alife_object_registry.h"
#include "alife_graph_registry.h"
#include "alife_group_registry.h"
#include "alife_registry_container.h"
#include "alife_simulator.h"
#include "xrServer.h"
#include "Level.h"
#include "../xrEngine/x_ray.h"
#include "saved_game_wrapper.h"
#include "../xrEngine/IGame_Persistent.h"
#include "autosave_manager.h"
#include "../xrCore/Save/SaveManager.h"
#include "../xrEngine/string_table.h"

XRCORE_API string_path g_bug_report_file;

using namespace ALife;

extern string_path g_last_saved_game;

CALifeStorageManager::~CALifeStorageManager()
{
}

void CALifeStorageManager::save(const char* save_name_no_check, bool update_name, bool non_async)
{
	const char* game_saves_path		= FS.get_path("$game_saves$")->m_Path;

	string_path					save_name;
	strncpy_s					(save_name, sizeof(save_name), save_name_no_check, sizeof(save_name)-5-xr_strlen(IXRAY_DEF_SAVE_EXTENSION)-xr_strlen(game_saves_path));

	xr_strcpy(g_last_saved_game, save_name);

	string_path					saveBackup;
	xr_strcpy					(saveBackup,m_save_name);
	if (save_name[0])
	{
		xr_strconcat(m_save_name, save_name, IXRAY_DEF_SAVE_EXTENSION);
	}
	else
	{
		if (!xr_strlen(m_save_name))
		{
			Log("There is no file name specified!");
			return;
		}
	}

	// To get the savegame fname to make our own custom save state
	luabind::functor<void> funct1;
	if (ai().script_engine().functor("alife_storage_manager.CALifeStorageManager_before_save", funct1))
	{
		funct1((str_c)m_save_name);
	}
	
	string_path temp;
	FS.update_path(temp, "$game_saves$", m_save_name);

	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		auto SaveObj = CSaveManager::GetInstance().BeginSave();
		SGameInfoFast info;
		info.m_actor_health = g_actor ? g_actor->GetfHealth() : 0;
		info.m_game_time = ai().alife().time_manager().game_time();
		auto map_name = Level().name(); // TODO: get actual level name
		info.m_level_name = map_name.size() ? map_name.c_str() : "Start";
		info.m_level_id = map_name.size() ? ai().level_graph().level_id() : u16(-1);
		header().Serialize(*SaveObj);
		BEGIN_CHUNK(*SaveObj, "MarshalData")
		{
			*SaveObj << marshal_save_data;
		}
		time_manager().Serialize(*SaveObj);
		spawns().Serialize(*SaveObj);
		objects().Serialize(*SaveObj);
		registry().Serialize(*SaveObj);
		CSaveManager::GetInstance().WriteSavedData(info, SaveObj, temp, !non_async);
	} else
	{
		u32 source_count;
		u32 dest_count;
		void* dest_data;
		{
			CMemoryWriter stream;
			header().save(stream);
			{
				stream.open_chunk(MARSHAL_CHUNK_DATA);
				stream.w_u32(marshal_save_data.size());
				stream.w(marshal_save_data.data(), marshal_save_data.size());
				stream.close_chunk();
			}
			time_manager().save(stream);
			spawns().save(stream);
			objects().save(stream);
			registry().save(stream);
			source_count = stream.tell();
			void* source_data = stream.pointer();
			dest_count = rtc_csize(source_count);
			dest_data = xr_malloc(dest_count);
			dest_count = (u32)rtc_compress(dest_data, dest_count, source_data, source_count);
		}

		IWriter* writer = FS.w_open(temp);
		writer->w_u32(u32(-1));
		writer->w_u32(ALIFE_VERSION);

		writer->w_u32(source_count);
		writer->w(dest_data, dest_count);
		xr_free(dest_data);
		FS.w_close(writer, non_async);
#ifdef DEBUG
		Msg("* Game %s is successfully saved to file '%s' (%d bytes compressed to %d)", m_save_name, temp, source_count, dest_count + 4);
#else // DEBUG
		Msg("* Game %s is successfully saved to file '%s'", m_save_name, temp);
#endif // DEBUG
	}

	// To get the savegame fname to make our own custom save states
	luabind::functor<void> funct3;
	if (ai().script_engine().functor("alife_storage_manager.CALifeStorageManager_after_save", funct3))
	{
		funct3((const char*)m_save_name);
	}

	if (!update_name)
		xr_strcpy					(m_save_name,saveBackup);
}

void CALifeStorageManager::load(IReader* stream, const char* file_name)
{
	IReader& source = *stream;
	CSaveObjectLoad* Obj = nullptr;
	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		Obj = CSaveManager::GetInstance().BeginLoad(stream);
		header().Serialize(*Obj);
		BEGIN_CHUNK(*Obj, "MarshalData")
		{
			*Obj << marshal_save_data;
		}
	}
	else
	{
		header().load(source);
		if (auto MarshalChunk = source.open_chunk(MARSHAL_CHUNK_DATA); MarshalChunk)
		{
			marshal_save_data.resize(MarshalChunk->r_u32());
			std::memcpy(marshal_save_data.data(), MarshalChunk->pointer(), marshal_save_data.size());
		}
	}
	
	
	// So we can get the fname to make our own custom save states
	luabind::functor<void> funct;
	ai().script_engine().functor("alife_storage_manager.CALifeStorageManager_load", funct);
	if (funct)
	{
		funct(file_name);
	}

	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		time_manager().Serialize(*Obj);
		spawns().Serialize(*Obj);
		graph().on_load();
		objects().Serialize(*Obj);
		registry().Serialize(*Obj);
		xr_delete(Obj);
	} else
	{
		time_manager().load(source);
		spawns().load(source, file_name);
		graph().on_load();
		objects().load(source);
		registry().load(source);
	}

	VERIFY(can_register_objects());
	can_register_objects(false);
	auto& Objects = objects().objects();
	for (auto& elem : Objects) {
		ALife::_OBJECT_ID id = elem.second->ID;
		elem.second->ID = server().PerformIDgen(id);
		VERIFY(id == elem.second->ID);
		register_object(elem.second, false);
	}

	can_register_objects(true);

	for (auto& elem : Objects)
	{
		elem.second->on_register();
	}

	if (!g_pGameLevel)
		return;

	Level().autosave_manager().on_game_loaded();
}

bool CALifeStorageManager::load(const char* save_name_no_check)
{
	const char* game_saves_path		= FS.get_path("$game_saves$")->m_Path;

	string_path					save_name;
	strncpy_s					(save_name, sizeof(save_name), save_name_no_check, sizeof(save_name)-5-xr_strlen(IXRAY_DEF_SAVE_EXTENSION)-xr_strlen(game_saves_path));

	CTimer timer;
	timer.Start();

	string_path					saveBackup;
	xr_strcpy					(saveBackup,m_save_name);
	if (!save_name[0]) {
		if (!xr_strlen(m_save_name))
		{
			Log("There is no file name specified!");
			return false;
		}
	}
	else
	{
		xr_strconcat(m_save_name, save_name, IXRAY_DEF_SAVE_EXTENSION);
	}

	string_path file_name;
	FS.update_path(file_name, "$game_saves$", m_save_name);

	xr_strcpy(g_last_saved_game, save_name);
	xr_strcpy(g_bug_report_file, file_name);

	xr_strcpy					(g_last_saved_game, save_name);
	xr_strcpy					(g_bug_report_file, file_name);

    IReader* stream = FS.r_open(file_name);
    if (!stream)
    {
        Msg("* Cannot open saved game %s", file_name);
        xr_strcpy(m_save_name, saveBackup);
        return false;
    }

	constexpr const char* mismatch = "Saved game version mismatch or saved game is corrupted";
	const bool gameSaveIsValid = CSavedGameWrapper::valid_saved_game(*stream);
	VERIFY3(gameSaveIsValid, mismatch, file_name);

	if (!gameSaveIsValid)
	{
		Msg("! %s [%s]", mismatch, file_name);

		xr_strcpy(m_save_name, saveBackup);
		return false;
	}

	string512					temp;
	xr_strconcat(temp, g_pStringTable->translate("st_loading_saved_game").c_str(), " \"", save_name, IXRAY_DEF_SAVE_EXTENSION, "\"");
	g_pGamePersistent->SetLoadStageTitle(temp);
	g_pGamePersistent->LoadTitle();

	unload();
	reload(m_section);

	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		stream->rewind();
		load(stream, file_name);
		FS.r_close(stream);
	} else
	{
		u32 source_count = stream->r_u32();
		void* source_data = xr_malloc(source_count);
		rtc_decompress(source_data, source_count, stream->pointer(), stream->length() - 3 * sizeof(u32));
		FS.r_close(stream);
		IReader ReaderStream = IReader(source_data, source_count);
		load(&ReaderStream, file_name);
		xr_free(source_data);
	}

	groups().on_after_game_load();

	VERIFY(graph().actor());

	Msg("* Game %s is successfully loaded from file '%s' (%.3fs)", save_name, file_name, timer.GetElapsed_sec());

	return true;
}

void CALifeStorageManager::save(NET_Packet& net_packet)
{
	prepare_objects_for_save();

	shared_str game_name;
	net_packet.r_stringZ(game_name);
	save(*game_name, !!net_packet.r_u8(), !EngineExternal()[EEngineExternalSystem::AdvancedSerialization]);
}

void CALifeStorageManager::prepare_objects_for_save()
{
	Level().ClientSend();
	if (!EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		Level().ClientSave();
	}
}
