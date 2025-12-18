// FX: Экспорты lua функций для поддежки EE
// для работы требуется xr_sound и sound_theme переместить из оригинальной триологии в папку скриптов EE

#include "StdAfx.h"
#include "pch_script.h"
#include "ai_space.h"
#include "Actor.h"
#include "Inventory.h"
#include "script_game_object.h"
#include "../xrEngine/xr_input.h"
#include "../xrScripts/exports/script_ini_file.h"
class CScriptIniFile;

SCRIPTS_API CScriptIniFile* create_ini_file(LPCSTR ini_string);
XRCORE_API xr_hash_map<xr_string, CInifile*>* cached_ini_map;

CScriptIniFile* CacheIni(const char* Name)
{
	auto Iter = cached_ini_map->find(Name);
	if (Iter != std::end(*cached_ini_map))
	{
		return (CScriptIniFile*)Iter->second;
	}

	auto& IniFile = (*cached_ini_map)[Name];
	IniFile = new CScriptIniFile(Name);
	return (CScriptIniFile*)IniFile;
}

void ExportEELayer(lua_State* L)
{
	// Other trash module
	luabind::module(L)
	[
		luabind::def("get_cached_ini",   &CacheIni),
		luabind::def("unlock_trophy",    +[]() {}),
		luabind::def("get_platform_id",  +[]() { return 7; }),
		luabind::def("detectKeyboard",   +[]() { return true; }),
		luabind::def("detectController", +[]() { return pInput->pGamePad != nullptr; }),

		// консольная хуйня, скип
		luabind::def("savedata_delete",  +[]() {}), 
		luabind::def("set_current_user", +[]() {}),
		luabind::def("is_full_savedata", +[]() { return false; })
	];

	// Enum для платформ
	luabind::module(L, "platform_ids")
	[
		luabind::def("zerofunc", +[](){})
	];
	luabind::object platform_ids = luabind::get_globals(L)["platform_ids"];
	platform_ids["PLATFORM_ORBIS"] = 0;
	platform_ids["PLATFORM_PROSPERO"] = 1;
	platform_ids["PLATFORM_DURANGO"] = 2;
	platform_ids["PLATFORM_GDK"] = 3;
	platform_ids["PLATFORM_GDK_1440"] = 4;
	platform_ids["PLATFORM_GDK_4K"] = 5;
	platform_ids["PLATFORM_NX64"] = 6;
	platform_ids["PLATFORM_WIN64"] = 7;

	// GameGraph module
	luabind::module(L)
	[
		luabind::def("gg_vertex_level_id", +[](u32 VertexID) 
		{
			return ai().game_graph().vertex(VertexID)->level_id();
		}),
			
		luabind::def("gg_levels_count", +[]() 
		{
			return ai().game_graph().header().levels().size();
		}),
			
		luabind::def("gg_level_id", +[](size_t Index)
		{
			const auto& Levels = ai().game_graph().header().levels();

			if (Index >= Levels.size())
			{
				return GameGraph::_LEVEL_ID(-1);
			}

			auto It = Levels.begin();
			std::advance(It, Index);
			return It->first;
		}),

		luabind::def("gg_distance", +[](u32 LeftVertexID, u32 RightVertexID)
		{
			auto RightPoint = ai().game_graph().vertex(RightVertexID)->game_point();
			return ai().game_graph().vertex(LeftVertexID)->game_point().distance_to(RightPoint);
		})
	];

	// FX: xr_sound_impl через костыль
	const char* xr_sound_code =
	R"(
		xr_sound_impl =
		{
			play_all_sound_looped_impl = function(obj)
				local obj_id = obj:id()
				if xr_sound.looped_sound[obj_id] then
					for k, v in pairs(xr_sound.looped_sound[obj_id]) do
						xr_sound.play_sound_looped(obj_id, k)
					end
				end
			end,
			
			sound_exists = function(npc_id)
				return xr_sound.sound_table[npc_id] ~= nil
			end,
			
			stop_sound = function(npc)
				local npc_id = npc:id()
				if xr_sound.sound_table[npc_id] then
					xr_sound.sound_table[npc_id]:stop(npc_id)
				end
			end,

			reset_sound = function(npc)
				local npc_id = npc:id()
				local sound_theme = xr_sound.sound_table[npc_id]
				if sound_theme and sound_theme.reset then
					sound_theme:reset(npc_id)
				end
			end
		}
	)";

	if (luaL_loadbuffer(L, xr_sound_code, strlen(xr_sound_code), "xr_sound_impl") == 0)
	{
		if (lua_pcall(L, 0, 0, 0) != 0)
		{
			const char* error = lua_tostring(L, -1);
			Msg("![xr_sound_impl load error]: %s", error);
			lua_pop(L, 1);
		}
	}
	else
	{
		const char* error = lua_tostring(L, -1);
		Msg("![xr_sound_impl loadbuffer error]: %s", error);
		lua_pop(L, 1);
	}
}

void IterateActiveItem(CScriptGameObject* Owner, luabind::object func, luabind::object context)
{
	if (Actor()->inventory().GetActiveSlot() != NO_ACTIVE_SLOT)
	{
		try
		{
			luabind::call_function<void>(func, Owner, Actor()->inventory().ItemFromSlot(Actor()->inventory().GetActiveSlot())->object().lua_game_object());
		}
		catch (...)
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Error in iterate_activeitems callback!");
		}
	}

	if (CInventoryItem* result = Actor()->inventory().ItemFromSlot(DEVICE_SLOT))
	{
		luabind::call_function<void>(func, Owner, result->object().lua_game_object());
	}
};

luabind::class_<CScriptGameObject> script_register_game_object_ee(luabind::class_<CScriptGameObject>&& instance)
{
	return std::move(instance).def("iterate_activeitems", &IterateActiveItem);
}