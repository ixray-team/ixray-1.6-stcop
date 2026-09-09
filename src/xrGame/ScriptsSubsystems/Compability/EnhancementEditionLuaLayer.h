#pragma once

void ExportEELayer(lua_State* L);
luabind::class_<CScriptGameObject> script_register_game_object_ee(luabind::class_<CScriptGameObject>&& instance);