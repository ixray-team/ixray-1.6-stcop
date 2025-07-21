#include "StdAfx.h"
#include "pch_script.h"
#include "addon_manager.h"
#include "../xrCore/xrAddons.h"

using namespace luabind;
#pragma optimize("s",on)

LPCSTR get_addon_name_script(CAddonManager::AddonInfo info)
{
	return info.AddonName.c_str();
}

LPCSTR get_addon_script_init(CAddonManager::AddonInfo info)
{
	return info.ScriptInit.c_str();
}

LPCSTR get_addon_entry_script(CAddonManager::AddonInfo info)
{
	return info.EntryDir.c_str();
}

void addon_manager::script_register(lua_State *L)
{
	module(L)
	[
		class_<CAddonManager>("CAddonManager")
			.def_readonly("Addons",					&CAddonManager::Addons)
			.def("AddonName",						&get_addon_name_script)
			.def("ScriptInit",						&get_addon_script_init)
			.def("EntryDir",						&get_addon_entry_script)
	];
}
