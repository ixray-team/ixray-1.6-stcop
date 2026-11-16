#pragma once

#include "../xrScripts/script_export_space.h"

struct console_registrator{
	static void ClearLuaCommands();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};