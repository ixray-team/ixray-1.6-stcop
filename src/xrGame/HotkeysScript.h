#pragma once

#include "../xrScripts/script_export_space.h"

namespace HotkeysScript
{
const char* CurrentContext();
bool Dispatch(int Dik);
}

struct hotkeys_registrator
{
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
