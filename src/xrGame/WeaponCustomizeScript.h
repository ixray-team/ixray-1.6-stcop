#pragma once

#include "../xrScripts/script_export_space.h"

class CInventoryItem;

namespace WeaponCustomizeScript
{
bool TryOpen(CInventoryItem* Item);
}

struct weapon_customize_registrator
{
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
