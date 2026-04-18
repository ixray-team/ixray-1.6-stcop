////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_item_inline.h
//	Created 	: 24.03.2003
//  Modified 	: 29.01.2004
//	Author		: Victor Reutsky, Yuri Dobronravin, Sokolov Evgeniy
//	Description : Inventory item inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC	bool CInventoryItem::useful_for_NPC() const
{
	return Useful() && m_flags.test(Fuseful_for_NPC);
}

IC CInventoryItem::Upgrades_type const& CInventoryItem::upgardes() const
{
	return m_upgrades;
}

template <XRay::Concepts::Arithmetic T>
IC bool CInventoryItem::process_if_exists(const char* section, const char* name, T& value, bool test)
{
	static_assert(!std::is_same_v<T, char> && !std::is_same_v<T, double>, "process_if_exists: type bool, char or double is not allowed");

	if (!pSettings->line_exist(section, name))
	{
		return false;
	}

	const char* str = pSettings->r_string(section, name);
	if (!str || !xr_strlen(str))
	{
		return false;
	}

	if (!test)
	{
		value += pSettings->read<T>(section, name); // add
	}

	return true;
}

template <XRay::Concepts::Arithmetic T>
IC bool CInventoryItem::process_if_exists_set(const char* section, const char* name, T& value, bool test)
{
	static_assert(!std::is_same_v<T, char> && !std::is_same_v<T, double>, "process_if_exists_set: type bool, char or double is not allowed");

	if (!pSettings->line_exist(section, name))
	{
		return false;
	}

	const char* str = pSettings->r_string(section, name);
	if (!str || !xr_strlen(str))
	{
		return false;
	}

	if (!test)
	{
		value = pSettings->read<T>(section, name);    // set
	}

	return true;
}

IC bool CInventoryItem::process_if_exists_set(const char* section, const char* name, shared_str& value, bool test)
{
	if (!pSettings->line_exist(section, name))
	{
		return false;
	}

	const char* str = pSettings->r_string(section, name);
	if (!str || !xr_strlen(str))
	{
		return false;
	}

	if (!test)
	{
		value._set(str);
	}

	return true;
}

IC bool CInventoryItem::process_if_exists_set(const char* section, const char* name, const char*& value, bool test)
{
	if (!pSettings->line_exist(section, name))
	{
		return false;
	}

	const char* str = pSettings->r_string(section, name);
	if (!str || !xr_strlen(str))
	{
		return false;
	}

	if (!test)
	{
		value = str;
	}

	return true;
}

IC bool CInventoryItem::process_if_exists_set(const char* section, const char* name, xr_string& value, bool test)
{
	if (!pSettings->line_exist(section, name))
	{
		return false;
	}

	const char* str = pSettings->r_string(section, name);
	if (!str || !xr_strlen(str))
	{
		return false;
	}

	if (!test)
	{
		value = str;
	}

	return true;
}

template <XRay::Concepts::FloatPoint T>
IC bool CInventoryItem::process_if_exists_deg2rad(const char* section, const char* name, T& value, bool test)
{
	if (!pSettings->line_exist(section, name))
	{
		return false;
	}

	const char* str = pSettings->r_string(section, name);
	if (!str || !xr_strlen(str))
	{
		return false;
	}

	if (!test)
	{
		value += deg2rad(pSettings->r_float(section, name));
	}

	return true;
}