////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_upgrade_base.cpp
//	Created 	: 19.10.2007
//  Modified 	: 27.11.2007
//	Author		: Evgeniy Sokolov
//	Description : inventory upgrade base class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"

#include "inventory_upgrade_base.h"
#include "inventory_upgrade_manager.h"
#include "inventory_upgrade_group.h"
#include "inventory_upgrade.h"

extern int g_upgrades_log;

using namespace inventory::upgrade;

void UpgradeBase::construct(const shared_str& upgrade_id, Manager& manager_r)
{
	m_id._set(upgrade_id);

	VERIFY2(pSettings->section_exist(m_id), make_string<const char*>("Section of upgrade [%s] not exist!", m_id.c_str()));
}

void UpgradeBase::add_dependent_groups(const char* groups_str, Manager& manager_r)
{
	string512 temp = {};

	const int n = _GetItemCount(groups_str);
	for (int i = 0; i < n; ++i)
	{
		Group* group_p = manager_r.add_group(_GetItem(groups_str, i, temp, sizeof(temp)), *this);

		if (std::find(m_depended_groups.begin(), m_depended_groups.end(), group_p) == m_depended_groups.end())
		{
			m_depended_groups.push_back(group_p);
		}
	}
}

#ifdef DEBUG
void UpgradeBase::log_hierarchy(const char* nest)
{
	for (const auto& depended_group : m_depended_groups)
	{
		depended_group->log_hierarchy(nest);
	}
}
#endif // DEBUG

bool UpgradeBase::is_root()
{
	return false;
}

bool UpgradeBase::make_known()
{
	m_known = true;
	return true;
}

bool UpgradeBase::contain_upgrade(const shared_str& upgrade_id)
{
	return (m_id._get() == upgrade_id._get());
}

void UpgradeBase::fill_root_container(Root* root)
{
	for (const auto& depended_group : m_depended_groups)
	{
		depended_group->fill_root(root);
	}
}

UpgradeStateResult UpgradeBase::can_install(CInventoryItem& item, bool loading)
{
	if (!m_known && !loading)
	{
		if (g_upgrades_log == 1)
		{
			Msg("- Upgrade <%s> (id = %d) is in mode <unknown>.", id_str(), item.object_id());
		}

		return result_e_unknown;
	}

	if (item.has_upgrade(m_id))
	{
		if (g_upgrades_log == 1)
		{
			Msg("- Upgrade <%s> (id = %d) is installed already.", id_str(), item.object_id());
		}

		return result_e_installed; // true
	}

	return result_ok;
}
