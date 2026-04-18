////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_upgrade_group.h
//	Created 	: 22.10.2007
//  Modified 	: 27.11.2007
//	Author		: Evgeniy Sokolov 
//	Description : inventory upgrade group class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "inventory_upgrade_manager.h"

namespace inventory::upgrade {

class Group final
{
public:
	Group(const Group& other) = delete;
	Group& operator=(const Group& other) = delete;
	Group() = default;
	virtual ~Group() = default;
	void construct(const shared_str& group_id, UpgradeBase& parent_upgrade, Manager& manager_r);
	void add_parent_upgrade(UpgradeBase& parent_upgrade);

	IC const shared_str& id() const { return m_id; }
	IC const char* id_str() const { return m_id.c_str(); }

#ifdef DEBUG
	void log_hierarchy(const char* nesting);
#endif // DEBUG

	void fill_root(Root* root);

	UpgradeStateResult can_install(CInventoryItem& item, UpgradeBase& test_upgrade, bool loading);

	void highlight_up();
	void highlight_down();

private:
	using Upgrades_type = xr_vector<UpgradeBase*>;

private:
	shared_str m_id;

	Upgrades_type m_parent_upgrades = {};
	Upgrades_type m_included_upgrades = {};

}; // class group
}
