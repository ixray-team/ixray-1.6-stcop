////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_upgrade_manager.h
//	Created 	: 19.10.2007
//  Modified 	: 27.11.2007
//	Author		: Dmitriy Iassenev, Evgeniy Sokolov
//	Description : inventory upgrade manager class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrCore/associative_vector.h"
#include "inventory_item_object.h"

#include "inventory_upgrade_base.h"

namespace inventory::upgrade {

class Upgrade;
class Root;
class Group;
class Property;

struct shared_str_predicate
{
	IC bool operator() (const shared_str& a, const shared_str& b) const
	{
		return (a._get() < b._get());
	}
};

class Manager final
{
public:
	using Properties_type = associative_vector<shared_str, Property*, shared_str_predicate>;

private:
	using Roots_type = associative_vector<shared_str, Root*, shared_str_predicate>;
	using Groups_type = associative_vector<shared_str, Group*, shared_str_predicate>;
	using Upgrades_type = associative_vector<shared_str, Upgrade*, shared_str_predicate>;

public:
	Manager(const Manager& other) = delete;
	Manager& operator=(const Manager& other) = delete;
	Manager();
	virtual	~Manager();

public:
	Root* get_root(shared_str const& root_id);
	Group* get_group(shared_str const& group_id);
	Upgrade* get_upgrade(shared_str const& upgrade_id);
	Property* get_property(shared_str const& property_id);

	Root* add_root(shared_str const& root_id);
	Group* add_group(shared_str const& group_id, UpgradeBase& parent_upgrade);
	Upgrade* add_upgrade(shared_str const& upgrade_id, Group& parent_group);
	Property* add_property(shared_str const& property_id);

	bool make_known_upgrade(CInventoryItem& item, shared_str const& upgrade_id);
	bool make_known_upgrade(shared_str const& upgrade_id);
	bool is_known_upgrade(CInventoryItem& item, shared_str const& upgrade_id);
	bool is_known_upgrade(shared_str const& upgrade_id);

	bool upgrade_install(CInventoryItem& item, shared_str const& upgrade_id, bool loading);
	void init_install(CInventoryItem& item);

	bool compute_range(const char* parameter, float& low, float& high);
	const char* get_item_scheme(CInventoryItem& item);
	const char* get_upgrade_by_index(CInventoryItem& item, Ivector2 const& index);

#ifdef DEBUG
	void log_hierarchy();
	void test_all_upgrades(CInventoryItem& item);
#endif // DEBUG
	void RefreshTranslations();

	void highlight_hierarchy(CInventoryItem& item, shared_str const& upgrade_id);
	void reset_highlight(CInventoryItem& item);

private:
	void load_all_inventory();
	void load_all_properties();

	Upgrade* upgrade_verify(shared_str const& item_section, shared_str const& upgrade_id);

	void compute_range_section(const char* section, const char* parameter, float& low, float& high);

private:
	Roots_type m_roots = {};
	Groups_type m_groups = {};
	Upgrades_type m_upgrades = {};

public:
	Properties_type m_properties = {};

}; // class Manager
} //namespace inventory::upgrade
