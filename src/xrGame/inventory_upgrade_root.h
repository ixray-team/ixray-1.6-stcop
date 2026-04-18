////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_upgrade_root.h
//	Created 	: 19.10.2007
//  Modified 	: 27.11.2007
//	Author		: Evgeniy Sokolov
//	Description : inventory upgrade root class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "inventory_upgrade_manager.h"

namespace inventory::upgrade
{

class Root final : public UpgradeBase
{
private:
	using inherited = UpgradeBase;
	using Upgrades_vec = xr_vector<Upgrade*>;

public:
	Root() = default;
	virtual ~Root() = default;
	void construct(const shared_str& root_id, Manager& manager_r);
	IC const char* scheme() const { return m_upgrade_scheme.c_str(); }

	void add_upgrade(Upgrade* upgr);
	virtual	bool is_root() override { return true; }

#ifdef DEBUG
	virtual void log_hierarchy(const char* nest) override;
	void test_all_upgrades(CInventoryItem& item);
#endif // DEBUG

	virtual bool contain_upgrade(const shared_str& upgrade_id);
	bool verify_scheme_index(const Ivector2& scheme_index);
	Upgrade* get_upgrade_by_index(Ivector2 const& index);

	void highlight_hierarchy(shared_str const& upgrade_id);
	void reset_highlight();

protected:
	shared_str m_upgrade_scheme;
	Upgrades_vec m_contained_upgrades = {};

}; // class Root
} // namespace inventory::upgrade