////////////////////////////////////////////////////////////////////////////
//	Module 		: UIInvUpgradeProperty.h
//	Created 	: 22.11.2007
//  Modified 	: 13.03.2009
//	Author		: Evgeniy Sokolov, Prishchepa Sergey
//	Description : inventory upgrade property UIWindow class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrUI/Widgets/UIStatic.h"
#include "inventory_item.h"
#include "inventory_upgrade_property.h"

namespace inventory::upgrade
{
	class Upgrade;
	class Property;
}

class UIProperty final : public CUIWindow
{
private:
	using inherited = CUIWindow;
	using Property_type = inventory::upgrade::Property;
	using Upgrade_type = inventory::upgrade::Upgrade;
	using ItemUpgrades_type = CInventoryItem::Upgrades_type;
	using PropertyFunctorParams_type = Property_type::FunctorParams_type;

protected:
	shared_str m_property_id;

	CUIStatic* m_ui_icon = nullptr;
	CUIStatic* m_ui_text = nullptr;
	string256 m_text = {};

public:
	UIProperty() = default;
	virtual	~UIProperty() = default;
	void init_from_xml(CUIXml& ui_xml);
	bool init_property(shared_str const& property_id);
	Property_type* get_property();

	bool read_value_from_section(LPCSTR section, LPCSTR param, float& result);
	bool compute_value(ItemUpgrades_type const& item_upgrades);
	bool show_result(LPCSTR values);

	virtual CUIWindow* ui_cast_window() { return this; }

}; // class UIProperty

// =========================================================================================

class UIInvUpgPropertiesWnd final : public CUIWindow
{
private:
	using inherited = CUIWindow;
	using Upgrade_type = inventory::upgrade::Upgrade;
	using Properties_type = xr_vector<UIProperty*>;
	using ItemUpgrades_type = CInventoryItem::Upgrades_type;

protected:
	Properties_type m_properties_ui = {};
	ItemUpgrades_type m_temp_upgrade_vector = {};
	CUIStatic* m_Upgr_line = nullptr;
	int	m_iNumUpgr = 0;
	float m_fsec_col_pos = 0.0f;
	float m_fnext_line_pos = 0.0f;

public:
	UIInvUpgPropertiesWnd();
	virtual	~UIInvUpgPropertiesWnd();
	void init_from_xml(LPCSTR xml_name);

	void set_upgrade_info(Upgrade_type& upgrade);
	void set_item_info(CInventoryItem& item);
	void UpdateStatsPos(float& h, Fvector2& pos, UIProperty* pWnd, int& counter) const;

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	void set_info(ItemUpgrades_type const& item_upgrades);

}; // class UIInvUpgPropertiesWnd