#include "StdAfx.h"
#include "InventoryItemDesc.h"
#include "../inventory_item.h"

void SInventoryItemDesc::Load(const shared_str& Section)
{
	if (!Section.size())
	{
		return;
	}
	if (pSettings->line_exist(Section, "parse_spawn_items") && pSettings->line_exist(Section, "parse_spawn_chances"))
	{
		shared_str SpawnList = pSettings->r_string(Section, "parse_spawn_items");
		shared_str ChanceList = pSettings->r_string(Section, "parse_spawn_chances");

		int Count = _GetItemCount(SpawnList.c_str());
		int Count2 = _GetItemCount(ChanceList.c_str());

		string256 sItem = {};

		for (int i = 0; i < Count; ++i)
		{
			m_parse_params.m_items.emplace_back(_GetItem(SpawnList.c_str(), i, sItem));
		}

		for (int i = 0; i < Count2; ++i)
		{
			m_parse_params.m_chances.push_back(atof(_GetItem(ChanceList.c_str(), i, sItem)));
		}
	}

	m_can_trade = pSettings->read_if_exists<bool>(Section, "can_trade", true);
	m_highlight_equipped = !!pSettings->read_if_exists<bool>(Section, "highlight_equipped", false);
	m_icon_name = pSettings->read_if_exists<str_c>(Section, "icon_name", nullptr);

	const static bool isLegacyUpgrade = EngineExternal()[EEngineExternalGame::EnableLegacyUpgradeSystem];
	m_legacy_upgrade_mode = pSettings->read_if_exists<bool>(Section, "legacy_upgrade_mode", isLegacyUpgrade);

	m_custom_text_auto_uses = pSettings->read_if_exists<bool>(Section, "item_custom_text_auto_uses", false);
	m_custom_text_anchor = CInventoryItem::ParseInvCellAnchor(pSettings->read_if_exists<str_c>(Section, "item_custom_text_anchor", "bottom_right"));
	m_custom_mark_anchor = CInventoryItem::ParseInvCellAnchor(pSettings->read_if_exists<str_c>(Section, "item_custom_mark_anchor", "bottom_right"));

	m_holder_range_modifier = pSettings->read_if_exists<float>(Section, "holder_range_modifier", 1.f);
	m_holder_fov_modifier = pSettings->read_if_exists<float>(Section, "holder_fov_modifier", 1.f);
}
