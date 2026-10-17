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

	const static bool isLegacyUpgrade = EngineExternal()[EEngineExternalGame::EnableLegacyUpgradeSystem];
	m_legacy_upgrade_mode = isLegacyUpgrade;

	XReflect::ReadIni(*this, Section);

	m_custom_text_anchor = CInventoryItem::ParseInvCellAnchor(pSettings->read_if_exists<str_c>(Section, "item_custom_text_anchor", "bottom_right"));
	m_custom_mark_anchor = CInventoryItem::ParseInvCellAnchor(pSettings->read_if_exists<str_c>(Section, "item_custom_mark_anchor", "bottom_right"));
}
