#pragma once
#include "DescRegistry.h"

enum class EInvCellAnchor : u8
{
	BottomRight = 0,
	BottomLeft,
	TopRight,
	TopLeft,
};

struct SInventoryParseItem
{
	RStringVec m_items = {};
	FloatVec m_chances = {};
};

struct SInventoryItemDesc final
{
	using Registry = TDescRegistry<SInventoryItemDesc>;

	shared_str m_icon_name;
	SInventoryParseItem m_parse_params;

	float m_holder_range_modifier = 1.0f;
	float m_holder_fov_modifier = 1.0f;

	EInvCellAnchor m_custom_text_anchor = EInvCellAnchor::BottomRight;
	EInvCellAnchor m_custom_mark_anchor = EInvCellAnchor::BottomRight;

	bool m_can_trade = true;
	bool m_highlight_equipped = false;
	bool m_legacy_upgrade_mode = false;
	bool m_custom_text_auto_uses = false;

	void Load(const shared_str& Section);
};
