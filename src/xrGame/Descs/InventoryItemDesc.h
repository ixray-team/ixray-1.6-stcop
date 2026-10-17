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

XREFLECT(IniRead)
struct SInventoryItemDesc final
{
	using Registry = TDescRegistry<SInventoryItemDesc>;

	XPROPERTY(IniReadExist, Name="icon_name")
	shared_str m_icon_name;
	SInventoryParseItem m_parse_params;

	XPROPERTY(IniReadExist, Name="holder_range_modifier")
	float m_holder_range_modifier = 1.0f;
	XPROPERTY(IniReadExist, Name="holder_fov_modifier")
	float m_holder_fov_modifier = 1.0f;

	EInvCellAnchor m_custom_text_anchor = EInvCellAnchor::BottomRight;
	EInvCellAnchor m_custom_mark_anchor = EInvCellAnchor::BottomRight;

	XPROPERTY(IniReadExist, Name="can_trade")
	bool m_can_trade = true;
	XPROPERTY(IniReadExist, Name="highlight_equipped")
	bool m_highlight_equipped = false;
	XPROPERTY(IniReadExist, Name="legacy_upgrade_mode")
	bool m_legacy_upgrade_mode = false;
	XPROPERTY(IniReadExist, Name="item_custom_text_auto_uses")
	bool m_custom_text_auto_uses = false;

	void Load(const shared_str& Section);
};
