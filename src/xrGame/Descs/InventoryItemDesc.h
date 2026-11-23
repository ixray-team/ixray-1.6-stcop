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

struct SAttachPlacement
{
	Fmatrix m_world = Fidentity;
	Fmatrix m_hud = Fidentity;
};

XREFLECT(IniRead, Owner="CInventoryItem")
struct SInventoryItemDesc final
{
	using Registry = TDescRegistry<SInventoryItemDesc>;

	XPROPERTY(IniReadExist, Name="icon_name")
	shared_str m_icon_name;
	SInventoryParseItem m_parse_params;
	xr_hash_map<shared_str, RStringVec> m_customize_bones;
	XPROPERTY(IniReadExist, Name="hud_visual")
	shared_str m_attach_hud_visual;
	xr_hash_map<shared_str, RStringVec> m_attach_hud_mirrors;
	XPROPERTY(IniReadExist, Name="anim_pose")
	shared_str m_anim_pose;
	XPROPERTY(IniReadExist, Name="ik_mark")
	shared_str m_ik_mark;
	XPROPERTY(IniReadExist, Name="ik_fingers_mark")
	shared_str m_fingers_mark;
	XPROPERTY(IniReadExist, Name="aim_bone")
	shared_str m_aim_bone;
	XPROPERTY(IniReadExist, Name="fire_bone")
	shared_str FireBone;
	XPROPERTY(IniReadExist, Name = "fire_point")
	Fvector FirePoint = zero_vel;
	XPROPERTY(IniReadExist, Name = "hud_fire_point")
	Fvector HudFirePoint = zero_vel;
	XPROPERTY(IniReadExist, Name="is_silencer")
	bool m_silencer = false;
	XPROPERTY(IniReadExist, Name="is_grenade_launcher")
	bool m_grenade_launcher = false;
	RStringVec m_conflicts;
	xr_hash_map<shared_str, SAttachPlacement> m_attach_placements;

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
