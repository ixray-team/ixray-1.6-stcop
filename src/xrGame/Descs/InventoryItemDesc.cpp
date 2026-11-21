#include "StdAfx.h"
#include "InventoryItemDesc.h"
#include "../inventory_item.h"

#include <nlohmann/json.hpp>

static Fmatrix ReadAttachPlacement(const nlohmann::json& Json)
{
	const auto ReadVector = [&Json](const char* Key, const Fvector& Default)
	{
		if (!Json.contains(Key))
		{
			return Default;
		}

		const nlohmann::json& Value = Json[Key];
		return Fvector().set(Value[0].get<float>(), Value[1].get<float>(), Value[2].get<float>());
	};

	const Fvector Rotation = ReadVector("rotation", {0.f, 0.f, 0.f});
	const Fvector Scale = ReadVector("scale", {1.f, 1.f, 1.f});

	Fmatrix Result;
	Result.setXYZ(deg2rad(Rotation.x), deg2rad(Rotation.y), deg2rad(Rotation.z));
	Result.i.mul(Scale.x);
	Result.j.mul(Scale.y);
	Result.k.mul(Scale.z);
	Result.translate_over(ReadVector("position", {0.f, 0.f, 0.f}));
	return Result;
}

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

	m_attach_hud_visual = pSettings->read_if_exists<str_c>(Section, "hud_visual", "");
	m_anim_pose = pSettings->read_if_exists<str_c>(Section, "anim_pose", "");
	m_ik_mark = pSettings->read_if_exists<str_c>(Section, "ik_mark", "");
	m_fingers_mark = pSettings->read_if_exists<str_c>(Section, "ik_fingers_mark", "");
	m_aim_bone = pSettings->read_if_exists<str_c>(Section, "aim_bone", "");
	m_fire_bone = pSettings->read_if_exists<str_c>(Section, "fire_bone", "");
	m_fire_point = pSettings->read_if_exists<Fvector3>(Section, "fire_point", m_fire_point);
	m_hud_fire_point = pSettings->read_if_exists<Fvector3>(Section, "hud_fire_point", m_fire_point);
	m_silencer = pSettings->read_if_exists<bool>(Section, "is_silencer", false);
	m_grenade_launcher = pSettings->read_if_exists<bool>(Section, "is_grenade_launcher", false);

	if (pSettings->line_exist(Section, "attach_hud_mirror"))
	{
		for (const auto& Pair : xr_string(pSettings->r_string(Section, "attach_hud_mirror")).RemoveWhitespaces().Split(','))
		{
			const auto Bones = Pair.Split(':');
			if (Bones.size() == 2)
			{
				m_attach_hud_mirrors[Bones[0].c_str()].push_back(Bones[1].c_str());
			}
		}
	}

	if (pSettings->line_exist(Section, "conflicts"))
	{
		for (const auto& Conflict : xr_string(pSettings->r_string(Section, "conflicts")).RemoveWhitespaces().Split(','))
		{
			m_conflicts.push_back(Conflict.c_str());
		}
	}

	if (!pSettings->line_exist(Section, "customize_bones"))
	{
		return;
	}

	for (const auto& Bone : xr_string(pSettings->r_string(Section, "customize_bones")).RemoveWhitespaces().Split(','))
	{
		if (!pSettings->line_exist(Section, Bone.c_str()))
		{
			continue;
		}

		auto& Attachments = m_customize_bones[Bone.c_str()];
		for (const auto& Attachment : xr_string(pSettings->r_string(Section, Bone.c_str())).RemoveWhitespaces().Split(','))
		{
			Attachments.push_back(Attachment.c_str());
		}
	}

	string_path Path;
	string_path FileName = {};
	xr_strconcat(FileName, "attachments\\", Section.c_str(), ".json");
	FS.update_path(Path, "$game_config$", FileName);

	if (!FS.exist(Path))
	{
		return;
	}

	IReader* Reader = FS.r_open(Path);
	const xr_string JsonString((const char*)Reader->pointer(), Reader->length());
	FS.r_close(Reader);

	const nlohmann::json Json = nlohmann::json::parse(JsonString.c_str(), nullptr, false);
	if (Json.is_discarded())
	{
		Msg("! Invalid attachment placement file: %s", Path);
		return;
	}

	for (const auto& [Name, Value] : Json.items())
	{
		SAttachPlacement& Placement = m_attach_placements[Name.c_str()];
		if (Value.contains("world"))
		{
			Placement.m_world = ReadAttachPlacement(Value["world"]);
		}
		if (Value.contains("hud"))
		{
			Placement.m_hud = ReadAttachPlacement(Value["hud"]);
		}
	}
}
