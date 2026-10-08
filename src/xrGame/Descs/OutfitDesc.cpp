#include "StdAfx.h"
#include "OutfitDesc.h"

void SCustomOutfitDesc::Load(const shared_str& Section)
{
	isDisableChangeSkin = READ_IF_EXISTS(pSettings, r_bool, Section, "forbid_change_skin", false);

	if (pSettings->line_exist(Section, "actor_visual"))
	{
		m_ActorVisual = pSettings->r_string(Section, "actor_visual");
	}

	m_ef_equipment_type = pSettings->r_u32(Section, "ef_equipment_type");
	m_full_icon_name = pSettings->r_string(Section, "full_icon_name");
	bIsHelmetAvaliable = !!READ_IF_EXISTS(pSettings, r_bool, Section, "helmet_avaliable", true);

	IsExo = READ_IF_EXISTS(pSettings, r_bool, Section, "is_exo", false);
	IsExoProto = READ_IF_EXISTS(pSettings, r_bool, Section, "is_exo_proto", false);

	if (pSettings->line_exist(Section, "character_portrait"))
	{
		m_character_portrait = pSettings->r_string(Section, "character_portrait");
	}

	PlayerHudSection = READ_IF_EXISTS(pSettings, r_string, Section, "player_hud_section", nullptr);
}
