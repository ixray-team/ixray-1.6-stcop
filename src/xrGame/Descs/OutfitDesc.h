#pragma once
#include "DescRegistry.h"

XREFLECT(IniRead)
struct SCustomOutfitDesc final
{
	using Registry = TDescRegistry<SCustomOutfitDesc>;

	XPROPERTY(IniReadExist, Name="actor_visual")
	shared_str m_ActorVisual;
	XPROPERTY(IniRead, Name="full_icon_name")
	shared_str m_full_icon_name;
	XPROPERTY(IniReadExist, Name="character_portrait")
	shared_str m_character_portrait;
	XPROPERTY(IniReadExist, Name="player_hud_section")
	shared_str PlayerHudSection;

	XPROPERTY(IniRead, Name="ef_equipment_type")
	u32 m_ef_equipment_type = 0;

	XPROPERTY(IniReadExist, Name="helmet_avaliable")
	bool bIsHelmetAvaliable = true;
	XPROPERTY(IniReadExist, Name="forbid_change_skin")
	bool isDisableChangeSkin = false;
	XPROPERTY(IniReadExist, Name="is_exo")
	bool IsExo = false;
	XPROPERTY(IniReadExist, Name="is_exo_proto")
	bool IsExoProto = false;

	void Load(const shared_str& Section);
};
