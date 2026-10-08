#pragma once
#include "DescRegistry.h"

struct SCustomOutfitDesc final
{
	using Registry = TDescRegistry<SCustomOutfitDesc>;

	shared_str m_ActorVisual;
	shared_str m_full_icon_name;
	shared_str m_character_portrait;
	shared_str PlayerHudSection;

	u32 m_ef_equipment_type = 0;

	bool bIsHelmetAvaliable = true;
	bool isDisableChangeSkin = false;
	bool IsExo = false;
	bool IsExoProto = false;

	void Load(const shared_str& Section);
};
