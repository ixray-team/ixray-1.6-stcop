#include "StdAfx.h"
#include "EatableItemDesc.h"
#include "EatableEffectsDesc.h"

void SEatableItemDesc::Load(const shared_str& Section)
{
	XReflect::ReadIni(*this, Section);

	if (m_sUseAnimator.size() && pSettings->line_exist(m_sUseAnimator, "last_use_section"))
	{
		m_sLastUseAnimator = pSettings->r_string(m_sUseAnimator, "last_use_section");
	}

	if (pSettings->line_exist(Section, "eat_portions_num"))
	{
		m_iMaxUses = pSettings->r_s32(Section, "eat_portions_num");
	}
	else
	{
		m_iMaxUses = pSettings->read_if_exists<u8>(Section, "max_uses", 1);
	}

	float EatCondition = pSettings->read_if_exists<float>(Section, "eat_condition", 1);
	m_iMaxUses /= EatCondition;
}

void SEatableEffectsDesc::Load(const shared_str& Section)
{
	Influence.Load(Section);

	for (u8 i = 0; i < (u8)eBoostMaxCount; i++)
	{
		if (pSettings->line_exist(Section, ef_boosters_section_names[i]))
		{
			SBooster& Booster = Boosters.emplace_back();
			Booster.Load(Section, (EBoostParams)i);
		}
	}
}
