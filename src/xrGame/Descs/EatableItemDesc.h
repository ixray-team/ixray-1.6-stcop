#pragma once
#include "DescRegistry.h"

XREFLECT(IniRead)
struct SEatableItemDesc final
{
	using Registry = TDescRegistry<SEatableItemDesc>;

	XPROPERTY(IniReadExist, Name="animator_sect")
	shared_str m_sUseAnimator;
	shared_str m_sLastUseAnimator;
	XPROPERTY(IniReadExist, Name="use_text")
	shared_str UseText = "st_use";

	u8 m_iMaxUses = 1;
	XPROPERTY(IniReadExist, Name="consume_charge_on_use")
	bool m_bConsumeChargeOnUse = true;

	void Load(const shared_str& Section);
};
