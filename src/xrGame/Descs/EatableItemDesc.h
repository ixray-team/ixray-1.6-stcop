#pragma once
#include "DescRegistry.h"

struct SEatableItemDesc final
{
	using Registry = TDescRegistry<SEatableItemDesc>;

	shared_str m_sUseAnimator;
	shared_str m_sLastUseAnimator;
	shared_str UseText;

	u8 m_iMaxUses = 1;
	bool m_bConsumeChargeOnUse = true;

	void Load(const shared_str& Section);
};
