#include "stdafx.h"
#include "pch_script.h"
#include "antigas_filter.h"

AntigasFilter::AntigasFilter()
{
	bIsAllowed = false;
	m_FilterProtection.resize(ALife::eHitTypeMax);
	m_FilterDamage.resize(ALife::eHitTypeMax);

	for (int i = 0; i < ALife::eHitTypeMax; i++)
	{
		m_FilterProtection[i] = 0.0f;
		m_FilterDamage[i] = 0.0f;
	}
}

AntigasFilter::~AntigasFilter()
{

}

bool AntigasFilter::IsAllowed()
{
	return bIsAllowed;
}

void AntigasFilter::SetAllowed(bool sate)
{
	bIsAllowed = sate;
}

void AntigasFilter::Load(const char* section)
{
	CInventoryItemObject::Load(section);

	if (!m_flags.test(FUsingCondition)) {
		m_flags.set(FUsingCondition, true);
	}

	SetAllowed(pSettings->read_if_exists<bool>(section, "is_antigas_filter", false));
	if (IsAllowed())
	{
		// immunities
		m_FilterProtection[ALife::eHitTypeBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_protection_burn", 0.0f);
		clamp(m_FilterProtection[ALife::eHitTypeBurn], 0.0f, 1.0f);

		m_FilterProtection[ALife::eHitTypeRadiation] = pSettings->read_if_exists<float>(section, "antigas_filter_protection_radiation", 0.0f);
		clamp(m_FilterProtection[ALife::eHitTypeRadiation], 0.0f, 1.0f);

		m_FilterProtection[ALife::eHitTypeChemicalBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_protection_chemical_burn", 0.0f);
		clamp(m_FilterProtection[ALife::eHitTypeChemicalBurn], 0.0f, 1.0f);

		m_FilterProtection[ALife::eHitTypeLightBurn] = m_FilterProtection[ALife::eHitTypeBurn];
		clamp(m_FilterProtection[ALife::eHitTypeLightBurn], 0.0f, 1.0f);

		// damage
		m_FilterDamage[ALife::eHitTypeBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_coeff_damage_burn", 0.0f);
		clamp(m_FilterDamage[ALife::eHitTypeBurn], 0.0f, 1.0f);

		m_FilterDamage[ALife::eHitTypeRadiation] = pSettings->read_if_exists<float>(section, "antigas_filter_coeff_damage_radiation", 0.0f);
		clamp(m_FilterDamage[ALife::eHitTypeRadiation], 0.0f, 1.0f);

		m_FilterDamage[ALife::eHitTypeChemicalBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_coeff_damage_chemical_burn", 0.0f);
		clamp(m_FilterDamage[ALife::eHitTypeChemicalBurn], 0.0f, 1.0f);

		m_FilterDamage[ALife::eHitTypeLightBurn] = m_FilterProtection[ALife::eHitTypeBurn];
		clamp(m_FilterDamage[ALife::eHitTypeLightBurn], 0.0f, 1.0f);
	}
}

using namespace luabind;
#pragma optimize("s",on)
void AntigasFilter::script_register(lua_State* L)
{
	module(L)
		[
			class_<AntigasFilter, CGameObject>("AntigasFilter")
				.def(constructor<>())
		];
}
