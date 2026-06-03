#include "stdafx.h"
#include "IRestoresOwner.h"

void IRestoresOwner::Load(const char* section)
{
	m_fHealthRestoreSpeed		= pSettings->read_if_exists<float>(section, "health_restore_speed", 0.0f);
	m_fRadiationRestoreSpeed	= pSettings->read_if_exists<float>(section, "radiation_restore_speed", 0.0f);
	m_fSatietyRestoreSpeed		= pSettings->read_if_exists<float>(section, "satiety_restore_speed", 0.0f);
	m_fThirstRestoreSpeed		= pSettings->read_if_exists<float>(section, "thirst_restore_speed", 0.0f);
	m_fPowerRestoreSpeed		= pSettings->read_if_exists<float>(section, "power_restore_speed", 0.0f);
	m_fBleedingRestoreSpeed		= pSettings->read_if_exists<float>(section, "bleeding_restore_speed", 0.0f);
}
