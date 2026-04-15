#include "StdAfx.h"
#include "IRestoresOwner.h"

IRestoresOwner::IRestoresOwner()
{
	m_fHealthRestoreSpeed		= 0.f;
	m_fRadiationRestoreSpeed	= 0.f;
	m_fSatietyRestoreSpeed		= 0.f;
	m_fThirstRestoreSpeed		= 0.f;
	m_fPowerRestoreSpeed		= 0.f;
	m_fBleedingRestoreSpeed		= 0.f;
}

IRestoresOwner::~IRestoresOwner()
{
}

void IRestoresOwner::Load(LPCSTR section)
{
	m_fHealthRestoreSpeed		= READ_IF_EXISTS(pSettings, r_float, section, "health_restore_speed", 0.0f);
	m_fRadiationRestoreSpeed	= READ_IF_EXISTS(pSettings, r_float, section, "radiation_restore_speed", 0.0f);
	m_fSatietyRestoreSpeed		= READ_IF_EXISTS(pSettings, r_float, section, "satiety_restore_speed", 0.0f);
	m_fThirstRestoreSpeed		= READ_IF_EXISTS(pSettings, r_float, section, "thirst_restore_speed", 0.0f);
	m_fPowerRestoreSpeed		= READ_IF_EXISTS(pSettings, r_float, section, "power_restore_speed", 0.0f);
	m_fBleedingRestoreSpeed		= READ_IF_EXISTS(pSettings, r_float, section, "bleeding_restore_speed", 0.0f);
}
