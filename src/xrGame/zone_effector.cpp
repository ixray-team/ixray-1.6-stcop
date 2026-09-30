#include "StdAfx.h"
#include "zone_effector.h"
#include "Level.h"
#include "../xrEngine/xr_object.h"
#include "Actor.h"
#include "CustomOutfit.h"

namespace
{
	EAuraPostEffectType hit_type_to_aura_type(ALife::EHitType hit_type)
	{
		switch (hit_type)
		{
		case ALife::eHitTypeLightBurn:
		case ALife::eHitTypeBurn:
		case ALife::eHitTypeFireWound:	return EAuraPostEffectType::Fire;
		case ALife::eHitTypeRadiation:	return EAuraPostEffectType::Radiation;
		case ALife::eHitTypeTelepatic:	return EAuraPostEffectType::Psi;
		default:						return EAuraPostEffectType::Chemical;
		}
	}
}

CZoneEffector::CZoneEffector()
{
	m_pActor	= nullptr;
	m_factor	= 0.1f;
	m_object_id	= cInvalidAuraObjectID;
	m_type		= EAuraPostEffectType::Chemical;
}

CZoneEffector::~CZoneEffector()
{
	Stop		();
}

void CZoneEffector::Load(const char* section)
{
	VERIFY2(pSettings->line_exist(section, "pp_eff_name") || pSettings->line_exist(section, "ppe_file"), section);
	m_pp_section			= section;
	r_min_perc				= pSettings->r_float(section,"radius_min");
	r_max_perc				= pSettings->r_float(section,"radius_max");
	VERIFY					(r_min_perc <= r_max_perc);
}

void CZoneEffector::Activate()
{
	CActorAuraPostEffectsBalancer::RegisterEffect	(
		m_type, m_factor, m_object_id, flt_max, m_pp_section);
}

void CZoneEffector::Stop()
{
	if (m_object_id == cInvalidAuraObjectID)
		return;

	CActorAuraPostEffectsBalancer::UnregisterEffect	(m_type, m_object_id, m_pp_section);
	m_object_id	= cInvalidAuraObjectID;
	m_pActor	= nullptr;
}

void CZoneEffector::Update(u32 object_id, float dist, float r, ALife::EHitType hit_type)
{
	float min_r = r * r_min_perc;
	float max_r = r * r_max_perc;

	CObject* obj = Level().CurrentEntity();
	m_pActor = obj != nullptr ? obj->cast_actor() : nullptr;

	if (m_pActor == nullptr || !m_pActor->g_Alive())
	{
		Stop();
		return;
	}

	float protection = 0.f;
	CCustomOutfit* outfit = m_pActor->GetOutfit();
	if (outfit)
	{
		protection = outfit->GetDefHitTypeProtection(hit_type);
	}

	m_factor = ((max_r - dist) / (max_r - min_r)) - protection;
	clamp(m_factor, 0.01f, 1.0f);

	m_object_id	= object_id;
	m_type		= hit_type_to_aura_type(hit_type);

	if (dist < max_r)
	{
		Activate();
	}
	else
	{
		Stop();
	}
}
