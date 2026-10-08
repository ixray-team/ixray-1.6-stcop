#include "StdAfx.h"
#include "ArtefactDesc.h"

void SArtefactDesc::Load(const shared_str& Section)
{
	if (!Section.size())
	{
		return;
	}

	m_sParticlesName = READ_IF_EXISTS(pSettings, r_string, Section, "particles", nullptr);

	m_bLightsEnabled = !!pSettings->r_bool(Section, "lights_enabled");
	if (m_bLightsEnabled)
	{
		m_TrailLightColor = pSettings->r_fcolor(Section, "trail_light_color");
		m_fTrailLightRange = pSettings->r_float(Section, "trail_light_range");
		TrailLightBone = READ_IF_EXISTS(pSettings, r_string, Section, "trail_light_bone", nullptr);
	}

	IdleLightShadow = READ_IF_EXISTS(pSettings, r_bool, Section, "idle_light_shadow", false);

	const char* HitAbsorbationSect = pSettings->r_string(Section, "hit_absorbation_sect");
	if (pSettings->section_exist(HitAbsorbationSect))
	{
		m_ArtefactHitImmunities.LoadImmunities(HitAbsorbationSect, pSettings);
	}

	m_bCanSpawnZone = !!pSettings->line_exist("artefact_spawn_zones", Section);
	m_af_rank = READ_IF_EXISTS(pSettings, r_u8, Section, "af_rank", 0);
	m_additional_weight = READ_IF_EXISTS(pSettings, r_float, Section, "additional_inventory_weight", 0.0f);
	m_fDegradationRate = READ_IF_EXISTS(pSettings, r_float, Section, "degrade_rate", 0.0f);

	CanBeControlled = READ_IF_EXISTS(pSettings, r_bool, Section, "can_be_controlled", false);
	DetShowParticles = READ_IF_EXISTS(pSettings, r_string, Section, "det_show_particles", nullptr);
	DetHideParticles = READ_IF_EXISTS(pSettings, r_string, Section, "det_hide_particles", nullptr);
	DetShowSound = READ_IF_EXISTS(pSettings, r_string, Section, "det_show_snd", nullptr);
	DetHideSound = READ_IF_EXISTS(pSettings, r_string, Section, "det_hide_snd", nullptr);
}
