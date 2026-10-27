#pragma once
#include "DescRegistry.h"
#include "../hit_immunity.h"

XREFLECT(IniRead, Owner="CArtefact")
struct SArtefactDesc final
{
	using Registry = TDescRegistry<SArtefactDesc>;

	XPROPERTY(IniReadExist, Name="particles")
	shared_str m_sParticlesName;

	XPROPERTY(IniRead, Name="lights_enabled")
	bool m_bLightsEnabled = false;
	XPROPERTY(IniReadExist, Name="trail_light_bone", If="m_bLightsEnabled")
	shared_str TrailLightBone;
	XPROPERTY(IniRead, Name="trail_light_color", If="m_bLightsEnabled")
	Fcolor m_TrailLightColor = {};
	XPROPERTY(IniRead, Name="trail_light_range", If="m_bLightsEnabled")
	float m_fTrailLightRange = 0.0f;
	XPROPERTY(IniReadExist, Name="idle_light_shadow")
	bool IdleLightShadow = false;

	CHitImmunity m_ArtefactHitImmunities;

	XPROPERTY(IniReadExist, Name="additional_inventory_weight")
	float m_additional_weight = 0.0f;
	XPROPERTY(IniReadExist, Name="degrade_rate")
	float m_fDegradationRate = 0.0f;
	XPROPERTY(IniReadExist, Name="af_rank")
	u8 m_af_rank = 0;
	bool m_bCanSpawnZone = false;

	XPROPERTY(IniReadExist, Name="can_be_controlled")
	bool CanBeControlled = false;
	XPROPERTY(IniReadExist, Name="det_show_particles")
	shared_str DetShowParticles;
	XPROPERTY(IniReadExist, Name="det_hide_particles")
	shared_str DetHideParticles;
	XPROPERTY(IniReadExist, Name="det_show_snd")
	shared_str DetShowSound;
	XPROPERTY(IniReadExist, Name="det_hide_snd")
	shared_str DetHideSound;

	void Load(const shared_str& Section);
};
