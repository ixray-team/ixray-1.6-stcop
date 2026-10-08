#pragma once
#include "DescRegistry.h"
#include "../hit_immunity.h"

struct SArtefactDesc final
{
	using Registry = TDescRegistry<SArtefactDesc>;

	shared_str m_sParticlesName;

	shared_str TrailLightBone;
	Fcolor m_TrailLightColor = {};
	float m_fTrailLightRange = 0.0f;
	bool m_bLightsEnabled = false;
	bool IdleLightShadow = false;

	CHitImmunity m_ArtefactHitImmunities;

	float m_additional_weight = 0.0f;
	float m_fDegradationRate = 0.0f;
	u8 m_af_rank = 0;
	bool m_bCanSpawnZone = false;

	bool CanBeControlled = false;
	shared_str DetShowParticles;
	shared_str DetHideParticles;
	shared_str DetShowSound;
	shared_str DetHideSound;

	void Load(const shared_str& Section);
};
