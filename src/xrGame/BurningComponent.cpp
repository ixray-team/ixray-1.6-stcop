#include "StdAfx.h"
#include "BurningComponent.h"

#include <algorithm>

#include "entity_alive.h"
#include "Hit.h"
#include "xrMessages.h"

void TBurningComponent::Load(str_c SectionName)
{
	m_burning_sounds_variants.clear();

	xr_string unsplittedPaths = pSettings->r_string(SectionName, "burn_sound");
	if (!unsplittedPaths.empty()) 
	{
		xr_vector<xr_string> paths = unsplittedPaths.RemoveWhitespaces().Split();
		for (xr_string& sound_path : paths)
		{
			m_burning_sounds_variants.emplace_back().create(sound_path.c_str(), st_Effect, sg_SourceType);
		}
	}
	m_burning_fx = pSettings->r_string(SectionName, "burning_fx");
}

void TBurningComponent::AddBurning(float TickDamage, float DamageDecreasing, CEntityAlive* NewInitiator)
{
	CurrentTickDamage = std::max(CurrentTickDamage, TickDamage);
	CurrentDamageDecreasing = std::min(CurrentDamageDecreasing, DamageDecreasing);
	LastInitiator = NewInitiator;
}

void TBurningComponent::Update(u32 dt, CEntityAlive* Owner)
{
	if (!IsBurning())
	{
		CurrentTickDamage = 0.f;
		CurrentDamageDecreasing = FLT_MAX;
		LastInitiator = nullptr;
		return;
	}
	TimeUntilUpdate -= s32(dt);
	while (TimeUntilUpdate <= 0)
	{
		TimeUntilUpdate += 100;
	
		if (OnServer())
		{	
			NET_Packet l_P;
			SHit Hit = SHit(CurrentTickDamage, {0,0,0}, Owner, BI_NONE, {0,0,0}, 0.0f, ALife::eHitTypeLightBurn, 0.0f, false);		
			Hit.GenHeader(GE_HIT, Owner->ID());
			Hit.whoID = Owner->ID();
			Hit.weaponID = Owner->ID();
			Hit.Write_Packet(l_P);
			CEntityAlive::u_EventSend(l_P);
		}
		CurrentTickDamage -= CurrentDamageDecreasing;
		
		if (!m_burning_sounds_variants.empty())
		{
			m_burning_sounds_variants[::Random.randI(m_burning_sounds_variants.size())].play(Owner);
		}
		
		TParticlesPlayer* PPlayer = Owner->GetOrCreateComponent<TParticlesPlayer>();

		u16 play_bone = PPlayer->GetRandomBone();
		if (play_bone != BI_NONE)
		{
			PPlayer->StartParticles(m_burning_fx, play_bone, Fvector().set(0, 1, 0), Owner->ID());
		}
	}
}