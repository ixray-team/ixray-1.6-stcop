#include "StdAfx.h"
#include "PuddleEffects.h"
#include "GamePersistent.h"
#include "ParticlesObject.h"
#include "../xrEngine/GameMtlLib.h"

void PuddleEffects::PlayParticles(const SGameMtlPair& Pair, const Fvector& Position, const Fvector& Normal, CRandom& Randomizer)
{
	if (Pair.CollideParticles.empty())
	{
		return;
	}

	const char* Name = *Pair.CollideParticles[Randomizer.randI(u32(Pair.CollideParticles.size()))];
	auto Particle = Particles::Details::Create(Name, true);
	Fmatrix Transform;
	Transform.identity();
	Transform.k = Normal;
	Fvector::generate_orthonormal_basis(Transform.k, Transform.j, Transform.i);
	Transform.c = Position;
	Particle->UpdateParent(Transform, zero_vel);
	GamePersistent().ps_needtoplay.push_back(Particle);
}
