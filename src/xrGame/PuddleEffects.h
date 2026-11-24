#pragma once

struct SGameMtlPair;

namespace PuddleEffects
{
	void PlayParticles(const SGameMtlPair& Pair, const Fvector& Position, const Fvector& Normal, CRandom& Randomizer);
}
