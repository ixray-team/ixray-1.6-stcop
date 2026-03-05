#pragma once

#include "Actor.h"
#include "actor_memory.h"

#include "../xrCore/_flags.h"
#include "../xrCore/_vector3d.h"

class CActor;
class CActorMemory;
class CEntityAlive;

namespace Feel
{
	typedef std::pair<CEntityAlive*, double> AutoAimCandidate;
	typedef _vector3<double> Dvector;

	bool PredicateSortTargetEstims(const AutoAimCandidate& t1, const AutoAimCandidate& t2);

	enum eFlagsPickAutoAim
	{
		eFlagsPickAutoAim_NoWeapon = 1<<1,
		eFlagsPickAutoAim_Enemies = 1<<2,
		eFlagsPickAutoAim_NonEnemies = 1<<3
	};

	bool auto_aim_pick_target(CActor* pActor, CActorMemory* pMem, CEntityAlive*& pTarget, flags32 flags);
	void look_at_pos_for_aiming(Fvector& dest, const CEntityAlive* pTarget, float heightFraction);
}