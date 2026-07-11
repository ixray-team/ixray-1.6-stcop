#pragma once

#include "Actor.h"
#include "actor_memory.h"

#include "../xrCore/_flags.h"
#include "../xrCore/_vector3d.h"

class CActor;
class CActorMemory;
class CEntityAlive;

class CAutoAim
{
	typedef std::pair<CEntityAlive*, double> AutoAimCandidate;

protected:
	// distance thresholds a-b-c
	float distA = 0.0f;
	float distB = 5.1f;
	float distC = 200.1f;

	// dotp thresholds easy,norm
	double dotpA_n = 0.5;
	double dotpB_n = 0.97;
	double dotpC_n = 0.97;

	double dotpA_e = 0.5;
	double dotpB_e = 0.97;
	double dotpC_e = 0.97;

	double easiness = 1.0;

	float heightFraction = 0.7f;

public:
	void load();

	static bool PredicateSortTargetEstims(const AutoAimCandidate& t1, const AutoAimCandidate& t2);

	bool auto_aim_pick_target(CActor* pActor, CActorMemory* pMem, CEntityAlive*& pTarget);
	void look_at_pos_for_aiming(Fvector& dest, const CEntityAlive* pTarget);
};