#include "StdAfx.h"
#include "ControllerAutoaim.h"
#include "../xrEngine/CameraBase.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../xrEngine/xr_collide_form.h"

void CAutoAim::load()
{
	// distance thresholds a-b-c
	distA = 0.0f;
	distB = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "distance_b", 5.1f);
	distC = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "distance_c", 200.1f);

	// dotp thresholds easy,norm
	dotpA_n = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "dotp_a_norm", 0.5f);
	dotpB_n = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "dotp_b_norm", 0.97f);
	dotpC_n = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "dotp_c_norm", 0.97f);

	dotpA_e = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "dotp_a_easy", 0.5f);
	dotpB_e = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "dotp_b_easy", 0.97f);
	dotpC_e = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "dotp_c_easy", 0.97f);

	// lerp easiness
	easiness = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "easiness", 1.0f);

	// What Y to aim at
	heightFraction = READ_IF_EXISTS(pSettings, r_float, "auto_aiming", "height_fraction", 0.7f);
}

void CAutoAim::look_at_pos_for_aiming(Fvector& dest, const CEntityAlive* pAim)
{
	Fbox bbox = pAim->CFORM()->getBBox();
	Fvector size;
	bbox.getsize(size);

	bbox.getcenter(dest);
	dest.y +=  -size.y/2.0f + (size.y)*heightFraction;
	pAim->XFORM().transform_tiny(dest);
}

bool CAutoAim::auto_aim_pick_target(CActor* pActor, CActorMemory* pMem, CEntityAlive*& pTarget)
{
	R_ASSERT(pMem);
	if (pMem->feel_visible.size() == 0)
	{
		return false;
	}

	double dotpA = dotpA_e + (dotpA_n - dotpA_e) * (1.0 - easiness);
	double dotpB = dotpB_e + (dotpB_n - dotpB_e) * (1.0 - easiness);
	double dotpC = dotpC_e + (dotpC_n - dotpC_e) * (1.0 - easiness);

	std::vector<AutoAimCandidate> targets;

	for (xr_vector<Feel::Vision::feel_visible_Item>::iterator it = pMem->feel_visible.begin(); it != pMem->feel_visible.end(); ++it)
	{
		if (it->fuzzy <= 0.0f)
		{
			continue;
		}

		CEntityAlive* pAlive = it->O->cast_entity_alive();
		if (!pAlive || !pAlive->g_Alive() || !pAlive->is_relation_enemy(pActor))
		{
			continue;
		}

		AutoAimCandidate e;
		e.first = pAlive;

		// Check distance
		float distance = pAlive->Position().distance_to(pActor->Position());
		if (distance >= distC)
		{
			continue;
		}
		// Check angle (camera direction, and direction from camera pos to the target's head)
		CCameraBase* pCam = pActor->cam_Active();
		Fvector camDirF = pCam->Direction();
		Fvector camPosF = pCam->Position();
		Fvector posInAimF;
		look_at_pos_for_aiming(posInAimF, pAlive);

		// Use doubles here to have more precision
		Dvector camDir, camPos, posInAim;
		camDir.set(camDirF);
		camPos.set(camPosF);
		posInAim.set(posInAimF);

		Dvector dirToAim;
		dirToAim.sub(posInAim, camPos);

		camDir.normalize();
		dirToAim.normalize();
		e.second = dirToAim.dotproduct(camDir);

		// Calculate thresholds for dotps on misc distances.. we have a-b(sin func) b-c(linear func)
		// Check min angle diff. The more the distance the less the angle diff that is acceptable
		double distR, threshDotP;
		if (distance < distB)
		{
			distR = (distance - distA) / (distB - distA);

			// sin 1st quarter
			threshDotP = std::sin(PI_DIV_2 * distR);
			threshDotP = dotpA + (dotpB - dotpA) * threshDotP;
		}
		else if (distance < distC)
		{
			// Linear func
			distR = (distance - distB) / (distC - distB);
			threshDotP = dotpB + (dotpC - dotpB) * distR;
		}
		
		if (e.second < threshDotP)
		{
			continue;
		}
		targets.push_back(e);
	}

	std::sort(targets.begin(), targets.end(), CAutoAim::PredicateSortTargetEstims);
	if (targets.size() > 0)
	{
		AutoAimCandidate& e = targets.front();
		pTarget = e.first;
		return true;
	}

	return false;
}


bool CAutoAim::PredicateSortTargetEstims(const AutoAimCandidate& t1, const AutoAimCandidate& t2)
{
	return (t1.second > t2.second);
}