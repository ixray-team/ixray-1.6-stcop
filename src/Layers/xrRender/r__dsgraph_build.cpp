#include "stdafx.h"

#include "FHierrarhyVisual.h"
#include "SkeletonCustom.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/IRenderable.h"

#include "FLOD.h"
#include "FVisual.h"
#include "ParticleGroup.h"
#include "FTreeVisual.h"
#include "ParticleEffect.h"
#include "newproject_dsgraph_constants.h"

using namespace R_dsgraph;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Scene graph actual insertion and sorting ////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
float r_ssaDISCARD;
float r_ssaDONTSORT;
float r_ssaLOD_A;
float r_ssaLOD_B;
float r_ssaLOD_MU0, r_ssaLOD_MU1, r_ssaLOD_MU2, r_ssaLOD_MU3, r_ssaLOD_MU4;
float r_ssaGLOD_start;
float r_ssaGLOD_end;
float r_ssaHZBvsTEX;

// Aproximate, adjusted by fov, distance from camera to position (For right work when looking though binoculars and scopes)
ICF float GetDistFromCamera(const Fvector& from_position)
{
	float distance = Device.vCameraPosition.distance_to(from_position);
	float fov_K = BASE_FOV / Device.fFOV;
	float adjusted_distane = distance / fov_K;

	return adjusted_distane;
}

ICF bool IsValuableToRender(dxRender_Visual* pVisual, bool isStatic, bool sm, Fmatrix& transform_matrix, bool ignore_optimize = false)
{
	if (ignore_optimize)
		return true;

	int opt_level = isStatic ? opt_static : opt_dynamic;
	if (opt_level < 1)
	{
		return true;
	}

	float sphere_volume = pVisual->getVisData().sphere.volume();
	Fvector pos = pVisual->vis.sphere.P;

	if (!isStatic)
	{
		transform_matrix.transform_tiny(pos, pVisual->vis.sphere.P);
	}

	float adjusted_distance = GetDistFromCamera(pos);

	// Настройки для статических и динамических объектов
	static Fvector4 static_sizes[12] =
	{
		o_optimize_static_l1_size, o_optimize_static_l2_size, o_optimize_static_l3_size,
		o_optimize_static_l4_size, o_optimize_static_l5_size, o_optimize_static_l6_size,
		o_optimize_static_l7_size, o_optimize_static_l8_size, o_optimize_static_l9_size,
		o_optimize_static_l10_size, o_optimize_static_l11_size, o_optimize_static_l12_size
	};
	static Fvector4 static_dists[12] =
	{
		o_optimize_static_l1_dist, o_optimize_static_l2_dist, o_optimize_static_l3_dist,
		o_optimize_static_l4_dist, o_optimize_static_l5_dist, o_optimize_static_l6_dist,
		o_optimize_static_l7_dist, o_optimize_static_l8_dist, o_optimize_static_l9_dist,
		o_optimize_static_l10_dist, o_optimize_static_l11_dist, o_optimize_static_l12_dist
	};
	static Fvector4 dynamic_sizes[5] =
	{
		o_optimize_dynamic_l1_size, o_optimize_dynamic_l2_size, o_optimize_dynamic_l3_size,
		o_optimize_dynamic_l4_size, o_optimize_dynamic_l5_size
	};
	static Fvector4 dynamic_dists[5] =
	{
		o_optimize_dynamic_l1_dist, o_optimize_dynamic_l2_dist, o_optimize_dynamic_l3_dist,
		o_optimize_dynamic_l4_dist, o_optimize_dynamic_l5_dist
	};

	auto CheckLevelLabmda = [](Fvector4* sizes, Fvector4* dists, int count, int opt_level, float sphere_volume, float adjusted_distance) -> bool
	{
		for (int i = 0; i < count; ++i)
		{
			Fvector4 sz = sizes[i];
			Fvector4 ds = dists[i];

			float level_size = sz.x;
			float level_dist = ds.x;

			// Определяем компоненту по opt_level
			switch (opt_level)
			{
				case 2: level_size = sz.y; level_dist = ds.y; break;
				case 3: level_size = sz.z; level_dist = ds.z; break;
				case 4: level_size = sz.w; level_dist = ds.w; break;
			}

			if (sphere_volume < level_size && adjusted_distance > level_dist)
			{
				return false;
			}
		}
		return true;
	};

	if (sm && ps_r__common_flags.test(RFLAG_OPT_SHAD_GEOM))
	{
		if (sphere_volume < 50000.f && adjusted_distance > 160.f)
		{
			return false;
		}

		if (!CheckLevelLabmda(static_sizes, static_dists, std::size(static_sizes), opt_level, sphere_volume, adjusted_distance))
		{
			return false;
		}
	}

	if (isStatic)
	{
		return CheckLevelLabmda(static_sizes, static_dists, std::size(static_sizes), opt_level, sphere_volume, adjusted_distance);
	}

	return CheckLevelLabmda(dynamic_sizes, dynamic_dists, std::size(dynamic_sizes), opt_level, sphere_volume, adjusted_distance);
}

ICF R_dsgraph::_NormalItem MakeNormalItem(float ssa, dxRender_Visual* pVisual)
{
	R_dsgraph::_NormalItem I = { ssa, pVisual->vis.sphere.R, pVisual, nullptr, 0, 0, 0, 0 };

	if (MT_NORMAL != pVisual->Type)
		return I;

	Fvisual* V = (Fvisual*)pVisual;
	IRender_Mesh* M = V;

#if (RENDER==R_R2) || (RENDER==R_R4)
	if (V->m_fast && RImplementation.phase == CRender::PHASE_SMAP)
		M = V->m_fast;
#endif

	I.geom		= M->rm_geom._get();
	I.vBase		= M->vBase;
	I.vCount	= M->vCount;
	I.iBase		= M->iBase;
	I.pCount	= M->dwPrimitives;

	return I;
}

ICF	float CalcSSA(float& distSQ, Fvector& C, dxRender_Visual* V)
{
	float R = V->vis.sphere.R + 0;
	distSQ = Device.vCameraPosition.distance_to_sqr(C) + EPS;
	return	R / distSQ;
}

ICF	float CalcSSA(float& distSQ, Fvector& C, float R)
{
	distSQ = Device.vCameraPosition.distance_to_sqr(C) + EPS;
	return	R / distSQ;
}

void R_dsgraph_structure::r_dsgraph_insert_dynamic(dxRender_Visual* pVisual, Fvector& Center, bool Force)
{
	PROF_EVENT("R_dsgraph_structure::r_dsgraph_insert_dynamic")
	CRender& RI = RImplementation;

	if (pVisual->vis.marker == RI.marker)	
	{
		return;
	}

	pVisual->vis.marker = RI.marker;

#if RENDER==R_R1
	if (RI.o.vis_intersect && (pVisual->vis.accept_frame != Device.dwFrame)) return;
	pVisual->vis.accept_frame = Device.dwFrame;
#endif

	float distSQ			;
	float SSA				=	CalcSSA		(distSQ,Center,pVisual);
	if (SSA<=r_ssaDISCARD && !Force)		return;

	// Distortive geometry should be marked and R2 special-cases it
	// a) Allow to optimize RT order
	// b) Should be rendered to special distort buffer in another pass

	if (!pVisual->shader._get()) return;

	bool bHUD = !!RI.val_bHUD;

	if (auto pParticle = pVisual->dcast_ParticleCustom())
	{
		if (pParticle->GetHudMode())
		{
			bHUD = true;
		}
	}

	ShaderElement* sh_d = pVisual->shader->E[4] ? &*pVisual->shader->E[4] : nullptr;

	if (RImplementation.o.distortion && sh_d && sh_d->flags.bDistort && pmask[sh_d->flags.iPriority / 2] && !psDeviceFlags.test(rsClearBB))
	{
		mapSorted_T& test = bHUD ? mapHUDDistort : mapDistort;
		mapSorted_Node* N = test.insertInAnyWay(distSQ);

		N->val.ssa = SSA;
		N->val.pObject = RI.val_pObject;
		N->val.pVisual = pVisual;
		N->val.Matrix = *RI.val_pTransform;
		N->val.se = sh_d;		// 4=L_special
	}

	if (sh_d && sh_d->flags.bScopeMask && pmask[0]) 
	{
		mapHUDScopeMask.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
	}

	ShaderElement* sh = RImplementation.rimp_select_sh_dynamic(pVisual, distSQ, bHUD);

	if (!sh || RI.val_bInvisible)
	{
		return;
	}

	// UI rendering
	if (RI.val_bUI)
	{
		if (sh->flags.bStrictB2F || sh->flags.iPriority > 1)
		{
			mapUISorted.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
		}
		else
		{
			mapUI.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
#if RENDER!=R_R1
			if (sh->flags.bEmissive)
			{
				mapUIEmissive.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
			}
#endif	//	RENDER!=R_R1
		}

		return;
	}

	if (pmask[sh->flags.iPriority / 2])
	{
		// HUD rendering
		if (bHUD)
		{
			if (sh->flags.bStrictB2F || sh->flags.iPriority > 1)
			{
				mapHUDSorted.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
			}
			else
			{
				mapHUD.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
#if RENDER!=R_R1
				if (sh->flags.bEmissive)
				{
					mapHUDEmissive.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
				}
#endif	//	RENDER!=R_R1
			}
			return;
		}

		// Shadows registering
#if RENDER==R_R1
		_MatrixItem item = { SSA,RI.val_pObject,pVisual,*RI.val_pTransform };
		RI.L_Shadows->add_element(item);
#endif

		// strict-sorting selection
		if (sh->flags.bStrictB2F || pVisual->dcast_ParticleCustom())
		{
			mapSorted.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
			return;
		}

#if RENDER!=R_R1
		if (sh->flags.bEmissive)
		{
			mapEmissive.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
		}

		if (sh->flags.bWmark && pmask[2])
		{
			mapWmark.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
			return;
		}
#endif
	}

	for (u32 iPass = 0; iPass < sh->passes.size(); ++iPass)
	{
		// the most common node
		if (sh->passes[iPass] == nullptr)
		{
			continue;
		}

		SPass& pass = *sh->passes[iPass];
		u32 iPriority = pass.iPriority != u8(-1) ? pass.iPriority : sh->flags.iPriority;

		if (!pmask[iPriority / 2])
		{
			continue;
		}

		mapMatrix_T& map = mapMatrixPasses[iPriority / 2][iPass];
		
#ifdef USE_RESOURCE_DEBUGGER
	#ifdef USE_DX11
		mapMatrixVS::TNode*			Nvs		= map.insert		(pass.vs);
		mapMatrixGS::TNode*			Ngs		= Nvs->val.insert	(pass.gs);
		mapMatrixPS::TNode*			Nps		= Ngs->val.insert	(pass.ps);
	#else //USE_DX11
		mapMatrixVS::TNode*			Nvs		= map.insert		(pass.vs);
		mapMatrixPS::TNode*			Nps		= Nvs->val.insert	(pass.ps);
	#endif
#else
	#ifdef USE_DX11
		mapMatrixVS::TNode*			Nvs		= map.insert		(&*pass.vs);
		mapMatrixGS::TNode*			Ngs		= Nvs->val.insert	(pass.gs->gs);
		mapMatrixPS::TNode*			Nps		= Ngs->val.insert	(pass.ps->ps);
	#else //USE_DX11
		mapMatrixVS::TNode*			Nvs		= map.insert		(pass.vs->vs);
		mapMatrixPS::TNode*			Nps		= Nvs->val.insert	(pass.ps->ps);
	#endif
#endif

#ifdef USE_DX11
#	ifdef USE_RESOURCE_DEBUGGER
		Nps->val.hs = pass.hs;
		Nps->val.ds = pass.ds;
		mapMatrixCS::TNode*			Ncs		= Nps->val.mapCS.insert	(pass.constants._get());
#	else
		Nps->val.hs = pass.hs->sh;
		Nps->val.ds = pass.ds->sh;
		mapMatrixCS::TNode*			Ncs		= Nps->val.mapCS.insert	(pass.constants._get());
#	endif
#else
		mapMatrixCS::TNode*			Ncs		= Nps->val.insert	(pass.constants._get());
#endif
		mapMatrixStates::TNode*		Nstate	= Ncs->val.insert	(pass.state->state);
		mapMatrixTextures::TNode*	Ntex	= Nstate->val.insert(pass.T._get());

		Ntex->val.visuals.push_back({ SSA, RI.val_pObject, pVisual, *RI.val_pTransform });
	}
}

void R_dsgraph_structure::r_dsgraph_insert_static(dxRender_Visual* pVisual)
{
	PROF_EVENT("R_dsgraph_structure::r_dsgraph_insert_static")
	CRender& RI = RImplementation;

	if (pVisual->vis.marker == RI.marker)	
	{
		return;
	}

	pVisual->vis.marker = RI.marker;

#if RENDER==R_R1
	if (RI.o.vis_intersect && (pVisual->vis.accept_frame != Device.dwFrame))
	{
		return;
	}

	pVisual->vis.accept_frame = Device.dwFrame;
#endif

	float distSQ;

	float SSA = CalcSSA(distSQ, pVisual->vis.sphere.P, pVisual);
	if (SSA <= r_ssaDISCARD)
	{
		return;
	}

	// Distortive geometry should be marked and R2 special-cases it
	// a) Allow to optimize RT order
	// b) Should be rendered to special distort buffer in another pass
	if (!pVisual->shader._get()) return;
	ShaderElement* sh_d = pVisual->shader->E[4] ? &*pVisual->shader->E[4] : nullptr;

	if (RImplementation.o.distortion && sh_d && sh_d->flags.bDistort && pmask[sh_d->flags.iPriority/2] && !psDeviceFlags.test(rsClearBB))
	{
		mapDistort.insertInAnyWay(distSQ, { SSA, nullptr, pVisual, Fidentity, sh_d });
	}

	// Select shader
	ShaderElement* sh = RImplementation.rimp_select_sh_static(pVisual, distSQ);
	if (!sh) 
	{
		return;
	}

	if (pmask[sh->flags.iPriority / 2])
	{
		// strict-sorting selection
		if (sh->flags.bStrictB2F)
		{
			mapSorted.insertInAnyWay(distSQ, { SSA, nullptr, pVisual, Fidentity, sh });
			return;
		}

#if RENDER!=R_R1
		if (sh->flags.bEmissive)
		{
			mapEmissive.insertInAnyWay(distSQ, { SSA, nullptr, pVisual, Fidentity, sh_d });
		}

		if (sh->flags.bWmark && pmask[2])
		{
			mapWmark.insertInAnyWay(distSQ, { SSA, nullptr, pVisual, Fidentity, sh });
			return;
		}
#endif

		if (val_feedback && counter_S == val_feedback_breakp)
		{
			val_feedback->rfeedback_static(pVisual);
		}

		counter_S++;
	}

	for (u32 iPass = 0; iPass < sh->passes.size(); ++iPass)
	{
		if (sh->passes[iPass] == nullptr)
		{
			continue;
		}

		SPass& pass = *sh->passes[iPass];
		u32 iPriority = pass.iPriority != u8(-1) ? pass.iPriority : sh->flags.iPriority;

		if (!pmask[iPriority / 2])
		{
			continue;
		}

		mapNormal_T& map = mapNormalPasses[iPriority / 2][iPass];

#ifdef USE_DX11
		mapNormalVS::TNode*			Nvs		= map.insert		(&*pass.vs);
		mapNormalGS::TNode*			Ngs		= Nvs->val.insert	(pass.gs->gs);
		mapNormalPS::TNode*			Nps		= Ngs->val.insert	(pass.ps->ps);
#	else //USE_DX11
		mapNormalVS::TNode*			Nvs		= map.insert		(pass.vs->vs);
		mapNormalPS::TNode*			Nps		= Nvs->val.insert	(pass.ps->ps);
#	endif

#ifdef USE_DX11
		Nps->val.hs = pass.hs->sh;
		Nps->val.ds = pass.ds->sh;
		mapNormalCS::TNode*			Ncs		= Nps->val.mapCS.insert	(pass.constants._get());
#else
		mapNormalCS::TNode*			Ncs		= Nps->val.insert	(pass.constants._get());
#endif
		mapNormalStates::TNode*		Nstate	= Ncs->val.insert	(pass.state->state);
		mapNormalTextures::TNode*	Ntex	= Nstate->val.insert(pass.T._get());

		Ntex->val.push_back(MakeNormalItem(SSA, pVisual));
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
void R_dsgraph_structure::add_leafs_Dynamic(dxRender_Visual *pVisual, bool IgnoreObject, bool Force)
{
	if (nullptr == pVisual)
	{
		return;
	}

	if (!IsValuableToRender(pVisual, false, phase == CRender::PHASE_SMAP, *val_pTransform, IgnoreObject))
	{
		return;
	}
	
	// Visual is 100% visible - simply add it
	switch (pVisual->Type)
	{
	case MT_PARTICLE_GROUP:
		{
			//PROF_EVENT("CRender::add_leafs_Dynamic: MT_PARTICLE_GROUP")
			// Add all children, doesn't perform any tests
			PS::CParticleGroup* pG = (PS::CParticleGroup*)pVisual;
			xrCriticalSectionGuard guard(pG->onframe_lock);
			for (PS::CParticleGroup::SItem& item : pG->items)
			{
				if (item.root_effect)
					add_leafs_Dynamic(item.root_effect);

				xrCriticalSectionGuard guard(item.childs_cs);
				for (dxRender_Visual* pEffect : item.children_related)
					add_leafs_Dynamic(pEffect);

				for (dxRender_Visual* pEffect : item.children_free)
					add_leafs_Dynamic(pEffect);
			}
		}
		return;
	case MT_HIERRARHY:
		{
			//PROF_EVENT("CRender::add_leafs_Dynamic: MT_HIERRARHY")
			// Add all children, doesn't perform any tests
			FHierrarhyVisual* pV = (FHierrarhyVisual*)pVisual;
			for (dxRender_Visual* V : pV->children)
			{
				add_leafs_Dynamic(V, IgnoreObject, Force);
			}
		}
		return;
	case MT_SKELETON_ANIM:
	case MT_SKELETON_RIGID:
		{
			//PROF_EVENT("CRender::add_leafs_Dynamic: MT_SKELETON")
			// Add all children, doesn't perform any tests
			CKinematics * pV = (CKinematics*)pVisual;
			bool _use_lod = false;
			if (pV->m_lod)				
			{
				Fvector Tpos; float D;
				val_pTransform->transform_tiny(Tpos, pV->vis.sphere.P);
				float ssa =	CalcSSA(D,Tpos,pV->vis.sphere.R/2.f);// assume dynamics never consume full sphere
				if (ssa<r_ssaLOD_A)
					_use_lod= true;
			}
			if (_use_lod)
			{
				add_leafs_Dynamic(pV->m_lod, IgnoreObject, Force);
			}
			else
			{
#if RENDER==R_R1
				pV->CalculateBones			(true);
				pV->CalculateWallmarks		();		//. bug?
#endif
				for (dxRender_Visual* V : pV->children)
				{
					add_leafs_Dynamic(V, IgnoreObject, Force);
				}
			}
		}
		return;
	default:
		{
			//PROF_EVENT("CRender::add_leafs_Dynamic: Default")
			// General type of visual
			// Calculate distance to it's center
			Fvector Tpos;
			val_pTransform->transform_tiny(Tpos, pVisual->vis.sphere.P);
			r_dsgraph_insert_dynamic(pVisual,Tpos, Force);
		}
		return;
	}
}

void add_leafs_Static(xr_vector<dxRender_Visual*>& children);
ICF void r_dsgraph_insert_static_lod(dxRender_Visual* pVisual)
{
	CRender& RI = RImplementation;
	FLOD* pV = (FLOD*)pVisual;
	float D; float ssa = CalcSSA(D, pV->vis.sphere.P, pV);
	ssa *= pV->lod_factor;
	if (ssa < r_ssaLOD_A)
	{
		if (ssa < r_ssaDISCARD)	return;
		mapLOD_Node* N = RI.mapLOD.insertInAnyWay(D);
		N->val.ssa = ssa;
		N->val.pVisual = pVisual;
	}
#if RENDER!=R_R1
	if (ssa > r_ssaLOD_B || RI.phase == CRender::PHASE_SMAP)
#else
	if (ssa > r_ssaLOD_B)
#endif
	{
		// Add all children, doesn't perform any tests
		add_leafs_Static(pV->children);
	}
}

//Seakad: лучше работает
IC float distance_to_aabb(const Fvector& point,const Fbox& box)
{
    float dx = 0.0f;
    float dy = 0.0f;
    float dz = 0.0f;

    // X
    if (point.x < box.min.x)
        dx = box.min.x - point.x;
    else if (point.x > box.max.x)
        dx = point.x - box.max.x;

    // Y
    if (point.y < box.min.y)
        dy = box.min.y - point.y;
    else if (point.y > box.max.y)
        dy = point.y - box.max.y;

    // Z
    if (point.z < box.min.z)
        dz = box.min.z - point.z;
    else if (point.z > box.max.z)
        dz = point.z - box.max.z;

    return _sqrt(dx * dx + dy * dy + dz * dz);
}

IC FMUMeshLOD* SelectLOD(float D, const xr_vector<dxRender_Visual*>& V)
{
	VERIFY(!V.empty());
	switch (V.size())
	{
		case 5:
		{
			if (D >= r_ssaLOD_MU3 && D < (r_ssaLOD_MU4/(ps_r__LOD_MU4_discard * 0.001f)))
			{
				return (FMUMeshLOD*)V[4];
			}
		}
		case 4:
		{
			if (D >= r_ssaLOD_MU2 && D < r_ssaLOD_MU3)
			{
				return (FMUMeshLOD*)V[3];
			}
		}
		case 3:
		{
			if (D >= r_ssaLOD_MU1 && D < r_ssaLOD_MU2)
			{
				return (FMUMeshLOD*)V[2];
			}
		}
		case 2:
		{
			if (D >= r_ssaLOD_MU0 && D < r_ssaLOD_MU1)
			{
				return (FMUMeshLOD*)V[1];
			}
		}
		case 1:
		{
			if (D < r_ssaLOD_MU0)
			{
				return (FMUMeshLOD*)V.at(0);
			}
		}
	}
	return nullptr;
}

void add_leafs_Static(xr_vector<dxRender_Visual*>& children)
{
	//PROF_EVENT("add_leafs_Static")
	CRender& RI = RImplementation;
	float fog_distance = g_pGamePersistent->Environment().CurrentEnv->fog_distance;
	for(dxRender_Visual* pVisual : children)
	{
		vis_data& vis = pVisual->vis;

#if RENDER!=R_R1
		if (RI.phase == CRender::PHASE_NORMAL)
#endif
		if (!RI.HOM.visible(vis))
			continue;

		if (!pVisual->IsIgnoreOptimize && !IsValuableToRender(pVisual, true, RI.phase == CRender::PHASE_SMAP, *RI.val_pTransform))
			continue;

		if (Device.vCameraPosition.distance_to_sqr(vis.sphere.P) > _sqr(fog_distance + vis.sphere.R))
			continue;

		float D = distance_to_aabb(Device.vCameraPosition,pVisual->vis.box);

		// Visual is 100% visible - simply add it
		switch (pVisual->Type)
		{
			case MT_HIERRARHY:
			{
				// Add all children, doesn't perform any tests
				add_leafs_Static(static_cast<FHierrarhyVisual*>(pVisual)->children);
			}continue;
			case MT_LOD:
			{
				r_dsgraph_insert_static_lod(pVisual);
			}continue;
			case MT_MESH_LODS:
			{
				FMUMeshLOD* SelectedLOD = SelectLOD(D, static_cast<FMUMeshLODs*>(pVisual)->children);
				if(SelectedLOD)
				{
					add_leafs_Static(SelectedLOD->children);
				}
				continue;
			}
			case MT_TREE_PM:
			case MT_TREE_ST:
			{
				//PROF_EVENT("CRender::add_leafs_Static: MT_TREE")
				// General type of visual
				RI.r_dsgraph_insert_static(pVisual);
			}continue;
			default:
			{
				//PROF_EVENT("CRender::add_leafs_Static: Default")
				// General type of visual
				RI.r_dsgraph_insert_static(pVisual);
			}continue;
		}
	}
}

void R_dsgraph_structure::add_Static(dxRender_Visual *pVisual, u32 planes)
{
	//PROF_EVENT("add_Static")
	if (!pVisual->IsIgnoreOptimize && !IsValuableToRender(pVisual, true, phase == CRender::PHASE_SMAP, *val_pTransform))
	{
		return;
	}

	// Check frustum visibility and calculate distance to visual's center
	EFC_Visible	VIS;
	vis_data& vis = pVisual->vis;
	if (pVisual->Type == MT_LOD || pVisual->Type == MT_LOD1 || pVisual->Type == MT_LOD2 || pVisual->Type == MT_LOD3 || pVisual->Type == MT_LOD4)
	{
		VIS = View->testAABB	(vis.box.data(),planes);
	}
	else
	{
		VIS = View->testSAABB	(vis.sphere.P,vis.sphere.R,vis.box.data(),planes);
	}

	if (fcvNone==VIS)		
		return;
#if RENDER!=R_R1
	if(phase==CRender::PHASE_NORMAL)
#endif
	if (!RImplementation.HOM.visible(vis))
		return;

	if (Device.vCameraPosition.distance_to_sqr(vis.sphere.P) > _sqr(g_pGamePersistent->Environment().CurrentEnv->fog_distance + vis.sphere.R))
		return;

	float D = distance_to_aabb(Device.vCameraPosition,pVisual->vis.box);

	PROF_EVENT("add_Static switch")
	// If we get here visual is visible or partially visible
	switch (pVisual->Type)
	{
	case MT_HIERRARHY:
	{
		// Add all children
		FHierrarhyVisual* pV = (FHierrarhyVisual*)pVisual;
		if (fcvPartial == VIS)
		{
			for (dxRender_Visual* V : pV->children)
			{
				add_Static(V, planes);
			}
		}
		else
		{
			add_leafs_Static(pV->children);
		}
		}return;
		case MT_LOD:
		{
			r_dsgraph_insert_static_lod(pVisual);
		}return;
		case MT_MESH_LODS:
		{
			auto Casted = SelectLOD(D, static_cast<FMUMeshLODs*>(pVisual)->children);
			if (Casted)
			{
				add_leafs_Static(Casted->children);
			}
		}
	case MT_TREE_ST:
	case MT_TREE_PM:
	{
		// General type of visual
		r_dsgraph_insert_static(pVisual);
	}return;
	default:
	{
		// General type of visual
		r_dsgraph_insert_static(pVisual);
	}return;
	}
}

CDB::MODEL* R_dsgraph_structure::GetHOMModel()
{
	return RImplementation.HOM.GetHOMModel();
}
xr_vector<u32>* R_dsgraph_structure::GetHOMInvaltids()
{
	return RImplementation.HOM.get_invaltids();
};