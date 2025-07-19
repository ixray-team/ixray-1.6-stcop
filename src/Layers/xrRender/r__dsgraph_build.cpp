#include "stdafx.h"

#include "FHierrarhyVisual.h"
#include "SkeletonCustom.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/IRenderable.h"

#include "FLOD.h"
#include "ParticleGroup.h"
#include "FTreeVisual.h"

using	namespace R_dsgraph;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Scene graph actual insertion and sorting ////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
float		r_ssaDISCARD;
float		r_ssaDONTSORT;
float		r_ssaLOD_A,			r_ssaLOD_B;
float		r_ssaGLOD_start,	r_ssaGLOD_end;
float		r_ssaHZBvsTEX;

ICF	float	CalcSSA				(float& distSQ, Fvector& C, dxRender_Visual* V)
{
	float R	= V->vis.sphere.R + 0;
	distSQ	= Device.vCameraPosition.distance_to_sqr(C)+EPS;
	return	R/distSQ;
}
ICF	float	CalcSSA				(float& distSQ, Fvector& C, float R)
{
	distSQ	= Device.vCameraPosition.distance_to_sqr(C)+EPS;
	return	R/distSQ;
}

void CDSGraphManager::r_dsgraph_insert_dynamic(dxRender_Visual *pVisual, Fmatrix* xform)
{
	if (m_visuals_dynamic.find(pVisual))
		return;
	else
		m_visuals_dynamic.insert(pVisual);

	Fvector Center;
	xform->transform_tiny(Center, pVisual->vis.sphere.P);

	float distSQ;
	float SSA = CalcSSA(distSQ,Center,pVisual);

	if (SSA<=r_ssaDISCARD)
		return;

	// Distortive geometry should be marked and R2 special-cases it
	// a) Allow to optimize RT order
	// b) Should be rendered to special distort buffer in another pass
	VERIFY(pVisual->shader._get());

	ShaderElement* sh_d	= &*pVisual->shader->E[4];
	if (RImplementation.o.distortion && sh_d && sh_d->flags.bDistort && i_mask[sh_d->flags.iPriority/2])
	{
		if (i_mask[CDSGraphManager::fl_hud])
			RGraph.mapHUDSorted.Distort.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh_d, i_mask[CDSGraphManager::fl_hud] });
		else
			RGraph.mapDynamicSorted.Distort.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh_d, i_mask[CDSGraphManager::fl_hud] });
	}

	if (sh_d && sh_d->flags.bScopeMask && i_mask[CDSGraphManager::fl_deffered] && i_mask[CDSGraphManager::fl_hud])
	{
		RGraph.mapHUDSorted.ScopeLens.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh_d, i_mask[CDSGraphManager::fl_hud] });
	}

	// Select shader
	ShaderElement* sh =	RImplementation.rimp_select_sh_dynamic(pVisual,distSQ);

	if (0==sh)
		return;
	u32 shader_priority = sh->flags.iPriority/2;
	if (!i_mask[shader_priority])
		return;

	// Create common node
	// NOTE: Invisible elements exist only in R1

	// HUD rendering
	if (i_mask[CDSGraphManager::fl_hud])
	{
		if (sh->flags.bStrictB2F)	
		{
			RGraph.mapHUDSorted.Sorted.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh, i_mask[CDSGraphManager::fl_hud] });
			return;
		} 
		else 
		{
			RGraph.mapHUD.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh, i_mask[CDSGraphManager::fl_hud] });
#if RENDER!=R_R1
			if (sh->flags.bEmissive) 
				RGraph.mapHUDSorted.Emissive.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh_d, i_mask[CDSGraphManager::fl_hud] });
#endif	//	RENDER!=R_R1
			return;
		}
	}

	// Shadows registering
#if RENDER==R_R1
	DSGraphItem item = DSGraphItem{ SSA,val_pObject,pVisual,xform,nullptr,i_mask[CDSGraphManager::fl_hud] };
	RImplementation.L_Shadows->add_element(item);
#endif
	if (i_mask[CDSGraphManager::fl_invisible])
		return;

	// strict-sorting selection
	if (sh->flags.bStrictB2F)
	{
		RGraph.mapDynamicSorted.Sorted.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh, i_mask[CDSGraphManager::fl_hud] });
		return;
	}

#if RENDER!=R_R1
	// Emissive geometry should be marked and R2 special-cases it
	// a) Allow to skeep already lit pixels
	// b) Allow to make them 100% lit and really bright
	// c) Should not cast shadows
	// d) Should be rendered to accumulation buffer in the second pass
	if (sh->flags.bEmissive)
		RGraph.mapDynamicSorted.Emissive.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh_d, i_mask[CDSGraphManager::fl_hud] });

	if (sh->flags.bWmark && i_mask[CDSGraphManager::fl_wmarks])
	{
		if (i_mask[CDSGraphManager::fl_hud])
			RGraph.mapHUDSorted.Wmark.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh, i_mask[CDSGraphManager::fl_hud] });
		else
			RGraph.mapDynamicSorted.Wmark.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, sh, i_mask[CDSGraphManager::fl_hud] });
		return;
	}
#endif

	for ( u32 iPass = 0; iPass<sh->passes.size(); ++iPass)
	{
		// the most common node
		if (sh->passes[iPass] == nullptr)
			continue;

		SPass& pass	= *sh->passes[iPass];
		mapDSGraphVS& map = RGraph.mapDynamicPasses[shader_priority][iPass];
		

#ifdef USE_RESOURCE_DEBUGGER
	#ifdef USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(pass.vs);
		mapDSGraphGS::TNode* Ngs = Nvs->val.insert(pass.gs);
		mapDSGraphPS::TNode* Nps = Ngs->val.insert(pass.ps);
	#else //USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(pass.vs);
		mapDSGraphPS::TNode* Nps = Nvs->val.insert(pass.ps);
	#endif
#else
	#ifdef USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(&*pass.vs);
		mapDSGraphGS::TNode* Ngs = Nvs->val.insert(pass.gs->gs);
		mapDSGraphPS::TNode* Nps = Ngs->val.insert(pass.ps->ps);
	#else //USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(pass.vs->vs);
		mapDSGraphPS::TNode* Nps = Nvs->val.insert(pass.ps->ps);
	#endif
#endif

#ifdef USE_DX11
#	ifdef USE_RESOURCE_DEBUGGER
		Nps->val.hs = pass.hs;
		Nps->val.ds = pass.ds;
		mapDSGraphCS::TNode* Ncs = Nps->val.mapCS.insert(pass.constants._get());
#	else
		Nps->val.hs = pass.hs->sh;
		Nps->val.ds = pass.ds->sh;
		mapDSGraphCS::TNode* Ncs = Nps->val.mapCS.insert(pass.constants._get());
#	endif
#else
		mapDSGraphCS::TNode* Ncs = Nps->val.insert(pass.constants._get());
#endif
		mapDSGraphStates::TNode* Nstate	= Ncs->val.insert(pass.state->state);
		mapDSGraphTextures::TNode* Ntex = Nstate->val.insert(pass.T._get());
#if RENDER==R_R1
		Ntex->val.push_back(item);
#else
		Ntex->val.push_back(DSGraphItem{ SSA, val_pObject, pVisual, xform, nullptr, i_mask[CDSGraphManager::fl_hud] });
#endif

	}
}

void CDSGraphManager::r_dsgraph_insert_static(dxRender_Visual *pVisual)
{
	if (m_visuals_static.find(pVisual))
		return;
	else
		m_visuals_static.insert(pVisual);

	float distSQ;

	float SSA =	CalcSSA(distSQ,pVisual->vis.sphere.P,pVisual);

	if (SSA<=r_ssaDISCARD)
		return;

	// Distortive geometry should be marked and R2 special-cases it
	// a) Allow to optimize RT order
	// b) Should be rendered to special distort buffer in another pass
	VERIFY(pVisual->shader._get());
	ShaderElement* sh_d	= &*pVisual->shader->E[4];
	if (RImplementation.o.distortion && sh_d && sh_d->flags.bDistort && i_mask[sh_d->flags.iPriority/2])
		RGraph.mapStaticSorted.Distort.push_back(DSGraphItem{ SSA, nullptr, pVisual, &Fidentity, sh_d,false });

	// Select shader
	ShaderElement* sh = RImplementation.rimp_select_sh_static(pVisual,distSQ);

	if (0==sh)
		return;
	u32 shader_priority = sh->flags.iPriority / 2;
	if (!i_mask[shader_priority])
		return;
	// strict-sorting selection
	if (sh->flags.bStrictB2F)
	{
		RGraph.mapStaticSorted.Sorted.push_back(DSGraphItem{ SSA, nullptr, pVisual, &Fidentity, sh,false });
		return;
	}

#if RENDER!=R_R1
	// Emissive geometry should be marked and R2 special-cases it
	// a) Allow to skeep already lit pixels
	// b) Allow to make them 100% lit and really bright
	// c) Should not cast shadows
	// d) Should be rendered to accumulation buffer in the second pass
	if (sh->flags.bEmissive)
		RGraph.mapStaticSorted.Emissive.push_back(DSGraphItem{ SSA, nullptr, pVisual, &Fidentity, sh_d,false });

	if (sh->flags.bWmark && i_mask[CDSGraphManager::fl_wmarks])
	{
		RGraph.mapStaticSorted.Wmark.push_back(DSGraphItem{SSA, nullptr, pVisual, &Fidentity, sh,false });
		return;
	}
#endif
	for ( u32 iPass = 0; iPass<sh->passes.size(); ++iPass)
	{
		// the most common node
		if (sh->passes[iPass] == nullptr)
			continue;

		SPass& pass	= *sh->passes[iPass];
		mapDSGraphVS& map = RGraph.mapStaticPasses[shader_priority][iPass];

#ifdef USE_RESOURCE_DEBUGGER
#ifdef USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(pass.vs);
		mapDSGraphGS::TNode* Ngs = Nvs->val.insert(pass.gs);
		mapDSGraphPS::TNode* Nps = Ngs->val.insert(pass.ps);
#	else //USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(pass.vs);
		mapDSGraphPS::TNode* Nps = Nvs->val.insert(pass.ps);
#	endif
#else // USE_RESOURCE_DEBUGGER
#ifdef USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(&*pass.vs);
		mapDSGraphGS::TNode* Ngs = Nvs->val.insert(pass.gs->gs);
		mapDSGraphPS::TNode* Nps = Ngs->val.insert(pass.ps->ps);
#	else //USE_DX11
		mapDSGraphVS::TNode* Nvs = map.insert(pass.vs->vs);
		mapDSGraphPS::TNode* Nps = Nvs->val.insert(pass.ps->ps);
#	endif
#endif // USE_RESOURCE_DEBUGGER

#ifdef USE_DX11
#	ifdef USE_RESOURCE_DEBUGGER
		Nps->val.hs = pass.hs;
		Nps->val.ds = pass.ds;
		mapDSGraphCS::TNode* Ncs = Nps->val.mapCS.insert(pass.constants._get());
#	else
		Nps->val.hs = pass.hs->sh;
		Nps->val.ds = pass.ds->sh;
		mapDSGraphCS::TNode* Ncs = Nps->val.mapCS.insert(pass.constants._get());
#	endif
#else
		mapDSGraphCS::TNode* Ncs = Nps->val.insert(pass.constants._get());
#endif
		mapDSGraphStates::TNode* Nstate	= Ncs->val.insert(pass.state->state);
		mapDSGraphTextures::TNode* Ntex = Nstate->val.insert(pass.T._get());

		Ntex->val.push_back(DSGraphItem{SSA,nullptr,pVisual,nullptr,nullptr,false});
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
void CDSGraphManager::add_Dynamic(IRenderVisual* piVisual, Fmatrix* xform)
{
	dxRender_Visual* pVisual = (dxRender_Visual*)piVisual;
	if (!pVisual) return;

	// Visual is 100% visible - simply add it
	switch (pVisual->Type)
	{
		case MT_PARTICLE_GROUP:
		{
			// Add all children, doesn't perform any tests
			PS::CParticleGroup* pG = (PS::CParticleGroup*)pVisual->dcast_ParticleCustom();
			xrCriticalSectionGuard guard(&pG->onframe_lock);
			for (PS::CParticleGroup::SItem& I_ : pG->items)
			{
				add_Dynamic(I_._effect, xform);
				add_leafs_Dynamic(I_._children_related, xform);
				add_leafs_Dynamic(I_._children_free, xform);
			}
		}return;

		case MT_HIERRARHY:
		{
			// Add all children, doesn't perform any tests
			FHierrarhyVisual* pV = (FHierrarhyVisual*)pVisual;
			add_leafs_Dynamic(pV->children, xform);
		}return;

		case MT_SKELETON_ANIM:
		case MT_SKELETON_RIGID:
		{
			// Add all children, doesn't perform any tests
			CKinematics * pV = (CKinematics*)pVisual;
			BOOL _use_lod = FALSE;
			if (pV->m_lod)				
			{
				Fvector Tpos;
				float D;
				xform->transform_tiny(Tpos, pV->vis.sphere.P);
				float ssa =	CalcSSA	(D,Tpos,pV->vis.sphere.R*0.5f);	// assume dynamics never consume full sphere
				if (ssa<r_ssaLOD_A)
					_use_lod = TRUE;
			}
			if (_use_lod)				
				add_Dynamic(pV->m_lod, xform);
			else
			{
				//pV->CalculateBones(TRUE);
				if (i_mask[CDSGraphManager::fl_normal])
					pV->CalculateWallmarks();
				add_leafs_Dynamic(pV->children, xform);
			}
		}return;

		default:
		{
			// General type of visual
			// Calculate distance to it's center
			r_dsgraph_insert_dynamic(pVisual, xform);
		}return;
	}
}

void CDSGraphManager::add_leafs_Dynamic(xr_vector<dxRender_Visual*>& children, Fmatrix* xform)
{
	for (dxRender_Visual* pVisual : children)
	{
		if (!pVisual) continue;

		// Visual is 100% visible - simply add it
		switch (pVisual->Type)
		{
		case MT_PARTICLE_GROUP:
		{
			// Add all children, doesn't perform any tests
			PS::CParticleGroup* pG = (PS::CParticleGroup*)pVisual->dcast_ParticleCustom();
			xrCriticalSectionGuard guard(&pG->onframe_lock);
			for (PS::CParticleGroup::SItem& I_ : pG->items)
			{
				add_Dynamic(I_._effect, xform);
				add_leafs_Dynamic(I_._children_related, xform);
				add_leafs_Dynamic(I_._children_free, xform);
			}
		}break;

		case MT_HIERRARHY:
		{
			// Add all children, doesn't perform any tests
			FHierrarhyVisual* pV = (FHierrarhyVisual*)pVisual;
			add_leafs_Dynamic(pV->children, xform);
		}break;

		case MT_SKELETON_ANIM:
		case MT_SKELETON_RIGID:
		{
			// Add all children, doesn't perform any tests
			CKinematics* pV = (CKinematics*)pVisual;
			BOOL _use_lod = FALSE;
			if (pV->m_lod)
			{
				Fvector Tpos;
				float D;
				xform->transform_tiny(Tpos, pV->vis.sphere.P);
				float ssa = CalcSSA(D, Tpos, pV->vis.sphere.R * 0.5f);	// assume dynamics never consume full sphere
				if (ssa < r_ssaLOD_A)
					_use_lod = TRUE;
			}
			if (_use_lod)
				add_Dynamic(pV->m_lod, xform);
			else
			{
				//pV->CalculateBones(TRUE);
				if (i_mask[CDSGraphManager::fl_normal])
					pV->CalculateWallmarks();
				add_leafs_Dynamic(pV->children, xform);
			}
		}break;

		default:
		{
			// General type of visual
			// Calculate distance to it's center
			r_dsgraph_insert_dynamic(pVisual, xform);
		}break;
		}
	}
}

void CDSGraphManager::add_Static(IRenderVisual* piVisual, CFrustum& frustum, u32 planes)
{
	dxRender_Visual* pVisual = (dxRender_Visual*)piVisual;
	if (!pVisual) return;
	// Check frustum visibility and calculate distance to visual's center
	vis_data& vis = pVisual->vis;
	EFC_Visible VIS = frustum.testSAABB(vis.sphere.P, vis.sphere.R, vis.box.data(), planes);

	if (fcvNone == VIS)
		return;

#if RENDER!=R_R1
	if (i_mask[CDSGraphManager::fl_normal])//phase normal
#endif
		if (!RImplementation.HOM.visible(vis))
			return;

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
				add_Static((IRenderVisual*)V, frustum, planes);
		}
		else
			add_leafs_Static(pV->children);
	}break;

	case MT_LOD:
	{
		FLOD* pV = (FLOD*)pVisual;
		float D;
		float ssa = CalcSSA(D, pV->vis.sphere.P, pV) * pV->lod_factor;

		if (ssa < r_ssaLOD_A)
		{
			if (ssa < r_ssaDISCARD)
				return;

			RGraph.mapLOD.push_back({ ssa, nullptr, pVisual, nullptr, nullptr });
		}
#if RENDER!=R_R1
		if (ssa > r_ssaLOD_B || i_mask[CDSGraphManager::fl_shmap])//phase shmap
#else
		if (ssa > r_ssaLOD_B)
#endif
		{
			// Add all children, perform tests
			add_leafs_Static(pV->children);
		}
	}break;

	case MT_TREE_ST:
	case MT_TREE_PM:
	default:
	{
		// General type of visual
		r_dsgraph_insert_static(pVisual);
	}return;
	}
}

void CDSGraphManager::add_leafs_Static(xr_vector<dxRender_Visual*>& children)
{
	for(dxRender_Visual* pVisual : children)
	{
#if RENDER!=R_R1
		if (i_mask[CDSGraphManager::fl_normal])//phase normal
#endif
			if (!RImplementation.HOM.visible(pVisual->vis))
				continue;

		// Visual is 100% visible - simply add it
		switch (pVisual->Type)
		{
		case MT_HIERRARHY:
		{
			// Add all children, doesn't perform any tests
			FHierrarhyVisual* pV = (FHierrarhyVisual*)pVisual;
			add_leafs_Static(pV->children);
		}break;

		case MT_LOD:
		{
			FLOD* pV = (FLOD*)pVisual;
			float D;
			float ssa = CalcSSA(D, pV->vis.sphere.P, pV) * pV->lod_factor;

			if (ssa < r_ssaLOD_A)
			{
				if (ssa < r_ssaDISCARD)
					break;

				RGraph.mapLOD.push_back({ ssa, nullptr, pVisual, nullptr, nullptr });
			}
#if RENDER!=R_R1
			if (ssa > r_ssaLOD_B || i_mask[CDSGraphManager::fl_shmap])//phase shmap
#else
			if (ssa > r_ssaLOD_B)
#endif
			{
				// Add all children, doesn't perform any tests
				add_leafs_Static(pV->children);
			}
		}break;

		case MT_TREE_PM:
		case MT_TREE_ST:
		default:
		{
			// General type of visual
			r_dsgraph_insert_static(pVisual);
		}break;
		}
	}
}
