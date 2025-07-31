#include "stdafx.h"

#include "../../xrEngine/Render.h"
#include "../../xrEngine/IRenderable.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/xr_object.h"

#include "FBasicVisual.h"
#include "CHudInitializer.h"
#include "SkeletonCustom.h"
using namespace		R_dsgraph;

extern float		r_ssaDISCARD;
extern float		r_ssaDONTSORT;
extern float		r_ssaHZBvsTEX;
extern float		r_ssaGLOD_start,	r_ssaGLOD_end;

ICF float calcLOD	(float ssa/*fDistSq*/, float R)
{
	return			_sqrt(clampr((ssa - r_ssaGLOD_end)/(r_ssaGLOD_start-r_ssaGLOD_end),0.f,1.f));
}

// ALPHA
void __fastcall sorted_L1		(mapSorted_Node *N)
{
	//PROF_EVENT("sorted_L1");
	VERIFY (N);
	dxRender_Visual *V				= N->val.pVisual;
	VERIFY (V && V->shader._get());
	RCache.set_Element(N->val.se);
	RCache.set_xform_world(N->val.Matrix);
	RImplementation.apply_object(N->val.pObject);
	RImplementation.apply_lmaterial	();
	V->Render(calcLOD(N->key, V->vis.sphere.R));
}

void R_dsgraph_structure::r_dsgraph_render_graph(u32 _priority, bool _clear)
{
	PROF_EVENT("r_dsgraph_render_graph");
	//GPU_EVENT(r_dsgraph_render_graph);
	CScopeTimer Timer(Device.Statistic->RenderDUMP);

	// **************************************************** NORMAL
	// Perform sorting based on ScreenSpaceArea
	// Sorting by SSA and changes minimizations
	{
		RCache.set_xform_world			(Fidentity);

		// Render several passes
		PROF_EVENT("NORMAL_SHADER_PASSES");
		for ( u32 iPass = 0; iPass<SHADER_PASSES_MAX; ++iPass)
		{
			//mapNormalVS&	vs				= mapNormal	[_priority];
			mapNormalVS&	vs				= mapNormalPasses[_priority][iPass];
			for (mapNormalVS::TNode& Nvs : vs)
			{
				RCache.set_VS					(Nvs.key);

#ifdef USE_DX11
				//	GS setup
				mapNormalGS&		gs			= Nvs.val;
				for (mapNormalGS::TNode& Ngs : gs)
				{
					RCache.set_GS					(Ngs.key);	

					mapNormalPS&		ps			= Ngs.val;
#else //USE_DX11
					mapNormalPS&		ps			= Nvs.val;
#endif
					for (mapNormalPS::TNode& Nps : ps)
					{
						RCache.set_PS					(Nps.key);	
#ifdef USE_DX11
						mapNormalCS&		cs			= Nps.val.mapCS;
						RCache.set_HS(Nps.val.hs);
						RCache.set_DS(Nps.val.ds);
#else //USE_DX11
						mapNormalCS&		cs			= Nps.val;
#endif
						for (mapNormalCS::TNode& Ncs : cs)
						{
							RCache.set_Constants			(Ncs.key);

							mapNormalStates&	states		= Ncs.val;
							for (mapNormalStates::TNode& Nstate : states)
							{
								RCache.set_States					(Nstate.key);

								mapNormalTextures&		tex			= Nstate.val;
								for (mapNormalTextures::TNode& Ntex : tex)
								{
									RCache.set_Textures					(Ntex.key);
									RImplementation.apply_lmaterial		();

									mapNormalItems&				items	= Ntex.val;
									for (_NormalItem& Ni : items)
									{
										float LOD = calcLOD(Ni.ssa, Ni.pVisual->vis.sphere.R);
#ifdef USE_DX11
										RCache.LOD.set_LOD(LOD);
#endif
										Ni.pVisual->Render(LOD);
									}if(_clear)items.clear();
								}if(_clear) tex.clear();
							}if(_clear) states.clear();
						}if(_clear) cs.clear();

					}if(_clear) ps.clear();
#ifdef USE_DX11
				}if(_clear) gs.clear();
#endif //USE_DX11
			}if(_clear) vs.clear();
		}
	}

	// **************************************************** MATRIX
	// Perform sorting based on ScreenSpaceArea
	// Sorting by SSA and changes minimizations
	// Render several passes
	PROF_EVENT("MATRIX_SHADER_PASSES");
	for ( u32 iPass = 0; iPass<SHADER_PASSES_MAX; ++iPass)
	{
		//mapMatrixVS&	vs				= mapMatrix	[_priority];
		mapMatrixVS&	vs				= mapMatrixPasses[_priority][iPass];
		for (mapMatrixVS::TNode& Nvs : vs)
		{
			RCache.set_VS					(Nvs.key);	

#ifdef USE_DX11
			mapMatrixGS&		gs			= Nvs.val;
			for (mapMatrixGS::TNode& Ngs : gs)
			{
				RCache.set_GS					(Ngs.key);	

				mapMatrixPS&		ps			= Ngs.val;
#else //USE_DX11
				mapMatrixPS&		ps			= Nvs.val;
#endif
				for (mapMatrixPS::TNode& Nps : ps)
				{
					RCache.set_PS					(Nps.key);	
#ifdef USE_DX11
					mapMatrixCS&		cs			= Nps.val.mapCS;
					RCache.set_HS(Nps.val.hs);
					RCache.set_DS(Nps.val.ds);
#else
					mapMatrixCS&		cs			= Nps.val;
#endif
					for (mapMatrixCS::TNode& Ncs : cs)
					{
						RCache.set_Constants			(Ncs.key);

						mapMatrixStates&	states		= Ncs.val;
						for (mapMatrixStates::TNode& Nstate : states)
						{
							RCache.set_States					(Nstate.key);

							mapMatrixTextures&		tex			= Nstate.val;
							for (mapMatrixTextures::TNode& Ntex : tex)
							{
								RCache.set_Textures					(Ntex.key);
								RImplementation.apply_lmaterial		();

								mapMatrixItems&				items	= Ntex.val;
								for (_MatrixItem& Ni : items)
								{
									if (Ni.pVisual->shader == nullptr)
									{
										continue;
									}
									RCache.set_xform_world(Ni.Matrix);
									RImplementation.apply_object(Ni.pObject);
									RImplementation.apply_lmaterial();

									float LOD = calcLOD(Ni.ssa, Ni.pVisual->vis.sphere.R);
#ifdef USE_DX11
									RCache.LOD.set_LOD(LOD);
#endif
									Ni.pVisual->Render(LOD);
								}if(_clear)items.clear();
							}if(_clear) tex.clear();
						}if(_clear) states.clear();
					}if(_clear) cs.clear();
				}if(_clear) ps.clear();
#ifdef USE_DX11
			}if(_clear) gs.clear();
#endif //USE_DX11
		}if(_clear) vs.clear();
	}
}

//////////////////////////////////////////////////////////////////////////
// HUD render
void R_dsgraph_structure::r_dsgraph_render_hud	()
{
	PROF_EVENT("r_dsgraph_render_hud");
	CHudInitializer initalizer(true);

	// Rendering
	rmNear						();
	mapHUD.traverseLR			(sorted_L1);
	mapHUD.clear				();

#if	RENDER==R_R1
	if (g_hud && g_hud->RenderActiveItemUIQuery())
		r_dsgraph_render_hud_ui						();				// hud ui
#endif

	rmNormal					();
}

void R_dsgraph_structure::r_dsgraph_render_hud_ui()
{
	PROF_EVENT("r_dsgraph_render_hud_ui");
	VERIFY(g_hud && g_hud->RenderActiveItemUIQuery());

	CHudInitializer initalizer(true);

#if	RENDER==R_R2
	// Targets, use accumulator for temporary storage
	const ref_rt	rt_null;
	RCache.set_RT(0,	1);
	RCache.set_RT(0,	2);
	RImplementation.Target->u_setrt(RImplementation.Target->rt_Color, rt_null, rt_null, RDepth);
#endif

	rmNear						();
	g_hud->RenderActiveItemUI	();
	rmNormal					();
}

//////////////////////////////////////////////////////////////////////////
// strict-sorted render
void	R_dsgraph_structure::r_dsgraph_render_sorted	(bool render_hud)
{
	PROF_EVENT("r_dsgraph_render_sorted");
	// Rendering
	// Sorted (back to front)

	mapSorted.traverseRL	(sorted_L1);
	mapSorted.clear			();

	if (render_hud) {
		r_dsgraph_render_sorted_hud();
	}
}

void R_dsgraph_structure::r_dsgraph_render_sorted_hud()
{
	PROF_EVENT("r_dsgraph_render_sorted_hud");

	CHudInitializer initalizer(true);

	rmNear();
	mapHUDSorted.traverseRL(sorted_L1);
	mapHUDSorted.clear();
	rmNormal();
}

//////////////////////////////////////////////////////////////////////////
// strict-sorted render
void	R_dsgraph_structure::r_dsgraph_render_emissive	()
{
	PROF_EVENT("r_dsgraph_render_emissive");
#if	RENDER!=R_R1
	// Rendering
	// Sorted (back to front)

	mapEmissive.traverseLR	(sorted_L1);
	mapEmissive.clear		();

	//	HACK: Calculate this only once
	CHudInitializer initalizer(true);

	rmNear();
	mapHUDEmissive.traverseLR(sorted_L1);
	mapHUDEmissive.clear();
	rmNormal();
#endif
}
// strict-sorted render
void	R_dsgraph_structure::r_dsgraph_render_scope	()
{
#if	RENDER==R_R4
	GPU_EVENT(SCOPE_BUFFER_RENDER);
	{
		GPU_EVENT(ZBUFFER_COPY);
		RCache.set_ZB(NULL);

		ID3D11Resource* res{};
		RDepth->GetResource(&res);

		RContext->CopyResource(RImplementation.Target->rt_Position->pSurface, res);
		_RELEASE(res);
	}

	RImplementation.Target->u_setrt(NULL, NULL, RDepth);
	CHudInitializer initalizer(true);

	mapHUDScopeMask.traverseLR(sorted_L1);
	mapHUDScopeMask.clear();
#endif
}

//////////////////////////////////////////////////////////////////////////
// strict-sorted render
void	R_dsgraph_structure::r_dsgraph_render_wmarks	()
{
	PROF_EVENT("r_dsgraph_render_wmarks");
#if	RENDER!=R_R1
	// Sorted (back to front)
	mapWmark.traverseLR	(sorted_L1);
	mapWmark.clear		();
#endif
}

//////////////////////////////////////////////////////////////////////////
// strict-sorted render
void	R_dsgraph_structure::r_dsgraph_render_distort	()
{
	PROF_EVENT("r_dsgraph_render_distort");
	// Sorted (back to front)
	mapDistort.traverseRL	(sorted_L1);
	mapDistort.clear		();

	//	HACK: Calculate this only once
	CHudInitializer initalizer(true);

	rmNear();
	mapHUDDistort.traverseLR(sorted_L1);
	mapHUDDistort.clear();
	rmNormal();
}

//////////////////////////////////////////////////////////////////////////
// sub-space rendering - shortcut to render with frustum extracted from matrix
void	R_dsgraph_structure::r_dsgraph_render_subspace	(IRender_Sector* _sector, Fmatrix& mCombined, Fvector& _cop, BOOL _dynamic, BOOL _precise_portals, CObject* O)
{
	if(!_sector) return;
	CFrustum	temp;
	temp.CreateFromMatrix			(mCombined,	FRUSTUM_P_ALL &(~FRUSTUM_P_NEAR));
	r_dsgraph_render_subspace		(_sector,&temp,mCombined,_cop,_dynamic,_precise_portals, O);
}

// sub-space rendering - main procedure
void	R_dsgraph_structure::r_dsgraph_render_subspace	(IRender_Sector* _sector, CFrustum* _frustum, Fmatrix& mCombined, Fvector& _cop, BOOL _dynamic, BOOL _precise_portals, CObject* O)
{
	PROF_EVENT("r_dsgraph_render_subspace")
	VERIFY							(_sector);
	RImplementation.marker			++;			// !!! critical here

	// Save and build new frustum, disable HOM
	CFrustum	ViewSave			= ViewBase;
	ViewBase						= *_frustum;
	View							= &ViewBase;

	if (_precise_portals && RImplementation.rmPortals)		{
		PROF_EVENT("precise_portals")
		// Check if camera is too near to some portal - if so force DualRender
		Fvector box_radius;		box_radius.set	(EPS_L*20,EPS_L*20,EPS_L*20);
		RImplementation.Sectors_xrc.box_options	(CDB::OPT_FULL_TEST);
		RImplementation.Sectors_xrc.box_query	(RImplementation.rmPortals,_cop,box_radius);
		for (int K=0; K<RImplementation.Sectors_xrc.r_count(); K++)
		{
			CPortal*	pPortal		= (CPortal*) RImplementation.Portals[RImplementation.rmPortals->get_tris()[RImplementation.Sectors_xrc.r_begin()[K].id].dummy];
			pPortal->bDualRender	= TRUE;
		}
	}

	// Traverse sector/portal structure
	PortalTraverser.traverse		( _sector, ViewBase, _cop, mCombined, 0 );
	{
		PROF_EVENT("add_static")
	// Determine visibility for static geometry hierrarhy
	for (u32 s_it=0; s_it<PortalTraverser.r_sectors.size(); s_it++)
	{
		CSector*	sector		= (CSector*)PortalTraverser.r_sectors[s_it];
		dxRender_Visual*	root	= sector->root();
		for (u32 v_it=0; v_it<sector->r_frustums.size(); v_it++)	{
			set_Frustum			(&(sector->r_frustums[v_it]));
			add_Geometry		(root);
			}
		}
	}

	if (_dynamic)
	{
		PROF_EVENT("add_dynamic")
		set_Object						(0);

		// Traverse object database
		g_SpatialSpace->q_frustum
			(
			lstRenderables,
			ISpatial_DB::O_ORDERED,
			STYPE_RENDERABLE + STYPE_RENDERABLESHADOW,
			ViewBase
			);

		// Determine visibility for dynamic part of scene
		for (u32 o_it=0; o_it<lstRenderables.size(); o_it++)
		{
			ISpatial*	spatial		= lstRenderables[o_it].get();
			CSector*	sector		= (CSector*)spatial->spatial.sector;
			if	(0==sector)										continue;	// disassociated from S/P structure
			if	(PortalTraverser.i_marker != sector->r_marker)	continue;	// inactive (untouched) sector
			for (u32 v_it=0; v_it<sector->r_frustums.size(); v_it++)
			{
				set_Frustum			(&(sector->r_frustums[v_it]));
				if (!View->testSphere_dirty(spatial->spatial.sphere.P,spatial->spatial.sphere.R))	continue;

				// renderable
				IRenderable*	renderable		= spatial->dcast_Renderable	();
				if (0==renderable)				continue;					// unknown, but renderable object (r1_glow???)
#if RENDER!=R_R1
				if(Device.vCameraPosition.distance_to_sqr(renderable->renderable.xform.c)<=10000.f)
				{
					CKinematics* pKin = (CKinematics*)renderable->renderable.visual;
					if(pKin)
					{
						if(spatial->spatial.type&STYPE_RENDERABLESHADOW)
						{
							pKin->CalculateBones(TRUE);
						}
						if(spatial->spatial.type&STYPE_RENDERABLE)
						{
							if(0==ViewSave.testSphere_dirty(spatial->spatial.sphere.P, spatial->spatial.sphere.R))
							{
								pKin->CalculateBones(TRUE);
							}
						}
					}
				}
#endif
				if(O && O->dcast_Renderable()==renderable) continue;

				renderable->renderable_Render	();
			}
		}
	}

	// Restore
	ViewBase						= ViewSave;
	View							= 0;
}

#include "FHierrarhyVisual.h"
#include "SkeletonCustom.h"
#include "../../xrEngine/Fmesh.h"
#include "FLOD.h"

void	R_dsgraph_structure::r_dsgraph_render_R1_box	(IRender_Sector* _S, Fbox& BB, int sh)
{
	CSector*	S			= (CSector*)_S;
	lstVisuals.clear		();
	lstVisuals.push_back	(S->root());
	
	for (u32 test=0; test<lstVisuals.size(); test++)
	{
		dxRender_Visual*	V		= 	lstVisuals[test];
		
		// Visual is 100% visible - simply add it
		xr_vector<dxRender_Visual*>::iterator I,E;	// it may be usefull for 'hierrarhy' visuals
		
		switch (V->Type) {
		case MT_HIERRARHY:
			{
				// Add all children
				FHierrarhyVisual* pV = (FHierrarhyVisual*)V;
				I = pV->children.begin	();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T			= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_SKELETON_ANIM:
		case MT_SKELETON_RIGID:
			{
				// Add all children	(s)
				CKinematics * pV		= (CKinematics*)V;
				pV->CalculateBones		(TRUE);
				I = pV->children.begin	();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_LOD:
			{
				FLOD		* pV		=	(FLOD*) V;
				I = pV->children.begin		();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		default:
			{
				// Renderable visual
				ShaderElement* E_	= V->shader->E[sh]._get();
				if (E_ && !(E_->flags.bDistort))
				{
					for (u32 pass=0; pass<E_->passes.size(); pass++)
					{
						RCache.set_Element			(E_,pass);
						V->Render					(-1.f);
					}
				}
			}
			break;
		}
	}
}

