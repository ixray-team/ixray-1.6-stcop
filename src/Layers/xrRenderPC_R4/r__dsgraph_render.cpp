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
#include "SVGStorage.h"

using namespace		R_dsgraph;

extern float		r_ssaDISCARD;
extern float		r_ssaDONTSORT;
extern float		r_ssaHZBvsTEX;
extern float		r_ssaGLOD_start,	r_ssaGLOD_end;

ICF float calcLOD	(float ssa/*fDistSq*/, float R)
{
	return _sqrt(clampr((ssa - r_ssaGLOD_end)/(r_ssaGLOD_start-r_ssaGLOD_end),0.f,1.f));
}

template <class K, class T, class A>
static void WipeMap(FixedMAP<K, T, A>& m);

static void WipeMap(R_dsgraph::mapNormalAdvStages& s);
static void WipeMap(R_dsgraph::mapMatrixAdvStages& s);
static void WipeMap(R_dsgraph::mapMatrixItems& i);

template <class T>
static void WipeMap(T& v)
{
	v.clear();
}

template <class K, class T, class A>
static void WipeMap(FixedMAP<K, T, A>& m)
{
	for (auto& n : m)
	{
		WipeMap(n.val);
	}
	m.clear();
}

static void WipeMap(R_dsgraph::mapNormalAdvStages& s)
{
	WipeMap(s.mapCS);
}

static void WipeMap(R_dsgraph::mapMatrixAdvStages& s)
{
	WipeMap(s.mapCS);
}

static void WipeMap(R_dsgraph::mapMatrixItems& i)
{
	i.visuals.clear();
	i.particles.clear();
}

void R_dsgraph_structure::r_dsgraph_clear_passes()
{
	for (u32 i = 0; i < 2; ++i)
	{
		for (u32 j = 0; j < SHADER_PASSES_MAX; ++j)
		{
			WipeMap(mapNormalPasses[i][j]);
			WipeMap(mapMatrixPasses[i][j]);
		}
	}
	r_dsgraph_clear_aux();
}

void R_dsgraph_structure::r_dsgraph_clear_aux()
{
	mapSorted.clear();
	mapDistort.clear();
	mapEmissive.clear();
	mapLOD.clear();
	mapWmark.clear();
	lstLODs.clear();
	lstLODgroups.clear();
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
		{
			// Render several passes
			PROF_EVENT("NORMAL_SHADER_PASSES");
			for ( u32 iPass = 0; iPass<SHADER_PASSES_MAX; ++iPass)
			{
				//mapNormalVS&	vs				= mapNormal	[_priority];
				mapNormalVS&	vs				= mapNormalPasses[_priority][iPass];
				for (mapNormalVS::TNode& Nvs : vs)
				{
					RCache.set_VS					(Nvs.key);
	
					//	GS setup
					mapNormalGS&		gs			= Nvs.val;
					for (mapNormalGS::TNode& Ngs : gs)
					{
						GRHI->SetShader(Ngs.key, ERHI_SHADER_TYPE::GS);
						mapNormalPS&		ps			= Ngs.val;
						for (mapNormalPS::TNode& Nps : ps)
						{
							GRHI->SetShader(Nps.key, ERHI_SHADER_TYPE::PS);	
							mapNormalCS&		cs			= Nps.val.mapCS;
							GRHI->SetShader(Nps.val.hs, ERHI_SHADER_TYPE::HS);
							GRHI->SetShader(Nps.val.ds, ERHI_SHADER_TYPE::DS);
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
											float LOD = calcLOD(Ni.ssa, Ni.R);
											RCache.LOD.set_LOD(LOD);
											if (Ni.geom)
											{
												RCache.set_Geometry(Ni.geom);
												RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Ni.vBase, 0, Ni.vCount, Ni.iBase, Ni.pCount);
												RCache.stat.r.s_static.add(Ni.vCount);
											}
											else
											{
												Ni.pVisual->Render(LOD);
											}
										}
										if(_clear)items.clear();
									}if(_clear) tex.clear();
								}if(_clear) states.clear();
							}if(_clear) cs.clear();
	
						}if(_clear) ps.clear();
					}if(_clear) gs.clear();
				}if(_clear) vs.clear();
			}
		}
	}

	{
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
				mapMatrixGS&		gs			= Nvs.val;
				for (mapMatrixGS::TNode& Ngs : gs)
				{
					GRHI->SetShader(Ngs.key, ERHI_SHADER_TYPE::GS);
	
					mapMatrixPS&		ps			= Ngs.val;
					for (mapMatrixPS::TNode& Nps : ps)
					{
						GRHI->SetShader(Nps.key, ERHI_SHADER_TYPE::PS);
						mapMatrixCS&		cs			= Nps.val.mapCS;
						GRHI->SetShader(Nps.val.hs, ERHI_SHADER_TYPE::HS);
						GRHI->SetShader(Nps.val.ds, ERHI_SHADER_TYPE::DS);
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
	
									mapMatrixItems& items = Ntex.val;
									auto& visuals = items.visuals;
									if(!visuals.empty())
									{
										for (_MatrixItem& Ni : visuals)
										{
											if (Ni.pVisual->shader == nullptr)
											{
												continue;
											}
											RCache.set_xform_world(Ni.Matrix);
											RImplementation.apply_object(Ni.pObject);
											RImplementation.apply_lmaterial();
	
											float LOD = calcLOD(Ni.ssa, Ni.pVisual->vis.sphere.R);
											RCache.LOD.set_LOD(LOD);
											Ni.pVisual->Render(LOD);
										}if (_clear)items.visuals.clear();
										continue;
									}
	
									auto& particles = items.particles;
									for (dxRender_Visual* pVisual : particles)
										pVisual->Render(0);
									if (_clear)items.particles.clear();
	
								}if(_clear) tex.clear();
							}if(_clear) states.clear();
						}if(_clear) cs.clear();
					}if(_clear) ps.clear();
				}if(_clear) gs.clear();
			}if(_clear) vs.clear();
		}
	}
}

ICF void RenderNode(mapSorted_Node& N, bool emissive = false)
{
	dxRender_Visual* V = N.val.pVisual;

	VERIFY(V && V->shader._get());
	RCache.set_Element(N.val.se);

	if (emissive)
	{
		GRHI->StateManager->SetRenderState(D3DRS_ALPHABLENDENABLE, TRUE);
		GRHI->StateManager->SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ONE);
		GRHI->StateManager->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
		RCache.set_ColorWriteEnable(D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE);
	}
	else if (RImplementation.GraphMain.val_bUI && N.val.se->flags.bEmissive)
	{
		RCache.set_ColorWriteEnable();
		GRHI->StateManager->SetRenderState(D3DRS_ZWRITEENABLE, TRUE);
	}

	if (V->dcast_ParticleCustom())
	{
		V->Render(0);
		return;
	}

	RCache.set_xform_world(N.val.Matrix);

	RImplementation.apply_object(N.val.pObject);
	RImplementation.apply_lmaterial();

	V->Render(calcLOD(N.val.ssa, V->vis.sphere.R));
}

ICF void sorted_L1(mapSorted_Node* N)
{
	RenderNode(*N);
}

ICF void RenderMap(mapSorted_T& Map, bool emissive = false)
{
	for (auto& Node : Map)
	{
		RenderNode(Node, emissive);
	}

	Map.clear();
}

void R_dsgraph_structure::r_dsgraph_render_ui()
{
	RenderMap(mapUI);
}

void R_dsgraph_structure::r_dsgraph_render_sorted_ui()
{
	RenderMap(mapUIEmissive);

	mapUISorted.traverseRL(sorted_L1);
	mapUISorted.clear();
}

void R_dsgraph_structure::r_dsgraph_render_hud()
{
	PROF_EVENT("r_dsgraph_render_hud");
	CHudInitializer initalizer(true, true);

	RenderMap(mapHUD);

}

void R_dsgraph_structure::r_dsgraph_render_hud_ui()
{
	PROF_EVENT("r_dsgraph_render_hud_ui");
	VERIFY(g_hud && g_hud->RenderActiveItemUIQuery());

	CHudInitializer initalizer(true, true);


	g_hud->RenderActiveItemUI();
}

void R_dsgraph_structure::r_dsgraph_render_sorted(bool render_hud)
{
	PROF_EVENT("r_dsgraph_render_sorted");

	mapSorted.traverseRL(sorted_L1);
	mapSorted.clear();

	if (render_hud)
	{
		r_dsgraph_render_sorted_hud();
	}
}

void R_dsgraph_structure::r_dsgraph_render_sorted_hud()
{
	PROF_EVENT("r_dsgraph_render_sorted_hud");

	CHudInitializer initalizer(true, true);
	auto velocity_target = RCache.get_RT(1);
	RCache.set_RT(nullptr, 1);
	RenderMap(mapHUDEmissive, true);
	RCache.set_RT(velocity_target, 1);

	if (g_hud && g_hud->RenderActiveItemUIQuery())
	{
		r_dsgraph_render_hud_ui();
	}

	mapHUDSorted.traverseRL(sorted_L1);
	mapHUDSorted.clear();
}

void R_dsgraph_structure::r_dsgraph_render_emissive()
{
	PROF_EVENT("r_dsgraph_render_emissive");

	RenderMap(mapEmissive);
}

void R_dsgraph_structure::r_dsgraph_render_scope()
{
	GPU_EVENT(SCOPE_BUFFER_RENDER);
	RImplementation.Target->copy_position();

	RImplementation.Target->u_setrt(NULL, NULL, RDepth);

	CHudInitializer initalizer(true);
	RenderMap(mapHUDScopeMask);
}

void R_dsgraph_structure::r_dsgraph_render_wmarks()
{
	PROF_EVENT("r_dsgraph_render_wmarks");

	RenderMap(mapWmark);
}

void R_dsgraph_structure::r_dsgraph_render_distort()
{
	PROF_EVENT("r_dsgraph_render_distort");
	RenderMap(mapDistort);

	CHudInitializer initalizer(true, true);
	RenderMap(mapHUDDistort);
}

//////////////////////////////////////////////////////////////////////////
// sub-space rendering - shortcut to render with frustum extracted from matrix
void	R_dsgraph_structure::r_dsgraph_render_subspace	(IRender_Sector* _sector, Fmatrix& mCombined, Fvector& _cop, bool _dynamic, bool _precise_portals, CObject* O, bool _static)
{
	if(!_sector) return;
	CFrustum	temp;
	temp.CreateFromMatrix			(mCombined,	FRUSTUM_P_ALL &(~FRUSTUM_P_NEAR));
	r_dsgraph_render_subspace		(_sector,&temp,mCombined,_cop,_dynamic,_precise_portals, O, _static);
}

// sub-space rendering - main procedure
void	R_dsgraph_structure::r_dsgraph_render_subspace	(IRender_Sector* _sector, CFrustum* _frustum, Fmatrix& mCombined, Fvector& _cop, bool _dynamic, bool _precise_portals, CObject* O, bool _static)
{
	PROF_EVENT("r_dsgraph_render_subspace")
	VERIFY							(_sector);

	// Shadow passes replace the camera frustum. Reflection passes keep their own copy
	// so they can run while the main view is using ViewBase.
	const bool isolated = PortalTraverser.own_clips;
	if (!isolated)
		marker++;			// !!! critical here
	CFrustum ViewSave;
	CFrustum light_frustum = *_frustum;
	if (!isolated)
	{
		ViewSave = RImplementation.ViewBase;
		RImplementation.ViewBase = light_frustum;
		View = &RImplementation.ViewBase;
	}
	else
	{
		View = &light_frustum;
	}

	if (_precise_portals && RImplementation.rmPortals)		{
		PROF_EVENT("precise_portals")
		// Check if camera is too near to some portal - if so force DualRender
		Fvector box_radius;		box_radius.set	(EPS_L*20,EPS_L*20,EPS_L*20);
		RImplementation.Sectors_xrc.box_options	(CDB::OPT_FULL_TEST);
		RImplementation.Sectors_xrc.box_query	(RImplementation.rmPortals,_cop,box_radius);
		for (int K=0; K<RImplementation.Sectors_xrc.r_count(); K++)
		{
			CPortal*	pPortal		= (CPortal*) RImplementation.Portals[RImplementation.rmPortals->get_tris()[RImplementation.Sectors_xrc.r_begin()[K].id].dummy];
			pPortal->bDualRender	= true;
		}
	}

	// Traverse sector/portal structure
	PortalTraverser.traverse(_sector, isolated ? light_frustum : RImplementation.ViewBase, _cop, mCombined, 0);
	{
		PROF_EVENT("add_static");
	// Determine visibility for static geometry hierrarhy
		if(_static && psDeviceFlags.test(rsDrawStatic))
		{
			for (u32 s_it = 0; s_it < PortalTraverser.r_sectors.size(); s_it++)
			{
				CSector* sector = (CSector*)PortalTraverser.r_sectors[s_it];
				dxRender_Visual* root = sector->root();
				xr_vector<CFrustum>& frustums = isolated
					? PortalTraverser.local_frustums[sector->index]
					: sector->r_frustums;
				for (u32 v_it = 0; v_it < frustums.size(); v_it++)
				{
					View = &frustums[v_it];
					add_Static((dxRender_Visual*)root, View->getMask());
				}
			}
		}
	}

	if (_dynamic && psDeviceFlags.test(rsDrawDynamic))
	{
		PROF_EVENT("add_dynamic")
		RImplementation.set_Object(0, this);

		// Traverse object database
		g_SpatialSpace->q_frustum
		(
			lstRenderables,
			ISpatial_DB::O_ORDERED,
			ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW,
			isolated ? light_frustum : RImplementation.ViewBase
		);

		// Determine visibility for dynamic part of scene
		for (u32 o_it=0; o_it<lstRenderables.size(); o_it++)
		{
			ISpatial*	spatial		= lstRenderables[o_it].get();
			if (!g_r_cull_tls.active)
				spatial->spatial_updatesector();
			CSector*	sector		= (CSector*)spatial->sector;
			if	(0==sector)										continue;	// disassociated from S/P structure
			if (isolated)
			{
				if (PortalTraverser.local_sector_marker[sector->index] != PortalTraverser.i_marker)
					continue;
			}
			else if (PortalTraverser.i_marker != sector->r_marker)
			{
				continue; // inactive (untouched) sector
			}
			xr_vector<CFrustum>& frustums = isolated
				? PortalTraverser.local_frustums[sector->index]
				: sector->r_frustums;
			for (u32 v_it=0; v_it<frustums.size(); v_it++)
			{
				View = &frustums[v_it];
				if (!View->testSphere_dirty(spatial->sphere.P, spatial->sphere.R))
				{
					continue;
				}

				// renderable
				IRenderable* renderable = spatial->dcast_Renderable();
				if (0 == renderable)				continue;					// unknown, but renderable object (r1_glow???)
				if (!isolated && Device.vCameraPosition.distance_to_sqr(renderable->renderable.xform.c)<=10000.f)
				{
					CKinematics* pKin = (CKinematics*)renderable->renderable.visual;
					if(pKin)
					{
						if ((spatial->type & ESPATIAL_TYPE::RENDERABLESHADOW) != ESPATIAL_TYPE::NONE)
						{
							pKin->CalculateBones(true);
						}
						if ((spatial->type & ESPATIAL_TYPE::RENDERABLE) != ESPATIAL_TYPE::NONE)
						{
							const CFrustum& camera = ViewSave;
							if(0==camera.testSphere_dirty(spatial->sphere.P, spatial->sphere.R))
							{
								pKin->CalculateBones(true);
							}
						}
					}
				}
				if(O && O->dcast_Renderable()==renderable) continue;

				const u32 phase_now = g_r_cull_tls.active ? g_r_cull_tls.phase : RImplementation.phase;
				if (phase_now != CRender::PHASE_SMAP)
				{
					RImplementation.set_Object(renderable, this);
				}

				renderable->renderable_Render(this);
			}
		}

		RImplementation.set_Object(0, this);
	}

	if (!isolated)
		RImplementation.ViewBase = ViewSave;
	View = nullptr;
}

#include "FHierrarhyVisual.h"
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
				pV->CalculateBones		(true);
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
		case MT_LOD1:
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
		case MT_LOD2:
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
		case MT_LOD3:
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
		case MT_LOD4:
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