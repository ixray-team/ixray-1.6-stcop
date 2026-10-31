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
	lstLODgroups.clear();
}

void R_dsgraph_structure::r_dsgraph_render_graph(u32 _priority, bool _clear)
{
	PROF_EVENT("r_dsgraph_render_graph");
	//GPU_EVENT(r_dsgraph_render_graph);
	CScopeTimer Timer(Device.Statistic->RenderDUMP);
	GfxCmdList& Cmd = *Backend;

	// **************************************************** NORMAL
	// Perform sorting based on ScreenSpaceArea
	// Sorting by SSA and changes minimizations
	{
		Cmd.SetObjectTransform(Fidentity);
		{
			// Render several passes
			PROF_EVENT("NORMAL_SHADER_PASSES");
			for ( u32 iPass = 0; iPass<SHADER_PASSES_MAX; ++iPass)
			{
				//mapNormalVS&	vs				= mapNormal	[_priority];
				mapNormalVS&	vs				= mapNormalPasses[_priority][iPass];
				for (mapNormalVS::TNode& Nvs : vs)
				{
					Cmd.SetVS					(Nvs.key);
	
					//	GS setup
					mapNormalGS&		gs			= Nvs.val;
					for (mapNormalGS::TNode& Ngs : gs)
					{
						Cmd.SetShader(Ngs.key, ERHI_SHADER_TYPE::GS);
						mapNormalPS&		ps			= Ngs.val;
						for (mapNormalPS::TNode& Nps : ps)
						{
							Cmd.SetShader(Nps.key, ERHI_SHADER_TYPE::PS);	
							mapNormalCS&		cs			= Nps.val.mapCS;
							Cmd.SetShader(Nps.val.hs, ERHI_SHADER_TYPE::HS);
							Cmd.SetShader(Nps.val.ds, ERHI_SHADER_TYPE::DS);
							for (mapNormalCS::TNode& Ncs : cs)
							{
								Cmd.SetConstantTable			(Ncs.key);
	
								mapNormalStates&	states		= Ncs.val;
								for (mapNormalStates::TNode& Nstate : states)
								{
									Cmd.SetStates					(Nstate.key);
	
									mapNormalTextures&		tex			= Nstate.val;
									for (mapNormalTextures::TNode& Ntex : tex)
									{
										Cmd.SetTextures					(Ntex.key);
										RImplementation.apply_lmaterial		(Cmd);
	
										mapNormalItems&				items	= Ntex.val;
										for (_NormalItem& Ni : items)
										{
											float LOD = calcLOD(Ni.ssa, Ni.R);
											Cmd.Constants.SetLOD(LOD);
											if (Ni.geom)
											{
												Cmd.SetGeometry(Ni.geom);
												Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Ni.pCount * 3, Ni.iBase, Ni.vBase);
											}
											else
											{
												Ni.pVisual->Render(Cmd, LOD);
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
				Cmd.SetVS					(Nvs.key);	
				mapMatrixGS&		gs			= Nvs.val;
				for (mapMatrixGS::TNode& Ngs : gs)
				{
					Cmd.SetShader(Ngs.key, ERHI_SHADER_TYPE::GS);
	
					mapMatrixPS&		ps			= Ngs.val;
					for (mapMatrixPS::TNode& Nps : ps)
					{
						Cmd.SetShader(Nps.key, ERHI_SHADER_TYPE::PS);
						mapMatrixCS&		cs			= Nps.val.mapCS;
						Cmd.SetShader(Nps.val.hs, ERHI_SHADER_TYPE::HS);
						Cmd.SetShader(Nps.val.ds, ERHI_SHADER_TYPE::DS);
						for (mapMatrixCS::TNode& Ncs : cs)
						{
							Cmd.SetConstantTable			(Ncs.key);
	
							mapMatrixStates&	states		= Ncs.val;
							for (mapMatrixStates::TNode& Nstate : states)
							{
								Cmd.SetStates					(Nstate.key);
	
								mapMatrixTextures&		tex			= Nstate.val;
								for (mapMatrixTextures::TNode& Ntex : tex)
								{
									Cmd.SetTextures					(Ntex.key);
	
									mapMatrixItems& items = Ntex.val;
									for (_MatrixItem& Ni : items)
									{
										if (Ni.pVisual->shader == nullptr)
										{
											continue;
										}
										Cmd.SetObjectTransform(Ni.Matrix);
										RImplementation.apply_object(Cmd, Ni.pObject);
										RImplementation.apply_lmaterial(Cmd);
	
										float LOD = calcLOD(Ni.ssa, Ni.pVisual->vis.sphere.R);
										Cmd.Constants.SetLOD(LOD);
										Ni.pVisual->Render(Cmd, LOD);
									}if (_clear) items.clear();
								}if(_clear) tex.clear();
							}if(_clear) states.clear();
						}if(_clear) cs.clear();
					}if(_clear) ps.clear();
				}if(_clear) gs.clear();
			}if(_clear) vs.clear();
		}
	}
}

ICF void RenderNode(GfxCmdList& Cmd, mapSorted_Node& N, bool emissive = false)
{
	dxRender_Visual* V = N.val.pVisual;

	VERIFY(V && V->shader._get());
	Cmd.SetPass(N.val.se);

	const auto* Settings = RImplementation.GraphMain.val_bUI ? Device.m_pRender->GetUIIconLighting() : nullptr;
	Hlsl::float4& Tint = Cmd.Constants.Write<CBUIIcon>().ui_emissive_tint;
	if (Settings)
	{
		Tint.set(Settings->Color.x * Settings->Intensity, Settings->Color.y * Settings->Intensity, Settings->Color.z * Settings->Intensity, 1.0f);
	}
	else
	{
		Tint.set(1.0f, 1.0f, 1.0f, 1.0f);
	}

	if (emissive)
	{
		Cmd.SetBlend(true, D3DBLEND_ONE, D3DBLEND_ONE);
		Cmd.SetColorWriteEnable(D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE);
	}
	else if (RImplementation.GraphMain.val_bUI && N.val.se->flags.bEmissive)
	{
		Cmd.SetColorWriteEnable();
		Cmd.SetDepthWrite(true);
	}

	if (V->dcast_ParticleCustom())
	{
		V->Render(Cmd, 0);
		return;
	}

	Cmd.SetObjectTransform(N.val.Matrix);

	RImplementation.apply_object(Cmd, N.val.pObject);
	RImplementation.apply_lmaterial(Cmd);

	V->Render(Cmd, calcLOD(N.val.ssa, V->vis.sphere.R));
}

ICF void sorted_L1(GfxCmdList& Cmd, mapSorted_Node* N)
{
	RenderNode(Cmd, *N);
}

ICF void RenderMap(GfxCmdList& Cmd, mapSorted_T& Map, bool emissive = false)
{
	for (auto& Node : Map)
	{
		RenderNode(Cmd, Node, emissive);
	}

	Map.clear();
}

void R_dsgraph_structure::r_dsgraph_render_ui()
{
	GfxCmdList& Cmd = *Backend;
	RenderMap(Cmd, mapUI);
}

void R_dsgraph_structure::r_dsgraph_render_sorted_ui()
{
	GfxCmdList& Cmd = *Backend;
	RenderMap(Cmd, mapUIEmissive);

	mapUISorted.traverseRL([&](mapSorted_Node* N) { sorted_L1(Cmd, N); });
	mapUISorted.clear();
}

void R_dsgraph_structure::r_dsgraph_render_hud()
{
	PROF_EVENT("r_dsgraph_render_hud");
	GfxCmdList& Cmd = *Backend;
	CHudInitializer initalizer(Cmd, true, true);

	RenderMap(Cmd, mapHUD);

}

void R_dsgraph_structure::r_dsgraph_render_hud_ui()
{
	PROF_EVENT("r_dsgraph_render_hud_ui");
	VERIFY(g_hud && g_hud->RenderActiveItemUIQuery());

	GfxCmdList& Cmd = *Backend;
	CHudInitializer initalizer(Cmd, true, true);


	g_hud->RenderActiveItemUI();
}

void R_dsgraph_structure::r_dsgraph_render_sorted(bool render_hud)
{
	PROF_EVENT("r_dsgraph_render_sorted");
	GfxCmdList& Cmd = *Backend;

	mapSorted.traverseRL([&](mapSorted_Node* N) { sorted_L1(Cmd, N); });
	mapSorted.clear();

	if (render_hud)
	{
		r_dsgraph_render_sorted_hud();
	}
}

void R_dsgraph_structure::r_dsgraph_render_sorted_hud()
{
	PROF_EVENT("r_dsgraph_render_sorted_hud");

	GfxCmdList& Cmd = *Backend;
	CHudInitializer initalizer(Cmd, true, true);
	auto velocity_target = Cmd.GetRenderTarget(1);
	Cmd.SetRenderTarget(nullptr, 1);
	RenderMap(Cmd, mapHUDEmissive, true);
	Cmd.SetRenderTarget(velocity_target, 1);

	if (g_hud && g_hud->RenderActiveItemUIQuery())
	{
		r_dsgraph_render_hud_ui();
	}

	mapHUDSorted.traverseRL([&](mapSorted_Node* N) { sorted_L1(Cmd, N); });
	mapHUDSorted.clear();
}

void R_dsgraph_structure::r_dsgraph_render_emissive()
{
	PROF_EVENT("r_dsgraph_render_emissive");
	GfxCmdList& Cmd = *Backend;

	RenderMap(Cmd, mapEmissive);
}

void R_dsgraph_structure::r_dsgraph_render_scope()
{
	GPU_EVENT(SCOPE_BUFFER_RENDER);
	GfxCmdList& Cmd = *Backend;
	RImplementation.Target->copy_position();

	RImplementation.Target->u_setrt(Cmd, NULL, NULL, RDepth);

	CHudInitializer initalizer(Cmd, true);
	RenderMap(Cmd, mapHUDScopeMask);
}

void R_dsgraph_structure::r_dsgraph_render_wmarks()
{
	PROF_EVENT("r_dsgraph_render_wmarks");
	GfxCmdList& Cmd = *Backend;

	RenderMap(Cmd, mapWmark);
}

void R_dsgraph_structure::r_dsgraph_render_distort()
{
	PROF_EVENT("r_dsgraph_render_distort");
	GfxCmdList& Cmd = *Backend;
	RenderMap(Cmd, mapDistort);

	CHudInitializer initalizer(Cmd, true, true);
	RenderMap(Cmd, mapHUDDistort);
}

//////////////////////////////////////////////////////////////////////////
// sub-space rendering - shortcut to render with frustum extracted from matrix
void	R_dsgraph_structure::r_dsgraph_render_subspace	(IRender_Sector* _sector, Fmatrix& mCombined, Fvector& _cop, bool _dynamic, bool _precise_portals, CObject* O, bool _static)
{
	// The editors have no sectors: collect their scene for the frustum
	if (Device.IsEditorMode())
	{
		CFrustum	EditorFrustum;
		EditorFrustum.CreateFromMatrix	(mCombined,	FRUSTUM_P_ALL &(~FRUSTUM_P_NEAR));
		RImplementation.EditorRenderSubspace(*Backend, EditorFrustum);
		return;
	}

	if(!_sector) return;
	CFrustum	temp;
	temp.CreateFromMatrix			(mCombined,	FRUSTUM_P_ALL &(~FRUSTUM_P_NEAR));
	r_dsgraph_render_subspace		(_sector,&temp,mCombined,_cop,_dynamic,_precise_portals, O, _static);
}

// sub-space rendering - main procedure
void	R_dsgraph_structure::r_dsgraph_render_subspace	(IRender_Sector* _sector, CFrustum* _frustum, Fmatrix& mCombined, Fvector& _cop, bool _dynamic, bool _precise_portals, CObject* O, bool _static)
{
	PROF_EVENT("r_dsgraph_render_subspace")
	if (Device.IsEditorMode())
	{
		RImplementation.EditorRenderSubspace(*Backend, *_frustum);
		return;
	}
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
	GfxCmdList&	Cmd			= *Backend;
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
						Cmd.SetPass			(E_,pass);
						V->Render					(Cmd, -1.f);
					}
				}
			}
			break;
		}
	}
}