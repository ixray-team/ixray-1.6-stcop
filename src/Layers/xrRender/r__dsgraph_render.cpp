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
#ifdef USE_DX11
				RCache.set_VS					(Nvs.key);

				//	GS setup
				mapNormalGS&		gs			= Nvs.val;
				for (mapNormalGS::TNode& Ngs : gs)
				{
					GRHI->SetShader(Ngs.key, ERHI_SHADER_TYPE::GS);
					mapNormalPS&		ps			= Ngs.val;
#else //USE_DX11
					GRHI->SetShader(Nvs.key, ERHI_SHADER_TYPE::VS);
					mapNormalPS&		ps			= Nvs.val;
#endif
					for (mapNormalPS::TNode& Nps : ps)
					{
						GRHI->SetShader(Nps.key, ERHI_SHADER_TYPE::PS);	
#ifdef USE_DX11
						mapNormalCS&		cs			= Nps.val.mapCS;
						GRHI->SetShader(Nps.val.hs, ERHI_SHADER_TYPE::HS);
						GRHI->SetShader(Nps.val.ds, ERHI_SHADER_TYPE::DS);
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
#ifdef USE_DX11
			RCache.set_VS					(Nvs.key);	
			mapMatrixGS&		gs			= Nvs.val;
			for (mapMatrixGS::TNode& Ngs : gs)
			{
				GRHI->SetShader(Ngs.key, ERHI_SHADER_TYPE::GS);

				mapMatrixPS&		ps			= Ngs.val;
#else //USE_DX11
				GRHI->SetShader(Nvs.key, ERHI_SHADER_TYPE::VS);
				mapMatrixPS&		ps			= Nvs.val;
#endif
				for (mapMatrixPS::TNode& Nps : ps)
				{
					GRHI->SetShader(Nps.key, ERHI_SHADER_TYPE::PS);
#ifdef USE_DX11
					mapMatrixCS&		cs			= Nps.val.mapCS;
					GRHI->SetShader(Nps.val.hs, ERHI_SHADER_TYPE::HS);
					GRHI->SetShader(Nps.val.ds, ERHI_SHADER_TYPE::DS);
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

								mapMatrixItems& items = Ntex.val;
								auto& visuals = items.visuals;
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
#ifdef USE_DX11
									RCache.LOD.set_LOD(LOD);
#endif
									Ni.pVisual->Render(LOD);
								}if(_clear)items.visuals.clear();

								auto& particles = items.particles;
								for (dxRender_Visual* pVisual : particles)
									pVisual->Render(0);
								if (_clear)items.particles.clear();

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
void R_dsgraph_structure::r_dsgraph_render_ui()
{
	mapUI.traverseLR(sorted_L1);
	mapUI.clear();
}

void R_dsgraph_structure::r_dsgraph_render_sorted_ui()
{
#if	RENDER!=R_R1
	mapUIEmissive.traverseLR(sorted_L1);
	mapUIEmissive.clear();
#endif

	mapUISorted.traverseLR(sorted_L1);
	mapUISorted.clear();
}

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
	RCache.set_RT(nullptr,	1);
	RCache.set_RT(nullptr,	2);
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
void R_dsgraph_structure::r_dsgraph_render_scope	()
{
#if	RENDER==R_R4
	GPU_EVENT(SCOPE_BUFFER_RENDER);
	RImplementation.Target->copy_position();

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
		PROF_EVENT("add_static");
	// Determine visibility for static geometry hierrarhy
		if(psDeviceFlags.test(rsDrawStatic))
		{
			for (u32 s_it = 0; s_it < PortalTraverser.r_sectors.size(); s_it++)
			{
				CSector* sector = (CSector*)PortalTraverser.r_sectors[s_it];
				dxRender_Visual* root = sector->root();
				for (u32 v_it = 0; v_it < sector->r_frustums.size(); v_it++)
				{
					set_Frustum(&(sector->r_frustums[v_it]));
					add_Geometry(root);
				}
			}
		}
	}

	if (_dynamic && psDeviceFlags.test(rsDrawDynamic))
	{
		PROF_EVENT("add_dynamic")
		set_Object						(0);

		// Traverse object database
		g_SpatialSpace->q_frustum
			(
			lstRenderables,
			ISpatial_DB::O_ORDERED,
			ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW,
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
						if ((spatial->spatial.type & ESPATIAL_TYPE::RENDERABLESHADOW) != ESPATIAL_TYPE::NONE)
						{
							pKin->CalculateBones(TRUE);
						}
						if ((spatial->spatial.type & ESPATIAL_TYPE::RENDERABLE) != ESPATIAL_TYPE::NONE)
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

#include "dxRenderDeviceRender.h"

void R_dsgraph_structure::renderImGuiDebugWindow_SVGStorage()
{
	if (ImGui::Begin("Render Debug - SVG Storage"))
	{
		if (DEV)
		{
			CSVGStorage* pStorage = DEV->GetSVGStorage();

			if (pStorage)
			{
				if (ImGui::CollapsingHeader("Runtime"))
				{
					CTextureAtlas* pDefault = pStorage->get_atlas(_kSVGStorage_DefaultAtlasID);

					auto p_atlas_draw = [](const CTextureAtlas* pAtlas)->void {

						static bool _ViewerState_EnableDeleting = false;
						static xr_stack_string<256> _ViewerState_QueryResult;
						static float _ViewerState_QueryWidth = 0.0f;
						static float _ViewerState_QueryHeight = 0.0f;

						char name[32];
						std::sprintf(name, "[%d] %s", pAtlas->getID(), _kSVGStorage_DefaultAtlasName);

						if (ImGui::CollapsingHeader(name))
						{
							const auto& elements = pAtlas->getElements();

							ImGui::SeparatorText("Atlas Info");

							ImGui::Text("width: %.2f", float(pAtlas->getWidth()));
							ImGui::Text("height: %.2f", float(pAtlas->getHeight()));

							ImGui::Checkbox("Deleting", &_ViewerState_EnableDeleting);

							ImGui::DragFloat("w", &_ViewerState_QueryWidth);
							ImGui::DragFloat("h", &_ViewerState_QueryHeight);

							if (ImGui::Button("find nearest"))
							{
								const auto* pElement = pAtlas->findNearest(_ViewerState_QueryWidth, _ViewerState_QueryHeight);

								if (pElement)
								{
									std::sprintf(_ViewerState_QueryResult.data(), "w: %.2f h: %.2f\nx: %.2f y: %.2f\nu0: %.2f v0: %.2f u1: %.2f v1: %.2f", pElement->w(), pElement->h(), pElement->x(), pElement->y(), pElement->u0(pAtlas->getWidth()), pElement->v0(pAtlas->getHeight()), pElement->u1(pAtlas->getWidth()), pElement->v1(pAtlas->getHeight()));
								}
								else
								{
									_ViewerState_QueryResult.clear();
									std::sprintf(_ViewerState_QueryResult.data(), "failed to obtain element!");
								}
							}

							if (_ViewerState_QueryResult.empty() == false)
							{
								ImGui::SameLine();
								if (ImGui::Button("Reset"))
								{
									_ViewerState_QueryResult.clear();
								}

								ImGui::Text("Nearest Query:");
								ImGui::Text("%s", _ViewerState_QueryResult.c_str());
							}


							ImGui::SeparatorText("Elements");
							ImGui::Text("amount: %zu", elements.size());

							ImGui::SeparatorText("Atlas");

							float atlasPixelW = pAtlas->getWidth();
							float atlasPixelH = pAtlas->getHeight();

							ImVec2 atlasDisplaySize = ImVec2((float)atlasPixelW, (float)atlasPixelH);

							ImGui::Image(pAtlas->getResource(), atlasDisplaySize, ImVec2(0, 0), ImVec2(1, 1), ImVec4(1, 1, 1, 1), ImVec4(1, 1, 1, 1));

							ImVec2 atlasMin = ImGui::GetItemRectMin();
							ImVec2 atlasMax = ImGui::GetItemRectMax();
							ImVec2 atlasOnScreenSize = ImVec2(atlasMax.x - atlasMin.x,
								atlasMax.y - atlasMin.y);

							float scaleX = atlasOnScreenSize.x / (float)atlasPixelW;
							float scaleY = atlasOnScreenSize.y / (float)atlasPixelH;

							ImVec2 parentCursorBackup = ImGui::GetCursorPos();

							int hoveredIndex = -1;
							ImVec2   hoveredSubMin, hoveredSubSize;

							ImVec2 mousePos = ImGui::GetMousePos();

							bool break_called = false;
							u32 hovered_icon_w;
							u32 hovered_icon_h;
							float hovered_icon_x;
							float hovered_icon_y;

							float hovered_icon_u0;
							float hovered_icon_v0;
							float hovered_icon_u1;
							float hovered_icon_v1;

							int i = 0;
							for (const auto& element : elements)
							{

								ImVec2 subMin = ImVec2(
									atlasMin.x + 1 + element.x() * scaleX,
									atlasMin.y + 1 + element.y() * scaleY
								);

								ImVec2 subSize = ImVec2(
									element.w() * scaleX,
									element.h() * scaleY
								);
								ImVec2 subMax = ImVec2(subMin.x + subSize.x,
									subMin.y + subSize.y);


								if (mousePos.x >= subMin.x && mousePos.x <= subMax.x &&
									mousePos.y >= subMin.y && mousePos.y <= subMax.y)
								{
									hoveredIndex = i;

									hovered_icon_w = element.w();
									hovered_icon_h = element.h();
									hovered_icon_x = element.x();
									hovered_icon_y = element.y();

									hovered_icon_u0 = element.u0(pAtlas->getWidth());
									hovered_icon_v0 = element.v0(pAtlas->getHeight());

									hovered_icon_u1 = element.u1(pAtlas->getWidth());
									hovered_icon_v1 = element.v1(pAtlas->getHeight());

									hoveredSubMin = subMin;
									hoveredSubSize = subSize;
									break_called = true;
									break; // stop after first hit (assuming subregions donâ€™t overlap)
								}

								++i;
							}

							i = 0;

							bool hovered_icon_clicked = false;
							for (const auto& element : elements)
							{
								ImVec2 subMin = ImVec2(
									atlasMin.x + 1 + element.x() * scaleX,
									atlasMin.y + 1 + element.y() * scaleY
								);
								ImVec2 subSize = ImVec2(
									element.w() * scaleX,
									element.h() * scaleY
								);


								ImU32 borderColor = (i == hoveredIndex)
									? IM_COL32(255, 255, 0, 255) // yellow
									: IM_COL32(255, 0, 0, 255); // red


								ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
								ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0, 0, 0, 0));

								ImGuiWindowFlags childFlags =
									ImGuiWindowFlags_NoTitleBar |
									ImGuiWindowFlags_NoResize |
									ImGuiWindowFlags_NoMove |
									ImGuiWindowFlags_NoScrollbar |
									ImGuiWindowFlags_NoScrollWithMouse |
									ImGuiWindowFlags_NoSavedSettings;

								ImGui::SetCursorScreenPos(subMin);
								ImGui::BeginChild(
									("SubRegion##" + std::to_string(i)).c_str(),
									subSize,
									/*border=*/false,
									childFlags
								);


								ImDrawList* dl = ImGui::GetWindowDrawList();
								ImVec2 rectMin = ImVec2(subMin.x - 0.5f,
									subMin.y);
								ImVec2 rectMax = ImVec2(subMin.x + subSize.x + 0.5f,
									subMin.y + subSize.y);

								dl->AddRect(rectMin,
									rectMax,
									borderColor,
									0.0f,
									0,
									2.0f);


								ImGui::Dummy(subSize);

								if (i == hoveredIndex)
								{
									hovered_icon_clicked = ImGui::IsItemClicked();
								}

								ImGui::EndChild();
								ImGui::PopStyleColor();
								ImGui::PopStyleVar();


								ImGui::SetCursorPos(parentCursorBackup);

								++i;
							}


							if (hoveredIndex >= 0 && hovered_icon_w && hovered_icon_h)
							{
								bool clicked = ImGui::IsItemClicked();

								if (_ViewerState_EnableDeleting)
								{
									if (hovered_icon_clicked)
									{
										// yeah slow (prob dumb), but it is for debug purposes, so there's no need to point out on that thing, seriously :/
										// upd: we don't need to make removeElement as const since it is obvious write operation and must be accessible only when we have non const pointer (like we don't read, but this viewer is for reading mainly)
										const_cast<CTextureAtlas*>(pAtlas)->removeElement(hovered_icon_w, hovered_icon_h);
									}
								}

								ImGui::BeginTooltip();
								ImGui::Text("Lookup id: %d", hoveredIndex);
								ImGui::SeparatorText("Dimensions");
								ImGui::Text("w=%.2f h=%.2f", float(hovered_icon_w), float(hovered_icon_h), hovered_icon_x, hovered_icon_y);
								ImGui::SeparatorText("Offset");
								ImGui::Text("x=%.2f y=%.2f", hovered_icon_x, hovered_icon_y);
								ImGui::SeparatorText("UV");
								ImGui::Text("u0=%.2f v0=%.2f\nu1=%.2f v1=%.2f", hovered_icon_u0, hovered_icon_v0, hovered_icon_u1, hovered_icon_v1);


								ImGui::EndTooltip();
							}
						}
						};

					p_atlas_draw(pDefault);
					const auto& atlases = pStorage->get_atlases();

					for (const auto& atlas : atlases)
					{
						p_atlas_draw(&atlas);
					}
				}

				if (ImGui::CollapsingHeader("Cache"))
				{

				}
			}
		}
	}

	ImGui::End();
}