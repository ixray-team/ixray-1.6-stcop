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

ICF float calcLOD(float ssa/*fDistSq*/, float R)
{
	return _sqrt(clampr((ssa - r_ssaGLOD_end)/(r_ssaGLOD_start-r_ssaGLOD_end),0.f,1.f));
}

void CDSGraphManager::r_dsgraph_render_graph_sorted(R_dsgraph::mapDSGraphItems& graph, bool _clear)
{
	for (DSGraphItem& item : graph)
	{
		dxRender_Visual* V = item.pVisual;
		VERIFY(V && V->shader._get());
		RCache.set_Element(item.pSE);
		RCache.set_xform_world(*item.pMatrix);
		RImplementation.apply_object(item.pObject);
		RImplementation.apply_lmaterial();
		//if (item.b_hud_mode)
		//{
		//	//new feature
		//}
		V->Render(calcLOD(item.ssa, V->vis.sphere.R));
	}

	if (_clear)
		graph.clear();

	RCache.set_xform_world(Fidentity);
}

void CDSGraphManager::r_dsgraph_render_graph(R_dsgraph::mapDSGraphPasses* graph, u32 _priority, bool _clear, bool static_geometry)
{
	RCache.set_xform_world(Fidentity);

	for (u32 iPass = 0; iPass < SHADER_PASSES_MAX; ++iPass)
	{
		mapDSGraphVS& vs = graph[_priority][iPass];
		for (mapDSGraphVS::TNode& Nvs : vs)
		{
			RCache.set_VS(Nvs.key);

#ifdef USE_DX11
			mapDSGraphGS& gs = Nvs.val;
			for (mapDSGraphGS::TNode& Ngs : gs)
			{
				RCache.set_GS(Ngs.key);

				mapDSGraphPS& ps = Ngs.val;
#else //USE_DX11
				mapDSGraphPS& ps = Nvs.val;
#endif
				for (mapDSGraphPS::TNode& Nps : ps)
				{
					RCache.set_PS(Nps.key);
#ifdef USE_DX11
					mapDSGraphCS& cs = Nps.val.mapCS;
					RCache.set_HS(Nps.val.hs);
					RCache.set_DS(Nps.val.ds);
#else
					mapDSGraphCS& cs = Nps.val;
#endif
					for (mapDSGraphCS::TNode& Ncs : cs)
					{
						RCache.set_Constants(Ncs.key);

						mapDSGraphStates& states = Ncs.val;
						for (mapDSGraphStates::TNode& Nstate : states)
						{
							RCache.set_States(Nstate.key);

							mapDSGraphTextures& tex = Nstate.val;
							for (mapDSGraphTextures::TNode& Ntex : tex)
							{
								RCache.set_Textures(Ntex.key);
								RImplementation.apply_lmaterial();

								mapDSGraphItems& items = Ntex.val;
								for (DSGraphItem& Ni : items)
								{
									if(!static_geometry)
									{
										RCache.set_xform_world(*Ni.pMatrix);
										RImplementation.apply_object(Ni.pObject);
										RImplementation.apply_lmaterial();
									}

									float LOD = calcLOD(Ni.ssa, Ni.pVisual->vis.sphere.R);
#ifdef USE_DX11
									RCache.LOD.set_LOD(LOD);
#endif
									Ni.pVisual->Render(LOD);
								}
								if (_clear)items.clear();
							}
							if (_clear) tex.clear();
						}
						if (_clear) states.clear();
					}
					if (_clear) cs.clear();
				}
				if (_clear) ps.clear();
#ifdef USE_DX11
			}
			if (_clear) gs.clear();
#endif //USE_DX11
		}
		if (_clear) vs.clear();
	}
}

//////////////////////////////////////////////////////////////////////////
// HUD render
void CDSGraphManager::r_dsgraph_render_hud()
{
	PROF_EVENT("r_dsgraph_render_hud");
	CHudInitializer initalizer(true);

	// Rendering
	RImplementation.rmNear();
	r_dsgraph_render_graph_sorted(RGraph.mapHUD);
#if	RENDER==R_R1
	if (g_hud && g_hud->RenderActiveItemUIQuery())
		r_dsgraph_render_hud_ui();				// hud ui
#endif

	RImplementation.rmNormal();
#if	RENDER==R_R4
	if(!RGraph.mapHUDSorted.ScopeLens.empty())
	{
		PROF_EVENT("r_dsgraph_render_hud_scope");
		ID3D11Resource* res{};
		RDepth->GetResource(&res);
		RContext->CopyResource(RImplementation.Target->rt_Position->pSurface, res);
		_RELEASE(res);

		r_dsgraph_render_graph_sorted(RGraph.mapHUDSorted.ScopeLens);
	}
#endif
}

void CDSGraphManager::r_dsgraph_render_hud_ui()
{
	PROF_EVENT("r_dsgraph_render_hud_ui");
	VERIFY(g_hud && g_hud->RenderActiveItemUIQuery());

	CHudInitializer initalizer(true);

#if	RENDER==R_R2
	// Targets, use accumulator for temporary storage
	const ref_rt rt_null;
	RCache.set_RT(0, 1);
	RCache.set_RT(0, 2);
	RImplementation.Target->u_setrt(RImplementation.Target->rt_Color, rt_null, rt_null, RDepth);
#endif

	RImplementation.rmNear();
	g_hud->RenderActiveItemUI();
	RImplementation.rmNormal();
}

//////////////////////////////////////////////////////////////////////////
// strict-sorted render

//void	R_dsgraph_structure::r_dsgraph_render_sorted	(bool render_hud)
void CDSGraphManager::r_dsgraph_render_sorted(bool render_hud)
{
	{
		PROF_EVENT("r_dsgraph_render_sorted");
		// Rendering
		r_dsgraph_render_graph_sorted(RGraph.mapStaticSorted.Sorted);
		r_dsgraph_render_graph_sorted(RGraph.mapDynamicSorted.Sorted);
	}

	if (render_hud)
		r_dsgraph_render_sorted_hud();
}
void CDSGraphManager::r_dsgraph_capture_hud()
{
	if (g_hud)
	{
		g_hud->Render_Last(dcast_IPortalTraverser());
		set_Object();
	}
}
void CDSGraphManager::r_dsgraph_render_sorted_hud()
{
	PROF_EVENT("r_dsgraph_render_sorted_hud");
#if	RENDER==R_R4
	RContext->CopyResource(RImplementation.Target->rt_Accumulator->pSurface, RImplementation.Target->rt_Generic_0->pSurface);
#endif
	CHudInitializer initalizer(true);

	RImplementation.rmNear();
	r_dsgraph_render_graph_sorted(RGraph.mapHUDSorted.Sorted);
	RImplementation.rmNormal();
}

//////////////////////////////////////////////////////////////////////////
// strict-sorted render
void CDSGraphManager::r_dsgraph_render_emissive()
{
	PROF_EVENT("r_dsgraph_render_emissive");
#if	RENDER!=R_R1
	// Rendering
	r_dsgraph_render_graph_sorted(RGraph.mapStaticSorted.Emissive);
	r_dsgraph_render_graph_sorted(RGraph.mapDynamicSorted.Emissive);
	//	HACK: Calculate this only once
	CHudInitializer initalizer(true);

	RImplementation.rmNear();
	r_dsgraph_render_graph_sorted(RGraph.mapHUDSorted.Emissive);
	RImplementation.rmNormal();
#endif
}
// strict-sorted render


//////////////////////////////////////////////////////////////////////////
// strict-sorted render
void CDSGraphManager::r_dsgraph_render_wmarks()
{
	PROF_EVENT("r_dsgraph_render_wmarks");
#if	RENDER!=R_R1
	// Rendering
	r_dsgraph_render_graph_sorted(RGraph.mapStaticSorted.Wmark);
	r_dsgraph_render_graph_sorted(RGraph.mapDynamicSorted.Wmark);
	//	HACK: Calculate this only once
	CHudInitializer initalizer(true);

	RImplementation.rmNear();
	r_dsgraph_render_graph_sorted(RGraph.mapHUDSorted.Wmark);
	RImplementation.rmNormal();
#endif
}

//////////////////////////////////////////////////////////////////////////
// strict-sorted render
void CDSGraphManager::r_dsgraph_render_distort()
{
	PROF_EVENT("r_dsgraph_render_distort");
	// Rendering
	r_dsgraph_render_graph_sorted(RGraph.mapStaticSorted.Distort);
	r_dsgraph_render_graph_sorted(RGraph.mapDynamicSorted.Distort);
	//	HACK: Calculate this only once
	CHudInitializer initalizer(true);

	RImplementation.rmNear();
	r_dsgraph_render_graph_sorted(RGraph.mapHUDSorted.Distort);
	RImplementation.rmNormal();
}
#include"LightTrack.h"
void CDSGraphManager::r_dsgraph_capture_static()
{
	PROF_EVENT("r_dsgraph_capture_static")
	if (i_start)
	{
		// Traverse sector/portal structure
		if (psDeviceFlags.test(rsDrawStatic))
		{
			// Determine visibility for static geometry hierrarhy
			for (auto& pair : m_sector_frustums)
			{
				for (auto& frustum_node : pair.val.frustums)
					add_Static((IRenderVisual*)pair.key->root(), frustum_node, frustum_node.getMask());
			}
		}
	}
}
static void dbg_text_renderer(const Fvector& pos, u32 color = color_rgba(0, 255, 100, 255), shared_str str = "+")
{
	Fvector4		v_res;
	Device.mFullTransform.transform(v_res, pos);

	float x = (1.f + v_res.x) / 2.f * (Device.Width);
	float y = (1.f - v_res.y) / 2.f * (Device.Height);

	if (v_res.z < 0 || v_res.w < 0)
		return;

	if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y>1.f)
		return;

	g_FontManager->pFontSystem->SetAligment(CGameFont::alCenter);
	g_FontManager->pFontSystem->SetColor(color);
	g_FontManager->pFontSystem->Out(x, y, "%s", str.c_str());
}

void CDSGraphManager::r_dsgraph_capture_lights()
{
	PROF_EVENT("r_dsgraph_capture_lights")
	g_SpatialSpaceLights->q_frustum
	(
		lstLights,
		ISpatial_DB::O_ORDERED,
		STYPE_LIGHTSOURCE,
		i_frustum
	);

#if	RENDER==R_R1
	std::sort(lstLights.begin(), lstLights.end(), [](ISpatialShared& _1, ISpatialShared& _2)
	{
		if (!_1.get() || !_2.get()) return false;

		return	_1->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition_saved) < _2->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition_saved);
	});
#endif

	for (ISpatialShared& SH : lstLights)
	{
		ISpatial* spatial = SH.get();
		if (0 == spatial) continue; spatial->spatial_updatesector();
		CSector* sector = (CSector*)spatial->spatial.sector;
		if (0 == sector) continue;

		if (!RImplementation.HOM.visible(spatial->spatial.sphere)) continue;

		if ((spatial->spatial.type & STYPE_LIGHTSOURCE))
		{
			// lightsource
			if (light* L = (light*)(spatial->dcast_Light()))
			{
#if	RENDER==R_R1
				RImplementation.L_DB->add_light(L);
#else
				if (L->get_LOD() > EPS_L && L->has_light_visible_from_sectors(*this))
				{
					//dbg_text_renderer(L->SpatialComponent->spatial.sphere.P);
					RImplementation.Lights.add_light(L);
				}
#endif
			}
		}
	}
}

void CDSGraphManager::r_dsgraph_capture_dynamic(CObject* O)
{
	PROF_EVENT("r_dsgraph_capture_dynamic")
	if (i_start)
	{
		if (psDeviceFlags.test(rsDrawDynamic))
		{
			// Traverse object database
			g_SpatialSpace->q_frustum
			(
				lstRenderables,
				ISpatial_DB::O_ORDERED,
				i_doptions,
				i_frustum
			);
			set_Object();
#if	RENDER==R_R1
			if (i_mask[CDSGraphManager::fl_normal])//normal phase
			{
				std::sort(lstRenderables.begin(), lstRenderables.end(), [](ISpatialShared& _1, ISpatialShared& _2)
				{
					if (!_1.get() || !_2.get()) return false;

					return	_1->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition_saved) < _2->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition_saved);
				});

				if (psGameFlags.test(rsActorShadow))
					g_hud->Render_First(dcast_IPortalTraverser());

				r_dsgraph_capture_hud();
			}
#endif
			u32 uID_LTRACK = u32(-1);
			if (i_mask[CDSGraphManager::fl_normal])//normal phase
			{
				// update light-vis for current entity / actor
				if (CObject* O = g_pGameLevel->CurrentViewEntity())
				{
					if (!O->getDestroy())
					{
						if (CROS_impl* R = (CROS_impl*)O->ROS())
							R->update(O);
					}
				}

				RImplementation.uLastLTRACK++;
				if (!lstRenderables.empty())
				{
					uID_LTRACK = RImplementation.uLastLTRACK % lstRenderables.size();
#if	RENDER!=R_R1
					// update light-vis for selected entity
					// track lighting environment
					if (IRenderable* renderable = (IRenderable*)lstRenderables[uID_LTRACK]->dcast_Renderable())
					{
						if (CROS_impl* T = (CROS_impl*)renderable->renderable_ROS())
							T->update(renderable);
					}
#endif
				}

			}

			// Determine visibility for dynamic part of scene
			for (u32 o_it = 0; o_it < lstRenderables.size(); o_it++)
			{
				ISpatial* spatial = lstRenderables[o_it].get();
				if (0 == spatial) continue;
				CSector* sector = (CSector*)spatial->spatial.sector;
				if (0 == sector) continue;

				if (i_mask[CDSGraphManager::fl_normal] && !RImplementation.HOM.visible(spatial->spatial.sphere))
					continue;

#if	RENDER==R_R1
				if ((spatial->spatial.type & STYPE_GLOW))
				{
					if (CGlow* glow = spatial->dcast_CGlow())
					{
						// It may be an glow
						RImplementation.L_Glows->add(glow);
					}
					continue;
				}
#endif

//				if ((spatial->spatial.type & STYPE_LIGHTSOURCE))
//				{
//					// lightsource
//					if (light* L = (light*)(spatial->dcast_Light()))
//					{
//#if	RENDER==R_R1
//						RImplementation.L_DB->add_light(L);
//#else
//						if (L->get_LOD() > EPS_L && L->has_light_visible_from_sectors(PT))
//						{
//							//dbg_text_renderer(L->SpatialComponent->spatial.sphere.P);
//							RImplementation.Lights.add_light(L);
//						}
//#endif
//					}
//					continue;
//				}

				if(!(spatial->spatial.type & STYPE_RENDERABLE) && !(spatial->spatial.type & STYPE_PARTICLE) && !(spatial->spatial.type & STYPE_RENDERABLESHADOW))
					continue;
				if (!is_sector_visible(sector))
					continue;

				for (CFrustum& frustum : m_sector_frustums.find(sector)->val.frustums)
				{
					if (frustum.testSphere_dirty(spatial->spatial.sphere.P, spatial->spatial.sphere.R))
					{
						// renderable
						IRenderable* renderable = spatial->dcast_Renderable();
						if (0 == renderable) break;

						if (O && O->dcast_Renderable() == renderable) break;

						// Rendering
#if	RENDER==R_R1
						if (i_mask[CDSGraphManager::fl_normal] && o_it == uID_LTRACK && renderable->renderable_ROS())
						{
							// track lighting environment
							if(CROS_impl* T = (CROS_impl*)renderable->renderable_ROS())
								T->update(renderable);
						}
#endif
						if (i_mask[CDSGraphManager::fl_normal] && !(spatial->spatial.type & STYPE_PARTICLE))
							set_Object(renderable);

						renderable->renderable_Render(dcast_IPortalTraverser());

						if (i_mask[CDSGraphManager::fl_normal] && !(spatial->spatial.type & STYPE_PARTICLE))
							set_Object();
						break;
					}
				}
			}
		}
	}
}

void CDSGraphManager::r_dsgraph_capture(bool lights, bool dynamic, CObject* O)
{
	PROF_EVENT("r_dsgraph_capture")
	r_dsgraph_capture_static();

	if(lights)
		r_dsgraph_capture_lights();

	if (dynamic)
		r_dsgraph_capture_dynamic(O);
}
