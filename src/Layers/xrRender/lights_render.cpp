#include "stdafx.h"
#include "../../xrEngine/xr_object.h"
#include "FBasicVisual.h"
#include "SkeletonCustom.h"
static void dbg_text_renderer(const Fvector& pos, u32 color = color_rgba(0,255,100,255), shared_str str = "+")
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
void	CRender::render_lights	(light_Package& LP)
{
	{
		GPU_EVENT(SHADOWED_LIGHTS);
		{
			GPU_EVENT(PHASE_VIS_UPDATE);
			xr_vector<light*>& source = LP.v_shadowed;
			source.erase(std::remove_if(source.begin(), source.end(), [](light* L)
			{
				if(L->m_parent)
				{
					if(L->m_parent->omnipart[0] == L)
					{
						L->m_parent->vis_update();
						for (int f = 0; f < 6; f++)
						{
							L->m_parent->omnipart[f]->vis.pending = L->m_parent->vis.pending;
							L->m_parent->omnipart[f]->vis.visible = L->m_parent->vis.visible;
						}
					}
				}
				else
					L->vis_update();
				if (!L->vis.visible)
					return true;

				//dbg_text_renderer(L->SpatialComponent->spatial.sphere.P);
				L->optimize_smap_size();

				return false;
			}), source.end());
		}

		{
			GPU_EVENT(PHASE_CALC_POOLS);
			xr_vector<light*>& source = LP.v_shadowed;
			static xr_vector<light*> refactored;
			refactored.clear();
			u32 total = (u32)source.size();

			for (u16 smap_ID = 0; refactored.size() != total; smap_ID++)
			{
				LP_smap_pool.initialize(o.smapsize);
				std::sort(source.begin(), source.end(), [](light* _1, light* _2) {return _1->X.S.size > _2->X.S.size; });
				source.erase(std::remove_if(source.begin(), source.end(), [smap_ID](light* L)
				{
					SMAP_Rect R;
					if (RImplementation.LP_smap_pool.push(R, L->X.S.size))
					{
						L->X.S.posX = R.min.x;
						L->X.S.posY = R.min.y;
						L->vis.smap_ID = smap_ID;
						refactored.push_back(L);
						return true;
					}
					return false;
				}), source.end());
			}

			std::reverse(refactored.begin(), refactored.end());
			LP.v_shadowed = refactored;
		}

		//////////////////////////////////////////////////////////////////////////
		// sort lights by importance???
		// while (has_any_lights_that_cast_shadows) {
		//		if (has_point_shadowed)		->	generate point shadowmap
		//		if (has_spot_shadowed)		->	generate spot shadowmap
		//		switch-to-accumulator
		//		if (has_point_unshadowed)	-> 	accum point unshadowed
		//		if (has_spot_unshadowed)	-> 	accum spot unshadowed
		//		if (was_point_shadowed)		->	accum point shadowed
		//		if (was_spot_shadowed)		->	accum spot shadowed
		//	}
		//	if (left_some_lights_that_doesn't cast shadows)
		//		accumulate them
		while (!LP.v_shadowed.empty())
		{
			// if (has_spot_shadowed)
			static xr_vector<light*> L_spot_s;
			{
				GPU_EVENT(GENERATE_SHMAPS);
				// generate spot shadowmap
				Target->phase_smap_spot_clear();
				xr_vector<light*>& source = LP.v_shadowed;
				light* L = source.back();
				u16			sid = L->vis.smap_ID;
				while (!source.empty())
				{
					if (source.empty())		break;
					L = source.back();
					if (L->vis.smap_ID != sid)	break;
					source.pop_back();
					// render
					phase = PHASE_SMAP;

					GPU_EVENT(RENDER_SHADOWS);
					bool decorative_light = false;
					if (L->flags.bHudMode)
					{
						L_spot_s.push_back(L);
						decorative_light = true;
					}
					else
					{
						if ((L->decor_object[0] && !L->decor_object[0]->getDestroy()) || (L->decor_object[1] && !L->decor_object[1]->getDestroy()) || (L->decor_object[2] && !L->decor_object[2]->getDestroy()) || (L->decor_object[3] && !L->decor_object[3]->getDestroy()) || (L->decor_object[4] && !L->decor_object[4]->getDestroy()) || (L->decor_object[5] && !L->decor_object[5]->getDestroy()))
						{
							L->GMLight.m_visuals_dynamic.clear();
							for (int f = 0; f < 6; f++)
							{
								if (L->decor_object[f] && !L->decor_object[f]->getDestroy())
								{
									L->decor_object[f]->renderable_Render(&L->GMLight);
									decorative_light = true;
								}
							}
						}
						else
						{
							if (L->m_moving_frames<32u)
							{
								L->GMLight.RGraph.clear_static();
								L->GMLight.traverse((CSector*)L->SpatialComponent->spatial.sector, L->X.S.frustum, L->position, L->X.S.combine);
								L->GMLight.r_dsgraph_capture_static();
								//dbg_text_renderer(L->SpatialComponent->spatial.sphere.P);
								L->m_moving_frames++;
							}
							L->GMLight.m_visuals_dynamic.clear();
							L->GMLight.r_dsgraph_capture_dynamic(L->ignore_object);
						}
					}

					bool bDeffered_Shadows = L->GMLight.RGraph.mapStaticPasses[0][0].size() || L->GMLight.RGraph.mapDynamicPasses[0][0].size();
					bool bForward_Shadows = L->GMLight.RGraph.mapStaticPasses[1][0].size() || L->GMLight.RGraph.mapDynamicPasses[1][0].size() || L->GMLight.RGraph.mapStaticSorted.Sorted.size() || L->GMLight.RGraph.mapDynamicSorted.Sorted.size();
					if (bDeffered_Shadows || bForward_Shadows)
					{
						L_spot_s.push_back(L);
						Target->phase_smap_spot(L);
						RCache.set_xform_world(Fidentity);
						RCache.set_xform_view(L->X.S.view);
						RCache.set_xform_project(L->X.S.project);
						L->GMLight.r_dsgraph_render_static(0, false);
						L->GMLight.r_dsgraph_render_dynamic(0, true);
						if (ps_r2_ls_flags.test(R2FLAG_LIGHTS_DETAILS) &&
							psDeviceFlags.test(rsDetails) &&
							Details->dtFS &&
							L->flags.bShadow && !decorative_light && L->SpatialComponent->spatial.sphere.P.distance_to_sqr(RDEVICE.vCameraPosition) < 1600.f/*_sqr(40.f) && L->has_outdoor_light()*/)
						{
							Details->hw_Render(L);
						}
					
						L->X.S.transluent = FALSE;
						if (bForward_Shadows)
						{
							L->X.S.transluent = TRUE;
							Target->phase_smap_spot_tsh(L);
					
							L->GMLight.r_dsgraph_render_static(1, false);
							L->GMLight.r_dsgraph_render_dynamic(1, true);
					
							L->GMLight.r_dsgraph_render_sorted();			// strict-sorted geoms
						}
					}
					else if (L->flags.bVolumetric && ps_r2_ls_flags.test(R2FLAG_VOLUMETRIC_LIGHTS))
					{
						L_spot_s.push_back(L);
					}
				}
			}
			//		if (was_spot_shadowed)		->	accum spot shadowed
			if (!L_spot_s.empty())
			{
				PROF_EVENT("ACCUM_SPOT");
				for (light* L : L_spot_s)
				{
					Target->accum_spot(L);
					if (L->flags.bVolumetric && ps_r2_ls_flags.test(R2FLAG_VOLUMETRIC_LIGHTS))
						Target->accum_volumetric(L);
				}

				L_spot_s.clear();
			}
		}
	}

	{
		GPU_EVENT(UNSHADOWED_LIGHTS);
		{
			GPU_EVENT(POINT_LIGHTS_ACCUM_UNSH);
			// Point lighting (unshadowed, if left)
			if (!LP.v_point.empty())
			{
				for (light* L : LP.v_point)
				{
					L->vis_update();
					if (!L->vis.visible)
						continue;

					Target->accum_point(L);
				}
				LP.v_point.clear();
			}
		}
		{
			GPU_EVENT(SPOT_LIGHTS_ACCUM_UNSH);
			// Spot lighting (unshadowed, if left)
			if (!LP.v_spot.empty())
			{
				for (light* L : LP.v_spot)
				{
					L->vis_update();
					if (!L->vis.visible)
						continue;

					Target->accum_spot(L);
				}
				LP.v_spot.clear();
			}
		}
	}
}
