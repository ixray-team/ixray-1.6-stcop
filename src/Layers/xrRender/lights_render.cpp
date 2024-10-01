#include "stdafx.h"
#include "../../xrEngine/xr_object.h"
#include "FBasicVisual.h"
#include "SkeletonCustom.h"
void	CRender::render_lights	(light_Package& LP)
{
	//////////////////////////////////////////////////////////////////////////
	// Refactor order based on ability to pack shadow-maps
	// 1. calculate area + sort in descending order
	// const	u16		smap_unassigned		= u16(-1);
	if(!LP.v_shadowed.empty())
	{
		PROF_EVENT("GETTING_OCCQ_RESPONDS")
		xr_vector<light*>& source = LP.v_shadowed;
		source.erase(std::remove_if(source.begin(), source.end(), 
		[](light* L)
		{
			if(L->flags.bOccq)
			{
				L->vis_update();
				if(!L->vis.visible)
					return true;
			}
	
			//if(!L->flags.bHudMode)
			//	L->optimize_smap_size();
			//else
			//{
				L->X.S.posX	= 0;
				L->X.S.posY	= 0;
				L->X.S.size	= SMAP_adapt_max;
				L->X.S.transluent = FALSE;
			//}

			return false;
		}), source.end());
	}

	if(!LP.v_shadowed.empty())
	{
		xr_vector<xr_vector<light*>> m_lights_pool={};
		START_PROFILE("SHADOWED_LIGHTS_GENERATE_POOL")
		xr_vector<light*>& source = LP.v_shadowed;		
		std::sort(source.begin(),source.end(),[](light* _1, light* _2){return _1->X.S.size>_2->X.S.size;});	
		while(!source.empty())
		{
			PROF_EVENT("GENERATE_CHUNK")
			LP_smap_pool.initialize(RImplementation.o.smapsize);

			xr_vector<light*>chunk_lights={};
			source.erase(std::remove_if(source.begin(), source.end(), 
			[&](light* L)
			{
				PROF_EVENT("TRY_APPEND_LIGHT")
				SMAP_Rect R;
				if(LP_smap_pool.push(R,L->X.S.size))
				{
					// OK
					L->X.S.posX			= R.min.x;
					L->X.S.posY			= R.min.y;
					chunk_lights.insert(chunk_lights.begin(), L);
					return true;
				}
				return false;
			}), source.end());

			if(!chunk_lights.empty())
				m_lights_pool.insert(m_lights_pool.begin(), std::move(chunk_lights));
		}
		STOP_PROFILE // SHADOWED_LIGHTS_GENERATE_POOL

		PROF_EVENT("SHADOWED_LIGHTS_RENDER")
		for (auto &chunk_lights : m_lights_pool)
		{
			START_PROFILE("LIGHTS_CHUNK_CALC")
			Target->phase_smap_spot_clear();
			stats.s_used		++;
			for(light* L : chunk_lights)
			{
				PROF_EVENT("LIGHT_SOURCE_CALC")
				// render
				phase = PHASE_SMAP;
				r_pmask	(true, !!RImplementation.o.Tshadows);

				bool decorative_light = false;
				if (L->flags.bHudMode)
				{
					RImplementation.marker			++;			// !!! critical here
					RImplementation.set_Object		(0);
					CSector*	sector		= (CSector*)L->spatial.sector;
					dxRender_Visual*	root	= sector->root();
					for (u32 v_it=0; v_it<sector->r_frustums.size(); v_it++)	{
						set_Frustum			(&(sector->r_frustums[v_it]));
						add_Geometry		(root);
					}
					decorative_light = true;
				}
				else
				{
					if((L->decor_object[0]&&!L->decor_object[0]->getDestroy()) || (L->decor_object[1]&&!L->decor_object[1]->getDestroy()) || (L->decor_object[2]&&!L->decor_object[2]->getDestroy()) || (L->decor_object[3]&&!L->decor_object[3]->getDestroy()) || (L->decor_object[4]&&!L->decor_object[4]->getDestroy()) || (L->decor_object[5]&&!L->decor_object[5]->getDestroy()))
					{
						RImplementation.marker			++;			// !!! critical here
						RImplementation.set_Object		(0);
						for (int f=0; f<6; f++)
						{
							if(L->decor_object[f]&&!L->decor_object[f]->getDestroy())
							{
								L->decor_object[f]->renderable_Render();
								decorative_light = true;
							}
						}
					}
					else
						r_dsgraph_render_subspace(L->spatial.sector, L->X.S.combine, L->position, TRUE, FALSE, L->ignore_object);
				}
				bool	bNormal							= mapNormalPasses[0][0].size() || mapMatrixPasses[0][0].size();
				bool	bSpecial						= mapNormalPasses[1][0].size() || mapMatrixPasses[1][0].size() || mapSorted.size();
				if ( bNormal || bSpecial)
				{
					stats.s_merged						++;
					Target->phase_smap_spot				(L);
					RCache.set_xform_world				(Fidentity);
					RCache.set_xform_view				(L->X.S.view);
					RCache.set_xform_project			(L->X.S.project);
					r_dsgraph_render_graph				(0);
					if (ps_r2_ls_flags.test(R2FLAG_LIGHTS_DETAILS) && 
						psDeviceFlags.test(rsDetails) &&
						Details->dtFS &&
						RImplementation.pOutdoorSector && PortalTraverser.i_marker == RImplementation.pOutdoorSector->r_marker &&
						L->flags.bShadow && !decorative_light && L->spatial.sphere.P.distance_to_sqr(RDEVICE.vCameraPosition) < 1600.f)
					{
						RCache.set_CullMode		(CULL_NONE);
						RCache.set_xform_world	(Fidentity);
						RCache.set_Geometry		(Details->hw_Geom);
						Details->hw_Render(L);
						RCache.set_CullMode		(CULL_CCW);
					}
					L->X.S.transluent					= FALSE;
					if (bSpecial)
					{
						L->X.S.transluent					= TRUE;
						Target->phase_smap_spot_tsh			(L);
						r_dsgraph_render_graph				(1);			// normal level, secondary priority
						r_dsgraph_render_sorted				( );			// strict-sorted geoms
					}
				}
				else
					stats.s_finalclip					++;

				r_pmask(true,false);
			}
			STOP_PROFILE // LIGHTS_CHUNK_CALC

			START_PROFILE("LIGHTS_CHUNK_RENDER")
			for (auto it : chunk_lights)
			{
				PROF_EVENT("LIGHT_SOURCE_RENDER")
				Target->accum_spot(it);
				if (ps_r2_ls_flags.test(R2FLAG_VOLUMETRIC_LIGHTS))
					Target->accum_volumetric(it);
			}
			STOP_PROFILE // LIGHTS_CHUNK_RENDER
		}
	}

	{
		// Point lighting (unshadowed, if left)
		PROF_EVENT("POINT_LIGHTS_ACCUM")
		for	(auto it : LP.v_point)
		{
			if(it->flags.bOccq&&!it->flags.bHudMode)
			{
				it->vis_update();
				if (it->vis.visible)
					Target->accum_point(it);
			}
			else
				Target->accum_point(it);
		}
	}

	{
		// Spot lighting (unshadowed, if left)
		PROF_EVENT("SPOT_LIGHTS_ACCUM")
		for	(auto it : LP.v_spot)
		{
			if(it->flags.bOccq&&!it->flags.bHudMode)
			{
				it->vis_update();
				if (it->vis.visible)
					Target->accum_spot(it);
			}
			else
				Target->accum_spot(it);
		}
	}
}
