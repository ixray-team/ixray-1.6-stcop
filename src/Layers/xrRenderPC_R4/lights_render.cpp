#include "stdafx.h"
#include "../../xrEngine/xr_object.h"
#include "FBasicVisual.h"
#include "SkeletonCustom.h"

void	CRender::render_lights	(GfxCmdList& Cmd, light_Package& LP)
{
	Target->BeginSpotShadowCacheFrame();
	//////////////////////////////////////////////////////////////////////////
	// Refactor order based on ability to pack shadow-maps
	// 1. calculate area + sort in descending order
	// const	u16		smap_unassigned		= u16(-1);
	{
		PROF_EVENT("vis_update");
		xr_vector<light*>&	source			= LP.v_shadowed;
		for (u32 it=0; it<source.size(); it++)
		{
			light*	L		= source[it];
			if(L->flags.bOccq)
			{
				L->vis_update	();
				if	(!L->vis.visible)	{
					source.erase		(source.begin()+it);
					it--;
				}
				else
				{
					if(!L->flags.bHudMode)
						L->optimize_smap_size();
				}
			}
			else
			{
				if(!L->flags.bHudMode)
					L->optimize_smap_size();
			}
		}
	}

	// 2. refactor - infact we could go from the backside and sort in ascending order
	{
		PROF_EVENT("PHASE_SORT");
		xr_vector<light*>&		source		= LP.v_shadowed;
		xr_vector<light*>		refactored	;
		refactored.reserve		(source.size());
		u32						total		= (u32)source.size();

		for		(u16 smap_ID=0; refactored.size()!=total; smap_ID++)
		{
			LP_smap_pool.initialize	(RImplementation.o.smapsize);
			std::sort				(source.begin(),source.end(),[](light* _1, light* _2){return _1->X.S.size>_2->X.S.size;});
			for	(u32 test=0; test<source.size(); test++)
			{
				light*	L	= source[test];
				SMAP_Rect	R;
				if		(LP_smap_pool.push(R,L->X.S.size))	{
					// OK
					L->X.S.posX			= R.min.x;
					L->X.S.posY			= R.min.y;
					L->vis.smap_ID		= smap_ID;
					refactored.push_back(L);
					source.erase		(source.begin()+test);
					test				--;
				}
			}
		}

		// save (lights are popped from back)
		std::reverse	(refactored.begin(),refactored.end());
		LP.v_shadowed	= refactored;
	}
	{
		PROF_EVENT("SHADOWED_LIGHTS");
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
			xr_vector<light*>	L_spot_s;
			L_spot_s.reserve(LP.v_shadowed.size());
			stats.s_used++;

			// generate spot shadowmap
			Target->phase_smap_spot_clear();
			xr_vector<light*>& source = LP.v_shadowed;
			light* L = source.back();
			u16 sid = L->vis.smap_ID;

			while (!source.empty())
			{
				L = source.back();
				if (L->vis.smap_ID != sid)
					break;

				source.pop_back();
				if (!Target->CheckSpotShadowGraph())
				{
					++stats.s_finalclip;
					continue;
				}

				// render
				phase = PHASE_SMAP;

				if (Target->CanCacheSpotShadow(*L))
				{
					if (Target->RenderSpotShadow(*L))
					{
						++stats.s_merged;
						L_spot_s.push_back(L);
					}
					else
					{
						++stats.s_finalclip;
					}
				}
				else
				{
					Target->CountLegacySpotShadow();
					PROF_EVENT("SHADOWED_LIGHTS_RENDER_SUBSPACE");

					GraphView ShadowView = MakeGraphView(PHASE_SMAP);
					ShadowView.SetSubspace(L->SpatialComponent->sector, L->X.S.combine, L->position);
					ShadowView.PassMask[1] = ps_r2_ls_flags_ext.test(R4FLAG_TSHDOWS);
					ShadowView.CalculateSkeletons = true;

					const bool HasDecorObject = (L->decor_object[0] && !L->decor_object[0]->getDestroy()) || (L->decor_object[1] && !L->decor_object[1]->getDestroy()) || (L->decor_object[2] && !L->decor_object[2]->getDestroy()) || (L->decor_object[3] && !L->decor_object[3]->getDestroy()) || (L->decor_object[4] && !L->decor_object[4]->getDestroy()) || (L->decor_object[5] && !L->decor_object[5]->getDestroy());

					bool decorative_light = false;
					R_dsgraph_structure* Graph = &GraphPass;
					if (L->flags.bHudMode)
					{
						GraphPass.Begin(ShadowView);
						L_spot_s.push_back(L);
						decorative_light = true;
					}
					else if (HasDecorObject)
					{
						GraphPass.Begin(ShadowView);
						for (int f = 0; f < 6; f++)
						{
							if (L->decor_object[f] && !L->decor_object[f]->getDestroy())
							{
								L->decor_object[f]->renderable_Render(&GraphPass);
								decorative_light = true;
							}
						}
					}
					else
					{
						Graph = &BuildPassGraph(Cmd, ShadowView, L->ignore_object);
					}

					bool bNormal = Graph->HasPasses(0);
					bool bSpecial = Graph->HasPasses(1) || Graph->mapSorted.size();
					if (bNormal || bSpecial)
					{
						stats.s_merged++;
						L_spot_s.push_back(L);
						Target->phase_smap_spot(Cmd, L);
						Cmd.SetObjectTransform(Fidentity);
						Cmd.SetView(L->X.S.view, L->X.S.project);
						Graph->r_dsgraph_render_graph(0);

						if (ps_r2_ls_flags.test(R2FLAG_LIGHTS_DETAILS) &&
							psDeviceFlags.is(rsDetails) &&
							GetDetails() && GetDetails()->dtSlots &&
							L->flags.bShadow && !decorative_light && L->SpatialComponent->sphere.P.distance_to_sqr(RDEVICE.vCameraPosition) <= _sqr(40.f + L->SpatialComponent->sphere.R))
						{
							GetDetails()->hw_Render(Cmd, &L->X.S.combine);
						}

						L->X.S.transluent = false;
						if (bSpecial)
						{
							L->X.S.transluent = true;
							Target->phase_smap_spot_tsh(Cmd, L);

							{
								PROF_EVENT("SHADOWED_LIGHTS_RENDER_GRAPH");
								Graph->r_dsgraph_render_graph(1); // normal level, secondary priority
							}

							{
								PROF_EVENT("SHADOWED_LIGHTS_RENDER_SORTED");
								Graph->r_dsgraph_render_sorted(false); // strict-sorted geoms
							}
						}
					}
					else
					{
						stats.s_finalclip++;
					}
				}
			}
			{
				PROF_EVENT("UNSHADOWED_LIGHTS");
				Target->phase_accumulator(Cmd);
				{
					PROF_EVENT("POINT_LIGHTS");

					if (!LP.v_point.empty())
					{
						light* L_ = LP.v_point.back();
						LP.v_point.pop_back();

						if (L_->flags.bOccq && !L_->flags.bHudMode)
						{
							L_->vis_update();
							if (L_->vis.visible)
							{
								Target->accum_point(Cmd, L_);
							}
						}
						else
						{
							Target->accum_point(Cmd, L_);
						}
					}
				}

				{
					PROF_EVENT("SPOT_LIGHTS");

					if (!LP.v_spot.empty())
					{
						light* L_ = LP.v_spot.back();
						LP.v_spot.pop_back();
						if (L_->flags.bOccq && !L_->flags.bHudMode)
						{
							L_->vis_update();
							if (L_->vis.visible)
							{
								Target->accum_spot(Cmd, L_);
							}
						}
						else
						{
							Target->accum_spot(Cmd, L_);
						}
					}
				}
			}

			if (!L_spot_s.empty())
			{
				PROF_EVENT("ACCUM_SPOT");
				for (u32 it = 0; it < L_spot_s.size(); it++)
				{
					Target->accum_spot(Cmd, L_spot_s[it]);
					if (ps_r2_ls_flags.is(R2FLAG_VOLUMETRIC_LIGHTS))
					{
						Target->accum_volumetric_lv(Cmd, L_spot_s[it]);
					}
				}

				L_spot_s.clear();
			}
		}
	}
	{
		PROF_EVENT("POINT_LIGHTS_ACCUM");
		if (!LP.v_point.empty())
		{
			xr_vector<light*>& Lvec = LP.v_point;
			for (u32 pid = 0; pid < Lvec.size(); pid++)
			{
				if (Lvec[pid]->flags.bOccq && !Lvec[pid]->flags.bHudMode)
				{
					Lvec[pid]->vis_update();
					if (Lvec[pid]->vis.visible)
						Target->accum_point(Cmd, Lvec[pid]);
				}
				else
				{
					Target->accum_point(Cmd, Lvec[pid]);
				}
			}
			Lvec.clear();
		}
	}
	{
		PROF_EVENT("SPOT_LIGHTS_ACCUM");
		if (!LP.v_spot.empty())
		{
			xr_vector<light*>& Lvec = LP.v_spot;
			for (u32 pid = 0; pid < Lvec.size(); pid++)
			{
				if (Lvec[pid]->flags.bOccq && !Lvec[pid]->flags.bHudMode)
				{
					Lvec[pid]->vis_update();
					if (Lvec[pid]->vis.visible)
						Target->accum_spot(Cmd, Lvec[pid]);
				}
				else
				{
					Target->accum_spot(Cmd, Lvec[pid]);
				}
			}
			Lvec.clear();
		}
	}
}
