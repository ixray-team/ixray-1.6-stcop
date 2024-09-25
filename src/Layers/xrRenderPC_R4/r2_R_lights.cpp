#include "stdafx.h"
#include "../../xrEngine/xr_object.h"
IC		bool	pred_area		(light* _1, light* _2)
{
	u32		a0		= _1->X.S.size;
	u32		a1		= _2->X.S.size;
	return	a0>a1;	// reverse -> descending
}
#include "../xrRender/FBasicVisual.h"
#include "../xrRender/SkeletonCustom.h"
void	CRender::render_lights	(light_Package& LP)
{
	//////////////////////////////////////////////////////////////////////////
	// Refactor order based on ability to pack shadow-maps
	// 1. calculate area + sort in descending order
	// const	u16		smap_unassigned		= u16(-1);
	{
		xr_vector<light*>&	source			= LP.v_shadowed;
		for (u32 it=0; it<source.size(); it++)
		{
			light*	L		= source[it];
			if(L->flags.bOccq&&!L->flags.bHudMode)
			{
				L->vis_update	();
				if	(!L->vis.visible)	{
					source.erase		(source.begin()+it);
					it--;
				} else {
					LR.compute_xf_spot	(L);
				}
			}
			else
				LR.compute_xf_spot	(L);
		}
	}

	// 2. refactor - infact we could go from the backside and sort in ascending order
	{
		xr_vector<light*>&		source		= LP.v_shadowed;
		xr_vector<light*>		refactored	;
		refactored.reserve		(source.size());
		u32						total		= (u32)source.size();

		for		(u16 smap_ID=0; refactored.size()!=total; smap_ID++)
		{
			LP_smap_pool.initialize	(RImplementation.o.smapsize);
			std::sort				(source.begin(),source.end(),pred_area);
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

   PIX_EVENT(SHADOWED_LIGHTS);

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
	HOM.Disable	();
	while		(LP.v_shadowed.size() )
	{
		// if (has_spot_shadowed)
		xr_vector<light*>	L_spot_s;
		stats.s_used		++;

		// generate spot shadowmap
		Target->phase_smap_spot_clear	();
		xr_vector<light*>&	source		= LP.v_shadowed;
		light*		L		= source.back	()	;
		u16			sid		= L->vis.smap_ID	;
		while (source.size())	
		{
			if	(source.empty())		break;
			L	= source.back			();
			if	(L->vis.smap_ID!=sid)	break;
			source.pop_back				();
			if(L->flags.bOccq&&!L->flags.bHudMode)
				Lights_LastFrame.push_back	(L);

			// render
			phase									= PHASE_SMAP;
			if (RImplementation.o.Tshadows)	r_pmask	(true,true	);
			else							r_pmask	(true,false	);
			PIX_EVENT(SHADOWED_LIGHTS_RENDER_SUBSPACE);

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

			if(L->flags.bOccq&&!L->flags.bHudMode)
				L->svis.begin();
			
			bool	bNormal							= mapNormalPasses[0][0].size() || mapMatrixPasses[0][0].size();
			bool	bSpecial						= mapNormalPasses[1][0].size() || mapMatrixPasses[1][0].size() || mapSorted.size();
			if ( bNormal || bSpecial)	{
				stats.s_merged						++;
				L_spot_s.push_back					(L);
				Target->phase_smap_spot				(L);
				RCache.set_xform_world				(Fidentity);
				RCache.set_xform_view				(L->X.S.view);
				RCache.set_xform_project			(L->X.S.project);
				r_dsgraph_render_graph				(0);
				if (ps_r2_ls_flags.test(R2FLAG_LIGHTS_DETAILS) && 
					psDeviceFlags.is(rsDetails) &&
					Details->dtFS &&
					L->flags.bShadow && !decorative_light && L->spatial.sphere.P.distance_to_sqr(RDEVICE.vCameraPosition) < _sqr(40.f))
				{
					RCache.set_CullMode		(CULL_NONE);
					RCache.set_xform_world	(Fidentity);
					RCache.set_Geometry		(Details->hw_Geom);
					Details->hw_Render(L);
					RCache.set_CullMode		(CULL_CCW);
				}
				L->X.S.transluent					= FALSE;
				if (bSpecial)						{
					L->X.S.transluent					= TRUE;
					Target->phase_smap_spot_tsh			(L);
               PIX_EVENT(SHADOWED_LIGHTS_RENDER_GRAPH);
					r_dsgraph_render_graph				(1);			// normal level, secondary priority
               PIX_EVENT(SHADOWED_LIGHTS_RENDER_SORTED);
					r_dsgraph_render_sorted				( );			// strict-sorted geoms
				}
			} else {
				stats.s_finalclip					++;
			}

			if(L->flags.bOccq&&!L->flags.bHudMode)
				L->svis.end								();
			r_pmask									(true,false);
		}

      PIX_EVENT(UNSHADOWED_LIGHTS);

      //		switch-to-accumulator
		Target->phase_accumulator			();
		HOM.Disable							();
  
      PIX_EVENT(POINT_LIGHTS);
  
		//		if (has_point_unshadowed)	-> 	accum point unshadowed
		if		(!LP.v_point.empty())	{
			light*	L_	= LP.v_point.back	();		LP.v_point.pop_back		();
			if(L_->flags.bOccq&&!L_->flags.bHudMode)
			{
				L_->vis_update				();
				if (L_->vis.visible)			{ 
					Target->accum_point		(L_);
				}
			}
			else
				Target->accum_point		(L_);
		}

      PIX_EVENT(SPOT_LIGHTS);

      //		if (has_spot_unshadowed)	-> 	accum spot unshadowed
		if		(!LP.v_spot.empty())	{
			light*	L_	= LP.v_spot.back	();		LP.v_spot.pop_back			();
			if(L_->flags.bOccq&&!L_->flags.bHudMode)
			{
				L_->vis_update				();
				if (L_->vis.visible)			{ 
					LR.compute_xf_spot		(L_);
					Target->accum_spot		(L_);
				}
			}
			else
			{
				LR.compute_xf_spot		(L_);
				Target->accum_spot		(L_);
			}
		}

      PIX_EVENT(SPOT_LIGHTS_ACCUM_VOLUMETRIC);
		
      //		if (was_spot_shadowed)		->	accum spot shadowed
	  if (!L_spot_s.empty())
	  {
		  PIX_EVENT(ACCUM_SPOT);
		  for (u32 it = 0; it < L_spot_s.size(); it++)
		  {
			  Target->accum_spot(L_spot_s[it]);
			  if (ps_r2_ls_flags.is(R2FLAG_VOLUMETRIC_LIGHTS))
				  Target->accum_volumetric(L_spot_s[it]);
		  }

		  L_spot_s.clear();
	  }
	}

   PIX_EVENT(POINT_LIGHTS_ACCUM);
	// Point lighting (unshadowed, if left)
	if (!LP.v_point.empty())		{
		xr_vector<light*>&	Lvec		= LP.v_point;
		for	(u32 pid=0; pid<Lvec.size(); pid++)	{
			if(Lvec[pid]->flags.bOccq&&!Lvec[pid]->flags.bHudMode)
			{
				Lvec[pid]->vis_update		();
				if (Lvec[pid]->vis.visible)	{
					Target->accum_point		(Lvec[pid]);
				}
			}
			else
				Target->accum_point		(Lvec[pid]);
		}
		Lvec.clear	();
	}

   PIX_EVENT(SPOT_LIGHTS_ACCUM);
	// Spot lighting (unshadowed, if left)
	if (!LP.v_spot.empty())		{
		xr_vector<light*>&	Lvec		= LP.v_spot;
		for	(u32 pid=0; pid<Lvec.size(); pid++)	{
			if(Lvec[pid]->flags.bOccq&&!Lvec[pid]->flags.bHudMode)
			{
				Lvec[pid]->vis_update		();
				if (Lvec[pid]->vis.visible)	{
					LR.compute_xf_spot		(Lvec[pid]);
					Target->accum_spot		(Lvec[pid]);
				}
			}
			else
			{
				LR.compute_xf_spot		(Lvec[pid]);
				Target->accum_spot		(Lvec[pid]);
			}
		}
		Lvec.clear	();
	}
}
