#include "stdafx.h"
#include "../../xrEngine/xr_object.h"
IC		bool	pred_area		(light* _1, light* _2)
{
	u32		a0		= _1->X.S.size;
	u32		a1		= _2->X.S.size;
	return	a0>a1;	// reverse -> descending
}
#include "FBasicVisual.h"
#include "SkeletonCustom.h"
static void optimize_smap_size(light* L)
{
	int _cached_size = L->X.S.size;
	L->X.S.posX	= 0;
	L->X.S.posY	= 0;
	L->X.S.size	= SMAP_adapt_max;
	L->X.S.transluent = FALSE;
	// Compute approximate screen area (treating it as an point light) - R*R/dist_sq
	// Note: we clamp screen space area to ONE, although it is not correct at all
	float	dist				= Device.vCameraPosition.distance_to(L->spatial.sphere.P)-L->spatial.sphere.R;
			if (dist<0)	dist	= 0;
	float	ssa					= clampr	(L->range*L->range / (1.f+dist*dist),0.f,1.f);

	// compute intensity
	//float	intensity0			= (L->color.r + L->color.g + L->color.b)/3.f;
	//float	intensity1			= (L->color.r * 0.2125f + L->color.g * 0.7154f + L->color.b * 0.0721f);
	//float	intensity			= (intensity0+intensity1)/2.f;		// intensity1 tends to underestimate...

	// compute how much duelling frusta occurs	[-1..1]-> 1 + [-0.5 .. +0.5]
	float	duel_dot			= 1.f -	0.5f*Device.vCameraDirection.dotproduct(L->direction);

	// compute how large the light is - give more texels to larger lights, assume 8m as being optimal radius
	float	sizefactor			= L->range/8.f;				// 4m = .5, 8m=1.f, 16m=2.f, 32m=4.f

	// compute how wide the light frustum is - assume 90deg as being optimal
	float	widefactor			= L->cone/deg2rad(90.f);	// 

	// factors
	float	factor0				= powf	(ssa,		1.f/2.f);		// ssa is quadratic
	//float	factor1				= powf	(intensity, 1.f/16.f);		// less perceptually important?
	float	factor2				= powf	(duel_dot,	1.f/4.f);		// difficult to fast-change this -> visible
	float	factor3				= powf	(sizefactor,1.f/4.f);		// this shouldn't make much difference
	float	factor4				= powf	(widefactor,1.f/2.f);		// make it linear ???
	float	factor				= ps_r2_ls_squality * factor0 /** factor1*/ * factor2 * factor3 * factor4;
	
	// final size calc
	u32 _size					= iFloor( factor * SMAP_adapt_optimal );
	if (_size<SMAP_adapt_min)	_size	= SMAP_adapt_min;
	if (_size>SMAP_adapt_max)	_size	= SMAP_adapt_max;
	int _epsilon				= iCeil	(float(_size)*0.01f);
	int _diff					= _abs	(int(_size)-int(_cached_size));
	L->X.S.size					= (_diff>=_epsilon)?_size:_cached_size;
}

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
						optimize_smap_size(L);
				}
			}
			else
			{
				if(!L->flags.bHudMode)
					optimize_smap_size(L);
			}
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
#if USE_DX11
   PIX_EVENT(SHADOWED_LIGHTS);
#endif
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
			phase = PHASE_SMAP;

#if USE_DX11
			r_pmask(true, !!RImplementation.o.Tshadows);
#else
			if (RImplementation.o.Tshadows)	r_pmask	(true,true	);
			else							r_pmask	(true,false	);
#endif
#if USE_DX11
			PIX_EVENT(SHADOWED_LIGHTS_RENDER_SUBSPACE);
#endif
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
#if USE_DX11
					PROF_EVENT("decor_object")
#endif
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
				{
#if USE_DX11
					PROF_EVENT("r_dsgraph_render_subspace")
#endif
					r_dsgraph_render_subspace(L->spatial.sector, L->X.S.combine, L->position, TRUE, FALSE, L->ignore_object);
				}
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
#if USE_DX11
               PIX_EVENT(SHADOWED_LIGHTS_RENDER_GRAPH);
#endif
					r_dsgraph_render_graph				(1);			// normal level, secondary priority
#if USE_DX11
               PIX_EVENT(SHADOWED_LIGHTS_RENDER_SORTED);
#endif
					r_dsgraph_render_sorted				( );			// strict-sorted geoms
				}
			} else {
				stats.s_finalclip					++;
			}

			if(L->flags.bOccq&&!L->flags.bHudMode)
				L->svis.end								();
			r_pmask									(true,false);
		}
#if USE_DX11
      PIX_EVENT(UNSHADOWED_LIGHTS);
#endif
      //		switch-to-accumulator
		Target->phase_accumulator			();
		HOM.Disable							();
#if USE_DX11
      PIX_EVENT(POINT_LIGHTS);
#endif
		//		if (has_point_unshadowed)	-> 	accum point unshadowed
		if		(!LP.v_point.empty())	{
			light*	L_	= LP.v_point.back	();		LP.v_point.pop_back		();
			if(L_->flags.bOccq&&!L_->flags.bHudMode)
			{
				L_->vis_update				();
				if (L_->vis.visible)
					Target->accum_point		(L_);
			}
			else
				Target->accum_point		(L_);
		}
#if USE_DX11
      PIX_EVENT(SPOT_LIGHTS);
#endif
      //		if (has_spot_unshadowed)	-> 	accum spot unshadowed
		if		(!LP.v_spot.empty())	{
			light*	L_	= LP.v_spot.back	();		LP.v_spot.pop_back			();
			if(L_->flags.bOccq&&!L_->flags.bHudMode)
			{
				L_->vis_update				();
				if (L_->vis.visible)
					Target->accum_spot		(L_);
			}
			else
				Target->accum_spot		(L_);
		}
#if USE_DX11
      PIX_EVENT(SPOT_LIGHTS_ACCUM_VOLUMETRIC);
#endif	
      //		if (was_spot_shadowed)		->	accum spot shadowed
	  if (!L_spot_s.empty())
	  {
#if USE_DX11
		  PIX_EVENT(ACCUM_SPOT);
#endif
		  for (u32 it = 0; it < L_spot_s.size(); it++)
		  {
			  Target->accum_spot(L_spot_s[it]);
			  if (ps_r2_ls_flags.is(R2FLAG_VOLUMETRIC_LIGHTS))
				  Target->accum_volumetric(L_spot_s[it]);
		  }

		  L_spot_s.clear();
	  }
	}
#if USE_DX11
   PIX_EVENT(POINT_LIGHTS_ACCUM);
#endif
	// Point lighting (unshadowed, if left)
	if (!LP.v_point.empty())		{
		xr_vector<light*>&	Lvec		= LP.v_point;
		for	(u32 pid=0; pid<Lvec.size(); pid++)	{
			if(Lvec[pid]->flags.bOccq&&!Lvec[pid]->flags.bHudMode)
			{
				Lvec[pid]->vis_update		();
				if (Lvec[pid]->vis.visible)
					Target->accum_point		(Lvec[pid]);
			}
			else
				Target->accum_point		(Lvec[pid]);
		}
		Lvec.clear	();
	}
#if USE_DX11
   PIX_EVENT(SPOT_LIGHTS_ACCUM);
#endif
	// Spot lighting (unshadowed, if left)
	if (!LP.v_spot.empty())		{
		xr_vector<light*>&	Lvec		= LP.v_spot;
		for	(u32 pid=0; pid<Lvec.size(); pid++)	{
			if(Lvec[pid]->flags.bOccq&&!Lvec[pid]->flags.bHudMode)
			{
				Lvec[pid]->vis_update		();
				if (Lvec[pid]->vis.visible)
					Target->accum_spot		(Lvec[pid]);
			}
			else
				Target->accum_spot		(Lvec[pid]);
		}
		Lvec.clear	();
	}
}
