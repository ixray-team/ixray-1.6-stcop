#include "stdafx.h"
#include "../../xrEngine/customhud.h"

float				g_fSCREEN		;

extern float		r_dtex_range	;
extern float		r_ssaDISCARD	;
extern float		r_ssaLOD_A		;
extern float		r_ssaLOD_B		;
extern float		r_ssaHZBvsTEX	;
extern float		r_ssaGLOD_start,	r_ssaGLOD_end;

void CRender::Calculate		()
{
	// Transfer to global space to avoid deep pointer access
	IRender_Target* T = getTarget();

	float fov_factor = _sqr(90.f / Device.fFOV);

	g_fSCREEN = float(T->get_width() * T->get_height()) * fov_factor;

	float sprite_lodding_dist_f	= g_fSCREEN * (EPS_S + ps_r__geomLodSpriteDistF_);
	float geom_q_dist_f			= g_fSCREEN * (EPS_S + ps_r__geomLodDistF_);
	float geom_discard_dist_f	= g_fSCREEN * (EPS_S + ps_r__geomDiscardDistF_);
	float geom_nt_dist_f		= g_fSCREEN * (EPS_S + ps_r__geomNTextureDistF_);
	float geom_dt_dist_f		= g_fSCREEN * (EPS_S + ps_r__geomDTextureDistF_);

	r_ssaDISCARD	= _sqr(ps_r__ssaDISCARD) / geom_discard_dist_f;
	r_ssaLOD_A		= _sqr(ps_r2_ssaLOD_A / 3) / sprite_lodding_dist_f;
	r_ssaLOD_B		= _sqr(ps_r2_ssaLOD_B / 3) / sprite_lodding_dist_f;
	r_ssaGLOD_start	= _sqr(ps_r__GLOD_ssa_start / 3) / geom_q_dist_f;
	r_ssaGLOD_end	= _sqr(ps_r__GLOD_ssa_end / 3) / geom_q_dist_f;
	r_ssaHZBvsTEX	= _sqr(ps_r__ssaHZBvsTEX / 3) / geom_nt_dist_f;
	r_dtex_range	= ps_r2_df_parallax_range * geom_dt_dist_f / (1024.f * 768.f);
	
	// Detect camera-sector
	if (!vLastCameraPos.similar(Device.vCameraPosition,EPS_S)) 
	{
		CSector* pSector		= (CSector*)detectSector(Device.vCameraPosition);
		if (pSector && (pSector!=pLastSector))
			g_pGamePersistent->OnSectorChanged( translateSector(pSector) );

		if (0==pSector) pSector = pLastSector;
		pLastSector = pSector;
		vLastCameraPos.set(Device.vCameraPosition);
	}

	// Check if camera is too near to some portal - if so force DualRender
	if (rmPortals) 
	{
		float	eps			= VIEWPORT_NEAR+EPS_L;
		Fvector box_radius; box_radius.set(eps,eps,eps);
		Sectors_xrc.box_options	(CDB::OPT_FULL_TEST);
		Sectors_xrc.box_query	(rmPortals,Device.vCameraPosition,box_radius);
		for (int K=0; K<Sectors_xrc.r_count(); K++)	{
			CPortal*	pPortal		= (CPortal*) Portals[rmPortals->get_tris()[Sectors_xrc.r_begin()[K].id].dummy];
			pPortal->bDualRender	= TRUE;
		}
	}

	//
	Lights.Update();

	// Check if we touch some light even trough portal
	lstRenderables.clear();
	g_SpatialSpace->q_sphere(lstRenderables,0,STYPE_LIGHTSOURCE,Device.vCameraPosition,EPS_L);
	for (u32 _it=0; _it<lstRenderables.size(); _it++)	{
		ISpatial*	spatial		= lstRenderables[_it];		spatial->spatial_updatesector	();
		CSector*	sector		= (CSector*)spatial->spatial.sector;
		if	(0==sector)										continue;	// disassociated from S/P structure

		VERIFY							(spatial->spatial.type & STYPE_LIGHTSOURCE);
		// lightsource
		light*			L				= (light*)	(spatial->dcast_Light());
		VERIFY							(L);
		Lights.add_light				(L);
	}
}
