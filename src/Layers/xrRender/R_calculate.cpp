#include "stdafx.h"
#include "../../xrEngine/CustomHUD.h"

float				g_fSCREEN		;

extern float		r_dtex_range	;
extern float		r_ssaDISCARD	;
extern float		r_ssaDONTSORT	;
extern float		r_ssaLOD_A		;
extern float		r_ssaLOD_B		;
extern float		r_ssaHZBvsTEX	;
extern float		r_ssaGLOD_start,	r_ssaGLOD_end;

void CRender::Calculate		()
{
	// Transfer to global space to avoid deep pointer access
	IRender_Target* T				=	getTarget	();
	float	fov_factor				=	_sqr		(90.f / Device.fFOV);
	g_fSCREEN						=	float(T->get_width()*T->get_height())*fov_factor*(EPS_S+ps_r__LOD);
	r_ssaDISCARD					=	_sqr(ps_r__ssaDISCARD)		/g_fSCREEN;
	r_ssaDONTSORT					=	_sqr(ps_r__ssaDONTSORT/3)	/g_fSCREEN;
	r_ssaLOD_A						=	_sqr(ps_r2_ssaLOD_A/3)		/g_fSCREEN;
	r_ssaLOD_B						=	_sqr(ps_r2_ssaLOD_B/3)		/g_fSCREEN;
	r_ssaGLOD_start					=	_sqr(ps_r__GLOD_ssa_start/3)/g_fSCREEN;
	r_ssaGLOD_end					=	_sqr(ps_r__GLOD_ssa_end/3)	/g_fSCREEN;
	r_ssaHZBvsTEX					=	_sqr(ps_r__ssaHZBvsTEX/3)	/g_fSCREEN;
	r_dtex_range					=	ps_r2_df_parallax_range * g_fSCREEN / (1024.f * 768.f);
	
	// Detect camera-sector
	if (!vLastCameraPos.similar(Device.vCameraPosition,EPS_S)) 
	{
		CSector* pSector		= (CSector*)detectLastSector(Device.vCameraPosition);
		if (pSector && (pSector!=pLastSector))
			g_pGamePersistent->OnSectorChanged( translateSector(pSector) );

		if (0==pSector) pSector = pLastSector;
		pLastSector = pSector;
		vLastCameraPos.set(Device.vCameraPosition);
	}

	Lights.Update();
}
