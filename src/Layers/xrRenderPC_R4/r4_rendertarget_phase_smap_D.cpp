#include "stdafx.h"

void	CRenderTarget::phase_smap_direct		(light* L, u32 sub_phase)
{
	//	TODO: DX10: Check thst we will never need old SMap implementation
	// Targets
	if (RImplementation.o.HW_smap)		u_setrt	(rt_smap_surf, nullptr, nullptr, rt_smap_depth->pZRT);
	//else								u_setrt	(rt_smap_surf, nullptr, nullptr, rt_smap_ZB);
	else								VERIFY(!"Use HW SMap only for DX10!");


	//	Don't have rect clear for DX10
	//	TODO: DX9:	Full clear must be faster for the near phase for SLI
	//	inobody clears this buffer _this_ frame.
	// Clear
	ClearData ClearDepth = {};
	ClearDepth.Depth = 1.0f;
	g_RenderRHI->Clear(eClearDepth, RCache.get_ZB(), ClearDepth);

	//	Prepare viewport for shadow map rendering
	if (sub_phase!=SE_SUN_RAIN_SMAP	)
		RImplementation.rmNormal();
	else
	{
		D3D_VIEWPORT VP					=	{(float)L->X.D.minX,(float)L->X.D.minY,
			(float)(L->X.D.maxX - L->X.D.minX) , 
			(float)(L->X.D.maxY - L->X.D.minY) , 
			0,1 };
		//CHK_DX								(RDevice->SetViewport(&VP));
		RContext->RSSetViewports(1, &VP);
	}

	// Stencil	- disable
	RCache.set_Stencil					( FALSE );

	//	TODO: DX10:	Implement culling reverse for DX10
	// Misc		- draw only front/back-faces
	/*
	if (SE_SUN_NEAR==sub_phase)			RCache.set_CullMode			( CULL_CCW	);	// near
	else								{
		if (RImplementation.o.HW_smap)	RCache.set_CullMode			( CULL_CW	);	// far, reversed
		else							RCache.set_CullMode			( CULL_CCW	);	// far, front-faces
	}
	if (RImplementation.o.HW_smap)		RCache.set_ColorWriteEnable	( FALSE		);
	else								RCache.set_ColorWriteEnable	( );
	*/
}

void	CRenderTarget::phase_smap_direct_tsh	(light* L, u32 sub_phase)
{
	VERIFY								(RImplementation.o.Tshadows);

	RCache.set_ColorWriteEnable			();

	//	Prepare viewport for shadow map rendering
	RImplementation.rmNormal();
	
	ClearData ColorRGBA = { 1.0f, 1.0f, 1.0f, 1.0f };
	g_RenderRHI->Clear(eClearTarget, RCache.get_RT(), ColorRGBA);
}
