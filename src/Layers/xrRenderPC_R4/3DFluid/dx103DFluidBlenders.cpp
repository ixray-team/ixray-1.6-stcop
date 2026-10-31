#include "stdafx.h"
#include "dx103DFluidBlenders.h"

#include "dx103DFluidManager.h"
#include "dx103DFluidRenderer.h"

namespace
{
void SetupTextures(CBlender_Compile& C, bool render = false)
{
	if (!render)
	{
		const char**	TNames = FluidManager.GetEngineTextureNames();
		const char**	RNames = FluidManager.GetShaderTextureNames();

		for ( int i=0; i<dx103DFluidManager::NUM_RENDER_TARGETS; ++i)
			C.r_dx10Texture(RNames[i], TNames[i]);
		return;
	}

	//	Renderer
	C.r_dx10Texture("sceneDepthTex", r2_RT_P);
	C.r_dx10Texture("colorTex", FluidManager.GetEngineTextureNames()[dx103DFluidManager::RENDER_TARGET_COLOR_IN]);
	C.r_dx10Texture("jitterTex", "$user$NVjitterTex");

	C.r_dx10Texture("HHGGTex", "$user$NVHHGGTex");

	C.r_dx10Texture("fireTransferFunction", "internal\\internal_fireTransferFunction");

	const char**	TNames = dx103DFluidRenderer::GetRTNames();
	const char**	RNames = dx103DFluidRenderer::GetResourceRTNames();

	for ( int i=0; i<dx103DFluidRenderer::RRT_NumRT; ++i)
		C.r_dx10Texture(RNames[i], TNames[i]);
}
}

void CBlender_fluid_advect::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement) 
	{
	case 0:			// Advect
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_advect", false,false,false,false);
		break;
	case 1:			// AdvectBFECC
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_advect_bfecc", false,false,false,false);
		break;
	case 2:			// AdvectTemp
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_advect_temp", false,false,false,false);
		break;
	case 3:			// AdvectBFECCTemp
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_advect_bfecc_temp", false,false,false,false);
		break;
	case 4:			// AdvectVel
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_advect_vel", false,false,false,false);
		break;
	}

	C.r_CullMode(D3DCULL_NONE);

	SetupTextures(C);

	//	Constants must be bound befor r_End()
	C.r_End		();
}

void CBlender_fluid_advect_velocity::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement) 
	{
	case 0:			// AdvectVel
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_advect_vel", false,false,false,false);
		break;
	case 1:			// AdvectVelGravity
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_advect_vel_g", false,false,false,false);
		break;
	}

	C.r_CullMode(D3DCULL_NONE);

	SetupTextures(C);

	//	Constants must be bound befor r_End()
	C.r_End		();
}

void CBlender_fluid_simulate::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement) 
	{
	case 0:			// Vorticity
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_vorticity", false,false,false,false);
		break;
	case 1:			// Confinement
		//	Use additive blending
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_confinement", false,false,false,true, D3DBLEND_ONE, D3DBLEND_ONE);
		break;
	case 2:			// Divergence
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_divergence", false,false,false,false);
		break;
	case 3:			// Jacobi
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_jacobi", false,false,false,false);
		break;
	case 4:			// Project
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_project", false,false,false,false);
		break;
	}

	C.r_CullMode(D3DCULL_NONE);

	SetupTextures(C);

	//	Constants must be bound before r_End()
	C.r_End		();
}

void CBlender_fluid_obst::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement) 
	{
		case 0:			// ObstStaticBox
			//	AABB
			//C.r_Pass	("fluid_grid", "fluid_array", "fluid_obststaticbox", false,false,false,false);
			//	OOBB
			C.r_Pass	("fluid_grid_oobb", "fluid_array_oobb", "fluid_obst_static_oobb", false,false,false,false);
			break;		
		case 1:			// ObstDynBox
			//	OOBB
			C.r_Pass	("fluid_grid_dyn_oobb", "fluid_array_dyn_oobb", "fluid_obst_dynamic_oobb", false,false,false,false);
			break;		
	}

	C.r_CullMode(D3DCULL_NONE);

	SetupTextures(C);

	//	Constants must be bound before r_End()
	C.r_End		();
}

void CBlender_fluid_emitter::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement) 
	{
	case 0:			// ET_SimpleGausian
		C.r_Pass	("fluid_grid", "fluid_array", "fluid_gaussian", false,false,false,true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA);
		C.RS.SetRS(D3DRS_DESTBLENDALPHA,	D3DBLEND_ONE	);
		C.RS.SetRS(D3DRS_SRCBLENDALPHA,		D3DBLEND_ONE	);
		break;		
	}

	C.r_CullMode(D3DCULL_NONE);

	SetupTextures(C);

	//	Constants must be bound before r_End()
	C.r_End		();
}

void CBlender_fluid_obstdraw::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement) 
	{
	case 0:			// DrawTexture
		C.r_Pass	("fluid_grid", "null", "fluid_draw_texture", false,false,false,false);
		break;
	}

	C.r_CullMode(D3DCULL_NONE);

	SetupTextures(C);

	//	Constants must be bound before r_End()
	C.r_End		();
}

void CBlender_fluid_raydata::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement)
	{
	case 0:			// CompRayData_Back
		C.r_Pass	("fluid_raydata_back", "null", "fluid_raydata_back", false,false,false,false);
		C.r_CullMode(D3DCULL_CW);	//	Front
		//C.r_CullMode(D3DCULL_CCW);	//	Front
		break;
	case 1:			// CompRayData_Front
		C.r_Pass	("fluid_raydata_front", "null", "fluid_raydata_front", false,false,false,true,D3DBLEND_ONE,D3DBLEND_ONE);
		//RS.SetRS(D3DRS_SRCBLENDALPHA,		bABlend?abSRC:D3DBLEND_ONE	);
		//	We need different blend arguments for color and alpha
		//	One Zero for color
		//	One One for alpha
		//	so patch dest color.
		//	Note: You can't set up dest blend to zero in r_pass
		//	since r_pass would disable blend if src=one and blend - zero.
		C.RS.SetRS(D3DRS_DESTBLEND,		D3DBLEND_ZERO	);

		C.RS.SetRS(D3DRS_BLENDOP,				D3DBLENDOP_REVSUBTRACT	); // DST - SRC
		C.RS.SetRS(D3DRS_BLENDOPALPHA,			D3DBLENDOP_REVSUBTRACT	); // DST - SRC

		C.r_CullMode(D3DCULL_CCW);	//	Back
		//C.r_CullMode(D3DCULL_CW);	//	Back
		break;
	case 2:			// QuadDownSampleRayDataTexture
		C.r_Pass	("fluid_raycast_quad", "null", "fluid_raydatacopy_quad", false,false,false,false);
		C.r_CullMode(D3DCULL_CCW);	//	Back
		break;
	}

	//C.PassSET_ZB(false,false);

	SetupTextures(C, true);

	//	Constants must be bound before r_End()
	C.r_End		();
}

void CBlender_fluid_raycast::Compile(CBlender_Compile& C)
{
	IBlender::Compile	(C);

	switch (C.iElement)
	{
	case 0:			// QuadEdgeDetect
		C.r_Pass	("fluid_edge_detect", "null", "fluid_edge_detect", false,false,false,false);
		C.r_CullMode(D3DCULL_NONE);	//	Back
		break;
	case 1:			// QuadRaycastFog
		C.r_Pass	("fluid_raycast_quad", "null", "fluid_raycast_quad", false,false,false,false);
		C.r_CullMode(D3DCULL_CCW);	//	Back
		break;
	case 2:			// QuadRaycastCopyFog
		C.r_Pass	("fluid_raycast_quad", "null", "fluid_raycastcopy_quad", false,false,false,true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA);
		C.r_ColorWriteEnable(true, true, true, false);
		C.r_CullMode(D3DCULL_CCW);	//	Back
		break;
	case 3:			// QuadRaycastFire
		C.r_Pass	("fluid_raycast_quad", "null", "fluid_raycast_quad_fire", false,false,false,false);
		C.r_CullMode(D3DCULL_CCW);	//	Back
		break;
	case 4:			// QuadRaycastCopyFire
		C.r_Pass	("fluid_raycast_quad", "null", "fluid_raycastcopy_quad_fire", false,false,false,true, D3DBLEND_SRCALPHA, D3DBLEND_INVSRCALPHA);
		C.r_ColorWriteEnable(true, true, true, false);
		C.r_CullMode(D3DCULL_CCW);	//	Back
		break;
	}

	SetupTextures(C, true);

	//	Constants must be bound before r_End()
	C.r_End		();
}