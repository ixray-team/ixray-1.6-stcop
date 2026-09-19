#include "stdafx.h"
#include "blender_procedural_clouds.h"

CBlender_procedural_clouds::CBlender_procedural_clouds() { description.CLS = 0; }
CBlender_procedural_clouds::~CBlender_procedural_clouds() {}

void CBlender_procedural_clouds::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	switch (C.iElement)
	{
		case 0:
			C.r_ComputePass("ComputeCloudsView");

			C.r_dx10Texture("s_cloud_aerial_perspective", r4_RT_aerial_perspective);
			C.r_dx10Texture("s_cloud_aerial_direct", r4_RT_aerial_direct);
			C.r_dx10Texture("s_cloud_aerial_transmittance", r4_RT_aerial_transmittance);
			C.r_dx10Texture("s_cloud_sky_octo_small", r4_RT_sky_octo_map_small);
			C.r_dx10Texture("s_cloud_transmittance_lut", "shaders\\sky\\transmittance_lut");
			C.r_dx10Texture("s_cloud_fbm_noise", "shaders\\sky\\noise_fbm_128");
			C.r_dx10Texture("s_cloud_fastnoise", "shaders\\sky\\fastnoise_clouds");

			C.r_dx10Sampler("smp_rtlinear");

			C.r_End();
			break;

		case 1: // full-resolution reconstruction; explicit ping-pong SRVs
			C.r_ComputePass("ComputeCloudsTemporal");
			C.r_dx10Sampler("smp_rtlinear");
			C.r_End();
			break;

		case 4: // small edge-aware blur of the optical-depth map
			C.r_ComputePass("ComputeCloudsShadowBlur");
			C.r_dx10Texture("s_cloud_shadow_map", r4_RT_procedural_clouds_shadow);
			C.r_End();
			break;

		case 3: // independent light-space 2D optical-depth map
			C.r_ComputePass("ComputeCloudsShadow");
			C.r_dx10Texture("s_cloud_fbm_noise", "shaders\\sky\\noise_fbm_128");
			C.r_End();
			break;

	}
}
