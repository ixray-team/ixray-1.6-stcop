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
			C.r_dx10Texture("s_cloud_sky_octo_small", r4_RT_sky_octo_map_small);
			C.r_dx10Texture("s_cloud_transmittance_lut", "shaders\\sky\\transmittance_lut");
			C.r_dx10Texture("s_cloud_fbm_noise", "shaders\\sky\\noise_fbm_128");
			C.r_dx10Texture("s_cloud_fastnoise", "shaders\\sky\\fastnoise_clouds");
			C.r_dx10Texture("s_cloud_shadow_map", r4_RT_procedural_clouds_shadow_filtered);

			C.r_dx10Sampler("smp_rtlinear");
			C.r_dx10Sampler("smp_linear");

			C.r_End();
			break;

		case 4: // small edge-aware blur of the stochastic optical-depth map
			C.r_ComputePass("ComputeCloudsShadowBlur");
			C.r_dx10Texture("s_cloud_shadow_map", r4_RT_procedural_clouds_shadow);
			C.r_End();
			break;

		case 3: // independent light-space 2D optical-depth map
			C.r_ComputePass("ComputeCloudsShadow");
			C.r_dx10Texture("s_cloud_fbm_noise", "shaders\\sky\\noise_fbm_128");
			C.r_dx10Sampler("smp_linear");
			C.r_End();
			break;

		case 1: // write history 0, read history 1
		case 2: // write history 1, read history 0
			C.r_ComputePass("ComputeCloudsTemporal");
			C.r_dx10Texture("s_cloud_current", r4_RT_procedural_clouds_raw);
			C.r_dx10Texture("s_cloud_depth", r4_RT_procedural_clouds_depth);
			C.r_dx10Texture("s_cloud_history", C.iElement == 1
				? r4_RT_procedural_clouds_history_1 : r4_RT_procedural_clouds_history_0);
			C.r_dx10Texture("s_cloud_history_depth", C.iElement == 1
				? r4_RT_procedural_clouds_history_depth_1 : r4_RT_procedural_clouds_history_depth_0);
			C.r_dx10Sampler("smp_rtlinear");
			C.r_End();
			break;
	}
}
