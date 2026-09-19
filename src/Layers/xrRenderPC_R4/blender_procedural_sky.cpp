#include "stdafx.h"
#include "blender_procedural_sky.h"

namespace
{
	constexpr const char* kTransmittanceLut = "shaders\\sky\\transmittance_lut";
	constexpr const char* kMultiScatteringLut = "shaders\\sky\\multi_scattering_lut";
} // namespace

CBlender_procedural_sky::CBlender_procedural_sky() { description.CLS = 0; }
CBlender_procedural_sky::~CBlender_procedural_sky() {}

void CBlender_procedural_sky::Compile(CBlender_Compile& C)
{
	IBlender::Compile(C);

	switch (C.iElement)
	{
		case 0: // ComputeSkyView
			C.r_ComputePass("ComputeSkyView");

			C.r_dx10Texture("s_transmittance_lut", kTransmittanceLut);
			C.r_dx10Texture("s_multi_scattering_lut", kMultiScatteringLut);
			C.r_dx10Sampler("smp_rtlinear");

			C.r_End();
			break;

		case 1: // ComputeAerialPerspective
			C.r_ComputePass("ComputeAP");
			C.r_dx10Texture("s_cloud_fbm_noise", "shaders\\sky\\noise_fbm_128");

			C.r_dx10Texture("s_transmittance_lut", kTransmittanceLut);
			C.r_dx10Texture("s_multi_scattering_lut", kMultiScatteringLut);
			C.r_dx10Sampler("smp_rtlinear");

			C.r_End();
			break;

		case 2: // ComputeOctoUnwrap
			C.r_ComputePass("ComputeOctoUnwrap");

			C.r_dx10Texture("s_sky_view_lut", r4_RT_sky_view);
			C.r_dx10Sampler("smp_rtlinear");

			C.r_End();
			break;

		case 3: // SkyOctoUnwrap 512x512 -> middle 128x128
			C.r_ComputePass("OctoDownsample");

			C.r_dx10Texture("s_sky_octo_input", r4_RT_sky_octo_map);
			C.r_dx10Sampler("smp_rtlinear");

			C.r_End();
			break;

		case 4: // SkyOctoUnwrap middle 128x128 -> small 32x32
			C.r_ComputePass("OctoDownsample");

			C.r_dx10Texture("s_sky_octo_input", r4_RT_sky_octo_map_middle);
			C.r_dx10Sampler("smp_rtlinear");

			C.r_End();
			break;
		case 5: // small octomap -> diffuse irradiance octomap
			C.r_ComputePass("OctoDownsample");

			C.r_dx10Texture("s_sky_octo_input", r4_RT_sky_octo_map_small);

			C.r_dx10Sampler("smp_rtlinear");

			C.r_End();
			break;
	}
}
