// Second axis and resolve of Kleber Garcia's circular convolution.
// Coefficients, attribution and license are in dof_bokeh.hlsli.
#include "common.hlsli"
#include "shared\post_cb.hlsli"
#define DOF_CIRCULAR
#include "dof_bokeh.hlsli"

Texture2D<float4> t_dof_real0 : register(t25);
Texture2D<float4> t_dof_imaginary0 : register(t26);
#ifndef DOF_NEAR
Texture2D<float4> t_dof_real1 : register(t28);
Texture2D<float4> t_dof_imaginary1 : register(t29);
#endif

float4 main(PSInputFullscreen I) : SV_Target
{
	float2 MinUV = 0.5f * dof_rt_size.zw;
	float2 CenterUV = clamp(I.texcoord.xy, MinUV, 1.0f - MinUV);
	float4 Center = t_metadata.SampleLevel(smp_nofilter, CenterUV, 0);
	float Radius = DofCircularRadius(CenterUV, Center);
	float4 Color = 0.0f;
	float Step = Radius / 8.0f;
	int Parts = clamp(int(ceil(Step / dof_blur_params.z)), 1, int(dof_focus_point.w));
	[unroll]
	for (int Tap = 0; Tap < 17; ++Tap)
	{
		float4 Real0 = 0.0f;
		float4 Imaginary0 = 0.0f;
#ifndef DOF_NEAR
		float4 Real1 = 0.0f;
		float4 Imaginary1 = 0.0f;
#endif
		[loop]
		for (int Part = 0; Part < Parts; ++Part)
		{
			float Offset = (float(Tap - 8) + (float(Part) + 0.5f) / float(Parts) - 0.5f) * Step;
			float2 SampleUV = clamp(CenterUV + DofLensOffset(float2(Offset, 0.0f), CenterUV, 1.0f / dof_blur_params.xy, dof_focus_point.z, dof_kernel.y) * dof_blur_params.xy, MinUV, 1.0f - MinUV);
			// Intermediates contain spread, not the original surface at SampleUV.
			// Depth rejection here would clip near against unrelated source-block depths.
			Real0 += t_dof_real0.SampleLevel(smp_rtlinear, SampleUV, 0);
			Imaginary0 += t_dof_imaginary0.SampleLevel(smp_rtlinear, SampleUV, 0);
#ifndef DOF_NEAR
			Real1 += t_dof_real1.SampleLevel(smp_rtlinear, SampleUV, 0);
			Imaginary1 += t_dof_imaginary1.SampleLevel(smp_rtlinear, SampleUV, 0);
#endif
		}
		Color += DofResolveComponent(Real0, Imaginary0, DofKernel0[Tap], DofKernelWeight0) / float(Parts);
#ifndef DOF_NEAR
		Color += DofResolveComponent(Real1, Imaginary1, DofKernel1[Tap], DofKernelWeight1) / float(Parts);
#endif
	}
	// Resolve negative ringing only after combining both axes and all components.
	if (Color.a <= EPS_S)
	{
		return 0.0f;
	}
#ifdef DOF_NEAR
	// Keep premultiplication and postpone the upper opacity clamp until the small gather is added.
	return float4(clamp(Color.rgb, 0.0f, 65504.0f), Color.a);
#else
	// Alpha is validity, not luminance. Black with positive weight is a valid background.
	// Signed kernels can leave a tiny positive weight beside a bright HDR edge.
	// Bound storage to finite FP16 after division, without clipping complex intermediates.
	return float4(clamp(Color.rgb / Color.a, 0.0f, 65504.0f), 1.0f);
#endif
}
