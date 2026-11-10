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
	[unroll]
	for (int Tap = 0; Tap < 17; ++Tap)
	{
		float Offset = float(Tap - 8) * (Radius / 8.0f);
		float2 SampleUV = clamp(CenterUV + float2(Offset * dof_blur_params.x, 0.0f), MinUV, 1.0f - MinUV);
		// Intermediates contain spread, not the original surface at SampleUV.
		// Depth rejection here would clip near against unrelated source-block depths.
		Color += DofResolveComponent(t_dof_real0.SampleLevel(smp_rtlinear, SampleUV, 0),
			t_dof_imaginary0.SampleLevel(smp_rtlinear, SampleUV, 0), DofKernel0[Tap], DofKernelWeight0);
#ifndef DOF_NEAR
		Color += DofResolveComponent(t_dof_real1.SampleLevel(smp_rtlinear, SampleUV, 0),
			t_dof_imaginary1.SampleLevel(smp_rtlinear, SampleUV, 0), DofKernel1[Tap], DofKernelWeight1);
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
