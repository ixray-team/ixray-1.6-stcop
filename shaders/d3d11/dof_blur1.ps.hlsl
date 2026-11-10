// First axis of Kleber Garcia's circular convolution; coefficients/license in dof_bokeh.hlsli.
#include "common.hlsli"
#include "shared\post_cb.hlsli"
#define DOF_CIRCULAR
#include "dof_bokeh.hlsli"

#ifdef DOF_NEAR
Texture2D<float4> t_near : register(t25);
#else
Texture2D<float4> t_far : register(t26);
#endif

struct DofComplexOutput
{
	float4 Real0 : SV_Target0;
	float4 Imaginary0 : SV_Target1;
#ifndef DOF_NEAR
	float4 Real1 : SV_Target2;
	float4 Imaginary1 : SV_Target3;
#endif
};

DofComplexOutput main(PSInputFullscreen I)
{
	float2 MinUV = 0.5f * dof_rt_size.zw;
	float2 CenterUV = clamp(I.texcoord.xy, MinUV, 1.0f - MinUV);
	float4 Center = t_metadata.SampleLevel(smp_nofilter, CenterUV, 0);
	float Radius = DofCircularRadius(CenterUV, Center);
	float CenterDepth = s_position.SampleLevel(smp_nofilter, CenterUV, 0).x;
	DofComplexOutput O;
	O.Real0 = 0.0f;
	O.Imaginary0 = 0.0f;
#ifndef DOF_NEAR
	O.Real1 = 0.0f;
	O.Imaginary1 = 0.0f;
#endif

	[unroll]
	for (int Tap = 0; Tap < 17; ++Tap)
	{
		float Offset = float(Tap - 8) * (Radius / 8.0f);
		float2 SampleUV = clamp(CenterUV + float2(0.0f, Offset * dof_blur_params.y), MinUV, 1.0f - MinUV);
#ifdef DOF_NEAR
		float4 Sample = t_near.SampleLevel(smp_rtlinear, SampleUV, 0);
		float SampleDepth = t_metadata.SampleLevel(smp_nofilter, SampleUV, 0).x;
		// Only the first axis has source-layer provenance. World near must not cover HUD.
		if (DofIsHud(CenterDepth) && !DofIsHud(SampleDepth))
		{
			Sample = 0.0f;
		}
#else
		float4 Sample = t_far.SampleLevel(smp_rtlinear, SampleUV, 0);
		// A blurred HUD may reveal background; a sharp HUD is restored by combine.
		if (DofIsHud(CenterDepth) && Center.z == 0.0f)
		{
			Sample = 0.0f;
		}
#endif
		O.Real0 += Sample * DofKernel0[Tap].x;
		O.Imaginary0 += Sample * DofKernel0[Tap].y;
#ifndef DOF_NEAR
		O.Real1 += Sample * DofKernel1[Tap].x;
		O.Imaginary1 += Sample * DofKernel1[Tap].y;
#endif
	}
	return O;
}
