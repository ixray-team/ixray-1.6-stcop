// Layered reconstruction inspired by AMD FidelityFX DOF:
// https://gpuopen.com/manuals/fidelityfx_sdk/techniques/depth-of-field/#combination-pass
// Full-resolution slight defocus follows the EEVEE resolve idea (independent IXR implementation):
// https://github.com/blender/blender/blob/v4.3.2/source/blender/draw/engines/eevee_next/shaders/eevee_depth_of_field_resolve_comp.glsl
// The small full-resolution gather retains Dennis Gustafsson's Voxagon spiral:
// https://blog.voxagon.se/2018/05/04/bokeh-depth-of-field-in-single-pass.html
#ifndef DOF_H_INCLUDED
#define DOF_H_INCLUDED

#include "dof_bokeh.hlsli"
#include "shared\post_cb.hlsli"

#ifdef USE_DOF
Texture2D<float4> s_dof_near : register(t27);
Texture2D<float4> s_dof_far : register(t28);
Texture2D<float4> s_dof_metadata : register(t29);
Texture2D<float> s_dof_coc : register(t30);
Texture2D<float> s_dof_scope_mask : register(t26);

DofLayers SampleDofBlur(float2 TexCoord, float CenterDepth, float CenterRadius, bool CenterIsNear)
{
	uint Width, Height;
	s_dof_near.GetDimensions(Width, Height);
	float2 Pixel = TexCoord * float2(Width, Height) - 0.5f;
	int2 Base = int2(floor(Pixel));
	float2 Fraction = frac(Pixel);
	DofLayers O;
	O.Near = 0.0f;
	O.Far = 0.0f;
	[unroll]
	for (int Y = 0; Y < 2; ++Y)
	{
		[unroll]
		for (int X = 0; X < 2; ++X)
		{
			int2 SamplePixel = clamp(Base + int2(X, Y), int2(0, 0), int2(Width, Height) - 1);
			float Weight = (X == 0 ? 1.0f - Fraction.x : Fraction.x) * (Y == 0 ? 1.0f - Fraction.y : Fraction.y);
			float4 Metadata = s_dof_metadata.Load(int3(SamplePixel, 0));
			// Circular near already contains spread; source metadata cannot identify its contributors.
			// HUD/world source rejection is applied before the complex convolution.
			O.Near += s_dof_near.Load(int3(SamplePixel, 0)) * Weight;

			float FarWeight = Weight;
			if (DofIsHud(CenterDepth) && !DofIsHud(Metadata.y) && !CenterIsNear)
			{
				FarWeight = 0.0f;
			}
			else if (DofIsBackground(CenterDepth, Metadata.y))
			{
				FarWeight *= smoothstep(0.5f, 1.5f, CenterRadius);
			}
			O.Far += s_dof_far.Load(int3(SamplePixel, 0)) * FarWeight;
		}
	}
	// Far alpha remains validity. Near RGB remains premultiplied by opacity.
	O.Far.rgb /= max(O.Far.a, EPS_S);
	return O;
}
#endif

float3 dof(float2 CenterUV)
{
	float3 SharpColor = s_image.SampleLevel(smp_rtlinear, CenterUV, 0).rgb;
#ifdef USE_DOF
	if (dof_combine_params.w <= 0.0f || s_dof_scope_mask.SampleLevel(smp_nofilter, CenterUV, 0) > 0.5f)
	{
		return SharpColor;
	}

	float CenterDepth = s_position.SampleLevel(smp_nofilter, CenterUV, 0).x;
	float CenterCoC = s_dof_coc.SampleLevel(smp_nofilter, CenterUV, 0);
	float CenterRadius = abs(CenterCoC);
	float PixelScale = dof_combine_params.z;
	float SmallRadius = min(PixelScale, dof_combine_params.w);
	DofLayers Blurred = SampleDofBlur(CenterUV, CenterDepth, CenterRadius, DofIsNear(CenterCoC, CenterDepth));

	// Conservative support query: a sharp receiver may still be covered by small near disks.
	uint Width, Height;
	s_dof_metadata.GetDimensions(Width, Height);
	float2 Extent = (SmallRadius + 0.5f) / dof_combine_params.xy;
	uint2 SourceSize = uint2(dof_combine_params.xy);
	uint2 ReducedSize = uint2(Width, Height);
	uint2 FirstSource = uint2(clamp(floor((CenterUV - Extent) * dof_combine_params.xy), 0.0f, dof_combine_params.xy - 1.0f));
	uint2 LastSource = uint2(clamp(floor((CenterUV + Extent) * dof_combine_params.xy), 0.0f, dof_combine_params.xy - 1.0f));
	// Invert the integer downsample partition, including non-divisible dimensions.
	int2 First = int2(((FirstSource + 1) * ReducedSize + SourceSize - 1) / SourceSize) - 1;
	int2 Last = int2(((LastSource + 1) * ReducedSize + SourceSize - 1) / SourceSize) - 1;
	bool HasNear = false;
	[loop]
	for (int Y = First.y; Y <= Last.y; ++Y)
	{
		[loop]
		for (int X = First.x; X <= Last.x; ++X)
		{
			HasNear = HasNear || s_dof_metadata.Load(int3(X, Y, 0)).z > 0.0f;
		}
	}

	float4 SmallNear = 0.0f;
	float4 SmallFar = 0.0f;
	float Normalization = 0.0f;
	float LargeBlend = DofLargeWeight(CenterRadius, PixelScale);
	if (HasNear || (CenterRadius > 0.0f && LargeBlend < 1.0f))
	{
		float Radius = 0.0f;
		float Angle = 0.0f;
		float2 MinUV = 0.5f / dof_combine_params.xy;
		[loop]
		while (Radius < SmallRadius + 0.5f)
		{
			float2 SampleUV = clamp(CenterUV + DofLensOffset(float2(cos(Angle), sin(Angle)) * Radius, CenterUV, dof_combine_params.xy, dof_focus_point.z, dof_kernel.y) / dof_combine_params.xy, MinUV, 1.0f - MinUV);
			float SampleCoC = s_dof_coc.SampleLevel(smp_nofilter, SampleUV, 0);
			float SampleDepth = s_position.SampleLevel(smp_nofilter, SampleUV, 0).x;
			float SampleRadius = abs(SampleCoC);
			float VisibleRadius = DofVisibleRadius(SampleRadius, CenterRadius, CenterDepth, SampleDepth);
			float Coverage = DofCoverage(VisibleRadius, Radius);
			float4 Sample = float4(s_image.SampleLevel(smp_nofilter, SampleUV, 0).rgb, 1.0f);
			if (DofIsNear(SampleCoC, SampleDepth))
			{
				float Weight = Coverage * DofSampleWeight(SampleRadius) * (1.0f - DofLargeWeight(SampleRadius, PixelScale));
				SmallNear += Sample * Weight;
			}
			else if (!DofIsHud(CenterDepth) || DofIsHud(SampleDepth))
			{
				float Weight = (Radius == 0.0f ? 1.0f : Coverage) * DofSampleWeight(SampleRadius);
				SmallFar += Sample * Weight;
			}
			Normalization += DofCoverage(SmallRadius, Radius) * DofSampleWeight(SmallRadius);
			if (Radius == 0.0f)
			{
				Radius = DofRadScale;
			}
			else
			{
				Radius += DofRadScale / Radius;
				Angle += DofGoldenAngle;
			}
		}
	}

	// Small and reduced near weights partition each source disk. Combine before clamping opacity.
	float4 Near = Blurred.Near + SmallNear / max(Normalization, EPS_S);
	float Opacity = saturate(Near.a);
	float3 NearColor = Near.rgb / max(Near.a, EPS_S);
	float3 SmallColor = SmallFar.a > EPS_S ? SmallFar.rgb / SmallFar.a : SharpColor;
	float3 BaseColor = lerp(SharpColor, SmallColor, smoothstep(0.5f, 1.0f, CenterRadius));
	if (Blurred.Far.a > EPS_S)
	{
		// Fill behind a partially transparent foreground instead of retaining its sharp silhouette.
		float FarBlend = DofIsNear(CenterCoC, CenterDepth) && Opacity > EPS_S ? 1.0f : LargeBlend;
		BaseColor = lerp(BaseColor, Blurred.Far.rgb, FarBlend);
	}
	return lerp(BaseColor, NearColor, Opacity);
#else
	return SharpColor;
#endif
}

#endif
