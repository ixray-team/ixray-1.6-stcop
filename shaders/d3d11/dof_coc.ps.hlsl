#include "common.hlsli"
#include "shared\post_cb.hlsli"
#include "dof_bokeh.hlsli"

Texture2D<float> s_dof_scope_mask : register(t26);

#ifdef USE_DOF_PHYSICAL
Texture2D<float> t_focus : register(t27);
#endif

Texture2D<float> t_coc_prev : register(t25);

float DofCoC(float2 UV)
{
	float RawDepth = s_position.SampleLevel(smp_nofilter, UV, 0).x;
	float Depth = GbufferGetPointRealUnjitter(UV, RawDepth).z;
	if (!isfinite(Depth) || Depth <= 0.0f)
	{
		return 0.0f;
	}

#ifdef USE_DOF_PHYSICAL
	float FocalLength = max(dof_params.x * 0.001f, EPS_S);
	float FNumber = max(dof_params.y, EPS_S);
	float SensorHeight = max(dof_params.z, EPS_S);
	float Focus = max(t_focus.Load(int3(0, 0, 0)), FocalLength + EPS);
	float CoCDiameter = (FocalLength * FocalLength / FNumber) * ((Depth - Focus) / (Depth * (Focus - FocalLength)));
	float RadiusPixels = 0.5f * CoCDiameter * dof_params.w / SensorHeight;
	return clamp(RadiusPixels, -dof_kernel.z, dof_kernel.z);
#else
	// Preserve the squared classic response and sky substitute for legacy profiles.
	float ClassicDepth = RawDepth > 0.9999f ? dof_classic.w : Depth;
	float Distance = ClassicDepth - dof_classic.y;
	float FarCoC = saturate(Distance / max(dof_classic.z - dof_classic.y, EPS_S));
	float NearCoC = saturate(Distance / min(dof_classic.x - dof_classic.y, -EPS_S));
	float ClassicCoC = sign(Distance) * min(dof_kernel.x * (NearCoC + FarCoC) * (NearCoC + FarCoC), dof_kernel.z);
	return ClassicCoC;
#endif
}

float main(PSInputFullscreen I) : SV_Target
{
	if (s_dof_scope_mask.SampleLevel(smp_nofilter, I.texcoord.xy, 0) > 0.5f)
	{
		return 0.0f;
	}

	// The depth buffer carries the anti-aliasing jitter and the colour does not: silhouette texels
	// change layer every frame. Keep last frame's radius where it fits this frame's neighbourhood.
	uint Width, Height;
	s_position.GetDimensions(Width, Height);
	float2 Texel = 1.0f / float2(Width, Height);
	float Center = DofCoC(I.texcoord.xy);
	float Low = Center;
	float High = Center;
	[unroll]
	for (int Y = -1; Y <= 1; ++Y)
	{
		[unroll]
		for (int X = -1; X <= 1; ++X)
		{
			float Near = DofCoC(I.texcoord.xy + float2(X, Y) * Texel);
			Low = min(Low, Near);
			High = max(High, Near);
		}
	}
	float History = t_coc_prev.SampleLevel(smp_nofilter, I.texcoord.xy, 0);
	return isfinite(History) ? lerp(clamp(History, Low, High), Center, 0.2f) : Center;
}
