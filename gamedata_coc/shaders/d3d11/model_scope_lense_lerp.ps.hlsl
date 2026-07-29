#include "common.hlsli"

uniform float4 m_hud_params;
uniform float4 m_zoom_deviation;

struct v2p
{
    float2 tc0: TEXCOORD0;
    float3 tc1: TEXCOORD1;
    float4 c0: COLOR0;
};

float parralax_fade(in float2 tc, in float3 Point)
{
    // Get edge vectors of the pixel triangle
    float3 dp1 = ddx_fine(Point);
    float3 dp2 = ddy_fine(Point);
	
    float2 duv1 = ddx(tc);
    float2 duv2 = ddy(tc);
	
	float3 T = float3(m_WV._11_12_13);
	float3 B = -float3(m_WV._21_22_23);
	float3 N = -float3(m_WV._31_32_33);
	
	float3x3 TBN = float3x3
	(
		T.x, B.x, N.x,
		T.y, B.y, N.y,
		T.z, B.z, N.z
	);
	
	float3 viewDir = mul(transpose(TBN), -Point);
	
	float fade = smoothstep(0.48f, 0.45f, length(tc.xy - 0.5f));
	tc.xy = tc.xy - viewDir.xy / viewDir.z * 100.0f;
	
	return min(fade, smoothstep(19.8, 18.0, length(tc.xy - 0.5)));
}

void main(v2p I, float4 pos2d : SV_POSITION, out IXRayForward O)
{
    if (m_hud_params.y * m_hud_params.a < 0.0001f)
	{
		discard;
	}

	float3 Point = GbufferGetPointRealJitter(pos2d.xy * pos_decompression_params2.zw, pos2d.z);
	float Fade = parralax_fade(I.tc0, Point);

    float2 coords = I.tc0;
    coords.x *= 0.5f;
	
    float4 base2 = s_base.Sample(smp_base, coords);
    coords.x += 0.5f;
	
    float4 t_base = s_base.Sample(smp_base, coords);
	
    t_base = lerp(base2, t_base, m_zoom_deviation.z);
	t_base.xyz = detonemap(t_base.xyz);
	
    float4 t_vp2 = s_image[pos2d.xy];
	
	float alpha = m_hud_params.y * m_hud_params.a;
	t_vp2.w = saturate(alpha * 2.0f - 1.0f);
	t_vp2.xyz *= t_vp2.w * t_vp2.w * Fade;
	
	alpha = saturate(alpha * 2.0f);

    float3 final = lerp(t_vp2.xyz, t_base.xyz, t_base.a);
	float2 vel = 0.0f;
	
    O.Color = float4(final.xyz, alpha);
	O.Velocity = float4(vel, 0.0f, t_base.a * alpha);
}

