#include "common.hlsli"


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
	
	float3 N = normalize(cross(dp1, dp2));

    // Solve the linear system
    float3 dp2perp = cross(dp2, N);
    float3 dp1perp = cross(N, dp1);
	
    float3 T = dp2perp * duv1.x + dp1perp * duv2.x;
    float3 B = dp2perp * duv1.y + dp1perp * duv2.y;
    float invmax = rsqrt(max(dot(T, T), dot(B, B)));

	T *= invmax;
    B *= invmax;
	
	float3x3 TBN = float3x3
	(
		T.x, B.x, N.x,
		T.y, B.y, N.y,
		T.z, B.z, N.z
	);
	
	float3 viewDir = mul(transpose(TBN),-Point);
 	viewDir = normalize(viewDir);
	
	tc.xy = tc.xy - viewDir.xy/viewDir.z * 1.0f;
	return smoothstep(0.5, 0.4, length(tc.xy - 0.5));
}

float4 main(v2p I, float4 pos2d : SV_POSITION) : SV_Target
{
    if (m_hud_params.y * m_hud_params.a < 0.0001f)
	{
		return 0.0f;
	}

	float3 Point = GbufferGetPointRealUnjitter(pos2d.xy * pos_decompression_params2.zw, pos2d.z);
	float Fade = parralax_fade(I.tc0, Point);

    float4 t_base = s_base.Sample(smp_base, I.tc0);
	t_base.xyz = detonemap(t_base.xyz);
	
    float4 t_vp2 = s_image[pos2d.xy];
	
	float alpha = m_hud_params.y * m_hud_params.a;
	t_vp2 *= saturate(alpha * 2.0f - 1.0f) * Fade;
	
    float3 final = lerp(t_vp2.xyz, t_base.xyz, t_base.a);
    return float4(final.xyz, saturate(alpha * 2.0f));
}

