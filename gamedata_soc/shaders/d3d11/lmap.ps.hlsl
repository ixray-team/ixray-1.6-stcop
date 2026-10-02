#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
	float2 tc0 : TEXCOORD0;
	float2 tc1 : TEXCOORD1;
	float2 tch : TEXCOORD2;
	float3 c0 : COLOR0;
	float3 c1 : COLOR1;
	float fog : TEXCOORD3;
};

#ifdef USE_R1_EMISSION
Texture2D s_emission;
#endif

float4 main(v2p I) : SV_Target
{
	float4 t_base = r1_sample_base(I.tc0);
	float4 t_lmap = s_lmap.Sample(smp_rtlinear, I.tc1);

	float3 l_base = t_lmap.rgb;
	float3 l_hemi = I.c0 * r1_p_hemi(I.tch);
	float3 l_sun = I.c1 * t_lmap.a;
	float3 light = L_ambient.xyz + l_base + l_sun + l_hemi;
#ifdef USE_R1_EMISSION
	light += s_emission.Sample(smp_base, I.tc0).rgb;
#endif

	float3 final = light * t_base.xyz * 2.0f;
	final = lerp(fog_color.xyz, final, I.fog);
	return float4(final, t_base.a);
}
