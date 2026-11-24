#include "common\common.hlsli"

struct v2p
{
    float4 factor : COLOR0;
    float3 p : TEXCOORD1;

#ifndef DISABLE_MOTION_VECTORS
    float4 hpos_curr : TEXCOORD2;
    float4 hpos_old : TEXCOORD3;
#endif

    float4 hpos : SV_POSITION;
};

void vs_main(PULL_INPUT, out v2p o)
{
    RingVertex V = PullRing(VertexId);
    float4 P = float4(V.P, 1.0f);

    o.hpos = mul(m_WVP, P);
	
    o.factor = V.C.zyxw;
    o.p = V.P;

#ifndef DISABLE_MOTION_VECTORS
    o.hpos_curr = o.hpos;
    o.hpos_old = mul(m_WVP_old, P);
#endif
	
    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
}

TextureCube s_sky0 : register(t25);
TextureCube s_sky1 : register(t26);

struct sky
{
    float4 Color : SV_Target0;
#ifndef DISABLE_MOTION_VECTORS
    float2 Velocity : SV_Target1;
#endif
};

void ps_main(in v2p I, out sky O)
{
	float3 TexCoord = I.p;
	
#ifndef USE_FULL_SKY_SPHERE
    RemapVector(TexCoord);
#endif

	float3 s0 = s_sky0.SampleLevel(smp_rtlinear, TexCoord, 0.0f).xyz;
	float3 s1 = s_sky1.SampleLevel(smp_rtlinear, TexCoord, 0.0f).xyz;
	float3 sky = lerp(s0, s1, I.factor.w);
    
#ifdef USE_BGRA_SKYCOLOR
    sky *= L_sky_color.zyx;
#else
    sky *= L_sky_color.xyz;
#endif

	O.Color = float4(SceneFromGamma(sky), 0.0f);

#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
#endif
}
