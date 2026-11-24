#include "common\common.hlsli"
#include "common\cloudconfig.hlsli"

struct vf
{
    float4 color : COLOR0; // rgb. intensity, for SM3 - tonemap-prescaled, HI-res
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float4 hpos : SV_POSITION;
};

vf vs_main(PULL_INPUT)
{
    vf o;
    RingVertex V = PullRing(VertexId);
    o.hpos = mul(m_WVP, float4(V.P, 1.0f)); // xform, input in world coords
    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;

    // generate tcs
    float2 d0 = V.C.zy * 2.0f - 1.0f;
    float2 d1 = V.C.wx * 2.0f - 1.0f;
    float2 _0 = V.P.xz * CLOUD_TILE0 + d0 * timers.z * CLOUD_SPEED0;
    float2 _1 = V.P.xz * CLOUD_TILE1 + d1 * timers.z * CLOUD_SPEED1;
    o.tc0 = _0;
    o.tc1 = _1;

    o.color = DecodeU8x4(V.X).zyxw;
    o.color.w *= pow(abs(V.P.y), 25.0f);

    return o;
}

struct v2p
{
    float4 color : COLOR0;
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
};

Texture2D s_clouds0 : register(t25);
Texture2D s_clouds1 : register(t26);

// Pixel
float4 ps_main(v2p I) : SV_Target
{
    float4 s0 = s_clouds0.Sample(smp_base, I.tc0);
    float4 s1 = s_clouds1.Sample(smp_base, I.tc1);
    float4 mix = I.color * (s0 + s1);
	
	return float4(SceneFromGamma(mix.xyz), mix.w);
}
