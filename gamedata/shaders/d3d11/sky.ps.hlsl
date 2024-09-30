#include "common.hlsli"

struct v2p
{
    float4 factor : COLOR0;

    float3 tc0 : TEXCOORD0;
    float3 tc1 : TEXCOORD1;

    float4 hpos_curr : TEXCOORD2;
    float4 hpos_old : TEXCOORD3;

    float4 hpos : SV_POSITION;
};

TextureCube s_sky0 : register(t0);
TextureCube s_sky1 : register(t1);

struct sky
{
    float4 Color : SV_Target0;
    float2 Velocity : SV_Target1;
};

void main(in v2p I, out sky O)
{
    float3 s0 = s_sky0.SampleLevel(smp_base, I.tc0, 0.0f).xyz;
    float3 s1 = s_sky1.SampleLevel(smp_base, I.tc1, 0.0f).xyz;
    float3 sky = L_sky_color.xyz * lerp(s0, s1, I.factor.w);

    O.Color = float4(sky, 0.0f);
    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
}
