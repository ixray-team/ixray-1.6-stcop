#include "r1_water.hlsli"

Texture2D s_distort0;
Texture2D s_distort1;

float4 main(vf_waterd I) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tbase);
    float2 distort = (s_distort0.Sample(smp_base, I.tdist0).xy + s_distort1.Sample(smp_base, I.tdist1).xy) * 0.5f;
    return float4(lerp(distort, 0.5f, t_base.a), 0.0f, 0.5f);
}
