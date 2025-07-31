#include "common.hlsli"
#include "mblur.hlsli"
#include "dof.hlsli"

float4 main(v2p_aa_AA I) : SV_Target
{
    float3 Color = max(0.0f, dof(I.Tex0));
    float4 Bloom = s_bloom.Sample(smp_rtlinear, I.Tex0);

    float Scale = s_tonemap.Sample(smp_nofilter, float2(0.5f, 0.5f)).x;
    Color = tonemap(Color, Scale);

    return combine_bloom(Color, Bloom);
}

