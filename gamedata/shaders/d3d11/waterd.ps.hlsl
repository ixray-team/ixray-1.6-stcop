#include "common.hlsli"
#include "shared\waterconfig.hlsli"

struct vf
{
    float2 tbase : TEXCOORD0;
    float2 tnorm0 : TEXCOORD1;
    float2 tnorm1 : TEXCOORD2;
    float3 M1 : TEXCOORD3;
    float3 M2 : TEXCOORD4;
    float3 M3 : TEXCOORD5;
    float3 v2point : TEXCOORD6;
    float4 tctexgen : TEXCOORD7;
    float3 pos : TEXCOORD8;
    float4 c0 : COLOR0;
    float fog : FOG;
    float4 hpos : SV_Position;
};

Texture2D s_distort;

// Pixel
float4 main(vf I, float4 pos2d : SV_Position) : SV_Target
{
    float4 t_base = s_base.Sample(smp_base, I.tbase);

    float2 t_d0 = s_distort.Sample(smp_base, I.tnorm0);
    float2 t_d1 = s_distort.Sample(smp_base, I.tnorm1);
    float2 distort = (t_d0 + t_d1) * 0.5f;

    float alpha = 1.0f - t_base.w;

#ifdef USE_SOFT_WATER
    float2 PosTc = I.tctexgen.xy / I.tctexgen.z;
    gbuffer_data gbd = gbuffer_load_data(PosTc, pos2d);

    float3 waterPos = gbd.P.xyz * rcp(gbd.P.z) * I.tctexgen.z;
    float waterDepth = length(waterPos - gbd.P) * 0.75f;

    alpha *= saturate(5.0f * waterDepth) * 0.25f;
#endif

    return float4(distort, 0.08f, alpha);
}
