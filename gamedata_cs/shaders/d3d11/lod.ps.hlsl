#include "common.hlsli"
#include "sload.hlsli"

void sample_Textures(inout float4 D, inout float4 H, float2 tc1, float2 tc0, float4 af)
{
    float4 D1 = s_base.SampleLevel(smp_base, tc1, 0.0f);
    float4 D0 = s_base.SampleLevel(smp_base, tc0, 0.0f);
    float4 H0 = s_hemi.SampleLevel(smp_base, tc0, 0.0f);
    float4 H1 = s_hemi.SampleLevel(smp_base, tc1, 0.0f);

    H0.xyz = H0.rgb * 2.0f - 1.0f;
    H1.xyz = H1.rgb * 2.0f - 1.0f;

    D = lerp(D0, D1, af.w);
    D.w *= af.z;
    H = lerp(H0, H1, af.w);
    H.w *= af.x;
}

void main(in p_bilbord I, out IXrayGbufferPack O)
{
    float4 D, H;
    sample_Textures(D, H, I.tc1, I.tc0, I.af);
    float3 N = normalize(H.xyz);

    clip(D.w - def_aref);

    float Sun = saturate(H.w * 2.0f);

    IXrayMaterial M;
    M.Depth = I.position.z;

    M.Point = I.position.xyz;
    M.Color = D;

    M.Sun = Sun;

    M.AO = 1.0f;
    M.SSS = 0.0f;
    M.Hemi = H.w;

    M.Normal = N.xyz;

    M.Roughness = 0.95f;
    M.Metalness = 0.0f;

#ifdef USE_LEGACY_LIGHT
    M.Metalness = L_material.w;
    M.Roughness = def_gloss;
#endif

    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
    GbufferPack(O, M);
}
