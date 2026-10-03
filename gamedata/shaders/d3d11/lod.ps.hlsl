#ifdef USE_R1_STATIC_LIGHTING
#include "r1_lod.ps.hlsl"
#else
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
}

void main(in p_bilbord I, out IXRayGbufferPack O)
{
    float4 D, H;
    sample_Textures(D, H, I.tc1, I.tc0, I.af);
    float3 N = normalize(H.xyz);

    clip(D.w - def_aref);

    float Lighting = 0.5f + 0.5f * H.w;

    IXRayMaterial M = (IXRayMaterial)NULL;
    M.Depth = I.position.z;

    M.Point = I.position.xyz;
    M.Color = D;

    M.Sun = saturate(I.af.y * Lighting);
    M.Hemi = saturate(I.af.x * Lighting);

    M.Normal = N.xyz;

#ifdef USE_LEGACY_LIGHT
    M.Material = L_material.w;
    M.Gloss = def_gloss;
#else
    M.AO = 1.0f;
    M.SSS = 0.0f;
	
    M.Roughness = 1.0f;
    M.Metalness = 0.0f;
	M.Specular = 0.0f;
#endif

	M.MaterialID = FOLIAGE_ID;
	
#ifndef DISABLE_MOTION_VECTORS
    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
#endif
	
    GbufferPack(O, M);
}


#endif
