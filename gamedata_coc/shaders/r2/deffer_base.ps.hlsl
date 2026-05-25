#include "common.hlsli"
#include "sload.hlsli"

void main(p_bumped_new I, out f_deffer O)
{
    XrayMaterial M;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

    SloadNew(I, M);

#ifdef USE_AREF
    clip(M.Color.w - def_aref);
#endif

    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
    M.Normal = normalize(M.Normal);

#ifdef USE_LM_HEMI
    float4 lm = tex2D(s_hemi, I.tcdh.zw);

    M.Sun = get_sun(lm);
    M.Hemi = get_hemi(lm);
#endif

#ifndef USE_R2_STATIC_SUN
    M.Sun = xmaterial;
#endif

    O = pack_gbuffer(float4(M.Normal, M.Hemi), float4(M.Point, M.Sun), float4(M.Color.xyz, 1.0f - M.Roughness));
}
