#include "common.hlsli"
#include "sload.hlsli"
#include "lmodel.hlsli"
#include "hmodel.hlsli"

void main(p_bumped_new I, out float4 Color : COLOR0)
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

    M.Sun = saturate(M.Sun * 2.0f);
    M.Color.xyz = saturate(M.Color.xyz);

    float MaterialID = 0.5f;
    float Gloss = 1.0f - M.Roughness;

    float4 Light = float4(L_sun_color, 1.0f) * M.Sun * plight_infinity(MaterialID, M.Point, M.Normal, L_sun_dir_e);
    float3 Diffuse, Specular;

    hmodel(Diffuse, Specular, MaterialID, M.Hemi, Gloss, M.Point, M.Normal);

    Color = float4(Diffuse + Light.xyz, M.Color.w);
    Color.xyz *= M.Color.xyz;
    Color.xyz += Light.w * Gloss + Specular;

    float fog = saturate(length(M.Point) * fog_params.w + fog_params.x);
    Color.xyz = M.Normal * 0.5f + 0.5f; //lerp(Color, fog_color, fog);
}

