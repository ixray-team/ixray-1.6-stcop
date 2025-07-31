#include "common.hlsli"
#include "sload.hlsli"

void main(p_bumped_new I, out f_deffer O)
{
    XrayMaterial M;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

    M.Color = tex2D(s_base, I.tcdh.xy);

    float4 Lmap = tex2D(s_lmap, I.tcdh.xy);
    float4 Mask = tex2D(s_mask, I.tcdh.xy);
    Mask /= dot(Mask, 1.0f);

    float2 tcdbump = I.tcdh.xy * dt_params.xy;

    float3 Detail_R = tex2D(s_dt_r, tcdbump).xyz * Mask.x;
    float3 Detail_G = tex2D(s_dt_g, tcdbump).xyz * Mask.y;
    float3 Detail_B = tex2D(s_dt_b, tcdbump).xyz * Mask.z;
    float3 Detail_A = tex2D(s_dt_a, tcdbump).xyz * Mask.w;
    float3 Detail = Detail_R + Detail_G + Detail_B + Detail_A;

    float4 Normal_R = tex2D(s_dn_r, tcdbump) * Mask.x;
    float4 Normal_G = tex2D(s_dn_g, tcdbump) * Mask.y;
    float4 Normal_B = tex2D(s_dn_b, tcdbump) * Mask.z;
    float4 Normal_A = tex2D(s_dn_a, tcdbump) * Mask.w;

    float3 Normal = Normal_R.wzy + Normal_G.wzy + Normal_B.wzy + Normal_A.wzy - 0.5;
    Normal.z *= 0.5f;

    M.Roughness = saturate(1.0f - Normal_R.x + Normal_G.x + Normal_B.x + Normal_A.x);
    M.Color.xyz *= Detail * 2.0f;

    M.Metalness = 0.0f;
    M.SSS = 0.0f;
    M.AO = 1.0f;

    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), Normal);
    M.Normal = normalize(M.Normal);

    M.Sun = Lmap.w;
    M.Hemi = M.Color.w;

#ifndef USE_R2_STATIC_SUN
    M.Sun = xmaterial;
#endif

    O = pack_gbuffer(float4(M.Normal, M.Hemi), float4(M.Point, M.Sun), float4(M.Color.xyz, 1.0f - M.Roughness));
}
