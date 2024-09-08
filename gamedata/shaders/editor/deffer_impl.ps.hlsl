#include "common.hlsli"
#include "sload.hlsli"
#include "lmodel.hlsli"
#include "hmodel.hlsli"

// void main(p_bumped_new I, out f_editor_gbuffer O)
// {
	// cotangent_frame(I);
    // XrayMaterial M;

    // M.Sun = I.tcdh.w;
    // M.Hemi = I.tcdh.z;
    // M.Point = I.position.xyz;

    // SloadNew(I, M);

// #ifdef USE_AREF
    // clip(M.Color.w - def_aref);
// #endif

    // M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
    // M.Normal = normalize(M.Normal);

    // M.Sun = saturate(M.Sun * 2.0f);
    // M.Color.xyz = saturate(M.Color.xyz);
    // float MaterialID = 0.25f;

    // float Gloss = 1.0f - M.Roughness;
	
// #ifndef FORWARD_ONLY
	// O.Albedo = float4(M.Color.xyz, Gloss);
	// O.Normal = float4(M.Normal.xyz, Gloss);
	// O.PointZ = float4(M.Point.xyz, MaterialID);
// #endif

    // float4 Light = float4(L_sun_color, 1.0f) * M.Sun * plight_infinity(MaterialID, M.Point, M.Normal, L_sun_dir_e);
    // float3 Diffuse, Specular;

    // hmodel(Diffuse, Specular, MaterialID, M.Hemi, Gloss, M.Point, M.Normal);

    // O.Color = float4(Diffuse + Light.xyz, M.Color.w);
    // O.Color.xyz *= M.Color.xyz;
    // O.Color.xyz += Light.w * Gloss + Specular;

    // float fog = saturate(length(M.Point) * fog_params.w + fog_params.x);
    // O.Color = lerp(O.Color, fog_color, fog);
// }

uniform sampler2D s_mask;

uniform sampler2D s_dt_r;
uniform sampler2D s_dt_g;
uniform sampler2D s_dt_b;
uniform sampler2D s_dt_a;

uniform sampler2D s_dn_r;
uniform sampler2D s_dn_g;
uniform sampler2D s_dn_b;
uniform sampler2D s_dn_a;

void main(p_bumped_new I, out f_editor_gbuffer O)
{
	if(is_lighting_enable.x < 0.5f) {
		O.Color = tex2D(s_base, I.tcdh.xy);

		float2 tcdbump = I.tcdh.xy * dt_params.xy;
		float4 Detail = tex2D(s_detail, tcdbump);
		
		O.Color.xyz *= Detail.xyz * 2.0f;

#ifndef FORWARD_ONLY
		O.Normal = float4(I.M1.xyz, def_gloss);
		O.Albedo = float4(O.Color.xyz, def_gloss);
		O.PointZ = float4(I.position.xyz, xmaterial);
#endif

		return;
	}
	
	cotangent_frame(I);
    XrayMaterial M;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

    M.Color = tex2D(s_base, I.tcdh.xy);

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

    M.Sun = saturate(M.Sun * 2.0f);
    M.Color.xyz = saturate(M.Color.xyz);
	
    float MaterialID = 0.25f;
    float Gloss = 1.0f - M.Roughness;
	
#ifndef FORWARD_ONLY
	O.Albedo = float4(M.Color.xyz, Gloss);
	O.Normal = float4(M.Normal.xyz, Gloss);
	O.PointZ = float4(M.Point.xyz, MaterialID);
#endif

    float4 Light = float4(L_sun_color, 1.0f) * M.Sun * plight_infinity(MaterialID, M.Point, M.Normal, L_sun_dir_e);
    float3 Diffuse, Specular;

    hmodel(Diffuse, Specular, MaterialID, M.Hemi, Gloss, M.Point, M.Normal);

    O.Color = float4(Diffuse + Light.xyz, M.Color.w);
    O.Color.xyz *= M.Color.xyz;
    O.Color.xyz += Light.w * Gloss + Specular;

    float fog = saturate(length(M.Point) * fog_params.w + fog_params.x);
    O.Color = lerp(O.Color, fog_color, fog);
}

