#include "common.hlsli"
#include "sload.hlsli"
#include "lmodel.hlsli"
#include "hmodel.hlsli"

void main(p_bumped_new I, out f_editor_gbuffer O)
{
	if(is_lighting_enable.x < 0.5f) {
		O.Color = tex2D(s_base, I.tcdh.xy);
		
#ifdef USE_AREF
    clip(O.Color.w - def_aref);
#endif

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

    SloadNew(I, M);

#ifdef USE_AREF
    clip(M.Color.w - def_aref);
#endif

    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
    M.Normal = normalize(M.Normal);

    M.Sun = saturate(M.Sun * 2.0f);
    M.Color.xyz = saturate(M.Color.xyz);
    float MaterialID = xmaterial;

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

