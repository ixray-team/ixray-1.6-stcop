#include "common.hlsli"
#include "sload.hlsli"
#include "shadow.hlsli"

#ifdef USE_OFFSCREEN_REFLECTIONS
	#define USE_VIEW_REFLECTIONS
#endif

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

#ifdef FORWARD_LIGHT
	uniform float4 L_model_light_color;
	uniform float4 L_model_light_dir;
#endif
	
#ifndef USE_LENGTH_BUFFER
	#define OutStructure IXRayForward
#else
	#define OutStructure IXRayVSLRGBuffer
#endif

void main(p_bumped_new I, out OutStructure O)
{
    IXRayMaterial M = (IXRayMaterial)NULL;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
	
    M.Depth = I.position.z;
    M.Point = I.position.xyz;

    SloadNew(I, M);
	
#if defined(USE_LENGTH_BUFFER) && defined(USE_AREF)
	clip(M.Color.w - def_aref);
	
    #ifdef USE_DXT1_HACK
		M.Color.xyz *= M.Color.w > 0.0f ? rcp(M.Color.w) : 0.0f;
    #endif
#endif

#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
#else
	M.Normal = float3(I.M1.z, I.M2.z, I.M3.z);
#endif

    M.Normal = normalize(M.Normal);

#ifdef USE_LM_HEMI
    float4 hs = s_hemi.Sample(smp_rtlinear, I.tcdh.zw);

    M.Sun = get_sun(hs);
    M.Hemi = get_hemi(hs);
#endif

    M.Sun = saturate(M.Sun * 2.0f);

#ifdef USE_LEGACY_LIGHT
	M.Material = L_material.w;
#else
    M.Color.xyz = GammaToLinear(M.Color.xyz);
    M.Specular = GammaToLinear(M.Specular);
#endif
	
	float3 LightDir = mul((float3x3)m_V, L_sun_dir_w.xyz);
	
#ifndef USE_R2_STATIC_SUN
	float4 Point = float4(M.Point.xyz, 1.f);
	
	#ifdef USE_LENGTH_BUFFER
		Point.xyz += M.Normal * 0.05f;
	#else
		Point.xyz += M.Normal * 0.025f;
	#endif
	
	Point.xyz = mul(m_invV, Point).xyz;

	int cascade_index;
	float3 smap_texcoord;
	
	bool is_in_bounds = calc_cascades(Point.xyz, m_shadow_sun, cascade_index, smap_texcoord);

	float Shadow = 1.0;

	if(is_in_bounds)
	{
		Shadow = pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0, cascade_index);
	}

	if(cascade_index >= 2)
	{
		float3 Factor = smoothstep(0.499f, 0.498f, abs(smap_texcoord - 0.5f));
		float Fade = Factor.x * Factor.y * Factor.z;

		float FarShadow = saturate(M.Hemi * 8.0f - 2.0f);
		Shadow = lerp(FarShadow, Shadow, Fade);
	
#ifdef USE_LENGTH_BUFFER
		float3 FlatNormal = normalize(cross(ddx(M.Point.xyz), ddy(M.Point.xyz)));
		Shadow *= step(0.0f, dot(FlatNormal, -LightDir));
#endif
	}
	
	M.Sun = Shadow;
#endif

	float4 LightColor = float4(L_sun_color.xyz, 0.5f);
	
	float ViewLength = length(M.Point);
	float3 View = M.Point.xyz * rcp(ViewLength);
	
	#ifndef USE_LEGACY_LIGHT
		float3 Diffuse = M.Color.xyz * float(1.0f - M.Metalness);
		float3 Specular = lerp(M.Specular, M.Color.xyz, M.Metalness);
		
		float3 Light = GammaToLinear(M.Sun) * DirectLight(LightColor, mul((float3x3)m_V, L_sun_dir_w.xyz), M.Normal, View, Diffuse, Specular, M.Roughness);
		float3 Ambient = GammaToLinear(M.AO) * AmbientLighting(View, M.Normal, Diffuse, Specular, M.Roughness, M.Hemi);
	#else
		float3 Light = M.Sun * DirectLightLegacy(LightColor, mul((float3x3)m_V, L_sun_dir_w.xyz), M.Normal, View, M.Color.xyz, M.Material, M.Gloss);
		float3 Ambient = AmbientLightingLegcay(View, M.Normal, M.Color.xyz, M.Material, M.Gloss, M.Hemi);
	#endif
	
#ifdef FORWARD_LIGHT
	float3 LocalLightDir = normalize(M.Point - L_model_light_dir);
	
	#ifndef USE_LEGACY_LIGHT
		Light += DirectLight(L_model_light_color, LocalLightDir, M.Normal, View, Diffuse, Specular, M.Roughness);
	#else
		Light += DirectLightLegacy(L_model_light_color, LocalLightDir, M.Normal, View, M.Color.xyz, M.Material, M.Gloss);
	#endif
#endif
	
#ifdef USE_LENGTH_BUFFER	
	#ifdef USE_LM_HEMI
		float3 Lmap = s_lmap.Sample(smp_rtlinear, I.tcdh.zw).xyz;
	#else
		float3 Lmap = I.lmap;
	#endif
	
	float Luma = max(Lmap.y, max(Lmap.z, Lmap.x));
	
	Lmap *= Luma > 0.0f ? rcp(Luma) : 0.0f; Luma *= 1.6f;
	Lmap = lerp(1.0f, Lmap, min(1.0f, Luma * 10.0f));
	
	#ifndef USE_LEGACY_LIGHT
		Light += Luma * DirectLight(Lmap.xyzy, View, M.Normal, View, Diffuse, Specular, M.Roughness);
	#else
		Light += Luma * DirectLightLegacy(Lmap.xyzy, View, M.Normal, View, M.Color.xyz, M.Material, M.Gloss);
	#endif
#endif
	
    O.Color.xyz = Ambient + Light.xyz;

#ifndef USE_LENGTH_BUFFER
    O.Color.w = M.Color.w;
	
    float Fog = saturate(ViewLength * fog_params.w + fog_params.x);
    O.Color = lerp(O.Color, GammaToLinear(fog_color), Fog * Fog);
	
	#ifndef DISABLE_MOTION_VECTORS
		O.Velocity.xy = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
		O.Velocity.zw = saturate(M.Color.w * 3.0f - 1.0f);
	#endif
	
	#if defined(ALLOW_WBOIT_TRANSPARENCY) && defined(USE_WBOIT_TRANSPARENCY)
		WboitBufferPack(O, M.Point);
	#endif
#else
	O.Length = ViewLength;
#endif
}

