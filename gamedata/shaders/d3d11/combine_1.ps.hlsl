#include "common.hlsli"

#ifdef USE_OFFSCREEN_REFLECTIONS
	#define USE_VIEW_REFLECTIONS
#endif

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "reflections.hlsli"

Texture2D s_occ;

float4 main(PSInputFullscreen I) : SV_Target
{
    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack((uint2)I.hpos.xy, O);
	
    float3 Light = s_accumulator.Load(int3(I.hpos.xy, 0)).xyz;

#ifdef USE_R2_STATIC_SUN
	#ifdef USE_LEGACY_LIGHT
		Light += O.Sun * DirectLightLegacy(Ldynamic_color, Ldynamic_dir, O.Normal, O.View.xyz, O.Color, O.Material, O.Gloss);
	#else
		Light += O.Sun * DirectLight(Ldynamic_color, Ldynamic_dir, O.Normal, O.View.xyz, O.Color, O.Specular, O.Roughness);
	#endif
#endif

    float Occ = s_occ.Load(int3(I.hpos.xy, 0)).x;
	
#ifndef USE_LEGACY_LIGHT
	Occ *= O.AO;
#endif

#ifndef USE_LEGACY_LIGHT
	#if defined(USE_SSLR_REFLECTIONS) && !defined(SSLR_SOURCE_PASS)
		float3 SpecularIrradance = LinearToGamma(max(s_refl.Load(int3(I.hpos.xy, 0)).xyz, 0.0f));
		//SpecularIrradance *= SpecularIrradance < 1.0f ? rcp(1.0f - SpecularIrradance) : 1.0f;
	#else
		float NdotV = max(0.0, dot(O.Normal, -O.View.xyz));
		#ifdef USE_VIEW_REFLECTIONS
			float SkyHemi = O.Depth > 0.02 ? O.Hemi : 1.0f;
		#else
			float SkyHemi = O.Hemi;
		#endif
		float3 SpecularIrradance = CompureSpecularIrradance(reflect(O.View, O.Normal), SpecularOcclusion(NdotV, SkyHemi, O.Roughness), O.Roughness);
	#endif

	float3 DiffuseIrradance = CompureDiffuseIrradance(O.Normal, O.Hemi) + L_ambient.xyz;
    float3 Ambient = AmbientLightingImpl(DiffuseIrradance, SpecularIrradance, max(0.0, dot(O.Normal, -O.View.xyz)), O.Color, O.Specular, O.Roughness);
#else
    float3 Ambient = AmbientLightingLegcay(O.View, O.Normal, O.Color, O.Material, O.Gloss, O.Hemi);
#endif

	float3 Color = Occ * Ambient + Light;
	
    float Fog = saturate(O.ViewDist * fog_params.w + fog_params.x);
	Fog = GammaToLinear(Fog);
	
	Color = lerp(Color, GammaToLinear(fog_color.xyz), Fog);
	
    return float4(Color, Fog * Fog);
}
