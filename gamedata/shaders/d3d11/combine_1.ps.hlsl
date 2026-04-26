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
		Light += O.Sun * DirectLightLegacy(Ldynamic_color, LightDirection, O.Normal, O.View.xyz, O.Color, O.Material, O.Gloss);
	#else
		Light += O.Sun * DirectLight(Ldynamic_color, LightDirection, O.Normal, O.View.xyz, O.Color, O.Specular, O.Roughness);
	#endif
#endif

    float Occ = s_occ.Load(int3(I.hpos.xy, 0)).x;
	
#ifndef USE_LEGACY_LIGHT
	Occ *= O.AO;
#endif

#ifndef USE_LEGACY_LIGHT
	#ifdef USE_SSLR_REFLECTIONS
		float3 SpecularIrradance = saturate(s_refl.Load(int3(I.hpos.xy, 0)).xyz);
		SpecularIrradance *= SpecularIrradance < 1.0f ? rcp(1.0f - SpecularIrradance) : 1.0f;
	#else
		float3 SpecularIrradance = CompureSpecularIrradance
		(
			reflect(O.View, O.Normal), 
		#ifdef USE_VIEW_REFLECTIONS
			O.Depth > 0.02 ? O.Hemi : 1.0f,
		#else
			O.Hemi,
		#endif
			O.Roughness
		);
	#endif

	float3 DiffuseIrradance = CompureDiffuseIrradance(O.Normal, O.Hemi) + L_ambient.xyz;
    float3 Ambient = AmbientLightingImpl(DiffuseIrradance, SpecularIrradance, max(0.0, dot(O.Normal, -O.View.xyz)), O.Color, O.Specular, O.Roughness);
#else
    float3 Ambient = AmbientLightingLegcay(O.View, O.Normal, O.Color, O.Material, O.Gloss, O.Hemi);
#endif

    float3 Color = Occ * Ambient + Light;
    float Fog = 0.0f;
	
#ifndef NEW_FOGGIN
    Fog = saturate(O.ViewDist * fog_params.w + fog_params.x);
    Fog *= Fog;
#else  //NEW_FOGGIN
    float denom = F_base - exp(-F_dens * (fog_params.z - fog_params.y));
    Fog = (F_base - exp(-F_dens * (O.ViewDist - fog_params.y))) / denom;
    Fog = saturate(Fog);
#endif

	// Color = O.Roughness * 0.5f;

#ifdef USE_LEGACY_LIGHT
	Fog *= Fog;
#endif

    return float4(Color, Fog);
}

