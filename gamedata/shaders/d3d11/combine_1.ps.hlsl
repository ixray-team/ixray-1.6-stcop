#include "common.hlsli"

#ifdef USE_OFFSCREEN_REFLECTIONS
	#define USE_VIEW_REFLECTIONS
#endif

#ifdef USE_PROCEDURAL_AERIAL_PERSPECTIVE
    #include "common_aerial.hlsli"
    Texture3D<float4> s_aerial_perspective_lut;
    Texture3D<float4> s_aerial_direct_lut;
    Texture3D<float4> s_aerial_transmittance_lut;
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
		//SpecularIrradance *= SpecularIrradance < 1.0f ? rcp(1.0f - SpecularIrradance) : 1.0f;
	#else
		float3 SpecularIrradance = CompureSpecularIrradance
		(
			//reflect(O.View, O.Normal), 
        IBLReflectionDirection(O.View, O.Normal),
		#ifdef USE_VIEW_REFLECTIONS
			O.Depth > 0.02 ? O.Hemi : 1.0f,
		#else
			O.Hemi,
		#endif
			O.Roughness
		);
	#endif

    float hack_ambient = 0.01 + smoothstep(0.0, 0.7, O.Hemi);
    float3 DiffuseIrradance = CompureDiffuseIrradance(O.Normal, O.Hemi) + L_ambient.xyz * hack_ambient;
    float NdotV = saturate(dot(O.Normal, -O.View.xyz));
    float3 Ambient = AmbientLightingImpl(DiffuseIrradance, SpecularIrradance, NdotV, O.Color, O.Specular, O.Roughness);
#else
    float3 Ambient = AmbientLightingLegcay(O.View, O.Normal, O.Color, O.Material, O.Gloss, O.Hemi);
#endif

    float3 Color = Occ * Ambient + Light;
    Color = max(Color, 0.0f);
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
    
    #ifdef USE_PROCEDURAL_AERIAL_PERSPECTIVE
        uint ap_width, ap_height, ap_depth;
        s_aerial_perspective_lut.GetDimensions(ap_width, ap_height, ap_depth);
        float3 ap_uv = sky_aerial_uv(I.texcoord, O.ViewDist * SKY_WORLD_TO_KM, ap_depth);
        float3 ap_ambient = s_aerial_perspective_lut.SampleLevel(smp_rtlinear, ap_uv, 0.0f).rgb;
        float3 ap_direct = s_aerial_direct_lut.SampleLevel(smp_rtlinear, ap_uv, 0.0f).rgb;
        float3 ap_T = s_aerial_transmittance_lut.SampleLevel(smp_rtlinear, ap_uv, 0.0f).rgb;
        Color = Color * saturate(ap_T) + ap_ambient + ap_direct;
    #endif
    
#define DEBUG_IBL_REFLECTION_VECTOR 0

#if DEBUG_IBL_REFLECTION_VECTOR

    float3 debug_view = safe_normalize(O.View);

    float3 debug_normal = safe_normalize(O.Normal);

    float3 debug_reflection = safe_normalize(reflect(debug_view, debug_normal));

    NdotV = dot(debug_normal, -debug_view);

    float NdotR = dot(debug_normal, debug_reflection);
    

    // Red:
    // shading normal направлена от камеры.
    //
    // Green:
    // reflected vector оказался под shading surface.
    //
    // Blue:
    // нарушилось ожидаемое тождество NdotR == NdotV.
    return float4(NdotV < 0.0f ? 1.0f : 0.0f, NdotR < 0.0f ? 1.0f : 0.0f, NdotV, 0.0f);
    //return float4(NdotV, 0.f, 0.f, 0.0f);
#endif

    return float4(Color, 0.0f);
}

