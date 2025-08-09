#include "common.hlsli"

#if defined(USE_OFFSCREEN_REFLECTIONS) && !defined(USE_SSLR_REFLECTIONS)
#define USE_VIEW_REFLECTIONS
#endif

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "reflections.hlsli"

Texture2D<float> s_occ;

struct _input
{
    float4 tc0 : TEXCOORD0;
    float2 tcJ : TEXCOORD1;
    float4 pos2d : SV_POSITION;
};

float4 main(_input I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.tc0.xy, I.pos2d.xy, O);
    float3 Light = s_accumulator.Load(int3(I.pos2d.xy, 0)).xyz;

#ifdef USE_R2_STATIC_SUN
    Light += O.SSS * DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Metalness, O.Roughness, O.F0);
#endif

    float Occ = min(O.AO, s_occ.SampleLevel(smp_rtlinear, I.tc0.xy, 0.0f).x);

#ifndef USE_LEGACY_LIGHT
	#ifdef USE_SSLR_REFLECTIONS
		float2 motion_vectors = s_velocity.SampleLevel(smp_nofilter, I.tc0, 0.0).xy * float2(0.5, -0.5);
		float3 SpecularIrradance = s_refl.SampleLevel(smp_rtlinear, I.tc0 - motion_vectors, 0.0).xyz;
		SpecularIrradance *= rcp(1.00001f - SpecularIrradance);
	#else
		float3 SpecularIrradance = CompureSpecularIrradance(reflect(O.View, O.Normal), O.Hemi, O.Roughness);
	#endif

	float3 DiffuseIrradance = CompureDiffuseIrradance(O.Normal, O.Hemi) + L_ambient.xyz;	
    float3 Ambient = AmbientLighting(DiffuseIrradance, SpecularIrradance, max(0.0, dot(O.Normal, -O.View.xyz)), O.Color, O.Metalness, O.Roughness, O.F0);
#else
    float3 Ambient = AmbientLighting(O.View, O.Normal, O.Color, O.Metalness, O.Roughness, O.Hemi, O.F0);
#endif

    float3 Color = Occ * Ambient + Light;

    float Fog = PushGamma(saturate(O.ViewDist * fog_params.w + fog_params.x));
    Color = lerp(Color, PushGamma(fog_color.xyz), Fog);

#ifdef USE_LEGACY_LIGHT
	Fog *= Fog;
#endif

    return float4(Color, Fog);
}

