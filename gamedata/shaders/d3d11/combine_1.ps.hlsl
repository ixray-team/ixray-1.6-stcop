#include "common.hlsli"

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "reflections.hlsli"

Texture2D<float> s_occ;

float4 main(PSInputFullscreen I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);
    float3 Light = s_accumulator.Load(int3(I.hpos.xy, 0)).xyz;

#ifdef USE_R2_STATIC_SUN
    Light += O.SSS * DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Metalness, O.Roughness, O.F0);
#endif

    float Occ = O.AO * s_occ.SampleLevel(smp_rtlinear, I.texcoord.xy, 0.0f).x;

#ifndef USE_LEGACY_LIGHT
	#ifdef USE_SSLR_REFLECTIONS
		float3 SpecularIrradance = s_refl.SampleLevel(smp_rtlinear, I.texcoord, 0.0).xyz;
		SpecularIrradance *= SpecularIrradance > 0.0f ? rcp(1.0f - SpecularIrradance) : 0.0f;
	#else
		float3 SpecularIrradance = CompureSpecularIrradance(reflect(O.View, O.Normal), O.Hemi, O.Roughness);
	#endif

	float3 DiffuseIrradance = CompureDiffuseIrradance(O.Normal, O.Hemi) + L_ambient.xyz;	
    float3 Ambient = AmbientLighting(DiffuseIrradance, SpecularIrradance, max(0.0, dot(O.Normal, -O.View.xyz)), O.Color, O.Metalness, O.Roughness, O.F0);
#else
    float3 Ambient = AmbientLighting(O.View, O.Normal, O.Color, O.Metalness, O.Roughness, O.Hemi, O.F0);
#endif

    float3 Color = Occ * Ambient + Light;
    float Fog = 0.0f;
#ifndef NEW_FOGGIN
    Fog = PushGamma(saturate(O.ViewDist * fog_params.w + fog_params.x));
#else  //NEW_FOGGIN
    float a = 1.0f;
    float b = 0.0008f;
    float denom = a - exp(-b * (fog_params.z - fog_params.y));
    Fog = (a - exp(-b * (O.ViewDist - fog_params.y))) / denom;
    Fog = saturate(Fog);
#endif

    Color = lerp(Color, lerp(0.f, 1.f, 0.1*fog_color.rgb), Fog);

#ifdef USE_LEGACY_LIGHT
	Fog *= Fog;
#endif

    return float4(Color, Fog*Fog);
}

