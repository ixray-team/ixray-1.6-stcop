#include "common.hlsli"

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "reflections.hlsli"

Texture2D<float> s_occ;

float3 ReconstructNormalFromPosition(float3 P)
{
    float3 dx = ddx(P);
    float3 dy = ddy(P);

    float3 N = normalize(cross(dx, dy));

    return N;
}

//#define DEBUG_GBUFFER_NORMAL_COMPARE

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
        Fog = saturate(O.ViewDist * fog_params.w + fog_params.x);
        Fog *= Fog;
    #else  //NEW_FOGGIN
        //float a = 1.0f;
        //float b = 0.002f;
        float denom = F_base - exp(-F_dens * (fog_params.z - fog_params.y));
        Fog = (F_base - exp(-F_dens * (O.ViewDist - fog_params.y))) / denom;
        Fog = saturate(Fog);
    #endif

    #ifdef USE_LEGACY_LIGHT
        Fog *= Fog;
    #endif
    #ifdef DEBUG_GBUFFER_NORMAL_COMPARE
        float3 a = GbufferGetPoint(I.hpos.xy).xyz;
        float3 b = GbufferGetPoint(I.hpos.xy + float2(1,0)).xyz;
        float3 c = GbufferGetPoint(I.hpos.xy + float2(0,1)).xyz;
        float3 restoredNormal = normalize(cross(b - a, c - a));
        float3 normalcheck = 0.5 - 0.5 * dot(restoredNormal, O.Normal);
    #endif

    float3 nfogcolor = 0.0f;
    float maxlod = 1.0f;
    float3 vec = O.View.xyz;
    float3 SampleLast = sky_s0.SampleLevel(smp_linear, vec, maxlod).xyz;
	float3 SampleNext = sky_s1.SampleLevel(smp_linear, vec, maxlod).xyz;
    nfogcolor = lerp(SampleLast, SampleNext, L_hemi_color.w);

    Color = lerp(Color, nfogcolor * fog_color.rgb, Fog);


    return float4(Color, 0.f);
}

