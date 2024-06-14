#include "common.hlsli"

#include "lmodel.hlsli"
#include "hmodel.hlsli"

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

Texture2D<float> s_occ;

struct _input
{
    float4 tc0 : TEXCOORD0;
    float2 tcJ : TEXCOORD1;
    float4 pos2d : SV_Position;
};

float4 main(_input I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.tc0, I.pos2d, O);
    float4 Light = s_accumulator.Sample(smp_nofilter, I.tc0);

#ifdef USE_R2_STATIC_SUN
    Light.xyz += O.SSS * DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.PointReal.xyz, O.Color, O.Metalness, O.Roughness);
#endif

    //  Calculate SSAO
    float Occ = 1.0f;

#if SSAO_QUALITY > 0
    Occ = s_occ.Sample(smp_nofilter, I.tc0);
#endif

    float3 Ambient = Occ * AmbientLighting(O.PointReal, O.Normal, O.Color, O.Metalness, O.Roughness, O.Hemi);
    float3 Color = Ambient + Light.xyz;

    // here should be distance fog
    float Fog = saturate(length(O.PointReal) * fog_params.w + fog_params.x);
    Color = lerp(Color, fog_color, Fog);

    return float4(Color, Fog * Fog);
}
