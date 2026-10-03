#include "common.hlsli"
#define USE_SSLR_DEPTH_MIN
Texture2D<float> s_sslr_depth_min;
#include "reflections.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "metalic_roughness_light.hlsli"

RWTexture2D<float3> u_sslr : register(u0);
RWTexture2D<float4> u_sslr_data : register(u1);

float3 ReflectionSky(float3 Direction, float Hemi, float Roughness)
{
    float3 WorldDirection = mul((float3x3)m_invV, Direction);
    float2 Rotation;
    sincos(L_sky_color.w, Rotation.x, Rotation.y);
    WorldDirection.xz = float2(WorldDirection.x * Rotation.y - WorldDirection.z * Rotation.x,
        WorldDirection.z * Rotation.y + WorldDirection.x * Rotation.x);
    return GammaToLinear(CompureSpecularIrradance(mul((float3x3)m_V, WorldDirection), Hemi, Roughness));
}

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
    uint Width, Height;
    u_sslr.GetDimensions(Width, Height);
    if (any(DTid >= uint2(Width, Height)))
        return;

    IXRayGbuffer O = (IXRayGbuffer)0;
    GbufferUnpack(DTid, O);
    float2 TexCoord = (DTid + 0.5f) * pos_decompression_params2.zw;
    float3 ReflectPoint = GbufferGetPointRealJitter(TexCoord, O.Depth);
    float3 View = normalize(ReflectPoint);
    if (O.Depth >= 1.0f)
    {
        u_sslr[DTid] = min(ReflectionSky(View, 1.0f, 0.2f), 64000.0f);
        u_sslr_data[DTid] = float4(View * fog_params.z, 0.0f);
        return;
    }

    float2 Jitter = s_blue_noise[uint3(DTid % 128, uint(m_taa_jitter.w) % 32)].xy;
#ifndef USE_LEGACY_LIGHT
    float Roughness = O.Roughness;
    float3 Half = sample_vndf_isotropic(O.Normal, -View, Jitter * float2(1.0f, 0.7f), Roughness * Roughness);
#else
    float Roughness = 1.0f - O.Gloss;
    float3 Half = O.Normal;
#endif
    if (!all(isfinite(Half)))
        Half = O.Normal;
    float3 Reflection = reflect(View, Half);
    if (dot(Reflection, O.Normal) < 0.0f)
        Reflection = normalize(Reflection + O.Normal);
    if (!all(isfinite(Reflection)))
        Reflection = reflect(View, O.Normal);

    float PDF = pdf_vndf_isotropic(O.Normal, -View, Reflection, Roughness * Roughness);
    PDF = isfinite(PDF) && PDF > EPS_S ? PDF : EPS_S;
    bool IsHUD = O.Depth < 0.02f;
    float3 StartPoint = ReflectPoint + (IsHUD ? 0.0f : O.Normal * 0.025f);
    float3 HitPoint = StartPoint + Reflection * fog_params.z;
#ifdef USE_OFFSCREEN_REFLECTIONS
    float SkyHemi = 1.0f;
#else
    float SkyHemi = IsHUD ? 1.0f : O.Hemi;
#endif
    float3 Hemi = ReflectionSky(Reflection, SkyHemi, 0.0f);
    float3 Color = Hemi;

    ReflectionHit SSR = FastViewReflectionsSSR(StartPoint, Reflection, IsHUD);
    float3 ScreenColor = 0.0f;
    float Confidence = 0.0f;
    if (SSR.Confidence > 0.0f && ReflectionScreenUV(SSR.UV))
    {
        ScreenColor = s_image.SampleLevel(smp_rtlinear, SSR.UV, 0).xyz;
        if (all(isfinite(ScreenColor)))
            Confidence = SSR.Confidence;
    }

#ifdef USE_OFFSCREEN_REFLECTIONS
    if (Confidence < 1.0f)
    {
        float4 VSLR = FastViewReflections(StartPoint, Reflection);
        if (VSLR.w > 0.0f)
        {
            float3 CapturePoint = ReflectionCapturePoint(VSLR.xyz);
            float3 CaptureColor = s_env.SampleLevel(smp_linear, CapturePoint, 0).xyz;
            float Fog = saturate((length(StartPoint) + length(VSLR.xyz - StartPoint)) * fog_params.w + fog_params.x);
            Color = lerp(Color, CaptureColor, VSLR.w * (1.0f - Fog * Fog));
            HitPoint = VSLR.xyz;
        }
    }
#endif
    if (Confidence > 0.0f)
    {
        Color = lerp(Color, ScreenColor, Confidence);
        HitPoint = SSR.Point;
    }

    float Fog = saturate(max(length(HitPoint), length(StartPoint) + length(HitPoint - StartPoint)) * fog_params.w + fog_params.x);
    Color = lerp(Color, Hemi, Fog);
    Color = all(isfinite(Color)) ? max(Color, 0.0f) : Hemi;
    float Weight = (24.0f - clamp(log2(PDF), -23.5f, 23.5f)) * (IsHUD ? -1.0f : 1.0f);
    u_sslr[DTid] = min(Color, 64000.0f);
    u_sslr_data[DTid] = float4(HitPoint, Weight);
}
