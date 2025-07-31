#include "common.hlsli"

#if SUN_QUALITY > 2
    #define USE_ULTRA_SHADOWS
#endif

#include "shadow.hlsli"
#include "metalic_roughness_light.hlsli"
#include "ScreenSpaceContactShadows.hlsl"

float4 main(v2p_volume I) : SV_Target
{
    float2 tcProj = I.tc.xy / I.tc.w;
	
    IXrayGbuffer O;
    GbufferUnpack(tcProj, I.hpos.xy, O);

    float3 Shift = O.Normal;

    if (O.SSS > 0.0f)
    {
        Shift *= dot(Ldynamic_dir.xyz, Shift) >= 0.0 ? -1.0f : 1.0f;
    }

    float4 Point = float4(O.Point.xyz, 1.f);
    Point.xyz += Shift * 0.025f;
	
    float4 PS = mul(m_shadow, Point);

#ifdef USE_FAR_ATTENTION
    float3 Factor = smoothstep(0.5f, 0.45f, abs(PS.xyz / PS.w - 0.5f));
    float Fade = Factor.x * Factor.y * Factor.z;
	
    O.SSS *= 0.5f + 0.5f * Fade;
#endif

    float3 Light = DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Metalness, O.Roughness, O.F0);
    Light += SimpleTranslucency(Ldynamic_color.xyz, Ldynamic_dir.xyz, O.Normal) * O.SSS * O.Color;

#if SUN_QUALITY == 2
    float Shadow = shadow_high(PS);
#else
    float Shadow = shadow(PS);
#endif

#ifdef USE_FAR_ATTENTION
	float FarShadow = dot(Ldynamic_dir.xyz, O.Normal.xyz);
	FarShadow = smoothstep(0.75f, 0.6f, FarShadow) * saturate(O.Hemi * 8.0f - 2.0f);
	
    Shadow = lerp(FarShadow, Shadow, Fade);

#elif defined(USE_HUD_SHADOWS)
    if (O.Depth < 0.02f && dot(Shadow.xxx, Light.xyz) > 0.0001f)
    {
        RayTraceContactShadow(tcProj, O.PointHud, Ldynamic_dir.xyz, Light);
    }
#endif

    Shadow *= sunmask(Point);
    return float4(Light * Shadow, Shadow);
}

