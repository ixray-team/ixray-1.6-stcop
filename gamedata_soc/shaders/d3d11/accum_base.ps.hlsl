#include "common.hlsli"
#include "shadow.hlsli"

#include "metalic_roughness_light.hlsli"
#include "ScreenSpaceContactShadows.hlsl"


[earlydepthstencil] //LVutner: Force early-z
float4 main(p_volume I, float4 pos2d : SV_POSITION) : SV_Target
{
    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack((uint2)pos2d.xy, O);

    float4 Point = float4(Ldynamic_hud > 0 ? O.PointHud.xyz : O.Point.xyz, 1.0f);
	float3 LightDirection = normalize(O.PointReal.xyz - Ldynamic_pos.xyz);
	
#ifdef USE_LEGACY_LIGHT
    float3 Light = DirectLightLegacy(Ldynamic_color, LightDirection, O.Normal, O.View.xyz, O.Color, O.Material, O.Gloss);
#else
    float3 Light = DirectLight(Ldynamic_color, LightDirection, O.Normal, O.View.xyz, O.Color, O.Specular, O.Roughness);
#endif

    float3 Lightmap = GetLightAttention(Point.xyz - Ldynamic_pos.xyz, Ldynamic_pos.w, 2.0f);
    Point.xyz = O.Normal * 0.025f + Point.xyz;

    float4 PS = mul(m_shadow, Point);

#ifdef USE_SHADOW
    Lightmap *= max(Ldynamic_hud, shadow_local(PS.xyz / PS.w));

    #ifdef USE_HUD_SHADOWS
		[branch]
		if (O.Depth < 0.02f && dot(Lightmap.xyz, Light.xyz) > EPS_S)
		{
			Lightmap *= RayTraceContactShadow(I.tc.xy / I.tc.w, O.PointHud, LightDirection);
		}
    #endif
#endif

#if defined(USE_LMAP) || defined(USE_SMAP)
	Point = float4(O.Point.xyz, 1.0f);
	PS = mul(m_shadow, Point);
#endif

#ifdef USE_SMAP
	Lightmap *= O.Depth > 0.02f ? s_mask.SampleLevel(smp_rtlinear, PS.xy / PS.w, 0.0f).xyz : 1.0f;
#endif

#ifdef USE_LMAP
    #ifdef USE_LMAPXFORM
		PS.x = dot(Point, m_lmap[0]);
		PS.y = dot(Point, m_lmap[1]);
    #endif
	
    Lightmap *= GammaToLinear(s_lmap.SampleLevel(smp_rtlinear, PS.xy / PS.w, 0.0f).xyz);
#endif
	
    return float4(Lightmap * Light, 0.0f);
}


