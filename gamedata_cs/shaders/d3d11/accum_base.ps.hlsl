#include "common.hlsli"
#include "shadow.hlsli"

#include "metalic_roughness_light.hlsli"
#include "ScreenSpaceContactShadows.hlsl"

uniform float4 m_lmap[2];
uniform int Ldynamic_hud;

float4 main(p_volume I, float4 pos2d : SV_POSITION) : SV_Target
{
    float2 tcProj = I.tc.xy / I.tc.w;
	
    IXrayGbuffer O;
    GbufferUnpack(tcProj, pos2d.xy, O);

    float4 Point = float4(Ldynamic_hud > 0 ? O.PointHud.xyz : O.Point.xyz, 1.0f);

	float3 LightDirection = normalize(O.PointReal.xyz - Ldynamic_pos.xyz);
    float3 Light = DirectLight(Ldynamic_color, LightDirection, O.Normal, O.View.xyz, O.Color, O.Metalness, O.Roughness, O.F0);

    float3 Lightmap = ComputeLightAttention(Point.xyz - Ldynamic_pos.xyz, Ldynamic_pos.w);
    Point.xyz += O.Normal * 0.025f;

    float4 PS = mul(m_shadow, Point);

#ifdef USE_SHADOW
    Lightmap *= max(Ldynamic_hud, shadow(PS));

    #ifdef USE_HUD_SHADOWS
		if (O.Depth < 0.02f && dot(Lightmap.xyz, Light.xyz) > 0.0001f)
		{
			RayTraceContactShadow(tcProj, O.PointHud, LightDirection, Lightmap);
		}
    #endif
#endif

#ifdef USE_LMAP
    #ifdef USE_LMAPXFORM
		PS.x = dot(Point, m_lmap[0]);
		PS.y = dot(Point, m_lmap[1]);
    #endif
    Lightmap *= s_lmap.SampleLevel(smp_rtlinear, PS.xy / PS.w, 0.0f).xyz;
#endif

    return float4(Lightmap.xyz * Light.xyz, 0.0f);
}

