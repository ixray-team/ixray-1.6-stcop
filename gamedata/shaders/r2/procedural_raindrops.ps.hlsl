#include "common.hlsli"
#include "shared\VisorDrops.hlsli"

uniform float4 visor_rain;
uniform float4 screen_res;

float4 main(float2 Texcoord : TEXCOORD0) : COLOR
{
	float RainAmount = saturate(visor_rain.x);
	float2 Aspect = float2(screen_res.x / screen_res.y, -1.f);
	float2 Normal = DistortUVSM3(Texcoord, RainAmount, timers.x, Aspect, screen_res.xy);
	float3 Scene = tex2D(s_image, Texcoord).xyz;
	float3 RainScene = tex2D(s_image, Texcoord + Normal).xyz;
	return float4(lerp(Scene, RainScene, RainAmount), 1.f);
}
