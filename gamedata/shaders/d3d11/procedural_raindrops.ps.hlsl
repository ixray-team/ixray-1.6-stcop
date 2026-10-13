#include "common.hlsli"
#include "shared\VisorDrops.hlsli"

uniform float4 visor_rain;

float4 main(PSInputFullscreen I) : SV_Target
{
	float RainAmount = saturate(visor_rain.x);
	float2 Aspect = float2(screen_res.x / screen_res.y, -1.f);
	float2 Normal = DistortUV(I.texcoord, RainAmount, timers.x, Aspect);
	float3 Scene = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0.f).xyz;
	float3 RainScene = s_image.SampleLevel(smp_rtlinear, I.texcoord + Normal, 0.f).xyz;
	return float4(lerp(Scene, RainScene, RainAmount), 1.f);
}
