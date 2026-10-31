#include "common.hlsli"
#include "reflections.hlsli"

TextureCube s_env0 : register(t26);
TextureCube s_env1 : register(t49);

float4 main(PSInputFullscreen I) : SV_Target
{
#if defined(USE_LEGACY_LIGHT) && defined(USE_RAIN_PUDDLES)
	float Amount = s_surface.Load(int3(I.hpos.xy, 0)).z;
	[branch]
	if (Amount < EPS_L)
	{
		discard;
	}

	IXRayGbuffer Gbuffer = (IXRayGbuffer)0;
	GbufferUnpack((uint2)I.hpos.xy, Gbuffer);
	if (Gbuffer.Depth <= 0.02f || Gbuffer.Depth >= 1.0f || Gbuffer.MaterialID != TERRAIN_ID)
	{
		discard;
	}

	float3 Reflection = reflect(Gbuffer.View, Gbuffer.Normal);
	float3 WorldReflection = mul((float3x3)m_invV, Reflection);
	float2 Rotation;
	sincos(L_sky_color.w, Rotation.x, Rotation.y);
	WorldReflection.xz = float2(WorldReflection.x * Rotation.y - WorldReflection.z * Rotation.x, WorldReflection.x * Rotation.x + WorldReflection.z * Rotation.y);
#ifndef USE_FULL_SKY_SPHERE
	RemapVector(WorldReflection);
#endif
	float3 Environment0 = s_env0.SampleLevel(smp_rtlinear, WorldReflection, 0.0f).xyz;
	float3 Environment1 = s_env1.SampleLevel(smp_rtlinear, WorldReflection, 0.0f).xyz;
	float3 Environment = lerp(Environment0, Environment1, L_ambient.w);
#ifdef USE_BGRA_SKYCOLOR
	Environment *= L_sky_color.zyx;
#else
	Environment *= L_sky_color.xyz;
#endif
	float3 Color = GammaToLinear(Environment);
#ifdef USE_SSLR_ON_WATER
	float3 Point = GbufferGetPointRealUnjitter(I.texcoord, Gbuffer.Depth);
	// Use the same trace and previous-frame reprojection as the water shader.
	float4 ReflectedScene = ScreenSpaceLocalReflections(Point * 0.99f + Gbuffer.Normal * 0.07f, Reflection);
	Color = lerp(Color, ReflectedScene.xyz, ReflectedScene.w);
#endif
	float Fresnel = pow(saturate(dot(Reflection, Gbuffer.View)), 5.0f);
	float Fog = saturate(Gbuffer.ViewDist * fog_params.w + fog_params.x);
	float Opacity = Amount * (0.25f + 0.25f * Fresnel) * 0.8f * (1.0f - Fog * Fog);
	return float4(Color, Opacity);
#else
	return 0.0f;
#endif
}
