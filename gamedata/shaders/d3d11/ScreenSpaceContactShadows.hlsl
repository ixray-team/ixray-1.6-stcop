#ifndef screenspacecontactshadows_hlsl_included
#define screenspacecontactshadows_hlsl_included

#ifndef HUD_SHADOWS_STEPS
#define HUD_SHADOWS_STEPS 35
#endif

#ifndef HUD_SHADOWS_TRACE_LEN
	#define HUD_SHADOWS_TRACE_LEN 0.07f
#endif

float RayTraceContactShadow(float2 TexCoord, float3 Point, float3 LightDir)
{
	Point.xyz *= 0.99f;
	LightDir *= min(Point.z, HUD_SHADOWS_TRACE_LEN);
	
	float4 StartProj = mul(m_P_hud, float4(Point, 1.0f)); StartProj.xyz /= StartProj.w;
	float4 EndProj = mul(m_P_hud, float4(Point - LightDir, 1.0f)); EndProj.xyz /= EndProj.w;
	
	StartProj.xy = StartProj.xy * float2(0.5f, -0.5f) + 0.5f; StartProj.z *= 0.02f;
	EndProj.xy = EndProj.xy * float2(0.5f, -0.5f) + 0.5f; EndProj.z *= 0.02f;
	
	LightDir = EndProj.xyz - StartProj.xyz;
	StartProj.xy = TexCoord.xy;
	
	float Len = GetMaxDirLength(StartProj.xyz, rcp(LightDir));
	
	LightDir *= min(1.0f, Len);
	LightDir *= rcp(HUD_SHADOWS_STEPS);
	
	float ContactShadow = 0.0f;	
	
	[loop]
	for (int i = 0; i < HUD_SHADOWS_STEPS; ++i)
	{
		StartProj.xyz += LightDir * float(0.8f + 0.4f * Hash(StartProj.xyz));
		float HitDepth = s_position.SampleLevel(smp_nofilter, StartProj.xy, 0).x;
		
		if (HitDepth <= StartProj.z)
		{
			ContactShadow += 0.2f;
			if (ContactShadow >= 1.0f)
			{
				ContactShadow = 1.0f;
				break;
			}
		}
	}
	
	ContactShadow *= GetBorderAtten(StartProj.xy, 0.0125f);
	return 1.0f - saturate(ContactShadow);
}

// Depth march after raymarch.hlsl by Tomasz Stachowiak (Tiny Glade), dual licensed Apache-2.0 / MIT
// Every term varies continuously with depth (fixed step count, no pixel-count clamp), so receding surfaces show no iso-depth lines
float SunContactOcclusion(float2 TexCoord, float3 Point, float3 LightDir, float Jitter)
{
	float4 Start = mul(m_P, float4(Point, 1.0f));
	float4 End = mul(m_P, float4(Point - LightDir * min(Point.z * 0.5f, sun_contact_ray.y), 1.0f));

	float3 Delta = float3((End.xy / End.w - Start.xy / Start.w) * float2(0.5f, -0.5f), rcp(End.w) - rcp(Start.w));
	float2 Exit = max((1.0f - TexCoord) / Delta.xy, -TexCoord / Delta.xy);
	Delta *= min(1.0f, min(Exit.x, Exit.y));

	float StartT = min(sun_contact_ray.z * rcp(length(Delta.xy * pos_decompression_params2.xy)), 0.5f);
	float Thickness = sun_contact_depth.x + Point.z * sun_contact_depth.y;
	float Bias = sun_contact_depth.z + Point.z * sun_contact_depth.w;
	float Occlusion = 0.0f;

	[loop]
	for (float Step = 0.0f; Step < sun_contact_ray.x && Occlusion < 1.0f; ++Step)
	{
		float T = (Step + Jitter) / sun_contact_ray.x;
		float3 Ray = float3(TexCoord, rcp(Start.w)) + Delta * lerp(StartT, 1.0f, T * T);
		// The HUD sits in the same depth buffer at depth < 0.02: never let it occlude the world, nor blend into a filtered depth
		float4 Raw = s_position.GatherRed(smp_nofilter, Ray.xy);
		if (any(Raw < 0.02f))
		{
			continue;
		}
		float4 Z = rcp(m_invP._43 * Raw + m_invP._44);
		float2 F = frac(Ray.xy * pos_decompression_params2.xy - 0.5f);
		float Filtered = rcp(m_invP._43 * lerp(lerp(Raw.w, Raw.z, F.x), lerp(Raw.x, Raw.y, F.x), F.y) + m_invP._44);
		float Nearest = F.y < 0.5f ? (F.x < 0.5f ? Z.w : Z.z) : (F.x < 0.5f ? Z.x : Z.y);
		float RayZ = rcp(Ray.z);
		// A quad spanning more than the thickness straddles a silhouette: its filtered depth mixes foreground and background
		float Edge = saturate(2.0f - (max(max(Z.x, Z.y), max(Z.z, Z.w)) - min(min(Z.x, Z.y), min(Z.z, Z.w))) / Thickness);
		float Hit = saturate((RayZ - max(Filtered, Nearest)) / Bias - 1.0f) * smoothstep(Thickness, 0.5f * Thickness, RayZ - min(Filtered, Nearest));
		Occlusion = max(Occlusion, Hit * Edge * GetBorderAtten(Ray.xy, 0.0125f));
	}

	return Occlusion;
}
#endif

