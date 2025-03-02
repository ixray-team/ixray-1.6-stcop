#ifndef screenspacecontactshadows_hlsl_included
#define screenspacecontactshadows_hlsl_included

#ifndef HUD_SHADOWS_STEPS
#define HUD_SHADOWS_STEPS 35
#endif

#ifndef HUD_SHADOWS_TRACE_LEN
#define HUD_SHADOWS_TRACE_LEN 0.07f
#endif

float SampleHudHitPoint(float2 TexCoord)
{
    float depth = s_position.SampleLevel(smp_nofilter, TexCoord, 0).x;
    return depth_unpack.z * rcp(min(1.0f, depth * 50.0f) - depth_unpack.w);
}

float2 GetPointTexCoord(float3 Point)
{
    Point.xy *= rcp(pos_decompression_params_hud.xy * Point.z);
    return saturate(Point.xy * 0.5f + 0.5f);
}

float GetMaxDirLength(float3 Point, float3 RDir)
{
	float3 FirstPoint = RDir - Point * RDir;
	float3 LastPoint = -Point * RDir;
	
	float3 MaxPoint = max(FirstPoint, LastPoint);
	return min(MaxPoint.x, min(MaxPoint.z, MaxPoint.y));
}

void RayTraceContactShadow(float2 TexCoord, float3 Point, float3 LightDir, inout float3 Light)
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
	
	[unroll(HUD_SHADOWS_STEPS)]
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
	Light *= PushGamma(1.0f - saturate(ContactShadow));
}
#endif

