#ifndef screenspacecontactshadows_hlsl_included
#define screenspacecontactshadows_hlsl_included

#ifndef HUD_SHADOWS_STEPS
#define HUD_SHADOWS_STEPS 24
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

void RayTraceContactShadow(float2 TexCoord, float3 Point, float3 LightDir, inout float3 Light)
{
	float2 uv = TexCoord * float2(1920,1080);
    uint frame = uint(uint(m_taa_jitter.w) % 32);

    if((frame & 2u) != 0u) uv = float2(-uv.y, uv.x);
    if((frame & 1u) != 0u) uv.x = -uv.x;

 
    // http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/#dither
    float jit =  frac(uv.x*0.7548776662 + uv.y*0.56984029 + float(frame)*0.41421356*1.0);

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

	float ContactShadow = 0.0f;	
	
	[unroll(HUD_SHADOWS_STEPS)]
	for (int i = 0; i < HUD_SHADOWS_STEPS; i+=2)
	{
		float2 inc = (jit + float2(i, i + 1)) / HUD_SHADOWS_STEPS;
		float4 rt_coords = StartProj.xyxy + LightDir.xyxy * inc.xxyy;

		float2 rt_depth;
		rt_depth.x = s_position.SampleLevel(smp_nofilter, rt_coords.xy, 0).x;
		rt_depth.y = s_position.SampleLevel(smp_nofilter, rt_coords.zw, 0).x;

		float2 rt_z = StartProj.zz + LightDir.zz * inc.xy;
		
		[unroll]
		for(int j = 0; j < 2; j++)
		{
			if (rt_depth[j] <= rt_z[j])
			{
				ContactShadow += 0.2f;
				if (ContactShadow >= 1.0f)
				{
					ContactShadow = 1.0f;
					break;
				}
			}
		}
	}
	
	Light *= PushGamma(1.0f - saturate(ContactShadow));
}
#endif

