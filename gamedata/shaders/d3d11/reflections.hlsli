#ifndef reflections_h_2134124_inc
#define reflections_h_2134124_inc

// Screen Space Sky Reflections off
#define SKYBLED_FADE
#define USE_BASE_HUD_REFLECTIONS

#define USE_VASYAN_CUTOFF
// #define VSLR_SLOW_BREAK
// #define SSLR_SLOW_BREAK

uniform float4 scaled_screen_res;

float get_depth_fast(float2 tc)
{
    float P = s_position.SampleLevel(smp_rtlinear, tc, 0).x;
    return depth_unpack.x * rcp(P - depth_unpack.y);
}

float3 gbuf_unpack_position(float2 uv)
{
    float depth = get_depth_fast(uv);
    uv = uv * 2.0f - 1.0f;
    return float3(uv * pos_decompression_params.xy, 1.0f) * depth;
}

float3 gbuf_unpack_position(float2 uv, float depth)
{
    uv = uv * 2.0f - 1.0f;
    return float3(uv * pos_decompression_params.xy, 1.0f) * depth;
}

float2 gbuf_unpack_uv(float3 position)
{
    position.xy *= rcp(pos_decompression_params.xy * position.z);
    return saturate(position.xy * 0.5 + 0.5);
}

#define SSLR_STEPS 30
#define MAX_FIND_STEP 5

float BinaryRefinement(inout float3 EndProj, float3 Reflect)
{
	float HitDepth = 0.0f;
	
	[unroll(MAX_FIND_STEP)]
	for(int i = 0; i < MAX_FIND_STEP; ++i)
	{
		HitDepth = s_env.SampleLevel(smp_rtlinear, EndProj.xyz, 0).w;
		HitDepth *= HitDepth;
		
		Reflect *= 0.5f;
		EndProj += dot(EndProj, EndProj) > HitDepth ? -Reflect : Reflect;
	}
	
	HitDepth = s_env.SampleLevel(smp_rtlinear, EndProj.xyz, 0).w;
	HitDepth *= HitDepth;
	
	return HitDepth;
}

float BinaryRefinementHUD(inout float3 EndProj, float3 Reflect)
{
	float HitDepth = 0.0f;
	
	[unroll(MAX_FIND_STEP)]
	for(int i = 0; i < MAX_FIND_STEP; ++i)
	{
		HitDepth = s_position.SampleLevel(smp_nofilter, EndProj.xy, 0).x;
		
		Reflect *= 0.5f;
		EndProj += EndProj.z > HitDepth ? -Reflect : Reflect;
	}
	
	HitDepth = s_position.SampleLevel(smp_nofilter, EndProj.xy, 0).x;
	
	return HitDepth;
}

float4 FastViewReflections(float3 Point, float3 Reflect)
{
	float3 SamplePoint = Reflect;
	
	float SampleHitPointLen = 0;
	float Step = rcp(SSLR_STEPS + 1) * 0.01f;
	float L = 0.011f;
	
	float RadiusS = fog_params.z * fog_params.z;
	float DistanceS = dot(Point, Point);
	
	float DirectionS = dot(Reflect, Reflect);
	
	if(DistanceS >= RadiusS) {
		return float4(SamplePoint, 0.0f);
	}
	
	Step *= sqrt((RadiusS - DistanceS) * rcp(DirectionS));
	
	float Fade = 0;
	float Delta = 0.0f;
	
	[loop]
	for(uint i = 0; i < SSLR_STEPS; ++i)
	{
		float JStep = Step * lerp(0.8f, 1.2f, Hash(dot(sin(SamplePoint.xyz * timers.x), float3(12.989, 42.364, 78.233))));
		L += JStep;
		
		Step *= 1.25f;
		
		SamplePoint.xyz = Point.xyz + Reflect * L;
		
		SampleHitPointLen = s_env.SampleLevel(smp_rtlinear, SamplePoint.xyz, 0).w;
		SampleHitPointLen *= SampleHitPointLen;
		
		Delta = dot(SamplePoint, SamplePoint) - SampleHitPointLen;
		
		if (Delta > 0 /*&& Delta <= JStep * 0.8f*/)
		{
			float3 JReflect = Reflect * JStep * 0.5f;
			SamplePoint.xyz -= JReflect;
			
			SampleHitPointLen = BinaryRefinement(SamplePoint.xyz, JReflect);
			Delta = dot(SamplePoint.xyz, SamplePoint.xyz) - SampleHitPointLen;
			Fade = abs(Delta * rcp(L * L + 1.0f)) < 0.15f;

#ifdef VSLR_SLOW_BREAK
			if(Fade)
#endif
			break;
		}
	}
	
	SamplePoint = normalize(SamplePoint) * sqrt(SampleHitPointLen);
	return float4(SamplePoint, Fade);
}

float4 FastViewReflectionsSSR(float3 Point, float3 Reflect, bool is_hud)
{
	float4 StartProj, EndProj;
	float3 ReflectBase = Reflect;
	
	float Step = rcp(SSLR_STEPS + 1);
	bool Fade = false;

	if(is_hud) {
		StartProj = mul(m_P_hud, float4(Point, 1.0f)); StartProj.xyz /= StartProj.w;
		EndProj = mul(m_P_hud, float4(Point + Reflect * Point.z, 1.0f)); EndProj.xyz /= EndProj.w;
		
		StartProj.z *= 0.02f;
		EndProj.z *= 0.02f;
	} else {
		StartProj = mul(m_P, float4(Point, 1.0f)); StartProj.xyz /= StartProj.w;
		EndProj = mul(m_P, float4(Point + Reflect * Point.z, 1.0f)); EndProj.xyz /= EndProj.w;
	}
	
	Reflect = EndProj.xyz - StartProj.xyz;
	
	StartProj.xy = StartProj.xy * float2(0.5f, -0.5f) + 0.5f;
	Reflect.xy = Reflect.xy * float2(0.5f, -0.5f);
	
	Reflect.xyz = normalize(Reflect.xyz);
	Step *= GetMaxDirLength(StartProj.xyz, rcp(Reflect));
	
	float L = 0.001f;
	
	Step *= is_hud ? 0.2f : 1.0f;
	float StepScale = is_hud ? 1.095f : 1.0f;
	
	[loop]
	for(uint i = 0; i < SSLR_STEPS; ++i)
	{
		float JStep = Step * lerp(0.8f, 1.2f, Hash(dot(sin(EndProj.xyz * timers.x), float3(12.989, 42.364, 78.233))));
		L += JStep;
		
		Step *= StepScale;
		
		EndProj.xyz = StartProj.xyz + Reflect * L;
		
		float HitDepth = s_position.SampleLevel(smp_nofilter, EndProj.xy, 0).x;		
		float Delta = EndProj.z - HitDepth;
		
		if (Delta > 0 && (is_hud || HitDepth > 0.02f))
		{
			float3 JReflect = Reflect * JStep * 0.5f;
			EndProj.xyz -= JReflect;
			
			HitDepth = BinaryRefinementHUD(EndProj.xyz, JReflect);
			Delta = EndProj.z - HitDepth;
			
			Fade = is_hud || abs(Delta) * rcp(max(HitDepth, 0.001f)) < 0.007f;
			EndProj.z = HitDepth;
			
#ifdef SSLR_SLOW_BREAK
			if(Fade)
#endif
			break;
		}
	}
	
	if(is_hud) {
		Fade = Fade && EndProj.z < 0.02f;
	
#ifdef USE_BASE_HUD_REFLECTIONS
		if(!Fade && ReflectBase.z > 0.0f) {
			EndProj = mul(m_P, float4(ReflectBase, 1.0f)); EndProj.xyz /= EndProj.w;
			EndProj.xy = EndProj.xy * float2(0.5f, -0.5f) + 0.5f;
			EndProj.xy = saturate(EndProj.xy);
			
			EndProj.z = s_position.SampleLevel(smp_nofilter, EndProj.xy, 0).x;
			Fade = GetBorderAtten(EndProj.xy, 0.001f) > 0.0f;
			Fade = Fade && EndProj.z > 0.02f && EndProj.z < 1.0f;
			
			EndProj.z *= 0.0002f;
		}
#endif
	} else {
		Fade = Fade && EndProj.z < 1.0f && EndProj.z > 0.02f;
	}
	
	float3 ReflPoint = GbufferGetPointRealUnjitter(EndProj.xy, EndProj.z);
	return float4(ReflPoint, Fade);
}

float4 ScreenSpaceLocalReflections(float3 Point, float3 Reflect)
{
    float2 ReflUV = 0.0;
    float3 HitPos, TestPos;
    float L = 0.025f, DeltaL = 0.0f;

    float Fade = saturate(dot(Reflect, normalize(Point)) * 4.0f);

    if (Fade < 0.001f)
    {
        return 0.0f;
    }

    [unroll(15)]
    for (int i = 0; i < 15; i++)
    {
        TestPos = Point + Reflect * L;
        ReflUV = gbuf_unpack_uv(TestPos);
        HitPos = gbuf_unpack_position(ReflUV);
        if (all(min(min(1.f - ReflUV.x, ReflUV.x), min(1.f - ReflUV.y, ReflUV.y))))
        {
            L = length(Point - HitPos);
        }
        else
        {
            return 0.0f;
        }
    }

    DeltaL = length(HitPos) - length(Point);
    Fade *= step(-0.4f, DeltaL);

    float Attention = GetBorderAtten(ReflUV, 0.125f);
    ReflUV -= s_velocity.SampleLevel(smp_rtlinear, ReflUV, 0).xy * float2(0.5f, -0.5f);
    Fade *= min(Attention, GetBorderAtten(ReflUV, 0.125f));

#ifdef SKYBLED_FADE
    float Fog = saturate(length(HitPos) * fog_params.w + fog_params.x);
    Fade *= 1.f - Fog * Fog;
#endif

    float3 Color = s_image.SampleLevel(smp_rtlinear, ReflUV, 0).xyz;
    return float4(Color, Fade);
}

float3 BiteralReflectionsFiler(float2 TexCoord) {
    float4 Reflections = s_refl.SampleLevel(smp_rtlinear, TexCoord, 0.0f);
	
	float FinalWeight = 2.0f; float Devider = rcp(Reflections.w);
	float3 SpecularIrradance = Reflections.xyz * FinalWeight;

	for(int dx = -1; dx <= 1; ++dx) {
		for(int dy = -1; dy <= 1; ++dy) {
			float2 Offset = TexCoord + scaled_screen_res.zw * (float2(dx, dy) * 2.0f + 0.5f);
			float4 Sample = s_refl.SampleLevel(smp_rtlinear, Offset, 0.0f);
			float SampleWeight = 25.0f * abs(Sample.w - Reflections.w);
			SampleWeight = 1.0f - saturate(SampleWeight * Devider);
			FinalWeight += SampleWeight; SpecularIrradance += SampleWeight * Sample.xyz;
		}
	}

	SpecularIrradance *= rcp(FinalWeight);
	SpecularIrradance *= rcp(1.00001f - SpecularIrradance.xyz);
	
	return SpecularIrradance;
}

float4 calc_reflections(float2 pos2d, float zpos, float3 vreflect)
{
    float3 Point = zpos * float3(pos2d * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);
    return ScreenSpaceLocalReflections(Point, mul((float3x3)m_V, vreflect));
}
#endif

