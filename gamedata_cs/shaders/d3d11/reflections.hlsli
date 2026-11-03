#ifndef reflections_h_2134124_inc
#define reflections_h_2134124_inc

// Screen Space Sky Reflections off
#define SKYBLED_FADE
#define USE_BASE_HUD_REFLECTIONS

#define USE_VASYAN_CUTOFF

// #define VSLR_SLOW_BREAK	
// #define SSLR_SLOW_BREAK

float3 gbuf_unpack_position(float2 uv)
{
	float4 Point = float4
	(
		uv,	s_position.SampleLevel(smp_rtlinear, uv, 0).x, 1.0f
	);
	
	Point.x = Point.x * 2.0f - 1.0f;
	Point.y = 1.0f - Point.y * 2.0f;
	
	Point = mul(m_invP, Point);
    return Point.xyz / Point.w;
}

float2 gbuf_unpack_uv(float3 position)
{
	float4 Point = mul(m_P, float4(position, 1.0f));
	Point.xy = Point.xy * float2(0.5f, -0.5f) * rcp(Point.w) + 0.5f;
	
    return saturate(Point.xy);
}

#define SSLR_STEPS 20
#define MAX_FIND_STEP 4

float BinaryRefinement(inout float3 EndProj, float3 Reflect)
{
	float HitDepth = 0.0f;
	
	[unroll]
	for(int i = 0; i < MAX_FIND_STEP; ++i)
	{
		HitDepth = s_env_dist.SampleLevel(smp_nofilter, EndProj.xyz, 0).x;
		HitDepth *= HitDepth;
		
		Reflect *= 0.5f;
		EndProj += dot(EndProj, EndProj) > HitDepth ? -Reflect : Reflect;
	}
	
	HitDepth = s_env_dist.SampleLevel(smp_nofilter, EndProj.xyz, 0).x;
	HitDepth *= HitDepth;
	
	return HitDepth;
}

float BinaryRefinementHUD(inout float3 EndProj, float3 Reflect)
{
	float HitDepth = 0.0f;
	
	[unroll]
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
	float RadiusS = fog_params.z * fog_params.z;
	float DistanceS = dot(Point, Point);
	
	if(DistanceS >= RadiusS) 
	{
		return float4(Reflect, 0.0f);
	}
	
	float3 SamplePoint = Reflect;
	
	float Step = rcp(SSLR_STEPS + 1) * 0.02f;
	float L = 0.011f;
	
	Step *= fog_params.z - sqrt(DistanceS);
	
	bool Fade = false;
	
	float Delta = 0.0f;
	float OldDelta = 0.0f;
	float SampleHitPointLen = 0;
	
	float MaxLen = 0; //s_env_dist.SampleLevel(smp_nofilter, Reflect.xyz, 0).x; MaxLen *= MaxLen;

	[loop]
	for(uint i = 0; i < SSLR_STEPS; ++i)
	{
		float JStep = Step * lerp(0.8f, 1.2f, Hash(dot(sin(SamplePoint.xyz * timers.x), float3(12.989, 42.364, 78.233))));
		L += JStep;
		
		Step *= 1.342264f;
		
		SamplePoint.xyz = Point.xyz + Reflect * L;
		
		SampleHitPointLen = s_env_dist.SampleLevel(smp_nofilter, SamplePoint.xyz, 0).x;
		SampleHitPointLen *= SampleHitPointLen;
		
		MaxLen = max(MaxLen, SampleHitPointLen);
		Delta = dot(SamplePoint, SamplePoint) - SampleHitPointLen;
		
		
		if (Delta > 0 && OldDelta <= 0)
		{
			float3 JReflect = Reflect * JStep * 0.5f;
			SamplePoint.xyz -= JReflect;
			
			SampleHitPointLen = BinaryRefinement(SamplePoint.xyz, JReflect);
			MaxLen = max(MaxLen, SampleHitPointLen);
		
			Delta = dot(SamplePoint.xyz, SamplePoint.xyz) - SampleHitPointLen;
			Fade = abs(Delta) / max(dot(SamplePoint.xyz, SamplePoint.xyz), SampleHitPointLen) < 0.1f;

#ifdef VSLR_SLOW_BREAK
			if(Fade)
#endif
			break;
		}
		
		OldDelta = Delta;
	}
	
	SamplePoint = normalize(SamplePoint) * sqrt(SampleHitPointLen);
	MaxLen = Fade ? 1.0f : 1.0f - saturate(2.5f * sqrt(MaxLen) * fog_params.w + fog_params.x);
	
	return float4(SamplePoint, MaxLen);
}

float4 FastViewReflectionsSSR(float3 Point, float3 Reflect, bool is_hud)
{
	float4 StartProj, EndProj;
	float3 ReflectBase = Reflect;
	
	float Step = rcp(SSLR_STEPS + 1);
	bool Fade = false;

	if(is_hud) 
	{
		StartProj = mul(m_P_hud, float4(Point, 1.0f)); StartProj.xyz /= StartProj.w;
		EndProj = mul(m_P_hud, float4(Point + Reflect * Point.z, 1.0f)); EndProj.xyz /= EndProj.w;
		
		StartProj.z *= 0.02f;
		EndProj.z *= 0.02f;
	} 
	else 
	{
		StartProj = mul(m_P, float4(Point, 1.0f)); StartProj.xyz /= StartProj.w;
		EndProj = mul(m_P, float4(Point + Reflect * Point.z, 1.0f)); EndProj.xyz /= EndProj.w;
	}
	
	Reflect = EndProj.xyz - StartProj.xyz;
	
	StartProj.xy = StartProj.xy * float2(0.5f, -0.5f) + 0.5f;
	Reflect.xy = Reflect.xy * float2(0.5f, -0.5f);
	
	Reflect.xyz = normalize(Reflect.xyz);
	Step *= GetMaxDirLength(StartProj.xyz, rcp(Reflect));
	
	float L = 0.001f;
	
	Step *= 0.1f;
	float StepScale = 1.21f;
	
	float Delta = 0.0f;
	float OldDelta = 0.0f;
	
	[loop]
	for(uint i = 0; i < SSLR_STEPS; ++i)
	{
		float JStep = Step * lerp(0.8f, 1.2f, Hash(dot(sin(EndProj.xyz * timers.x), float3(12.989, 42.364, 78.233))));
		L += JStep;
		
		Step *= StepScale;
		
		EndProj.xyz = StartProj.xyz + Reflect * L;
		
		float HitDepth = s_position.SampleLevel(smp_nofilter, EndProj.xy, 0).x;		
		Delta = EndProj.z - HitDepth;
		
		if(!GetBorderAtten(EndProj.xy))
		{
			return 0.0f;
		}
		
		if (Delta > 0 && OldDelta <= 0 && (is_hud || HitDepth > 0.02f))
		{
			float3 JReflect = Reflect * JStep * 0.5f;
			EndProj.xyz -= JReflect;
			
			HitDepth = BinaryRefinementHUD(EndProj.xyz, JReflect);
			
		 	Fade = is_hud || abs(HitDepth - EndProj.z) * rcp(max(EndProj.z, HitDepth)) < 0.01f;
			EndProj.z = HitDepth;
			
#ifdef SSLR_SLOW_BREAK
			if(Fade)
#endif
			break;
		}
#ifdef USE_OFFSCREEN_REFLECTIONS
		else if(!is_hud && HitDepth < 0.02f)
		{
			return 0.0f;
		}
#endif
		OldDelta = Delta;
	}
	
	if(is_hud)
	{
		Fade = Fade && EndProj.z < 0.02f;
	
#ifdef USE_BASE_HUD_REFLECTIONS
		if(!Fade && ReflectBase.z > 0.0f) 
		{
			EndProj = mul(m_P, float4(ReflectBase, 1.0f)); EndProj.xyz /= EndProj.w;
			EndProj.xy = EndProj.xy * float2(0.5f, -0.5f) + 0.5f;
			EndProj.xy = saturate(EndProj.xy);
			
			EndProj.z = s_position.SampleLevel(smp_nofilter, EndProj.xy, 0).x;
			Fade = GetBorderAtten(EndProj.xy) && EndProj.z > 0.02f && EndProj.z < 1.0f;
		}
#endif
	}
	else
	{
		Fade = Fade && EndProj.z < 1.0f && EndProj.z > 0.02f;
	}
	
	float3 ReflPoint = GbufferGetPointRealUnjitter(EndProj.xy, EndProj.z);
	return float4(ReflPoint, Fade);
}

// Stochastic Screen-Space Reflections, Tomasz Stachowiak (SIGGRAPH 2015).
// Hierarchical trace over the min-Z pyramid: the ray walks cell by cell in screen space
// (uv, device z), goes one level coarser after an empty cell and one level finer
// when it dips below the cell minimum. A hit is reported once it is below level 0.
Texture2D<float> s_hiz;

#define HIZ_MAX_ITERATIONS 64
#define HIZ_RAY_EPS 0.005f
#define HIZ_THICKNESS 0.5f
#define HIZ_THICKNESS_SCALE 0.05f

float HiZViewDepth(float Depth)
{
	return m_P._34 / (Depth - m_P._33);
}

float4 HiZTraceSSR(float3 Point, float3 Reflect)
{
	// Keep the end point in front of the near plane
	float Near = -m_P._34 / m_P._33;
	float RayLength = Reflect.z < 0.0f ? min(fog_params.z, (Point.z - Near) * 0.99f / -Reflect.z) : fog_params.z;

	float4 StartProj = mul(m_P, float4(Point, 1.0f));
	float4 EndProj = mul(m_P, float4(Point + Reflect * RayLength, 1.0f));

	float3 Origin = StartProj.xyz * rcp(StartProj.w);
	float3 End = EndProj.xyz * rcp(EndProj.w);

	Origin.xy = Origin.xy * float2(0.5f, -0.5f) + 0.5f;
	End.xy = End.xy * float2(0.5f, -0.5f) + 0.5f;

	// Keep the sign of tiny components, the cell boundary choice depends on it
	float3 Dir = End - Origin;
	Dir = abs(Dir) > 1e-7f ? Dir : (Dir >= 0.0f ? 1e-7f : -1e-7f);

	float3 InvDir = rcp(Dir);

	// Ray leaves the screen or the depth range
	float3 TExit = ((Dir > 0.0f ? 1.0f : 0.0f) - Origin) * InvDir;
	float TMax = min(1.0f, min(TExit.x, min(TExit.y, TExit.z)));

	uint2 HizSize;
	uint HizLevels;
	s_hiz.GetDimensions(0, HizSize.x, HizSize.y, HizLevels);

	int MaxLevel = int(HizLevels) - 1;

	float2 CrossStep = Dir.xy >= 0.0f ? 1.0f : 0.0f;
	float2 CrossOffset = (Dir.xy >= 0.0f ? 1.0f : -1.0f) * HIZ_RAY_EPS / float2(HizSize);

	// Leave the origin pixel first, so the ray does not hit its own surface
	float2 Plane = (floor(Origin.xy * float2(HizSize)) + CrossStep) / float2(HizSize) + CrossOffset;
	float2 TPlane = (Plane - Origin.xy) * InvDir.xy;

	float T = min(TPlane.x, TPlane.y);
	float3 Ray = Origin + Dir * T;

	int Level = 0;
	uint Iteration = 0;

	[loop]
	while (Level >= 0 && Iteration < HIZ_MAX_ITERATIONS && T < TMax)
	{
		float2 CellCount = float2(max(HizSize >> uint(Level), 1u));
		float2 Cell = floor(Ray.xy * CellCount);

		float SurfaceZ = s_hiz.Load(int3(Cell, Level)).x;

		Plane = (Cell + CrossStep) / CellCount + CrossOffset;
		TPlane = (Plane - Origin.xy) * InvDir.xy;

		float TCell = min(TPlane.x, TPlane.y);
		float TSurface = Dir.z > 0.0f ? (SurfaceZ - Origin.z) * InvDir.z : 1e20f;

		bool Above = Ray.z < SurfaceZ;
		bool Skip = Above && TCell < TSurface;

		T = Above ? min(TCell, TSurface) : T;
		Ray = Origin + Dir * T;

		Level = min(Level + (Skip ? 1 : -1), MaxLevel);
		++Iteration;
	}

	bool Hit = Level < 0 && T < TMax && GetBorderAtten(Ray.xy);

	float HitDepth = s_position.Load(int3(Ray.xy * pos_decompression_params2.xy, 0)).x;
	Hit = Hit && HitDepth > 0.02f && HitDepth < 1.0f;

	// Depth buffer has no thickness: reject rays which went too far behind the surface
	float SurfaceViewZ = HiZViewDepth(HitDepth);
	float Thickness = max(HIZ_THICKNESS, SurfaceViewZ * HIZ_THICKNESS_SCALE);
	Hit = Hit && HiZViewDepth(Ray.z) - SurfaceViewZ < Thickness;

	float3 HitPoint = GbufferGetPointRealUnjitter(Ray.xy, HitDepth);
	return float4(HitPoint, Hit ? 1.0f : 0.0f);
}

float4 ScreenSpaceLocalReflections(float3 Point, float3 Reflect)
{
#if 1 //ndef USE_OFFSCREEN_REFLECTIONS
    float2 ReflUV = 0.0;
    float3 HitPos, TestPos;
    float L = 0.025f, DeltaL = 0.0f;
	
    float Fade = saturate(dot(Reflect, normalize(Point)) * 4.0f);
	
    if (Fade < 0.001f)
    {
       return 0.0f;
    }
	
    [loop]
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
#else
	float4 HitPos = FastViewReflectionsSSR(Point, Reflect, false);
	float2 ReflUV = gbuf_unpack_uv(HitPos.xyz);
	
	float Fade = HitPos.w;
#endif

    float Attention = GetBorderAtten(ReflUV, 0.125f);

	float4 PrevSpecularUV = mul(m_VP_old, float4(mul(m_invV, float4(HitPos.xyz, 1.0f)).xyz, 1.0f));
	ReflUV.xy = PrevSpecularUV.xy / PrevSpecularUV.w * float2(0.5f, -0.5f) + 0.5f;
	
#ifndef USE_OFFSCREEN_REFLECTIONS
    Fade *= min(Attention, GetBorderAtten(ReflUV, 0.125f));
#else
    Fade *= min(Attention, GetBorderAtten(ReflUV, 0.025f));
#endif
	
#ifdef SKYBLED_FADE
    float Fog = saturate(length(HitPos.xyz) * fog_params.w + fog_params.x);
    Fade *= 1.f - Fog * Fog;
#endif

    float3 Color = s_image.SampleLevel(smp_rtlinear, ReflUV, 0).xyz;
	Color = clamp(Color, 0.0f, 20.0f);
	
    return float4(Color, Fade);
}
#endif

