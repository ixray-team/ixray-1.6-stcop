#ifndef reflections_h_2134124_inc
#define reflections_h_2134124_inc

#define SSLR_STEPS 64
#define VSLR_STEPS 20
#define MAX_FIND_STEP 5

struct ReflectionHit
{
    float3 Point;
    float2 UV;
    float Depth;
    float Confidence;
};

float3 ReflectionCapturePoint(float3 Point)
{
    return mul(m_reflectionV, float4(mul(m_invV, float4(Point, 1.0f)), 1.0f));
}

float3 ReflectionViewPoint(float3 Point)
{
    return mul(m_V, float4(mul(m_invReflectionV, float4(Point, 1.0f)), 1.0f));
}

bool ReflectionScreenUV(float2 UV)
{
    return all(isfinite(UV)) && all(UV > 0.0f) && all(UV < 1.0f);
}

float3 ReflectionRayPoint(float3 Q0, float3 Q1, float K0, float K1, float T)
{
    return lerp(Q0, Q1, T) / lerp(K0, K1, T);
}

float4 FastViewReflections(float3 Point, float3 Reflect)
{
    if (reflection_params.y == 0.0f)
        return 0.0f;

    Point = ReflectionCapturePoint(Point);
    Reflect = mul((float3x3)m_reflectionV, mul((float3x3)m_invV, Reflect));
    float Radius = min(fog_params.z, reflection_params.x);
    float B = dot(Point, Reflect);
    float C = dot(Point, Point) - Radius * Radius;
    if (C >= 0.0f || !all(isfinite(Point)) || !all(isfinite(Reflect)))
        return 0.0f;

    float MaxDistance = min(fog_params.z, -B + sqrt(max(0.0f, B * B - C)));
    float Step = MaxDistance * 0.25f / (pow(1.25f, VSLR_STEPS) - 1.0f);
    float L = 0.0f;
    float PreviousDepth = s_env_dist.SampleLevel(smp_nofilter, Point, 0).x;
    bool HasPrevious = PreviousDepth > 0.0f && isfinite(PreviousDepth);
    float PreviousDelta = dot(Point, Point) - PreviousDepth * PreviousDepth;
    float3 NearestPoint = 0.0f;
    float NearestDepth = 0.0f;
    float NearestError = 1e20f;

    [loop]
    for (uint sample_idx = 0; sample_idx < VSLR_STEPS; ++sample_idx)
    {
        float PreviousL = L;
        L += Step;
        Step *= 1.25f;
        float3 SamplePoint = Point + Reflect * L;
        float Depth = s_env_dist.SampleLevel(smp_nofilter, SamplePoint, 0).x;
        if (Depth <= 0.0f || !isfinite(Depth))
        {
            HasPrevious = false;
            continue;
        }

        float SampleLength = length(SamplePoint);
        float3 SurfacePoint = SamplePoint * (Depth / max(SampleLength, EPS));
        float RayDistance = dot(SurfacePoint - Point, Reflect);
        float3 RayOffset = SurfacePoint - Point - Reflect * clamp(RayDistance, 0.0f, MaxDistance);
        float Error = dot(RayOffset, RayOffset) / max(Depth * Depth, EPS);
        if (RayDistance > 0.025f && RayDistance <= MaxDistance && Error < NearestError && all(isfinite(SurfacePoint)))
        {
            NearestPoint = SurfacePoint;
            NearestDepth = Depth;
            NearestError = Error;
        }

        float Delta = dot(SamplePoint, SamplePoint) - Depth * Depth;
        if (HasPrevious && PreviousDelta <= 0.0f && Delta > 0.0f)
        {
            float Low = PreviousL;
            float High = L;
            [unroll]
            for (uint refine_idx = 0; refine_idx < MAX_FIND_STEP; ++refine_idx)
            {
                float Middle = (Low + High) * 0.5f;
                float3 MiddlePoint = Point + Reflect * Middle;
                float MiddleDepth = s_env_dist.SampleLevel(smp_nofilter, MiddlePoint, 0).x;
                if (MiddleDepth > 0.0f && isfinite(MiddleDepth) && length(MiddlePoint) >= MiddleDepth)
                    High = Middle;
                else
                    Low = Middle;
            }
            SamplePoint = Point + Reflect * High;
            Depth = s_env_dist.SampleLevel(smp_nofilter, SamplePoint, 0).x;
            SampleLength = length(SamplePoint);
            float Thickness = max(0.05f, Depth * 0.01f);
            SurfacePoint = SamplePoint * (Depth / max(SampleLength, EPS));
            RayDistance = dot(SurfacePoint - Point, Reflect);
            if (Depth > 0.0f && isfinite(Depth) && abs(SampleLength - Depth) <= Thickness &&
                RayDistance > 0.025f && RayDistance <= MaxDistance && all(isfinite(SurfacePoint)))
                return float4(ReflectionViewPoint(SurfacePoint), 1.0f);
        }
        HasPrevious = true;
        PreviousDelta = Delta;
    }
    float3 FarPoint = Point + Reflect * MaxDistance;
    float FarDepth = s_env_dist.SampleLevel(smp_nofilter, FarPoint, 0).x;
    float FarLength = length(FarPoint);
    if (FarDepth <= 0.0f || !isfinite(FarDepth) || FarLength <= EPS || !isfinite(FarLength))
        return 0.0f;

    float Coverage = 1.0f;
    float3 Direction = FarPoint / FarLength;
    float3 FarSurface = Direction * FarDepth;
    if (NearestDepth == 0.0f && dot(FarSurface - Point, Reflect) <= 0.025f &&
        FarDepth + max(0.05f, FarDepth * 0.01f) < FarLength)
    {
        uint Width, Height;
        s_env_dist.GetDimensions(Width, Height);
        float TexelAngle = 2.0f / max(Width, Height);
        float MaxError = 64.0f * TexelAngle * TexelAngle;
        float3 Tangent = normalize(cross(Direction, abs(Direction.z) < 0.9f ? float3(0.0f, 0.0f, 1.0f) : float3(0.0f, 1.0f, 0.0f)));
        float3 Bitangent = cross(Direction, Tangent);
        [unroll]
        for (uint radius_idx = 1; radius_idx <= 4; radius_idx *= 2)
        {
            [unroll]
            for (int y_idx = -1; y_idx <= 1; ++y_idx)
            {
                [unroll]
                for (int x_idx = -1; x_idx <= 1; ++x_idx)
                {
                    if (x_idx == 0 && y_idx == 0)
                        continue;
                    float3 SampleDirection = normalize(Direction + (Tangent * x_idx + Bitangent * y_idx) * (TexelAngle * radius_idx));
                    float Depth = s_env_dist.SampleLevel(smp_nofilter, SampleDirection, 0).x;
                    if (!isfinite(Depth) || Depth <= FarDepth + max(0.05f, FarDepth * 0.01f))
                        continue;
                    float3 SurfacePoint = SampleDirection * Depth;
                    float RayDistance = dot(SurfacePoint - Point, Reflect);
                    float3 RayOffset = SurfacePoint - Point - Reflect * clamp(RayDistance, 0.0f, MaxDistance);
                    float Error = dot(RayOffset, RayOffset) / max(Depth * Depth, EPS);
                    if (RayDistance <= 0.025f || RayDistance > MaxDistance || Error >= min(NearestError, MaxError) || !all(isfinite(SurfacePoint)))
                        continue;
                    NearestPoint = SurfacePoint;
                    NearestDepth = Depth;
                    NearestError = Error;
                    Coverage = saturate(1.0f - Error / MaxError);
                }
            }
        }
    }
    if (NearestDepth > 0.0f)
    {
        float Confidence = Coverage * (1.0f - saturate(2.5f * NearestDepth * fog_params.w + fog_params.x));
        return float4(ReflectionViewPoint(NearestPoint), Confidence);
    }
    return 0.0f;
}

ReflectionHit TraceScreenReflection(float3 Point, float3 Reflect, bool IsHUD)
{
    ReflectionHit Hit = (ReflectionHit)0;
    float4 NearPoint = mul(IsHUD ? m_invP_hud : m_invP, float4(0.0f, 0.0f, 0.0f, 1.0f));
    float NearZ = NearPoint.z / NearPoint.w * 1.01f;
    float Distance = fog_params.z;
    if (Point.z <= NearZ || !all(isfinite(Point)) || !all(isfinite(Reflect)))
        return Hit;
    if (Reflect.z < -EPS)
        Distance = min(Distance, (NearZ - Point.z) / Reflect.z);
    if (Distance <= EPS)
        return Hit;

    float3 EndPoint = Point + Reflect * Distance;
    float4 StartClip = mul(IsHUD ? m_P_hud : m_P, float4(Point, 1.0f));
    float4 EndClip = mul(IsHUD ? m_P_hud : m_P, float4(EndPoint, 1.0f));
    if (StartClip.w <= EPS || EndClip.w <= EPS)
        return Hit;

    float K0 = rcp(StartClip.w);
    float K1 = rcp(EndClip.w);
    float3 Q0 = Point * K0;
    float3 Q1 = EndPoint * K1;
    float2 Jitter = m_taa_jitter.xy * float2(0.5f, -0.5f);
    float2 StartUV = StartClip.xy * K0 * float2(0.5f, -0.5f) + 0.5f + Jitter;
    float2 EndUV = EndClip.xy * K1 * float2(0.5f, -0.5f) + 0.5f + Jitter;
    if (!ReflectionScreenUV(StartUV))
        return Hit;

    float2 DeltaUV = EndUV - StartUV;
    float EndT = 1.0f;
    [unroll]
    for (uint axis_idx = 0; axis_idx < 2; ++axis_idx)
    {
        if (DeltaUV[axis_idx] > EPS)
            EndT = min(EndT, (1.0f - EPS - StartUV[axis_idx]) / DeltaUV[axis_idx]);
        else if (DeltaUV[axis_idx] < -EPS)
            EndT = min(EndT, (EPS - StartUV[axis_idx]) / DeltaUV[axis_idx]);
    }
    if (EndT <= EPS)
        return Hit;
    float PixelLength = max(abs(DeltaUV.x * pos_decompression_params2.x), abs(DeltaUV.y * pos_decompression_params2.y)) * EndT;
    uint Steps = min((uint)SSLR_STEPS, max(1u, (uint)ceil(PixelLength)));
    float PreviousT = 0.0f;
    float3 PreviousPoint = Point;

    [loop]
    for (uint sample_idx = 0; sample_idx < Steps; ++sample_idx)
    {
        float T = EndT * (sample_idx + 1.0f) / Steps;
        float2 UV = StartUV + DeltaUV * T;
        float3 RayPoint = ReflectionRayPoint(Q0, Q1, K0, K1, T);
        float LowT = PreviousT;
        float LowZ = min(PreviousPoint.z, RayPoint.z);
        float HighZ = max(PreviousPoint.z, RayPoint.z);
        PreviousT = T;
        PreviousPoint = RayPoint;
        if (!ReflectionScreenUV(UV))
            break;

#ifdef USE_SSLR_DEPTH_MIN
        uint2 Pixel = min(uint2(UV * pos_decompression_params2.xy), uint2(pos_decompression_params2.xy) - 1u);
        float MinDepth = s_sslr_depth_min.Load(int3(Pixel / 8u, 0));
        float4 HighClip = mul(IsHUD ? m_P_hud : m_P, float4(0.0f, 0.0f, HighZ, 1.0f));
        float HighDepth = HighClip.z / HighClip.w * (IsHUD ? 0.02f : 1.0f);
        if (HighDepth < MinDepth)
            continue;
#endif
        float Depth = s_position.SampleLevel(smp_nofilter, UV, 0).x;
        if (Depth >= 1.0f || (Depth < 0.02f) != IsHUD)
            continue;
        float SceneZ = GbufferGetPointRealJitter(UV, Depth).z;
        float Thickness = max(IsHUD ? 0.005f : 0.05f, SceneZ * 0.01f);
        if (SceneZ < LowZ - Thickness || SceneZ > HighZ + Thickness)
            continue;

        float HighT = T;
        [unroll]
        for (uint refine_idx = 0; refine_idx < MAX_FIND_STEP; ++refine_idx)
        {
            float MiddleT = (LowT + HighT) * 0.5f;
            float2 MiddleUV = StartUV + DeltaUV * MiddleT;
            float MiddleDepth = s_position.SampleLevel(smp_nofilter, MiddleUV, 0).x;
            float3 MiddlePoint = ReflectionRayPoint(Q0, Q1, K0, K1, MiddleT);
            bool IsScene = MiddleDepth < 1.0f && (MiddleDepth < 0.02f) == IsHUD;
            float SceneDepth = IsScene ? GbufferGetPointRealJitter(MiddleUV, MiddleDepth).z : 0.0f;
            if (IsScene && ((MiddlePoint.z >= SceneDepth) == (Reflect.z >= 0.0f)))
                HighT = MiddleT;
            else
                LowT = MiddleT;
        }
        UV = StartUV + DeltaUV * HighT;
        Depth = s_position.SampleLevel(smp_nofilter, UV, 0).x;
        if (Depth >= 1.0f || (Depth < 0.02f) != IsHUD)
            continue;
        float3 ScenePoint = GbufferGetPointRealJitter(UV, Depth);
        RayPoint = ReflectionRayPoint(Q0, Q1, K0, K1, HighT);
        float Error = abs(ScenePoint.z - RayPoint.z);
        Thickness = max(IsHUD ? 0.005f : 0.05f, ScenePoint.z * 0.01f);
        if (Error > Thickness || length(ScenePoint - Point) < (IsHUD ? 0.005f : 0.025f) || !all(isfinite(ScenePoint)))
            continue;

        Hit.Point = ScenePoint;
        Hit.UV = UV;
        Hit.Depth = Depth;
        Hit.Confidence = GetBorderAtten(UV, 0.025f) * saturate((Thickness - Error) / (Thickness * 0.25f));
        return Hit;
    }
    return Hit;
}

ReflectionHit FastViewReflectionsSSR(float3 Point, float3 Reflect, bool IsHUD)
{
    ReflectionHit Hit = TraceScreenReflection(Point, Reflect, IsHUD);
    if (!IsHUD || Hit.Confidence > 0.0f || Reflect.z <= EPS)
        return Hit;

    float4 Clip = mul(m_P, float4(Reflect, 0.0f));
    float2 UV = Clip.xy / Clip.w * float2(0.5f, -0.5f) + 0.5f;
    UV += m_taa_jitter.xy * float2(0.5f, -0.5f);
    if (!ReflectionScreenUV(UV))
        return Hit;
    float Depth = s_position.SampleLevel(smp_nofilter, UV, 0).x;
    if (Depth < 0.02f || Depth >= 1.0f)
        return Hit;
    Hit.Point = GbufferGetPointRealJitter(UV, Depth);
    Hit.UV = UV;
    Hit.Depth = Depth;
    Hit.Confidence = GetBorderAtten(UV, 0.025f);
    return Hit;
}

float4 ScreenSpaceLocalReflections(float3 Point, float3 Reflect)
{
    ReflectionHit Hit = FastViewReflectionsSSR(Point, Reflect, false);
    if (Hit.Confidence <= 0.0f || !ReflectionScreenUV(Hit.UV))
        return 0.0f;
    float Fog = saturate(length(Hit.Point) * fog_params.w + fog_params.x);
    float Confidence = Hit.Confidence * (1.0f - Fog * Fog);
    float3 Color = s_image.SampleLevel(smp_rtlinear, Hit.UV, 0).xyz;
    return all(isfinite(Color)) ? float4(Color, Confidence) : 0.0f;
}
#endif
