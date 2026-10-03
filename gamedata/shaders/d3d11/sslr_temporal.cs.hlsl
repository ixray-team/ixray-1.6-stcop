#include "common.hlsli"
#include "reflections.hlsli"

Texture2D<float4> s_refl_surface;
Texture2D<float4> s_refl_data;
RWTexture2D<float4> u_sslr : register(u0);
RWTexture2D<float4> u_sslr_history : register(u1);
RWTexture2D<float4> u_sslr_surface : register(u2);

float3 HistoryClamp(float3 History, float3 Minimum, float3 Maximum)
{
    float3 Center = (Maximum + Minimum) * 0.5f;
    float3 Extent = max((Maximum - Minimum) * 0.5f, EPS_L);
    float3 Delta = History - Center;
    float3 Ratio = abs(Delta / Extent);
    float Scale = max(1.0f, max(Ratio.x, max(Ratio.y, Ratio.z)));
    return Center + Delta / Scale;
}

float4 ReflectionHistory(float2 UV, float4 Surface, float3 WorldNormal, float4 Plane, bool IsHUD, float HUDDepth)
{
    float2 Size = pos_decompression_params2.xy;
    float2 Position = UV * Size - 0.5f;
    int2 Base = int2(floor(Position));
    float2 Fraction = frac(Position);
    float4 History = 0.0f;
    [unroll]
    for (int y_idx = 0; y_idx < 2; ++y_idx)
    {
        [unroll]
        for (int x_idx = 0; x_idx < 2; ++x_idx)
        {
            int2 Pixel = Base + int2(x_idx, y_idx);
            if (any(Pixel < 0) || any(Pixel >= int2(Size)))
                continue;
            float4 Old = s_refl.Load(int3(Pixel, 0));
            if (Old.w == 0.0f || (Old.w < 0.0f) != IsHUD || !all(isfinite(Old)))
                continue;

            float ExpectedDepth = HUDDepth;
            if (!IsHUD)
            {
                if (abs(Plane.z) <= EPS_S || !all(isfinite(Plane)))
                    continue;
                float2 TapUV = (Pixel + 0.5f) / Size - reflection_history_jitter.xy * float2(0.5f, -0.5f);
                float2 ClipXY = (TapUV - 0.5f) * float2(2.0f, -2.0f);
                float Depth = -dot(Plane.xyw, float3(ClipXY, 1.0f)) / Plane.z;
                float4 World = mul(m_invVP_old, float4(ClipXY, Depth, 1.0f));
                if (Depth < 0.0f || Depth >= 1.0f || World.w <= EPS_S || !all(isfinite(World)))
                    continue;
                ExpectedDepth = rcp(World.w);
            }
            float Thickness = max(IsHUD ? 0.005f : 0.05f, ExpectedDepth * 0.01f);
            if (abs(abs(Old.w) - ExpectedDepth) >= Thickness)
                continue;

            float4 OldSurface = s_refl_surface.Load(int3(Pixel, 0));
            float NormalFactor = saturate((dot(WorldNormal, NormalDecode(OldSurface.xy * 2.0f - 1.0f)) - 0.9f) * 10.0f);
            float MaterialFactor = saturate(1.0f - 10.0f * max(abs(Surface.z - OldSurface.z), abs(Surface.w - OldSurface.w)));
            float2 TapWeight = lerp(1.0f - Fraction, Fraction, float2(x_idx, y_idx));
            float Weight = TapWeight.x * TapWeight.y * NormalFactor * MaterialFactor;
            History += float4(Old.xyz, 1.0f) * Weight;
        }
    }
    return History;
}

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
    uint Width, Height;
    u_sslr.GetDimensions(Width, Height);
    if (any(DTid >= uint2(Width, Height)))
        return;

    IXRayGbuffer O = (IXRayGbuffer)0;
    GbufferUnpack(DTid, O);
    float2 UV = (DTid + 0.5f) * pos_decompression_params2.zw;
    float3 Current = s_image.Load(int3(DTid, 0)).xyz;
    float4 Surface = 0.0f;
    float HistoryDepth = 0.0f;

    if (O.Depth < 1.0f)
    {
        bool IsHUD = O.Depth < 0.02f;
        float3 Point = GbufferGetPointRealJitter(UV, O.Depth);
        float3 WorldPoint = mul(m_invV, float4(Point, 1.0f));
        float3 WorldNormal = normalize(mul((float3x3)m_invV, O.Normal));
        Surface.xy = NormalEncode(WorldNormal) * 0.5f + 0.5f;
#ifndef USE_LEGACY_LIGHT
        float Roughness = O.Roughness;
#else
        float Roughness = 1.0f - O.Gloss;
#endif
        Surface.z = Roughness;
        Surface.w = s_surface.Load(int3(DTid, 0)).x;
        HistoryDepth = Point.z * (IsHUD ? -1.0f : 1.0f);

        float3 Minimum = Current;
        float3 Maximum = Current;
        [unroll]
        for (int y_idx = -1; y_idx <= 1; ++y_idx)
        {
            [unroll]
            for (int x_idx = -1; x_idx <= 1; ++x_idx)
            {
                int2 Pixel = clamp(int2(DTid) + int2(x_idx, y_idx), int2(0, 0), int2(Width, Height) - 1);
                float Depth = s_position.Load(int3(Pixel, 0)).x;
                float3 Color = s_image.Load(int3(Pixel, 0)).xyz;
                if (Depth < 1.0f && (Depth < 0.02f) == IsHUD && all(isfinite(Color)))
                {
                    Minimum = min(Minimum, Color);
                    Maximum = max(Maximum, Color);
                }
            }
        }

        float4 PreviousClip = mul(m_VP_old, float4(WorldPoint, 1.0f));
        float2 PreviousUV = 0.0f;
        bool CanReproject = reflection_history_jitter.z > 0.0f && PreviousClip.w > EPS && all(isfinite(PreviousClip));
#ifndef DISABLE_MOTION_VECTORS
        PreviousUV = UV - m_taa_jitter.xy * float2(0.5f, -0.5f);
        PreviousUV += s_velocity.Load(int3(DTid, 0)).xy * float2(-0.5f, 0.5f);
#else
        CanReproject = CanReproject && !IsHUD;
        if (CanReproject)
            PreviousUV = PreviousClip.xy / PreviousClip.w * float2(0.5f, -0.5f) + 0.5f;
#endif
        if (!IsHUD && Roughness <= 0.1f)
        {
            float4 Hit = s_refl_data.Load(int3(DTid, 0));
            if (Hit.w != 0.0f && all(isfinite(Hit)) && length(Hit.xyz - Point) < fog_params.z * 0.99f)
            {
                float3 VirtualPoint = Hit.xyz - 2.0f * O.Normal * dot(Hit.xyz - Point, O.Normal);
                float4 VirtualClip = mul(m_VP_old, float4(mul(m_invV, float4(VirtualPoint, 1.0f)), 1.0f));
                CanReproject = CanReproject && VirtualClip.w > EPS && all(isfinite(VirtualClip));
                if (CanReproject)
                    PreviousUV = VirtualClip.xy / VirtualClip.w * float2(0.5f, -0.5f) + 0.5f;
            }
        }
        PreviousUV += reflection_history_jitter.xy * float2(0.5f, -0.5f);
        CanReproject = CanReproject && ReflectionScreenUV(PreviousUV);
        if (CanReproject)
        {
            float4 Plane = mul(float4(WorldNormal, -dot(WorldNormal, WorldPoint)), m_invVP_old);
            float4 History = ReflectionHistory(PreviousUV, Surface, WorldNormal, Plane, IsHUD, Point.z);
            if (History.w > EPS_S)
            {
                History.xyz /= History.w;
                float3 Difference = abs(History.xyz - Current) / max(max(History.xyz, Current), 0.05f);
                float Change = max(Difference.x, max(Difference.y, Difference.z));
                float2 Motion = PreviousUV - UV - (reflection_history_jitter.xy - m_taa_jitter.xy) * float2(0.5f, -0.5f);
                float Weight = lerp(0.85f, 0.95f, Roughness) * History.w * saturate(1.0f - Change) * exp2(-length(Motion * pos_decompression_params2.xy) * 0.1f);
                Current = lerp(Current, HistoryClamp(History.xyz, Minimum, Maximum), Weight);
            }
        }
    }

    float4 Result = float4(Current, HistoryDepth);
    u_sslr[DTid] = Result;
    u_sslr_history[DTid] = Result;
    u_sslr_surface[DTid] = Surface;
}
