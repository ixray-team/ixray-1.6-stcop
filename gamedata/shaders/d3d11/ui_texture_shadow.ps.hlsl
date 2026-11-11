#include "common.hlsli"

uniform float4 ui_shadow_rect;
uniform float4 ui_shadow_blur;
uniform float4 ui_shadow_range;
uniform float4 ui_shadow_light;
uniform float4 ui_shadow_basis;

float ShadowAlpha(float2 UV)
{
    uint Width, Height;
    s_base.GetDimensions(Width, Height);
    float2 Size = float2(Width, Height);
    float2 HalfTexel = 0.5f / Size;
    // Transparent, bilinearly filtered atlas border; never sample neighboring entries.
    float2 NearEdge = saturate((UV - ui_shadow_rect.xy) * Size + 0.5f);
    float2 FarEdge = saturate((ui_shadow_rect.zw - UV) * Size + 0.5f);
    float BorderCoverage = NearEdge.x * NearEdge.y * FarEdge.x * FarEdge.y;
    float2 SampleUV = clamp(UV, ui_shadow_rect.xy + HalfTexel, ui_shadow_rect.zw - HalfTexel);
    return s_base.SampleLevel(smp_base, SampleUV, 0).a * BorderCoverage;
}

float4 main(v2p_TL I) : SV_Target
{
    float2x2 KernelMap = float2x2(ui_shadow_blur.x, 0.0f, 0.0f, ui_shadow_blur.y);
    if (ui_shadow_light.w > 0.0f)
    {
        // Finite emitter covariance: tangential spread is constant, radial spread
        // increases with the ray's grazing angle. Evaluate it at every texel.
        float2x2 Basis = float2x2(ui_shadow_basis.xy, ui_shadow_basis.zw);
        float Determinant = ui_shadow_basis.x * ui_shadow_basis.w - ui_shadow_basis.y * ui_shadow_basis.z;
        float2x2 InverseBasis = float2x2(ui_shadow_basis.w, -ui_shadow_basis.y,
            -ui_shadow_basis.z, ui_shadow_basis.x) / Determinant;
        float2 Ray = mul(Basis, I.Tex0 - ui_shadow_light.xy);
        float RayLengthSquared = dot(Ray, Ray);
        float2 Direction = Ray * rsqrt(max(RayLengthSquared, 1e-8f));
        float Stretch = min(sqrt(1.0f + RayLengthSquared / (ui_shadow_light.z * ui_shadow_light.z)), ui_shadow_light.w);
        float Extra = Stretch - 1.0f;
        float2x2 Shape = float2x2(1.0f + Extra * Direction.x * Direction.x, Extra * Direction.x * Direction.y,
            Extra * Direction.x * Direction.y, 1.0f + Extra * Direction.y * Direction.y);
        KernelMap = mul(InverseBasis, Shape) * ui_shadow_blur.x;
    }
    float Alpha = ShadowAlpha(I.Tex0);
    if (ui_shadow_blur.z > 0.0f && any(ui_shadow_blur.xy > 0.0f))
    {
        float Sigma = max(ui_shadow_blur.z, 0.001f);
        float Coverage = 0.0f;
        float WeightSum = 0.0f;
        // Dense sampling prevents visible repeated silhouettes at UI scale.
        [unroll] for (int Y = -8; Y <= 8; ++Y)
        {
            [unroll] for (int X = -8; X <= 8; ++X)
            {
                float2 Offset = float2(X, Y) * 0.125f;
                // Both the kernel value and slope vanish at its outer boundary.
                float2 Taper = 1.0f - smoothstep(0.75f, 1.0f, abs(Offset));
                float Weight = exp(-dot(Offset, Offset) / (2.0f * Sigma * Sigma)) * Taper.x * Taper.y;
                Coverage += ShadowAlpha(I.Tex0 + mul(KernelMap, Offset)) * Weight;
                WeightSum += Weight;
            }
        }
        Alpha = Coverage / WeightSum;
    }
    // Do not sharpen or renormalize the Gaussian edge after convolution.
    // Maximum is the source shadow opacity; minimum marks the faint outer tail.
    float CoverageAlpha = saturate(Alpha);
    float MinimumRatio = saturate(ui_shadow_range.x);
    float TailFade = MinimumRatio > 0.0f ? smoothstep(0.0f, MinimumRatio, CoverageAlpha) : 1.0f;
    float TailPower = max(ui_shadow_blur.w / 3.0f, 1.0f);
    float RangedAlpha = CoverageAlpha * pow(TailFade, TailPower);
    return float4(I.Color.rgb, saturate(RangedAlpha * I.Color.a));
}
