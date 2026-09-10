#include "common_clouds.hlsli"

Texture2D<float4> s_cloud_shadow_map;
RWTexture2D<float4> u_cloud_shadow_map : register(u0);

// Blur optical depth only. The center ray interval remains authoritative, so
// neighbouring light rays cannot smear a different cloud layer into this ray.
[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint width, height;
    u_cloud_shadow_map.GetDimensions(width, height);
    const uint2 pixel = dispatch_id.xy;
    if (any(pixel >= uint2(width, height)))
        return;

    const float4 center = s_cloud_shadow_map.Load(int3(pixel, 0));
    if (center.b <= center.g)
    {
        u_cloud_shadow_map[pixel] = float4(0.0f, 0.0f, 0.0f, 1.0f);
        return;
    }

    const int2 offsets[5] = { int2(0, 0), int2(-1, 0), int2(1, 0), int2(0, -1), int2(0, 1) };
    const float weights[5] = { 0.5f, 0.125f, 0.125f, 0.125f, 0.125f };
    float filtered_tau = 0.0f;
    float total_weight = 0.0f;
    [unroll]
    for (uint tap = 0u; tap < 5u; ++tap)
    {
        const int2 sample_pixel = clamp(int2(pixel) + offsets[tap], int2(0, 0), int2(width, height) - 1);
        const float4 sample_value = s_cloud_shadow_map.Load(int3(sample_pixel, 0));
        const float overlap = min(center.b, sample_value.b) - max(center.g, sample_value.g);
        const float weight = overlap > 1e-5f ? weights[tap] : 0.0f;
        filtered_tau += max(sample_value.r, 0.0f) * weight;
        total_weight += weight;
    }
    const float tau = filtered_tau / max(total_weight, 1e-6f);
    u_cloud_shadow_map[pixel] = float4(tau, center.g, center.b, exp(-tau));
}
