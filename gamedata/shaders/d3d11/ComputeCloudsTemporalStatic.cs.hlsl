#include "common_clouds.hlsli"

Texture2D<float4> s_cloud_current : register(t8);
RWTexture2D<float4> u_cloud_history : register(u0);

// When the view is unchanged, all unsampled pixels already contain the exact
// history needed by the sky pass. Update only this frame's measured pixels.
[numthreads(8, 4, 1)]
void main(uint3 dispatch_id : SV_DispatchThreadID)
{
    uint raw_width, raw_height;
    s_cloud_current.GetDimensions(raw_width, raw_height);
    if (dispatch_id.x >= raw_width || dispatch_id.y >= raw_height)
        return;

    uint2 pixel = cloud_trace_pixel(dispatch_id.xy);
    if (any(pixel >= uint2(cloud_screen_params.xy)))
        return;

    float4 result = s_cloud_current.Load(int3(dispatch_id.xy, 0));
    if (!all(isfinite(result)))
        result = float4(0.0f, 0.0f, 0.0f, 1.0f);
    u_cloud_history[pixel] = float4(max(result.rgb, 0.0f), saturate(result.a));
}
