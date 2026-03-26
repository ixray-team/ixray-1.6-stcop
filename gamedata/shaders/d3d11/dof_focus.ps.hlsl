#include "common.hlsli"

Texture2D<float> t_focus_prev;
float4 dof_focus_params; // x - manual focus distance, y focus speed, z - autofocus point x, w - autofocus point y 
float autofocus_enabled; // 1 if autofocus is enabled, 0 otherwise

float SampleAutofocusDepthWeighted(float2 uv, float2 pixelSize)
{
    float centerDepth = GbufferGetPointRealUnjitter(uv).z;

    float sum = 0.0f;
    float wsum = 0.0f;

    [unroll]
    for (int y = -1; y <= 1; ++y)
    {
        [unroll]
        for (int x = -1; x <= 1; ++x)
        {
            float2 suv = saturate(uv + float2((float) x, (float) y) * pixelSize);
            float d = GbufferGetPointRealUnjitter(suv).z;

            float spatialW = exp(-0.5f * (x * x + y * y)); 
            float depthW = exp(-6.0f * abs(d - centerDepth)); 
            float w = spatialW * depthW;

            sum += d * w;
            wsum += w;
        }
    }

    return sum / max(wsum, 1e-5f);
}

float4 main(PSInputFullscreen I) : SV_Target
{
    float2 uv = dof_focus_params.zw; // autofocus point
    float2 pixelsize = rcp(pos_decompression_params2.xy);
    // todo add manual focus distance here
    float focus = SampleAutofocusDepthWeighted(uv, pixelsize);
    float lastframefocus = t_focus_prev.Sample(smp_nofilter, 0.5f).x;

    float a = 1.f - exp(-(timers.x - timers.y) / 1.f);
    return lerp(focus, lastframefocus, a);
}