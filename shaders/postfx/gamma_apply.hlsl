#include "common\common.hlsli"

// Alpha is written as 1: the debug scene target has an alpha channel and is shown through ImGui.
// c_scale.xy is where the view starts in the target, c_scale.zw the view's pixels per pixel drawn: 1 is a plain copy
float4 ps_main(PSInputFullscreen I) : SV_Target
{
    float2 pixel = (I.hpos.xy - c_scale.xy) * c_scale.zw;
    float3 color;

    if (all(c_scale.zw == 1.0f)) {
        color = s_image[pixel].xyz;
    } else {
        float2 size;
        s_image.GetDimensions(size.x, size.y);
        color = s_image.SampleLevel(smp_rtlinear, pixel / size, 0.0f).xyz;
    }

    color = color_params.x * pow(abs(color), color_params.y) + color_params.z;
    color = saturate(color.xyz * color_grading.xyz + (Hash(float3(I.hpos.xy, frac(timers.x) * 1024.0f)) - 0.5f) / 255.0f);

    return float4(color, 1.0f);
}
