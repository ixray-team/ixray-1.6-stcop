#include "common.hlsli"
Texture2D s_vollight;

uniform float4 scaled_screen_res;

float4 main(PSInputFullscreen I) : SV_Target
{
    float4 Color = 0;
    
    Color += s_vollight.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(-0.5f, -0.5f) * scaled_screen_res.zw, 0);
    Color += s_vollight.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(-0.5f, 1.5f) * scaled_screen_res.zw, 0);
    Color += s_vollight.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(1.5f, -0.5f) * scaled_screen_res.zw, 0);
    Color += s_vollight.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(1.5f, 1.5f) * scaled_screen_res.zw, 0);

    return Color * 0.25f;
}
