#include "common.hlsli"

Texture2D<float> t_focus_prev;

float4 main(PSInputFullscreen I) : SV_Target
{
    //Sample depth buffer
	//float focus = s_position.SampleLevel(smp_nofilter, 0.5f, 0.f).x;
    float focus = GbufferGetPointRealUnjitter(0.5f).z;
    float lastframefocus = t_focus_prev.Sample(smp_nofilter, 0.5f).x;
    float a = 1.f - exp(-(timers.x - timers.y) / 1.f);
    return lerp(focus, lastframefocus, a);
}