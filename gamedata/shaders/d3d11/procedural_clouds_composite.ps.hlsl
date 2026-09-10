#include "common.hlsli"

Texture2D<float4> s_procedural_clouds;

float4 main(p_screen I) : SV_Target
{
    return s_procedural_clouds.SampleLevel(smp_nofilter, I.tc0, 0.0f);
}
