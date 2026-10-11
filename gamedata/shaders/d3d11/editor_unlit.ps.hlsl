#include "common.hlsli"

// Unlit view of the editors (no weather): G-buffer albedo
float4 main(PSInputFullscreen I) : SV_Target
{
    return float4(s_image.Sample(smp_nofilter, I.texcoord).xyz, 1.0f);
}