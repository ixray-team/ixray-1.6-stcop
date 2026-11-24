#include "common\common.hlsli"

// Unlit view of the editors (no weather): the base textures the scene shows in its own encoding, for the screen
float4 ps_main(PSInputFullscreen I) : SV_Target
{
    return float4(SceneToGamma(s_image.Sample(smp_nofilter, I.texcoord).xyz), 1.0f);
}
