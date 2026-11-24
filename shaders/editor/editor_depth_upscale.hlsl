#include "common\common.hlsli"

// Scene depth into the viewport-sized depth of the editor helpers (scaled render quality)
float ps_main(PSInputFullscreen I) : SV_Depth
{
    return s_position.SampleLevel(smp_nofilter, I.texcoord, 0).x;
}
