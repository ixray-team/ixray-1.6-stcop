#include "common.hlsli"

// Scene depth into the viewport-sized depth of the editor helpers (scaled render quality)
float main(PSInputFullscreen I) : SV_Depth
{
    return s_position.SampleLevel(smp_nofilter, I.texcoord, 0).x;
}
