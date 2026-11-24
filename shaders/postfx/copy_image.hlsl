#include "common\common.hlsli"

float4 ps_main(PSInputFullscreen I) : SV_Target
{
    return s_image.Sample(FILTER_TYPE, I.texcoord);
}
