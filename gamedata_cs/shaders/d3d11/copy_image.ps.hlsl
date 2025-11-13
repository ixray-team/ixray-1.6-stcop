#include "common.hlsli"

float4 main(PSInputFullscreen I) : SV_Target
{
    return s_image.Sample(FILTER_TYPE, I.texcoord);
}
