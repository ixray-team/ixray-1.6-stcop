#include "common.hlsli"

float4 main(float2 tc : TEXCOORD0) : SV_Target
{
    return s_image.Sample(FILTER_TYPE, tc);
}
