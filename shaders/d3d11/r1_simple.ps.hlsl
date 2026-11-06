#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
};

float4 main(v2p I) : SV_Target
{
    return r1_sample_base(I.tc0);
}
