#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v2p
{
    float4 c0 : COLOR0;
};

float4 main(v2p I) : SV_Target
{

    return I.c0;
}
