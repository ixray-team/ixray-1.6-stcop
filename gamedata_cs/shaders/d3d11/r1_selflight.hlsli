#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct v_selflight
{
    float4 Nh : NORMAL;
    float4 T : TANGENT;
    float4 B : BINORMAL;
    int2 tc : TEXCOORD0;
    float4 P : POSITION;
};
