#include "common.hlsli"	

struct v2p
{
    float4 C : COLOR0;
    float4 P : SV_POSITION;
};

uniform float4 tfactor;

void main(in v_TL_positiont I, out v2p O)
{
    O.P = I.P;
    O.C = I.Color;
}