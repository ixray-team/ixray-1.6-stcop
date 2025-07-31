#include "common.hlsli"

struct _in
{
    float4 p : POSITION;
    float2 tcJ : TEXCOORD0;
};

struct _out
{
    float4 hpos : POSITION;
    float4 tc0 : TEXCOORD0;
    float2 tcJ : TEXCOORD1;
};

void main(in _in I, out _out O)
{
    O.hpos = float4(I.p.x, -I.p.y, 0.0f, 1.0f);
    O.tc0 = float4(I.p.zw, 1.0f, 1.0f);

    O.tcJ = I.tcJ;
}
