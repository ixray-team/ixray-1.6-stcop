#include "common.hlsli"

struct _in
{
    float4 P : POSITIONT; // xy=pos, zw=tc0
    float2 tcJ : TEXCOORD0; // jitter coords
};

struct v2p
{
    float4 tc0 : TEXCOORD0; // tc.xy, tc.w = tonemap scale
    float2 tcJ : TEXCOORD1; // jitter coords
    float4 hpos : SV_POSITION;
};

// Vertex
v2p main(_in I)
{
    v2p O;
    O.hpos = float4(I.P.x, -I.P.y, 0, 1);
    O.tc0 = float4(I.P.zw, 1, 1);
    O.tcJ = I.tcJ;
    return O;
}
