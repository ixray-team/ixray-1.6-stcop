#include "common.hlsli"

uniform float4 grass_tint_bounds;
uniform float4 grass_tint_height_range;

struct GrassTerrainCapture
{
    float4 hpos : SV_POSITION;
    float2 uv : TEXCOORD0;
    float height : TEXCOORD1;
};

GrassTerrainCapture main(v_static I)
{
    GrassTerrainCapture O;
    float2 uv = (I.P.xz - grass_tint_bounds.xy) * grass_tint_bounds.zw;
    float depth = (grass_tint_height_range.z - I.P.y) * grass_tint_height_range.w;
    O.hpos = float4(uv.x * 2.0f - 1.0f, 1.0f - uv.y * 2.0f, depth, 1.0f);
    O.uv = unpack_tc_base(I.tc, I.T.w, I.B.w);
    O.height = I.P.y;
    return O;
}
