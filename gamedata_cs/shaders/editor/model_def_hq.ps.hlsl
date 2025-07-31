#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
};

// Pixel
float4 main_ps_1_1(v2p I) : COLOR
{
    return tex2D(s_base, I.tc0);
}
