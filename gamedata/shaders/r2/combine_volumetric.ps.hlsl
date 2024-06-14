#include "common.hlsli"

uniform sampler2D s_vollight;

float4 main(float2 tc : TEXCOORD0) : COLOR
{
    return tex2D(s_vollight, tc);
}
