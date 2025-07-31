#include "common.hlsli"
Texture2D s_vollight;

struct _input
{
    float4 tc0 : TEXCOORD0;
};

float4 main(_input I) : SV_Target
{
    return s_vollight.Load(int3(I.tc0.xy * pos_decompression_params2.xy, 0));
}

