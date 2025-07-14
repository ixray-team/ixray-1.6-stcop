#include "common.hlsli"
Texture2D s_vollight;

float4 main(PSInputFullscreen I) : SV_Target
{
    return s_vollight.Load(int3(I.texcoord.xy * pos_decompression_params2.xy, 0)); //LVutner: ???
}