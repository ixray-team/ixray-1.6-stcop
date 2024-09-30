#include "common.hlsli"

#define EPS (0.9f / 255.f)
#define CLIP_THRESHOLD (1.0f / 255.f)

float4 main(p_TL I, float4 pos2d : SV_POSITION) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.Tex0, pos2d.xy, O);

    float4 NH = float4(O.Normal, O.Hemi);
    float L = NH.w - 0.001f + O.SSS * 3.0f;

    clip(L);

    return float4(L, L, L, L);
}

