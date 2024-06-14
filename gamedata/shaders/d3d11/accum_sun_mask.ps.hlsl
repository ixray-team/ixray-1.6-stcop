#include "common.hlsli"

//	TODO: DX10: move to load instead of sample (will need to provide integer texture coordinates)
#define EPS (0.9f / 255.f)
// #define EPS	(40.f/255.f)
#define CLIP_THRESHOLD (1.0f / 255.f)

float4 main(p_TL I, float4 pos2d : SV_Position) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.Tex0, pos2d, O);

    float4 NH = float4(O.Normal, O.Hemi);
    float L = NH.w * dot(Ldynamic_dir, (float3)NH) + EPS + O.SSS * 100.0f + 1000;

    clip(L - CLIP_THRESHOLD);

    return float4(L, L, L, L);
}
