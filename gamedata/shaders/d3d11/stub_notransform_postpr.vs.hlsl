#include "common.hlsli"
#include "shared\post_cb.hlsli"

v2p_postpr main(uint VertexId : SV_VertexID)
{
    v2p_postpr O;
    uint Corner = (0x123210u >> (VertexId * 4u)) & 0xFu;
    float2 S = float2(Corner >> 1u, ~Corner & 1u);
    float2 P = PostRect.xy + S * PostRect.zw + 0.5f;

    O.HPos = float4(P * screen_res.zw * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);
    O.Tex0 = lerp(PostUV[0].xy, PostUV[0].zw, S);
    O.Tex1 = lerp(PostUV[1].xy, PostUV[1].zw, S);
    O.Tex2 = lerp(PostUV[2].xy, PostUV[2].zw, S);
    O.Color = PostColor[0];
    O.Gray = PostColor[1];

    return O;
}
