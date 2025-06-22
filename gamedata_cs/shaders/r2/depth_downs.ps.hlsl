#include "common.hlsli"

uniform float4 screen_res;

float4 main(float2 tc : TEXCOORD0) : COLOR
{
#if SSAO_OPT_DATA == 2
    // FIXME: Should add a float-texel offset to I.tc0 here
    // This would fix horizontal line issue
    float4 P0 = tex2D(s_position, tc + 0.5f * screen_res.zw); // position.(mtl or sun)
    float4 P1 = tex2D(s_position, tc - 0.5f * screen_res.zw); // position.(mtl or sun)
    float4 P2 = tex2D(s_position, tc + 0.5f * float2(screen_res.z, -screen_res.w)); // position.(mtl or sun)
    float4 P3 = tex2D(s_position, tc + 0.5f * float2(-screen_res.z, screen_res.w)); // position.(mtl or sun)

    float4 P = P0;

    if (P1.z < P.z)
    {
        P = P1;
    }
    if (P2.z < P.z)
    {
        P = P2;
    }
    if (P3.z < P.z)
    {
        P = P3;
    }

    return float4(P.zzzz);
#else
    return tex2D(s_position, tc - 0.5f * screen_res.zw).zzzz;
#endif
}
