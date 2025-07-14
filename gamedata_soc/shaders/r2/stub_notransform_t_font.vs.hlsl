#include "common.hlsli"

uniform float4 screen_res;

struct v2p_TL
{
    float2 Tex0 : TEXCOORD0;
    float4 Color : COLOR;
    float4 HPos : POSITION; // Clip-space position 	(for rasterization)
};

v2p_TL main(v_vert I)
{
    v2p_TL O;

    {
        // I.P.xy += 0.5f;
        O.HPos.x = I.P.x * screen_res.z * 2 - 1;
        O.HPos.y = (I.P.y * screen_res.w * 2 - 1) * -1;
        O.HPos.zw = I.P.zw;
    }

    O.Tex0 = 0; // I.uv + screen_res.zw * 0.5f;
    O.Color = I.color.rgba; //	swizzle vertex colour

    return O;
}
