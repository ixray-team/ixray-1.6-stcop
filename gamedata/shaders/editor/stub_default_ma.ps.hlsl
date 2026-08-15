#include "common.hlsli"

// Texture2D s_base;
// sampler smp_base;


// Pixel
float4 main(p_TL I) : SV_Target
{
    float4 res = s_base.Sample(smp_base, I.Tex0) * I.Color;

    clip(res.a - 0.003f);

    return res;
    //	return float4(1,1,1,1);
}
