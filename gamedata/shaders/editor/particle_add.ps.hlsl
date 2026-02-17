#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 c : COLOR0; // diffuse
    float fog : FOG;
};

#if 0
float4 main(v2p I) : SV_Target
{
    float4 result = I.c * s_base.Sample(smp_base, I.tc0);

    clip(result.a - (0.01f / 255.0f));

    result.w *= I.fog;
    result.xyz *= I.fog;

    return result;
}
#endif

// Pixel
float4 main(v2p I) : SV_Target
{
    return I.c * s_base.Sample(smp_base, I.tc0);
}

