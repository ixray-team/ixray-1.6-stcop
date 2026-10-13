#include "r1_common.hlsli"
#include "r1_static.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0;
    float fog : TEXCOORD1;
};

float4 main(vf I) : SV_Target
{
    float4 color = s_base.Sample(smp_base, I.tc0);
    return float4(lerp(fog_color.xyz, color.rgb, I.fog), color.a);
}
