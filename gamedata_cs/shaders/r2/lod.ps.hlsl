#include "common.hlsli"
#include "sload.hlsli"

struct vf
{
    float4 hpos : POSITION;
    float3 position : TEXCOORD0;
    float2 tc0 : TEXCOORD1;
    float2 tc1 : TEXCOORD2;
    float4 af : COLOR1;
};

void main(in vf I, out f_deffer O)
{
    // 1. Base texture + kill pixels with low alpha
    float4 D0 = tex2D(s_base, I.tc0);
    float4 D1 = tex2D(s_base, I.tc1);
    float4 H0 = tex2D(s_hemi, I.tc0);
    float4 H1 = tex2D(s_hemi, I.tc1);

    float4 D = lerp(D0, D1, I.af.w);
    float4 H = lerp(H0, H1, I.af.w);

    H.xyz = H.rgb * 2.0f - 1.0f;
    D.w *= I.af.z;
    H.w *= I.af.x;

    clip(D.w - (96.h / 255.h));
    float3 N = normalize(H.xyz);

    O = pack_gbuffer(float4(N, H.w), float4(I.position, 0.0f), float4(D.xyz, def_gloss));
}
