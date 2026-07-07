#include "common.hlsli"

struct v2p
{
    float4 hpos : SV_POSITION;
    float2 tc0 : TEXCOORD0; // base
    float4 proj : TEXCOORD1; // base
    float4 c : COLOR0; // diffuse
    float fog : FOG;
};

// Pixel
uniform Texture2D s_distort;
uniform Texture2D s_image;

float4 main(v2p I) : SV_Target
{
	float2 uv = I.proj.xy / I.proj.w * float2(0.5f, -0.5f) + 0.5f;
    float4 distort = s_distort.Sample(smp_nofilter, I.tc0);
	
    float factor = distort.a * dot(I.c.xyz, 0.33f);
    float2 offset = (distort.xy - (127.0f / 255.0f)) * factor;
    float3 image = s_image.Sample(smp_nofilter, uv + offset * 0.05f).xyz;
	
    return float4(image.xyz, 1.0f);
}

