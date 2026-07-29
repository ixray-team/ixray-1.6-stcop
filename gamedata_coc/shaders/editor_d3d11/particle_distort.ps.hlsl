#include "common.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float4 proj : TEXCOORD1; // base
    float4 c : COLOR0; // diffuse
};

// Pixel
uniform sampler s_distort;
uniform sampler s_image;

float4 main(v2p I) : COLOR
{
	float2 uv = I.proj.xy / I.proj.w * float2(0.5f, -0.5f) + 0.5f;
    float4 distort = tex2D(s_distort, I.tc0);
	
    float factor = distort.a * dot(I.c.xyz, 0.33f);
    float2 offset = (distort.xy - (127.0f / 255.0f)) * factor;
    float3 image = tex2D(s_image, uv + offset * 0.05f).xyz;
	
    return float4(image.xyz, 1.0f);
}

