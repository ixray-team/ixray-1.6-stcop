#include "common.hlsli"
Texture2D s_distort;

float4 main(PSInputFullscreen I) : SV_Target
{
    float4 distort = s_distort.SampleLevel(smp_nofilter, I.texcoord, 0);
    float2 offset = distort.xy - (127.0f / 255.0f);

    float2 center = I.texcoord + offset * def_distort;
    float depth_x = s_position.SampleLevel(smp_nofilter, center, 0).x;

#ifdef SIMPLE_DISTORTION_FIX
    float depth = s_position.SampleLevel(smp_nofilter, I.texcoord, 0).x;
#else
	#define depth 0.02f
#endif
    center = depth_x < depth ? I.texcoord : center;
	
    return s_image.SampleLevel(smp_nofilter, center, 0);
}

