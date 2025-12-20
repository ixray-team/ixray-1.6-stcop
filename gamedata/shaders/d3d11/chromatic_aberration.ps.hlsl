#include "common.hlsli"

float4 screen_res;

float4 main(PSInputFullscreen I) : SV_Target
{
    float intensity = 1.5;
    float2 offset = saturate(distance(I.texcoord, float2(0.5, 0.5))) * screen_res.zw * intensity;

    float3 col;
	col.x = s_image.SampleLevel(smp_rtlinear, I.texcoord + offset, 0.0).x;
	col.y = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0.0).y;
	col.z = s_image.SampleLevel(smp_rtlinear, I.texcoord - offset, 0.0).z;
    return float4(col, 1);
}

