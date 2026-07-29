#include "common.hlsli"

float4 main(PSInputFullscreen I) : SV_Target
{
    float2 from_center = I.texcoord - 0.5;
    float4 rg_shift = from_center.xyxy * float2(0.99065, 0.99373).xxyy + 0.5; //LVutner: Precalculated for 0.55 intensity, see AMD code.

    float3 col;
	col.x = s_image.SampleLevel(smp_rtlinear, rg_shift.xy, 0.0).x;
	col.y = s_image.SampleLevel(smp_rtlinear, rg_shift.zw, 0.0).y;
	col.z = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0.0).z;
    return float4(col, 1.0);
}


