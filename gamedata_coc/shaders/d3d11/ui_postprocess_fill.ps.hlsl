#include "common.hlsli"

float4 main(PSInputFullscreen I) : SV_Target
{
	float4 Color = s_image.SampleLevel(smp_nofilter, I.texcoord, 0);
	Color.xyz = detonemap(Color.xyz) * rcp(1.5f);

	return Color;
}

