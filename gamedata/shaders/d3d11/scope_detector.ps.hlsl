#include "common.hlsli"

float4 main(p_TL I) : SV_Target
{
	float4 Color = s_base.Sample(smp_base, I.Tex0) * I.Color;
	// Store linear premultiplied color and coverage in the shared item UI target.
	Color.xyz = detonemap(Color.xyz) * Color.w;
	return Color;
}
