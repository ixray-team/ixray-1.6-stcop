#include "common.hlsli"

#include "shared\post_cb.hlsli"

float4 main(PSInputFullscreen I) : SV_Target
{
	float4 Color = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0);
	if (ui_has_background.x > 0.5f)
	{
		// Inventory targets already contain the composited background.
		Color.xyz = tonemap(Color.xyz, 1.5f);
		Color.w = Color.w > 0.0f;
		Color.xyz *= Color.w;
	}
	else
	{
		// A transparent target stores premultiplied linear color and coverage.
		Color.w = saturate(Color.w);
		Color.xyz = tonemap(Color.xyz / max(Color.w, EPS_S), 1.5f) * Color.w;
	}

	Color.xyz *= static_color.xyz * static_color.w;
	Color.w *= static_color.w;
	return Color;
}

